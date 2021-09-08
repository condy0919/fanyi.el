;;; fanyi.el --- Not only English-Chinese translator -*- lexical-binding: t -*-

;; Copyright (C) 2021 Zhiwei Chen
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/condy0919/fanyi.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (s "1.12.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A multi dictionaries interface for Emacs.
;;
;; There is only one public command to users: `fanyi-dwim'.

;;; Code:

(require 's)
(require 'seq)
(require 'url)
(require 'imenu)
(require 'button)
(require 'cl-lib)
(require 'outline)

(require 'fanyi-base)

(defgroup fanyi nil
  "Not only English-Chinese translator for Emacs."
  :prefix "fanyi-"
  :group 'tools
  :link '(url-link "https://github.com/condy0919/fanyi.el"))

(defface fanyi-word-face
  '((t :height 1.75 :weight bold :foreground "dark cyan"))
  "Face used for user requested word."
  :group 'fanyi)

(defface fanyi-dict-face
  '((t :height 1.25 :weight bold :foreground "#ecbe7b" :extend t))
  "Face used for dictionary name."
  :group 'fanyi)

(defface fanyi-sub-headline-face
  '((t :weight bold :foreground "#a9a1e1" :extend t))
  "Face used for sub-headline. Normally it's part of speech."
  :group 'fanyi)

(defface fanyi-list-face
  '((t :foreground "#51afef"))
  "Face used for list."
  :group 'fanyi)

(defface fanyi-quote-face
  '((t :inherit font-lock-comment-face))
  "Face used for quotes of word."
  :group 'fanyi)

(defface fanyi-pos-face
  '((t :foreground "green"))
  "Face used for part of speech."
  :group 'fanyi)

(defun fanyi-set-providers (sym providers)
  "Set SYM with evaluated PROVIDERS."
  (set-default-toplevel-value
   sym
   (cl-loop for p in providers
            do (let ((name (symbol-name p)))
                 (cl-assert (s-suffix? "-provider" name))
                 (require (intern (substring name 0 -9))))
            collect (symbol-value p))))

(defcustom fanyi-providers '(fanyi-haici-provider
                             fanyi-etymon-provider
                             fanyi-longman-provider
                             fanyi-youdao-thesaurus-provider)
  "The providers used by `fanyi-dwim'."
  :type '(repeat fanyi-base-service)
  :initialize #'custom-initialize-set
  :set #'fanyi-set-providers
  :group 'fanyi)

(defvar fanyi-history nil
  "History list for `fanyi-dwim'.")

;; Silence compile warnings.
(defvar url-http-end-of-headers)

(defvar fanyi--tasks nil)
(defvar fanyi--tasks-completed 0)

(defun fanyi--spawn (instance)
  "Spawn a thread for searching. The result is powered by INSTANCE."
  (let ((url (format (oref instance :url)
                     (oref instance :word))))
    (cl-pushnew
     (make-thread
      (lambda ()
        (url-retrieve url (lambda (status)
                            ;; Something went wrong.
                            (when (or (not status) (plist-member status :error))
                              (user-error "Something went wrong.\n\n%s" (pp-to-string (plist-get status :error))))
                            ;; Redirection is inhibited.
                            (when (plist-member status :redirect)
                              (user-error "Redirection is inhibited.\n\n%s" (pp-to-string (plist-get status :redirect))))
                            ;; Move point to the real http content. Plus 1 for '\n'.
                            (goto-char (1+ url-http-end-of-headers))
                            ;; Normalize data.
                            ;;
                            ;; `json-read' failed to parse in undecoded buffer.
                            ;;
                            ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50391
                            (let ((result (pcase (oref instance :api-type)
                                            ('xml (libxml-parse-html-region (point) (point-max) url))
                                            ('json (json-parse-buffer)))))
                              (catch 'not-found
                                ;; Extract information.
                                (fanyi-parse-from instance result)
                                ;; Since `fanyi-render' manipulates `fanyi-buffer-name',
                                ;; a mutex is required in multi-threaded
                                ;; situation.
                                (with-mutex fanyi-buffer-mtx
                                  (fanyi-render instance))
                                (cl-incf fanyi--tasks-completed))))
                      nil
                      t
                      t)))
     fanyi--tasks)))

(defvar fanyi--current-word nil)

(defun fanyi-format-header-line ()
  "Used as `header-line-format'."
  (concat "Translating "
          (propertize fanyi--current-word 'face 'fanyi-word-face)
          (when (< fanyi--tasks-completed (length fanyi-providers))
            (format " %d/%d" fanyi--tasks-completed (length fanyi-providers)))))

;; Emacs 28.1 can have multiple eldoc functions and it's called with a callback.
;; While in Emacs 27, it's called without arguments.
(defun fanyi-eldoc-function (&rest _)
  "ElDoc for `fanyi-mode'."
  (get-char-property (point) 'help-echo))

(defvar fanyi-mode-font-lock-keywords
  `(;; Dictionary name
    ("^# .*" . 'fanyi-dict-face)
    ;; Sub headline
    ("^##" . 'fanyi-sub-headline-face)
    ;; Quotes
    ("^> .*" . 'fanyi-quote-face)
    ;; List
    ("^-" . 'fanyi-list-face)
    ;; Part of speech
    ("(\\([a-z\.]+?\\))" . 'fanyi-pos-face)
    ;; Italic
    ("/\\([^/]+?\\)/" . 'italic)
    ;; Bold
    ("\\*\\([^\\*]+?\\)\\*" . 'bold)
    ;; Underline
    ("_\\([^_]+?\\)_" . 'underline)
    ;; Other dicts can define their own font-lock keywords
    ,@fanyi-mode-font-lock-keywords-extra)
  "Keywords to highlight in `fanyi-mode'.")

(defvar fanyi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] #'fanyi-tab)
    (define-key map [backtab] #'fanyi-backtab)
    (define-key map "q" #'quit-window)
    (define-key map "s" #'fanyi-dwim)
    map)
  "Keymap for `fanyi-mode'.")

(define-derived-mode fanyi-mode special-mode "Fanyi"
  "Major mode for viewing multi translators result.
\\{fanyi-mode-map}"
  :interactive nil
  :group 'fanyi

  ;; Make it foldable.
  (setq-local outline-regexp "^#+")
  (setq-local outline-minor-mode t)

  ;; Better display
  (setq-local line-spacing 0.1)

  ;; ElDoc integration.
  ;;
  ;; Emacs 28.1 can have multiple eldoc functions.
  (if (boundp 'eldoc-documentation-functions)
      (add-hook 'eldoc-documentation-functions #'fanyi-eldoc-function nil t)
    (setq-local eldoc-documentation-function #'fanyi-eldoc-function))

  (setq font-lock-defaults '(fanyi-mode-font-lock-keywords))
  (setq imenu-generic-expression '(("Dict" "^# \\(.*\\)" 1)))
  (setq header-line-format '((:eval (fanyi-format-header-line)))))

;;;###autoload
(defun fanyi-dwim (word)
  "Translate WORD."
  (interactive (let* ((default (if (use-region-p)
                                   (buffer-substring-no-properties (region-beginning) (region-end))
                                 (thing-at-point 'word t)))
                      (prompt (if (stringp default)
                                  (format "Search Word (default \"%s\"): " default)
                                "Search Word: ")))
                 (list (read-string prompt nil 'fanyi-history default))))
  ;; libxml2 is required.
  (unless (fboundp 'libxml-parse-html-region)
    (error "This function requires Emacs to be compiled with libxml2"))
  ;; libjson is required.
  (unless (fboundp 'json-parse-buffer)
    (error "This function requires Emacs to be compiled with libjson"))
  ;; Save current query word.
  (setq fanyi--current-word word)
  ;; Cancel the still pending threads.
  (seq-do (lambda (th)
            (when (thread-live-p th)
              (thread-signal th nil nil)))
          fanyi--tasks)
  (setq fanyi--tasks nil)
  ;; Reset the counter of completed tasks.
  (setq fanyi--tasks-completed 0)

  (let ((buf (get-buffer-create fanyi-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (inhibit-point-motion-hooks t))
        ;; Clear the previous search result.
        (erase-buffer)
        (fanyi-mode)))

    ;; Create a new instance per search.
    (let ((instances (seq-map #'clone fanyi-providers)))
      (seq-do (lambda (i)
                ;; Overwrite the dummy word.
                (oset i :word (url-hexify-string (downcase word)))
                ;; Do search.
                (fanyi--spawn i))
              instances))
    (pop-to-buffer buf)))

;; Internals

(defun fanyi-tab ()
  "Context-aware tab in `fanyi-mode'."
  (interactive nil fanyi-mode)
  (unless (derived-mode-p 'fanyi-mode)
    (error "Not in fanyi-mode"))
  (if (outline-on-heading-p)
      (outline-cycle)
    (forward-button 1 t t t)))

(defun fanyi-backtab ()
  "Context-aware backtab in `fanyi-mode'."
  (interactive nil fanyi-mode)
  (unless (derived-mode-p 'fanyi-mode)
    (error "Not in fanyi-mode"))
  (if (outline-on-heading-p)
      (outline-cycle-buffer)
    (backward-button 1 t t t)))

(provide 'fanyi)
;;; fanyi.el ends here
