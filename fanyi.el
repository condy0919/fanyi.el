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
;; Invoke `fanyi-dwim', `fanyi-dwim2' or `fanyi-from-history' to see the
;; results.

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

(defface fanyi-tasks-pending-face
  '((t :foreground "yellow"))
  "Face used for tasks still pending."
  :group 'fanyi)

(defface fanyi-tasks-failed-face
  '((t :foreground "red"))
  "Face used for tasks failed."
  :group 'fanyi)

(defface fanyi-tasks-completed-face
  '((t :foreground "green"))
  "Face used for tasks completed."
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
                             fanyi-youdao-thesaurus-provider
                             fanyi-libre-provider)
  "The providers used by `fanyi-dwim'."
  :type '(repeat fanyi-base-service)
  :initialize #'custom-initialize-set
  :set #'fanyi-set-providers
  :group 'fanyi)

(defcustom fanyi-headline-max-length 30
  "The maximum displayed length of `fanyi-current-word'."
  :type 'integer
  :group 'fanyi)

(defvar fanyi-history nil
  "History list for `fanyi-dwim' and `fanyi-dwim2'.")

;; Silence compile warnings.
(defvar url-http-end-of-headers)

(defvar fanyi--tasks nil)
(defvar fanyi--tasks-failed 0)
(defvar fanyi--tasks-completed 0)

(defun fanyi--spawn (instance)
  "Spawn a thread for searching. The result is powered by INSTANCE."
  (let ((url (format (oref instance :url)
                     (oref instance :word))))
    (cl-pushnew
     (make-thread
      (lambda ()
        (let ((url-request-method (oref instance :method))
              (url-request-extra-headers (oref instance :headers))
              (url-request-data (oref instance :body)))
          (url-retrieve url (lambda (status)
                              ;; Something went wrong.
                              (when (or (not status) (plist-member status :error))
                                (cl-incf fanyi--tasks-failed)
                                (fanyi-user-error "Something went wrong.\n\n%s" (pp-to-string (plist-get status :error))))
                              ;; Redirection is inhibited. In most cases, the word is misspelled by users.
                              (when (plist-member status :redirect)
                                (cl-incf fanyi--tasks-failed)
                                (fanyi-user-error "Did you misspell the word?\n\n%s" (pp-to-string (plist-get status :redirect))))
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
                                (unless
                                    (catch 'not-found
                                      ;; Extract information. A `not-found' exception may be thrown.
                                      (fanyi-parse-from instance result)
                                      ;; Since `fanyi-render' manipulates `fanyi-buffer-name',
                                      ;; a mutex is required in multi-threaded
                                      ;; situation.
                                      (with-mutex fanyi-buffer-mtx
                                        (fanyi-render instance))
                                      (cl-incf fanyi--tasks-completed))
                                  (cl-incf fanyi--tasks-failed))))
                        nil
                        t
                        t))))
     fanyi--tasks)))

(defvar fanyi-current-word nil)

(defun fanyi-format-header-line ()
  "Used as `header-line-format'."
  (let ((truncated (if (> (length fanyi-current-word) fanyi-headline-max-length)
                       (concat (substring fanyi-current-word 0 fanyi-headline-max-length) "...")
                     fanyi-current-word)))
    (concat "Translating "
            (propertize truncated 'face 'fanyi-word-face)
            " "
            (propertize (number-to-string (- (length fanyi--tasks) fanyi--tasks-completed fanyi--tasks-failed)) 'face 'fanyi-tasks-pending-face)
            " "
            (propertize (number-to-string fanyi--tasks-completed) 'face 'fanyi-tasks-completed-face)
            " "
            (propertize (number-to-string fanyi--tasks-failed) 'face 'fanyi-tasks-failed-face))))

;; Emacs 28.1 can have multiple eldoc functions and it's called with a callback.
;; While in Emacs 27, it's called without arguments.
(defun fanyi-eldoc-function (&rest _)
  "ElDoc for `fanyi-mode'."
  (get-char-property (point) 'help-echo))

(defvar fanyi-mode-font-lock-keywords
  `(;; Dictionary name
    ("^# .*" 0 (list 'face 'fanyi-dict-face
                     'keymap fanyi-mode-cycle-map))
    ;; Sub headline
    ("^## .*" 0 (list 'face 'fanyi-sub-headline-face
                      'keymap fanyi-mode-cycle-map))
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
    (define-key map [tab] #'forward-button)
    (define-key map [backtab] #'backward-button)
    (define-key map "q" #'quit-window)
    (define-key map "s" #'fanyi-dwim)
    (define-key map "w" #'fanyi-copy-query-word)
    (define-key map "h" #'fanyi-from-history)
    map)
  "Keymap for `fanyi-mode'.")

(defvar fanyi-mode-cycle-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] #'outline-cycle)
    (define-key map [backtab] #'outline-cycle-buffer)
    map)
  "Keymap used by headlines of `fanyi-mode'.")

(define-derived-mode fanyi-mode special-mode "Fanyi"
  "Major mode for viewing multi translators result.
\\{fanyi-mode-map}"
  :interactive nil
  :group 'fanyi

  ;; Set these variables to nil, it's handled by ourselves.
  (setq-local outline-minor-mode-cycle nil)
  (setq-local outline-minor-mode-highlight nil)
  ;; Make it foldable.
  (setq-local outline-regexp "^#+")
  (setq-local outline-minor-mode t)

  ;; `bookmark' integration.
  (setq-local bookmark-make-record-function #'fanyi-bookmark-make-record)

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
  (setq fanyi-current-word word)
  ;; Cancel the still pending threads.
  (seq-do (lambda (th)
            (when (thread-live-p th)
              (thread-signal th nil nil)))
          fanyi--tasks)
  (setq fanyi--tasks nil)
  ;; Reset the counter of completed/failed tasks.
  (setq fanyi--tasks-completed 0)
  (setq fanyi--tasks-failed 0)

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
                ;; Set the query word.
                (fanyi-set-query-word i (downcase word))
                ;; Do search.
                (fanyi--spawn i))
              instances))
    (pop-to-buffer buf)))

;;;###autoload
(defun fanyi-dwim2 ()
  "A more dwim version of `fanyi-dwim'.
No prompt if the region is active or `thing-at-point' returns
non-nil."
  (interactive)
  (if-let ((word (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (thing-at-point 'word t))))
      (progn
        ;; Calling `fanyi-dwim' with an argument will never prompt users, no
        ;; `read-string' function calls, then no new item in `fanyi-history'. We
        ;; manually push the word to `fanyi-history'.
        (fanyi-dwim word)
        (cl-pushnew word fanyi-history))
    (call-interactively #'fanyi-dwim)))

;;;###autoload
(defun fanyi-from-history ()
  "Invoke `fanyi-dwim' from history."
  (interactive)
  (fanyi-dwim (completing-read "Fanyi history: " fanyi-history nil t)))

(defun fanyi-copy-query-word ()
  "Copy current query word."
  (interactive nil fanyi-mode)
  (kill-new fanyi-current-word)
  (message "Copied %s" fanyi-current-word))

;;; Bookmark support.
(declare-function bookmark-prop-get "bookmark" (bookmark prop))

(defun fanyi-bookmark-make-record ()
  "Create a bookmark for the current query word."
  `(,fanyi-current-word
    (handler . fanyi-bookmark-jump)))

;;;###autoload
(defun fanyi-bookmark-jump (bookmark)
  "Default BOOKMARK handler for fanyi."
  (let ((word (car bookmark)))
    (fanyi-dwim word)))

(put 'fanyi-bookmark-jump 'bookmark-handler-type "Fanyi")

(provide 'fanyi)
;;; fanyi.el ends here
