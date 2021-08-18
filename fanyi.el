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

(defcustom fanyi-providers '(fanyi-haici-provider
                             fanyi-etymon-provider)
  "The providers used by `fanyi-dwim'."
  :type '(repeat fanyi-base-service)
  :initialize #'(lambda (var providers)
                  (set-default-toplevel-value
                   var
                   (cl-loop for p in (eval providers)
                            do (let ((name (symbol-name p)))
                                 (cl-assert (s-suffix? "-provider" name))
                                 (require (intern (substring name 0 -9))))
                            collect (symbol-value p))))
  :group 'fanyi)

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
                            ;; Move point to the real http content.
                            (goto-char url-http-end-of-headers)
                            ;; Parse the html into a dom tree.
                            (let ((dom (libxml-parse-html-region (point) (point-max) url)))
                              (catch 'not-found
                                ;; Extract information.
                                (fanyi-parse-from instance dom)
                                ;; Since `fanyi-render' manipulates `fanyi-buffer-name',
                                ;; a mutex is required in multi-threaded situation.
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

(defvar fanyi-mode-font-lock-keywords
  '(;; Dictionary name
    ("^# .*" . 'fanyi-dict-face)
    ;; Sub headline
    ("^##" . 'fanyi-sub-headline-face)
    ;; Quotes
    ("^> .*" . 'fanyi-quote-face)
    ;; Fancy star
    ("★" . 'fanyi-star-face)
    ;; List
    ("^-" . 'fanyi-list-face)
    ;; Use Minion New font to fontify pronunciation of American Heritage
    ;; dictionary.
    ("\u200b\\([^\u200b]+?\\)\u200b" . 'fanyi-ah-pronunciation-face)
    ;; Italic
    ("/\\([^/]+?\\)/" . 'italic)
    ;; Bold
    ("\\*\\([^\\*]+?\\)\\*" . 'bold))
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
                 (list (read-string prompt nil nil default))))
  ;; libxml2 is required.
  (unless (fboundp 'libxml-parse-html-region)
    (error "This function requires Emacs to be compiled with libxml2"))
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
                (oset i :word (url-hexify-string word))
                ;; Do search.
                (fanyi--spawn i))
              instances))
    (pop-to-buffer buf)))

;; Internals

(defun fanyi-tab ()
  "Smart tab in `fanyi-mode'."
  (interactive nil fanyi-mode)
  (unless (derived-mode-p 'fanyi-mode)
    (error "Not in fanyi-mode"))
  (if (outline-on-heading-p)
      (outline-cycle)
    (forward-button 1 t t t)))

(defun fanyi-backtab ()
  "Smart backtab in `fanyi-mode'."
  (interactive nil fanyi-mode)
  (unless (derived-mode-p 'fanyi-mode)
    (error "Not in fanyi-mode"))
  (if (outline-on-heading-p)
      (outline-cycle-buffer)
    (backward-button 1 t t t)))

(provide 'fanyi)
;;; fanyi.el ends here
