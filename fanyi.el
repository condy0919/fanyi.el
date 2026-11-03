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
(require 'subr-x)
(require 'seq)
(require 'url)
(require 'imenu)
(require 'button)
(require 'browse-url)
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

(defcustom fanyi-providers '(fanyi-haici-provider
                             fanyi-etymon-provider
                             fanyi-longman-provider
                             fanyi-youdao-thesaurus-provider)
  "Dictionary provider symbols used by `fanyi-dwim'.
Changes take effect on the next query, including changes made with `setq'.
Providers are loaded and cloned when a query starts.  Service instances
are also accepted for compatibility with existing configurations."
  :type '(repeat symbol)
  :group 'fanyi)

(defun fanyi--make-provider (provider)
  "Load PROVIDER if needed and return a fresh service instance."
  (let ((service provider))
    (when (symbolp provider)
      (unless (boundp provider)
        (let ((name (symbol-name provider)))
          (unless (and (string-suffix-p "-provider" name)
                       (require (intern (substring name 0 -9)) nil t))
            (user-error "Cannot load dictionary provider: %S" provider))))
      (setq service (symbol-value provider)))
    (unless (and (eieio-object-p service)
                 (object-of-class-p service 'fanyi-base-service))
      (user-error "Not a dictionary provider: %S" provider))
    (clone service)))

(defcustom fanyi-headline-max-length 30
  "The maximum displayed length of `fanyi-current-word'."
  :type 'integer
  :group 'fanyi)

(defcustom fanyi-auto-select t
  "Non-nil means select fanyi window."
  :type 'boolean
  :group 'fanyi)

(defcustom fanyi-request-timeout 15
  "Maximum seconds to wait for each dictionary request.
Nil disables the timeout."
  :type '(choice (const :tag "No timeout" nil)
                 (number :tag "Seconds"))
  :group 'fanyi)

(defvar fanyi-history nil
  "History list for `fanyi-dwim' and `fanyi-dwim2'.")

;; Silence compile warnings.
(defvar url-http-end-of-headers)

(defvar fanyi--tasks nil)
(defvar fanyi--tasks-failed 0)
(defvar fanyi--tasks-completed 0)

(defvar fanyi--query-id 0
  "Identifier of the current query.")

(defvar fanyi-current-word nil)

(cl-defstruct (fanyi--task (:constructor fanyi--make-task))
  query-id instance buffer timer done)

(define-error 'fanyi--cloudflare-challenge "Browser verification required")

(defun fanyi--cloudflare-challenge-p ()
  "Return non-nil when the current HTTP response is a Cloudflare challenge.
Only inspect response headers, never matching text in the response body."
  (let ((end (bound-and-true-p url-http-end-of-headers)))
    (and (integer-or-marker-p end)
         (<= (point-min) end (point-max))
         (save-excursion
           (save-match-data
             (goto-char (point-min))
             (let ((case-fold-search t))
               (re-search-forward "^cf-mitigated:[ \t]*challenge[ \t]*\r?$" end t)))))))

(defun fanyi--task-active-p (task)
  "Return non-nil if TASK still belongs to the current pending query."
  (and (= (fanyi--task-query-id task) fanyi--query-id)
       (not (fanyi--task-done task))))

(defun fanyi--stop-task-timer (task)
  "Cancel TASK's timeout timer."
  (when (fanyi--task-timer task)
    (cancel-timer (fanyi--task-timer task))
    (setf (fanyi--task-timer task) nil)))

(defun fanyi--cancel-task (task)
  "Cancel the network request associated with TASK."
  ;; Invalidate first: deleting a process can invoke its callback.
  (setf (fanyi--task-done task) t)
  (fanyi--stop-task-timer task)
  (let ((buffer (fanyi--task-buffer task)) seen)
    (while (and (buffer-live-p buffer) (not (memq buffer seen)))
      (push buffer seen)
      (let ((next (buffer-local-value 'url-redirect-buffer buffer))
            (process (get-buffer-process buffer)))
        (when process
          (delete-process process))
        (kill-buffer buffer)
        (setq buffer next)))))

(defun fanyi--report-task-error (task description &optional browser-url)
  "Display TASK's failure DESCRIPTION and an appropriate action.
When BROWSER-URL is non-nil, offer to open it in an external browser
instead of repeating the blocked query."
  (let* ((instance (fanyi--task-instance task))
         (name (url-host (url-generic-parse-url (oref instance url))))
         (word fanyi-current-word))
    (fanyi-log "%s: %s" name description)
    (when fanyi-verbose
      (message "%s: %s" name description))
    (with-current-buffer (get-buffer-create fanyi-buffer-name)
      (save-excursion
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (insert "# " (or name "Dictionary") "\n\n" description "\n\n")
          (if browser-url
              (insert-button "Open in browser"
                             'action (lambda (_) (browse-url-default-browser browser-url))
                             'help-echo "Read this entry in an external browser"
                             'follow-link t)
            (insert-button "Retry query"
                           'action (lambda (_) (fanyi-dwim word))
                           'follow-link t))
          (insert "\n\n"))))))

(defun fanyi--finish-task (task &optional failure browser-url)
  "Settle TASK once, reporting FAILURE if non-nil.
BROWSER-URL offers an external browser action for blocked requests."
  (when (fanyi--task-active-p task)
    (setf (fanyi--task-done task) t)
    (fanyi--stop-task-timer task)
    (if failure
        (progn
          (cl-incf fanyi--tasks-failed)
          (fanyi--report-task-error task failure browser-url))
      (cl-incf fanyi--tasks-completed))
    (force-mode-line-update t)))

(defun fanyi--timeout (task)
  "Fail and cancel TASK if its response has not arrived."
  (when (fanyi--task-active-p task)
    (unwind-protect
        (fanyi--finish-task task "Request timed out")
      (fanyi--cancel-task task))))

(defun fanyi--render-task (task)
  "Render TASK's result atomically and mark it complete."
  (when (fanyi--task-active-p task)
    (with-mutex fanyi-buffer-mtx
      (with-current-buffer (get-buffer-create fanyi-buffer-name)
        (let ((inhibit-read-only t))
          ;; Do not leave half a dictionary section on failure.
          (atomic-change-group
            (fanyi-render (fanyi--task-instance task))))))
    (fanyi--finish-task task)))

(defun fanyi--handle-response (task url status)
  "Parse and render TASK's response from URL with retrieval STATUS."
  (let ((response-buffer (current-buffer))
        (instance (fanyi--task-instance task)))
    (unwind-protect
        (when (fanyi--task-active-p task)
          (condition-case err
              (progn
                ;; A challenge is not dictionary content, even if it arrives
                ;; with a successful HTTP status.  Detect it before parsing
                ;; or handling the generic HTTP error from url.el.
                (when (fanyi--cloudflare-challenge-p)
                  (signal 'fanyi--cloudflare-challenge nil))
                ;; An empty status is success.  Redirects are followed by
                ;; url.el and are not failures unless accompanied by :error.
                (when (plist-member status :error)
                  (error "Request failed: %S" (plist-get status :error)))
                (unless (and (bound-and-true-p url-http-end-of-headers)
                             (integer-or-marker-p url-http-end-of-headers)
                             (<= (point-min) url-http-end-of-headers)
                             (< url-http-end-of-headers (point-max)))
                  (error "Invalid HTTP response"))
                (goto-char (1+ url-http-end-of-headers))
                (let ((result (pcase (oref instance api-type)
                                ('xml (libxml-parse-html-region (point) (point-max) url))
                                ('json (json-parse-buffer))
                                (_ (error "Unsupported response format")))))
                  (unless (catch 'not-found
                            (fanyi-parse-from instance result)
                            t)
                    (error "No results found")))
                (fanyi--render-task task))
            (fanyi--cloudflare-challenge
             (fanyi--finish-task
              task "Cloudflare requires browser verification. Read this entry in your browser."
              url))
            (error (fanyi--finish-task task (error-message-string err)))))
      (fanyi--cleanup-response response-buffer)
      (fanyi--cleanup-response (fanyi--task-buffer task)))))

(defun fanyi--spawn (instance)
  "Start INSTANCE's lookup or render its browser link for the current query."
  (let ((task (fanyi--make-task :query-id fanyi--query-id
                                :instance instance)))
    ;; Register before starting the request: a callback may run immediately.
    (push task fanyi--tasks)
    (condition-case err
        (if (eq (oref instance api-type) 'browser)
            (fanyi--render-task task)
          (let ((url (format (oref instance url) (oref instance word)))
                (url-request-method (oref instance method))
                (url-request-extra-headers (oref instance headers))
                (url-request-data (oref instance body)))
            (setf (fanyi--task-buffer task)
                  (url-retrieve url
                                (lambda (status)
                                  (fanyi--handle-response task url status))
                                nil t t))
            (cond
             ((fanyi--task-done task)
              ;; The callback may have completed before url-retrieve returned.
              (fanyi--cleanup-response (fanyi--task-buffer task)))
             ((not (buffer-live-p (fanyi--task-buffer task)))
              (error "Could not start request"))
             (fanyi-request-timeout
              (setf (fanyi--task-timer task)
                    (run-at-time fanyi-request-timeout nil #'fanyi--timeout task))))))
      (error
       (unwind-protect
           (fanyi--finish-task task (error-message-string err))
         (fanyi--cancel-task task))))
    task))

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

(defun fanyi--font-lock-search (regexp limit)
  "Search for REGEXP before LIMIT outside rendered HTML.
HTML providers supply their own semantic `font-lock-face' properties.
Confining each search to one text region also prevents matches from
crossing into HTML and consuming delimiters in a later dictionary."
  (let (found)
    (while (and (< (point) limit) (not found))
      (let ((end (next-single-property-change (point) 'fanyi-html nil limit)))
        (if (get-text-property (point) 'fanyi-html)
            (goto-char end)
          (setq found (re-search-forward regexp end t))
          (unless found (goto-char end)))))
    found))

(defvar fanyi-mode-font-lock-keywords
  (append
   '(;; Dictionary name
     ("^# .*" 0 (list 'face 'fanyi-dict-face
                      'keymap fanyi-mode-cycle-map))
     ;; Keep HTML heading styles and the common folding keymap.
     ("^## .*" 0 (list 'face
                       (unless (and (get-text-property (match-beginning 0) 'fanyi-html)
                                    (get-text-property (match-beginning 0) 'font-lock-face))
                         'fanyi-sub-headline-face)
                       'keymap fanyi-mode-cycle-map)))
   (mapcar (lambda (rule)
             (list (apply-partially #'fanyi--font-lock-search (car rule))
                   0 (list 'quote (cdr rule))))
           '(("^> .*" . fanyi-quote-face)
             ("^-" . fanyi-list-face)
             ("(\\([a-z\.]+?\\))" . fanyi-pos-face)
             ("/\\([^/]+?\\)/" . italic)
             ("\\*\\([^\\*]+?\\)\\*" . bold)
             ("_\\([^_]+?\\)_" . underline))))
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
  (setq-local outline-minor-mode-use-buttons nil)
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

  ;; Results are regenerated by queries; retaining undo history wastes memory.
  (setq buffer-undo-list t)
  ;; Providers may be loaded after fanyi.el or changed between queries.
  (setq font-lock-defaults
        (list (append fanyi-mode-font-lock-keywords
                      fanyi-mode-font-lock-keywords-extra)))
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
  (let ((instances (mapcar #'fanyi--make-provider fanyi-providers)))
    ;; Browser entries do not need either response parser.
    (when (and (seq-some (lambda (i) (eq (oref i api-type) 'xml)) instances)
               (not (fboundp 'libxml-parse-html-region)))
      (error "This function requires Emacs to be compiled with libxml2"))
    (when (and (seq-some (lambda (i) (eq (oref i api-type) 'json)) instances)
               (not (fboundp 'json-parse-buffer)))
      (error "This function requires Emacs to be compiled with libjson"))
    ;; Save current query word.
    (setq fanyi-current-word word)
    ;; Invalidate callbacks before cancelling requests, which can itself invoke
    ;; process sentinels.  `url-retrieve' is already asynchronous; no threads are
    ;; needed to start concurrent dictionary requests.
    (cl-incf fanyi--query-id)
    (mapc #'fanyi--cancel-task fanyi--tasks)
    (setq fanyi--tasks nil)
    ;; Reset the counter of completed/failed tasks.
    (setq fanyi--tasks-completed 0)
    (setq fanyi--tasks-failed 0)

    (let ((buf (get-buffer-create fanyi-buffer-name)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          ;; Clear the previous search result.
          (erase-buffer)
          (fanyi-mode)))

      (seq-do (lambda (i)
                ;; Set the query word.
                (fanyi-set-query-word i (downcase word))
                ;; Do search.
                (fanyi--spawn i))
              instances)
      (if fanyi-auto-select
          (pop-to-buffer buf)
        (display-buffer buf)))))

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
