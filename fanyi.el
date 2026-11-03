;;; fanyi.el --- Not only English-Chinese translator -*- lexical-binding: t -*-

;; Copyright (C) 2021 Zhiwei Chen
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/condy0919/fanyi.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

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

(require 'subr-x)
(require 'seq)
(require 'url)
(require 'imenu)
(require 'button)
(require 'cl-lib)
(require 'outline)
(require 'eldoc)

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
Providers are loaded when a query starts, and each lookup gets a separate
request and result.  `fanyi-provider' records are also accepted."
  :type '(repeat symbol)
  :group 'fanyi)

(defun fanyi--resolve-provider (provider)
  "Load PROVIDER if needed and return its configuration."
  (let ((configuration provider))
    (when (symbolp provider)
      (unless (boundp provider)
        (let ((name (symbol-name provider)))
          (unless (and (string-suffix-p "-provider" name)
                       (require (intern (substring name 0 -9)) nil t))
            (user-error "Cannot load dictionary provider: %S" provider))))
      (setq configuration (symbol-value provider)))
    (unless (fanyi-provider-p configuration)
      (user-error "Not a dictionary provider: %S" provider))
    configuration))

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

(defvar-local fanyi--buffer-query-id 0
  "Query generation owned by this result buffer.")

(defvar-local fanyi--tasks nil
  "Tasks belonging to this result buffer's query.")
(defvar-local fanyi--tasks-failed 0)
(defvar-local fanyi--tasks-completed 0)

(defvar fanyi--active-buffer nil
  "Result buffer whose query supersedes the previous pending query.")

(defvar fanyi--query-id 0
  "Counter assigning query generations to result buffers.")

(defvar-local fanyi-current-word nil
  "Original query text displayed in this result buffer.")

(cl-defstruct (fanyi--task (:constructor fanyi--make-task))
  query-id request result result-buffer buffer timer done failure start end)

(defun fanyi--task-active-p (task)
  "Return non-nil if TASK still belongs to the current pending query."
  (and (not (fanyi--task-done task))
       (buffer-live-p (fanyi--task-result-buffer task))
       (with-current-buffer (fanyi--task-result-buffer task)
         (and (derived-mode-p 'fanyi-mode)
              (= (fanyi--task-query-id task) fanyi--buffer-query-id)
              (memq task fanyi--tasks)))))

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

(defun fanyi--cancel-buffer-tasks ()
  "Cancel tasks owned by the result buffer being killed or changing mode."
  (let ((retries (cl-remove-if-not
                  (lambda (task) (and (not (fanyi--task-done task))
                                      (fanyi--task-failure task)))
                  fanyi--tasks)))
    ;; Invalidate every callback before cancelling any network process.
    (dolist (task fanyi--tasks)
      (setf (fanyi--task-done task) t))
    (mapc #'fanyi--cancel-task fanyi--tasks)
    (dolist (task retries)
      (fanyi--restore-retry-failure task))))

(defun fanyi--restore-retry-failure (task)
  "Restore the unresolved failure when TASK's retry is cancelled."
  (with-current-buffer (fanyi--task-result-buffer task)
    (cl-incf fanyi--tasks-failed)
    (fanyi--write-task-error task "Retry cancelled")))

(defun fanyi--quit-task (task condition)
  "Cancel TASK's resources and propagate the original quit CONDITION."
  (unwind-protect
      (let ((inhibit-quit t)
            (pending (fanyi--task-active-p task)))
        (fanyi--cancel-task task)
        ;; An interrupted retry leaves its original failure unresolved.
        (when (and pending (fanyi--task-failure task))
          (fanyi--restore-retry-failure task))
        (force-mode-line-update t))
    (signal (car condition) (cdr condition))))

(defun fanyi--write-task (task writer)
  "Call WRITER to replace only TASK's output, preserving other dictionaries."
  (with-current-buffer (fanyi--task-result-buffer task)
    (save-excursion
      (save-restriction
        (widen)
        (let* ((start (if (fanyi--task-start task)
                          (marker-position (fanyi--task-start task)) (point-max)))
               (end (if (fanyi--task-end task)
                        (marker-position (fanyi--task-end task)) start))
               (inhibit-read-only t)
               (deactivate-mark deactivate-mark)
               (mark-active mark-active))
          (narrow-to-region start end)
          (condition-case err
              (atomic-change-group
                (delete-region (point-min) (point-max))
                (goto-char (point-min))
                (funcall writer))
            ((error quit)
             ;; Undo restores text, but insertion-type markers need their
             ;; original boundaries before the failure can replace this area.
             (when (fanyi--task-start task)
               (set-marker (fanyi--task-start task) start)
               (set-marker (fanyi--task-end task) end))
             (signal (car err) (cdr err))))
          (let ((inhibit-quit t))
            (unless (fanyi--task-start task)
              (setf (fanyi--task-start task) (make-marker)
                    (fanyi--task-end task) (make-marker))
              (set-marker-insertion-type (fanyi--task-start task) t))
            (set-marker (fanyi--task-start task) start)
            (set-marker (fanyi--task-end task) (point-max))))))))

(defun fanyi--write-task-error (task description)
  "Replace TASK's output with DESCRIPTION and its current retry action."
  (fanyi--write-task
   task
   (lambda ()
     (let ((name (url-host (url-generic-parse-url
                            (fanyi-request-url (fanyi--task-request task))))))
       (insert "# " (or name "Dictionary") "\n\n" description "\n\n")
       (insert-button (if (fanyi--task-done task) "Retry dictionary" "Retrying dictionary…")
                      'action #'fanyi--retry-task 'button-data task 'follow-link t)
       (insert "\n\n")))))

(defun fanyi--report-task-error (task description)
  "Display TASK's failure DESCRIPTION and a retry action."
  (let ((name (url-host (url-generic-parse-url
                         (fanyi-request-url (fanyi--task-request task))))))
    (fanyi-log "%s: %s" name description)
    (when fanyi-verbose (message "%s: %s" name description)))
  (fanyi--write-task-error task description))

(defun fanyi--retry-task (task)
  "Retry a failed TASK in its original buffer using its request snapshot."
  (when (buffer-live-p (fanyi--task-result-buffer task))
    (with-current-buffer (fanyi--task-result-buffer task)
      ;; Replaced buttons and callbacks must never start a duplicate attempt.
      (when (and (derived-mode-p 'fanyi-mode) (memq task fanyi--tasks)
                 (fanyi--task-done task) (fanyi--task-failure task)
                 (= (fanyi--task-query-id task) fanyi--buffer-query-id))
        (fanyi--spawn (fanyi--task-request task) (current-buffer) task)))))

(defun fanyi--finish-task (task &optional failure)
  "Settle TASK once, reporting FAILURE if non-nil."
  (when (fanyi--task-active-p task)
    (setf (fanyi--task-done task) t
          (fanyi--task-failure task) failure)
    (fanyi--stop-task-timer task)
    (with-current-buffer (fanyi--task-result-buffer task)
      (if failure
          (progn
            (cl-incf fanyi--tasks-failed)
            (fanyi--report-task-error task failure))
        (cl-incf fanyi--tasks-completed)))
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
    (fanyi--write-task
     task (lambda () (fanyi-render-result (fanyi--task-request task) (fanyi--task-result task))))
    (fanyi--finish-task task)))

(defun fanyi--handle-response (task url status)
  "Parse and render TASK's response from URL with retrieval STATUS."
  (let ((response-buffer (current-buffer))
        (request (fanyi--task-request task)))
    (unwind-protect
        (when (fanyi--task-active-p task)
          (condition-case err
              (progn
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
                (let ((response (pcase (fanyi-request-api-type request)
                                  ('xml (libxml-parse-html-region (point) (point-max) url))
                                  ('json (json-parse-buffer))
                                  (_ (error "Unsupported response format")))))
                  (unless (catch 'not-found
                            (setf (fanyi--task-result task)
                                  (fanyi-parse-response request response))
                            t)
                    (error "No results found")))
                (fanyi--render-task task))
            (error (fanyi--finish-task task (error-message-string err)))
            (quit (fanyi--quit-task task err))))
      (fanyi--cleanup-response response-buffer)
      (fanyi--cleanup-response (fanyi--task-buffer task)))))

(defun fanyi--spawn (request result-buffer &optional replacing)
  "Start REQUEST in RESULT-BUFFER, optionally REPLACING a failed attempt."
  (let ((task (fanyi--make-task
               :query-id (buffer-local-value 'fanyi--buffer-query-id result-buffer)
               :request request :result-buffer result-buffer)))
    ;; Register before starting the request: a callback may run immediately.
    (with-current-buffer result-buffer
      (if (not replacing)
          (push task fanyi--tasks)
        (setf (fanyi--task-start task) (copy-marker (fanyi--task-start replacing) t)
              (fanyi--task-end task) (copy-marker (fanyi--task-end replacing))
              (fanyi--task-failure task) (fanyi--task-failure replacing))
        (setq fanyi--tasks (cl-substitute task replacing fanyi--tasks :test #'eq))
        (cl-decf fanyi--tasks-failed)
        (set-marker (fanyi--task-start replacing) nil)
        (set-marker (fanyi--task-end replacing) nil)))
    (condition-case err
        (progn
          (when replacing (fanyi--write-task-error task (fanyi--task-failure task)))
          (if (eq (fanyi-request-api-type request) 'browser)
              (fanyi--render-task task)
            (let ((url (fanyi-request-url request))
                  (url-request-method (fanyi-request-method request))
                  (url-request-extra-headers (fanyi-request-headers request))
                  (url-request-data (fanyi-request-body request)))
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
                      (run-at-time fanyi-request-timeout nil #'fanyi--timeout task)))))))
      (error
       (unwind-protect
           (fanyi--finish-task task (error-message-string err))
         (fanyi--cancel-task task)))
      (quit (fanyi--quit-task task err)))
    task))

(defun fanyi-format-header-line ()
  "Used as `header-line-format'."
  (let ((truncated (if (> (length fanyi-current-word) fanyi-headline-max-length)
                       (concat (substring fanyi-current-word 0 fanyi-headline-max-length) "...")
                     (or fanyi-current-word ""))))
    (concat "Translating "
            (propertize truncated 'face 'fanyi-word-face)
            " "
            (propertize (number-to-string (cl-count-if-not #'fanyi--task-done fanyi--tasks)) 'face 'fanyi-tasks-pending-face)
            " "
            (propertize (number-to-string fanyi--tasks-completed) 'face 'fanyi-tasks-completed-face)
            " "
            (propertize (number-to-string fanyi--tasks-failed) 'face 'fanyi-tasks-failed-face))))

(defun fanyi-eldoc-function (_callback &rest _ignored)
  "Return help at point for `fanyi-mode' ElDoc."
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

(defun fanyi-forward-button (count)
  "Move COUNT buttons forward, skipping hidden buttons and wrapping at the end.
With a negative COUNT, move backward.  A zero COUNT moves to the start of
the visible button at point."
  (interactive "p")
  (let ((button
         (save-excursion
           (let ((iterator (if (< count 0) #'previous-button #'next-button))
                 (boundary (if (< count 0) (point-max) (point-min)))
                 (button (button-at (point))))
             (dotimes (_ (abs count))
               (let (seen found)
                 (while (not found)
                   (setq button (or (funcall iterator (point))
                                    (funcall iterator boundary t)))
                   (unless (and button (not (memq (button-start button) seen)))
                     (user-error "No visible buttons"))
                   (goto-char (button-start button))
                   (push (point) seen)
                   (setq found (not (or (button-get button 'skip)
                                        (invisible-p (point))))))))
             (when (and button (not (invisible-p (button-start button)))) button)))))
    (when button
      (goto-char (button-start button))
      (when-let* ((help (button-get button 'help-echo))
                  (_ (stringp help)))
        (message "%s" help)))
    button))

(defun fanyi-backward-button (count)
  "Move COUNT visible buttons backward, wrapping at the beginning."
  (interactive "p")
  (fanyi-forward-button (- count)))

(defvar fanyi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] #'fanyi-forward-button)
    (define-key map (kbd "TAB") #'fanyi-forward-button)
    (define-key map [backtab] #'fanyi-backward-button)
    ;; Button text and overlays have their own keymaps.  Remapping also
    ;; covers those bindings without changing Emacs's global button map.
    (define-key map [remap forward-button] #'fanyi-forward-button)
    (define-key map [remap backward-button] #'fanyi-backward-button)
    (define-key map "q" #'quit-window)
    (define-key map "s" #'fanyi-dwim)
    (define-key map "w" #'fanyi-copy-query-word)
    (define-key map "h" #'fanyi-from-history)
    map)
  "Keymap for `fanyi-mode'.")

(defvar fanyi-mode-cycle-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] #'outline-cycle)
    (define-key map (kbd "TAB") #'outline-cycle)
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
  (add-hook 'eldoc-documentation-functions #'fanyi-eldoc-function nil t)

  ;; A result buffer owns its requests; closing or repurposing it ends them.
  (add-hook 'kill-buffer-hook #'fanyi--cancel-buffer-tasks nil t)
  (add-hook 'change-major-mode-hook #'fanyi--cancel-buffer-tasks nil t)

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
  "Translate WORD after trimming surrounding whitespace.
Record every accepted query in `fanyi-history', with the most recent first."
  (interactive (let* ((history-add-new-input nil)
                      (default (if (use-region-p)
                                   (buffer-substring-no-properties (region-beginning) (region-end))
                                 (thing-at-point 'word t)))
                      (prompt (if (stringp default)
                                  (format "Search Word (default \"%s\"): " default)
                                "Search Word: ")))
                 (list (read-string prompt nil 'fanyi-history default))))
  (unless (stringp word)
    (user-error "Query must be a string"))
  (setq word (string-trim (substring-no-properties word)))
  (when (string-empty-p word)
    (user-error "Query must not be empty"))
  (let ((requests (mapcar (lambda (provider)
                            (fanyi-make-request (fanyi--resolve-provider provider)
                                                (downcase word)))
                          fanyi-providers)))
    ;; Browser entries do not need either response parser.
    (when (and (seq-some (lambda (request) (eq (fanyi-request-api-type request) 'xml))
                         requests)
               (not (fboundp 'libxml-parse-html-region)))
      (error "This function requires Emacs to be compiled with libxml2"))
    (when (and (seq-some (lambda (request) (eq (fanyi-request-api-type request) 'json))
                         requests)
               (not (fboundp 'json-parse-buffer)))
      (error "This function requires Emacs to be compiled with libjson"))
    (let ((history-delete-duplicates t))
      ;; Also remove duplicates restored from history by older versions.
      (add-to-history 'fanyi-history word nil t))
    ;; Invalidate callbacks before cancelling requests, which can itself invoke
    ;; process sentinels.  `url-retrieve' is already asynchronous; no threads are
    ;; needed to start concurrent dictionary requests.
    (cl-incf fanyi--query-id)
    (when (buffer-live-p fanyi--active-buffer)
      (with-current-buffer fanyi--active-buffer
        (fanyi--cancel-buffer-tasks)))

    (let ((buf (get-buffer-create fanyi-buffer-name)))
      (setq fanyi--active-buffer buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          ;; Clear the previous search result.
          (erase-buffer)
          (fanyi-mode)
          (setq fanyi-current-word word
                fanyi--buffer-query-id fanyi--query-id)))

      (dolist (request requests)
        (fanyi--spawn request buf))
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
      (fanyi-dwim word)
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

(defun fanyi-bookmark-make-record ()
  "Create a bookmark for the current query word."
  `(,fanyi-current-word
    (fanyi-word . ,fanyi-current-word)
    (handler . fanyi-bookmark-jump)))

;;;###autoload
(defun fanyi-bookmark-jump (bookmark)
  "Default BOOKMARK handler for fanyi."
  (let ((word (or (alist-get 'fanyi-word (cdr bookmark))
                  ;; Older records stored the word only as the bookmark name.
                  (car bookmark))))
    (fanyi-dwim word)))

(put 'fanyi-bookmark-jump 'bookmark-handler-type "Fanyi")

(provide 'fanyi)
;;; fanyi.el ends here
