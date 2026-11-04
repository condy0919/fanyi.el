;;; fanyi-base.el --- Shared dictionary definitions and utilities -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022 Zhiwei Chen
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
;; Dictionary configuration, request snapshots, and shared utilities.

;;; Code:

(require 'url)
(require 'cl-lib)

;; Silence compile warning.
(defvar url-http-end-of-headers)

(defcustom fanyi-sound-player
  (or (executable-find "mpv")
      (executable-find "mplayer")
      (executable-find "mpg123"))
  "Program to play sound."
  :type 'string
  :group 'fanyi)

(defcustom fanyi-sound-player-support-https nil
  "Does `fanyi-sound-player' support https?

If non-nil, url will be passed to `fanyi-sound-player' directly.
Otherwise, `url-retrieve' first, the data will then be sent to
`fanyi-sound-player' process through a pipe."
  :type 'boolean
  :group 'fanyi)

(defcustom fanyi-sound-timeout 15
  "Maximum seconds to wait for an audio download.
Nil disables this timeout.  This does not limit playback duration or downloads
handled directly by `fanyi-sound-player-support-https'."
  :type '(choice (const :tag "No timeout" nil)
                 (number :tag "Seconds"))
  :group 'fanyi)

(defcustom fanyi-use-glyphs t
  "Non-nil means use glyphs when available."
  :type 'boolean
  :group 'fanyi)

(defcustom fanyi-verbose nil
  "Whether to make `fanyi-dwim' verbose."
  :type 'boolean
  :group 'fanyi)

(defcustom fanyi-log nil
  "Whether to enable log."
  :type 'boolean
  :group 'fanyi)

(defface fanyi-female-speaker-face
  '((t :foreground "#ec5e66"))
  "Face used for female speaker button."
  :group 'fanyi)

(defface fanyi-male-speaker-face
  '((t :foreground "#57a7b7"))
  "Face used for male speaker button."
  :group 'fanyi)

(defun fanyi-display-glyphs-p ()
  "Can we use glyphs instead of plain text?"
  (and fanyi-use-glyphs (display-images-p)))

(defun fanyi--sound-player-options (player)
  "Get the necessary options for PLAYER."
  (pcase player
    ((pred (string-match-p "mplayer"))
     '("-cache" "1024"))
    (_ nil)))

(defun fanyi--cleanup-response (buffer)
  "Kill BUFFER and any response buffers reached through redirects.
Completed HTTP connections are managed by `url-retrieve', so do not
explicitly delete their processes here."
  (let (seen)
    (while (and (buffer-live-p buffer) (not (memq buffer seen)))
      (push buffer seen)
      (let ((next (buffer-local-value 'url-redirect-buffer buffer)))
        (kill-buffer buffer)
        (setq buffer next)))))

(defun fanyi--report-sound-error (err)
  "Report audio download or playback error ERR."
  (let ((text (error-message-string err)))
    (fanyi-log "Audio: %s" text)
    (when fanyi-verbose
      (message "Fanyi audio: %s" text))))

(cl-defstruct (fanyi--sound-task (:constructor fanyi--make-sound-task))
  program options response process timer downloaded)

(defvar fanyi--sound-task nil
  "Current pronunciation download or player, superseded by the next click.")

(defun fanyi--stop-sound-timer (task)
  "Cancel TASK's download timeout."
  (when (fanyi--sound-task-timer task)
    (cancel-timer (fanyi--sound-task-timer task))
    (setf (fanyi--sound-task-timer task) nil)))

(defun fanyi--cancel-sound-task (task)
  "Invalidate TASK and release its player, download and timeout."
  (let ((inhibit-quit t))
    ;; Invalidate first: cancelling an HTTP process may invoke its callback.
    (when (eq task fanyi--sound-task) (setq fanyi--sound-task nil))
    (fanyi--stop-sound-timer task)
    (let ((process (fanyi--sound-task-process task))
          (buffer (fanyi--sound-task-response task)) seen)
      (setf (fanyi--sound-task-process task) nil
            (fanyi--sound-task-response task) nil)
      (when (processp process)
        ;; User cancellation is not a playback failure.
        (set-process-sentinel process #'ignore)
        (when (process-live-p process) (delete-process process)))
      (while (and (buffer-live-p buffer) (not (memq buffer seen)))
        (push buffer seen)
        (let ((next (buffer-local-value 'url-redirect-buffer buffer))
              (connection (get-buffer-process buffer)))
          (when connection (delete-process connection))
          (when (buffer-live-p buffer) (kill-buffer buffer))
          (setq buffer next))))))

;;;###autoload
(defun fanyi-stop-sound ()
  "Stop the current pronunciation, including any pending download."
  (interactive)
  (when fanyi--sound-task (fanyi--cancel-sound-task fanyi--sound-task)))

(defun fanyi--fail-sound (task err)
  "Cancel current TASK and report ERR once, ignoring superseded tasks."
  (when (eq task fanyi--sound-task)
    (fanyi--cancel-sound-task task)
    (fanyi--report-sound-error err))
  nil)

(defun fanyi--sound-timeout (task)
  "Stop TASK if its audio download has not completed."
  (when (and (eq task fanyi--sound-task) (not (fanyi--sound-task-downloaded task)))
    (fanyi--fail-sound task '(error "Audio download timed out"))))

(defun fanyi--start-sound-player (task input)
  "Start TASK's player with INPUT, returning the player process."
  (let* ((program (fanyi--sound-task-program task))
         (process
          (make-process
           :name "fanyi-player-process"
           :buffer nil
           :command (append (list program input) (fanyi--sound-task-options task))
           :noquery t
           :connection-type 'pipe
           :coding 'binary
           :sentinel
           (lambda (process event)
             (when (and (eq task fanyi--sound-task)
                        (memq (process-status process) '(exit signal)))
               (if (and (eq (process-status process) 'exit)
                        (zerop (process-exit-status process)))
                   (setq fanyi--sound-task nil)
                 (fanyi--fail-sound
                  task (list 'error (format "Player %s %s" program (string-trim event))))))))))
    (setf (fanyi--sound-task-process task) process)
    ;; A synchronous process notification may already have ended this task.
    (unless (eq task fanyi--sound-task) (fanyi--cancel-sound-task task))
    process))

(defun fanyi--handle-sound-response (task status)
  "Play current TASK's downloaded response, given retrieval STATUS."
  (let ((buffer (current-buffer))
        (response (fanyi--sound-task-response task)))
    ;; The HTTP exchange has ended; cancellation must no longer delete its
    ;; possibly reusable connection when parsing or playback fails.
    (setf (fanyi--sound-task-response task) nil)
    (unwind-protect
        (when (and (eq task fanyi--sound-task) (not (fanyi--sound-task-downloaded task)))
          (setf (fanyi--sound-task-downloaded task) t)
          (fanyi--stop-sound-timer task)
          (condition-case err
              (progn
                (when (plist-member status :error)
                  (error "Audio download failed: %S" (plist-get status :error)))
                (unless (and (bound-and-true-p url-http-end-of-headers)
                             (integer-or-marker-p url-http-end-of-headers)
                             (<= (point-min) url-http-end-of-headers)
                             (< url-http-end-of-headers (point-max)))
                  (error "Invalid audio HTTP response"))
                (goto-char (1+ url-http-end-of-headers))
                (let ((process (fanyi--start-sound-player task "-")))
                  (when (eq task fanyi--sound-task)
                    (process-send-region process (point) (point-max))
                    (when (and (eq task fanyi--sound-task) (process-live-p process))
                      (process-send-eof process)))))
            (error (fanyi--fail-sound task err))
            (quit
             (fanyi--cancel-sound-task task)
             (signal (car err) (cdr err)))))
      ;; Completed HTTP connections remain managed by url.el.
      (let ((inhibit-quit t))
        (fanyi--cleanup-response response)
        (fanyi--cleanup-response buffer)))))

(defun fanyi-play-sound (url)
  "Play URL, replacing the previous pronunciation or download.
See `fanyi-sound-player' and `fanyi-sound-timeout'.  Return the player process
if playback starts synchronously, otherwise nil."
  (unless fanyi-sound-player
    (user-error "Set `fanyi-sound-player' first"))
  (when (string-empty-p url)
    (user-error "No voice found"))
  (let* ((program (copy-sequence fanyi-sound-player))
         (task (fanyi--make-sound-task
                :program program
                :options (mapcar #'copy-sequence (fanyi--sound-player-options program)))))
    (fanyi-stop-sound)
    (setq fanyi--sound-task task)
    (condition-case err
        (if fanyi-sound-player-support-https
            (fanyi--start-sound-player task url)
          ;; "-" streams the downloaded bytes to the player's binary stdin.
          (setf (fanyi--sound-task-response task)
                (url-retrieve url (lambda (status) (fanyi--handle-sound-response task status))
                              nil t t))
          (cond
           ((fanyi--sound-task-downloaded task)
            ;; The callback can finish before url-retrieve returns its buffer.
            (fanyi--cleanup-response (fanyi--sound-task-response task))
            (setf (fanyi--sound-task-response task) nil))
           ((not (eq task fanyi--sound-task)) (fanyi--cancel-sound-task task))
           ((not (buffer-live-p (fanyi--sound-task-response task)))
            (error "Could not start audio download"))
           (fanyi-sound-timeout
            (setf (fanyi--sound-task-timer task)
                  (run-at-time fanyi-sound-timeout nil #'fanyi--sound-timeout task))))
          (fanyi--sound-task-process task))
      (error (fanyi--fail-sound task err))
      (quit
       (fanyi--cancel-sound-task task)
       (signal (car err) (cdr err))))))

(defconst fanyi-buffer-name "*fanyi*"
  "The default name of translation buffer.")

(defconst fanyi-log-buffer-name "*fanyi-log*"
  "The default name of fanyi log buffer.")

(defvar fanyi-log-buffer-mtx (make-mutex)
  "The mutex for \"*fanyi-log*\" buffer.")

(defvar fanyi-mode-font-lock-keywords-extra nil
  "Dicts can define their own font-lock keywords.")

(cl-defstruct (fanyi-provider (:constructor fanyi-provider-create))
  "Dictionary configuration shared across queries, without request state.
URL is a format string accepting an encoded query word.  API-TYPE is
`xml', `json', or `browser'.  PREPARE, if present, receives a fresh request
and may adjust it.  PARSE receives (response request) and returns a result;
RENDER receives (result request) and inserts at point in the current buffer.
Browser providers do not need PARSE."
  url sound-url (method "GET") headers body api-type prepare parse render)

(cl-defstruct (fanyi-request (:constructor fanyi-request-create))
  "A single query's resolved parameters and callbacks.
WORD is the query text; URL is the final encoded URL.  The result is stored
separately, so parsing and rendering do not modify this request."
  word url sound-url method headers body api-type parse render)

(defun fanyi--copy-request-value (value)
  "Copy strings and lists in request configuration VALUE."
  (cond ((consp value)
         (cons (fanyi--copy-request-value (car value))
               (fanyi--copy-request-value (cdr value))))
        ((stringp value) (copy-sequence value))
        (t value)))

(defun fanyi-make-request (provider word)
  "Create a request for WORD from PROVIDER without changing its configuration.
Copy mutable HTTP settings and capture callbacks before invoking PREPARE.
Subsequent provider configuration changes apply only to future requests."
  (unless (and (fanyi-provider-p provider)
               (stringp (fanyi-provider-url provider))
               (memq (fanyi-provider-api-type provider) '(xml json browser))
               (functionp (fanyi-provider-render provider))
               (or (eq (fanyi-provider-api-type provider) 'browser)
                   (functionp (fanyi-provider-parse provider)))
               (or (null (fanyi-provider-prepare provider))
                   (functionp (fanyi-provider-prepare provider))))
    (user-error "Invalid dictionary provider: %S" provider))
  (let ((request
         (fanyi-request-create
          :word (copy-sequence word)
          :url (format (fanyi-provider-url provider) (url-hexify-string word))
          :sound-url (fanyi--copy-request-value (fanyi-provider-sound-url provider))
          :method (fanyi--copy-request-value (fanyi-provider-method provider))
          :headers (fanyi--copy-request-value (fanyi-provider-headers provider))
          :body (fanyi--copy-request-value (fanyi-provider-body provider))
          :api-type (fanyi-provider-api-type provider)
          :parse (fanyi-provider-parse provider)
          :render (fanyi-provider-render provider))))
    (when (fanyi-provider-prepare provider)
      (funcall (fanyi-provider-prepare provider) request))
    request))

(define-error 'fanyi-parse-error "Could not parse dictionary response")

(defun fanyi-parse-response (request response)
  "Parse RESPONSE for REQUEST and return independent dictionary data."
  (funcall (fanyi-request-parse request) response request))

(defun fanyi-render-result (request result)
  "Insert RESULT for REQUEST at point in the current buffer.
The caller supplies the writable destination, insertion point and transaction."
  (funcall (fanyi-request-render request) result request))

(defun fanyi-log (fmt &rest args)
  "Collect the messages into *fanyi-log* with non-nil `fanyi-log'.

The FMT and ARGS will be passed to `format-message'."
  (when fanyi-log
    (let ((timestamp (format-time-string "[%F %T]"))
          (text (apply #'format-message fmt args)))
      (with-mutex fanyi-log-buffer-mtx
        (with-current-buffer (get-buffer-create fanyi-log-buffer-name)
          (let ((inhibit-read-only t))
            (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-max))
                (insert timestamp " " text "\n")))))))))

(defun fanyi-user-error (fmt &rest args)
  "Signal a user error by passing FMT and ARGS to `user-error'.
If `fanyi-verbose' is nil (default), message won't be displayed.
If `fanyi-log' is non-nil, the message will also be collected in
\"*fanyi-log*\" buffer."
  (apply #'fanyi-log fmt args)
  (if fanyi-verbose
      (apply #'user-error fmt args)
    (signal 'user-error nil)))

(provide 'fanyi-base)
;;; fanyi-base.el ends here
