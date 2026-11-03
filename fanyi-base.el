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

(defun fanyi-play-sound (url)
  "Play URL via external program.
See `fanyi-sound-player'."
  (unless fanyi-sound-player
    (user-error "Set `fanyi-sound-player' first"))
  (when (string-empty-p url)
    (user-error "No voice found"))
  (condition-case err
      (if fanyi-sound-player-support-https
          (start-process fanyi-sound-player nil fanyi-sound-player url)
        ;; Download first for players without HTTPS support.  "-" selects
        ;; standard input; mplayer also needs a cache for this stream.
        (let (response finished)
          (setq response
                (url-retrieve
                 url
                 (lambda (status)
                   (let ((buffer (current-buffer)))
                     (unwind-protect
                         (condition-case err
                             (progn
                               (when (plist-member status :error)
                                 (error "Audio download failed: %S"
                                        (plist-get status :error)))
                               (goto-char (1+ url-http-end-of-headers))
                               (let ((proc
                                      (make-process
                                       :name "fanyi-player-process"
                                       :buffer nil
                                       :command `(,fanyi-sound-player "-"
                                                  ,@(fanyi--sound-player-options
                                                     fanyi-sound-player))
                                       :noquery t
                                       :connection-type 'pipe))
                                     delivered)
                                 (unwind-protect
                                     (progn
                                       (process-send-region proc (point) (point-max))
                                       (when (process-live-p proc)
                                         (process-send-eof proc))
                                       (setq delivered t))
                                   (when (and (not delivered) (process-live-p proc))
                                     (delete-process proc)))))
                           (error (fanyi--report-sound-error err)))
                       (setq finished t)
                       (fanyi--cleanup-response response)
                       (fanyi--cleanup-response buffer))))
                 nil t t))
          ;; A synchronous callback can finish before RESPONSE is assigned.
          (when finished
            (fanyi--cleanup-response response))))
    (error (fanyi--report-sound-error err))))

(defconst fanyi-buffer-name "*fanyi*"
  "The default name of translation buffer.")

(defconst fanyi-log-buffer-name "*fanyi-log*"
  "The default name of fanyi log buffer.")

(defvar fanyi-log-buffer-mtx (make-mutex)
  "The mutex for \"*fanyi-log*\" buffer.")

(defvar fanyi-buffer-mtx (make-mutex)
  "The mutex for \"*fanyi*\" buffer.")

(defvar fanyi-mode-font-lock-keywords-extra nil
  "Dicts can define their own font-lock keywords.")

(cl-defstruct (fanyi-provider (:constructor fanyi-provider-create))
  "Dictionary configuration shared across queries, without request state.
URL is a format string accepting an encoded query word.  API-TYPE is
`xml', `json', or `browser'.  PREPARE, if present, receives a fresh request
and may adjust it.  PARSE receives (response request) and returns a result;
RENDER receives (result request).  Browser providers do not need PARSE."
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
  "Render RESULT for REQUEST into the Fanyi buffer."
  (funcall (fanyi-request-render request) result request))

(defmacro fanyi-with-fanyi-buffer (&rest body)
  "Evaluate BODY with `*fanyi*' buffer temporarily current."
  (declare (indent 0))
  `(with-current-buffer (get-buffer-create fanyi-buffer-name)
     (save-excursion
       ;; Go to the end of buffer.
       (goto-char (point-max))
       (let ((inhibit-read-only t))
         ,@body))))

(defun fanyi-log (fmt &rest args)
  "Collect the messages into *fanyi-log* with non-nil `fanyi-log'.

The FMT and ARGS will be passed to `format-message'."
  (when fanyi-log
    (let ((timestamp (format-time-string "[%F %T]"))
          (text (apply #'format-message fmt args)))
      (with-mutex fanyi-log-buffer-mtx
        (with-current-buffer (get-buffer-create fanyi-log-buffer-name)
          (insert timestamp " " text "\n"))))))

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
