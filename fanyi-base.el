;;; fanyi.el --- The base class of all dictionaries -*- lexical-binding: t -*-

;; Copyright (C) 2021 Zhiwei Chen
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/condy0919/fanyi.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

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
;; The base class of all dictionaries.

;;; Code:

(require 'url)
(require 'eieio)

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
`fanyi-sound-player' through a pipe."
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

(defun fanyi-play-sound (url)
  "Play URL via external program.
See `fanyi-sound-player'."
  (unless fanyi-sound-player
    (user-error "Set `fanyi-sound-player' first"))
  (when (string-empty-p url)
    (user-error "Can't play it"))
  (if fanyi-sound-player-support-https
      (start-process fanyi-sound-player nil fanyi-sound-player url)
    ;; Some programs, e.g. mpg123, can't play https files. So we download then
    ;; play them via `fanyi-sound-player'.
    ;;
    ;; "-" stands for standard input, which is an UNIX convention.
    ;;
    ;; mplayer needs an additional option, or exits with an error:
    ;;
    ;;     Cannot seek backward in linear streams!
    (url-retrieve url (lambda (status)
                        (cl-block nil
                          ;; Something went wrong, but we don't care.
                          (when (or (not status) (plist-member status :error))
                            (cl-return))
                          ;; Move point to the real http content. Plus 1 for '\n'.
                          (goto-char (1+ url-http-end-of-headers))
                          (let ((proc (make-process :name "fanyi-player-process"
                                                    :buffer nil
                                                    :command `(,fanyi-sound-player "-" ,@(fanyi--sound-player-options fanyi-sound-player))
                                                    :noquery t
                                                    :connection-type 'pipe)))
                            (process-send-region proc (point) (point-max))
                            (when (process-live-p proc)
                              (process-send-eof proc)))))
                  nil
                  t
                  t)))

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

(defclass fanyi-base-service ()
  ((word :initarg :word
         :type string
         :protection :protected
         :documentation "The query word.")
   (url :initarg :url
        :type string
        :protection :protected
        :documentation "Dictionary translation url.")
   (sound-url :initarg :sound-url
              :type string
              :protection :protected
              :documentation "Dictionary sound url.")
   (api-type :initarg :api-type
             :type symbol
             :protection :protected
             :documentation "API type. Currently it chould be 'xml or 'json."))
  "The base class of dictionary service."
  :abstract t)

;; Silence unknown slots warning.
(eieio-declare-slots :word :url :sound-url :api-type)

(cl-defmethod fanyi-parse-from ((this fanyi-base-service) _dom)
  "Implement your own `fanyi-parse-from' for THIS class."
  (error "Implement `fanyi-parse-from' for class '%s'" (eieio-object-class-name this)))

(cl-defmethod fanyi-render ((this fanyi-base-service))
  "Implement your own `fanyi-render' for THIS class."
  (error "Implement `fanyi-render' for class '%s'" (eieio-object-class-name this)))

(defmacro fanyi-with-fanyi-buffer (&rest body)
  "Evaluate BODY with `*fanyi*' buffer temporarily current."
  `(with-current-buffer (get-buffer-create ,fanyi-buffer-name)
     (save-excursion
       ;; Go to the end of buffer.
       (goto-char (point-max))
       (let ((inhibit-read-only t))
         ,@body))))

(defun fanyi-log (fmt &rest args)
  "When the option `fanyi-log' is non-nil, collect the message by
passing FMT and ARGS to `format-message'."
  (when fanyi-log
    (let ((timestamp (format-time-string "[%F %T]"))
          (text (format-message fmt args)))
      (with-mutex fanyi-log-buffer-mtx
        (with-current-buffer (get-buffer-create fanyi-log-buffer-name)
          (insert timestamp " " text "\n"))))))

(defun fanyi-user-error (fmt &rest args)
  "Signal a user error by passing FMT and ARGS to `user-error'.
If `fanyi-verbose' is nil (defualt), message won't be displayed.
If `fanyi-log' is non-nil, the message will also be collected in
\"*fanyi-log*\" buffer."
  (fanyi-log fmt args)
  (if fanyi-verbose
      (user-error fmt args)
    (user-error "")))

(provide 'fanyi-base)
;;; fanyi-base.el ends here
