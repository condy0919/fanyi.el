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

(defcustom fanyi-use-glyphs t
  "Non-nil means use glyphs when available."
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
  "Get the required options for PLAYER in fanyi usage situation."
  (pcase player
    ((pred (string-match-p "mplayer"))
     '("-cache" "1024"))
    (_ nil)))

(defun fanyi-play-sound (url)
  "Download URL then play it via external program.
See `fanyi-sound-player'."
  (unless fanyi-sound-player
    (user-error "Set `fanyi-sound-player' first"))
  (when (string-empty-p url)
    (user-error "Can't play it"))
  ;; Some programs, e.g. mpg123, can't play https files. So we download them
  ;; then play via `fanyi-sound-player'.
  ;;
  ;; "-" stands for standard input.
  ;;
  ;; mplayer needs an additional option, otherwise will get an error:
  ;;
  ;; ```text
  ;; Cannot seek backward in linear streams!
  ;; ```
  (url-retrieve url (lambda (status)
                      (cl-block nil
                        ;; Something went wrong, but we dont' care.
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
                t))

(defconst fanyi-buffer-name "*fanyi*"
  "The default name of translation buffer.")

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
              :documentation "Dictionary sound url."))
  "The base class of dictionary service."
  :abstract t)

;; Silence unknown slots warning.
(eieio-declare-slots :word :url :sound-url)

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

(provide 'fanyi-base)
;;; fanyi-base.el ends here
