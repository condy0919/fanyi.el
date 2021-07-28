;;; fanyi.el --- English-Chinese translator for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2021 Zhiwei Chen
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/condy0919/fanyi.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))

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



;;; Code:

(require 'dom)
(require 'json)
(require 'thingatpt)
(require 'url)

;; Silence compile warnings.
(defvar url-http-end-of-headers)

(defgroup fanyi nil
  "English-Chinese translator for Emacs."
  :prefix "fanyi-"
  :group 'tools
  :link '(url-link "https://github.com/condy0919/fanyi.el"))

(defconst haici-url-template "https://dict.cn/%s"
  "HaiCi dictionary translation service url.")

(defun fanyi-dwim (word)
  "Translate WORD."
  (interactive
   (let* ((default (if (use-region-p)
                       (buffer-substring-no-properties (region-beginning) (region-end))
                     (thing-at-point 'word t)))
          (prompt (if (stringp default)
                      (format "Word (default \"%s\"): " default)
                    "Word: ")))
     (list (read-string prompt nil nil default))))
  (let ((url (format haici-url-template word)))
    ;; libxml2 is required.
    (when (not (fboundp 'libxml-parse-html-region))
      (error "This function requires Emacs to be compiled with libxml2"))
    (url-retrieve url
                  (lambda (status)
                    (when (or (not status) (plist-member status :error))
                      (user-error "Something went error.\n\n%s" (pp-to-string (plist-get status :error))))
                    (goto-char url-http-end-of-headers)
                    (let ((dom (libxml-parse-html-region (point) (point-max) url)))
                      (setq xxx dom))))))


(provide 'fanyi)

;;; fanyi.el ends here
