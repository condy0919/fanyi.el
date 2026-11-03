;;; fanyi-etymon.el --- Etymonline dictionary service -*- lexical-binding: t -*-

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
;; Open Etymonline entries in an external browser.

;;; Code:

(require 'fanyi-base)

(require 'button)
(require 'browse-url)

(defun fanyi-etymon-render (_result request)
  "Insert a browser link for REQUEST without fetching dictionary content."
  (let ((url (fanyi-request-url request)))
    (insert "# Etymonline\n\n")
    (insert-button "Open in browser"
                   'action (lambda (_) (browse-url-default-browser url))
                   'help-echo url
                   'follow-link t)
    (insert "\n\n")))

(defconst fanyi-etymon-provider
  (fanyi-provider-create :url "https://www.etymonline.com/word/%s"
                         :api-type 'browser
                         :render #'fanyi-etymon-render)
  "Etymonline browser entry configuration.")

(provide 'fanyi-etymon)
;;; fanyi-etymon.el ends here
