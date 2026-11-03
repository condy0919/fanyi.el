;;; fanyi-etymon.el --- Etymonline dictionary service -*- lexical-binding: t -*-

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
;; Open Etymonline entries in an external browser.

;;; Code:

(require 'fanyi-base)

(require 'cl-lib)
(require 'button)
(require 'browse-url)

(defclass fanyi-etymon-service (fanyi-base-service) ()
  "An Etymonline entry opened in an external browser.")

(cl-defmethod fanyi-render ((this fanyi-etymon-service))
  "Insert a browser link for THIS without fetching dictionary content."
  (let ((url (format (oref this url) (oref this word))))
    (fanyi-with-fanyi-buffer
      (insert "# Etymonline\n\n")
      (insert-button "Open in browser"
                     'action (lambda (_) (browse-url-default-browser url))
                     'help-echo url
                     'follow-link t)
      (insert "\n\n"))))

(defconst fanyi-etymon-provider
  (fanyi-etymon-service :word "dummy"
                        :url "https://www.etymonline.com/word/%s"
                        :api-type 'browser)
  "Etymonline browser entry service instance.")

(provide 'fanyi-etymon)
;;; fanyi-etymon.el ends here
