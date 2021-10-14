;;; ol-fanyi.el --- Store query word from fanyi-mode -*- lexical-binding: t -*-

;; Copyright (C) 2021 Zhiwei Chen
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/condy0919/fanyi.el
;; Version: 0.1.0
;; Package-Requires: ((fanyi "0.1.0"))

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
;; When this module is active, `org-store-link' in fanyi buffer stores a link to
;; the current query word of the fanyi buffer.

;;; Code:

(require 'ol)
(require 'fanyi)

(org-link-set-parameters "fanyi"
                         :follow #'fanyi-org-open-link
                         :store #'fanyi-org-store-link)

(defun fanyi-org-open-link (word _)
  "Query WORD with `fanyi-dwim' in the current buffer."
  (fanyi-dwim word))

(defun fanyi-org-store-link ()
  "Store a link to the query word of current fanyi buffer."
  (when (eq major-mode 'fanyi-mode)
    (org-link-store-props
     :type "fanyi"
     :link (concat "fanyi:" fanyi-current-word)
     :description fanyi-current-word)))

(provide 'ol-fanyi)
;;; ol-fanyi.el ends here
