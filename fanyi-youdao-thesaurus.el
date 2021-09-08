;;; fanyi.el --- Youdao thesaurus service -*- lexical-binding: t -*-

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
;; Youdao thesaurus service.

;;; Code:

(require 'fanyi-base)

(require 's)
(require 'json)
(require 'seq)
(require 'cl-lib)

(defcustom fanyi-youdao-thesaurus-max-count 10
  "Max count of entries displayed."
  :type 'integer
  :group 'fanyi)

(defcustom fanyi-youdao-thesaurus-indent 2
  "Default indent for explanation."
  :type 'integer
  :group 'fanyi)

(defclass fanyi-youdao-thesaurus-service (fanyi-base-service)
  ((entries :initarg :entries
            :type list
            :documentation "Thesaurus entries of the word."))
  "The Youdao thesaurus service.")

(defclass fanyi-youdao-thesaurus-entry ()
  ((entry :initarg :entry
          :type string
          :documentation "Thesaurus.")
   (explain :initarg :explain
            :type string
            :documentation "The explanation of an entry."))
  "An entry of the word.")

;; Silence unknown slots warning.
(eieio-declare-slots :entries)
(eieio-declare-slots :entry :explain)

(cl-defmethod fanyi-parse-from ((this fanyi-youdao-thesaurus-service) js)
  "Complete the fields of THIS from JS json.
A 'not-found exception will be thrown if there is no result."
  ;; Nothing is found or the json is malformed.
  ;;
  ;; An example:
  ;;
  ;; ```js
  ;; {
  ;;     "result":
  ;;     {
  ;;         "code": 200
  ;;     },
  ;;     "data":
  ;;     {
  ;;         "entries":
  ;;         [
  ;;             {
  ;;                 "explain": "vi. 累积",
  ;;                 "entry": "accumulate"
  ;;             }
  ;;         ]
  ;;     }
  ;; }
  ;; ```
  (unless (when-let* ((result (gethash "result" js))
                      (code (gethash "code" result))
                      (data (gethash "data" js)))
            (and (equal code 200)
                 (gethash "entries" data)))
    (throw 'not-found nil))
  (let ((entries (gethash "entries" (gethash "data" js))))
    (oset this :entries
          (cl-loop for entry across entries
                   for ent = (gethash "entry" entry)
                   for explain = (gethash "explain" entry)
                   collect (fanyi-youdao-thesaurus-entry :entry ent
                                                         :explain explain)))))

(cl-defmethod fanyi-render ((this fanyi-youdao-thesaurus-service))
  "Render THIS page into a buffer named `fanyi-buffer-name'.
It's NOT thread-safe, caller should hold `fanyi-buffer-mtx'
before calling this method."
  (fanyi-with-fanyi-buffer
   ;; The headline about Youdao thesaurus service.
   (insert "# 有道同义词词典 \n\n")
   (cl-loop for entry in (oref this :entries)
            for ent = (oref entry :entry)
            for explain = (oref entry :explain)
            do (insert "- " ent "\n")
            do (insert (s-repeat fanyi-youdao-thesaurus-indent " ") explain)
            do (insert "\n"))
   (insert "\n")))

(defconst fanyi-youdao-thesaurus-provider
  (fanyi-youdao-thesaurus-service :word "dummy"
                                  :url (concat "https://dict.youdao.com/suggest?q=%s&doctype=json&num="
                                               (int-to-string fanyi-youdao-thesaurus-max-count))
                                  :sound-url "unused"
                                  :api-type 'json)
  "Youdao thesaurus service instance.")

(provide 'fanyi-youdao-thesaurus)
;;; fanyi-youdao-thesaurus.el ends here
