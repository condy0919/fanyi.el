;;; fanyi.el --- Youdao translation service -*- lexical-binding: t -*-

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
;; Youdao translation service.

;;; Code:

(require 'fanyi-base)

(require 's)
(require 'cl-lib)

(defcustom fanyi-youdao-translate-lang "AUTO"
  "The source lang and target lang."
  :type '(radio (const :tag "自动检测语言" "AUTO")
                (const :tag "中文 -> 英语" "ZH_CN2EN")
                (const :tag "英语 -> 中文" "EN2ZH_CN")
                (const :tag "中文 -> 日语" "ZH_CN2JA")
                (const :tag "日语 -> 中文" "JA2ZH_CN")
                (const :tag "中文 -> 韩语" "ZH_CN2KR")
                (const :tag "韩语 -> 中文" "KR2ZH_CN"))
  :group 'fanyi)

(defclass fanyi-youdao-translate-service (fanyi-base-service)
  ((text :initarg :text
         :type string
         :documentation "The translated text."))
  "The Youdao translation service.")

;; Silence unknown slots warning.
(eieio-declare-slots :text)

(cl-defmethod fanyi-set-query-word ((this fanyi-youdao-translate-service) query)
  "Set QUERY to THIS."
  (oset this :word query)
  (oset this :body (url-build-query-string `(("i" ,query)))))

(cl-defmethod fanyi-parse-from ((this fanyi-youdao-translate-service) js)
  "Complete the fields of THIS from JS json."
  (unless (zerop (gethash "errorCode" js))
    (throw 'not-found nil))
  (oset this :text
        (s-join "\n" (cl-loop for line across-ref (gethash "translateResult" js)
                              for target = (gethash "tgt" (aref line 0))
                              collect target))))

(cl-defmethod fanyi-render ((this fanyi-youdao-translate-service))
  "Render THIS page into a buffer named `fanyi-buffer-name'.
It's NOT thread-safe, caller should hold `fanyi-buffer-mtx'
before calling this method."
  (fanyi-with-fanyi-buffer
   ;; The headline about Youdao translate.
   (insert "# 有道翻译\n\n")
   ;; The source text. '\n' character may be present.
   (cl-loop for line in (s-split "\n" (oref this :word) :omit-nulls)
            do (insert "> " line "\n")
            finally do (insert "\n"))
   ;; The translated text.
   (insert (oref this :text) "\n\n")))

(defconst fanyi-youdao-translate-provider
  (fanyi-youdao-translate-service :word "used to save the source text"
                                  :url (concat "https://fanyi.youdao.com/translate?doctype=json&type=" fanyi-youdao-translate-lang)
                                  :sound-url "unused"
                                  :method "POST"
                                  :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
                                  :body "placeholder"
                                  :api-type 'json)
  "Youdao translation service instance.")

(provide 'fanyi-youdao-translate)
;;; fanyi-youdao-translate.el ends here
