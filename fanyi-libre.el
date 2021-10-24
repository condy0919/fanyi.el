;;; fanyi-libre.el --- LibreTranslate service -*- lexical-binding: t -*-

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
;; LibreTranslate service.
;;
;; Visit https://libretranslate.com/ for more information.

;;; Code:

(require 'fanyi-base)

(require 's)
(require 'seq)
(require 'json)
(require 'cl-lib)

(defconst fanyi-libre-supported-lang-alist
  '(("English"    . "en")
    ("Arabic"     . "ar")
    ("Chinese"    . "zh")
    ("French"     . "fr")
    ("German"     . "de")
    ("Hindi"      . "hi")
    ("Indonesian" . "id")
    ("Irish"      . "ga")
    ("Italian"    . "it")
    ("Japanese"   . "ja")
    ("Korean"     . "ko")
    ("Polish"     . "pl")
    ("Portuguese" . "pt")
    ("Russian"    . "ru")
    ("Spanish"    . "es")
    ("Turkish"    . "tr")
    ("Vietnamese" . "vi")))

(defcustom fanyi-libre-service-url "https://translate.argosopentech.com/translate"
  "The URL of LibreTranslate instance.

Host your own server or use other online services.

https://github.com/LibreTranslate/LibreTranslate#mirrors"
  :type 'string
  :group 'fanyi)

(defcustom fanyi-libre-source-lang "auto"
  "Source language.
A string designating a language supported by LibreTranslate. Set
this variable to \"auto\" if you want LibreTranslate to auto
detect the source language. See
`fanyi-libre-supported-lang-alist' for the list of all supported
languages."
  :type `(radio ,@(seq-map (lambda (lang)
                             `(const :tag ,(car lang) ,(cdr lang)))
                           fanyi-libre-supported-lang-alist)
                (const :tag "Auto Detect (Experimental)" "auto"))
  :group 'fanyi)

(defcustom fanyi-libre-target-lang "zh"
  "Target language.
A string designating a language supported by LibreTranslate. See
`fanyi-libre-supported-lang-alist' for the list of all supported
languages."
  :type `(radio ,@(seq-map (lambda (lang)
                             `(const :tag ,(car lang) ,(cdr lang)))
                           fanyi-libre-supported-lang-alist))
  :group 'fanyi)

(defclass fanyi-libre-service (fanyi-base-service)
  ((text :initarg :text
         :type string
         :documentation "Translated text."))
  "The LibreTranslate service.")

;; Silence unknown slots warnings.
(eieio-declare-slots :text)

(cl-defmethod fanyi-set-query-word ((this fanyi-libre-service) query)
  "Set QUERY to THIS."
  ;; `url-retrieve' doesn't support multibytes payload. So convert some
  ;; frequently used multibytes into the equivalent ASCII.
  (let ((bytes (s-replace-all '(("‘" . "\'")
                                ("’" . "\'")
                                ("“" . "\"")
                                ("”" . "\""))
                              query)))
    (oset this :word query)
    (oset this :body (json-encode `(("q"      . ,bytes)
                                    ("source" . ,fanyi-libre-source-lang)
                                    ("target" . ,fanyi-libre-target-lang)
                                    ("format" . "text"))))))

(cl-defmethod fanyi-parse-from ((this fanyi-libre-service) js)
  "Complete the fields of THIS from JS json."
  (oset this :text (gethash "translatedText" js)))

(cl-defmethod fanyi-render ((this fanyi-libre-service))
  "Render THIS page into a buffer named `fanyi-buffer-name'.
It's NOT thread-safe, caller should hold `fanyi-buffer-mtx'
before calling this method."
  (fanyi-with-fanyi-buffer
   ;; The headline about LibreTranslate service.
   (insert "# LibreTranslate\n\n")
   ;; [From] -> [Target]
   (insert (format "[%s] -> [%s]" fanyi-libre-source-lang fanyi-libre-target-lang) "\n\n")
   ;; The source text. '\n' character may be present.
   (cl-loop for line in (s-split "\n" (oref this :word) :omit-nulls)
            do (insert "> " line "\n")
            finally do (insert "\n"))
   ;; The translated text.
   (insert (oref this :text) "\n\n")))

(defconst fanyi-libre-provider
  (fanyi-libre-service :word "used to save the source text"
                       :url fanyi-libre-service-url
                       :sound-url "unused"
                       :method "POST"
                       :headers '(("Content-Type" . "application/json"))
                       :body "placeholder"
                       :api-type 'json)
  "LibreTranslate service instance.")

(provide 'fanyi-libre)
;;; fanyi-libre.el ends here
