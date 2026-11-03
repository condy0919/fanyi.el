;;; fanyi-etymon.el --- Etymonline dictionary service -*- lexical-binding: t -*-

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
;; Etymonline dictionary service.

;;; Code:

(require 'fanyi-base)

(require 's)
(require 'subr-x)
(require 'dom)
(require 'seq)
(require 'cl-lib)
(require 'button)

(defclass fanyi-etymon-service (fanyi-base-service)
  ((definitions :initarg :definitions
                :type list
                :documentation "List of (word . def).
Where def could be a list of string/(string 'face face)/(string 'button data)."))
  "The etymonline service.")

;; Silence unknown slots warning.
(eieio-declare-slots :definitions)

;; Silence compile warning.
(declare-function fanyi-dwim "fanyi")

(defun fanyi-etymon--content-p (content)
  "Return non-nil when CONTENT contains text or a nonempty query button."
  (seq-some (lambda (part)
              (string-match-p "\\S-" (if (stringp part) part (car part))))
            content))

(defun fanyi-etymon--parse-node (node)
  "Convert a definition NODE into text and query buttons.
Unknown formatting elements retain their text; empty paragraphs keep spacing."
  (if (stringp node)
      (list (s-replace-all '(("\u00a0" . " ") ("\u0009" . " ")) node))
    (let ((tag (dom-tag node))
          (classes (split-string (or (dom-attr node 'class) ""))))
      (cond
       ((memq tag '(script style)) nil)
       ((eq tag 'br) '("\n"))
       ((member "foreign" classes)
        (let ((text (s-replace-all '(("\u00a0" . " ") ("\u0009" . " "))
                                   (dom-texts node ""))))
          (unless (string-empty-p (string-trim text))
            (let ((marker (if (dom-by-tag node 'strong) "*" "/")))
              (list (concat marker text marker))))))
       ((member "crossreference" classes)
        (let ((text (string-trim (dom-texts node ""))))
          (unless (string-empty-p text)
            (list (list text 'button text)))))
       (t
        (let ((content (seq-mapcat #'fanyi-etymon--parse-node (dom-children node))))
          ;; Empty paragraphs are separators in some responses.  Retain their
          ;; whitespace, but do not turn empty quotes into visible content.
          (append (when (and (eq tag 'blockquote)
                             (fanyi-etymon--content-p content))
                    '("> "))
                  content
                  (when (memq tag '(p blockquote)) '("\n\n")))))))))

(cl-defmethod fanyi-parse-from ((this fanyi-etymon-service) dom)
  "Complete THIS from DOM, rejecting pages without usable definitions.
The /italic/ and *bold* styles are borrowed from `org-mode',
while the quote style is from mailing lists."
  (oset this definitions
        (cl-loop for def in (dom-by-class dom "^\\(word--C9UPa\\)$")
                 for title = (string-trim (dom-texts (dom-by-class def "word__name--TTbAA") ""))
                 for details = (dom-by-class def "^\\(word__defination--2q7ZH\\)$")
                 for content = (seq-mapcat #'fanyi-etymon--parse-node (dom-children details))
                 when (and (not (string-empty-p title))
                           (fanyi-etymon--content-p content))
                 collect (list title content)))
  (unless (oref this definitions)
    (signal 'fanyi-parse-error '("Etymonline: missing or empty definitions"))))

(cl-defmethod fanyi-render ((this fanyi-etymon-service))
  "Render THIS page into a buffer named `fanyi-buffer-name'.
It's NOT thread-safe, caller should hold `fanyi-buffer-mtx'
before calling this method."
  (fanyi-with-fanyi-buffer
    ;; The headline about Etymonline service.
    (insert "# Etymonline\n\n")
    (cl-loop for i in (oref this definitions)
             for (word def) = i
             do (insert "## " word "\n\n")
             do (seq-do (lambda (arg)
                          (pcase arg
                            (`(,s button ,word)
                             (insert-button s
                                            'action #'fanyi-dwim
                                            'button-data word
                                            'follow-link t))
                            (s
                             (insert s))))
                        def)
             do (while (equal (char-before) ?\n)
                  (delete-char -1))
             do (insert "\n\n"))))

(defconst fanyi-etymon-provider
  (fanyi-etymon-service :word "dummy"
                        :url "https://www.etymonline.com/word/%s"
                        :sound-url "unused"
                        :api-type 'xml)
  "Etymonline dictionary service instance.")

(provide 'fanyi-etymon)
;;; fanyi-etymon.el ends here
