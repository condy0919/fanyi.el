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

(defvar fanyi-etymon--consecutive-newlines nil)

(cl-defmethod fanyi-parse-from ((this fanyi-etymon-service) dom)
  "Complete the fields of THIS from DOM tree.
If the definitions of word are not found, http 404 error is
expected.

The /italic/ and *bold* styles are borrowed from `org-mode',
while the quote style is from mailing list."
  (let ((defs (dom-by-class dom "^\\(word--C9UPa\\)$")))
    (oset this :definitions
          (cl-loop for def in defs
                   for title = (dom-text (dom-by-class def "word__name--TTbAA"))
                   for details = (dom-children (dom-by-class def "^\\(word__defination--2q7ZH\\)$"))
                   collect (list title
                                 (seq-mapcat
                                  (lambda (arg)
                                    (pcase arg
                                      ('(p nil)
                                       (prog1
                                           ;; Eliminate extra '\n\n' if it happens two or more times.
                                           (if fanyi-etymon--consecutive-newlines
                                               ""
                                             "\n\n")
                                         (setq fanyi-etymon--consecutive-newlines t)))
                                      (_
                                       (cl-assert (> (length arg) 2))
                                       (setq fanyi-etymon--consecutive-newlines nil)
                                       (seq-concatenate
                                        'list
                                        (when (equal (car arg) 'blockquote)
                                          '("> "))
                                        (seq-map (lambda (arg)
                                                   (cond ((stringp arg) arg)
                                                         ((dom-by-class arg "foreign notranslate")
                                                          (cond ((dom-by-tag arg 'strong)
                                                                 (concat "*" (dom-texts arg) "*"))
                                                                (t
                                                                 (concat "/" (dom-text arg) "/"))))
                                                         ((dom-by-class arg "crossreference notranslate")
                                                          (list (dom-text arg) 'button (dom-text arg)))
                                                         (t
                                                          (s-replace-all '(("\u00a0" . "")
                                                                           ("\u0009" . ""))
                                                                         (dom-text arg)))))
                                                 (cddr arg))))))
                                  details))))))

(cl-defmethod fanyi-render ((this fanyi-etymon-service))
  "Render THIS page into a buffer named `fanyi-buffer-name'.
It's NOT thread-safe, caller should hold `fanyi-buffer-mtx'
before calling this method."
  (fanyi-with-fanyi-buffer
   ;; The headline about Etymonline service.
   (insert "# Etymonline\n\n")
   (cl-loop for i in (oref this :definitions)
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
