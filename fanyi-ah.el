;;; fanyi.el --- American Heritage dictionary service -*- lexical-binding: t -*-

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
;; American Heritage dictionary service.

;;; Code:

(require 'fanyi-base)

(require 's)
(require 'dom)
(require 'seq)
(require 'cl-lib)
(require 'button)
(require 'generator)

(defclass fanyi-ah-service (fanyi-base-service)
  ((syllable :initarg :syllable
             :type string
             :documentation "Syllable of the word.")
   (sound :initarg :sound
          :type string
          :documentation "Sound uri.")
   (pronunciation :initarg :pronunciation
                  :type string
                  :documentation "Pronunciation.")
   (paraphrases :initarg :paraphrases
                :type list
                :documentation "List of (pos . examples . paraphrase).
Where examples could be nil, paraphrase can be a string or a list of strings or a list of lists of strings.")
   (idioms :initarg :idioms
           :type list
           :documentation "Idioms of the word.")
   (synonyms :initarg :synonyms
             :type list
             :documentation "Synonyms of the word."))
  "The American Heritage dictionary service.")

;; Silence unknown slots warning.
(eieio-declare-slots :syllable :sound :pronunciation :paraphrases :idioms :synonyms)

;; Silence compile warning.
(autoload 'fanyi-dwim "fanyi")

(iter-defun fanyi--parse-ah-pseg (dom)
  "Helper function to parse the paraphrase segment DOM of American
Heritage dictionary."
  ;; Order of `cond' matters.
  (cond ((dom-by-class dom "ds-single")
         (let ((ds-single (dom-by-class dom "ds-single")))
           ;; Seems a space always exists at front, so a space is emitted for
           ;; indentation.
           (iter-yield " ")
           (cl-loop for child in (dom-children ds-single)
                    do (pcase child
                         ((pred stringp)
                          (iter-yield child))
                         (`(span nil ,s)
                          (iter-yield s))
                         (`(font nil (i nil ,s))
                          (iter-yield (list s 'face 'italic)))
                         (`(a ,_href (span nil ,s1) ,s2)
                          (iter-yield s1)
                          (iter-yield (list s2 'button s2)))
                         (_ (iter-yield ""))))))
        ((dom-by-class dom "sds-list")
         ;; TODO
         )
        ((dom-by-class dom "ds-list")
         (let ((ds-list (dom-by-class dom "ds-list")))
           (cl-loop for ds in ds-list
                    ;; There is no space at front, so two spaces are emitted for
                    ;; indentation.
                    do (iter-yield "  ")
                    do (cl-loop for child in (dom-children ds)
                                do (pcase child
                                     ((pred stringp)
                                      (iter-yield child))
                                     (`(b nil (font ,_face ,s))
                                      (iter-yield s))
                                     (`(span nil ,s)
                                      (iter-yield s))
                                     (`(font nil (i nil ,s))
                                      (iter-yield (list s 'face 'italic)))
                                     (_ (iter-yield ""))))
                    do (iter-yield "\n\n"))))
        (t (iter-yield "\n"))))

(cl-defmethod fanyi-parse-from ((this fanyi-ah-service) dom)
  "Complete the fields of THIS from DOM tree.
A 'not-found exception may be thrown."
  (let ((results (dom-by-id dom "results")))
    ;; Nothing is found.
    (when (string= (dom-text results) "No word definition found")
      (throw 'not-found nil))
    ;; Syllable, sound and pronunciation.
    (let ((rtseg (dom-by-class results "rtseg")))
      (oset this :syllable (dom-texts (dom-child-by-tag rtseg 'b)))
      (oset this :sound (dom-attr (dom-child-by-tag rtseg 'a) 'href))
      ;; The pronunciation contains private unicodes, which conflict with
      ;; `all-the-icons'. Fix later.
      (oset this :pronunciation (s-trim (s-join ""
                                                (seq-map (lambda (arg)
                                                           (pcase arg
                                                             ((pred stringp) arg)
                                                             (`(font ,_face ,s) s)
                                                             (_ "")))
                                                         (dom-children rtseg))))))
    ;; Paraphrases.
    (let ((psegs (dom-by-class results "pseg")))
      (oset this :paraphrases
            (cl-loop for pseg in psegs
                     for pos = (cl-loop for child in (dom-children pseg)
                                        ;; Italic tags are consecutive, so `while'.
                                        while (listp child)
                                        for (i _nil s) = child
                                        when (and (symbolp i) (eq i 'i) (stringp s))
                                        concat s)
                     for examples = (cl-loop for child in (dom-children pseg)
                                             when (pcase child
                                                    (`(b nil ,s)
                                                     s))
                                             collect (dom-texts child))
                     collect (list pos
                                   examples
                                   (cl-loop for it iter-by (fanyi--parse-ah-pseg pseg)
                                            collect it)))))
    ;; Idioms.
    (let ((idms (dom-by-class results "idmseg")))
      )
    ;; Synonyms.
    (let ((syntx (dom-by-class results "syntx")))
      )
    )
  )

(cl-defmethod fanyi-render ((this fanyi-ah-service))
  "Render THIS page into a buffer named `fanyi-buffer-name'.
It's NOT thread-safe, caller should hold `fanyi-buffer-mtx'
before calling this method."
  (with-current-buffer (get-buffer-create fanyi-buffer-name)
    (save-excursion
      ;; Go to the end of buffer.
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        ;; The headline about American Heritage dictionary.
        (insert "# American Heritage\n\n")
        ;; Syllable, sound and pronunciation.
        (insert (oref this :syllable))
        (insert " ")
        (insert-button "🔊"
                       'action #'fanyi-play-sound
                       'button-data (format (oref this :sound-url) (oref this :sound))
                       'face 'fanyi-male-speaker-face
                       'follow-link t)
        (insert " ")
        ;; Use zero width space as boundary to fontify pronunciation. Since the
        ;; pronunciation has private unicodes, which conflict with
        ;; `all-the-icons', we use `fanyi-ah-pronunciation-face' to specify the
        ;; font family to display.
        (insert "\u200b"
                (oref this :pronunciation)
                "\u200b"
                "\n\n")
        ;; Paraphrases.
        (cl-loop for paraphrase in (oref this :paraphrases)
                 for (pos examples para) = paraphrase
                 do (insert "## " pos " "
                            (s-join ", " (seq-map (lambda (s)
                                                    ;; A word may end with a comma. Fix it manually.
                                                    (setq s (s-trim-right s))
                                                    (when (s-suffix? "," s)
                                                      (setq s (substring s 0 -1)))
                                                    (concat "*" s "*"))
                                                  examples))
                            "\n")
                 do (seq-do (lambda (arg)
                              (pcase arg
                                ((pred stringp)
                                 (insert arg))
                                (`(,s face italic)
                                 (insert "/" s "/"))
                                (`(,s face bold)
                                 (insert "*" s "*"))
                                (`(,s button ,data)
                                 (insert-button s
                                                'action #'fanyi-dwim
                                                'button-data data
                                                'follow-link t))))
                            para))
        (insert "\n\n")
        ))
    ))

;; ;; Use Minion New font to fontify pronunciation of American Heritage
;; ;; dictionary.
;; ("\u200b\\([^\u200b]+?\\)\u200b" . 'fanyi-ah-pronunciation-face)
;; (add-to-list
;;  'fanyi-mode-font-lock-keywords-extra
;;  '("\u200b\\([^\u200b]+?\\)\u200b" . 'fanyi-ah-pronunciation-face))

(defconst fanyi-ah-provider
  (fanyi-ah-service :word "dummy"
                    :url "https://ahdictionary.com/word/search.html?q=%s"
                    :sound-url "https://ahdictionary.com/%s")
  "American Heritage dictionary service instance.")

(provide 'fanyi-ah)
;;; fanyi-ah.el ends here
