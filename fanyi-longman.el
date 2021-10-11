;;; fanyi.el --- Longman dictionary service -*- lexical-binding: t -*-

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
;; Longman dictionary service with beautiful SVG icons.

;;; Code:

(require 'fanyi-base)

(require 's)
(require 'dom)
(require 'seq)
(require 'svg)
(require 'cl-lib)
(require 'button)

(defcustom fanyi-longman-svg-horizonal-padding 4
  "Default horizonal padding in pixels for text."
  :type 'integer
  :group 'fanyi)

(defcustom fanyi-longman-svg-vertical-padding 2
  "Default vertical padding in pixels for text."
  :type 'integer
  :group 'fanyi)

(defcustom fanyi-longman-svg-horizonal-offset 0
  "Default horizonal offset in pixels for text."
  :type 'integer
  :group 'fanyi)

(defcustom fanyi-longman-svg-vertical-offset -5
  "Default vertical offset in pixels for text."
  :type 'integer
  :group 'fanyi)

(defcustom fanyi-longman-svg-border-radius 7
  "Default border radius in pixels."
  :type 'integer
  :group 'fanyi)

(defcustom fanyi-longman-example-indent 2
  "Default indent for examples."
  :type 'integer
  :group 'fanyi)

(defface fanyi-longman-svg-asset-face
  '((((background light))
     :foreground "white"
     :background "#f1d600")
    (((background dark))
     :foreground "black"
     :background "#a6a7bd"))
  "Face used for asset intro."
  :group 'fanyi)

(defface fanyi-longman-svg-signpost-face
  '((((background light))
     :foreground "white"
     :background "#f18500")
    (((background dark))
     :foreground "black"
     :background "#ef932d"))
  "Face used for signpost."
  :group 'fanyi)

(defface fanyi-longman-registerlab-face
  '((t :foreground "purple" :slant italic))
  "Face used for register label."
  :group 'fanyi)

(defface fanyi-longman-dot-face
  '((t :foreground "red"))
  "Face used for level dot."
  :group 'fanyi)

(defface fanyi-longman-grammar-face
  '((t :foreground "green" :weight bold))
  "Face used for grammar."
  :group 'fanyi)

(defface fanyi-longman-geo-face
  '((t :foreground "#364395" :slant italic))
  "Face used for geography."
  :group 'fanyi)

(defface fanyi-longman-example-face
  '((t :foreground "gray"))
  "Face used for examples."
  :group 'fanyi)

(defclass fanyi-longman-service (fanyi-base-service)
  ((word-family :initarg :word-family
                :initform nil
                :type list
                :documentation "Word family of the word.")
   (dicts :initarg :dicts
          :initform nil
          :type list
          :documentation "Dictionary entry.")
   (etymon :initarg :etymon
           :type string
           :documentation "Etymon of the word."))
  "The Longman dictionary service.")

(defclass fanyi-longman-dict ()
  ((name :initarg :name
         :type string
         :documentation "Name of the dictionary.")
   (hyphenation :initarg :hyphenation
                :initform nil
                :documentation "Hyphenation form.")
   (pronunciation :initarg :pronunciation
                  :initform nil
                  :documentation "Pronunciation of the word.")
   (level :initarg :level
          :initform nil
          :documentation "Level of the word.")
   (freqs :initarg :freqs
          :initform nil
          :documentation "Frequence of the word.")
   (academy :initarg :academy
            :initform nil
            :documentation "Academic usage.")
   (pos :initarg :pos
        :initform nil
        :documentation "Part of speech.")
   (inflection :initarg :inflection
               :initform nil
               :documentation "Inflection.")
   (grammar :initarg :grammar
            :initform nil
            :documentation "Grammar.")
   (british :initarg :british
            :documentation "British voice.")
   (american :initarg :american
             :documentation "American voice.")
   (senses :initarg :senses
           :type list
           :documentation "List of `fanyi-longman-dict-sense's."))
  "The Longman dictionary entry.")

(defclass fanyi-longman-dict-sense ()
  ((signpost :initarg :signpost
             :initform nil
             :documentation "Signpost.")
   (grammar :initarg :grammar
            :initform nil
            :documentation "Grammar.")
   (registerlab :initarg :registerlab
                :initform nil
                :documentation "Register Label.")
   (lexunit :initarg :lexunit
            :initform nil
            :documentation "Lexical unit.")
   (geo :initarg :geo
        :initform nil
        :documentation "Geography.")
   (def :initarg :def
        :type list
        :documentation "The definition.
Typically it can be a list of strings or \"riched\" strings.")
   (crossref :initarg :crossref
             :initform nil
             :documentation "Crossref.")
   (syn :initarg :syn
        :initform nil
        :documentation "Synonym.")
   (examples :initarg :examples
             :type list
             :documentation "List of examples.")
   (footnote-expl :initarg :footnote-expl
                  :type string
                  :documentation "Footnote explanation.
Typically it can be a list of strings or \"riched\" strings.")
   (footnote-example :initarg :footnote-example
                     :type string
                     :documentation "Footnote example.
Typically it can be a list of strings or \"riched\" strings."))
  "The Longman dictionary entry sense.")

;; Silence unknown slots warning.
(eieio-declare-slots :word-family :dicts :etymon)
(eieio-declare-slots :name :hyphenation :pronunciation :level :freqs :academy :pos :inflection :grammar :british :american :senses)
(eieio-declare-slots :signpost :grammar :registerlab :lexunit :geo :def :crossref :syn :examples :footnote-expl :footnote-example)

;; Silence compile warning.
(declare-function fanyi-dwim "fanyi")

;; e.g.
;;
;; ```
;; .-------------.
;; | Word family |
;; '-------------'
;; ```
(defun fanyi-longman-svg-tag-make (text face)
  "Create a SVG image displaying TEXT with FACE in a rounded box."
  (let* ((tag-width (* (length text) (window-font-width)))
         (tag-height (window-font-height))
         (svg-width (+ tag-width (* 2 fanyi-longman-svg-horizonal-padding)))
         (svg-height (+ tag-height (* 2 fanyi-longman-svg-vertical-padding)))
         (svg (svg-create svg-width svg-height)))
    (svg-rectangle svg 0 0 svg-width svg-height
                   :rx fanyi-longman-svg-border-radius
                   :fill (face-attribute face :background))
    (svg-text svg text
              :fill (face-attribute face :foreground)
              :x (+ fanyi-longman-svg-horizonal-padding fanyi-longman-svg-horizonal-offset)
              :y (+ tag-height fanyi-longman-svg-vertical-padding fanyi-longman-svg-vertical-offset))
    (svg-image svg :scale 1 :ascent 'center)))

(cl-defmethod fanyi-parse-from ((this fanyi-longman-service) dom)
  "Complete the fields of THIS from DOM tree."
  (let ((dict (dom-by-class dom "^\\(dictionary\\)$")))
    ;; Word family, e.g. (noun) accumulation (adjective) accumulative (verb) accumulate (adverb) accumulatively
    (when-let ((wordfams (dom-children (dom-by-class dict "wordfams"))))
      (let ((head1 (pop wordfams))
            (head2 (pop wordfams)))
        (cl-assert (s-blank-str? head1))
        (cl-assert (dom-by-class head2 "asset_intro")))
      (oset this :word-family
            (cl-loop with pos = ""
                     with fam-set = nil
                     for node in wordfams
                     when (and (stringp node) (not (s-blank-str? node))) do
                     (cl-pushnew
                      (s-trim node)
                      (alist-get pos fam-set))
                     when (listp node)
                     if (dom-by-class node "pos") do
                     (setq pos (s-trim (dom-text node)))
                     else do
                     (cl-pushnew
                      (s-trim (dom-text node))
                      (alist-get pos fam-set))
                     finally return fam-set)))
    ;; Dicts
    (let ((dict-entries (dom-by-class dict "^\\(dictentry\\)$")))
      (oset this :dicts
            (cl-loop for entry in dict-entries
                     collect (let ((name (dom-text (dom-by-class entry "dictionary_intro")))
                                   (head (dom-by-class entry "Head"))
                                   (senses (dom-by-class entry "^\\(Sense\\)$"))
                                   (dict (fanyi-longman-dict)))
                               (oset dict :name name)
                               ;; Hyphenation.
                               (when-let ((hyph (dom-by-class head "HYPHENATION")))
                                 (oset dict :hyphenation (dom-text hyph)))
                               ;; Pronunciation.
                               (when-let ((codes (dom-by-class head "PronCodes")))
                                 (oset dict :pronunciation (s-trim (dom-texts codes ""))))
                               ;; Level.
                               (when-let ((level (dom-by-class head "LEVEL")))
                                 (oset dict :level (cons (s-trim (dom-text level)) (dom-attr level 'title))))
                               ;; Frequency.
                               (when-let ((freqs (dom-by-class head "FREQ")))
                                 (oset dict :freqs (cl-loop for f in freqs
                                                            collect (cons (s-trim (dom-text f)) (dom-attr f 'title)))))
                               ;; Academic.
                               (when-let ((ac (dom-by-class head "AC")))
                                 (oset dict :academy (cons (dom-text ac) (dom-attr ac 'title))))
                               ;; Part of speech.
                               (when-let ((pos (dom-by-class head "POS")))
                                 (oset dict :pos (s-trim (dom-text pos))))
                               ;; Inflection.
                               (when-let ((inflect (dom-by-class head "Inflections")))
                                 (oset dict :inflection (s-trim (dom-texts inflect ""))))
                               ;; Grammar.
                               (when-let ((gram (dom-by-class head "^\\(GRAM\\)$")))
                                 (oset dict :grammar (s-trim (dom-texts gram ""))))
                               ;; Voice
                               (oset dict :british (dom-attr (dom-by-class head "brefile") 'data-src-mp3))
                               (oset dict :american (dom-attr (dom-by-class head "amefile") 'data-src-mp3))
                               ;; Senses.
                               (oset dict :senses (cl-loop for sense in senses
                                                           collect (let ((dict-sense (fanyi-longman-dict-sense)))
                                                                     ;; Signpost.
                                                                     (when-let ((signpost (dom-by-class sense "SIGNPOST")))
                                                                       (oset dict-sense :signpost (dom-text signpost)))
                                                                     ;; Grammar.
                                                                     (when-let ((grammar (dom-by-class sense "^\\(GRAM\\)$")))
                                                                       (oset dict-sense :grammar (s-trim (dom-texts grammar ""))))
                                                                     ;; Register label.
                                                                     (when-let ((label (dom-by-class sense "REGISTERLAB")))
                                                                       (oset dict-sense :registerlab (s-trim (dom-text label))))
                                                                     ;; LEXUNIT.
                                                                     (when-let ((unit (dom-by-class sense "LEXUNIT")))
                                                                       (oset dict-sense :lexunit (s-trim (dom-text unit))))
                                                                     ;; Geography.
                                                                     (when-let ((geo (dom-by-class sense "GEO")))
                                                                       (oset dict-sense :geo (s-trim (dom-text geo))))
                                                                     ;; Definition.
                                                                     (oset dict-sense :def
                                                                           (seq-map (lambda (node)
                                                                                      (pcase (type-of node)
                                                                                        ('string (s-trim node))
                                                                                        ('cons (cond ((dom-by-class node "defRef")
                                                                                                      (list (dom-texts node "") 'button (dom-texts node "")))
                                                                                                     ((dom-by-class node "REFHWD")
                                                                                                      (list (dom-texts node "") 'face 'italic))
                                                                                                     (t (user-error "Unimplemented. %s" (pp-to-string node)))))
                                                                                        (_ (user-error "Unimplemented. %s" (pp-to-string node)))))
                                                                                    (dom-children (dom-by-class sense "^\\(DEF\\)$"))))
                                                                     ;; Crossref.
                                                                     (when-let* ((crossref (dom-by-class sense "Crossref"))
                                                                                 (href (dom-attr (dom-child-by-tag crossref 'a) 'href)))
                                                                       (cl-assert (s-prefix? "/dictionary/" href))
                                                                       (oset dict-sense :crossref
                                                                             (cons (s-trim (dom-texts crossref ""))
                                                                                   (car (s-split "#" (substring href 12))))))
                                                                     ;; Synonym.
                                                                     (when-let ((syn (dom-by-class sense "^\\(SYN\\)$")))
                                                                       (oset dict-sense :syn (s-trim (dom-text syn))))
                                                                     ;; Examples
                                                                     (oset dict-sense :examples
                                                                           (cl-loop for example in (dom-by-class sense "EXAMPLE")
                                                                                    collect (cons (dom-attr (dom-by-class example "speaker") 'data-src-mp3)
                                                                                                  (s-trim (s-replace-all '(("\u00a0" . "")
                                                                                                                           ("\u0009" . ""))
                                                                                                                         (dom-texts example ""))))))
                                                                     ;; Footnote.
                                                                     (let ((footnote (dom-by-class sense "F2NBox")))
                                                                       (oset dict-sense :footnote-expl
                                                                             (cl-loop for child in (dom-children (dom-by-class footnote "EXPL"))
                                                                                      concat (cl-etypecase child
                                                                                               (string child)
                                                                                               (list (concat "*" (dom-text child) "*")))))
                                                                       (oset dict-sense :footnote-example
                                                                             (cl-loop for child in (dom-children (dom-by-class footnote "EXAMPLE"))
                                                                                      concat (cl-etypecase child
                                                                                               (string child)
                                                                                               (list (concat "*" (dom-text child) "*"))))))
                                                                     dict-sense)))
                               dict))))
    ;; Etymon
    (let ((etymon (dom-by-class (dom-by-class dict "etym") "Sense")))
      (oset this :etymon (dom-texts etymon "")))))

(cl-defmethod fanyi-render ((this fanyi-longman-service))
  "Render THIS page into a buffer named `fanyi-buffer-name'.
It's NOT thread-safe, caller should hold `fanyi-buffer-mtx'
before calling this method."
  (fanyi-with-fanyi-buffer
   ;; The headline about Longman dictionary service.
   (insert "# Longman\n\n")
   ;; Word family section.
   (when-let (word-family (oref this :word-family))
     (insert (propertize "Word family"
                         'display (when (fanyi-display-glyphs-p)
                                    (fanyi-longman-svg-tag-make "Word family" 'fanyi-longman-svg-asset-face))))
     (cl-loop for wf in word-family
              for pos = (car wf)
              for words = (cdr wf)
              do (insert " " pos)
              do (cl-loop for w in words
                          do (insert " ")
                          do (insert-button w
                                            'action #'fanyi-dwim
                                            'button-data w
                                            'follow-link t)))
     (insert "\n\n"))
   ;; Dicts.
   (cl-loop for dict in (oref this :dicts)
            do (insert "## " (oref dict :name) "\n\n")
            ;; job /dʒɒb $ dʒɑːb/ ●●● S1 W1 AWL (noun) 🔊 🔊
            ;; ^            ^       ^               ^
            ;; hyphenation  pron    level           pos
            do (when-let ((hyph (oref dict :hyphenation)))
                 (insert hyph))
            do (when-let ((pron (oref dict :pronunciation)))
                 (insert " " pron))
            do (when-let ((level (oref dict :level)))
                 (insert " "
                         (propertize (car level) 'help-echo (cdr level))))
            do (insert
                (s-join " "
                        (seq-map (pcase-lambda (`(,freq . ,desc))
                                   (propertize freq
                                               'help-echo desc
                                               'display (when (fanyi-display-glyphs-p)
                                                          (fanyi-longman-svg-tag-make freq 'fanyi-longman-svg-asset-face))))
                                 (oref dict :freqs))))
            do (when-let ((ac (oref dict :academy)))
                 (insert " "
                         (propertize (car ac)
                                     'help-echo (cdr ac)
                                     'display (when (fanyi-display-glyphs-p)
                                                (fanyi-longman-svg-tag-make (car ac) 'fanyi-longman-svg-asset-face)))))
            do (when-let ((pos (oref dict :pos)))
                 (insert " (" pos ")"))
            do (when-let ((inflect (oref dict :inflection)))
                 (insert " " inflect))
            do (when-let ((gram (oref dict :grammar)))
                 (insert " " gram))
            do (when (oref dict :british)
                 (insert " ")
                 (insert-button "🔊"
                                'action #'fanyi-play-sound
                                'button-data (oref dict :british)
                                'face 'fanyi-female-speaker-face
                                'help-echo "Play British pronunciation"
                                'follow-link t)
                 (insert " ")
                 (insert-button "🔊"
                                'action #'fanyi-play-sound
                                'button-data (oref dict :american)
                                'face 'fanyi-male-speaker-face
                                'help-echo "Play American pronunciation"
                                'follow-link t))
            do (insert "\n\n")
            ;; - work [countable] informal unit   GEO   the regular paid work SYN *foo* link
            ;;   ^             ^         ^    ^     ^       ^                 ^           ^
            ;;   signpost      grammar  lbl lexut  geo      button          synonym     crossref
            ;;
            ;; For easy implementation, crossref is put at the end of
            ;; definition.
            do (cl-loop for sense in (oref dict :senses)
                        do (insert "- ")
                        do (when-let ((signpost (oref sense :signpost)))
                             (insert (propertize signpost
                                                 'display (when (fanyi-display-glyphs-p)
                                                            (fanyi-longman-svg-tag-make signpost 'fanyi-longman-svg-signpost-face)))
                                     " "))
                        do (when-let ((grammar (oref sense :grammar)))
                             (insert grammar " "))
                        do (when-let ((geo (oref sense :geo)))
                             (insert (propertize geo
                                                 'font-lock-face 'fanyi-longman-geo-face)
                                     " "))
                        do (when-let ((label (oref sense :registerlab)))
                             (insert (propertize label
                                                 'font-lock-face 'fanyi-longman-registerlab-face)
                                     " "))
                        do (when-let ((unit (oref sense :lexunit)))
                             (insert "*" unit "*"
                                     " "))
                        do (seq-do (lambda (s)
                                     (pcase s
                                       ((pred stringp)
                                        (insert s " "))
                                       (`(,text face italic)
                                        (insert "/" text "/"))
                                       (`(,text button ,data)
                                        (insert-button text
                                                       'action #'fanyi-dwim
                                                       'button-data data
                                                       'follow-link t)
                                        (insert " "))))
                                   (oref sense :def))
                        do (when-let ((synonym (oref sense :syn)))
                             (insert (propertize "SYN"
                                                 'display (when (fanyi-display-glyphs-p)
                                                            (fanyi-longman-svg-tag-make "SYN" 'fanyi-longman-svg-asset-face)))
                                     " "
                                     "*" synonym "*"
                                     " "))
                        do (when-let ((crossref (oref sense :crossref)))
                             (insert-button (car crossref)
                                            'action #'fanyi-dwim
                                            'button-data (cdr crossref)
                                            'follow-link t))
                        do (insert "\n")
                        ;; Examples.
                        do (cl-loop for example in (oref sense :examples)
                                    for mp3 = (car example)
                                    for expl = (cdr example)
                                    do (insert-button (if mp3 "🔊" "🔇")
                                                      'action #'fanyi-play-sound
                                                      'button-data (or mp3 "")
                                                      'face 'fanyi-longman-example-face
                                                      'line-prefix (s-repeat fanyi-longman-example-indent " ")
                                                      'help-echo "Play Example"
                                                      'follow-link t)
                                    do (insert " "
                                               (propertize expl
                                                           'font-lock-face 'fanyi-longman-example-face
                                                           'wrap-prefix (s-repeat fanyi-longman-example-indent " "))
                                               "\n"))
                        ;; Footnote.
                        do (when-let* ((expl (oref sense :footnote-expl))
                                       ((s-present? expl)))
                             (insert (propertize "> "
                                                 'line-prefix (s-repeat fanyi-longman-example-indent " "))
                                     (propertize expl
                                                 'wrap-prefix (s-repeat fanyi-longman-example-indent " "))
                                     "\n"))
                        do (when-let* ((ex (oref sense :footnote-example))
                                       ((s-present? ex)))
                             (insert (propertize ">> "
                                                 'line-prefix (s-repeat (* 2 fanyi-longman-example-indent) " "))
                                     (propertize ex
                                                 'font-lock-face 'fanyi-longman-example-face
                                                 'wrap-prefix (s-repeat (* 2 fanyi-longman-example-indent) " "))
                                     "\n")))
            do (insert "\n"))
   ;; Etymon.
   (when (s-present? (oref this :etymon))
     (insert "## Etymon\n\n")
     (insert (propertize "Origin"
                         'display (when (fanyi-display-glyphs-p)
                                    (fanyi-longman-svg-tag-make "Origin" 'fanyi-longman-svg-asset-face))))
     (insert " ")
     (insert "*" (oref this :word) "*")
     (insert " ")
     (insert (oref this :etymon)))
   ;; The end.
   (insert "\n\n")))

;; The Longman dict specific font-lock keywords
(add-to-list 'fanyi-mode-font-lock-keywords-extra '("●" . 'fanyi-longman-dot-face))
(add-to-list 'fanyi-mode-font-lock-keywords-extra '("○" . 'fanyi-longman-dot-face))
(add-to-list 'fanyi-mode-font-lock-keywords-extra '("\\[\\([a-zA-Z, ]+?\\)\\]" . 'fanyi-longman-grammar-face))
(add-to-list 'fanyi-mode-font-lock-keywords-extra '("\u201c\\([a-zA-Z ]+?\\)\u201d" . 'font-lock-string-face))

(defconst fanyi-longman-provider
  (fanyi-longman-service :word "dummy"
                         :url "https://www.ldoceonline.com/dictionary/%s"
                         :sound-url "unused"
                         :api-type 'xml)
  "Longman dictionary service instance.")

(provide 'fanyi-longman)
;;; fanyi-longman.el ends here
