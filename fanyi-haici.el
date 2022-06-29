;;; fanyi-haici.el --- Haici dictionary service -*- lexical-binding: t -*-

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
;; Haici dictionary service.

;;; Code:

(require 'fanyi-base)

(require 's)
(require 'dom)
(require 'seq)
(require 'json)
(require 'chart)
(require 'cl-lib)
(require 'button)

(defface fanyi-haici-star-face
  '((t :foreground "gold"))
  "Face used for star of word."
  :group 'fanyi)

(defcustom fanyi-haici-chart-inhibit-same-window nil
  "Non-nil means the distribution chart will be popped in another window."
  :type 'boolean
  :group 'fanyi)

(defclass fanyi-haici-service (fanyi-base-service)
  ((syllable :initarg :syllable
             :initform "-"
             :type string
             :documentation "Syllable of the word.")
   (star :initarg :star
         :initform 0
         :type number
         :documentation "Frequency of the word.")
   (star-desc :initarg :star-desc
              :initform ""
              :type string
              :documentation "Frequency description of the word.")
   (phonetics :initarg :phonetics
              :type list
              :documentation "Phonetics of the word.
It could be either British pronunciation or American pronunciation.")
   (paraphrases :initarg :paraphrases
                :type list
                :documentation "List of (pos . paraphrase).")
   (distribution :initarg :distribution
                 :initform nil
                 :type list
                 :documentation "List of (percent . sense).")
   (related :initarg :related
            :type list
            :documentation "List of other parts of speech.")
   (etymons :initarg :etymons
            :type list
            :documentation "The etymons of the word."))
  "The Haici dictionary service.")

;; Silence unknown slots warning.
(eieio-declare-slots :syllable :star :star-desc :phonetics :paraphrases :distribution :related :etymons)

(defconst fanyi-haici-distribution-chart-title
  "fanyi-haici-distribution-chart"
  "The default name of Haici distribution chart buffer.")

(defconst fanyi-haici-speaker-xpm
  "\
/* XPM */
static char* speaker_xpm[] = {
\"13 14 3 1\",
\" 	c None\",
\".	c #000000\",
\"+	s color\",
\"        +++  \",
\"       ++++  \",
\"      +++++ +\",
\"     +++++++ \",
\"     ++++++  \",
\"++++ ++++++  \",
\"++++ ++++++++\",
\"++++ ++++++  \",
\"++++ ++++++  \",
\"     ++++++  \",
\"     +++++++ \",
\"      +++++ +\",
\"       ++++  \",
\"        +++  \"};"
  "The speaker xpm image.")

;; Silence compile warning.
(declare-function fanyi-dwim "fanyi")

(cl-defmethod fanyi-parse-from ((this fanyi-haici-service) dom)
  "Complete the fields of THIS from DOM tree.
A \='not-found exception will be thrown if there is no result."
  ;; No brief paraphrase is found, return early.
  (unless (dom-by-class dom "dict-basic-ul")
    (throw 'not-found nil))
  ;; Syllable, could be nil.
  (when-let* ((str (dom-attr (dom-by-class dom "keyword") 'tip))
              (matches (s-match "\\([a-zA-Z·]+\\)" str)))
    (oset this :syllable (nth 1 matches)))
  ;; Star and description, could be nil.
  (when-let* ((str (dom-attr (dom-by-class dom "level-title") 'level))
              (matches (s-match "\\([12345]\\)" str)))
    (oset this :star (string-to-number (nth 1 matches)))
    (oset this :star-desc str))
  ;; Phonetics, a list of (pronunciation, female sound url, male sound url)
  ;;
  ;; British: female, male
  ;; American: female, male
  (let ((phonetics (dom-children (dom-by-class dom "phonetic"))))
    (oset this :phonetics
          (cl-loop for p being the elements of phonetics using (index idx)
                   ;; British -> 1
                   ;; American -> 3
                   when (or (= idx 1) (= idx 3))
                   collect (list
                            ;; Pronunciation
                            (dom-text
                             (dom-search p
                                         (lambda (x)
                                           (string= (dom-attr x 'lang) "EN-US"))))
                            ;; Female sound url
                            (dom-attr (dom-by-class p "^\\(sound fsound\\)$") 'naudio)
                            ;; Male sound url
                            (dom-attr (dom-by-class p "^\\(sound\\)$") 'naudio)))))
  ;; Brief paraphrases, list of (pos, paraphrase)
  (let ((paraphrases (butlast (dom-by-tag (dom-by-class dom "dict-basic-ul") 'li))))
    (oset this :paraphrases
          (cl-loop for p in paraphrases
                   for pos = (dom-text (nth 3 p))
                   for para = (dom-text (nth 5 p))
                   collect (list pos para))))
  ;; Distribution of senses, could be nil
  (when-let* ((chart-basic (dom-attr (dom-by-id dom "dict-chart-basic") 'data))
              (json (json-read-from-string (url-unhex-string chart-basic))))
    (oset this :distribution
          ;; Transform (\1 (percent . 55) (sense . "abc")) into (55 "abc")
          (cl-loop for j in json
                   collect (seq-map #'cdr (seq-drop j 1)))))
  ;; The related words, could be nil.
  (let ((shapes (dom-children (dom-by-class dom "shape"))))
    (oset this :related
          (seq-partition (cl-loop for i in shapes
                                  when (consp i)
                                  collect (s-trim (dom-text i)))
                         2)))
  ;; The etymons of the word, could be nil.
  (let ((etymons (dom-attributes (dom-children (dom-by-class dom "layout etm")))))
    (oset this :etymons (cl-loop for i in etymons
                                 when (consp i)
                                 collect (dom-texts i)))))

(cl-defmethod fanyi-render ((this fanyi-haici-service))
  "Render THIS page into a buffer named `fanyi-buffer-name'.
It's NOT thread-safe, caller should hold `fanyi-buffer-mtx'
before calling this method."
  (fanyi-with-fanyi-buffer
    ;; The headline about Haici service.
    (insert "# 海词\n\n")
    ;; Syllables and star/level description.
    (insert (oref this :syllable)
            " "
            (s-repeat (oref this :star) "★")
            " "
            (oref this :star-desc)
            "\n\n")
    ;; Phonetics.
    ;; British: pronunciation, female sound url, male sound url
    ;; American: pronunciation, female sound url, male sound url
    (let ((phonetics (oref this :phonetics)))
      (cl-assert (equal (length phonetics) 2))
      (cl-loop for i from 0 to 1
               for (pronunciation female male) = (nth i phonetics)
               do (insert (if (equal i 0) "英" "  美"))
               do (insert " " pronunciation " ")
               do (insert-button "🔊"
                                 'display (when (fanyi-display-glyphs-p)
                                            (find-image `((:type xpm
                                                                 :data ,fanyi-haici-speaker-xpm
                                                                 :ascent center
                                                                 :color-symbols
                                                                 (("color" . ,(face-attribute 'fanyi-female-speaker-face :foreground)))))))
                                 'action #'fanyi-play-sound
                                 'button-data (format (oref this :sound-url) female)
                                 'face 'fanyi-female-speaker-face
                                 'follow-link t
                                 'help-echo "女声版发音")
               do (insert " ")
               do (insert-button "🔊"
                                 'display (when (fanyi-display-glyphs-p)
                                            (find-image `((:type xpm
                                                                 :data ,fanyi-haici-speaker-xpm
                                                                 :ascent center
                                                                 :color-symbols
                                                                 (("color" . ,(face-attribute 'fanyi-male-speaker-face :foreground)))))))
                                 'action #'fanyi-play-sound
                                 'button-data (format (oref this :sound-url) male)
                                 'face 'fanyi-male-speaker-face
                                 'follow-link t
                                 'help-echo "男声版发音"))
      (insert "\n\n"))
    ;; Paraphrases.
    ;; - n. 荣誉；荣幸；尊敬；信用；正直；贞洁
    ;; - vt. 尊敬；使荣幸；对...表示敬意；兑现
    ;; ...
    (cl-loop for pa in (oref this :paraphrases)
             for (pos p) = pa
             do (insert "- " pos " " p "\n"))
    (insert "\n")
    ;; Make a button for distribution chart.
    (when-let ((dist (oref this :distribution)))
      (insert-button "Click to view the distribution chart"
                     'action (lambda (dist)
                               ;; `switch-to-buffer' is used in `chart-bar-quickie' which means a new chart buffer will
                               ;; always be popped in the same window.
                               ;;
                               ;; Typing `q' (`quit-window') in chart buffer will
                               ;;
                               ;; 1. bury the *fanyi-haici-distribution-chart* buffer.
                               ;; 2. reset the `restore-quit' window parameter of current window to nil.
                               ;;
                               ;; When the second `q' is typed in *fanyi* buffer, `quit-window' will
                               ;;
                               ;; 1. bury the *fanyi* buffer.
                               ;; 2. `switch-to-prev-buffer'
                               ;;
                               ;; It can't restore the window configuration since `restore-quit' was reset to nil.
                               ;;
                               ;; Once `fanyi-haici-chart-inhibit-same-window' is non-nil, the chart buffer won't be popped
                               ;; in the same window which prevent `quit-window' window parameter from being reset.
                               ;;
                               ;; See (C-x C-e) the below manual
                               ;;
                               ;; (info "(elisp)Quitting Windows")
                               ;;
                               ;; for more information.
                               (let ((switch-to-buffer-obey-display-actions fanyi-haici-chart-inhibit-same-window)
                                     (display-buffer-alist `((,(concat "\\*" fanyi-haici-distribution-chart-title "\\*")
                                                              (display-buffer-pop-up-window)
                                                              (inhibit-same-window . t)))))
                                 (chart-bar-quickie
                                  'vertical
                                  fanyi-haici-distribution-chart-title
                                  (seq-map #'cadr dist) "Senses"
                                  (seq-map #'car dist) "Percent")))
                     'button-data dist
                     'follow-link t)
      (insert "\n\n"))
    ;; Make buttons for related words.
    (when-let ((rs (oref this :related)))
      (cl-loop for r in rs
               for (k v) = r
               do (insert k " ")
               do (insert-button v
                                 'action #'fanyi-dwim
                                 'button-data v
                                 'follow-link t)
               do (insert " "))
      (insert "\n\n"))
    ;; The etymons.
    (when-let ((etymons (oref this :etymons)))
      (insert "## 起源\n\n")
      (cl-loop for i in etymons
               do (insert "- " i "\n"))
      (insert "\n"))
    ;; Visit the url for more information.
    (insert-button "Browse the full page via eww"
                   'action #'eww
                   'button-data (format (oref this :url) (oref this :word))
                   'follow-link t)
    (insert "\n\n")))

;; The Haici dict specific font-lock keywords
(add-to-list 'fanyi-mode-font-lock-keywords-extra '("★" . 'fanyi-haici-star-face))

(defconst fanyi-haici-provider
  (fanyi-haici-service :word "dummy"
                       :url "https://dict.cn/%s"
                       :sound-url "https://audio.dict.cn/%s"
                       :api-type 'xml)
  "Haici dictionary service instance.")

(provide 'fanyi-haici)
;;; fanyi-haici.el ends here
