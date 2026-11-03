;;; fanyi-haici.el --- Haici dictionary service -*- lexical-binding: t -*-

;; Copyright (C) 2021 Zhiwei Chen
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/condy0919/fanyi.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

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

(require 'subr-x)
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

(cl-defstruct fanyi-haici-result
  "Parsed Haici content for a single lookup."
  (syllable "-") (star 0) (star-desc "")
  phonetics paraphrases distribution related etymons)

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

(defun fanyi-haici--present-p (value)
  "Return non-nil if VALUE is a nonblank string."
  (and (stringp value) (not (string-empty-p (string-trim value)))))

(defun fanyi-haici--distribution (data)
  "Read optional chart DATA, ignoring malformed rows and chart data."
  (when (fanyi-haici--present-p data)
    (condition-case nil
        (let ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol))
          (cl-loop for row in (json-read-from-string (url-unhex-string data))
                   for fields = (and (listp (cdr-safe row)) (cdr-safe row))
                   for percent = (cdr (assq 'percent fields))
                   for sense = (cdr (assq 'sense fields))
                   when (and (numberp percent) (<= 0 percent 100)
                             (fanyi-haici--present-p sense))
                   collect (list percent sense)))
      (error nil))))

(defun fanyi-haici-parse (dom _request)
  "Parse DOM, preserving definitions when optional data is absent.
Signal `fanyi-parse-error' when no usable definitions can be extracted."
  (let ((result (make-fanyi-haici-result)))
    ;; Locate elements rather than depending on whitespace or a final dummy li.
    (setf (fanyi-haici-result-paraphrases result)
          (cl-loop for item in (dom-by-tag (dom-by-class dom "dict-basic-ul") 'li)
                   for pos = (string-trim (dom-texts (dom-child-by-tag item 'span) ""))
                   for para = (string-trim (dom-texts (dom-child-by-tag item 'strong) ""))
                   when (fanyi-haici--present-p para)
                   collect (list pos para)))
    (unless (fanyi-haici-result-paraphrases result)
      (signal 'fanyi-parse-error '("Haici response contains no usable definitions")))
    (save-match-data
      ;; Syllable, could be nil.
      (when-let* ((str (dom-attr (dom-by-class dom "keyword") 'tip)))
        (when (string-match "\\([a-zA-Z·]+\\)" str)
          (setf (fanyi-haici-result-syllable result) (match-string 1 str))))
      ;; Star and description, could be nil.
      (when-let* ((str (dom-attr (dom-by-class dom "level-title") 'level)))
        (when (string-match "\\([12345]\\)" str)
          (setf (fanyi-haici-result-star result) (string-to-number (match-string 1 str))
                (fanyi-haici-result-star-desc result) str))))
    (setf (fanyi-haici-result-phonetics result)
          (cl-loop for item in (dom-non-text-children (dom-by-class dom "phonetic"))
                   for phonetic = (dom-search item (lambda (node) (dom-attr node 'lang)))
                   for pronunciation = (string-trim (dom-texts phonetic ""))
                   for label-text = (string-trim (dom-text item))
                   for lang = (downcase (or (dom-attr phonetic 'lang) ""))
                   for label = (cond ((string-match-p "\\`\\(?:英\\|UK\\|British\\)" label-text) "英")
                                     ((string-match-p "\\`\\(?:美\\|US\\|American\\)" label-text) "美")
                                     ((equal lang "en-gb") "英")
                                     ((equal lang "en-us") "美")
                                     (t "音标"))
                   for female = (dom-attr (dom-by-class item "\\bfsound\\b") 'naudio)
                   for male = (dom-attr
                               (dom-search item
                                           (lambda (node)
                                             (let ((classes (split-string (or (dom-attr node 'class) ""))))
                                               (and (member "sound" classes)
                                                    (not (member "fsound" classes))))))
                               'naudio)
                   when (or (fanyi-haici--present-p pronunciation)
                            (fanyi-haici--present-p female)
                            (fanyi-haici--present-p male))
                   collect (list label pronunciation female male)))
    (setf (fanyi-haici-result-distribution result)
          (fanyi-haici--distribution (dom-attr (dom-by-id dom "dict-chart-basic") 'data)))
    ;; Ignore incomplete related-word pairs.
    (setf (fanyi-haici-result-related result)
          (cl-loop for (label word)
                   in (seq-partition
                       (mapcar (lambda (node) (string-trim (dom-texts node "")))
                               (dom-non-text-children (dom-by-class dom "shape")))
                       2)
                   when (and (fanyi-haici--present-p label)
                             (fanyi-haici--present-p word))
                   collect (list label word)))
    (let* ((section (dom-by-class dom "\\betm\\b"))
           (items (or (dom-by-tag section 'li) (dom-by-tag section 'p))))
      (setf (fanyi-haici-result-etymons result)
            (cl-loop for item in items
                     for text = (string-trim (dom-texts item ""))
                     when (fanyi-haici--present-p text)
                     collect text)))
    result))

(defun fanyi-haici--insert-sound (sound-url audio face description)
  "Insert an AUDIO button using SOUND-URL, FACE and DESCRIPTION when available."
  (when (fanyi-haici--present-p audio)
    (insert " ")
    (insert-button "🔊"
                   'display (when (fanyi-display-glyphs-p)
                              (find-image `((:type xpm
                                                   :data ,fanyi-haici-speaker-xpm
                                                   :ascent center
                                                   :color-symbols
                                                   (("color" . ,(face-attribute face :foreground)))))))
                   'action #'fanyi-play-sound
                   'button-data (format sound-url audio)
                   'face face
                   'follow-link t
                   'help-echo description)))

(defun fanyi-haici-render (result request)
  "Render RESULT for REQUEST in the buffer named `fanyi-buffer-name'.
It's NOT thread-safe, caller should hold `fanyi-buffer-mtx'
before calling this function."
  (fanyi-with-fanyi-buffer
    ;; The headline about Haici service.
    (insert "# 海词\n\n")
    ;; Syllables and star/level description.
    (insert (fanyi-haici-result-syllable result)
            " "
            (make-string (fanyi-haici-result-star result) ?★)
            " "
            (fanyi-haici-result-star-desc result)
            "\n\n")
    ;; Each available pronunciation carries its own label and optional audio.
    (when (fanyi-haici-result-phonetics result)
      (cl-loop for (label pronunciation female male) in (fanyi-haici-result-phonetics result)
               for first = t then nil
               do (unless first (insert "  "))
               do (insert label " " pronunciation)
               do (fanyi-haici--insert-sound (fanyi-request-sound-url request)
                                             female 'fanyi-female-speaker-face "女声版发音")
               do (fanyi-haici--insert-sound (fanyi-request-sound-url request)
                                             male 'fanyi-male-speaker-face "男声版发音"))
      (insert "\n\n"))
    ;; Paraphrases.
    ;; - n. 荣誉；荣幸；尊敬；信用；正直；贞洁
    ;; - vt. 尊敬；使荣幸；对...表示敬意；兑现
    ;; ...
    (cl-loop for pa in (fanyi-haici-result-paraphrases result)
             for (pos p) = pa
             do (insert "- " pos " " p "\n"))
    (insert "\n")
    ;; Make a button for distribution chart.
    (when-let ((dist (fanyi-haici-result-distribution result)))
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
    (when-let ((rs (fanyi-haici-result-related result)))
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
    (when-let ((etymons (fanyi-haici-result-etymons result)))
      (insert "## 起源\n\n")
      (cl-loop for i in etymons
               do (insert "- " i "\n"))
      (insert "\n"))
    ;; Visit the url for more information.
    (insert-button "Browse the full page via eww"
                   'action #'eww
                   'button-data (fanyi-request-url request)
                   'follow-link t)
    (insert "\n\n")))

;; The Haici dict specific font-lock keywords
(add-to-list 'fanyi-mode-font-lock-keywords-extra '("★" . 'fanyi-haici-star-face))

(defconst fanyi-haici-provider
  (fanyi-provider-create :url "https://dict.cn/%s"
                         :sound-url "https://audio.dict.cn/%s"
                         :api-type 'xml
                         :parse #'fanyi-haici-parse
                         :render #'fanyi-haici-render)
  "Haici dictionary provider.")

(provide 'fanyi-haici)
;;; fanyi-haici.el ends here
