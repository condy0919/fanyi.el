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
(require 'fanyi-shr)

(require 'subr-x)
(require 'dom)
(require 'seq)
(require 'json)
(require 'chart)
(require 'cl-lib)
(require 'button)
(require 'url-parse)
(require 'url-expand)

(defface fanyi-haici-star-face
  '((t :foreground "gold"))
  "Face used for star of word."
  :group 'fanyi)

(defface fanyi-haici-link-face
  '((t :inherit link :weight bold))
  "Face used for dictionary words and phrases."
  :group 'fanyi)

(define-button-type 'fanyi-haici-button
  'face nil
  'follow-link t)

(defcustom fanyi-haici-chart-inhibit-same-window nil
  "Non-nil means the distribution chart will be popped in another window."
  :type 'boolean
  :group 'fanyi)

(cl-defstruct fanyi-haici-result
  "Parsed Haici content for a single lookup."
  (syllable "-") (star 0) (star-desc "")
  phonetics paraphrases distribution related etymons content)

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

(defun fanyi-haici--class-p (node class)
  "Return non-nil if NODE has the exact CLASS token."
  (member class (split-string (or (dom-attr node 'class) ""))))

(defun fanyi-haici--text (node)
  "Return NODE's text with HTML whitespace collapsed."
  (string-trim (replace-regexp-in-string "[ \t\r\n]+" " " (dom-texts node ""))))

(defun fanyi-haici--link-url (href base)
  "Resolve an HTTP HREF against BASE, ignoring browser script actions."
  (when (fanyi-haici--present-p href)
    (let ((url (url-expand-file-name (string-trim href) base)))
      (when (member (url-type (url-generic-parse-url url)) '("http" "https"))
        url))))

(defun fanyi-haici--link-word (url)
  "Return the decoded dictionary word in URL, or nil for other pages."
  (when url
    (let ((parsed (url-generic-parse-url url)))
      (when (and (member (url-host parsed) '("dict.cn" "www.dict.cn"))
                 (string-match "\\`/\\([^/?#]+\\)\\(?:[?#].*\\)?\\'"
                               (url-filename parsed)))
        (decode-coding-string (url-unhex-string (match-string 1 (url-filename parsed)))
                              'utf-8)))))

(defun fanyi-haici--prepare-node (node request context)
  "Copy NODE into a reusable SHR DOM for REQUEST and tab CONTEXT.
Inactive layout tabs are dictionary content, even with display:none.
Other hidden elements and browser controls are omitted."
  (if (stringp node)
      (replace-regexp-in-string "[ \t\r\n]+" " " node)
    (let* ((tag (dom-tag node))
           (classes (split-string (or (dom-attr node 'class) ""))))
      (unless (or (memq tag '(script style iframe form input button textarea img svg
                                     object embed audio video source link meta base))
                  (cl-intersection classes '("fold" "unfold" "dict-chart" "wordbook"
                                             "newflag" "wtip") :test #'equal)
                  (dom-attr node 'slot)
                  (string-prefix-p "div-gpt-ad-" (or (dom-attr node 'id) ""))
                  (and (not (member "layout" classes))
                       (or (assq 'hidden (dom-attributes node))
                           (equal (dom-attr node 'aria-hidden) "true")
                           (string-match-p "display[ \t]*:[ \t]*none"
                                           (downcase (or (dom-attr node 'style) ""))))))
        (let ((children (delq nil (mapcar (lambda (child)
                                            (fanyi-haici--prepare-node child request context))
                                          (dom-children node))))
              (attributes (cl-loop for name in '(start value colspan rowspan)
                                   for value = (dom-attr node name)
                                   when value collect (cons name value)))
              face block)
          (cond
           ((eq tag 'a)
            (when-let* ((url (fanyi-haici--link-url (dom-attr node 'href)
                                                    (fanyi-request-url request))))
              (setq attributes `((href . ,url) (word . ,(fanyi-haici--link-word url))))))
           ((and (member "sound" classes) (fanyi-haici--present-p (dom-attr node 'naudio)))
            (setq tag 'fanyi-haici-audio
                  attributes `((audio . ,(dom-attr node 'naudio))
                               (sound-url . ,(fanyi-request-sound-url request))
                               (female . ,(member "fsound" classes)))
                  children nil))
           ((and (eq tag 'span) (member context '("detail" "dual" "en")))
            (setq face 'fanyi-pos-face block t))
           ((eq tag 'bdo) (setq face 'shadow))
           ((eq tag 'em) (setq face '(fanyi-pos-face bold)))
           ((and (eq tag 'p) (member context '("en" "auth")))
            (setq face 'shadow block t))
           ((and (eq tag 'b) (equal context "auth"))
            (setq face 'shadow block t))
           ((and (eq tag 'ol) (equal context "sort")) (setq tag 'ul))
           ((and (eq tag 'li) (equal context "sort"))
            ;; Keep the translation on its own line, with the list's hanging
            ;; indentation also applied to wrapped English and Chinese text.
            (when-let* ((break (seq-find (lambda (child)
                                           (and (consp child) (eq (dom-tag child) 'br)))
                                         children))
                        (tail (memq break children)))
              (setcdr tail `((fanyi-haici-face ((face . shadow)) ,@(cdr tail)))))))
          (when (and (member "nwd" classes) children)
            (setq tag 'ul
                  children (cl-loop for child in children
                                    when (or (consp child) (fanyi-haici--present-p child))
                                    collect `(li nil ,child))))
          ;; Presentation is semantic; saved CSS, scripts and remote assets
          ;; never control the reader's font, dimensions or network access.
          (if face
              `(fanyi-haici-face ((face . ,face) (block . ,block)) ,@children)
            `(,tag ,attributes ,@children)))))))

(defun fanyi-haici--content (dom request)
  "Extract all dictionary tabs from DOM for REQUEST in document order."
  (let (groups)
    (dolist (group (dom-by-class dom "\\bsection\\b"))
      (when (fanyi-haici--class-p group "section")
        (let (title tabs)
          (dolist (node (dom-non-text-children group))
            (cond
             ((eq (dom-tag node) 'h3) (setq title (fanyi-haici--text node)))
             ((fanyi-haici--class-p node "layout")
              (let* ((classes (split-string (or (dom-attr node 'class) "")))
                     (context (seq-find (lambda (class) (member class classes))
                                        '("detail" "dual" "en" "sort" "auth")))
                     (body (fanyi-haici--prepare-node node request context)))
                (when (fanyi-haici--present-p (fanyi-haici--text body))
                  (push `(fanyi-haici-section ((title . ,(or title "更多释义"))
                                               (class . ,(dom-attr node 'class)))
                                              ,body)
                        tabs)))
              (setq title nil))))
          (when tabs
            (push `(div ((class . ,(dom-attr group 'class)))
                        (fanyi-haici-heading nil
                                             ,(cond ((fanyi-haici--class-p group "def") "释义")
                                                    ((fanyi-haici--class-p group "sent") "用法与例句")
                                                    ((fanyi-haici--class-p group "rel") "相关资料")
                                                    (t (fanyi-haici--text (dom-child-by-tag group 'h2)))))
                        ,@(nreverse tabs))
                  groups)))))
    `(div nil ,@(nreverse groups))))

(defun fanyi-haici-parse (dom request)
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
    (setf (fanyi-haici-result-content result) (fanyi-haici--content dom request))
    (unless (or (fanyi-haici-result-paraphrases result)
                (seq-some (lambda (group)
                            (and (fanyi-haici--class-p group "def")
                                 (seq-some (lambda (item)
                                             (fanyi-haici--present-p (fanyi-haici--text item)))
                                           (dom-by-tag group 'li))))
                          (dom-non-text-children (fanyi-haici-result-content result))))
      (signal 'fanyi-parse-error '("Haici response contains no usable definitions")))
    (setf (fanyi-haici-result-syllable result) (fanyi-request-word request))
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

(defun fanyi-haici--shr-face (dom)
  "Render DOM with a semantic face and optional line boundaries."
  (when (dom-attr dom 'block) (shr-ensure-newline))
  (let ((start (point)))
    (shr-generic dom)
    (add-face-text-property start (point) (dom-attr dom 'face) t))
  (when (dom-attr dom 'block) (shr-ensure-newline)))

(defun fanyi-haici--shr-heading (dom)
  "Render DOM as a second-level Fanyi outline heading."
  (shr-ensure-paragraph)
  (let ((start (point)))
    (insert "## ")
    (shr-generic dom)
    (add-face-text-property start (point) 'fanyi-sub-headline-face))
  (shr-ensure-paragraph))

(defun fanyi-haici--shr-section (dom)
  "Render DOM as a locally folding dictionary tab."
  (shr-ensure-paragraph)
  (fanyi-shr-insert-section
   dom `(fanyi-haici-face ((face . bold)) ,(dom-attr dom 'title))
   `(blockquote nil ,@(dom-children dom)))
  (shr-ensure-paragraph))

(defun fanyi-haici--shr-link (dom)
  "Render DOM as a dictionary query button or an EWW link."
  (let ((start (point)) (url (dom-attr dom 'href)) (word (dom-attr dom 'word)))
    (shr-generic dom)
    (when (and url (< start (point)))
      (add-face-text-property start (point) 'fanyi-haici-link-face)
      (make-text-button start (point)
                        'type 'fanyi-haici-button
                        'action (if word #'fanyi-dwim #'eww)
                        'button-data (or word url)
                        'help-echo url))))

(defun fanyi-haici--shr-audio (dom)
  "Render DOM's optional pronunciation as a native audio button."
  (shr-insert " ")
  (let ((female (dom-attr dom 'female)))
    (fanyi-haici--insert-sound (dom-attr dom 'sound-url) (dom-attr dom 'audio)
                               (if female 'fanyi-female-speaker-face 'fanyi-male-speaker-face)
                               (if female "女声版发音" "男声版发音"))))

(defun fanyi-haici--insert-sound (sound-url audio face description)
  "Insert an AUDIO button using SOUND-URL, FACE and DESCRIPTION when available."
  (when (fanyi-haici--present-p audio)
    (insert " ")
    (insert-button "🔊"
                   'type 'fanyi-haici-button
                   'display (when (fanyi-display-glyphs-p)
                              (find-image `((:type xpm
                                                   :data ,fanyi-haici-speaker-xpm
                                                   :ascent center
                                                   :color-symbols
                                                   (("color" . ,(face-attribute face :foreground)))))))
                   'action #'fanyi-play-sound
                   'button-data (format sound-url audio)
                   'face face
                   'font-lock-face face
                   'follow-link t
                   'help-echo description)))

(defun fanyi-haici-render (result request)
  "Insert RESULT for REQUEST at point in the current buffer."
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
           do (insert "- " (propertize pos 'font-lock-face 'fanyi-pos-face) " " p "\n"))
  (insert "\n")
  ;; Make a button for distribution chart.
  (when-let* ((dist (fanyi-haici-result-distribution result)))
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
  (when-let* ((rs (fanyi-haici-result-related result)))
    (cl-loop for r in rs
             for (k v) = r
             do (insert k " ")
             do (insert-button v
                               'action #'fanyi-dwim
                               'button-data v
                               'follow-link t)
             do (insert " "))
    (insert "\n\n"))
  ;; Rich tabs use the same folding, font-lock and cached reflow as Longman.
  (when-let* ((content (fanyi-haici-result-content result))
              (_ (dom-children content)))
    (fanyi-shr-insert content
                      '((a . fanyi-haici--shr-link)
                        (fanyi-haici-face . fanyi-haici--shr-face)
                        (fanyi-haici-heading . fanyi-haici--shr-heading)
                        (fanyi-haici-section . fanyi-haici--shr-section)
                        (fanyi-haici-audio . fanyi-haici--shr-audio)))
    (insert "\n"))
  ;; Older or partial responses can supply an origin outside the tab groups.
  (when-let* ((etymons (fanyi-haici-result-etymons result))
              (_ (not (dom-by-class (fanyi-haici-result-content result) "\\betm\\b"))))
    (insert "## 起源\n\n")
    (cl-loop for i in etymons
             do (insert "- " i "\n"))
    (insert "\n"))
  ;; Visit the url for more information.
  (insert-button "Browse the full page via eww"
                 'action #'eww
                 'button-data (fanyi-request-url request)
                 'follow-link t)
  (insert "\n\n"))

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
