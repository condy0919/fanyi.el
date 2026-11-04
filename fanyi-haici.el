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
(require 'svg)

(require 'shr)
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

(defcustom fanyi-haici-expanded-sections '(dual sort anno nfo)
  "Sections initially expanded in new Haici results.
Use t for all sections, nil for none, or a list of section symbols:
detail (Chinese definitions), dual (bilingual definitions), en (English),
sort (examples), anno (collocations), auth (quotations), nfo (synonyms),
nwd (nearby words), and etm (origin).  If dual is requested but absent,
detail is expanded instead.  Existing reader folds survive reflow."
  :type '(choice (const :tag "All" t) (const :tag "None" nil)
                 (repeat symbol))
  :group 'fanyi)

(defface fanyi-haici-label-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used for Haici vocabulary level labels."
  :group 'fanyi)

(defcustom fanyi-haici-chart-inhibit-same-window nil
  "Non-nil means the distribution chart will be popped in another window."
  :type 'boolean
  :group 'fanyi)

(cl-defstruct fanyi-haici-result
  "Parsed Haici content for a single lookup."
  (syllable "-") (star 0) (star-desc "")
  phonetics paraphrases distribution related etymons content level-label)

(defconst fanyi-haici-distribution-chart-title
  "fanyi-haici-distribution-chart"
  "The default name of Haici distribution chart buffer.")

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

(defun fanyi-haici--visible-dom (node &optional parent-section-p)
  "Copy visible NODE while preserving inactive dictionary tabs.
PARENT-SECTION-P means NODE is a direct child of a dictionary section.
Only its layout tabs may ignore display:none; hidden and aria-hidden
always hide the complete subtree, including otherwise visible descendants."
  (cond
   ((stringp node) node)
   ((consp node)
    (unless (or (assq 'hidden (dom-attributes node))
                (equal (dom-attr node 'aria-hidden) "true")
                (and (not (and parent-section-p (fanyi-haici--class-p node "layout")))
                     (string-match-p
                      "\\(?:\\`\\|;\\)[ \t\r\n]*display[ \t\r\n]*:[ \t\r\n]*none\\(?:[ \t\r\n]*[;!]\\|[ \t\r\n]*\\'\\)"
                      (downcase (or (dom-attr node 'style) "")))))
      `(,(dom-tag node) ,(copy-tree (dom-attributes node))
        ,@(delq nil (mapcar (lambda (child)
                              (fanyi-haici--visible-dom child (fanyi-haici--class-p node "section")))
                            (dom-children node))))))))

(defun fanyi-haici--text (node)
  "Return NODE's text with HTML whitespace collapsed."
  (string-trim (replace-regexp-in-string "[ \t\r\n]+" " " (fanyi-shr-dom-text node))))

(defun fanyi-haici--syllable (dom)
  "Read the complete syllabified headword from DOM, or its visible keyword."
  (let* ((keyword (dom-by-class dom "keyword"))
         (tip (dom-attr keyword 'tip))
         (syllable (and (stringp tip)
                        (string-trim
                         (replace-regexp-in-string
                          "\\`[ \t]*音节划分[ \t]*[:：][ \t]*" "" tip))))
         (word (fanyi-haici--text keyword)))
    (cond ((fanyi-haici--present-p syllable) syllable)
          ((fanyi-haici--present-p word) word))))

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

(defun fanyi-haici--entries (children)
  "Convert simple query rows in CHILDREN to aligned entries.
Return nil for mixed or unfamiliar markup, which keeps normal HTML rendering."
  (let ((items (seq-filter #'consp children)))
    (when (and items
               (not (seq-some #'fanyi-haici--present-p children))
               (seq-every-p
                (lambda (item)
                  (and (eq (dom-tag item) 'li)
                       (= (length (dom-non-text-children item)) 1)
                       (eq (dom-tag (car (dom-non-text-children item))) 'a)
                       (dom-attr (car (dom-non-text-children item)) 'word))) items))
      (mapcar
       (lambda (item)
         (let ((link (car (dom-non-text-children item))))
           `(entry ((word . ,(dom-attr link 'word)) (url . ,(dom-attr link 'href)))
                   ,(string-trim (mapconcat (lambda (child) (if (stringp child) child ""))
                                            (dom-children item) "")))))
       items))))

(defun fanyi-haici--prepare-node (node request context)
  "Copy visible NODE into a reusable SHR DOM for REQUEST and tab CONTEXT.
Browser controls are omitted; visibility is resolved before extracting tabs."
  (if (stringp node)
      (replace-regexp-in-string "[ \t\r\n]+" " " node)
    (let* ((tag (dom-tag node))
           (classes (split-string (or (dom-attr node 'class) ""))))
      (unless (or (memq tag '(script style iframe form input button textarea img svg
                                     object embed audio video source link meta base))
                  (cl-intersection classes '("fold" "unfold" "dict-chart" "wordbook"
                                             "newflag" "wtip") :test #'equal)
                  (dom-attr node 'slot)
                  (string-prefix-p "div-gpt-ad-" (or (dom-attr node 'id) "")))
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
           ((and (eq tag 'li) (equal context "dual"))
            (when-let* ((label (seq-find (lambda (child)
                                           (and (consp child) (eq (dom-tag child) 'strong)))
                                         children))
                        (tail (memq label children)))
              (setcdr tail (cons '(br nil) (cdr tail)))))
           ((and (eq tag 'li) (equal context "sort"))
            (setq tag 'fanyi-haici-example)
            ;; Keep the translation on its own line, with the list's hanging
            ;; indentation also applied to wrapped English and Chinese text.
            (when-let* ((break (seq-find (lambda (child)
                                           (and (consp child) (eq (dom-tag child) 'br)))
                                         children))
                        (tail (memq break children)))
              (setcdr tail `((fanyi-haici-face ((face . shadow)) ,@(cdr tail)))))))
          (when (and (eq tag 'ul) (member context '("anno" "nfo")))
            (when-let* ((entries (fanyi-haici--entries children)))
              (setq tag 'fanyi-haici-entries children entries)))
          (when (and (member "nwd" classes) children)
            (let ((entries (fanyi-haici--entries
                            (cl-loop for child in children
                                     when (or (consp child) (fanyi-haici--present-p child))
                                     collect `(li nil ,child)))))
              (when entries (setq tag 'fanyi-haici-words children entries))))
          ;; Presentation is semantic; saved CSS, scripts and remote assets
          ;; never control the reader's font, dimensions or network access.
          (if face
              `(fanyi-haici-face ((face . ,face) (block . ,block)) ,@children)
            `(,tag ,attributes ,@children)))))))

(defun fanyi-haici--item-count (node)
  "Count nonempty outer entries in prepared NODE, excluding nested lists."
  (cond
   ((stringp node) 0)
   ((memq (dom-tag node) '(li entry fanyi-haici-example))
    (if (or (fanyi-haici--present-p (dom-attr node 'word))
            (fanyi-haici--present-p (fanyi-haici--text node))) 1 0))
   (t (cl-loop for child in (dom-children node)
               sum (fanyi-haici--item-count child)))))

(defun fanyi-haici--section-count (body context)
  "Count usable entries in prepared BODY for known tab CONTEXT.
Return nil when the structure does not provide a reliable item count."
  (when context
    (let ((count (fanyi-haici--item-count body)))
      (when (zerop count)
        (setq count
              (pcase context
                ("etm" (cl-count-if
                        (lambda (node)
                          (and (eq (dom-tag node) 'p)
                               (fanyi-haici--present-p (fanyi-haici--text node))))
                        (dom-non-text-children body)))
                ("nwd" (cl-count-if
                        (lambda (node) (fanyi-haici--present-p (dom-attr node 'word)))
                        (dom-by-tag body 'a)))
                (_ 0))))
      (when (> count 0) count))))

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
                                        '("detail" "dual" "en" "sort" "auth" "anno" "nfo" "nwd" "etm")))
                     (body (fanyi-haici--prepare-node node request context)))
                (when (or (fanyi-haici--present-p (fanyi-haici--text body))
                          (dom-by-tag body 'entry))
                  (push `(fanyi-haici-section ((title . ,(or title "更多释义"))
                                               (kind . ,(and context (intern context)))
                                               (count . ,(fanyi-haici--section-count body context))
                                               (class . ,(dom-attr node 'class)))
                                              ,body)
                        tabs)))
              (setq title nil))))
          (when tabs
            (push `(div ((class . ,(dom-attr group 'class)))
                        ,@(nreverse tabs))
                  groups)))))
    `(div nil ,@(nreverse groups))))

(defun fanyi-haici-parse (dom request)
  "Parse DOM, preserving definitions when optional data is absent.
Throw `not-found' for Haici's explicit missing-entry page.  Signal
`fanyi-parse-error' for other responses without usable definitions."
  (let ((dom (fanyi-haici--visible-dom dom))
        (result (make-fanyi-haici-result)))
    ;; Locate elements rather than depending on whitespace or a final dummy li.
    (setf (fanyi-haici-result-paraphrases result)
          (cl-loop for item in (dom-by-tag (dom-by-class dom "dict-basic-ul") 'li)
                   for pos = (string-trim (fanyi-shr-dom-text (dom-child-by-tag item 'span)))
                   for para = (string-trim (fanyi-shr-dom-text (dom-child-by-tag item 'strong)))
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
      ;; A missing definition alone could mean a changed page or an error.
      ;; Require Haici's missing-entry title and its normal suggestion prompt.
      (if (and (equal (fanyi-haici--text (car (dom-by-tag dom 'title)))
                      "该词条未找到_海词词典")
               (string-match-p "您要查找的是不是\\|Sorry，没有找到与此相符的结果"
                               (fanyi-haici--text (car (dom-by-tag dom 'body)))))
          (throw 'not-found nil)
        (signal 'fanyi-parse-error '("Haici response contains no usable definitions"))))
    (setf (fanyi-haici-result-syllable result)
          (or (fanyi-haici--syllable dom) (fanyi-request-word request)))
    (setf (fanyi-haici-result-level-label result)
          (fanyi-haici--text (dom-by-class dom "level-title")))
    (save-match-data
      ;; Star and description, could be nil.
      (when-let* ((str (dom-attr (dom-by-class dom "level-title") 'level)))
        (when (string-match "\\([12345]\\)" str)
          (setf (fanyi-haici-result-star result) (string-to-number (match-string 1 str))
                (fanyi-haici-result-star-desc result) str))))
    (setf (fanyi-haici-result-phonetics result)
          (cl-loop for item in (dom-non-text-children (dom-by-class dom "phonetic"))
                   for phonetic = (dom-search item (lambda (node) (dom-attr node 'lang)))
                   for pronunciation = (string-trim (fanyi-shr-dom-text phonetic))
                   for label-text = (string-trim
                                     (mapconcat #'identity (seq-filter #'stringp (dom-children item)) " "))
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
                       (mapcar (lambda (node) (string-trim (fanyi-shr-dom-text node)))
                               (dom-non-text-children (dom-by-class dom "shape")))
                       2)
                   when (and (fanyi-haici--present-p label)
                             (fanyi-haici--present-p word))
                   collect (list label word)))
    (let* ((section (dom-by-class dom "\\betm\\b"))
           (items (or (dom-by-tag section 'li) (dom-by-tag section 'p))))
      (setf (fanyi-haici-result-etymons result)
            (cl-loop for item in items
                     for text = (string-trim (fanyi-shr-dom-text item))
                     when (fanyi-haici--present-p text)
                     collect text)))
    result))

(defun fanyi-haici--layout-width ()
  "Return the available width in columns for the current SHR render."
  (max 1 (min shr-internal-width
              (/ (window-body-width nil t) (max 1 (window-font-width))))))

(defun fanyi-haici--wrap-lines (text width)
  "Trim TEXT and wrap it into lines of WIDTH columns."
  (fanyi-shr-wrap-lines (string-trim text) width))

(defun fanyi-haici--insert-word (entry text)
  "Insert TEXT as ENTRY's native query button.
TEXT may include wrapping whitespace; the action always uses the full word."
  (let ((start (point))
        (word (dom-attr entry 'word)))
    (insert text)
    (when (< start (point))
      (make-text-button start (point)
                        'type 'fanyi-haici-button
                        'action #'fanyi-dwim
                        'button-data word
                        'help-echo (or (dom-attr entry 'url) (concat "Query " word)))
      (put-text-property start (point) 'face 'fanyi-haici-link-face))))

(defun fanyi-haici--shr-entries (dom)
  "Render DOM's word/explanation entries as columns or stacked rows.
Each child is (entry ((word . WORD) (url . URL)) EXPLANATION).
Keep columns only when every word fits in 30 columns and at least 24 columns
remain for explanations.  Respect the current SHR indentation, inserting
the layout directly so SHR does not collapse alignment spaces."
  (shr-ensure-newline)
  (let* ((entries (dom-non-text-children dom))
         (width (fanyi-haici--layout-width))
         (indent (min (floor shr-indentation) (max 0 (- width 8))))
         (padding (make-string indent ?\s))
         (word-width (apply #'max 0 (mapcar (lambda (entry)
                                              (string-width (dom-attr entry 'word)))
                                            entries)))
         (column (+ word-width 2))
         (columns (and (<= word-width 30) (>= (- width indent column) 24)))
         (explanation-indent (+ indent (if columns column (min 2 (max 0 (- width indent 1))))))
         (explanation-padding (make-string explanation-indent ?\s)))
    (dolist (entry entries)
      (let ((word (dom-attr entry 'word))
            (explanation (string-trim (fanyi-shr-dom-text entry))))
        (insert padding)
        (fanyi-haici--insert-word
         entry (if columns word
                 (mapconcat #'identity (fanyi-haici--wrap-lines word (- width indent))
                            (concat "\n" padding))))
        (unless (string-empty-p explanation)
          (if columns
              (insert (make-string (- column (string-width word)) ?\s))
            (insert "\n" explanation-padding))
          (insert (mapconcat #'identity
                             (fanyi-haici--wrap-lines explanation (- width explanation-indent))
                             (concat "\n" explanation-padding))))
        (insert (if columns "\n" "\n\n"))))))

(defun fanyi-haici--shr-words (dom)
  "Render DOM's entries as compact word buttons with automatic wrapping.
Children use the same word/url attributes as `fanyi-haici--shr-entries'."
  (shr-ensure-newline)
  (let* ((width (fanyi-haici--layout-width))
         (indent (min (floor shr-indentation) (max 0 (- width 8))))
         (padding (make-string indent ?\s))
         (column indent))
    (dolist (entry (dom-non-text-children dom))
      (let* ((word (dom-attr entry 'word))
             (word-width (string-width word)))
        (when (and (> column indent) (> (+ column 2 word-width) width))
          (insert "\n")
          (setq column indent))
        (if (= column indent)
            (insert padding)
          (insert "  ")
          (setq column (+ column 2)))
        (let ((lines (fanyi-haici--wrap-lines word (- width indent))))
          (fanyi-haici--insert-word entry (mapconcat #'identity lines (concat "\n" padding)))
          (setq column (+ (if (cdr lines) indent column) (string-width (car (last lines))))))))
    (shr-ensure-newline)))

(defun fanyi-haici--shr-face (dom)
  "Render DOM with a semantic face and optional line boundaries."
  (when (dom-attr dom 'block) (shr-ensure-newline))
  (let ((start (point)))
    (shr-generic dom)
    (add-face-text-property start (point) (dom-attr dom 'face) t)
    (when (dom-attr dom 'title)
      (put-text-property start (point) 'help-echo (dom-attr dom 'title))))
  (when (dom-attr dom 'block) (shr-ensure-newline)))

(defun fanyi-haici--shr-section (dom)
  "Render DOM as a locally folding dictionary tab."
  (shr-ensure-paragraph)
  (let ((kind (dom-attr dom 'kind))
        (count (dom-attr dom 'count)))
    (fanyi-shr-insert-section
     dom `(span nil (fanyi-haici-face ((face . bold)) ,(dom-attr dom 'title))
                ,@(when (and count (> count 0))
                    `((fanyi-haici-face ((face . shadow)) ,(format " · %d" count)))))
     `(fanyi-haici-body nil ,@(dom-children dom))
     (not (or (eq fanyi-haici-expanded-sections t)
              (memq kind fanyi-haici-expanded-sections)
              (and (eq kind 'detail) (dom-attr dom 'fallback)
                   (memq 'dual fanyi-haici-expanded-sections))))))
  (shr-ensure-paragraph))

(defun fanyi-haici--shr-body (dom)
  "Render DOM with a compact body indentation."
  (let ((shr-indentation (+ shr-indentation 2)))
    (shr-generic dom)))

(defun fanyi-haici--shr-line (dom)
  "Render DOM on a separate line without adding empty paragraphs."
  (shr-ensure-newline)
  (shr-generic dom)
  (shr-ensure-newline))

(defvar fanyi-haici--number-width 1
  "Width of the current ordered list's numbers.")

(defun fanyi-haici--shr-list (dom)
  "Render DOM's list without paragraph gaps around its part-of-speech label."
  (shr-ensure-newline)
  (let* ((start (dom-attr dom 'start))
         (shr-list-mode (if (eq (dom-tag dom) 'ol)
                            (if (and start (string-match-p "\\`[+-]?[0-9]+\\'" start))
                                (string-to-number start) 1)
                          'ul))
         (fanyi-haici--number-width
          (if (numberp shr-list-mode)
              (max (length (number-to-string shr-list-mode))
                   (length (number-to-string (+ shr-list-mode (1- (length (dom-by-tag dom 'li)))))))
            1)))
    (shr-generic dom))
  (shr-ensure-newline))

(defun fanyi-haici--shr-item (dom)
  "Render DOM with aligned numbers and hanging continuation indentation."
  (if (not (numberp shr-list-mode))
      (shr-tag-li dom)
    (shr-ensure-newline)
    (let* ((start (point))
           (bullet (format (format "%%%dd " fanyi-haici--number-width) shr-list-mode)))
      (setq shr-list-mode (1+ shr-list-mode))
      (insert bullet)
      (shr-mark-fill start)
      (let ((shr-indentation (+ shr-indentation (shr-string-pixel-width bullet))))
        (put-text-property start (1+ start) 'shr-continuation-indentation shr-indentation)
        (put-text-property start (1+ start) 'shr-prefix-length (length bullet))
        (shr-generic dom)))
    (shr-ensure-newline)))

(defun fanyi-haici--shr-example (dom)
  "Keep each English/Chinese example together with space between pairs."
  (shr-tag-li dom)
  (shr-ensure-paragraph))

(defun fanyi-haici--shr-label (dom)
  "Render DOM's vocabulary label as SVG with searchable fallback text."
  (let ((start (point)))
    (shr-generic dom)
    (add-face-text-property start (point) 'fanyi-haici-label-face)
    (put-text-property start (point) 'help-echo (dom-attr dom 'title))
    (when (and (< start (point)) (fanyi-display-glyphs-p) (image-type-available-p 'svg))
      (let* ((text (buffer-substring-no-properties start (point)))
             (height (window-font-height))
             (padding (window-font-width))
             (width (* padding (+ 2 (string-width text))))
             (color (face-attribute 'fanyi-haici-label-face :foreground nil 'default))
             (svg (svg-create width height)))
        (svg-rectangle svg 0.5 0.5 (1- width) (1- height)
                       :rx 4 :fill "none" :stroke color :stroke-width 1)
        (svg-text svg text :x padding :y (* height 0.75)
                  :font-family (face-attribute 'default :family nil t)
                  :font-size (* height 0.75) :fill color)
        (put-text-property start (point) 'display (svg-image svg :ascent 'center))))))

(defun fanyi-haici--shr-action (dom)
  "Render DOM's generated chart or source-page action."
  (let ((start (point)))
    (shr-generic dom)
    (add-face-text-property start (point) 'link)
    (make-text-button start (point) 'type 'fanyi-haici-button
                      'action (dom-attr dom 'action) 'button-data (dom-attr dom 'data))))

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
    (insert-text-button "🔊"
                        'type 'fanyi-haici-button
                        'display (fanyi-speaker-image face)
                        'action #'fanyi-play-sound
                        'button-data (format sound-url audio)
                        'face face
                        'font-lock-face face
                        'follow-link t
                        'help-echo description)))

(defun fanyi-haici--show-distribution (distribution)
  "Display DISTRIBUTION, honoring the chart window preference."
  (let ((switch-to-buffer-obey-display-actions fanyi-haici-chart-inhibit-same-window)
        (display-buffer-alist `((,(concat "\\*" fanyi-haici-distribution-chart-title "\\*")
                                 (display-buffer-pop-up-window)
                                 (inhibit-same-window . t)))))
    (chart-bar-quickie 'vertical fanyi-haici-distribution-chart-title
                       (mapcar #'cadr distribution) "Senses"
                       (mapcar #'car distribution) "Percent")))

(defun fanyi-haici--header (result request)
  "Build the compact, reflowable header for RESULT and REQUEST."
  `(div nil
        (fanyi-haici-line nil
                          (strong nil ,(fanyi-haici-result-syllable result)) " "
                          (fanyi-haici-face ((face . fanyi-haici-star-face)
                                             (title . ,(fanyi-haici-result-star-desc result)))
                                            ,(make-string (fanyi-haici-result-star result) ?★)) " "
                          (fanyi-haici-label ((title . ,(fanyi-haici-result-star-desc result)))
                                             ,(or (fanyi-haici-result-level-label result) "")))
        ,@(when (fanyi-haici-result-phonetics result)
            `((fanyi-haici-line nil
                                ,@(cl-loop for (label pronunciation female male) in (fanyi-haici-result-phonetics result)
                                           append `(" " ,label " " ,pronunciation
                                                    ,@(cl-loop for audio in (list female male)
                                                               for is-female = t then nil
                                                               when (fanyi-haici--present-p audio)
                                                               collect `(fanyi-haici-audio
                                                                         ((audio . ,audio)
                                                                          (sound-url . ,(fanyi-request-sound-url request))
                                                                          (female . ,is-female)))))))))
        ,@(cl-loop for (pos text) in (fanyi-haici-result-paraphrases result)
                   collect `(fanyi-haici-line nil
                                              (fanyi-haici-face ((face . fanyi-pos-face)) ,pos) " " ,text))
        ,@(when (fanyi-haici-result-related result)
            `((fanyi-haici-line nil
                                ,@(cl-loop for (label word) in (fanyi-haici-result-related result)
                                           append `(,label " " (a ((word . ,word)
                                                                   (href . ,(concat "https://dict.cn/" (url-hexify-string word))))
                                                                  ,word) " ")))))
        (fanyi-haici-line nil
                          ,@(when-let* ((distribution (fanyi-haici-result-distribution result)))
                              `((fanyi-haici-action ((action . fanyi-haici--show-distribution) (data . ,distribution))
                                                    "释义分布") "  "))
                          (fanyi-haici-action ((action . eww) (data . ,(fanyi-request-url request))) "原网页"))))

(defun fanyi-haici--render-style (window)
  "Return settings affecting Haici's layout in WINDOW."
  (let ((frame (window-frame window)))
    (list fanyi-use-glyphs
          (window-font-width window) (window-font-height window)
          (display-images-p frame)
          (image-type-available-p 'svg) (image-type-available-p 'xpm)
          (face-attribute 'default :family frame t)
          (face-attribute 'fanyi-haici-label-face :foreground frame 'default)
          (face-attribute 'fanyi-female-speaker-face :foreground frame 'default)
          (face-attribute 'fanyi-male-speaker-face :foreground frame 'default))))

(defun fanyi-haici-render (result request)
  "Insert RESULT for REQUEST at point, preserving native dictionary actions."
  (insert "# " (fanyi-request-display-name request) "\n\n")
  (let* ((content (copy-tree (fanyi-haici-result-content result)))
         (sections (dom-by-tag content 'fanyi-haici-section))
         (has-dual (seq-some (lambda (section) (eq (dom-attr section 'kind) 'dual)) sections)))
    (unless has-dual
      (dolist (section sections)
        (when (eq (dom-attr section 'kind) 'detail)
          (dom-set-attribute section 'fallback t))))
    (fanyi-shr-insert
     `(div nil ,(fanyi-haici--header result request) ,content
           ,@(when (and (fanyi-haici-result-etymons result)
                        (not (dom-by-class content "\\betm\\b")))
               `((fanyi-haici-section ((title . "起源") (kind . etm)
                                       (count . ,(length (fanyi-haici-result-etymons result))))
                                      (ul nil ,@(mapcar (lambda (text) `(li nil ,text))
                                                        (fanyi-haici-result-etymons result)))))))
     '((a . fanyi-haici--shr-link)
       (li . fanyi-haici--shr-item)
       (ol . fanyi-haici--shr-list)
       (ul . fanyi-haici--shr-list)
       (fanyi-haici-face . fanyi-haici--shr-face)
       (fanyi-haici-line . fanyi-haici--shr-line)
       (fanyi-haici-section . fanyi-haici--shr-section)
       (fanyi-haici-body . fanyi-haici--shr-body)
       (fanyi-haici-example . fanyi-haici--shr-example)
       (fanyi-haici-audio . fanyi-haici--shr-audio)
       (fanyi-haici-label . fanyi-haici--shr-label)
       (fanyi-haici-action . fanyi-haici--shr-action)
       (fanyi-haici-entries . fanyi-haici--shr-entries)
       (fanyi-haici-words . fanyi-haici--shr-words))
     #'fanyi-haici--render-style))
  (insert "\n"))

;; The Haici dict specific font-lock keywords
(add-to-list 'fanyi-mode-font-lock-keywords-extra '("★" . 'fanyi-haici-star-face))

(defconst fanyi-haici-provider
  (fanyi-provider-create :name "海词"
                         :url "https://dict.cn/%s"
                         :sound-url "https://audio.dict.cn/%s"
                         :api-type 'xml
                         :parse #'fanyi-haici-parse
                         :render #'fanyi-haici-render)
  "Haici dictionary provider.")

(provide 'fanyi-haici)
;;; fanyi-haici.el ends here
