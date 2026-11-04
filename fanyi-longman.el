;;; fanyi-longman.el --- Longman dictionary service -*- lexical-binding: t -*-

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
;; Render Longman dictionary content with shr.

;;; Code:

(require 'fanyi-base)
(require 'fanyi-shr)

(require 'subr-x)
(require 'dom)
(require 'seq)
(require 'cl-lib)
(require 'button)
(require 'browse-url)
(require 'shr)
(require 'svg)
(require 'outline)

(defcustom fanyi-longman-svg-border-radius 7
  "Border radius in pixels for Longman labels."
  :type 'natnum
  :group 'fanyi)

(defcustom fanyi-longman-example-indent 2
  "Indentation of Longman examples rendered by shr."
  :type 'natnum
  :group 'fanyi)

(defface fanyi-longman-example-face
  '((t :foreground "#808080"))
  "Face used for examples."
  :group 'fanyi)

(defface fanyi-longman-british-speaker-face
  '((t :foreground "red"))
  "Face used for British pronunciation speakers."
  :group 'fanyi)

(defface fanyi-longman-american-speaker-face
  '((t :foreground "blue"))
  "Face used for American pronunciation speakers."
  :group 'fanyi)

(defface fanyi-longman-svg-asset-face
  '((((background light)) :foreground "white" :background "#f1d600")
    (((background dark)) :foreground "black" :background "#a6a7bd"))
  "Face used for Longman SVG labels."
  :group 'fanyi)

(defface fanyi-longman-grammar-face
  '((t :foreground "#008000" :weight bold))
  "Face used for grammar."
  :group 'fanyi)

(defface fanyi-longman-dot-face
  '((t :foreground "red" :height 1.2))
  "Face used for level dots."
  :group 'fanyi)

(defface fanyi-longman-registerlab-face
  '((t :foreground "#800080" :slant italic))
  "Face used for register labels."
  :group 'fanyi)

(defface fanyi-longman-geo-face
  '((t :foreground "#364395"))
  "Face used for geographical labels."
  :group 'fanyi)

(defface fanyi-longman-headword-face
  '((t :foreground "red" :weight bold :height 1.6))
  "Face used for the displayed headword."
  :group 'fanyi)

(defface fanyi-longman-body-face
  '((((background light)) :foreground "#333333")
    (((background dark)) :foreground "#dddddd"))
  "Face used for ordinary dictionary text."
  :group 'fanyi)

(defface fanyi-longman-pos-face
  '((t :foreground "#008000" :weight bold))
  "Face used for parts of speech."
  :group 'fanyi)

(defface fanyi-longman-dictionary-face
  '((t :foreground "white" :background "#35a3ff" :weight normal :extend t))
  "Face used for the dictionary source heading."
  :group 'fanyi)

(defface fanyi-longman-heading-face
  '((((background light)) :foreground "#333333" :weight bold :height 1.1)
    (((background dark)) :foreground "#eeeeee" :weight bold :height 1.1))
  "Face used for supplementary section headings."
  :group 'fanyi)

(defface fanyi-longman-topic-face
  '((t :foreground "red" :underline t))
  "Face used for related topics."
  :group 'fanyi)

(defface fanyi-longman-definition-link-face
  '((t :underline (:color "gray" :style line)))
  "Face used for links within definitions and examples."
  :group 'fanyi)

(defface fanyi-longman-box-heading-face
  '((t :inherit fanyi-longman-body-face :weight bold :height 1.2))
  "Face used for thesaurus and collocation section headings."
  :group 'fanyi)

(defface fanyi-longman-note-heading-face
  '((t :inherit fanyi-longman-body-face :weight bold :height 1.0))
  "Face used for short usage-note headings such as Register."
  :group 'fanyi)

(defface fanyi-longman-group-heading-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used for group headings within supplementary sections."
  :group 'fanyi)

(defface fanyi-longman-frequency-face
  '((t :foreground "red" :weight bold :box (:line-width 1 :color "red")))
  "Face used for frequency labels and their outlined SVG badges."
  :group 'fanyi)

(defface fanyi-longman-signpost-face
  '((t :foreground "white" :background "#f18500" :weight bold))
  "Face used for the site's orange sense labels and their SVG badges."
  :group 'fanyi)

(defface fanyi-longman-synopp-face
  '((t :foreground "white" :background "#f1d600" :weight bold))
  "Face used for synonym and antonym labels and their SVG badges."
  :group 'fanyi)

(defface fanyi-longman-homonym-face
  '((t :foreground "red" :weight bold :height 1.0))
  "Face used for superscript homonym numbers in dictionary entries."
  :group 'fanyi)

(defconst fanyi-longman--class-styles
  '(("dictionary_intro" :tag fanyi-heading)
    ("asset_intro" :tag fanyi-heading)
    ("Head" :tag fanyi-line :space t)
    ("Sense" :tag fanyi-sense)
    ("SIGNPOST" :tag fanyi-signpost)
    ("synopp" :tag fanyi-synopp)
    ("Crossref" :tag fanyi-line :space t)
    ("topics_container" :tag div)
    ("EXAMPLE" :tag fanyi-example)
    ("exa" :tag fanyi-example)
    ("exaEntry" :tag fanyi-block)
    ("ColloBox" :tag fanyi-section :indent 2)
    ("F2NBox" :tag fanyi-block :indent 2)
    ("ThesBox" :tag fanyi-section :indent 2)
    ("Section" :tag fanyi-block)
    ("heading" :face fanyi-longman-box-heading-face)
    ("HEADING" :face fanyi-longman-box-heading-face)
    ("SECHEADING" :tag fanyi-group-heading)
    ("Collocate" :tag fanyi-block :indent 1)
    ("Exponent" :tag fanyi-block :indent 1)
    ("EXPL" :tag fanyi-line)
    ("COLLOC" :face bold)
    ("EXP" :face bold)
    ("EXPR" :face bold)
    ("COLLOINEXA" :face bold)
    ("DEFBOLD" :face bold)
    ("GramExa" :tag fanyi-line)
    ("PROPFORM" :tag fanyi-line :indent 2 :face bold)
    ("PROPFORMPREP" :tag fanyi-line :indent 2 :face bold)
    ("title" :tag span :face bold)
    ("headword" :tag span :face bold)
    ("HYPHENATION" :face fanyi-longman-headword-face)
    ("HWD" :face bold)
    ("HOMNUM" :tag fanyi-homonym :face fanyi-longman-homonym-face)
    ("PHRVBHWD" :face fanyi-longman-headword-face)
    ("POS" :face (fanyi-longman-pos-face fanyi-pos-face) :space t)
    ("pos" :face (fanyi-longman-pos-face fanyi-pos-face) :space t)
    ("GRAM" :face fanyi-longman-grammar-face :space t)
    ("LEVEL" :face fanyi-longman-dot-face :space t)
    ("REGISTERLAB" :face fanyi-longman-registerlab-face :space t)
    ("GEO" :face fanyi-longman-geo-face :space t)
    ("geo" :face fanyi-longman-geo-face)
    ("GLOSS" :face fanyi-longman-geo-face)
    ("NodeW" :face bold)
    ("sensenum" :face bold :space t)
    ("LEXUNIT" :face bold :space t)
    ("LEXVAR" :face bold :space t)
    ("REFHWD" :face bold)
    ("PTandPPX" :face bold)
    ("PRESPARTX" :face bold)
    ("FULLFORM" :face bold)
    ("PronCodes" :space t)
    ("FREQ" :tag fanyi-label)
    ("AC" :space t)
    ("DEF" :space t)
    ("DATE" :space t)
    ("CENTURY" :space t)
    ("LANG" :space t)
    ("Etymon" :space t)
    ("ORIGIN" :space t)
    ("TRAN" :space t))
  "Small subset of Longman's CSS expressed as SHR layout and Emacs faces.
Based on user-saved dragon, sun and explode pages; see test/fixtures/README.md.
Source and corpus headings, buttons and inline SVG labels
retain Fanyi's requested presentation.  This is not a CSS interpreter.")

(declare-function fanyi-dwim "fanyi")
(autoload 'eww "eww")

(define-button-type 'fanyi-longman-button
  'face nil)

(defun fanyi-longman--label-color (face attribute &optional frame)
  "Return FACE's color ATTRIBUTE on FRAME, using the dictionary text fallback."
  (face-attribute face attribute frame '(fanyi-longman-body-face default)))

(defun fanyi-longman-svg-tag-make (text face)
  "Create a rounded SVG label displaying TEXT with FACE.
A boxed FACE produces a transparent badge with a one-pixel outline."
  (let* ((height (window-font-height))
         (padding (window-font-width))
         (width (* padding (+ (string-width text) 2)))
         (foreground (fanyi-longman--label-color face :foreground))
         (box (face-attribute face :box nil t))
         (border (and box (not (eq box 'unspecified))
                      (or (and (stringp box) box)
                          (and (listp box) (plist-get box :color))
                          foreground)))
         (inset (if border 0.5 0))
         (svg (svg-create width height)))
    (svg-rectangle svg inset inset (- width (* 2 inset)) (- height (* 2 inset))
                   :rx fanyi-longman-svg-border-radius
                   :fill (if border "none" (fanyi-longman--label-color face :background))
                   :stroke (or border "none") :stroke-width (if border 1 0))
    (svg-text svg text :x padding :y (* height 0.75)
              :font-family (face-attribute 'default :family nil t)
              :font-size (* height 0.75)
              :font-weight (if (eq (face-attribute face :weight nil t) 'bold) "bold" "normal")
              :fill foreground)
    (svg-image svg :ascent 'center)))

(defun fanyi-longman--class-p (node class)
  "Return non-nil when NODE has the exact CSS CLASS."
  (member class (split-string (or (dom-attr node 'class) ""))))

(defun fanyi-longman--nodes (dom class)
  "Return nodes in DOM with CSS CLASS."
  (dom-search dom (lambda (node) (fanyi-longman--class-p node class))))

(defun fanyi-longman--http-url (value base)
  "Resolve a nonblank HTTP(S) VALUE against BASE, or return nil."
  (when (and (stringp value) (not (string-empty-p (string-trim value))))
    (let ((url (url-expand-file-name (string-trim value) base)))
      (when (member (url-type (url-generic-parse-url url)) '("http" "https"))
        url))))

(defun fanyi-longman--crossref-target (href)
  "Return the decoded dictionary word for HREF, or nil for other links."
  (when (and (stringp href)
             (string-match
              "\\`\\(?:https?://\\(?:www\\.\\)?ldoceonline\\.com\\)?/dictionary/\\([^?#]+\\)"
              href))
    (decode-coding-string (url-unhex-string (match-string 1 href)) 'utf-8)))

(defvar fanyi-longman--query-link nil
  "Word and original URL of the dictionary link currently being followed.")

(defun fanyi-longman--prepare-request (request)
  "Keep the original link URL when preparing its REQUEST."
  (when (and fanyi-longman--query-link
             (equal (fanyi-request-word request) (downcase (car fanyi-longman--query-link))))
    (setf (fanyi-request-url request) (copy-sequence (cdr fanyi-longman--query-link)))))

(defun fanyi-longman--query (link)
  "Query LINK's word, retaining its original URL for Longman."
  (let ((fanyi-longman--query-link link))
    (fanyi-dwim (car link))))

(defun fanyi-longman--follow-anchor (url)
  "Jump to URL's anchor in the current result, or open URL in EWW."
  (let ((target (decode-coding-string
                 (url-unhex-string (url-target (url-generic-parse-url url))) 'utf-8))
        position)
    (save-restriction
      (widen)
      (let ((cursor (point-min)))
        (while (and (< cursor (point-max)) (not position))
          (when (member target (get-text-property cursor 'shr-target-id))
            (setq position cursor))
          (setq cursor (next-single-property-change cursor 'shr-target-id nil (point-max))))))
    (if (not position)
        (eww url)
      (when (or (< position (point-min)) (>= position (point-max))) (widen))
      (goto-char position)
      (skip-chars-forward " \t\r\n")
      (dolist (overlay (overlays-at (point)))
        (when-let* ((open (overlay-get overlay 'isearch-open-invisible)))
          (funcall open overlay)))
      (when (outline-invisible-p) (outline-show-entry)))))

(defun fanyi-longman--entry-p (entry base)
  "Return non-nil if ENTRY has a definition or usable cross-reference."
  (or (seq-some (lambda (node) (not (string-empty-p (string-trim (fanyi-shr-dom-text node)))))
                (fanyi-longman--nodes entry "DEF"))
      (seq-some
       (lambda (node)
         (seq-some (lambda (link)
                     (and (not (string-empty-p (string-trim (fanyi-shr-dom-text link))))
                          (fanyi-longman--crossref-target
                           (fanyi-longman--http-url (dom-attr link 'href) base))))
                   (dom-by-tag node 'a)))
       (fanyi-longman--nodes entry "Crossref"))))

(defun fanyi-longman--asset-heading (node)
  "Copy NODE, joining its asset label and nested title into one heading."
  (let* ((copy (copy-tree node))
         (intro (car (fanyi-longman--nodes copy "asset_intro")))
         (title (car (fanyi-longman--nodes copy "title"))))
    (when (and intro title (not (memq title (fanyi-longman--nodes intro "title"))))
      (dom-remove-node copy title)
      (dom-append-child intro " ")
      (dom-append-child intro title))
    copy))

(defun fanyi-longman--group-assets (children)
  "Group an asset label with its following assetlink in CHILDREN.
The saved page puts the corpus label and title in sibling containers."
  (let (result)
    (while children
      (let ((node (pop children)) (rest children))
        (while (and (stringp (car rest)) (string-empty-p (string-trim (car rest))))
          (setq rest (cdr rest)))
        (if (and (consp node) (fanyi-longman--class-p node "asset")
                 (consp (car rest)) (fanyi-longman--class-p (car rest) "assetlink")
                 (fanyi-longman--nodes (car rest) "title"))
            (progn
              (push `(span ((class . "asset")) ,node ,(car rest)) result)
              (setq children (cdr rest)))
          (push node result))))
    (nreverse result)))

(defun fanyi-longman--node-context (node classes context)
  "Extend CONTEXT with the semantic sections introduced by NODE and CLASSES."
  (append (when (member "wordfams" classes) '(family))
          (when (eq (dom-tag node) 'a) '(link))
          (when (member "etym" classes) '(origin))
          (when (member "Sense" classes) '(sense))
          (when (member "F2NBox" classes) '(note))
          (when (member "asset_intro" classes) '(asset-heading))
          (when (cl-intersection classes '("exaGroup" "corpus") :test #'equal)
            '(corpus))
          (when (and (member "Head" classes)
                     (fanyi-longman--visible-hyphenation-p node))
            '(hyphenated))
          context))

(defun fanyi-longman--visible-hyphenation-p (node &optional hyphenated)
  "Whether NODE contains visible, nonblank hyphenated headword text.
HYPHENATED means NODE is inside a HYPHENATION element.  Check ancestor
visibility as well as the element itself before replacing the plain headword."
  (if (stringp node)
      (and hyphenated (string-match-p "[^[:space:]\u00a0]" node))
    (let ((classes (split-string (or (dom-attr node 'class) ""))))
      (unless (fanyi-longman--hidden-node-p node classes nil)
        (seq-some (lambda (child)
                    (fanyi-longman--visible-hyphenation-p
                     child (or hyphenated (member "HYPHENATION" classes))))
                  (dom-children node))))))

(defun fanyi-longman--hidden-node-p (node classes context)
  "Return non-nil if NODE with CLASSES is omitted in CONTEXT."
  (or (memq (dom-tag node) '(script style iframe form input img svg
                                    object embed audio video source link meta base))
      (and (eq (dom-tag node) 'button) (not (dom-attr node 'data-src-mp3)))
      (cl-intersection classes '("FIELD" "ACTIV" "suppressed")
                       :test #'equal)
      (and (memq 'hyphenated context) (member "HWD" classes))
      (dom-attr node 'hidden)
      (equal (dom-attr node 'aria-hidden) "true")
      (string-match-p "display[ \t]*:[ \t]*none" (or (dom-attr node 'style) ""))))

(defun fanyi-longman--prepare-link (node base context)
  "Return NODE's tag and copied attributes, resolving links against BASE.
A word-family anchor without a URL becomes a query link in CONTEXT."
  (let ((tag (dom-tag node))
        (attributes (copy-tree (dom-attributes node))))
    (when (eq tag 'a)
      (setf (alist-get 'href attributes)
            (fanyi-longman--http-url (dom-attr node 'href) base))
      (when-let* ((href (alist-get 'href attributes))
                  (word (fanyi-longman--crossref-target href)))
        (let ((title (string-trim (or (dom-attr node 'title) "")))
              (label (replace-regexp-in-string "[ \t\r\n]+" " "
                                               (string-trim (fanyi-shr-dom-text node)))))
          (setf (alist-get 'word attributes)
                (cond ((not (string-empty-p title)) title)
                      ((equal (replace-regexp-in-string " " "-" (downcase label)) word) label)
                      (t word))))
        (when (and (url-target (url-generic-parse-url href))
                   (equal word (fanyi-longman--crossref-target base)))
          (setf (alist-get 'fanyi-local-anchor attributes) t)))
      (when (and (memq 'family context) (not (dom-attr node 'href)))
        (setq tag 'fanyi-query)
        (setf (alist-get 'word attributes) (string-trim (fanyi-shr-dom-text node)))))
    (cons tag attributes)))

(defun fanyi-longman--class-style (classes)
  "Merge rules matching CLASSES, keeping the first value for each property.
Rule order determines precedence, independently of the order in CLASSES.
An explicit nil value also takes precedence over later rules."
  (let (style)
    (dolist (rule fanyi-longman--class-styles)
      (when (member (car rule) classes)
        (cl-loop for (property value) on (cdr rule) by #'cddr
                 unless (plist-member style property)
                 do (setq style (plist-put style property value)))))
    style))

(defun fanyi-longman--node-style (tag classes context audio)
  "Apply CONTEXT and AUDIO choices to the base styling for CLASSES.
TAG is the original HTML tag: some contexts deliberately bypass the class
renderer.  Use the class tag only when no contextual branch applies."
  (let ((style (fanyi-longman--class-style classes)))
    (cond
     (audio (setq tag 'fanyi-audio))
     ((member "etym" classes) (setq tag 'fanyi-origin))
     ((and (memq 'origin context) (member "asset_intro" classes))
      (setq tag 'fanyi-origin-label))
     ((and (memq 'family context) (member "asset_intro" classes))
      (setq tag 'fanyi-asset-label))
     ((and (memq 'origin context) (member "HOMNUM" classes))
      (setq tag 'fanyi-homonym style (plist-put style :face 'bold)))
     ((memq 'origin context)
      ;; Keep emphasis and links while laying out the origin inline.
      (when (memq tag '(div p h1 h2 h3 h4 h5 h6 br))
        (setq tag 'span)))
     ((and (member "Crossref" classes) (memq 'sense context))
      (setq tag 'span))
     ((and (member "title" classes) (memq 'corpus context)
           (not (memq 'asset-heading context)))
      (setq tag 'fanyi-block
            style (plist-put style :face 'fanyi-longman-heading-face)
            style (plist-put style :indent 1)))
     ((and (member "HEADING" classes) (member "newline" classes))
      (setq tag 'fanyi-line))
     ((and (memq 'note context) (member "heading" classes))
      (setq style (plist-put style :face 'fanyi-longman-note-heading-face)))
     ((plist-get style :tag) (setq tag (plist-get style :tag))))
    (plist-put style :tag tag)))

(defun fanyi-longman--association-link (children)
  "Make a local query from an unambiguous term in prepared CHILDREN.
Keep leading badges and punctuation outside the button, retaining inline
markup inside it.  Existing links, lists, qualifiers and prose remain intact.
Only alphabetic terms of up to four words, without and/or, are inferred."
  (let ((body (copy-sequence children)) prefix suffix)
    (while (and body
                (let ((node (car body)))
                  (cond
                   ((and (stringp node) (string-match "\\`[^[:alnum:]]+" node))
                    (push (match-string 0 node) prefix)
                    (setcar body (substring node (match-end 0)))
                    (when (string-empty-p (car body)) (setq body (cdr body)))
                    t)
                   ((and (not (stringp node))
                         (or (eq (dom-tag node) 'fanyi-synopp)
                             (and (not (dom-by-tag node 'a))
                                  (not (string-match-p "[[:alnum:]]" (fanyi-shr-dom-text node))))))
                    (push (pop body) prefix)
                    t)))))
    (when (and (stringp (car (last body)))
               (string-match "[[:space:],.;:!?]+\\'" (car (last body))))
      (setq suffix (list (match-string 0 (car (last body)))))
      (setcar (last body) (substring (car (last body)) 0 (match-beginning 0))))
    (let* ((dom `(span nil ,@body))
           (word (replace-regexp-in-string "[[:space:]]+" " "
                                           (string-trim (fanyi-shr-dom-text dom)))))
      (if (and (string-match-p
                "\\`[[:alpha:]][[:alpha:]’'-]*\\(?: [[:alpha:]][[:alpha:]’'-]*\\)\\{0,3\\}\\'" word)
               (not (string-match-p "\\b\\(?:and\\|or\\)\\b" word))
               (not (dom-search
                     dom (lambda (node)
                           (or (not (memq (dom-tag node)
                                          '(span fanyi-face b strong i em u s del)))
                               (cl-intersection
                                (split-string (or (dom-attr node 'class) ""))
                                '("GEO" "REGISTERLAB" "POS" "GRAM") :test #'equal))))))
          (append (nreverse prefix) (list `(fanyi-query ((word . ,word)) ,@body)) suffix)
        children))))

(defun fanyi-longman--association-children (children classes linked)
  "Prepare association CHILDREN with CLASSES as labeled local queries.
If LINKED is non-nil, preserve the enclosing anchor without nesting a query."
  (let* ((label (seq-find (lambda (class) (member class classes)) '("SYN" "OPP")))
         (children (copy-sequence children))
         embedded-label)
    (when (and label (not (dom-by-tag `(span nil ,@children) 'fanyi-synopp)))
      ;; Some responses supply the label as plain text, or omit it entirely.
      (while (and (stringp (car children)) (string-blank-p (car children)))
        (setq children (cdr children)))
      (cond
       ((and (stringp (car children))
             (string-match (concat "\\`[[:space:]]*" label "\\(?:[[:space:]]+\\|\\'\\)")
                           (car children)))
        (setcar children (substring (car children) (match-end 0))))
       ((and (consp (car children))
             (equal (string-trim (fanyi-shr-dom-text (car children))) label))
        (setq children (cdr children)))
       ((string-match-p (concat "\\`" label "\\(?:[[:space:]]\\|\\'\\)")
                        (string-trim (fanyi-shr-dom-text `(span nil ,@children))))
        ;; Do not duplicate a label buried in unfamiliar inline markup.
        (setq embedded-label t)))
      (unless embedded-label (push `(fanyi-synopp ((class . "synopp")) ,label) children)))
    (unless (or linked embedded-label)
      (setq children (fanyi-longman--association-link children)))
    (if label (append '(" ") children '(" ")) children)))

(defun fanyi-longman--prepare-node (node base &optional context)
  "Copy NODE into a reusable SHR DOM, resolving URLs against BASE.
CONTEXT records the semantic sections enclosing NODE.
Keep ordinary HTML and unfamiliar inline markup for shr to render."
  (if (stringp node)
      node
    (let* ((classes (split-string (or (dom-attr node 'class) "")))
           (context (fanyi-longman--node-context node classes context)))
      (unless (fanyi-longman--hidden-node-p node classes context)
        (let* ((node (if (cl-intersection classes '("asset" "corpus") :test #'equal)
                         (fanyi-longman--asset-heading node)
                       node))
               (link (fanyi-longman--prepare-link node base context))
               (attributes (cdr link))
               (audio (fanyi-longman--http-url (dom-attr node 'data-src-mp3) base))
               (style (fanyi-longman--node-style (car link) classes context audio))
               (tag (plist-get style :tag))
               (face (plist-get style :face))
               (children (delq nil (mapcar (lambda (child)
                                             (fanyi-longman--prepare-node child base context))
                                           (fanyi-longman--group-assets (dom-children node))))))
          (when (plist-get style :indent)
            (setf (alist-get 'fanyi-indent attributes) (plist-get style :indent)))
          (when audio (setf (alist-get 'src attributes) audio))
          (when (and (eq tag 'fanyi-origin)
                     (not (fanyi-longman--nodes node "asset_intro")))
            (push '(fanyi-origin-label nil "Origin") children))
          (when (plist-get style :space)
            (setq children (append '(" ") children '(" "))))
          (when (cl-intersection classes '("SYN" "OPP" "RELATEDWD") :test #'equal)
            (setq children (fanyi-longman--association-children children classes (memq 'link context))))
          (let ((prepared (cons tag (cons attributes children))))
            ;; Styling wraps the node so anchors and native emphasis keep
            ;; their own rendering semantics.
            (if face `(fanyi-face ((face . ,face)) ,prepared) prepared)))))))

(defun fanyi-longman--sections (node)
  "Select nonoverlapping dictionary sections under NODE in document order."
  (if (seq-some (lambda (class) (fanyi-longman--class-p node class))
                '("wordfams" "dictentry" "etym"))
      (list node)
    (cl-mapcan #'fanyi-longman--sections (dom-non-text-children node))))

(defun fanyi-longman-parse (dom request)
  "Select, prepare and validate reusable dictionary DOM for REQUEST.
The returned DOM is ready for SHR; DOM itself is not modified.
Throw `not-found' for a recognizable spelling-suggestion page; other
responses without usable entries signal `fanyi-parse-error'."
  (let* ((base (fanyi-request-url request))
         (dictionary (car (fanyi-longman--nodes dom "dictionary")))
         sections has-entry)
    (dolist (node (fanyi-longman--sections dictionary))
      (let ((entry (fanyi-longman--class-p node "dictentry"))
            (prepared (fanyi-longman--prepare-node node base)))
        (when (and prepared (or (not entry) (fanyi-longman--entry-p prepared base)))
          (when entry (setq has-entry t))
          (push prepared sections))))
    (unless has-entry
      ;; Only a visible suggestion list with dictionary links establishes a
      ;; normal missing entry.  Empty or unfamiliar pages remain parse errors.
      ;; Preparing the whole tree here also rejects hidden ancestor elements.
      (if (seq-some
           (lambda (node)
             (seq-some
              (lambda (link)
                (and (fanyi-longman--crossref-target (dom-attr link 'href))
                     (not (string-empty-p (string-trim (fanyi-shr-dom-text link))))))
              (dom-by-tag node 'a)))
           (fanyi-longman--nodes (fanyi-longman--prepare-node dom base) "didyoumean"))
          (throw 'not-found nil)
        (signal 'fanyi-parse-error '("Longman response contains no usable dictionary entries"))))
    `(div nil ,@(nreverse sections))))

(defun fanyi-longman--shr-heading (dom)
  "Render DOM as a Fanyi outline heading."
  (shr-ensure-paragraph)
  (let ((start (point)))
    (insert "## ")
    (shr-generic dom)
    ;; Include the line break so the source banner extends to the window edge.
    (shr-ensure-newline)
    (add-face-text-property
     start (point)
     (list (if (fanyi-longman--class-p dom "dictionary_intro")
               'fanyi-longman-dictionary-face
             'fanyi-longman-heading-face)
           'fanyi-sub-headline-face)
     nil))
  (shr-ensure-paragraph))

(defun fanyi-longman--shr-face (dom)
  "Render DOM with its semantic face."
  (let ((start (point)))
    (shr-generic dom)
    (let ((face (dom-attr dom 'face)))
      ;; Semantic sizes/colors take precedence over SHR's base font.
      ;; The document default is deliberately the lowest-priority face.
      (add-face-text-property start (point) face
                              (eq face 'fanyi-longman-body-face)))))

(defun fanyi-longman--shr-line (dom)
  "Render DOM on a separate line."
  (shr-ensure-newline)
  (let ((shr-indentation (+ shr-indentation (or (dom-attr dom 'fanyi-indent) 0))))
    (shr-generic dom))
  (shr-ensure-newline))

(defun fanyi-longman--shr-block (dom)
  "Render DOM as a paragraph with optional indentation."
  (shr-ensure-paragraph)
  (fanyi-longman--shr-line dom)
  (shr-ensure-paragraph))

(defun fanyi-longman--shr-section (dom)
  "Render DOM as a supplementary section with a locally folding heading."
  (let ((children (dom-children dom)) heading)
    ;; Some headings have a sibling qualifier, such as "Meanings 1 & 2".
    ;; Keep that complete heading visible when the section is closed.
    (while (and children
                (let ((child (car children)))
                  (if (stringp child)
                      (string-empty-p (string-trim child))
                    (let ((node (if (eq (dom-tag child) 'fanyi-face)
                                    (car (dom-children child)) child)))
                      (or (fanyi-longman--class-p node "heading")
                          (fanyi-longman--class-p node "HEADING"))))))
      (push (pop children) heading))
    (if (not (seq-some #'consp heading))
        (fanyi-longman--shr-block dom)
      (shr-ensure-paragraph)
      (let ((shr-indentation (+ shr-indentation (or (dom-attr dom 'fanyi-indent) 0))))
        (fanyi-shr-insert-section dom `(span nil ,@(nreverse heading))
                                  `(div nil ,@children)))
      (shr-ensure-paragraph))))

(defun fanyi-longman--shr-label (dom &optional face uppercase)
  "Render DOM as an inline label using FACE, optionally in UPPERCASE."
  (shr-insert " ")
  (let ((start (point))
        (face (or face 'fanyi-longman-frequency-face)))
    (shr-generic dom)
    (when uppercase (upcase-region start (point)))
    (if (or (fanyi-longman--class-p dom "FREQ")
            (fanyi-longman--class-p dom "SIGNPOST")
            (fanyi-longman--class-p dom "synopp"))
        (fanyi-longman--badge start (point) face)
      (add-face-text-property start (point) face))
    (when (dom-attr dom 'title)
      (put-text-property start (point) 'help-echo (dom-attr dom 'title))))
  (shr-insert " "))

(defun fanyi-longman--shr-signpost (dom)
  "Render DOM as the site's uppercase orange sense badge."
  (fanyi-longman--shr-label dom 'fanyi-longman-signpost-face t))

(defun fanyi-longman--shr-synopp (dom)
  "Render DOM as an uppercase synonym or antonym badge."
  (fanyi-longman--shr-label dom 'fanyi-longman-synopp-face t))

(defun fanyi-longman--shr-group-heading (dom)
  "Render DOM as a separate collocation group label."
  (shr-ensure-paragraph)
  (fanyi-longman--shr-label dom 'fanyi-longman-group-heading-face t)
  (shr-ensure-newline))

(defun fanyi-longman--shr-homonym (dom)
  "Render DOM as a superscript number at the surrounding body text size."
  ;; SHR's `sup' also shrinks the text, unlike Longman's HOMNUM rule.
  (let ((start (point)))
    (shr-generic dom)
    (put-text-property start (point) 'display '(raise 0.2))))

(defun fanyi-longman--shr-sense (dom)
  "Render DOM with the site's sense indentation and paragraph spacing."
  (shr-ensure-newline)
  (let ((shr-indentation (+ shr-indentation 2)))
    (shr-generic dom))
  (shr-ensure-paragraph))

(defun fanyi-longman--badge (start end face)
  "Style the text between START and END as a badge using FACE.
Keep the underlying text available for searching and copying."
  (add-face-text-property start end face)
  (when (and (< start end) (fanyi-display-glyphs-p) (image-type-available-p 'svg))
    (put-text-property start end 'display
                       (fanyi-longman-svg-tag-make
                        (buffer-substring-no-properties start end) face))
    ;; Transparent SVG corners must reveal the surrounding text background,
    ;; not the filled semantic face retained for the text fallback.
    (add-face-text-property
     start end
     `(:box nil :background ,(face-attribute 'fanyi-longman-body-face
                                             :background nil 'default)))))

(defun fanyi-longman--shr-asset-label (dom)
  "Render DOM as an inline asset badge, with a styled text fallback."
  (let ((start (point)))
    (shr-generic dom)
    (fanyi-longman--badge start (point) 'fanyi-longman-svg-asset-face)
    (shr-insert " ")))

(defun fanyi-longman--shr-origin (dom)
  "Render DOM as one origin paragraph."
  (shr-ensure-paragraph)
  (shr-generic dom)
  (shr-ensure-paragraph))

(defun fanyi-longman--shr-audio (dom)
  "Insert a pronunciation button for DOM without starting playback."
  (shr-insert " ")
  (let ((start (point))
        (speaker-face (cond ((fanyi-longman--class-p dom "brefile") 'fanyi-longman-british-speaker-face)
                            ((fanyi-longman--class-p dom "amefile") 'fanyi-longman-american-speaker-face))))
    ;; SHR must see the first character to mark this line for indentation
    ;; and wrapping, including examples that begin with a speaker button.
    (shr-insert "🔊")
    (make-text-button start (point)
                      'type 'fanyi-longman-button
                      'action #'fanyi-play-sound
                      'button-data (dom-attr dom 'src)
                      'fanyi-audio-kind (and speaker-face 'pronunciation)
                      'fanyi-audio-dialect (cond ((fanyi-longman--class-p dom "brefile") 'british)
                                                 ((fanyi-longman--class-p dom "amefile") 'american))
                      'face (or speaker-face 'fanyi-longman-example-face)
                      'display (and speaker-face (fanyi-speaker-image speaker-face))
                      'help-echo (cond ((fanyi-longman--class-p dom "brefile") "Play British pronunciation")
                                       ((fanyi-longman--class-p dom "amefile") "Play American pronunciation")
                                       (t "Play example"))
                      'follow-link t))
  (shr-insert " ")
  (shr-generic dom))

(defun fanyi-longman--shr-link (dom)
  "Render DOM as a query, an EWW topic link or an external browser link."
  (let* ((word (or (dom-attr dom 'word)
                   (fanyi-longman--crossref-target (dom-attr dom 'href))))
         (url (or (dom-attr dom 'href)
                  (and word (concat "https://www.ldoceonline.com/dictionary/"
                                    (url-hexify-string word)))))
         (start (point)))
    (shr-generic dom)
    (when (and url (> (point) start))
      (shr-urlify start url)
      (let ((position start) next faces
            (style (if (fanyi-longman--class-p dom "topic")
                       'fanyi-longman-topic-face
                     'fanyi-longman-definition-link-face)))
        ;; Replace SHR's link color, preserving emphasis within the anchor.
        (while (< position (point))
          (setq next (next-single-property-change position 'face nil (point))
                faces (get-text-property position 'face))
          (unless (and (listp faces) (not (keywordp (car faces))))
            (setq faces (list faces)))
          (put-text-property position next 'face
                             (cons style (remq 'shr-link (copy-sequence faces))))
          (setq position next)))
      (make-text-button start (point)
                        'type 'fanyi-longman-button
                        'action (cond ((dom-attr dom 'fanyi-local-anchor)
                                       #'fanyi-longman--follow-anchor)
                                      (word #'fanyi-longman--query)
                                      ((member (url-host (url-generic-parse-url url))
                                               '("ldoceonline.com" "www.ldoceonline.com"))
                                       #'eww)
                                      (t #'browse-url))
                        'button-data (if (and word (not (dom-attr dom 'fanyi-local-anchor)))
                                         (cons word url) url)
                        'keymap button-map
                        'follow-link t))))

(defun fanyi-longman--shr-example (dom)
  "Render DOM as an indented example."
  (shr-ensure-newline)
  (let ((shr-indentation (+ shr-indentation
                            (if (fanyi-longman--class-p dom "exa")
                                2
                              fanyi-longman-example-indent)))
        (start (point)))
    (when (and (fanyi-longman--class-p dom "exa")
               (not (string-prefix-p "•" (string-trim-left (fanyi-shr-dom-text dom)))))
      (shr-insert "• "))
    (shr-generic dom)
    (add-face-text-property start (point) 'fanyi-longman-example-face)
    (shr-ensure-newline)))

(defun fanyi-longman--render-style (window)
  "Return the settings used to render Longman labels and examples in WINDOW."
  (let ((frame (window-frame window)))
    (list fanyi-use-glyphs fanyi-longman-svg-border-radius fanyi-longman-example-indent
          (display-images-p frame) (image-type-available-p 'svg) (image-type-available-p 'xpm)
          (window-font-width window) (window-font-height window)
          (face-attribute 'default :family frame t)
          (face-attribute 'fanyi-longman-body-face :background frame 'default)
          (face-attribute 'fanyi-longman-british-speaker-face :foreground frame 'default)
          (face-attribute 'fanyi-longman-american-speaker-face :foreground frame 'default)
          (mapcar (lambda (face)
                    (mapcar (lambda (attribute)
                              (if (memq attribute '(:foreground :background))
                                  (fanyi-longman--label-color face attribute frame)
                                (face-attribute face attribute frame t)))
                            '(:foreground :background :weight :box)))
                  '(fanyi-longman-frequency-face fanyi-longman-svg-asset-face
                                                 fanyi-longman-signpost-face
                                                 fanyi-longman-synopp-face)))))

(defun fanyi-longman-render (content request)
  "Insert prepared CONTENT at point with Fanyi headings and actions."
  (insert "# " (fanyi-request-display-name request) "\n\n")
  (fanyi-shr-insert
   `(fanyi-face ((face . fanyi-longman-body-face)) ,content)
   '((a . fanyi-longman--shr-link)
     (fanyi-query . fanyi-longman--shr-link)
     (fanyi-audio . fanyi-longman--shr-audio)
     (fanyi-heading . fanyi-longman--shr-heading)
     (fanyi-face . fanyi-longman--shr-face)
     (fanyi-line . fanyi-longman--shr-line)
     (fanyi-block . fanyi-longman--shr-block)
     (fanyi-section . fanyi-longman--shr-section)
     (fanyi-label . fanyi-longman--shr-label)
     (fanyi-signpost . fanyi-longman--shr-signpost)
     (fanyi-synopp . fanyi-longman--shr-synopp)
     (fanyi-group-heading . fanyi-longman--shr-group-heading)
     (fanyi-homonym . fanyi-longman--shr-homonym)
     (fanyi-sense . fanyi-longman--shr-sense)
     (fanyi-example . fanyi-longman--shr-example)
     (fanyi-origin . fanyi-longman--shr-origin)
     (fanyi-origin-label . fanyi-longman--shr-asset-label)
     (fanyi-asset-label . fanyi-longman--shr-asset-label))
   #'fanyi-longman--render-style)
  (unless (bolp) (insert "\n"))
  (insert "\n"))

(defconst fanyi-longman-provider
  (fanyi-provider-create :name "Longman"
                         :url "https://www.ldoceonline.com/dictionary/%s"
                         :api-type 'xml
                         :prepare #'fanyi-longman--prepare-request
                         :parse #'fanyi-longman-parse
                         :render #'fanyi-longman-render)
  "Longman dictionary configuration.")

(provide 'fanyi-longman)
;;; fanyi-longman.el ends here
