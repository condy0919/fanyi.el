;;; fanyi-longman.el --- Longman dictionary service -*- lexical-binding: t -*-

;; Copyright (C) 2021 Zhiwei Chen
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/condy0919/fanyi.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

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

(require 'subr-x)
(require 'dom)
(require 'seq)
(require 'cl-lib)
(require 'button)
(require 'browse-url)
(require 'shr)
(require 'svg)

(defcustom fanyi-longman-svg-border-radius 7
  "Border radius in pixels for Longman labels."
  :type 'natnum
  :group 'fanyi)

(defcustom fanyi-longman-example-indent 2
  "Indentation of Longman examples rendered by shr."
  :type 'natnum
  :group 'fanyi)

(defface fanyi-longman-example-face
  '((t :inherit font-lock-doc-face))
  "Face used for examples."
  :group 'fanyi)

(defface fanyi-longman-svg-asset-face
  '((((background light)) :foreground "white" :background "#f1d600")
    (((background dark)) :foreground "black" :background "#a6a7bd"))
  "Face used for Longman SVG labels."
  :group 'fanyi)

(defface fanyi-longman-grammar-face
  '((t :foreground "green" :weight bold))
  "Face used for grammar."
  :group 'fanyi)

(defface fanyi-longman-dot-face
  '((t :foreground "red"))
  "Face used for level dots."
  :group 'fanyi)

(defface fanyi-longman-registerlab-face
  '((t :foreground "purple" :slant italic))
  "Face used for register labels."
  :group 'fanyi)

(defface fanyi-longman-geo-face
  '((t :foreground "#364395" :slant italic))
  "Face used for geographical labels."
  :group 'fanyi)

(defclass fanyi-longman-service (fanyi-base-service)
  ((content :initform nil :type list
            :documentation "Selected dictionary DOM, prepared for shr."))
  "The Longman dictionary service.")

(eieio-declare-slots :content)
(declare-function fanyi-dwim "fanyi")
(autoload 'eww "eww")

(defun fanyi-longman-svg-tag-make (text face)
  "Create a rounded SVG label displaying TEXT with FACE."
  (let* ((height (frame-char-height))
         (padding (frame-char-width))
         (width (* padding (+ (string-width text) 2)))
         (svg (svg-create width height)))
    (svg-rectangle svg 0 0 width height
                   :rx fanyi-longman-svg-border-radius
                   :fill (face-attribute face :background nil t))
    (svg-text svg text :x padding :y (* height 0.75)
              :font-family (face-attribute 'default :family nil t)
              :font-size (* height 0.75)
              :fill (face-attribute face :foreground nil t))
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

(defun fanyi-longman--entry-p (entry base)
  "Return non-nil if ENTRY has a definition or usable cross-reference."
  (or (seq-some (lambda (node) (not (string-empty-p (string-trim (dom-texts node "")))))
                (fanyi-longman--nodes entry "DEF"))
      (seq-some
       (lambda (node)
         (seq-some (lambda (link)
                     (and (not (string-empty-p (string-trim (dom-texts link ""))))
                          (fanyi-longman--crossref-target
                           (fanyi-longman--http-url (dom-attr link 'href) base))))
                   (dom-by-tag node 'a)))
       (fanyi-longman--nodes entry "Crossref"))))

(defun fanyi-longman--join-headings (children)
  "Join an asset heading and its following title in CHILDREN."
  (let (result)
    (while children
      (let ((node (pop children)))
        (when (and (consp node) (eq (dom-tag node) 'fanyi-heading))
          (while (and (stringp (car children))
                      (string-empty-p (string-trim (car children))))
            (pop children))
          (when (and (consp (car children))
                     (or (fanyi-longman--class-p (car children) "title")
                         (fanyi-longman--class-p (car children) "headword")))
            (setq node (append node (list " " (pop children))))))
        (push node result)))
    (nreverse result)))

(defun fanyi-longman--prepare-node (node base &optional context)
  "Copy NODE for shr, adapting CSS layout and resolving URLs against BASE.
CONTEXT records word-family, headword and origin sections enclosing NODE.
Keep ordinary HTML and unfamiliar inline markup for shr to render."
  (if (stringp node)
      node
    (let* ((tag (dom-tag node))
           (classes (split-string (or (dom-attr node 'class) "")))
           (context (append
                     (when (member "wordfams" classes) '(family))
                     (when (member "etym" classes) '(origin))
                     (when (and (member "Head" classes)
                                (fanyi-longman--nodes node "HYPHENATION"))
                       '(hyphenated))
                     context)))
      (unless (or (memq tag '(script style iframe form input img svg
                                     object embed audio video source link meta base))
                  (and (eq tag 'button) (not (dom-attr node 'data-src-mp3)))
                  (cl-intersection classes '("FIELD" "SIGNPOST") :test #'equal)
                  (and (memq 'hyphenated context) (member "HWD" classes))
                  (dom-attr node 'hidden)
                  (equal (dom-attr node 'aria-hidden) "true")
                  (string-match-p "display[ \t]*:[ \t]*none"
                                  (or (dom-attr node 'style) "")))
        (let* ((attributes (copy-tree (dom-attributes node)))
               (children (fanyi-longman--join-headings
                          (delq nil (mapcar (lambda (child)
                                              (fanyi-longman--prepare-node child base context))
                                            (dom-children node)))))
               (audio (fanyi-longman--http-url (dom-attr node 'data-src-mp3) base))
               (face (cdr (seq-find
                           (lambda (item) (member (car item) classes))
                           '(("POS" . fanyi-pos-face) ("pos" . fanyi-pos-face)
                             ("GRAM" . fanyi-longman-grammar-face)
                             ("LEVEL" . fanyi-longman-dot-face)
                             ("REGISTERLAB" . fanyi-longman-registerlab-face)
                             ("GEO" . fanyi-longman-geo-face)
                             ("GLOSS" . font-lock-string-face))))))
          ;; URL normalization avoids relying on shr's document-local base URL.
          (when (eq tag 'a)
            (setf (alist-get 'href attributes)
                  (fanyi-longman--http-url (dom-attr node 'href) base))
            (when (and (memq 'family context) (not (dom-attr node 'href)))
              (setq tag 'fanyi-query)
              (setf (alist-get 'word attributes) (string-trim (dom-texts node "")))))
          (cond
           (audio
            (setq tag 'fanyi-audio)
            (setf (alist-get 'src attributes) audio))
           ((member "etym" classes)
            (setq tag 'fanyi-origin)
            (unless (fanyi-longman--nodes node "asset_intro")
              (push '(fanyi-origin-label nil "Origin") children)))
           ((and (memq 'origin context) (member "asset_intro" classes))
            (setq tag 'fanyi-origin-label))
           (face
            (setq tag 'fanyi-face)
            (setf (alist-get 'face attributes) face))
           ((memq 'origin context)
            ;; Keep emphasis and links while laying out the origin inline.
            (when (memq tag '(div p h1 h2 h3 h4 h5 h6 br))
              (setq tag 'span)))
           ((cl-intersection classes '("dictionary_intro" "asset_intro") :test #'equal)
            (setq tag 'fanyi-heading))
           ((cl-intersection classes '("EXAMPLE" "exa") :test #'equal)
            (setq tag 'fanyi-example))
           ((member "Head" classes) (setq tag 'p))
           ((cl-intersection classes '("Sense" "Crossref") :test #'equal)
            (setq tag 'fanyi-line))
           ((cl-intersection classes '("title" "headword") :test #'equal)
            (setq tag 'span))
           ((or (member "HYPHENATION" classes) (member "LEXUNIT" classes))
            (setq tag 'b)))
          (when (cl-intersection classes '("PronCodes" "LEVEL" "FREQ" "AC" "GRAM"
                                           "REGISTERLAB" "GEO" "pos" "POS" "Head"
                                           "LEXUNIT" "DEF" "Crossref" "sensenum"
                                           "DATE" "LANG" "Etymon" "GLOSS") :test #'equal)
            (setq children (append '(" ") children '(" "))))
          (when (member "SYN" classes)
            (setq children
                  (append (if (string-match-p "\\`SYN\\(?:[[:space:]]\\|\\'\\)"
                                              (string-trim (dom-texts node "")))
                              '(" ") '(" SYN "))
                          children '(" "))))
          (cons tag (cons attributes children)))))))

(defun fanyi-longman--sections (node)
  "Select nonoverlapping dictionary sections under NODE in document order."
  (if (seq-some (lambda (class) (fanyi-longman--class-p node class))
                '("wordfams" "dictentry" "etym"))
      (list node)
    (cl-mapcan #'fanyi-longman--sections (dom-non-text-children node))))

(cl-defmethod fanyi-parse-from ((this fanyi-longman-service) dom)
  "Select usable dictionary content from DOM for THIS."
  (let* ((base (format (oref this url) (oref this word)))
         (dictionary (car (fanyi-longman--nodes dom "dictionary")))
         (content (and dictionary (fanyi-longman--prepare-node dictionary base)))
         (sections (seq-filter
                    (lambda (node)
                      (or (not (fanyi-longman--class-p node "dictentry"))
                          (fanyi-longman--entry-p node base)))
                    (fanyi-longman--sections content))))
    (unless (seq-some (lambda (node) (fanyi-longman--class-p node "dictentry")) sections)
      (signal 'fanyi-parse-error '("Longman response contains no usable dictionary entries")))
    (oset this content `(div nil ,@sections))))

(defun fanyi-longman--shr-heading (dom)
  "Render DOM as a Fanyi outline heading."
  (shr-ensure-paragraph)
  (insert "## ")
  (shr-generic dom)
  (shr-ensure-paragraph))

(defun fanyi-longman--shr-face (dom)
  "Render DOM with its semantic face."
  (let ((start (point)))
    (shr-generic dom)
    (add-face-text-property start (point) (dom-attr dom 'face) t)))

(defun fanyi-longman--shr-line (dom)
  "Render DOM on a separate line."
  (shr-ensure-newline)
  (shr-generic dom)
  (shr-ensure-newline))

(defun fanyi-longman--shr-origin-label (dom)
  "Render DOM as an Origin badge, with a text fallback."
  (let ((start (point)))
    (shr-generic dom)
    (when (and (fanyi-display-glyphs-p) (image-type-available-p 'svg))
      (put-text-property
       start (point) 'display
       (fanyi-longman-svg-tag-make
        (buffer-substring-no-properties start (point))
        'fanyi-longman-svg-asset-face)))
    (shr-insert " ")))

(defun fanyi-longman--shr-origin (dom)
  "Render DOM as one origin paragraph."
  (shr-ensure-paragraph)
  (shr-generic dom)
  (shr-ensure-paragraph))

(defun fanyi-longman--shr-audio (dom)
  "Insert a pronunciation button for DOM without starting playback."
  (shr-insert " ")
  (insert-button "🔊"
                 'action #'fanyi-play-sound
                 'button-data (dom-attr dom 'src)
                 'face (cond ((fanyi-longman--class-p dom "brefile") 'fanyi-female-speaker-face)
                             ((fanyi-longman--class-p dom "amefile") 'fanyi-male-speaker-face)
                             (t 'fanyi-longman-example-face))
                 'help-echo (cond ((fanyi-longman--class-p dom "brefile") "Play British pronunciation")
                                  ((fanyi-longman--class-p dom "amefile") "Play American pronunciation")
                                  (t "Play example"))
                 'follow-link t)
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
      (make-text-button start (point)
                        'action (cond (word #'fanyi-dwim)
                                      ((member (url-host (url-generic-parse-url url))
                                               '("ldoceonline.com" "www.ldoceonline.com"))
                                       #'eww)
                                      (t #'browse-url))
                        'button-data (or word url)
                        'keymap button-map
                        'follow-link t))))

(defun fanyi-longman--shr-example (dom)
  "Render DOM as an indented example."
  (shr-ensure-newline)
  (let ((shr-indentation (if (fanyi-longman--class-p dom "exa")
                             shr-indentation
                           (+ shr-indentation fanyi-longman-example-indent)))
        (start (point)))
    (when (and (fanyi-longman--class-p dom "exa")
               (not (string-prefix-p "•" (string-trim-left (dom-texts dom "")))))
      (shr-insert "• "))
    (shr-generic dom)
    (add-face-text-property start (point) 'fanyi-longman-example-face t)
    (shr-ensure-newline)))

(defun fanyi-longman--preserve-faces ()
  "Keep SHR's styling when Fanyi's font-lock clears and rebuilds `face'."
  (let ((start (point-min)) end)
    (while (< start (point-max))
      (setq end (next-single-property-change start 'face nil (point-max)))
      (when (get-text-property start 'face)
        (put-text-property start end 'font-lock-face
                           (get-text-property start 'face)))
      (setq start end))))

(cl-defmethod fanyi-render ((this fanyi-longman-service))
  "Render THIS with shr, adapting only Fanyi headings and actions."
  (fanyi-with-fanyi-buffer
    (insert "# Longman\n\n")
    (let ((shr-use-fonts nil)
          (shr-use-colors nil)
          (shr-inhibit-images t)
          (shr-external-rendering-functions
           '((a . fanyi-longman--shr-link)
             (fanyi-query . fanyi-longman--shr-link)
             (fanyi-audio . fanyi-longman--shr-audio)
             (fanyi-heading . fanyi-longman--shr-heading)
             (fanyi-face . fanyi-longman--shr-face)
             (fanyi-line . fanyi-longman--shr-line)
             (fanyi-example . fanyi-longman--shr-example)
             (fanyi-origin . fanyi-longman--shr-origin)
             (fanyi-origin-label . fanyi-longman--shr-origin-label)))
          (hscroll (window-hscroll)))
      (unwind-protect
          (save-restriction
            ;; shr's filling and text properties must not affect earlier sources.
            (narrow-to-region (point) (point))
            (shr-insert-document (oref this content))
            (delete-trailing-whitespace (point-min) (point-max))
            (fanyi-longman--preserve-faces))
        (set-window-hscroll nil hscroll)))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "\n")))

(defconst fanyi-longman-provider
  (fanyi-longman-service :word "dummy"
                         :url "https://www.ldoceonline.com/dictionary/%s"
                         :sound-url "unused"
                         :api-type 'xml)
  "Longman dictionary service instance.")

(provide 'fanyi-longman)
;;; fanyi-longman.el ends here
