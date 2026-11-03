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

(defcustom fanyi-longman-example-indent 2
  "Indentation of Longman examples rendered by shr."
  :type 'natnum
  :group 'fanyi)

(defface fanyi-longman-example-face
  '((t :inherit font-lock-doc-face))
  "Face used for examples."
  :group 'fanyi)

(defclass fanyi-longman-service (fanyi-base-service)
  ((content :initform nil :type list
            :documentation "Selected dictionary DOM, prepared for shr."))
  "The Longman dictionary service.")

(eieio-declare-slots :content)
(declare-function fanyi-dwim "fanyi")

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

(defun fanyi-longman--prepare-node (node base &optional family)
  "Copy NODE for shr, adapting CSS layout and resolving URLs against BASE.
FAMILY marks word-family links that use text instead of an href.
Keep ordinary HTML and unfamiliar inline markup for shr to render."
  (if (stringp node)
      node
    (let* ((tag (dom-tag node))
           (classes (split-string (or (dom-attr node 'class) "")))
           (family (or family (member "wordfams" classes))))
      (unless (or (memq tag '(script style iframe form input img svg
                                     object embed audio video source link meta base))
                  (and (eq tag 'button) (not (dom-attr node 'data-src-mp3)))
                  (dom-attr node 'hidden)
                  (equal (dom-attr node 'aria-hidden) "true")
                  (string-match-p "display[ \t]*:[ \t]*none"
                                  (or (dom-attr node 'style) "")))
        (let* ((attributes (copy-tree (dom-attributes node)))
               (children (delq nil (mapcar (lambda (child)
                                             (fanyi-longman--prepare-node child base family))
                                           (dom-children node))))
               (audio (fanyi-longman--http-url (dom-attr node 'data-src-mp3) base)))
          ;; URL normalization avoids relying on shr's document-local base URL.
          (when (eq tag 'a)
            (setf (alist-get 'href attributes)
                  (fanyi-longman--http-url (dom-attr node 'href) base))
            (when (and family (not (dom-attr node 'href)))
              (setq tag 'fanyi-query)
              (setf (alist-get 'word attributes) (string-trim (dom-texts node "")))))
          (cond
           (audio
            (setq tag 'fanyi-audio)
            (setf (alist-get 'src attributes) audio))
           ((member "dictionary_intro" classes) (setq tag 'fanyi-heading))
           ((member "EXAMPLE" classes) (setq tag 'fanyi-example))
           ((member "Head" classes) (setq tag 'p))
           ((member "Sense" classes) (setq tag 'div))
           ((member "POS" classes)
            (setq tag 'i children (append '(" (") children '(") "))))
           ((or (member "HYPHENATION" classes) (member "LEXUNIT" classes)
                (member "SIGNPOST" classes))
            (setq tag 'b))
           ((member "etym" classes)
            (setq children (cons '(fanyi-heading nil "Etymon") children))))
          (when (cl-intersection classes '("PronCodes" "LEVEL" "FREQ" "AC" "GRAM"
                                           "REGISTERLAB" "GEO" "pos" "SIGNPOST"
                                           "LEXUNIT" "DEF" "Crossref") :test #'equal)
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

(defun fanyi-longman--shr-audio (dom)
  "Insert a pronunciation button for DOM without starting playback."
  (shr-insert " ")
  (insert-button "🔊"
                 'action #'fanyi-play-sound
                 'button-data (dom-attr dom 'src)
                 'help-echo (cond ((fanyi-longman--class-p dom "brefile") "Play British pronunciation")
                                  ((fanyi-longman--class-p dom "amefile") "Play American pronunciation")
                                  (t "Play example"))
                 'follow-link t)
  (shr-insert " ")
  (shr-generic dom))

(defun fanyi-longman--shr-link (dom)
  "Render DOM as a dictionary query or an ordinary browser link."
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
                        'action (if word #'fanyi-dwim #'browse-url)
                        'button-data (or word url)
                        'keymap button-map
                        'follow-link t))))

(defun fanyi-longman--shr-example (dom)
  "Render DOM as an indented example."
  (shr-ensure-paragraph)
  (let ((shr-indentation (+ shr-indentation fanyi-longman-example-indent))
        (start (point)))
    (shr-generic dom)
    (add-face-text-property start (point) 'fanyi-longman-example-face t)
    (shr-ensure-paragraph)))

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
            (fanyi-example . fanyi-longman--shr-example)))
         (hscroll (window-hscroll)))
     (unwind-protect
         (save-restriction
           ;; shr's filling and text properties must not affect earlier sources.
           (narrow-to-region (point) (point))
           (shr-insert-document (oref this content)))
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
