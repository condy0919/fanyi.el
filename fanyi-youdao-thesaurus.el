;;; fanyi-youdao-thesaurus.el --- Youdao thesaurus service -*- lexical-binding: t -*-

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
;; Youdao thesaurus service.

;;; Code:

(require 'fanyi-base)
(require 'fanyi-shr)

(require 'subr-x)
(require 'cl-lib)
(require 'button)
(require 'dom)

(declare-function fanyi-dwim "fanyi")

(defface fanyi-youdao-thesaurus-word-face
  '((t :inherit link :weight bold))
  "Face used for queryable Youdao entries."
  :group 'fanyi)

(define-button-type 'fanyi-youdao-thesaurus-button
  'face nil
  'follow-link t
  'action #'fanyi-dwim)

(defcustom fanyi-youdao-thesaurus-layout 'columns
  "Preferred layout for Youdao entries.
`columns' aligns words with explanations, switching to stacked entries
when there is too little room for the explanation.  `stacked' always
places explanations below their words."
  :type '(choice (const :tag "Columns when space permits" columns)
                 (const :tag "Stacked entries" stacked))
  :group 'fanyi)

(defcustom fanyi-youdao-thesaurus-max-count 10
  "Max count of entries displayed."
  :type 'integer
  :group 'fanyi)

(defcustom fanyi-youdao-thesaurus-indent 2
  "Preferred indentation of explanations in stacked entries.
Reduce it in narrow windows to leave at least eight columns for text,
or the full width when fewer than eight columns are available."
  :type 'natnum
  :group 'fanyi)

(defun fanyi-youdao-thesaurus-prepare-request (request)
  "Set the current result limit on REQUEST."
  (let* ((url (url-generic-parse-url (fanyi-request-url request)))
         (path (url-filename url))
         (limit (format "num=%d" fanyi-youdao-thesaurus-max-count)))
    ;; Preserve custom endpoints and other parameters on this request.
    (setf (url-filename url)
          (if (string-match-p "[?&]num=" path)
              (replace-regexp-in-string
               "[?&]num=[^&]*"
               (lambda (match) (concat (substring match 0 1) limit)) path t t)
            (concat path (if (string-match-p "?" path) "&" "?") limit)))
    (setf (fanyi-request-url request) (url-recreate-url url))))

(defun fanyi-youdao-thesaurus-parse (js _request)
  "Parse JS into entries, distinguishing empty results from invalid data."
  (unless (hash-table-p js)
    (signal 'fanyi-parse-error '("Youdao: expected a response object")))
  (let ((result (gethash "result" js))
        (data (gethash "data" js)))
    (unless (and (hash-table-p result) (equal (gethash "code" result) 200)
                 (hash-table-p data) (vectorp (gethash "entries" data)))
      (signal 'fanyi-parse-error '("Youdao: missing or invalid result entries")))
    (let ((entries (gethash "entries" data)))
      (when (= (length entries) 0)
        (throw 'not-found nil))
      (cl-loop for entry across entries
               collect
               (progn
                 (unless (and (hash-table-p entry)
                              (stringp (gethash "entry" entry))
                              (not (string-empty-p (string-trim (gethash "entry" entry)))))
                   (signal 'fanyi-parse-error '("Youdao: invalid entry text")))
                 (let ((explain (gethash "explain" entry)))
                   (cons (gethash "entry" entry)
                         (if (and (stringp explain)
                                  (not (string-empty-p (string-trim explain))))
                             explain
                           "NO EXPLANATION"))))))))

(defconst fanyi-youdao-thesaurus--pos-regexp
  (concat "\\_<" (regexp-opt '("pron." "prep." "conj." "abbr." "det." "adj."
                               "adv." "art." "vt." "vi." "n." "v.")))
  "Part-of-speech labels starting at a word boundary.")

(defun fanyi-youdao-thesaurus--pos-end-p (text end)
  "Whether a label ending at END in TEXT is separate from an ASCII word."
  (not (and (< end (length text))
            (string-match-p "[A-Za-z0-9_]" (substring text end (1+ end))))))

(defun fanyi-youdao-thesaurus--propertize-pos (explanation)
  "Return EXPLANATION with faces on independent part-of-speech labels."
  (let ((text (copy-sequence explanation))
        (case-fold-search nil)
        (start 0))
    (while (string-match fanyi-youdao-thesaurus--pos-regexp text start)
      (let ((begin (match-beginning 0))
            (end (match-end 0)))
        ;; Permit Chinese definitions immediately after the label, while
        ;; excluding labels that continue into an ASCII word or identifier.
        (when (fanyi-youdao-thesaurus--pos-end-p text end)
          (add-text-properties
           begin end '(face fanyi-pos-face font-lock-face fanyi-pos-face) text))
        (setq start end)))
    text))

(defun fanyi-youdao-thesaurus--explanation-parts (explanation)
  "Split EXPLANATION at explicit part-of-speech changes after separators.
Keep semicolons within one meaning and unfamiliar labels as ordinary text."
  (let ((explanation (string-trim explanation))
        (regexp (concat "[；;\n][ \t\r\n]*\\((?" fanyi-youdao-thesaurus--pos-regexp "\\)"))
        (case-fold-search nil)
        (start 0) (position 0) parts)
    (while (string-match regexp explanation position)
      (setq position (match-end 0))
      (when (fanyi-youdao-thesaurus--pos-end-p explanation position)
        (push (string-trim (substring explanation start (match-beginning 0))) parts)
        (setq start (match-beginning 1))))
    (push (string-trim (substring explanation start)) parts)
    (nreverse parts)))

(defun fanyi-youdao-thesaurus--fill-explanation (text width)
  "Fill TEXT to WIDTH columns, keeping persistent part-of-speech faces."
  (fanyi-youdao-thesaurus--fill-text
   (if (equal text "NO EXPLANATION")
       (propertize "暂无释义" 'face 'shadow)
     (fanyi-youdao-thesaurus--propertize-pos text))
   width))

(defun fanyi-youdao-thesaurus--fill-text (text width)
  "Return TEXT wrapped to WIDTH columns, preserving text properties."
  (mapconcat #'identity (fanyi-shr-wrap-lines text width) "\n"))

(defun fanyi-youdao-thesaurus--shr-entries (dom)
  "Render DOM's entries as aligned columns or stacked word blocks."
  (let* ((entries (dom-children dom))
         (width (max 1 (min shr-internal-width
                            (/ (window-body-width nil t) (max 1 (window-font-width))))))
         (column (+ 2 (apply #'max 0
                             (mapcar (lambda (entry) (string-width (dom-attr entry 'word))) entries))))
         (columns (and (eq fanyi-youdao-thesaurus-layout 'columns)
                       (>= width (+ column 32))))
         (indent (if columns column
                   (min fanyi-youdao-thesaurus-indent (max 0 (- width 8)))))
         (padding (make-string indent ?\s)))
    (dolist (entry entries)
      (let ((word (dom-attr entry 'word))
            (start (point)))
        (insert (if columns word (fanyi-youdao-thesaurus--fill-text word width)))
        (make-text-button start (point) :type 'fanyi-youdao-thesaurus-button
                          'button-data word
                          'help-echo (concat "Query " word))
        (put-text-property start (point) 'face 'fanyi-youdao-thesaurus-word-face)
        (if columns
            (insert (make-string (- column (string-width word)) ?\s))
          (insert "\n" padding))
        (cl-loop for part in (dom-children entry)
                 for first = t then nil
                 do (unless first (insert "\n" padding))
                 do (insert (replace-regexp-in-string
                             "\n" (concat "\n" padding)
                             (fanyi-youdao-thesaurus--fill-explanation part (- width indent))
                             t t)))
        (insert "\n\n")))))

(defun fanyi-youdao-thesaurus--render-style (window)
  "Return layout settings affecting Youdao content in WINDOW."
  (list fanyi-youdao-thesaurus-layout fanyi-youdao-thesaurus-indent
        (window-font-width window)))

(defun fanyi-youdao-thesaurus-render (result request)
  "Insert RESULT at point in the current buffer."
  (insert "# " (fanyi-request-display-name request) "\n\n")
  (fanyi-shr-insert
   `(fanyi-youdao-entries nil
                          ,@(mapcar (lambda (entry)
                                      `(entry ((word . ,(string-trim (car entry))))
                                              ,@(fanyi-youdao-thesaurus--explanation-parts (cdr entry))))
                                    result))
   '((fanyi-youdao-entries . fanyi-youdao-thesaurus--shr-entries))
   #'fanyi-youdao-thesaurus--render-style)
  (insert "\n"))

(defconst fanyi-youdao-thesaurus-provider
  (fanyi-provider-create :name "有道同义词词典"
                         :url "https://dict.youdao.com/suggest?q=%s&doctype=json"
                         :api-type 'json
                         :prepare #'fanyi-youdao-thesaurus-prepare-request
                         :parse #'fanyi-youdao-thesaurus-parse
                         :render #'fanyi-youdao-thesaurus-render)
  "Youdao thesaurus provider.")

(provide 'fanyi-youdao-thesaurus)
;;; fanyi-youdao-thesaurus.el ends here
