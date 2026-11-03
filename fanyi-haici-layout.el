;;; fanyi-haici-layout.el --- Haici word-list layouts -*- lexical-binding: t -*-

;; Copyright (C) 2026 Zhiwei Chen
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Render prepared Haici word lists as responsive columns or compact links.
;; These SHR handlers retain their DOM and text order across window changes.

;;; Code:

(require 'shr)
(require 'dom)
(require 'button)
(require 'cl-lib)
(require 'subr-x)

(declare-function fanyi-dwim "fanyi")

(defun fanyi-haici-layout--width ()
  "Return the available width in columns for the current SHR render."
  (max 1 (min shr-internal-width
              (/ (window-body-width nil t) (max 1 (window-font-width))))))

(defun fanyi-haici-layout--lines (text width)
  "Wrap TEXT to WIDTH columns without dropping non-whitespace characters.
Break unusually long unspaced words as well as ordinary prose."
  (let ((width (max 1 width)) lines)
    (with-temp-buffer
      (insert (string-trim text))
      (let ((fill-column width)
            (fill-prefix nil)
            (adaptive-fill-mode nil)
            (sentence-end-double-space nil))
        (fill-region (point-min) (point-max)))
      (dolist (line (split-string (buffer-string) "\n"))
        (while (> (string-width line) width)
          (let* ((part (truncate-string-to-width line width))
                 ;; A double-width character must still fit in a one-column
                 ;; window.  Consume it rather than producing an empty line.
                 (length (max 1 (length part))))
            (push (substring line 0 length) lines)
            (setq line (substring line length))))
        (push line lines)))
    (nreverse lines)))

(defun fanyi-haici-layout--word (entry text)
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

(defun fanyi-haici-layout-entries (dom)
  "Render DOM's word/explanation entries as columns or stacked rows.
Each child is (entry ((word . WORD) (url . URL)) EXPLANATION).
Keep columns only when every word fits in 30 columns and at least 24 columns
remain for explanations.  Respect the current SHR indentation, inserting
the layout directly so SHR does not collapse alignment spaces."
  (shr-ensure-newline)
  (let* ((entries (dom-non-text-children dom))
         (width (fanyi-haici-layout--width))
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
            (explanation (string-trim (dom-texts entry ""))))
        (insert padding)
        (fanyi-haici-layout--word
         entry (if columns word
                 (mapconcat #'identity (fanyi-haici-layout--lines word (- width indent))
                            (concat "\n" padding))))
        (unless (string-empty-p explanation)
          (if columns
              (insert (make-string (- column (string-width word)) ?\s))
            (insert "\n" explanation-padding))
          (insert (mapconcat #'identity
                             (fanyi-haici-layout--lines explanation (- width explanation-indent))
                             (concat "\n" explanation-padding))))
        (insert (if columns "\n" "\n\n"))))))

(defun fanyi-haici-layout-words (dom)
  "Render DOM's entries as compact word buttons with automatic wrapping.
Children use the same word/url attributes as `fanyi-haici-layout-entries'."
  (shr-ensure-newline)
  (let* ((width (fanyi-haici-layout--width))
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
        (let ((lines (fanyi-haici-layout--lines word (- width indent))))
          (fanyi-haici-layout--word entry (mapconcat #'identity lines (concat "\n" padding)))
          (setq column (+ (if (cdr lines) indent column) (string-width (car (last lines))))))))
    (shr-ensure-newline)))

(provide 'fanyi-haici-layout)
;;; fanyi-haici-layout.el ends here
