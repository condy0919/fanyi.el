;;; fanyi-shr.el --- HTML rendering for Fanyi -*- lexical-binding: t -*-

;; Copyright (C) 2026 Zhiwei Chen
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Render dictionary DOMs in isolated regions and reflow them when displayed
;; window widths change.  Reflow uses the saved DOM without another request.

;;; Code:

(require 'cl-lib)
(require 'shr)

(cl-defstruct (fanyi-shr--region (:constructor fanyi-shr--region-create))
  start end dom renderers layout)

(defvar-local fanyi-shr--regions nil
  "Rendered HTML regions in the current buffer.")
(defvar-local fanyi-shr--window nil
  "Window whose width determines the buffer's HTML layout.")
(defvar-local fanyi-shr--updating nil
  "Non-nil while inserting or reflowing HTML.")

(defun fanyi-shr--window ()
  "Choose one displayed window for the current buffer's layout."
  (let ((buffer (current-buffer)))
    (cond ((eq (window-buffer (selected-window)) buffer) (selected-window))
          ((and (window-live-p fanyi-shr--window)
                (eq (window-buffer fanyi-shr--window) buffer))
           fanyi-shr--window)
          (t (get-buffer-window buffer t)))))

(defun fanyi-shr--layout (window)
  "Describe the rendering width and font settings of WINDOW."
  (list (window-body-width window t) shr-width shr-max-width
        (frame-char-width (window-frame window)) shr-use-colors))

(defun fanyi-shr--render (dom renderers window)
  "Insert DOM with RENDERERS, using WINDOW when it is displayed."
  (let ((start (point)) end
        (shr-use-fonts nil)
        (shr-external-rendering-functions renderers)
        (shr-inhibit-images t)
        (shr-content-cache nil)
        ;; Pixel measurements can invoke redisplay before SHR has finished
        ;; styling its text.  Defer JIT fontification until those faces have
        ;; been saved and the complete region has been marked as HTML.
        (fontification-functions nil))
    (cl-labels
        ((render ()
           (save-restriction
             (goto-char start)
             (narrow-to-region start start)
             (shr-insert-document dom)
             (delete-trailing-whitespace (point-min) (point-max))
             ;; Font-lock owns `face'; keep HTML styling across refontify.
             (let ((position (point-min)) next)
               (while (< position (point-max))
                 (setq next (next-single-property-change
                             position 'face nil (point-max)))
                 (when (get-text-property position 'face)
                   (put-text-property position next 'font-lock-face
                                      (get-text-property position 'face)))
                 (setq position next)))
             (put-text-property (point-min) (point-max) 'fanyi-html t)
             (setq end (point-max)))))
      (if window
          (with-selected-window window
            (save-excursion (render)))
        (render)))
    (goto-char end)))

(defun fanyi-shr--forget (region)
  "Detach REGION's markers."
  (set-marker (fanyi-shr--region-start region) nil)
  (set-marker (fanyi-shr--region-end region) nil))

(defun fanyi-shr--clear ()
  "Release saved DOMs and hooks when the buffer is cleared or killed."
  (mapc #'fanyi-shr--forget fanyi-shr--regions)
  (setq fanyi-shr--regions nil fanyi-shr--window nil)
  (remove-hook 'window-size-change-functions #'fanyi-shr--window-change t)
  (remove-hook 'window-selection-change-functions #'fanyi-shr--window-change t)
  (remove-hook 'before-change-functions #'fanyi-shr--before-change t)
  (remove-hook 'kill-buffer-hook #'fanyi-shr--clear t)
  (remove-hook 'change-major-mode-hook #'fanyi-shr--clear t))

(defun fanyi-shr--before-change (begin end)
  "Forget HTML regions whose contents are about to change between BEGIN and END."
  (unless fanyi-shr--updating
    (setq fanyi-shr--regions
          (cl-delete-if
           (lambda (region)
             (let ((start (marker-position (fanyi-shr--region-start region)))
                   (finish (marker-position (fanyi-shr--region-end region))))
               (when (or (not start) (not finish) (>= start finish)
                         (and (< begin finish) (> end start)))
                 (fanyi-shr--forget region)
                 t)))
           fanyi-shr--regions))
    (unless fanyi-shr--regions (fanyi-shr--clear))))

(defun fanyi-shr-insert (dom renderers)
  "Insert DOM with RENDERERS and remember it for automatic width changes.
The current buffer may already contain other dictionary results.  HTML text
is marked with `fanyi-html', and its styling survives font-lock refreshes.
Use the buffer's default font and character-based layout, with SHR's colors."
  (let ((start (point))
        (window (fanyi-shr--window))
        (inhibit-read-only t)
        (fanyi-shr--updating t))
    (fanyi-shr--render dom renderers window)
    (when (> (point) start)
      (push (fanyi-shr--region-create
             :start (copy-marker start t) :end (copy-marker (point))
             :dom dom :renderers renderers
             :layout (and window (fanyi-shr--layout window)))
            fanyi-shr--regions)
      (setq fanyi-shr--window window)
      (add-hook 'window-size-change-functions #'fanyi-shr--window-change nil t)
      (add-hook 'window-selection-change-functions #'fanyi-shr--window-change nil t)
      (add-hook 'before-change-functions #'fanyi-shr--before-change nil t)
      (add-hook 'kill-buffer-hook #'fanyi-shr--clear nil t)
      (add-hook 'change-major-mode-hook #'fanyi-shr--clear nil t))))

(defun fanyi-shr--anchor (position)
  "Remember POSITION across changes in HTML line wrapping."
  (let ((region (cl-find-if
                 (lambda (entry)
                   (and (<= (fanyi-shr--region-start entry) position)
                        (< position (fanyi-shr--region-end entry))))
                 fanyi-shr--regions)))
    (if (not region)
        (copy-marker position t)
      (list region
            (save-excursion
              (goto-char (fanyi-shr--region-start region))
              (let ((count 0))
                (while (re-search-forward "[^ \t\r\n]" position t)
                  (setq count (1+ count)))
                count))
            (not (memq (char-after position) '(?\s ?\t ?\r ?\n)))))))

(defun fanyi-shr--position (anchor)
  "Restore the position described by ANCHOR."
  (if (markerp anchor)
      (prog1 (marker-position anchor) (set-marker anchor nil))
    (save-excursion
      (goto-char (fanyi-shr--region-start (car anchor)))
      (let ((limit (fanyi-shr--region-end (car anchor))))
        (dotimes (_ (cadr anchor))
          (re-search-forward "[^ \t\r\n]" limit t))
        (when (nth 2 anchor) (skip-chars-forward " \t\r\n" limit)))
      (point))))

(defun fanyi-shr--replace (region window layout)
  "Re-render REGION for WINDOW and remember LAYOUT."
  (let ((start (marker-position (fanyi-shr--region-start region)))
        (end (marker-position (fanyi-shr--region-end region))))
    (condition-case error-data
        (atomic-change-group
          (delete-region start end)
          (goto-char start)
          (fanyi-shr--render (fanyi-shr--region-dom region)
                             (fanyi-shr--region-renderers region) window))
      (error
       ;; Text is restored by the change group; restore its boundary markers.
       (set-marker (fanyi-shr--region-start region) start)
       (set-marker (fanyi-shr--region-end region) end)
       (signal (car error-data) (cdr error-data))))
    (set-marker (fanyi-shr--region-start region) start)
    (set-marker (fanyi-shr--region-end region) (point))
    (setf (fanyi-shr--region-layout region) layout)))

(defun fanyi-shr--outline-folds (regions)
  "Remember outline folds overlapping REGIONS before replacing their text."
  (mapcar
   (lambda (overlay)
     (list overlay (fanyi-shr--anchor (overlay-start overlay))
           (fanyi-shr--anchor (overlay-end overlay))))
   (cl-delete-duplicates
    (cl-mapcan
     (lambda (region)
       (cl-remove-if-not
        (lambda (overlay) (eq (overlay-get overlay 'invisible) 'outline))
        (overlays-in (fanyi-shr--region-start region) (fanyi-shr--region-end region))))
     regions)
    :test #'eq)))

(defun fanyi-shr-reflow (&optional force)
  "Reflow saved HTML for its displayed window without fetching content.
With FORCE, re-render even if the window width has not changed.  When several
windows display the buffer, prefer its selected window, then its previous one."
  (interactive "P")
  (let ((window (fanyi-shr--window)))
    (when (and window fanyi-shr--regions (not fanyi-shr--updating))
      (let* ((layout (fanyi-shr--layout window))
             (regions (cl-remove-if
                       (lambda (region)
                         (and (not force) (equal layout (fanyi-shr--region-layout region))))
                       fanyi-shr--regions)))
        (when regions
          (let ((position (fanyi-shr--anchor (point)))
                (folds (fanyi-shr--outline-folds regions))
                (windows (mapcar
                          (lambda (win)
                            (list win (fanyi-shr--anchor (window-point win))
                                  (fanyi-shr--anchor (window-start win))))
                          (get-buffer-window-list (current-buffer) nil t)))
                (modified (buffer-modified-p))
                (inhibit-read-only t)
                (buffer-undo-list t)
                (fanyi-shr--updating t))
            (unwind-protect
                (progn
                  ;; Measure all text while rendering, including folded bodies.
                  (dolist (fold folds) (delete-overlay (car fold)))
                  (dolist (region regions) (fanyi-shr--replace region window layout))
                  (setq fanyi-shr--window window))
              (dolist (fold folds)
                (move-overlay (car fold) (fanyi-shr--position (cadr fold))
                              (fanyi-shr--position (caddr fold)) (current-buffer)))
              (dolist (state windows)
                (let ((win (car state))
                      (point (fanyi-shr--position (cadr state)))
                      (start (fanyi-shr--position (caddr state))))
                  (when (window-live-p win)
                    (set-window-point win point)
                    (set-window-start win start t))))
              (goto-char (fanyi-shr--position position))
              (set-buffer-modified-p modified))))))))

(defun fanyi-shr--window-change (window)
  "Reflow displayed HTML after a window size or buffer change."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (condition-case error-data
          (fanyi-shr-reflow)
        (error (message "Fanyi HTML reflow failed: %s" (error-message-string error-data)))))))

(provide 'fanyi-shr)
;;; fanyi-shr.el ends here
