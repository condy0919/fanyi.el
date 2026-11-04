;;; fanyi-shr.el --- HTML rendering for Fanyi -*- lexical-binding: t -*-

;; Copyright (C) 2026 Zhiwei Chen
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Render dictionary DOMs in isolated regions and reflow them when displayed
;; window widths change.  Reflow uses the saved DOM without another request.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'dom)
(require 'shr)

(defun fanyi-shr-dom-text (node)
  "Return NODE's descendant text without adding separators.
Ignore scripts and comments.  Use `dom-inner-text' when available, with
equivalent text extraction on Emacs 29 and 30."
  (cond
   ((memq (dom-tag node) '(script comment)) "")
   ((fboundp 'dom-inner-text) (dom-inner-text node))
   (t (mapconcat (lambda (child)
                   (if (stringp child) child (fanyi-shr-dom-text child)))
                 (dom-children node) ""))))

(defun fanyi-shr-wrap-lines (text width)
  "Wrap TEXT to WIDTH columns and return a list of lines.
Keep text properties and all non-whitespace characters.  Fill paragraphs
before splitting overlong words; retain empty lines between paragraphs.
WIDTH is at least one column.  A character wider than WIDTH occupies its
own line so unusually narrow windows never discard characters."
  (let ((width (max 1 width)) lines)
    (with-temp-buffer
      (insert text)
      (let ((fill-column width)
            (fill-prefix nil)
            (adaptive-fill-mode nil)
            (sentence-end-double-space nil))
        (fill-region (point-min) (point-max)))
      (dolist (line (split-string (buffer-string) "\n"))
        (while (> (string-width line) width)
          (let ((length (max 1 (length (truncate-string-to-width line width)))))
            (push (substring line 0 length) lines)
            (setq line (substring line length))))
        (push line lines)))
    (nreverse lines)))

(cl-defstruct (fanyi-shr--region (:constructor fanyi-shr--region-create))
  start end dom renderers style-function layout failed-layout sections)

(cl-defstruct (fanyi-shr--section (:constructor fanyi-shr--section-create))
  identity heading-start heading-end body-start body-end overlay closed)

(defvar fanyi-shr--sections 'inactive
  "Sections collected during the current HTML render.")

(defvar-local fanyi-shr--section-invisibility-added nil
  "Whether Fanyi added its section symbol to `buffer-invisibility-spec'.")

(define-button-type 'fanyi-shr-section-button
  'face nil
  'follow-link t
  'action #'fanyi-shr-toggle-section)

(defun fanyi-shr-insert-section (identity heading-dom body-dom &optional initially-closed)
  "Render HEADING-DOM and BODY-DOM as one folding section.
INITIALLY-CLOSED, when non-nil, hides the body on the initial render.
Reflow preserves the existing section state instead of reapplying this default.
Call from a renderer used by `fanyi-shr-insert'.  IDENTITY must be the same
object on each render, normally the cached DOM node containing the section.
Outside `fanyi-shr-insert', simply render the expanded content."
  (shr-ensure-newline)
  (let ((section (unless (eq fanyi-shr--sections 'inactive)
                   (fanyi-shr--section-create :identity identity :closed initially-closed))))
    (when section
      (push section fanyi-shr--sections)
      (setf (fanyi-shr--section-heading-start section) (copy-marker (point))))
    (shr-descend heading-dom)
    (when section
      (setf (fanyi-shr--section-heading-end section) (copy-marker (point))))
    (shr-ensure-newline)
    (when section
      (setf (fanyi-shr--section-body-start section) (copy-marker (point))))
    (shr-descend body-dom)
    (shr-ensure-newline)
    (when section
      (setf (fanyi-shr--section-body-end section) (copy-marker (point))))))

(defun fanyi-shr--section-forget (section)
  "Release SECTION's markers and overlay."
  (let ((start (fanyi-shr--section-heading-start section))
        (end (fanyi-shr--section-heading-end section)))
    ;; During replacement old markers may point at the new title.  Only
    ;; remove properties that still belong to this particular section.
    (when (and (markerp start) (eq (marker-buffer start) (current-buffer))
               (markerp end) (eq (marker-buffer end) (current-buffer))
               (< start end) (eq (get-text-property start 'fanyi-shr-section) section))
      (with-silent-modifications
        (remove-text-properties start (1+ start) '(display nil))
        (remove-text-properties start end
                                '(button nil category nil help-echo nil fanyi-shr-section nil)))))
  (when (overlayp (fanyi-shr--section-overlay section))
    (delete-overlay (fanyi-shr--section-overlay section)))
  (dolist (marker (list (fanyi-shr--section-heading-start section)
                        (fanyi-shr--section-heading-end section)
                        (fanyi-shr--section-body-start section)
                        (fanyi-shr--section-body-end section)))
    (when (markerp marker) (set-marker marker nil))))

(defun fanyi-shr--section-set-closed (section closed)
  "Set SECTION to CLOSED and update its title without changing its text."
  (when (and closed (not (eq buffer-invisibility-spec t))
             (not (memq 'fanyi-shr-section buffer-invisibility-spec)))
    (add-to-invisibility-spec 'fanyi-shr-section)
    (setq fanyi-shr--section-invisibility-added t))
  (setf (fanyi-shr--section-closed section) closed)
  (overlay-put (fanyi-shr--section-overlay section)
               'invisible (and closed 'fanyi-shr-section))
  (let ((start (fanyi-shr--section-heading-start section))
        (end (fanyi-shr--section-heading-end section)))
    (with-silent-modifications
      (put-text-property start (1+ start) 'display
                         (concat (if closed "▸ " "▾ ")
                                 (char-to-string (char-after start))))
      (put-text-property start end 'help-echo
                         (if closed "Expand section" "Collapse section")))))

(defun fanyi-shr-toggle-section (&optional button)
  "Toggle the local section at BUTTON or point."
  (interactive)
  (when-let* ((button (or button (button-at (point))))
              (section (button-get button 'fanyi-shr-section))
              (_ (eq (marker-buffer (fanyi-shr--section-heading-start section))
                     (current-buffer))))
    (fanyi-shr--section-set-closed section (not (fanyi-shr--section-closed section)))
    (when (and (fanyi-shr--section-closed section)
               (<= (fanyi-shr--section-body-start section) (point))
               (< (point) (fanyi-shr--section-body-end section)))
      (goto-char (fanyi-shr--section-heading-start section)))))

(defun fanyi-shr--section-open (overlay)
  "Open the section represented by OVERLAY for an isearch match."
  (fanyi-shr--section-set-closed (overlay-get overlay 'fanyi-shr-section) nil))

(defun fanyi-shr--section-install (section)
  "Install SECTION's title button and body overlay after SHR has filled lines."
  (let ((start (fanyi-shr--section-heading-start section))
        (end (fanyi-shr--section-heading-end section)))
    (save-excursion
      (goto-char start)
      (skip-chars-forward " \t\n\r" end)
      (set-marker start (point))
      (goto-char end)
      (skip-chars-backward " \t\n\r" start)
      (set-marker end (point)))
    (when (< start end)
      (make-text-button start end :type 'fanyi-shr-section-button
                        'fanyi-shr-section section)
      (let ((overlay (make-overlay (fanyi-shr--section-body-start section)
                                   (fanyi-shr--section-body-end section))))
        (setf (fanyi-shr--section-overlay section) overlay)
        (overlay-put overlay 'fanyi-shr-section section)
        (overlay-put overlay 'isearch-open-invisible #'fanyi-shr--section-open))
      (fanyi-shr--section-set-closed section (fanyi-shr--section-closed section))
      section)))

(defvar fanyi-shr--buffers nil
  "Buffers with saved HTML regions to refresh after theme changes.")

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

(defun fanyi-shr--layout (window &optional style-function)
  "Describe WINDOW's layout and provider state from STYLE-FUNCTION.
Copy the style signature so later list mutations cannot change this snapshot."
  (list (window-body-width window t) shr-width shr-max-width
        (frame-char-width (window-frame window)) (window-font-width window)
        shr-use-colors
        (when style-function (copy-tree (funcall style-function window) t))))

(defun fanyi-shr--render-width ()
  "Return the effective column width for the selected rendering window.
Preserve explicit `shr-width' and the automatic `shr-max-width' limit.
Use the buffer's remapped font width, including text scaling, while keeping
SHR's own allowance for continuation glyphs and fringes."
  (or shr-width
      (let* ((columns (/ (window-body-width nil t) (max 1 (window-font-width))))
             (reserved (- (window-body-width) (shr--window-width))))
        (max 1 (min (or shr-max-width columns) (- columns reserved))))))

(defun fanyi-shr--render (dom renderers window)
  "Insert DOM with RENDERERS, using WINDOW when it is displayed."
  (let ((start (point)) end complete
        (fanyi-shr--sections nil)
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
             ;; SHR's automatic character width uses frame metrics.  Bind
             ;; its public override only inside the actual rendering window
             ;; so text scaling can both shrink and widen the line length.
             (let ((shr-width (fanyi-shr--render-width)))
               (shr-insert-document dom))
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
      (unwind-protect
          (progn
            (if window
                (with-selected-window window
                  (save-excursion (render)))
              (render))
            (dolist (section fanyi-shr--sections)
              (unless (fanyi-shr--section-install section)
                (fanyi-shr--section-forget section)))
            (setq fanyi-shr--sections
                  (cl-remove-if-not #'fanyi-shr--section-overlay fanyi-shr--sections))
            (goto-char end)
            (setq complete t)
            fanyi-shr--sections)
        (unless complete (mapc #'fanyi-shr--section-forget fanyi-shr--sections))))))

(defun fanyi-shr--forget (region)
  "Detach REGION's markers."
  (mapc #'fanyi-shr--section-forget (fanyi-shr--region-sections region))
  (set-marker (fanyi-shr--region-start region) nil)
  (set-marker (fanyi-shr--region-end region) nil))

(defun fanyi-shr--clear ()
  "Release saved DOMs and hooks when the buffer is cleared or killed."
  (mapc #'fanyi-shr--forget fanyi-shr--regions)
  (when fanyi-shr--section-invisibility-added
    (remove-from-invisibility-spec 'fanyi-shr-section)
    (setq fanyi-shr--section-invisibility-added nil))
  (setq fanyi-shr--regions nil fanyi-shr--window nil
        fanyi-shr--buffers (delq (current-buffer) fanyi-shr--buffers))
  (unless fanyi-shr--buffers
    (remove-hook 'enable-theme-functions #'fanyi-shr--theme-change)
    (remove-hook 'disable-theme-functions #'fanyi-shr--theme-change))
  (remove-hook 'post-command-hook #'fanyi-shr--post-command t)
  (remove-hook 'text-scale-mode-hook #'fanyi-shr--post-command t)
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

(defun fanyi-shr-insert (dom renderers &optional style-function)
  "Insert DOM with RENDERERS and remember it for automatic layout changes.
STYLE-FUNCTION, when non-nil, receives the display window and returns a
signature of provider settings that should trigger rendering when changed.
The current buffer may already contain other dictionary results.  HTML text
is marked with `fanyi-html', and its styling survives font-lock refreshes.
Use the buffer's default font and character-based layout, with SHR's colors."
  (let ((start (point)) sections layout complete
        (window (fanyi-shr--window))
        (inhibit-read-only t)
        (fanyi-shr--updating t))
    (unwind-protect
        (atomic-change-group
          (setq sections (fanyi-shr--render dom renderers window)
                layout (and window (fanyi-shr--layout window style-function))
                complete t))
      (unless complete (mapc #'fanyi-shr--section-forget sections)))
    (when (> (point) start)
      (push (fanyi-shr--region-create
             :start (copy-marker start t) :end (copy-marker (point))
             :dom dom :renderers renderers :style-function style-function
             :layout layout :sections sections)
            fanyi-shr--regions)
      (setq fanyi-shr--window window)
      (cl-pushnew (current-buffer) fanyi-shr--buffers)
      (add-hook 'post-command-hook #'fanyi-shr--post-command nil t)
      (add-hook 'text-scale-mode-hook #'fanyi-shr--post-command nil t)
      (add-hook 'enable-theme-functions #'fanyi-shr--theme-change)
      (add-hook 'disable-theme-functions #'fanyi-shr--theme-change)
      (add-hook 'window-size-change-functions #'fanyi-shr--window-change nil t)
      (add-hook 'window-selection-change-functions #'fanyi-shr--window-change nil t)
      (add-hook 'before-change-functions #'fanyi-shr--before-change nil t)
      (add-hook 'kill-buffer-hook #'fanyi-shr--clear nil t)
      (add-hook 'change-major-mode-hook #'fanyi-shr--clear nil t))))

(defun fanyi-shr--text-offset (start position)
  "Count non-whitespace characters between START and POSITION."
  (save-excursion
    (goto-char start)
    (let ((count 0))
      (while (< (point) position)
        (skip-chars-forward " \t\r\n" position)
        (cl-incf count (skip-chars-forward "^ \t\r\n" position)))
      count)))

(defun fanyi-shr--text-anchor (start position)
  "Describe POSITION after START using text and whitespace offsets.
The non-whitespace character count survives wrapping changes.  Keep the
offset within the whitespace run as well, so unchanged layout restores
positions inside spaces, indentation and blank lines exactly."
  (list (fanyi-shr--text-offset start position)
        (not (memq (char-after position) '(?\s ?\t ?\r ?\n)))
        (save-excursion
          (goto-char position)
          (skip-chars-backward " \t\r\n" start)
          (- position (point)))))

(defun fanyi-shr--text-position (start end offset content-p &optional whitespace-offset)
  "Return the position at non-whitespace OFFSET between START and END.
When CONTENT-P is non-nil, advance to the next non-whitespace character;
otherwise move WHITESPACE-OFFSET characters into the following whitespace
run, clamped to its new length.  A nil WHITESPACE-OFFSET stays at the end
of the preceding word."
  (save-excursion
    (goto-char start)
    (while (> offset 0)
      (let ((before (point)))
        (skip-chars-forward " \t\r\n" end)
        (let ((count (skip-chars-forward
                      "^ \t\r\n" (min end (+ (point) offset)))))
          (if (> count 0)
              (cl-decf offset count)
            ;; Keep the last text boundary if the new document is shorter.
            (goto-char before)
            (setq offset 0)))))
    (cond
     (content-p (skip-chars-forward " \t\r\n" end))
     (whitespace-offset
      (let ((begin (point)))
        (skip-chars-forward " \t\r\n" end)
        (goto-char (min (point) (+ begin whitespace-offset))))))
    (point)))

(defun fanyi-shr--anchor (position)
  "Remember POSITION across changes in HTML line wrapping."
  (let ((region (cl-find-if
                 (lambda (entry)
                   (and (<= (fanyi-shr--region-start entry) position)
                        (< position (fanyi-shr--region-end entry))))
                 fanyi-shr--regions)))
    (if (not region)
        (copy-marker position t)
      (cons region (fanyi-shr--text-anchor (fanyi-shr--region-start region) position)))))

(defun fanyi-shr--position (anchor)
  "Restore the position described by ANCHOR."
  (if (markerp anchor)
      (prog1 (marker-position anchor) (set-marker anchor nil))
    (fanyi-shr--text-position (fanyi-shr--region-start (car anchor))
                              (fanyi-shr--region-end (car anchor))
                              (cadr anchor) (nth 2 anchor) (nth 3 anchor))))

(defun fanyi-shr--replace (region window layout)
  "Re-render REGION for WINDOW and remember LAYOUT."
  (let ((start (marker-position (fanyi-shr--region-start region)))
        (end (marker-position (fanyi-shr--region-end region)))
        (sections (fanyi-shr--region-sections region))
        (saved (mapcar
                (lambda (section)
                  (mapcar (lambda (marker) (cons marker (marker-position marker)))
                          (list (fanyi-shr--section-heading-start section)
                                (fanyi-shr--section-heading-end section)
                                (fanyi-shr--section-body-start section)
                                (fanyi-shr--section-body-end section))))
                (fanyi-shr--region-sections region)))
        replacement)
    (condition-case error-data
        (atomic-change-group
          (dolist (section sections) (delete-overlay (fanyi-shr--section-overlay section)))
          (delete-region start end)
          (goto-char start)
          (setq replacement (fanyi-shr--render (fanyi-shr--region-dom region)
                                               (fanyi-shr--region-renderers region) window))
          (dolist (section replacement)
            (when-let* ((old (cl-find (fanyi-shr--section-identity section) sections
                                      :key #'fanyi-shr--section-identity :test #'eq)))
              (fanyi-shr--section-set-closed section (fanyi-shr--section-closed old)))))
      ((error quit)
       (unwind-protect
           (let ((inhibit-quit t))
             ;; Retry renderer errors only after the layout changes or the
             ;; reader explicitly requests a refresh.  C-g must remain retryable.
             (when (memq 'error (get (car error-data) 'error-conditions))
               (setf (fanyi-shr--region-failed-layout region) layout))
             ;; Text is restored by the change group; restore the old state
             ;; before discarding the replacement's overlays and markers.
             (set-marker (fanyi-shr--region-start region) start)
             (set-marker (fanyi-shr--region-end region) end)
             (dolist (markers saved)
               (dolist (state markers) (set-marker (car state) (cdr state))))
             (dolist (section sections)
               (move-overlay (fanyi-shr--section-overlay section)
                             (fanyi-shr--section-body-start section)
                             (fanyi-shr--section-body-end section) (current-buffer)))
             (mapc #'fanyi-shr--section-forget replacement))
         ;; A cleanup failure must not replace the original error or C-g.
         (signal (car error-data) (cdr error-data)))))
    ;; The text has committed; finish switching its cached state together.
    (let ((inhibit-quit t))
      (mapc #'fanyi-shr--section-forget sections)
      (set-marker (fanyi-shr--region-start region) start)
      (set-marker (fanyi-shr--region-end region) (point))
      (setf (fanyi-shr--region-layout region) layout
            (fanyi-shr--region-failed-layout region) nil
            (fanyi-shr--region-sections region) replacement))))

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

(defun fanyi-shr--reflow-regions (updates window)
  "Apply region/layout UPDATES for WINDOW in the widened current buffer."
  (let ((position (fanyi-shr--anchor (point)))
        (mark (and (mark t) (fanyi-shr--anchor (mark t))))
        ;; Replacing text must not deactivate an existing selection,
        ;; or reactivate one that the current command has dismissed.
        (mark-active mark-active)
        (deactivate-mark deactivate-mark)
        (folds (fanyi-shr--outline-folds (mapcar #'car updates)))
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
          (dolist (update updates)
            (fanyi-shr--replace (car update) window (cdr update)))
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
      (set-marker (mark-marker) (and mark (fanyi-shr--position mark)))
      (set-buffer-modified-p modified))))

(defun fanyi-shr-reflow (&optional force)
  "Reflow saved HTML for its displayed window without fetching content.
With FORCE, re-render even if the layout and provider styles have not changed.
Otherwise, retry a failed render only after its layout or styles change.
Preserve any narrowing by anchoring its boundaries in the rendered text.
When several windows display the buffer, prefer its selected window, then its
previous one."
  (interactive "P")
  (let ((window (fanyi-shr--window)))
    (when (and window fanyi-shr--regions (not fanyi-shr--updating))
      (let ((updates
             (cl-loop for region in fanyi-shr--regions
                      for layout = (fanyi-shr--layout
                                    window (fanyi-shr--region-style-function region))
                      for failed = (fanyi-shr--region-failed-layout region)
                      ;; Visiting the last successful layout also permits a
                      ;; retry when the reader returns to the failed layout.
                      do (when (and failed (not (equal layout failed)))
                           (setf (fanyi-shr--region-failed-layout region) nil))
                      unless (and (not force)
                                  (or (equal layout (fanyi-shr--region-layout region))
                                      (equal layout failed)))
                      collect (cons region layout))))
        (when updates
          (let ((bounds
                 (when (buffer-narrowed-p)
                   (let ((start (point-min)) (end (point-max)))
                     (save-restriction
                       (widen)
                       (cons (fanyi-shr--anchor start) (fanyi-shr--anchor end)))))))
            (unwind-protect
                (progn
                  (widen)
                  (fanyi-shr--reflow-regions updates window))
              (when bounds
                (let ((inhibit-quit t))
                  (narrow-to-region (fanyi-shr--position (car bounds))
                                    (fanyi-shr--position (cdr bounds))))))))))))

(defun fanyi-shr--window-change (window)
  "Reflow displayed HTML after a window size or buffer change."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (condition-case error-data
          (fanyi-shr-reflow)
        (error (message "Fanyi HTML reflow failed: %s" (error-message-string error-data)))))))

(defun fanyi-shr--post-command ()
  "Refresh the current buffer's displayed HTML after local commands."
  (when-let* ((window (fanyi-shr--window)))
    (fanyi-shr--window-change window)))

(defun fanyi-shr--theme-change (&rest _)
  "Refresh visible HTML after a theme is enabled or disabled."
  (dolist (buffer (copy-sequence fanyi-shr--buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; The local refresh handler isolates errors in each buffer.
        (fanyi-shr--post-command)))))

(provide 'fanyi-shr)
;;; fanyi-shr.el ends here
