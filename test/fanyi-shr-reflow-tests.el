;;; fanyi-shr-reflow-tests.el --- HTML layout lifecycle tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'button)
(require 'outline)
(require 'fanyi-shr)

(defconst fanyi-shr-test--text
  (mapconcat (lambda (number) (format "word%02d" number))
             (number-sequence 1 45) " "))

(defmacro fanyi-shr-test--with-window (&rest body)
  "Evaluate BODY in a temporary buffer displayed in the selected window."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (save-window-excursion
       (delete-other-windows)
       (set-window-buffer nil (current-buffer))
       (let ((shr-width nil)
             (shr-use-fonts nil))
         ,@body))))

(defun fanyi-shr-test--text ()
  "Return rendered content with layout whitespace normalized."
  (replace-regexp-in-string "[ \t\r\n]+" " " (string-trim (buffer-string))))

(defun fanyi-shr-test--max-line-width ()
  "Return the largest line width in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((width 0))
      (while (not (eobp))
        (end-of-line)
        (setq width (max width (current-column)))
        (forward-line))
      width)))

(ert-deftest fanyi-shr-reflow-preserves-other-providers-and-reading-position ()
  (fanyi-shr-test--with-window
    (let ((prefix (propertize "# Earlier\n\nPreserve this.\n\n" 'face 'bold))
          (suffix (propertize "\n# Later\n\nKeep this too.\n" 'help-echo "later")))
      (insert prefix)
      (fanyi-shr-insert `(p nil ,fanyi-shr-test--text) nil)
      (insert suffix)
      (let ((old-lines (count-lines (point-min) (point-max))))
        (goto-char (point-min))
        (search-forward "word25")
        (goto-char (match-beginning 0))
        (let ((reading (buffer-substring-no-properties (point) (+ (point) 3)))
              (buffer-read-only t)
              (buffer-undo-list nil))
          (set-buffer-modified-p nil)
          (split-window-right 35)
          (fanyi-shr-reflow)
          (should (equal reading (buffer-substring-no-properties (point) (+ (point) 3))))
          (should buffer-read-only)
          (should-not buffer-undo-list)
          (should-not (buffer-modified-p))
          (should (> (count-lines (point-min) (point-max)) old-lines))
          (should (equal-including-properties
                   prefix (buffer-substring (point-min) (+ (point-min) (length prefix)))))
          (should (equal-including-properties
                   suffix (buffer-substring (- (point-max) (length suffix)) (point-max)))))))))

(ert-deftest fanyi-shr-uses-the-displaying-window-not-the-selected-window ()
  (fanyi-shr-test--with-window
    (let* ((buffer (current-buffer))
           (selected (selected-window))
           (target (split-window-right -25)))
      (set-window-buffer target buffer)
      (set-window-buffer selected (get-buffer-create " *fanyi-shr-other-window*"))
      (unwind-protect
          (progn
            (fanyi-shr-insert `(p nil ,fanyi-shr-test--text) nil)
            (should (eq selected (selected-window)))
            (should (< (fanyi-shr-test--max-line-width) (window-body-width target)))
            (should (equal (fanyi-shr-test--text) fanyi-shr-test--text)))
        (kill-buffer " *fanyi-shr-other-window*")))))

(ert-deftest fanyi-shr-rendering-preserves-horizontal-scroll ()
  (fanyi-shr-test--with-window
    (set-window-hscroll nil 7)
    (fanyi-shr-insert `(p nil ,fanyi-shr-test--text) nil)
    (should (= (window-hscroll) 7))
    (should (equal (fanyi-shr-test--text) fanyi-shr-test--text))
    (let ((shr-width 35))
      (fanyi-shr-reflow)
      (should (= (window-hscroll) 7))
      (should (<= (fanyi-shr-test--max-line-width) shr-width))
      (should (equal (fanyi-shr-test--text) fanyi-shr-test--text)))))

(ert-deftest fanyi-shr-reflows-only-when-layout-changes ()
  (fanyi-shr-test--with-window
    (let ((renders 0)
          (renderer (lambda (dom) (shr-generic dom))))
      (cl-letf (((symbol-function 'fanyi-shr-test--count-render)
                 (lambda (dom) (cl-incf renders) (funcall renderer dom))))
        (fanyi-shr-insert `(sample nil ,fanyi-shr-test--text)
                          '((sample . fanyi-shr-test--count-render)))
        (should (= renders 1))
        (fanyi-shr--window-change (selected-window))
        (should (= renders 1))
        (split-window-right 35)
        ;; Batch Emacs does not run redisplay hooks, so invoke the registered
        ;; hook with the real resized window rather than mocking its width.
        (run-hook-with-args 'window-size-change-functions (selected-window))
        (should (= renders 2))
        (fanyi-shr--window-change (selected-window))
        (should (= renders 2))
        (let ((second (next-window)))
          (set-window-buffer second (current-buffer))
          (select-window second)
          (run-hook-with-args 'window-selection-change-functions second)
          (should (= renders 3)))))))

(ert-deftest fanyi-shr-reflow-retains-actions-without-network ()
  (fanyi-shr-test--with-window
    (let (clicked)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (&rest _) (ert-fail "Reflow must not fetch resources")))
                ((symbol-function 'url-retrieve-synchronously)
                 (lambda (&rest _) (ert-fail "Reflow must not fetch resources")))
                ((symbol-function 'url-queue-retrieve)
                 (lambda (&rest _) (ert-fail "Reflow must not fetch resources"))))
        (fanyi-shr-insert
         `(div nil (p nil ,fanyi-shr-test--text)
               (a ((href . "https://example.invalid/query")))
               (img ((src . "https://example.invalid/image.png"))))
         `((a . ,(lambda (dom)
                   (insert-button "Query" 'button-data (dom-attr dom 'href)
                                  'action (lambda (url) (setq clicked url)))))))
        (split-window-right 35)
        (fanyi-shr-reflow)
        (should-not clicked)
        (goto-char (point-min))
        (search-forward "Query")
        (let ((button (button-at (match-beginning 0))))
          (should button)
          (button-activate button))
        (should (equal clicked "https://example.invalid/query"))))))

(ert-deftest fanyi-shr-defers-fontification-until-html-is-complete ()
  (fanyi-shr-test--with-window
    (let* ((calls 0)
           (fontification-functions
            (list (lambda (_position)
                    (cl-incf calls)
                    (with-silent-modifications
                      (remove-text-properties (point-min) (point-max) '(face nil)))))))
      (fanyi-shr-insert
       '(sample nil "Styled content")
       `((sample . ,(lambda (dom)
                      (let ((start (point)))
                        (shr-generic dom)
                        (add-face-text-property start (point) 'bold)
                        ;; GUI pixel measurement may request fontification at
                        ;; this stage, before the region is marked as HTML.
                        (run-hook-with-args 'fontification-functions start))))))
      (should (= calls 0))
      (should (get-text-property (point-min) 'fanyi-html))
      (should (eq (get-text-property (point-min) 'font-lock-face) 'bold))
      ;; The caller's fontification callbacks resume after insertion.
      (run-hook-with-args 'fontification-functions (point-min))
      (should (= calls 1))
      (should (eq (get-text-property (point-min) 'font-lock-face) 'bold)))))

(ert-deftest fanyi-shr-clears-saved-dom-on-erase-and-mode-change ()
  (fanyi-shr-test--with-window
    (dolist (clear '(erase-buffer fundamental-mode))
      (fanyi-shr-insert '(p nil "Saved content") nil)
      (let ((region (car fanyi-shr--regions)))
        (funcall clear)
        (should-not fanyi-shr--regions)
        (should-not (marker-buffer (fanyi-shr--region-start region)))
        (should-not (marker-buffer (fanyi-shr--region-end region)))
        (should-not (memq #'fanyi-shr--window-change window-size-change-functions))))))

(ert-deftest fanyi-shr-clears-saved-dom-on-kill ()
  (let ((buffer (generate-new-buffer " *fanyi-shr-kill*")) region)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (fanyi-shr-insert '(p nil "Saved content") nil)
            (setq region (car fanyi-shr--regions)))
          (kill-buffer buffer)
          (should-not (marker-buffer (fanyi-shr--region-start region)))
          (should-not (marker-buffer (fanyi-shr--region-end region))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest fanyi-shr-reflow-keeps-old-content-on-render-error ()
  (fanyi-shr-test--with-window
    (let (fail)
      (fanyi-shr-insert
       `(div nil (p nil ,fanyi-shr-test--text) (sample nil "Last sentence."))
       `((sample . ,(lambda (dom)
                      (if fail (error "Rendering failed") (shr-generic dom))))))
      (let ((before (buffer-string))
            (start (marker-position (fanyi-shr--region-start (car fanyi-shr--regions))))
            (end (marker-position (fanyi-shr--region-end (car fanyi-shr--regions)))))
        (setq fail t)
        (set-window-hscroll nil 7)
        (should-error (fanyi-shr-reflow t))
        (should (= (window-hscroll) 7))
        (should (equal-including-properties before (buffer-string)))
        (should (= start (fanyi-shr--region-start (car fanyi-shr--regions))))
        (should (= end (fanyi-shr--region-end (car fanyi-shr--regions))))))))

(ert-deftest fanyi-shr-reflow-preserves-collapsed-outline-subtrees ()
  (fanyi-shr-test--with-window
    (setq-local outline-regexp "^#+")
    (outline-minor-mode 1)
    (fanyi-shr-insert
     `(div nil (p nil "## Hidden section") (p nil ,fanyi-shr-test--text)
           (p nil "## Visible section") (p nil "Visible conclusion."))
     nil)
    (goto-char (point-min))
    (outline-hide-subtree)
    (let* ((body (save-excursion (search-forward "word25") (match-beginning 0)))
           (fold (cl-find-if (lambda (overlay)
                               (eq (overlay-get overlay 'invisible) 'outline))
                             (overlays-at body))))
      (should fold)
      (should (outline-invisible-p body))
      (split-window-right 35)
      (fanyi-shr-reflow)
      (should (eq (overlay-buffer fold) (current-buffer)))
      (should-not (outline-invisible-p (point-min)))
      (goto-char (point-min))
      (search-forward "word25")
      (should (outline-invisible-p (match-beginning 0)))
      (search-forward "## Visible section")
      (should-not (outline-invisible-p (match-beginning 0)))
      (search-forward "Visible conclusion.")
      (should-not (outline-invisible-p (match-beginning 0))))))

(provide 'fanyi-shr-reflow-tests)
;;; fanyi-shr-reflow-tests.el ends here
