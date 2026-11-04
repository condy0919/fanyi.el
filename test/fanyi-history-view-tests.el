;;; fanyi-history-view-tests.el --- Cached reading views -*- lexical-binding: t; -*-

(require 'fanyi-test-support)

(defconst fanyi-history-view-test--text
  (mapconcat (lambda (number) (format "row%02d content" number))
             (number-sequence 0 59) "\n"))

(defun fanyi-history-view-test--row (number)
  "Return the start of row NUMBER in the current result."
  (save-excursion
    (goto-char (point-min))
    (search-forward (format "row%02d" number))
    (match-beginning 0)))

(ert-deftest fanyi-history-keeps-window-views-when-focus-changes ()
  (fanyi-test--with-query
    (fanyi-dwim "alpha")
    (fanyi-test--respond (car fanyi-test--requests) nil fanyi-history-view-test--text)
    (save-window-excursion
      (delete-other-windows)
      (set-window-buffer nil (get-buffer fanyi-buffer-name))
      (let* ((left (selected-window))
             (right (split-window-right)) expected)
        (with-current-buffer fanyi-buffer-name
          (set-window-point left (fanyi-history-view-test--row 5))
          (set-window-start left (fanyi-history-view-test--row 3) t)
          (set-window-point right (fanyi-history-view-test--row 25))
          (set-window-start right (fanyi-history-view-test--row 23) t)
          (setq expected (list (window-point left) (window-start left)
                               (window-point right) (window-start right)))
          (fanyi-dwim "beta")
          (fanyi-test--respond (cadr fanyi-test--requests) nil "new page")
          (dolist (window (list right left))
            (select-window window)
            (fanyi-back)
            (should (equal (list (window-point left) (window-start left)
                                 (window-point right) (window-start right))
                           expected))
            (should (eq (selected-window) window))
            (fanyi-forward)))))))

(ert-deftest fanyi-history-restores-new-windows-without-touching-other-buffers ()
  (dolist (keep-old-window '(nil t))
    (ert-info ((format "Keep old window showing another buffer: %S" keep-old-window))
      (fanyi-test--with-query
        (with-temp-buffer
          (insert "Another buffer\nAnother line\n")
          (let ((other (current-buffer)))
            (fanyi-dwim "alpha")
            (fanyi-test--respond (car fanyi-test--requests) nil fanyi-history-view-test--text)
            (save-window-excursion
              (delete-other-windows)
              (set-window-buffer nil (get-buffer fanyi-buffer-name))
              (let* ((left (selected-window))
                     (right (split-window-right)) first start replacement)
                (with-current-buffer fanyi-buffer-name
                  (setq first (fanyi-history-view-test--row 5)
                        start (fanyi-history-view-test--row 3))
                  (set-window-point left first)
                  (set-window-start left start t)
                  (set-window-point right (fanyi-history-view-test--row 25))
                  (set-window-start right (fanyi-history-view-test--row 23) t)
                  (fanyi-dwim "beta")
                  (fanyi-test--respond (cadr fanyi-test--requests) nil "new page")
                  (if keep-old-window
                      (progn
                        (set-window-buffer right other)
                        (set-window-point right 5)
                        (set-window-start right 1 t))
                    (delete-window right))
                  (setq replacement (split-window left nil 'below))
                  (set-window-buffer replacement (current-buffer))
                  (select-window replacement)
                  (fanyi-back)
                  (dolist (window (list left replacement))
                    (should (= (window-point window) first))
                    (should (= (window-start window) start)))
                  (when keep-old-window
                    (should (eq (window-buffer right) other))
                    (should (= (window-point right) 5))
                    (should (= (window-start right) 1))))))))))))

(ert-deftest fanyi-history-failed-render-keeps-errors-visible-and-healthy-views-intact ()
  (dolist (view '(failed healthy folded))
    (ert-info ((format "Previously viewed part: %S" view))
      (fanyi-test--with-query
        (let ((first (fanyi-test--provider))
              (healthy (fanyi-test--provider))
              (case-fold-search nil) fail saved-text)
          (setf (fanyi-provider-name first) "First"
                (fanyi-provider-name healthy) "Healthy"
                (fanyi-provider-render first)
                (lambda (result request)
                  (if fail
                      (error "Cannot render old dictionary")
                    (insert "# " (fanyi-request-display-name request) "\n\n" result "\n\n")))
                (fanyi-provider-render healthy)
                (lambda (result request)
                  (insert "# " (fanyi-request-display-name request) "\n\n" result "\n\n")))
          (setq fanyi-providers (list first healthy))
          (fanyi-dwim "alpha")
          (fanyi-test--respond (nth 0 fanyi-test--requests) nil
                               (concat (make-string 500 ?x) "\nchosen paragraph with some words"))
          (fanyi-test--respond (nth 1 fanyi-test--requests) nil "healthy paragraph with some words")
          (with-current-buffer fanyi-buffer-name
            (goto-char (point-min))
            (if (eq view 'folded)
                (outline-hide-subtree)
              (search-forward (if (eq view 'failed) "chosen" "healthy"))
              (set-mark (match-beginning 0))
              (setq mark-active t deactivate-mark nil)
              (narrow-to-region (mark) (line-end-position))
              (setq saved-text (buffer-string)))
            (setq fanyi-providers (list healthy))
            (fanyi-dwim "beta")
            (fanyi-test--respond (nth 2 fanyi-test--requests) nil "new page")
            (setq fail t)
            (fanyi-back)
            (should (= fanyi--tasks-failed 1))
            (should (= fanyi--tasks-completed 1))
            (if (eq view 'healthy)
                (progn
                  (should (buffer-narrowed-p))
                  (should (equal (buffer-string) saved-text))
                  (should mark-active)
                  (should (equal (buffer-substring-no-properties (mark) (point)) "healthy")))
              (should-not (buffer-narrowed-p))
              (should (= (point) (point-min)))
              (when (eq view 'failed) (should-not mark-active))
              (search-forward "Cannot render old dictionary")
              (should-not (invisible-p (match-beginning 0)))
              (search-forward "Retry dictionary")
              (should-not (invisible-p (match-beginning 0)))
              (should (button-at (match-beginning 0))))))))))

(ert-deftest fanyi-history-existing-failure-keeps-its-reading-view ()
  (fanyi-test--with-query
    (fanyi-dwim "alpha")
    (fanyi-test--respond (car fanyi-test--requests) '(:error (error http 500)))
    (with-current-buffer fanyi-buffer-name
      (goto-char (point-min))
      (search-forward "Request failed")
      (set-mark (match-beginning 0))
      (setq mark-active t deactivate-mark nil)
      (narrow-to-region (mark) (line-end-position))
      (let ((text (buffer-string)))
        (fanyi-dwim "beta")
        (fanyi-test--respond (cadr fanyi-test--requests))
        (fanyi-back)
        (should (buffer-narrowed-p))
        (should (equal (buffer-string) text))
        (should mark-active)
        (should (equal (buffer-substring-no-properties (mark) (point)) "Request failed"))))))

(provide 'fanyi-history-view-tests)
;;; fanyi-history-view-tests.el ends here
