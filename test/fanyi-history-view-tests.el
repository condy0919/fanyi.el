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

(provide 'fanyi-history-view-tests)
;;; fanyi-history-view-tests.el ends here
