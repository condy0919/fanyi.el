;;; fanyi-anchor-tests.el --- Precise text and whitespace anchors -*- lexical-binding: t; -*-

(require 'fanyi-test-support)

(ert-deftest fanyi-anchors-round-trip-every-position-within-unchanged-text ()
  (dolist (text '("" " \t\n\r " "中文  🙂\tword\n\n  last  " "a b　c"))
    (with-temp-buffer
      (insert text)
      (cl-loop for position from (point-min) to (point-max)
               do (let ((anchor (fanyi-shr--text-anchor (point-min) position)))
                    (should (= (apply #'fanyi-shr--text-position
                                      (point-min) (point-max) anchor)
                               position)))))))

(ert-deftest fanyi-anchors-clamp-whitespace-offsets-without-entering-other-words ()
  (with-temp-buffer
    (insert "中文  \t\n🙂 end")
    (let ((space (fanyi-shr--text-anchor 1 5))
          (word-end (fanyi-shr--text-anchor 1 3))
          (next-word (fanyi-shr--text-anchor 1 7)))
      (dolist (gap '(" " "\n  " "\t\n\n  "))
        (erase-buffer)
        (insert "中文" gap "🙂 end")
        (should (= (apply #'fanyi-shr--text-position 1 (point-max) space)
                   (+ 3 (min 2 (length gap)))))
        (should (= (apply #'fanyi-shr--text-position 1 (point-max) word-end) 3))
        (should (= (apply #'fanyi-shr--text-position 1 (point-max) next-word)
                   (+ 3 (length gap))))))))

(ert-deftest fanyi-history-restores-whitespace-selections-in-both-directions ()
  (dolist (run '("  " "\t \t" "\n\n\n" "\r\n\r\n"))
    (dolist (reverse-selection '(nil t))
      (fanyi-test--with-query
        (fanyi-dwim "alpha")
        (fanyi-test--respond (car fanyi-test--requests) nil (concat "中文" run "🙂 end"))
        (with-current-buffer fanyi-buffer-name
          (goto-char (point-min))
          (search-forward "中文")
          (forward-char 1)
          (set-mark (point))
          (forward-char 1)
          (when reverse-selection (exchange-point-and-mark))
          (setq mark-active t deactivate-mark nil)
          (let ((before (point))
                (before-mark (mark))
                (selected (buffer-substring-no-properties (region-beginning) (region-end))))
            (fanyi-dwim "beta")
            (fanyi-test--respond (cadr fanyi-test--requests))
            (fanyi-back)
            (should (= (length fanyi-test--requests) 2))
            (should mark-active)
            (should (= (point) before))
            (should (= (mark) before-mark))
            (should (equal (buffer-substring-no-properties (region-beginning) (region-end))
                           selected))))))))

(ert-deftest fanyi-reflow-restores-whitespace-selections-and-clamps-new-layout ()
  (dolist (run '("  " "\t \t" "\n\n\n"))
    (dolist (reverse-selection '(nil t))
      (fanyi-shr-test--with-window
        (let ((shr-width 60))
          (fanyi-shr-insert
           `(sample nil ,run)
           '((sample . (lambda (dom)
                         (insert "中文" (if (> shr-internal-width 30) (nth 2 dom) " ")
                                 "🙂 end")))))
          (goto-char (point-min))
          (search-forward "中文")
          (forward-char 1)
          (set-mark (point))
          (forward-char 1)
          (when reverse-selection (exchange-point-and-mark))
          (let ((mark-active t) (deactivate-mark nil)
                (before (point)) (before-mark (mark)))
            ;; Force the same renderer/layout, as when regenerating SVG labels.
            (fanyi-shr-reflow t)
            (should (= (point) before))
            (should (= (mark) before-mark))
            (should mark-active)
            ;; A narrower layout shortens this whitespace run.  Neither
            ;; endpoint may move into the following Unicode word.
            (setq shr-width 20)
            (fanyi-shr-reflow)
            (should (= (point) 4))
            (should (= (mark) 4))
            (should (looking-at-p "🙂"))))))))

(provide 'fanyi-anchor-tests)
;;; fanyi-anchor-tests.el ends here
