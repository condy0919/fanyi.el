;;; fanyi-retry-tests.el --- Independent dictionary retry tests -*- lexical-binding: t; -*-

(require 'fanyi-test-support)
(require 'fanyi-longman)

(ert-deftest fanyi-retry-remains-available-with-read-only-diagnostic-log ()
  (let ((fanyi-log-buffer-name (generate-new-buffer-name " *fanyi-test-log*")))
    (unwind-protect
        (fanyi-test--with-query
          (setq fanyi-log t)
          (with-current-buffer (get-buffer-create fanyi-log-buffer-name) (special-mode))
          (fanyi-dwim "sample")
          (fanyi-test--respond (car fanyi-test--requests) '(:error (error http 503)))
          (with-current-buffer fanyi-buffer-name
            (should (= fanyi--tasks-failed 1))
            (should (string-match-p "Request failed" (buffer-string)))
            (let ((task (car (fanyi-test--button-data #'fanyi--retry-task))))
              (should task)
              (fanyi--retry-task task)
              (fanyi-test--respond (car (last fanyi-test--requests)) nil "recovered")
              (should (= fanyi--tasks-completed 1))
              (should (= fanyi--tasks-failed 0))
              (should (string-match-p "recovered" (buffer-string)))))
          (with-current-buffer fanyi-log-buffer-name
            (should (string-match-p "503" (buffer-string)))))
      (when (get-buffer fanyi-log-buffer-name) (kill-buffer fanyi-log-buffer-name)))))

(ert-deftest fanyi-retry-replaces-only-failure-and-preserves-html-reading-state ()
  (fanyi-test--with-query
    (setq fanyi-buffer-name (generate-new-buffer-name "fanyi-retry-test"))
    (let ((fanyi-providers (list fanyi-longman-provider
                                 (fanyi-test--provider "https://failed.invalid/%s")
                                 (fanyi-test--provider "https://later.invalid/%s")))
          (fanyi-use-glyphs nil))
      (fanyi-dwim "explode")
      (fanyi-test--deliver
       (car fanyi-test--requests) nil
       (with-temp-buffer
         (insert-file-contents (expand-file-name "longman-explode-saved.html" fanyi-test--fixture-directory))
         (buffer-string)))
      (fanyi-test--respond (cadr fanyi-test--requests) '(:error (error http 503)))
      (fanyi-test--respond (nth 2 fanyi-test--requests) nil "FOLLOWING DICTIONARY")
      (with-current-buffer fanyi-buffer-name
        (save-window-excursion
          (set-window-buffer nil (current-buffer))
          (button-activate (fanyi-test--longman-style-button "THESAURUS"))
          ;; Reflow changes the failed section's positions before the retry.
          (let ((shr-width 45)) (fanyi-shr-reflow))
          (fanyi-test--fontify)
          (let* ((failed (cl-find-if #'fanyi--task-failure fanyi--tasks))
                 (request (fanyi--task-request failed))
                 (region (car fanyi-shr--regions))
                 (section (car (fanyi-shr--region-sections region)))
                 (html (buffer-substring (fanyi-shr--region-start region)
                                         (fanyi-shr--region-end region))))
            (goto-char (point-min))
            (search-forward "In everyday English")
            (set-mark (match-beginning 0))
            (let ((selected (buffer-substring-no-properties (mark) (point)))
                  (mark-active t) (deactivate-mark nil) (transient-mark-mode t)
                  ;; Retry uses the original request even if enabled providers change.
                  (fanyi-providers nil))
              (fanyi--retry-task failed)
              (fanyi--retry-task failed)
              (should (= (length fanyi-test--requests) 4))
              (let ((retry (cl-find-if-not #'fanyi--task-done fanyi--tasks)))
                (should (eq (fanyi--task-request retry) request))
                (fanyi--retry-task retry)
                (should (= (length fanyi-test--requests) 4)))
              (should (= fanyi--tasks-completed 2))
              (should (= fanyi--tasks-failed 0))
              (fanyi-test--respond (cadr fanyi-test--requests) nil "STALE RESPONSE")
              (fanyi-test--respond (nth 3 fanyi-test--requests) nil "RETRIED DEFINITION")
              (should (= fanyi--tasks-completed 3))
              (should (= fanyi--tasks-failed 0))
              (should (use-region-p))
              (should (equal selected (buffer-substring-no-properties (mark) (point))))
              (should (eq region (car fanyi-shr--regions)))
              (should (eq section (car (fanyi-shr--region-sections region))))
              (should (fanyi-shr--section-closed section))
              (should (equal-including-properties html
                                                  (buffer-substring (fanyi-shr--region-start region)
                                                                    (fanyi-shr--region-end region))))
              (should (string-match-p "RETRIED DEFINITION\nexplode: FOLLOWING DICTIONARY"
                                      (buffer-string)))
              (should-not (string-match-p "STALE RESPONSE\\|Retry dictionary\\|Retrying" (buffer-string)))
              (should (equal fanyi-history '("explode"))))))))))

(ert-deftest fanyi-retry-handles-render-failure-timeout-and-cancellation ()
  (fanyi-test--with-query
    (let (failure)
      (setf (fanyi-provider-render (car fanyi-providers))
            (lambda (result request)
              (fanyi-test--render result request)
              (when failure (signal failure '("Retry interrupted")))))
      (fanyi-dwim "word")
      (fanyi-test--respond (car fanyi-test--requests) '(:error (error http 500)))
      (dolist (stage '(error timeout quit success))
        (with-current-buffer fanyi-buffer-name
          (fanyi--retry-task (car fanyi--tasks)))
        (let* ((task (car (fanyi-test--query-state 'fanyi--tasks)))
               (response (car (last fanyi-test--requests))))
          (setq failure (and (memq stage '(error quit)) stage))
          (if (eq stage 'timeout)
              (fanyi--timeout task)
            (condition-case err
                (fanyi-test--respond response nil (if failure "PARTIAL" "complete"))
              (quit (should (eq stage 'quit)) (should (equal err '(quit "Retry interrupted"))))))
          (should (fanyi--task-done task))
          (should-not (fanyi--task-timer task))
          (with-current-buffer fanyi-buffer-name
            (should-not (string-match-p "PARTIAL\\|Retrying" (buffer-string)))
            (if (eq stage 'success)
                (progn
                  (should (= fanyi--tasks-failed 0))
                  (should (= fanyi--tasks-completed 1))
                  (should (equal (buffer-string) "word: complete\n")))
              (should (= fanyi--tasks-failed 1))
              (should (string-match-p "Retry dictionary" (buffer-string)))
              (should (< (fanyi--task-start task) (fanyi--task-end task))))))))))

(ert-deftest fanyi-retry-synchronous-response-cleans-up-and-settles-once ()
  (fanyi-test--with-query
    (fanyi-dwim "word")
    (fanyi-test--respond (car fanyi-test--requests) '(:error (error http 500)))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (&rest args)
                 (let ((buffer (apply #'fanyi-test--retrieve args)))
                   (fanyi-test--respond (car (last fanyi-test--requests)) nil "complete")
                   buffer))))
      (with-current-buffer fanyi-buffer-name (fanyi--retry-task (car fanyi--tasks))))
    (with-current-buffer fanyi-buffer-name
      (should (= fanyi--tasks-completed 1))
      (should (= fanyi--tasks-failed 0))
      (should (= (length fanyi--tasks) 1))
      (should-not (fanyi--task-timer (car fanyi--tasks)))
      (should (equal (buffer-string) "word: complete\n")))
    (should-not (cl-some #'buffer-live-p fanyi-test--buffers))))

(ert-deftest fanyi-retry-saved-result-stays-independent-of-new-queries ()
  (fanyi-test--with-query
    (fanyi-dwim "saved")
    (fanyi-test--respond (car fanyi-test--requests) '(:error (error http 500)))
    (let ((saved fanyi--active-buffer))
      (unwind-protect
          (progn
            (with-current-buffer saved (rename-buffer (generate-new-buffer-name " *saved-fanyi*")))
            (fanyi-dwim "new")
            (with-current-buffer saved (fanyi--retry-task (car fanyi--tasks)))
            ;; A subsequent main query cancels the previous main query, while
            ;; the explicit retry still belongs to the saved result's generation.
            (fanyi-dwim "newer")
            (fanyi-test--respond (nth 2 fanyi-test--requests) nil "restored")
            (fanyi-test--respond (nth 3 fanyi-test--requests) nil "latest")
            (with-current-buffer saved
              (should (equal fanyi-current-word "saved"))
              (should (= fanyi--tasks-completed 1))
              (should (equal (buffer-string) "saved: restored\n")))
            (with-current-buffer fanyi--active-buffer
              (should (equal fanyi-current-word "newer"))
              (should (= fanyi--tasks-completed 1))
              (should (equal (buffer-string) "newer: latest\n"))))
        (when (buffer-live-p saved) (kill-buffer saved))))))

(ert-deftest fanyi-retry-cancelled-by-a-new-query-remains-retryable ()
  (fanyi-test--with-query
    (fanyi-dwim "saved")
    (fanyi-test--respond (car fanyi-test--requests) '(:error (error http 500)))
    (let ((saved fanyi--active-buffer))
      (unwind-protect
          (progn
            (with-current-buffer saved
              (rename-buffer (generate-new-buffer-name " *saved-fanyi*"))
              (fanyi--retry-task (car fanyi--tasks))
              (should (= fanyi--tasks-failed 0)))
            (fanyi-dwim "new")
            (with-current-buffer saved
              (should (= fanyi--tasks-failed 1))
              (should (string-match-p "Retry cancelled" (buffer-string)))
              (should-not (string-match-p "Retrying" (buffer-string)))
              (fanyi--retry-task (car fanyi--tasks))
              (should (= fanyi--tasks-failed 0)))
            (fanyi-test--respond (nth 3 fanyi-test--requests) nil "complete")
            (with-current-buffer saved
              (should (= fanyi--tasks-failed 0))
              (should (= fanyi--tasks-completed 1))))
        (when (buffer-live-p saved) (kill-buffer saved))))))

(ert-deftest fanyi-query-earlier-render-failure-preserves-following-html-and-reading-state ()
  (dolist (failure '(error quit))
    (fanyi-test--with-query
      (setq fanyi-buffer-name (generate-new-buffer-name "fanyi-order-test"))
      (let ((bad (fanyi-test--provider)) (fanyi-use-glyphs nil))
        (setf (fanyi-provider-render bad)
              (lambda (&rest _)
                (fanyi-shr-insert '(p nil "PARTIAL HTML") nil)
                (signal failure '("Interrupted first dictionary"))))
        (setq fanyi-providers (list bad fanyi-longman-provider))
        (fanyi-dwim "explode")
        (fanyi-test--deliver
         (cadr fanyi-test--requests) nil
         (with-temp-buffer
           (insert-file-contents (expand-file-name "longman-explode-saved.html" fanyi-test--fixture-directory))
           (buffer-string)))
        (with-current-buffer fanyi-buffer-name
          (save-window-excursion
            (set-window-buffer nil (current-buffer))
            (button-activate (fanyi-test--longman-style-button "THESAURUS"))
            (let* ((successor (car fanyi--tasks))
                   (region (car fanyi-shr--regions))
                   (section (car (fanyi-shr--region-sections region)))
                   (html (buffer-string)))
              (goto-char (point-min))
              (set-mark (+ (point) 4))
              (narrow-to-region (point-min) (1- (point-max)))
              (let ((mark-active t) (transient-mark-mode t) (deactivate-mark nil)
                    (selected (buffer-substring-no-properties (point) (mark)))
                    (narrowed (buffer-string)))
                (set-window-start nil (point-min))
                (condition-case err
                    (fanyi-test--respond (car fanyi-test--requests) nil "first")
                  (quit (should (eq failure 'quit))))
                (should (use-region-p))
                (should (equal selected (buffer-substring-no-properties (point) (mark))))
                (should (equal-including-properties narrowed (buffer-string)))
                (should (= (point-min) (window-start)))
                (widen)
                (should (eq region (car fanyi-shr--regions)))
                (should (= (length fanyi-shr--regions) 1))
                (should (eq section (car (fanyi-shr--region-sections region))))
                (should (fanyi-shr--section-closed section))
                (should (overlay-buffer (fanyi-shr--section-overlay section)))
                (should (equal-including-properties html
                                                    (buffer-substring (fanyi--task-start successor)
                                                                      (fanyi--task-end successor))))))))))))

(provide 'fanyi-retry-tests)
;;; fanyi-retry-tests.el ends here
