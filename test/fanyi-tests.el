;;; fanyi-tests.el --- Query lifecycle tests -*- lexical-binding: t; -*-

(require 'fanyi-test-support)

(ert-deftest fanyi-query-normalizes-input-before-building-requests ()
  (fanyi-test--with-query
    (fanyi-dwim " \tChase  the Dragon\n ")
    (let ((request (fanyi--task-request (car (fanyi-test--query-state 'fanyi--tasks)))))
      (should (equal (fanyi-request-word request) "chase  the dragon"))
      (should (equal (fanyi-request-url request)
                     "https://example.invalid/chase%20%20the%20dragon")))
    (should (equal (fanyi-test--query-state 'fanyi-current-word) "Chase  the Dragon"))
    (should (equal fanyi-history '("Chase  the Dragon")))))

(ert-deftest fanyi-query-invalid-input-preserves-running-query ()
  (fanyi-test--with-query
    (fanyi-dwim "running")
    (with-current-buffer fanyi-buffer-name
      (let ((inhibit-read-only t))
        (insert "Existing results")))
    (let* ((task (car (fanyi-test--query-state 'fanyi--tasks)))
           (timer (fanyi--task-timer task))
           (response (fanyi-test-request-buffer (car fanyi-test--requests)))
           (process (make-pipe-process :name "fanyi-test-valid-query"
                                       :buffer response :noquery t))
           (query-id fanyi--query-id))
      (cl-letf (((symbol-function 'fanyi--resolve-provider)
                 (lambda (&rest _)
                   (ert-fail "Invalid input must not prepare providers"))))
        (dolist (input '(nil 42 word ("word") "" " \t\n "))
          (should-error (fanyi-dwim input) :type 'user-error)
          (should (equal (fanyi-test--query-state 'fanyi--tasks) (list task)))
          (should-not (fanyi--task-done task))
          (should (memq timer timer-list))
          (should (buffer-live-p response))
          (should (process-live-p process))
          (should (= (length fanyi-test--requests) 1))
          (should (= fanyi--query-id query-id))
          (should (equal (fanyi-test--query-state 'fanyi-current-word) "running"))
          (should (equal fanyi-history '("running")))
          (with-current-buffer fanyi-buffer-name
            (should (equal (buffer-string) "Existing results")))))
      (fanyi-test--respond (car fanyi-test--requests) nil "completed")
      (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 1)))))

(ert-deftest fanyi-query-entry-points-share-recent-deduplicated-history ()
  (fanyi-test--with-query
    (let ((history-delete-duplicates nil))
      (setq fanyi-history (list "first" "second"))
      (fanyi-dwim (copy-sequence "second"))
      (should (equal fanyi-history '("second" "first")))
      (with-temp-buffer
        (insert "second")
        (goto-char (point-min))
        (fanyi-dwim2))
      (should (equal fanyi-history '("second" "first")))
      (fanyi-bookmark-jump '("first" (handler . fanyi-bookmark-jump)))
      (should (equal fanyi-history '("first" "second")))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt candidates &rest _)
                   (should (equal candidates '("first" "second")))
                   (copy-sequence "second"))))
        (fanyi-from-history))
      (should (equal fanyi-history '("second" "first")))
      ;; Old persisted history may contain a duplicate of its first entry.
      (setq fanyi-history (list "second" "first" "second"))
      (fanyi-dwim (copy-sequence "second"))
      (should (equal fanyi-history '("second" "first"))))))

(ert-deftest fanyi-query-interactive-input-records-only-normalized-history ()
  (fanyi-test--with-query
    (setq fanyi-history '("previous"))
    (with-temp-buffer
      (insert "default")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt _initial history default &rest _)
                   (should (eq history 'fanyi-history))
                   (should (equal default "default"))
                   (should-not history-add-new-input)
                   " \tMixed Case\n ")))
        (call-interactively #'fanyi-dwim)))
    (should (equal (fanyi-test--query-state 'fanyi-current-word) "Mixed Case"))
    (should (equal fanyi-history '("Mixed Case" "previous")))))

(ert-deftest fanyi-query-failed-preparation-does-not-record-history ()
  (fanyi-test--with-query
    (setq fanyi-history '("previous"))
    (setf (fanyi-provider-prepare (car fanyi-providers))
          (lambda (_request) (error "Cannot prepare request")))
    (should-error (fanyi-dwim "next"))
    (should (equal fanyi-history '("previous")))
    (should-not (fanyi-test--query-state 'fanyi-current-word))
    (should-not fanyi-test--requests)
    (should-not (get-buffer fanyi-buffer-name))))

(ert-deftest fanyi-query-ignores-stale-success ()
  (fanyi-test--with-query
    (fanyi-dwim "old")
    (let ((old-request (car fanyi-test--requests)))
      (fanyi-dwim "new")
      (should-not (buffer-live-p
                   (fanyi-test--respond old-request '(:peer nil) "obsolete")))
      (with-current-buffer fanyi-buffer-name
        (should (equal (buffer-string) "")))
      (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 0))
      (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 0))
      (should (= (length (fanyi-test--query-state 'fanyi--tasks)) 1))
      (fanyi-test--respond (cadr fanyi-test--requests) '(:peer nil) "current")
      (with-current-buffer fanyi-buffer-name
        (should (equal (buffer-string) "new: current\n")))
      (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 1))
      (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 0)))))

(ert-deftest fanyi-query-ignores-stale-error ()
  (fanyi-test--with-query
    (fanyi-dwim "old")
    (let ((old-request (car fanyi-test--requests)))
      (fanyi-dwim "new")
      (should-not (buffer-live-p
                   (fanyi-test--respond old-request '(:error (error http 500)))))
      (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 0))
      (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 0))
      (with-current-buffer fanyi-buffer-name
        (should (equal (buffer-string) "")))
      (fanyi-test--respond (cadr fanyi-test--requests) '(:peer nil))
      (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 1)))))

(ert-deftest fanyi-query-repeated-word-still-invalidates-old-request ()
  (fanyi-test--with-query
    (fanyi-dwim "word")
    (let ((old-request (car fanyi-test--requests)))
      (fanyi-dwim "word")
      (fanyi-test--respond (cadr fanyi-test--requests) '(:peer nil) "current")
      (fanyi-test--respond old-request '(:peer nil) "obsolete")
      (with-current-buffer fanyi-buffer-name
        (should (equal (buffer-string) "word: current\n")))
      (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 1))
      (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 0)))))

(ert-deftest fanyi-query-cancels-previous-request ()
  (fanyi-test--with-query
    (fanyi-dwim "old")
    (let* ((request (car fanyi-test--requests))
           (timer (fanyi--task-timer (car (fanyi-test--query-state 'fanyi--tasks))))
           (buffer (fanyi-test-request-buffer request))
           (process (make-pipe-process :name "fanyi-test-pending"
                                       :buffer buffer :noquery t)))
      (should (process-live-p process))
      (fanyi-dwim "new")
      (should-not (process-live-p process))
      (should-not (buffer-live-p buffer))
      (should-not (memq timer timer-list))
      (should (= (length (fanyi-test--query-state 'fanyi--tasks)) 1))
      (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 0))
      (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 0)))))

(ert-deftest fanyi-query-cancels-redirected-request ()
  (fanyi-test--with-query
    (fanyi-dwim "old")
    (let* ((original (fanyi-test-request-buffer (car fanyi-test--requests)))
           (redirected (fanyi-test--response-buffer))
           (process (make-pipe-process :name "fanyi-test-redirected"
                                       :buffer redirected :noquery t)))
      (with-current-buffer original
        (setq-local url-redirect-buffer redirected))
      (should (process-live-p process))
      (fanyi-dwim "new")
      (should-not (process-live-p process))
      (should-not (buffer-live-p original))
      (should-not (buffer-live-p redirected))
      (should (= (length (fanyi-test--query-state 'fanyi--tasks)) 1))
      (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 0))
      (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 0)))))

(ert-deftest fanyi-query-killing-result-buffer-cancels-pending-resources ()
  (fanyi-test--with-query
    (setq fanyi-providers (list (car fanyi-providers) (car fanyi-providers)))
    (fanyi-dwim "word")
    (let ((tasks (copy-sequence (fanyi-test--query-state 'fanyi--tasks)))
          (timers (mapcar #'fanyi--task-timer (fanyi-test--query-state 'fanyi--tasks)))
          buffers processes)
      (dolist (request fanyi-test--requests)
        (let ((original (fanyi-test-request-buffer request))
              (redirected (fanyi-test--response-buffer)))
          (with-current-buffer original
            (setq-local url-redirect-buffer redirected))
          (dolist (buffer (list original redirected))
            (push buffer buffers)
            (push (make-pipe-process :name "fanyi-test-close-pending"
                                     :buffer buffer :noquery t)
                  processes))))
      (should (cl-every #'timerp timers))
      (should (cl-every #'process-live-p processes))
      (kill-buffer fanyi-buffer-name)
      (should (cl-every #'fanyi--task-done tasks))
      (dolist (task tasks) (should-not (fanyi--task-timer task)))
      (dolist (timer timers) (should-not (memq timer timer-list)))
      (dolist (process processes) (should-not (process-live-p process)))
      (dolist (buffer buffers) (should-not (buffer-live-p buffer))))))

(ert-deftest fanyi-query-killed-results-ignore-late-callbacks-and-timeout ()
  (dolist (replacement '(nil t))
    (ert-info ((format "Create another buffer with the old name: %S" replacement))
      (fanyi-test--with-query
        (fanyi-dwim "word")
        (let ((request (car fanyi-test--requests))
              (task (car (fanyi-test--query-state 'fanyi--tasks)))
              replacement-buffer)
          (kill-buffer fanyi-buffer-name)
          (when replacement
            (setq replacement-buffer (get-buffer-create fanyi-buffer-name))
            (with-current-buffer replacement-buffer
              (insert "Unrelated result buffer")
              (fanyi-mode)))
          (dolist (status '(nil (:error (error http 500))))
            (should-not (buffer-live-p (fanyi-test--respond request status "too late"))))
          ;; A timeout callback may already have been queued when cancelled.
          (fanyi--timeout task)
          (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 0))
          (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 0))
          (if replacement
              (with-current-buffer replacement-buffer
                (should (eq (get-buffer fanyi-buffer-name) replacement-buffer))
                (should (equal (buffer-string) "Unrelated result buffer"))
                (should (eq major-mode 'fanyi-mode)))
            (should-not (get-buffer fanyi-buffer-name))))))))

(ert-deftest fanyi-query-changing-result-mode-cancels-pending-work ()
  (fanyi-test--with-query
    (fanyi-dwim "word")
    (let ((request (car fanyi-test--requests))
          (task (car (fanyi-test--query-state 'fanyi--tasks)))
          (timer (fanyi--task-timer (car (fanyi-test--query-state 'fanyi--tasks)))))
      (with-current-buffer fanyi-buffer-name
        (fundamental-mode)
        (let ((inhibit-read-only t)) (insert "Reused for editing")))
      (should (fanyi--task-done task))
      (should-not (memq timer timer-list))
      (should-not (buffer-live-p (fanyi-test-request-buffer request)))
      (dolist (status '(nil (:error (error http 500))))
        (should-not (buffer-live-p (fanyi-test--respond request status "too late"))))
      (with-current-buffer fanyi-buffer-name
        (should (eq major-mode 'fundamental-mode))
        (should (equal (buffer-string) "Reused for editing"))))))

(ert-deftest fanyi-query-quit-hides-results-without-cancelling-them ()
  (fanyi-test--with-query
    (fanyi-dwim "word")
    (let ((target (get-buffer fanyi-buffer-name))
          (task (car (fanyi-test--query-state 'fanyi--tasks))))
      (save-window-excursion
        (set-window-buffer nil target)
        (with-current-buffer target
          (call-interactively (key-binding (kbd "q"))))
        (should (buffer-live-p target))
        (should-not (eq (window-buffer) target))
        (should-not (fanyi--task-done task))
        (should (memq (fanyi--task-timer task) timer-list))
        (fanyi-test--respond (car fanyi-test--requests) nil "ready when reopened")
        (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 1))
        (with-current-buffer target
          (should (eq major-mode 'fanyi-mode))
          (should buffer-read-only)
          (should (equal (buffer-string) "word: ready when reopened\n")))))))

(ert-deftest fanyi-query-renamed-results-retain-their-buffer-identity ()
  (fanyi-test--with-query
    (fanyi-dwim "word")
    (let ((target (get-buffer fanyi-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer target
              (rename-buffer (generate-new-buffer-name " *fanyi-test-renamed*")))
            (with-current-buffer (get-buffer-create fanyi-buffer-name)
              (insert "Different buffer with the original name")
              (fanyi-mode))
            (fanyi-test--respond (car fanyi-test--requests) nil "original lookup")
            (with-current-buffer target
              (should (equal (buffer-string) "word: original lookup\n")))
            (with-current-buffer fanyi-buffer-name
              (should (equal (buffer-string) "Different buffer with the original name")))
            (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 1)))
        (when (buffer-live-p target) (kill-buffer target))))))

(ert-deftest fanyi-query-saved-results-keep-their-own-word-and-status ()
  (require 'ol-fanyi)
  (fanyi-test--with-query
    (setq fanyi-providers (list (car fanyi-providers) (car fanyi-providers)))
    (fanyi-dwim "Dragon")
    (fanyi-test--respond (car fanyi-test--requests) nil "old definition")
    (let ((saved (get-buffer fanyi-buffer-name))
          (late (cadr fanyi-test--requests))
          (kill-ring nil) stored)
      (push saved fanyi-test--buffers)
      (with-current-buffer saved
        (rename-buffer (generate-new-buffer-name " *fanyi-saved-query*"))
        ;; Starting another query from an old result must not overwrite its word.
        (fanyi-dwim "Sun"))
      (fanyi-test--respond (nth 2 fanyi-test--requests) nil "first new definition")
      (fanyi-test--respond (nth 3 fanyi-test--requests) nil "second new definition")
      (fanyi-test--respond late nil "obsolete")
      (with-current-buffer saved
        (should (equal fanyi-current-word "Dragon"))
        (should (equal (buffer-string) "dragon: old definition\n"))
        (should (equal (substring-no-properties (fanyi-format-header-line))
                       "Translating Dragon 0 1 0"))
        (fanyi-copy-query-word)
        (should (equal (car kill-ring) "Dragon"))
        (should (equal (car (fanyi-bookmark-make-record)) "Dragon"))
        (cl-letf (((symbol-function 'org-link-store-props)
                   (lambda (&rest properties) (setq stored properties))))
          (fanyi-org-store-link))
        (should (equal (plist-get stored :link) "fanyi:Dragon")))
      (with-current-buffer fanyi-buffer-name
        (should (equal fanyi-current-word "Sun"))
        (should (equal (substring-no-properties (fanyi-format-header-line))
                       "Translating Sun 0 2 0"))))))

(ert-deftest fanyi-query-accepts-success-status-and-cleans-response ()
  (fanyi-test--with-query
    (dolist (status '(nil (:redirect "https://example.invalid/canonical")))
      (ert-info ((format "Success status: %S" status))
        (fanyi-dwim "word")
        (let* ((task (car (fanyi-test--query-state 'fanyi--tasks)))
               (timer (fanyi--task-timer task))
               (response (fanyi-test--respond (car (last fanyi-test--requests))
                                              status)))
          (should-not (buffer-live-p response))
          (should-not (memq timer timer-list))
          (should (fanyi--task-done task))
          (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 1))
          (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 0))
          (with-current-buffer fanyi-buffer-name
            (should (equal (buffer-string) "word: definition\n"))))))))

(ert-deftest fanyi-query-passes-request-options-to-http-retrieval ()
  (fanyi-test--with-query
    (let ((provider (car fanyi-providers)) captured)
      (setf (fanyi-provider-method provider) "POST"
            (fanyi-provider-headers provider) '(("X-Token" . "test-token"))
            (fanyi-provider-body provider) "payload")
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (url &rest args)
                   (setq captured (list url url-request-method
                                        url-request-extra-headers url-request-data))
                   (apply #'fanyi-test--retrieve url args))))
        (fanyi-dwim "Mixed Word?"))
      (should (equal captured
                     '("https://example.invalid/mixed%20word%3F" "POST"
                       (("X-Token" . "test-token")) "payload")))
      (should (equal (fanyi-test--query-state 'fanyi-current-word) "Mixed Word?")))))

(ert-deftest fanyi-query-shared-provider-keeps-requests-and-results-independent ()
  (fanyi-test--with-query
    (let* ((provider (car fanyi-providers))
           (before (prin1-to-string provider)))
      (setq fanyi-providers (list provider provider))
      (fanyi-dwim "word")
      (let ((first (cadr (fanyi-test--query-state 'fanyi--tasks)))
            (second (car (fanyi-test--query-state 'fanyi--tasks))))
        (should-not (eq (fanyi--task-request first) (fanyi--task-request second)))
        ;; Responses can arrive in reverse order, and a previous result can
        ;; still be rendered after the other request has finished parsing.
        (fanyi-test--respond (cadr fanyi-test--requests) nil "second result")
        (fanyi-test--respond (car fanyi-test--requests) nil "first result")
        (should (equal (fanyi--task-result first) "first result"))
        (should (equal (fanyi--task-result second) "second result"))
        (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 2))
        (should (equal (prin1-to-string provider) before))
        (with-current-buffer fanyi-buffer-name
          (should (equal (buffer-string) "word: first result\nword: second result\n")))
        (with-temp-buffer
          (let ((fanyi-buffer-name (buffer-name)))
            (let ((inhibit-read-only t)) (fanyi-render-result (fanyi--task-request second) (fanyi--task-result second))))
          (should (equal (buffer-string) "word: second result\n")))))))

(ert-deftest fanyi-query-isolates-failures-and-cleans-response ()
  (fanyi-test--with-query
    (setq fanyi-providers (list (car fanyi-providers)
                                (car fanyi-providers)))
    (dolist (failure '(http json parse render not-found))
      (ert-info ((format "Failure stage: %S" failure))
        (setq fanyi-test--requests nil)
        (fanyi-dwim "word")
        (let ((parse (symbol-function 'fanyi-parse-response))
              (render (symbol-function 'fanyi-render-result)))
          (cl-letf (((symbol-function 'fanyi-parse-response)
                     (lambda (&rest args)
                       (pcase failure
                         ('parse (error "Malformed dictionary response"))
                         ('not-found (throw 'not-found nil))
                         (_ (apply parse args)))))
                    ((symbol-function 'fanyi-render-result)
                     (lambda (&rest args)
                       (if (eq failure 'render)
                           (with-current-buffer fanyi-buffer-name
                             (let ((inhibit-read-only t))
                               (insert "PARTIAL RESULT")
                               (error "Cannot render dictionary")))
                         (apply render args)))))
            (let ((response
                   (fanyi-test--deliver
                    (car fanyi-test--requests)
                    (when (eq failure 'http) '(:error (error http 500)))
                    (if (eq failure 'json) "{invalid" "{\"definition\":\"valid\"}"))))
              (should-not (buffer-live-p response)))))
        (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 1))
        (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 0))
        (with-current-buffer fanyi-buffer-name
          (should (string-match-p "Retry dictionary" (buffer-string)))
          (should-not (string-match-p "PARTIAL RESULT" (buffer-string))))
        ;; A broken provider must not prevent another provider from finishing.
        (fanyi-test--respond (cadr fanyi-test--requests) nil "healthy")
        (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 1))
        (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 1))
        (with-current-buffer fanyi-buffer-name
          (should (string-match-p "word: healthy" (buffer-string))))))))

(ert-deftest fanyi-query-settles-callback-only-once ()
  (fanyi-test--with-query
    (fanyi-dwim "word")
    (let ((request (car fanyi-test--requests)))
      (fanyi-test--respond request nil "first")
      (should-not (buffer-live-p (fanyi-test--respond request nil "duplicate")))
      (should-not (buffer-live-p
                   (fanyi-test--respond request '(:error (error http 500)))))
      (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 1))
      (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 0))
      (with-current-buffer fanyi-buffer-name
        (should (equal (buffer-string) "word: first\n"))))))

(ert-deftest fanyi-query-http-error-skips-parsing-and-settles-once ()
  (fanyi-test--with-query
    (fanyi-dwim "word")
    (let* ((request (car fanyi-test--requests))
           (task (car (fanyi-test--query-state 'fanyi--tasks)))
           (timer (fanyi--task-timer task))
           parsed rendered)
      (cl-letf (((symbol-function 'json-parse-buffer)
                 (lambda (&rest _) (setq parsed t)))
                ((symbol-function 'fanyi-parse-response)
                 (lambda (&rest _) (setq parsed t)))
                ((symbol-function 'fanyi-render-result)
                 (lambda (&rest _) (setq rendered t))))
        ;; Error pages need not have the provider's response format.  Report
        ;; the HTTP failure before trying to interpret this HTML as JSON.
        (should-not (buffer-live-p
                     (fanyi-test--deliver request '(:error (error http 403))
                                          "<html><title>Forbidden</title></html>"))))
      (should-not parsed)
      (should-not rendered)
      (should (fanyi--task-done task))
      (should-not (fanyi--task-timer task))
      (should-not (memq timer timer-list))
      (with-current-buffer fanyi-buffer-name
        (should (string-match-p "Request failed.*403" (buffer-string)))
        (should (string-match-p "Retry dictionary" (buffer-string))))
      (let ((before (with-current-buffer fanyi-buffer-name (buffer-string))))
        ;; Both late success and another failure must leave the first failure
        ;; intact while still disposing of the recreated response buffer.
        (dolist (status '(nil (:error (error http 403))))
          (should-not (buffer-live-p (fanyi-test--respond request status "late"))))
        (with-current-buffer fanyi-buffer-name
          (should (equal (buffer-string) before))))
      (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 1))
      (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 0)))))

(ert-deftest fanyi-query-timeout-settles-once-and-cancels-request ()
  (fanyi-test--with-query
    (fanyi-dwim "word")
    (let* ((task (car (fanyi-test--query-state 'fanyi--tasks)))
           (timer (fanyi--task-timer task))
           (request (car fanyi-test--requests))
           (buffer (fanyi-test-request-buffer request))
           (process (make-pipe-process :name "fanyi-test-timeout"
                                       :buffer buffer :noquery t)))
      (should (timerp timer))
      (should (memq timer timer-list))
      (fanyi--timeout task)
      (should (fanyi--task-done task))
      (should-not (memq timer timer-list))
      (should-not (buffer-live-p buffer))
      (should-not (process-live-p process))
      (with-current-buffer fanyi-buffer-name
        (should (string-match-p "[Tt]im.*out" (buffer-string))))
      (let ((before (with-current-buffer fanyi-buffer-name (buffer-string))))
        (fanyi--timeout task)
        (should-not (buffer-live-p (fanyi-test--respond request nil "late")))
        (should-not (buffer-live-p
                     (fanyi-test--respond request '(:error (error http 500)))))
        (with-current-buffer fanyi-buffer-name
          (should (equal (buffer-string) before))))
      (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 1))
      (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 0))
      (with-current-buffer fanyi-buffer-name
        (should-not (string-match-p "word: late" (buffer-string)))))))

(ert-deftest fanyi-query-start-error-does-not-block-other-provider ()
  (fanyi-test--with-query
    (setq fanyi-providers (list (car fanyi-providers)
                                (car fanyi-providers)))
    (let ((attempts 0))
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (&rest args)
                   (if (= (cl-incf attempts) 1)
                       (error "Cannot start request")
                     (apply #'fanyi-test--retrieve args)))))
        (fanyi-dwim "word"))
      (should (= attempts 2)))
    (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 1))
    (fanyi-test--respond (car fanyi-test--requests) nil)
    (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 1))
    (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 1))
    (dolist (task (fanyi-test--query-state 'fanyi--tasks))
      (should (fanyi--task-done task))
      (should-not (fanyi--task-timer task)))))

(ert-deftest fanyi-query-synchronous-callback-leaves-no-timer ()
  (fanyi-test--with-query
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (&rest args)
                 (let ((buffer (apply #'fanyi-test--retrieve args)))
                   (fanyi-test--respond (car fanyi-test--requests) nil)
                   buffer))))
      (fanyi-dwim "word"))
    (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 1))
    (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 0))
    (should (fanyi--task-done (car (fanyi-test--query-state 'fanyi--tasks))))
    (should-not (fanyi--task-timer (car (fanyi-test--query-state 'fanyi--tasks))))
    (should-not (buffer-live-p
                 (fanyi-test-request-buffer (car fanyi-test--requests))))))

(ert-deftest fanyi-query-interrupted-response-settles-only-its-own-task ()
  (dolist (stage '(parse render))
    (dolist (timeout '(nil 15))
      (fanyi-test--with-query
        (let* ((fanyi-request-timeout timeout)
               (provider (car fanyi-providers))
               (interrupted (copy-fanyi-provider provider)))
          (if (eq stage 'parse)
              (setf (fanyi-provider-parse interrupted)
                    (lambda (&rest _) (signal 'quit '(interrupted-task))))
            (setf (fanyi-provider-render interrupted)
                  (lambda (result request)
                    (fanyi-test--render result request)
                    (signal 'quit '(interrupted-task)))))
          (setq fanyi-providers (list interrupted provider))
          (fanyi-dwim "word")
          (let* ((task (cadr (fanyi-test--query-state 'fanyi--tasks)))
                 (timer (fanyi--task-timer task))
                 (response (car fanyi-test--requests)))
            (should (equal (condition-case err (fanyi-test--respond response nil "PARTIAL")
                             (quit err))
                           '(quit interrupted-task)))
            (should (fanyi--task-done task))
            (should-not (fanyi--task-active-p task))
            (should-not (fanyi--task-timer task))
            (should-not (memq timer timer-list))
            (should-not (buffer-live-p (fanyi-test-request-buffer response)))
            (with-current-buffer fanyi-buffer-name
              (should-not (string-match-p "PARTIAL" (buffer-string))))
            ;; Neither a late response nor the cancelled timeout settles twice.
            (fanyi-test--respond response nil "late")
            (fanyi--timeout task)
            (fanyi-test--respond (cadr fanyi-test--requests) nil "healthy")
            (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 1))
            (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 0))
            (with-current-buffer fanyi-buffer-name
              (should (equal (buffer-string) "word: healthy\n")))))))))

(ert-deftest fanyi-query-interrupted-start-or-synchronous-response-leaves-no-pending-task ()
  (dolist (stage '(start synchronous browser))
    (fanyi-test--with-query
      (when (eq stage 'browser)
        (setf (fanyi-provider-api-type (car fanyi-providers)) 'browser
              (fanyi-provider-render (car fanyi-providers))
              (lambda (&rest _) (signal 'quit '(interrupted-start)))))
      (setf (fanyi-provider-parse (car fanyi-providers))
            (lambda (&rest _) (signal 'quit '(interrupted-start))))
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (&rest args)
                   (if (eq stage 'start)
                       (signal 'quit '(interrupted-start))
                     (apply #'fanyi-test--retrieve args)
                     (fanyi-test--respond (car fanyi-test--requests))))))
        (should (equal (condition-case err (fanyi-dwim "word") (quit err))
                       '(quit interrupted-start))))
      (let ((task (car (fanyi-test--query-state 'fanyi--tasks))))
        (should (fanyi--task-done task))
        (should-not (fanyi--task-timer task))
        (should-not (buffer-live-p (fanyi--task-buffer task)))
        (should-not (cl-some #'buffer-live-p fanyi-test--buffers))))))

(ert-deftest fanyi-query-retry-button-reuses-original-request ()
  (fanyi-test--with-query
    (fanyi-dwim "Word")
    (fanyi-test--respond (car fanyi-test--requests) '(:error (error http 500)))
    (with-current-buffer fanyi-buffer-name
      (goto-char (point-min))
      (search-forward "Retry dictionary")
      (let ((button (button-at (match-beginning 0))))
        (should button)
        (button-activate button)))
    (should (equal (fanyi-test--query-state 'fanyi-current-word) "Word"))
    (should (equal fanyi-history '("Word")))
    (should (= (length fanyi-test--requests) 2))
    (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 0))
    (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 0))
    (fanyi-test--respond (cadr fanyi-test--requests) nil "retried")
    (with-current-buffer fanyi-buffer-name
      (should (equal (buffer-string) "word: retried\n")))))

(ert-deftest fanyi-query-keeps-provider-order-for-all-response-orders ()
  (dolist (empty '(nil empty quit))
    (dolist (order '((0 1 2) (0 2 1) (1 0 2) (1 2 0) (2 0 1) (2 1 0)))
      (fanyi-test--with-query
        (setf (fanyi-provider-render (car fanyi-providers))
              (lambda (result request)
                (if (and empty (equal result "1"))
                    (when (eq empty 'quit)
                      (insert "PARTIAL\n")
                      (signal 'quit '("Interrupted middle dictionary")))
                  (fanyi-test--render result request))))
        (setq fanyi-providers (list (car fanyi-providers) (car fanyi-providers) (car fanyi-providers)))
        (fanyi-dwim "word")
        (cl-loop for number in order
                 for first = t then nil
                 do (condition-case err
                        (fanyi-test--respond (nth number fanyi-test--requests) nil (number-to-string number))
                      (quit (should (eq empty 'quit)) (should (= number 1))))
                 do (when first
                      (with-current-buffer fanyi-buffer-name
                        (should (= (point) (point-min))))))
        (with-current-buffer fanyi-buffer-name
          (should (equal (buffer-string) (if empty "word: 0\nword: 2\n" "word: 0\nword: 1\nword: 2\n")))
          (cl-loop for task in (reverse fanyi--tasks)
                   for number from 0
                   do (if (and empty (= number 1))
                          (progn (should-not (fanyi--task-start task)) (should-not (fanyi--task-end task)))
                        (should (equal (buffer-substring (fanyi--task-start task) (fanyi--task-end task))
                                       (format "word: %d\n" number))))))))))

(ert-deftest fanyi-query-orders-browser-and-synchronous-network-results ()
  (dolist (synchronous '(nil 1 2))
    (fanyi-test--with-query
      (setq fanyi-providers
            (list (fanyi-test--provider)
                  (fanyi-provider-create :url "https://browser.invalid/%s" :api-type 'browser
                                         :render (lambda (_result request)
                                                   (fanyi-test--render "browser" request)))
                  (fanyi-test--provider)))
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (&rest args)
                   (let ((buffer (apply #'fanyi-test--retrieve args)))
                     (when (equal (length fanyi-test--requests) synchronous)
                       (fanyi-test--respond (car (last fanyi-test--requests)) nil
                                            (if (= synchronous 1) "first" "last")))
                     buffer))))
        (fanyi-dwim "word"))
      (with-current-buffer fanyi-buffer-name
        (should (string-match-p "word: browser" (buffer-string)))
        (should (= (point) (point-min))))
      ;; The synchronous response may be repeated; it must still settle once.
      (fanyi-test--respond (cadr fanyi-test--requests) nil "last")
      (fanyi-test--respond (car fanyi-test--requests) nil "first")
      (with-current-buffer fanyi-buffer-name
        (should (= fanyi--tasks-completed 3))
        (should (equal (buffer-string) "word: first\nword: browser\nword: last\n"))))))

(ert-deftest fanyi-query-inserts-before-a-dictionary-without-moving-its-reader ()
  (dolist (narrow '(nil t))
    (dolist (direction '(cursor forward backward))
      (fanyi-test--with-query
        (setq fanyi-providers (list (car fanyi-providers) (car fanyi-providers)))
        (fanyi-dwim "word")
        (fanyi-test--respond (cadr fanyi-test--requests) nil "second")
        (with-current-buffer fanyi-buffer-name
          (save-window-excursion
            (set-window-buffer nil (current-buffer))
            (goto-char (point-min))
            (when narrow (narrow-to-region (point-min) (1- (point-max))))
            (pcase direction
              ('forward (set-mark (point)) (forward-char 4))
              ('backward (set-mark (+ (point) 4))))
            (let ((mark-active (not (eq direction 'cursor)))
                  (transient-mark-mode t) (deactivate-mark nil)
                  (before (buffer-string)))
              (set-window-start nil (point-min))
              (fanyi-test--respond (car fanyi-test--requests) nil "first")
              (let ((start (fanyi--task-start (car fanyi--tasks))))
                (should (= (window-start) start))
                (when narrow
                  (should (equal (buffer-string) before))
                  (should (= (point-min) start)))
                (if (eq direction 'cursor)
                    (should (= (point) start))
                  (should (use-region-p))
                  (should (equal (buffer-substring-no-properties (point) (mark)) "word")))))))))))

(ert-deftest fanyi-query-insertion-does-not-expand-a-selection-ending-at-the-boundary ()
  (dolist (direction '(forward backward))
    (fanyi-test--with-query
      (setq fanyi-providers (list (car fanyi-providers) (car fanyi-providers) (car fanyi-providers)))
      (fanyi-dwim "word")
      (fanyi-test--respond (car fanyi-test--requests) nil "first")
      (fanyi-test--respond (nth 2 fanyi-test--requests) nil "last")
      (with-current-buffer fanyi-buffer-name
        (goto-char (fanyi--task-start (car fanyi--tasks)))
        (set-mark (point-min))
        (when (eq direction 'backward) (exchange-point-and-mark))
        (let ((mark-active t) (transient-mark-mode t) (deactivate-mark nil))
          (fanyi-test--respond (cadr fanyi-test--requests) nil "middle")
          (should (use-region-p))
          (should (equal (buffer-substring-no-properties (point) (mark)) "word: first\n")))))))

(ert-deftest fanyi-query-insertion-preserves-a-preceding-narrowed-view ()
  (dolist (position '(between end empty))
    (fanyi-test--with-query
      (setq fanyi-providers (list (car fanyi-providers) (car fanyi-providers) (car fanyi-providers)))
      (fanyi-dwim "word")
      (fanyi-test--respond (car fanyi-test--requests) nil "first")
      (when (eq position 'between)
        (fanyi-test--respond (nth 2 fanyi-test--requests) nil "last"))
      (with-current-buffer fanyi-buffer-name
        (let ((end (if (eq position 'between)
                       (fanyi--task-start (car fanyi--tasks))
                     (point-max))))
          (narrow-to-region (pcase position
                              ('between (point-min))
                              ('empty end)
                              (_ (1+ (point-min))))
                            end))
        (goto-char (point-max))
        (let ((before (buffer-string)) (bounds (cons (point-min) (point-max))))
          (fanyi-test--respond (cadr fanyi-test--requests) nil "middle")
          (should (buffer-narrowed-p))
          (should (equal (buffer-string) before))
          (should (= (point-min) (car bounds)))
          (should (= (point-max) (cdr bounds)))
          (should (= (point) (point-max))))))))

(ert-deftest fanyi-query-background-window-keeps-its-selection-at-insertion-boundaries ()
  (dolist (selection '(upper-forward upper-backward lower-forward lower-backward cursor))
    (fanyi-test--with-query
      (setq fanyi-providers (list (car fanyi-providers) (car fanyi-providers) (car fanyi-providers)))
      (fanyi-dwim "word")
      (fanyi-test--respond (car fanyi-test--requests) nil "first")
      (fanyi-test--respond (nth 2 fanyi-test--requests) nil "last")
      (with-current-buffer fanyi-buffer-name
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer nil (current-buffer))
          (let ((result-window (selected-window))
                (other-window (split-window-right)))
            (with-temp-buffer
              (set-window-buffer other-window (current-buffer))
              (with-current-buffer fanyi-buffer-name
                (goto-char (fanyi--task-start (car fanyi--tasks)))
                (pcase selection
                  ((or 'upper-forward 'upper-backward) (set-mark (point-min)))
                  ((or 'lower-forward 'lower-backward) (set-mark (+ (point) 4)))
                  ('cursor (set-mark (point-min))))
                (when (memq selection '(upper-backward lower-forward))
                  (exchange-point-and-mark))
                (let* ((mark-active (not (eq selection 'cursor)))
                       (transient-mark-mode t) (deactivate-mark nil)
                       (selected (and mark-active (buffer-substring-no-properties (point) (mark)))))
                  (select-window other-window)
                  (fanyi-test--respond (cadr fanyi-test--requests) nil "middle")
                  (should (eq (selected-window) other-window))
                  (select-window result-window)
                  (if selected
                      (progn
                        (should (use-region-p))
                        (should (equal selected (buffer-substring-no-properties (point) (mark)))))
                    (should-not mark-active)
                    (should (= (point) (fanyi--task-start (car fanyi--tasks))))))))))))))

(ert-deftest fanyi-query-same-buffer-windows-use-their-own-selection-endpoints ()
  (dolist (mark-side '(nil before after))
    (fanyi-test--with-query
      (setq fanyi-providers (list (car fanyi-providers) (car fanyi-providers) (car fanyi-providers)))
      (fanyi-dwim "word")
      (fanyi-test--respond (car fanyi-test--requests) nil "first")
      (fanyi-test--respond (nth 2 fanyi-test--requests) nil "last")
      (with-current-buffer fanyi-buffer-name
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer nil (current-buffer))
          (let* ((reader (selected-window))
                 (other-window (split-window-right))
                 (boundary (marker-position (fanyi--task-start (car fanyi--tasks)))))
            (goto-char (point-min))
            (set-window-point other-window boundary)
            (set-mark (if (eq mark-side 'after) (+ boundary 4) (1+ (point-min))))
            (let* ((mark-active (and mark-side t))
                   (transient-mark-mode t) (deactivate-mark nil)
                   (selected (and mark-active (buffer-substring-no-properties (mark) (window-point other-window)))))
              (fanyi-test--respond (cadr fanyi-test--requests) nil "middle")
              (should (eq (selected-window) reader))
              (should (= (point) (point-min)))
              (should (= (window-point other-window)
                         (if (eq mark-side 'before) boundary
                           (fanyi--task-start (car fanyi--tasks)))))
              (when selected
                (should (equal selected (buffer-substring-no-properties (mark) (window-point other-window))))))))))))

(provide 'fanyi-tests)
;;; fanyi-tests.el ends here
