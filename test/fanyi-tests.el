;;; fanyi-tests.el --- Query lifecycle tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'fanyi)

(defclass fanyi-test-service (fanyi-base-service)
  ((definition :initform nil)))

(cl-defmethod fanyi-parse-from ((this fanyi-test-service) response)
  (oset this definition (gethash "definition" response)))

(cl-defmethod fanyi-render ((this fanyi-test-service))
  (with-current-buffer (get-buffer-create fanyi-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (oref this word) ": " (oref this definition) "\n"))))

(cl-defstruct fanyi-test-request callback buffer)

(defvar fanyi-test--requests nil)
(defvar fanyi-test--buffers nil)

(defun fanyi-test--response-buffer ()
  "Create a response buffer and record it for test cleanup."
  (let ((buffer (generate-new-buffer " *fanyi-test-response*")))
    (push buffer fanyi-test--buffers)
    buffer))

(defun fanyi-test--retrieve (_url callback &rest _args)
  "Record CALLBACK without accessing the network."
  (let* ((buffer (fanyi-test--response-buffer))
         (request (make-fanyi-test-request :callback callback :buffer buffer)))
    (setq fanyi-test--requests (append fanyi-test--requests (list request)))
    buffer))

(defun fanyi-test--deliver (request status body)
  "Deliver REQUEST with STATUS and raw response BODY, returning its buffer.
Recreate a cancelled response buffer to simulate a late callback,
including one delivered by a redirected request."
  (let ((buffer (fanyi-test-request-buffer request)))
    (unless (buffer-live-p buffer)
      (setq buffer (fanyi-test--response-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "\n" body))
    (with-current-buffer buffer
      (setq-local url-http-end-of-headers 1)
      (funcall (fanyi-test-request-callback request) status))
    buffer))

(defun fanyi-test--respond (request &optional status definition)
  "Deliver REQUEST with STATUS and DEFINITION, returning its buffer."
  (fanyi-test--deliver
   request status
   (json-encode `((definition . ,(or definition "definition"))))))

(defmacro fanyi-test--with-query (&rest body)
  "Evaluate BODY with isolated query state and mocked HTTP requests."
  (declare (indent 0) (debug t))
  `(let* ((fanyi-buffer-name (generate-new-buffer-name " *fanyi-test*"))
          (fanyi-providers (list (fanyi-test-service
                                 :word "dummy"
                                 :url "https://example.invalid/%s"
                                 :api-type 'json)))
          (fanyi-auto-select nil)
          (fanyi-current-word nil)
          (fanyi--query-id 0)
          (fanyi--tasks nil)
          (fanyi--tasks-completed 0)
          (fanyi--tasks-failed 0)
          (fanyi-verbose t)
          (fanyi-log nil)
          (fanyi-request-timeout 15)
          (fanyi-test--requests nil)
          (fanyi-test--buffers nil)
          (fanyi-test--initial-timers (copy-sequence timer-list)))
     (unwind-protect
         (cl-letf (((symbol-function 'url-retrieve) #'fanyi-test--retrieve)
                   ((symbol-function 'display-buffer) #'ignore))
           ,@body)
       (dolist (timer (copy-sequence timer-list))
         (unless (memq timer fanyi-test--initial-timers)
           (cancel-timer timer)))
       (dolist (buffer (cons (get-buffer fanyi-buffer-name)
                             fanyi-test--buffers))
         (when (buffer-live-p buffer)
           (let ((process (get-buffer-process buffer)))
             (when process
               (set-process-sentinel process #'ignore)
               (delete-process process)))
           (kill-buffer buffer))))))

(ert-deftest fanyi-query-ignores-stale-success ()
  (fanyi-test--with-query
    (fanyi-dwim "old")
    (let ((old-request (car fanyi-test--requests)))
      (fanyi-dwim "new")
      (should-not (buffer-live-p
                   (fanyi-test--respond old-request '(:peer nil) "obsolete")))
      (with-current-buffer fanyi-buffer-name
        (should (equal (buffer-string) "")))
      (should (= fanyi--tasks-completed 0))
      (should (= fanyi--tasks-failed 0))
      (should (= (length fanyi--tasks) 1))
      (fanyi-test--respond (cadr fanyi-test--requests) '(:peer nil) "current")
      (with-current-buffer fanyi-buffer-name
        (should (equal (buffer-string) "new: current\n")))
      (should (= fanyi--tasks-completed 1))
      (should (= fanyi--tasks-failed 0)))))

(ert-deftest fanyi-query-ignores-stale-error ()
  (fanyi-test--with-query
    (fanyi-dwim "old")
    (let ((old-request (car fanyi-test--requests)))
      (fanyi-dwim "new")
      (should-not (buffer-live-p
                   (fanyi-test--respond old-request '(:error (error http 500)))))
      (should (= fanyi--tasks-completed 0))
      (should (= fanyi--tasks-failed 0))
      (with-current-buffer fanyi-buffer-name
        (should (equal (buffer-string) "")))
      (fanyi-test--respond (cadr fanyi-test--requests) '(:peer nil))
      (should (= fanyi--tasks-completed 1)))))

(ert-deftest fanyi-query-repeated-word-still-invalidates-old-request ()
  (fanyi-test--with-query
    (fanyi-dwim "word")
    (let ((old-request (car fanyi-test--requests)))
      (fanyi-dwim "word")
      (fanyi-test--respond (cadr fanyi-test--requests) '(:peer nil) "current")
      (fanyi-test--respond old-request '(:peer nil) "obsolete")
      (with-current-buffer fanyi-buffer-name
        (should (equal (buffer-string) "word: current\n")))
      (should (= fanyi--tasks-completed 1))
      (should (= fanyi--tasks-failed 0)))))

(ert-deftest fanyi-query-cancels-previous-request ()
  (fanyi-test--with-query
    (fanyi-dwim "old")
    (let* ((request (car fanyi-test--requests))
           (timer (fanyi--task-timer (car fanyi--tasks)))
           (buffer (fanyi-test-request-buffer request))
           (process (make-pipe-process :name "fanyi-test-pending"
                                       :buffer buffer :noquery t)))
      (should (process-live-p process))
      (fanyi-dwim "new")
      (should-not (process-live-p process))
      (should-not (buffer-live-p buffer))
      (should-not (memq timer timer-list))
      (should (= (length fanyi--tasks) 1))
      (should (= fanyi--tasks-completed 0))
      (should (= fanyi--tasks-failed 0)))))

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
      (should (= (length fanyi--tasks) 1))
      (should (= fanyi--tasks-completed 0))
      (should (= fanyi--tasks-failed 0)))))

(ert-deftest fanyi-query-accepts-success-status-and-cleans-response ()
  (fanyi-test--with-query
    (dolist (status '(nil (:redirect "https://example.invalid/canonical")))
      (ert-info ((format "Success status: %S" status))
        (fanyi-dwim "word")
        (let* ((task (car fanyi--tasks))
               (timer (fanyi--task-timer task))
               (response (fanyi-test--respond (car (last fanyi-test--requests))
                                              status)))
          (should-not (buffer-live-p response))
          (should-not (memq timer timer-list))
          (should (fanyi--task-done task))
          (should (= fanyi--tasks-completed 1))
          (should (= fanyi--tasks-failed 0))
          (with-current-buffer fanyi-buffer-name
            (should (equal (buffer-string) "word: definition\n"))))))))

(ert-deftest fanyi-query-isolates-failures-and-cleans-response ()
  (fanyi-test--with-query
    (setq fanyi-providers (list (car fanyi-providers)
                                (clone (car fanyi-providers))))
    (dolist (failure '(http json parse render not-found))
      (ert-info ((format "Failure stage: %S" failure))
        (setq fanyi-test--requests nil)
        (fanyi-dwim "word")
        (let ((parse (symbol-function 'fanyi-parse-from))
              (render (symbol-function 'fanyi-render)))
          (cl-letf (((symbol-function 'fanyi-parse-from)
                     (lambda (&rest args)
                       (pcase failure
                         ('parse (error "Malformed dictionary response"))
                         ('not-found (throw 'not-found nil))
                         (_ (apply parse args)))))
                    ((symbol-function 'fanyi-render)
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
        (should (= fanyi--tasks-failed 1))
        (should (= fanyi--tasks-completed 0))
        (with-current-buffer fanyi-buffer-name
          (should (string-match-p "Retry query" (buffer-string)))
          (should-not (string-match-p "PARTIAL RESULT" (buffer-string))))
        ;; A broken provider must not prevent another provider from finishing.
        (fanyi-test--respond (cadr fanyi-test--requests) nil "healthy")
        (should (= fanyi--tasks-failed 1))
        (should (= fanyi--tasks-completed 1))
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
      (should (= fanyi--tasks-completed 1))
      (should (= fanyi--tasks-failed 0))
      (with-current-buffer fanyi-buffer-name
        (should (equal (buffer-string) "word: first\n"))))))

(ert-deftest fanyi-query-timeout-settles-once-and-cancels-request ()
  (fanyi-test--with-query
    (fanyi-dwim "word")
    (let* ((task (car fanyi--tasks))
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
      (fanyi--timeout task)
      (should-not (buffer-live-p (fanyi-test--respond request nil "late")))
      (should (= fanyi--tasks-failed 1))
      (should (= fanyi--tasks-completed 0))
      (with-current-buffer fanyi-buffer-name
        (should-not (string-match-p "word: late" (buffer-string)))))))

(ert-deftest fanyi-query-start-error-does-not-block-other-provider ()
  (fanyi-test--with-query
    (setq fanyi-providers (list (car fanyi-providers)
                                (clone (car fanyi-providers))))
    (let ((attempts 0))
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (&rest args)
                   (if (= (cl-incf attempts) 1)
                       (error "Cannot start request")
                     (apply #'fanyi-test--retrieve args)))))
        (fanyi-dwim "word"))
      (should (= attempts 2)))
    (should (= fanyi--tasks-failed 1))
    (fanyi-test--respond (car fanyi-test--requests) nil)
    (should (= fanyi--tasks-completed 1))
    (should (= fanyi--tasks-failed 1))
    (dolist (task fanyi--tasks)
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
    (should (= fanyi--tasks-completed 1))
    (should (= fanyi--tasks-failed 0))
    (should (fanyi--task-done (car fanyi--tasks)))
    (should-not (fanyi--task-timer (car fanyi--tasks)))
    (should-not (buffer-live-p
                 (fanyi-test-request-buffer (car fanyi-test--requests))))))

(ert-deftest fanyi-query-retry-button-repeats-current-query ()
  (fanyi-test--with-query
    (fanyi-dwim "Word")
    (fanyi-test--respond (car fanyi-test--requests) '(:error (error http 500)))
    (with-current-buffer fanyi-buffer-name
      (goto-char (point-min))
      (search-forward "Retry query")
      (let ((button (button-at (match-beginning 0))))
        (should button)
        (button-activate button)))
    (should (equal fanyi-current-word "Word"))
    (should (= (length fanyi-test--requests) 2))
    (should (= fanyi--tasks-completed 0))
    (should (= fanyi--tasks-failed 0))
    (fanyi-test--respond (cadr fanyi-test--requests) nil "retried")
    (with-current-buffer fanyi-buffer-name
      (should (equal (buffer-string) "word: retried\n")))))

(provide 'fanyi-tests)
;;; fanyi-tests.el ends here
