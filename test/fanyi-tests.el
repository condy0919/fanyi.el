;;; fanyi-tests.el --- Query lifecycle tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
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

(defun fanyi-test--respond (request &optional status definition)
  "Deliver REQUEST with STATUS and DEFINITION.
Recreate a cancelled response buffer to simulate a late callback,
including one delivered by a redirected request."
  (let ((buffer (fanyi-test-request-buffer request)))
    (unless (buffer-live-p buffer)
      (setq buffer (fanyi-test--response-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "\n" (json-encode `((definition . ,(or definition "definition"))))))
    (with-current-buffer buffer
      (setq-local url-http-end-of-headers 1)
      (funcall (fanyi-test-request-callback request) status))))

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
          (fanyi-test--requests nil)
          (fanyi-test--buffers nil))
     (unwind-protect
         (cl-letf (((symbol-function 'url-retrieve) #'fanyi-test--retrieve)
                   ((symbol-function 'display-buffer) #'ignore))
           ,@body)
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
      (fanyi-test--respond old-request '(:peer nil) "obsolete")
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
      (fanyi-test--respond old-request '(:error (error http 500)))
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
           (buffer (fanyi-test-request-buffer request))
           (process (make-pipe-process :name "fanyi-test-pending"
                                       :buffer buffer :noquery t)))
      (should (process-live-p process))
      (fanyi-dwim "new")
      (should-not (process-live-p process))
      (should-not (buffer-live-p buffer))
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

(provide 'fanyi-tests)
;;; fanyi-tests.el ends here
