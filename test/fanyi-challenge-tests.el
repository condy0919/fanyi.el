;;; fanyi-challenge-tests.el --- Browser challenge handling -*- lexical-binding: t; -*-

(require 'fanyi-tests)
(require 'browse-url)

(defun fanyi-test--http-respond (request status headers &optional body)
  "Deliver REQUEST with STATUS, raw HEADERS and BODY.
Recreate cancelled response buffers to model queued or redirected callbacks."
  (let ((buffer (fanyi-test-request-buffer request)))
    (unless (buffer-live-p buffer)
      (setq buffer (fanyi-test--response-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (if (plist-member status :error)
                  "HTTP/1.1 403 Forbidden\r\n"
                "HTTP/1.1 200 OK\r\n")
              headers "\r\n")
      ;; url.el's marker identifies the final newline before the body.
      (setq-local url-http-end-of-headers (copy-marker (1- (point))))
      (insert (or body "<html><title>Just a moment...</title></html>"))
      (funcall (fanyi-test-request-callback request) status))
    buffer))

(ert-deftest fanyi-challenge-recognizes-header-before-http-error-or-parsing ()
  (fanyi-test--with-query
    (dolist (status '(nil (:error (error http 403))))
      (dolist (headers '("cf-mitigated: challenge\r\n"
                          "Content-Type: text/html\r\nCf-Mitigated:\tChAlLeNgE \t\r\n"
                          "CF-MITIGATED: challenge\n"))
        (fanyi-dwim "word")
        (let ((task (car fanyi--tasks))
              (timer (fanyi--task-timer (car fanyi--tasks)))
              parsed rendered)
          (cl-letf (((symbol-function 'fanyi-parse-from)
                     (lambda (&rest _) (setq parsed t)))
                    ((symbol-function 'fanyi-render)
                     (lambda (&rest _) (setq rendered t))))
            (should-not (buffer-live-p
                         (fanyi-test--http-respond
                          (car (last fanyi-test--requests)) status headers))))
          (should-not parsed)
          (should-not rendered)
          (should (fanyi--task-done task))
          (should-not (fanyi--task-timer task))
          (should-not (memq timer timer-list))
          (should (= fanyi--tasks-failed 1))
          (should (= fanyi--tasks-completed 0))
          (with-current-buffer fanyi-buffer-name
            (should (string-match-p "Cloudflare" (buffer-string)))
            (should (string-match-p "[Bb]rowser verification" (buffer-string)))
            (should (string-match-p "Open in browser" (buffer-string)))
            (should-not (string-match-p "Retry query" (buffer-string)))))))))

(ert-deftest fanyi-challenge-button-opens-original-encoded-entry-on-demand ()
  (fanyi-test--with-query
    (oset (car fanyi-providers) url "https://www.etymonline.com/word/%s")
    (let (opened)
      (cl-letf (((symbol-function 'browse-url-default-browser)
                 (lambda (url &rest _) (push url opened))))
        (fanyi-dwim "Ice Cream?")
        (fanyi-test--http-respond (car fanyi-test--requests)
                                  '(:error (error http 403))
                                  "cf-mitigated: challenge\r\n")
        (should-not opened)
        ;; The button must retain the request URL, not read mutable query state.
        (setq fanyi-current-word "unrelated")
        (with-current-buffer fanyi-buffer-name
          (goto-char (point-min))
          (search-forward "Open in browser")
          (let ((button (button-at (match-beginning 0))))
            (should button)
            (button-activate button)))
        (should (equal opened '("https://www.etymonline.com/word/ice%20cream%3F")))
        (should (= (length fanyi-test--requests) 1))
        (should (= fanyi--tasks-failed 1))))))

(ert-deftest fanyi-challenge-does-not-misclassify-ordinary-forbidden-responses ()
  (fanyi-test--with-query
    (dolist (headers '("" "Server: cloudflare\r\n"
                        "cf-mitigated: other\r\n"
                        "cf-mitigated: challenge-extra\r\n"
                        "X-Note: cf-mitigated: challenge\r\n"))
      (fanyi-dwim "word")
      (fanyi-test--http-respond
       (car (last fanyi-test--requests)) '(:error (error http 403)) headers
       "<html>\ncf-mitigated: challenge\n<title>Just a moment...</title></html>")
      (should (= fanyi--tasks-failed 1))
      (with-current-buffer fanyi-buffer-name
        (should (string-match-p "Request failed" (buffer-string)))
        (should (string-match-p "Retry query" (buffer-string)))
        (should-not (string-match-p "Open in browser" (buffer-string)))))))

(ert-deftest fanyi-challenge-is-settled-once-and-other-providers-continue ()
  (fanyi-test--with-query
    (setq fanyi-providers (list (car fanyi-providers) (clone (car fanyi-providers))))
    (fanyi-dwim "word")
    (let ((request (car fanyi-test--requests)))
      (fanyi-test--http-respond request '(:error (error http 403))
                                "cf-mitigated: challenge\r\n")
      (let ((before (with-current-buffer fanyi-buffer-name (buffer-string))))
        (should-not (buffer-live-p
                     (fanyi-test--http-respond request '(:error (error http 403))
                                               "cf-mitigated: challenge\r\n")))
        (with-current-buffer fanyi-buffer-name
          (should (equal (buffer-string) before))))
      (fanyi-test--respond (cadr fanyi-test--requests) nil "healthy")
      (should (= fanyi--tasks-failed 1))
      (should (= fanyi--tasks-completed 1))
      (should (= (length fanyi-test--requests) 2))
      (with-current-buffer fanyi-buffer-name
        (should (string-match-p "word: healthy" (buffer-string)))))))

(ert-deftest fanyi-challenge-from-old-query-is-ignored ()
  (fanyi-test--with-query
    (fanyi-dwim "old")
    (let ((old-request (car fanyi-test--requests)))
      (fanyi-dwim "new")
      (should-not (buffer-live-p
                   (fanyi-test--http-respond old-request '(:error (error http 403))
                                             "cf-mitigated: challenge\r\n")))
      (should (= fanyi--tasks-failed 0))
      (should (= fanyi--tasks-completed 0))
      (with-current-buffer fanyi-buffer-name
        (should (equal (buffer-string) "")))
      (fanyi-test--respond (cadr fanyi-test--requests) nil "healthy")
      (with-current-buffer fanyi-buffer-name
        (should (equal (buffer-string) "new: healthy\n"))))))

(ert-deftest fanyi-challenge-after-timeout-does-not-change-the-failure ()
  (fanyi-test--with-query
    (fanyi-dwim "word")
    (let ((task (car fanyi--tasks))
          (request (car fanyi-test--requests)))
      (fanyi--timeout task)
      (let ((before (with-current-buffer fanyi-buffer-name (buffer-string))))
        (should-not (buffer-live-p
                     (fanyi-test--http-respond request '(:error (error http 403))
                                               "cf-mitigated: challenge\r\n")))
        (with-current-buffer fanyi-buffer-name
          (should (equal (buffer-string) before))))
      (should (= fanyi--tasks-failed 1))
      (should (= fanyi--tasks-completed 0)))))

(provide 'fanyi-challenge-tests)
;;; fanyi-challenge-tests.el ends here
