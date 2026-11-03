;;; fanyi-etymon-tests.el --- Etymonline browser entry tests -*- lexical-binding: t; -*-

(require 'fanyi-tests)
(require 'fanyi-etymon)

(defun fanyi-test--etymon-button ()
  "Return the Etymonline browser button in the current result buffer."
  (goto-char (point-min))
  (search-forward "Open in browser")
  (button-at (match-beginning 0)))

(ert-deftest fanyi-etymon-renders-without-network-or-parsing ()
  (fanyi-test--with-query
    (setq fanyi-providers '(fanyi-etymon-provider))
    (let (opened)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (&rest _) (ert-fail "Etymonline must not request HTTP")))
                ((symbol-function 'fanyi-parse-response)
                 (lambda (&rest _) (ert-fail "Etymonline must not parse a response")))
                ((symbol-function 'browse-url-default-browser)
                 (lambda (&rest _) (setq opened t))))
        (fanyi-dwim "sample"))
      (should-not opened))
    (should-not fanyi-test--requests)
    (should-not fanyi-test--buffers)
    (should (= (length (fanyi-test--query-state 'fanyi--tasks)) 1))
    (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 1))
    (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 0))
    (let ((task (car (fanyi-test--query-state 'fanyi--tasks))))
      (should (fanyi--task-done task))
      (should-not (fanyi--task-buffer task))
      (should-not (fanyi--task-timer task)))
    (with-current-buffer fanyi-buffer-name
      (should (string-match-p "# Etymonline" (buffer-string)))
      (should (fanyi-test--etymon-button))
      (should-not (string-match-p "Retry query" (buffer-string))))))

(ert-deftest fanyi-etymon-button-opens-current-entry-only-when-activated ()
  (fanyi-test--with-query
    (setq fanyi-providers '(fanyi-etymon-provider))
    (let (opened)
      (cl-letf (((symbol-function 'browse-url-default-browser)
                 (lambda (url &rest _) (push url opened))))
        (fanyi-dwim "Ice Cream?")
        (should-not opened)
        ;; The existing button retains its original URL despite global state.
        (setq fanyi-current-word "unrelated")
        (with-current-buffer fanyi-buffer-name
          (button-activate (fanyi-test--etymon-button)))
        (should (equal opened '("https://www.etymonline.com/word/ice%20cream%3F")))
        (fanyi-dwim "Second/Word")
        (should (= (length opened) 1))
        (with-current-buffer fanyi-buffer-name
          (let ((button (fanyi-test--etymon-button)))
            (should-not (next-button (button-end button) t))
            (button-activate button)))
        (should (equal opened '("https://www.etymonline.com/word/second%2Fword"
                                "https://www.etymonline.com/word/ice%20cream%3F"))))
      (should (= (length (fanyi-test--query-state 'fanyi--tasks)) 1))
      (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 1))
      (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 0))
      (should-not fanyi-test--requests))))

(ert-deftest fanyi-etymon-coexists-with-network-providers ()
  (fanyi-test--with-query
    (let ((network (car fanyi-providers)))
      (dolist (providers (list (list 'fanyi-etymon-provider network)
                               (list network 'fanyi-etymon-provider)))
        (setq fanyi-providers providers
              fanyi-test--requests nil)
        (fanyi-dwim "sample")
        (should (= (length fanyi-test--requests) 1))
        (should (= (length (fanyi-test--query-state 'fanyi--tasks)) 2))
        (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 1))
        (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 0))
        (with-current-buffer fanyi-buffer-name
          (should (fanyi-test--etymon-button)))
        (fanyi-test--respond (car fanyi-test--requests) nil "network definition")
        (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 2))
        (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 0))
        (with-current-buffer fanyi-buffer-name
          (should (string-match-p "sample: network definition" (buffer-string)))
          (should (fanyi-test--etymon-button)))))))

(ert-deftest fanyi-etymon-does-not-require-html-or-json-parsers ()
  (fanyi-test--with-query
    (setq fanyi-providers '(fanyi-etymon-provider))
    (cl-letf (((symbol-function 'libxml-parse-html-region) nil)
              ((symbol-function 'json-parse-buffer) nil))
      (fanyi-dwim "sample"))
    (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 1))
    (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 0))
    (with-current-buffer fanyi-buffer-name
      (should (fanyi-test--etymon-button)))))

(provide 'fanyi-etymon-tests)
;;; fanyi-etymon-tests.el ends here
