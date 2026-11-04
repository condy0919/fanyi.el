;;; fanyi-history-tests.el --- Cached lookup navigation -*- lexical-binding: t; -*-

(require 'fanyi-test-support)

(ert-deftest fanyi-history-reuses-results-and-new-queries-discard-forward-pages ()
  (fanyi-test--with-query
    (fanyi-dwim "First")
    (fanyi-test--respond (car fanyi-test--requests) nil "first result")
    (fanyi-dwim "Second")
    (fanyi-test--respond (cadr fanyi-test--requests) nil "second result")
    (with-current-buffer fanyi-buffer-name
      (let ((fanyi-providers nil))
        (fanyi-back)
        (should (equal fanyi-current-word "First"))
        (should (equal (buffer-string) "first: first result\n"))
        (fanyi-forward)
        (should (equal fanyi-current-word "Second")))
      (should (= (length fanyi-test--requests) 2))
      (fanyi-back)
      (fanyi-dwim "Third")
      (should-not fanyi--forward-history)
      (should (= (length fanyi--back-history) 1))
      (should-error (fanyi-forward) :type 'user-error)
      (should-error (fanyi-dwim " ") :type 'user-error)
      (should (= (length fanyi--back-history) 1)))))

(ert-deftest fanyi-history-bounds-total-pages-and-refreshes-without-pushing ()
  (fanyi-test--with-query
    (let ((fanyi-navigation-history-length 2))
      (dolist (word '("one" "two" "three" "four"))
        (fanyi-dwim word)
        (fanyi-test--respond (car (last fanyi-test--requests))))
      (with-current-buffer fanyi-buffer-name
        (dotimes (_ 3)
          (fanyi-back)
          (fanyi-back)
          (should (= (length fanyi--forward-history) 2))
          (should-error (fanyi-back) :type 'user-error)
          (fanyi-forward)
          (fanyi-forward)
          (should (= (length fanyi--back-history) 2)))
        (should (= (length fanyi-test--requests) 4))
        (fanyi--revert-buffer nil t)
        (should (= (length fanyi-test--requests) 5))
        (should (= (length fanyi--back-history) 2))
        (fanyi-back)
        (should (equal fanyi-current-word "three"))
        (let ((fanyi-navigation-history-length 0))
          (fanyi-dwim "disabled")
          (should-not fanyi--back-history)
          (should-not fanyi--forward-history))))))

(ert-deftest fanyi-history-renamed-buffer-keeps-its-own-stacks-and-snapshots ()
  (fanyi-test--with-query
    (fanyi-dwim "one")
    (fanyi-test--respond (car fanyi-test--requests))
    (fanyi-dwim "two")
    (let ((saved (get-buffer fanyi-buffer-name))
          (old (cadr fanyi-test--requests)))
      (push saved fanyi-test--buffers)
      (with-current-buffer saved
        (rename-buffer (generate-new-buffer-name " *fanyi-saved-history*")))
      ;; Cancels the renamed buffer's second request without losing its state.
      (fanyi-dwim "three")
      (fanyi-test--respond (nth 2 fanyi-test--requests))
      (setf (fanyi-provider-prepare (car fanyi-providers))
            (lambda (_) (ert-fail "History must not prepare another request")))
      (with-current-buffer saved
        (fanyi-back)
        (should (equal fanyi-current-word "one"))
        (fanyi-forward)
        (should (equal fanyi-current-word "two"))
        (should (= (length fanyi-test--requests) 4))
        (fanyi-test--respond old nil "stale")
        (should (equal (buffer-string) ""))
        (fanyi-test--respond (nth 3 fanyi-test--requests) nil "resumed")
        (should (equal (buffer-string) "two: resumed\n")))
      (with-current-buffer fanyi-buffer-name
        (should (equal fanyi-current-word "three"))
        (should-not fanyi--back-history)))))

(provide 'fanyi-history-tests)
;;; fanyi-history-tests.el ends here
