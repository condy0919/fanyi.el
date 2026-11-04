;;; fanyi-names-tests.el --- Dictionary display names -*- lexical-binding: t; -*-

(require 'fanyi-test-support)
(require 'fanyi-longman)

(ert-deftest fanyi-dictionary-name-is-snapshotted-and-used-for-errors ()
  (fanyi-test--with-query
    (setf (fanyi-provider-name (car fanyi-providers)) (copy-sequence "Test dictionary"))
    (fanyi-dwim "sample")
    (aset (fanyi-provider-name (car fanyi-providers)) 0 ?X)
    (fanyi-test--respond (car fanyi-test--requests) '(:error (error http 500)))
    (with-current-buffer fanyi-buffer-name
      (should (string-prefix-p "# Test dictionary\n" (buffer-string)))
      (should (equal (fanyi-request-display-name (fanyi--task-request (car fanyi--tasks)))
                     "Test dictionary")))))

(ert-deftest fanyi-dictionary-name-agrees-between-success-and-failure ()
  (fanyi-test--with-query
    (setq fanyi-providers '(fanyi-longman-provider))
    (fanyi-dwim "sample")
    (fanyi-test--respond (car fanyi-test--requests) '(:error (error http 500)))
    (with-current-buffer fanyi-buffer-name
      (should (string-prefix-p "# Longman\n" (buffer-string)))
      (fanyi--retry-task (car fanyi--tasks)))
    (fanyi-test--deliver
     (cadr fanyi-test--requests) nil
     (with-temp-buffer
       (insert-file-contents (expand-file-name "longman.html" fanyi-test--fixture-directory))
       (buffer-string)))
    (with-current-buffer fanyi-buffer-name
      (should (string-prefix-p "# Longman\n" (buffer-string)))))
  (should (equal (fanyi-request-display-name
                  (fanyi-make-request (fanyi-test--provider) "sample"))
                 "example.invalid")))

(provide 'fanyi-names-tests)
;;; fanyi-names-tests.el ends here
