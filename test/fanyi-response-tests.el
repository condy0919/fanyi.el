;;; fanyi-response-tests.el --- Invalid and partial response tests -*- lexical-binding: t; -*-

(require 'fanyi-tests)
(require 'fanyi-provider-tests)

(ert-deftest fanyi-youdao-distinguishes-empty-results-from-invalid-data ()
  (should-not
   (catch 'not-found
     (fanyi-parse-from (clone fanyi-youdao-thesaurus-provider)
                       (fanyi-test--read-fixture "youdao-empty.json"))
     'unexpected-success))
  (dolist (body '("null" "[]" "{}"
                  "{\"result\":{\"code\":500}}"
                  "{\"result\":{\"code\":200},\"data\":{}}"
                  "{\"result\":{\"code\":200},\"data\":{\"entries\":null}}"
                  "{\"result\":{\"code\":200},\"data\":{\"entries\":[{}]}}"
                  "{\"result\":{\"code\":200},\"data\":{\"entries\":[{\"entry\":\" \"}]}}"))
    (should-error
     (fanyi-parse-from (clone fanyi-youdao-thesaurus-provider) (json-parse-string body))
     :type 'fanyi-parse-error)))

(ert-deftest fanyi-youdao-keeps-entry-without-usable-explanation ()
  (dolist (explain '(nil :null "" " " 123))
    (let* ((data (fanyi-test--read-fixture "youdao.json"))
           (entry (aref (gethash "entries" (gethash "data" data)) 0)))
      (puthash "explain" explain entry)
      (fanyi-test--with-rendered-response fanyi-youdao-thesaurus-provider data
        (should (string-match-p "- sample" (buffer-string)))
        (should (equal (oref (car (oref instance entries)) explain) "NO EXPLANATION"))))))

(ert-deftest fanyi-invalid-html-is-failed-not-completed ()
  (fanyi-test--with-query
    (dolist (provider '(fanyi-haici-provider fanyi-longman-provider))
      (setq fanyi-providers (list provider))
      (fanyi-dwim "sample")
      (let ((body (with-temp-buffer
                    (insert-file-contents (expand-file-name "error-page.html" fanyi-test--fixture-directory))
                    (buffer-string))))
        (fanyi-test--deliver (car (last fanyi-test--requests)) nil body))
      (should (= fanyi--tasks-failed 1))
      (should (= fanyi--tasks-completed 0))
      (with-current-buffer fanyi-buffer-name
        (should (string-match-p "Could not parse dictionary response" (buffer-string)))))))

(ert-deftest fanyi-empty-api-result-shows-no-results ()
  (fanyi-test--with-query
    (setq fanyi-providers '(fanyi-youdao-thesaurus-provider))
    (fanyi-dwim "sample")
    (fanyi-test--deliver (car fanyi-test--requests) nil
                         "{\"result\":{\"code\":200},\"data\":{\"entries\":[]}}")
    (should (= fanyi--tasks-failed 1))
    (should (= fanyi--tasks-completed 0))
    (with-current-buffer fanyi-buffer-name
      (should (string-match-p "No results found" (buffer-string)))
      (should-not (string-match-p "Could not parse" (buffer-string))))))

(provide 'fanyi-response-tests)
;;; fanyi-response-tests.el ends here
