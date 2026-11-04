;;; fanyi-response-tests.el --- Invalid and partial response tests -*- lexical-binding: t; -*-

(require 'fanyi-test-support)
(require 'fanyi-haici)
(require 'fanyi-longman)
(require 'fanyi-youdao-thesaurus)

(ert-deftest fanyi-youdao-distinguishes-empty-results-from-invalid-data ()
  (should-not
   (catch 'not-found
     (fanyi-parse-response (fanyi-make-request fanyi-youdao-thesaurus-provider "sample")
                           (fanyi-test--read-fixture "youdao-empty.json"))
     'unexpected-success))
  (dolist (body '("null" "[]" "{}"
                  "{\"result\":{\"code\":500}}"
                  "{\"result\":{\"code\":200},\"data\":{}}"
                  "{\"result\":{\"code\":200},\"data\":{\"entries\":null}}"
                  "{\"result\":{\"code\":200},\"data\":{\"entries\":[{}]}}"
                  "{\"result\":{\"code\":200},\"data\":{\"entries\":[{\"entry\":\" \"}]}}"))
    (should-error
     (fanyi-parse-response (fanyi-make-request fanyi-youdao-thesaurus-provider "sample") (json-parse-string body))
     :type 'fanyi-parse-error)))

(ert-deftest fanyi-youdao-keeps-entry-without-usable-explanation ()
  (dolist (explain '(nil :null "" " " 123))
    (let* ((data (fanyi-test--read-fixture "youdao.json"))
           (entry (aref (gethash "entries" (gethash "data" data)) 0)))
      (puthash "explain" explain entry)
      (fanyi-test--with-rendered-response fanyi-youdao-thesaurus-provider data
        (should (equal (fanyi-test--button-data #'fanyi-dwim) '("sample" "specimen")))
        (should (equal (cdar result) "NO EXPLANATION"))))))

(ert-deftest fanyi-invalid-html-is-failed-not-completed ()
  (fanyi-test--with-query
    (dolist (provider '(fanyi-haici-provider fanyi-longman-provider))
      (setq fanyi-providers (list provider))
      (fanyi-dwim "sample")
      (let ((body (with-temp-buffer
                    (insert-file-contents (expand-file-name "error-page.html" fanyi-test--fixture-directory))
                    (buffer-string))))
        (fanyi-test--deliver (car (last fanyi-test--requests)) nil body))
      (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 1))
      (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 0))
      (with-current-buffer fanyi-buffer-name
        (should (string-match-p "Could not parse dictionary response" (buffer-string)))))))

(ert-deftest fanyi-empty-api-result-shows-no-results ()
  (fanyi-test--with-query
    (setq fanyi-providers '(fanyi-youdao-thesaurus-provider))
    (fanyi-dwim "sample")
    (fanyi-test--deliver (car fanyi-test--requests) nil
                         "{\"result\":{\"code\":200},\"data\":{\"entries\":[]}}")
    (should (= (fanyi-test--query-state 'fanyi--tasks-failed) 0))
    (should (= (fanyi-test--query-state 'fanyi--tasks-completed) 1))
    (with-current-buffer fanyi-buffer-name
      (should (string-match-p "No results found" (buffer-string)))
      (should (fanyi--task-not-found (car fanyi--tasks)))
      (should-not (fanyi--task-timer (car fanyi--tasks)))
      (should-not (fanyi-test--button-data #'fanyi--retry-task))
      (should-not (string-match-p "Could not parse" (buffer-string))))))

(ert-deftest fanyi-retry-can-finish-with-an-empty-result ()
  (fanyi-test--with-query
    (setf (fanyi-provider-parse (car fanyi-providers))
          (lambda (&rest _) (throw 'not-found nil)))
    (fanyi-dwim "sample")
    (fanyi-test--respond (car fanyi-test--requests) '(:error (error http 500)))
    (with-current-buffer fanyi-buffer-name (fanyi--retry-task (car fanyi--tasks)))
    (fanyi-test--respond (cadr fanyi-test--requests))
    (fanyi-test--respond (cadr fanyi-test--requests))
    (with-current-buffer fanyi-buffer-name
      (should (= fanyi--tasks-failed 0))
      (should (= fanyi--tasks-completed 1))
      (should-not (fanyi-test--button-data #'fanyi--retry-task))
      (should-not (fanyi--task-failure (car fanyi--tasks))))))

(provide 'fanyi-response-tests)
;;; fanyi-response-tests.el ends here
