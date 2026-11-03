;;; fanyi-response-tests.el --- Invalid and partial response tests -*- lexical-binding: t; -*-

(require 'fanyi-tests)
(require 'fanyi-provider-tests)

(ert-deftest fanyi-etymon-rejects-missing-or-empty-definitions ()
  (should-error
   (fanyi-parse-from (clone fanyi-etymon-provider)
                     (fanyi-test--read-fixture "error-page.html"))
   :type 'fanyi-parse-error)
  (dolist (missing '(title body text))
    (let* ((dom (fanyi-test--read-fixture "etymon.html"))
           (title (car (dom-by-class dom "word__name--TTbAA")))
           (body (car (dom-by-class dom "word__defination--2q7ZH"))))
      (pcase missing
        ('title (dom-remove-node dom title))
        ('body (dom-remove-node dom body))
        ('text (setcdr (cdr body) '((p nil) (br nil) (span nil "  ")))))
      (should-error (fanyi-parse-from (clone fanyi-etymon-provider) dom)
                    :type 'fanyi-parse-error))))

(ert-deftest fanyi-etymon-keeps-text-through-optional-markup ()
  (let* ((dom (fanyi-test--read-fixture "etymon.html"))
         (body (car (dom-by-class dom "word__defination--2q7ZH"))))
    (dom-append-child body '(span nil))
    (dom-append-child body '(br nil))
    (dom-append-child body '(p nil "An " (em nil "older " (strong nil "word")) "."))
    (fanyi-test--with-rendered-response fanyi-etymon-provider dom
      (should (string-match-p "An older word\\." (buffer-string)))
      (should (string-match-p "From /exemplum/" (buffer-string)))
      (should (equal (fanyi-test--button-data #'fanyi-dwim) '("example"))))))

(ert-deftest fanyi-etymon-rejects-empty-formatting ()
  (dolist (fragment '((blockquote nil "  ")
                      (blockquote nil (p nil))
                      (span ((class . "foreign notranslate")) "  ")
                      (span ((class . "foreign notranslate")) (strong nil "\u00a0"))
                      (script nil "This is not a definition")))
    (let* ((dom (fanyi-test--read-fixture "etymon.html"))
           (body (car (dom-by-class dom "word__defination--2q7ZH"))))
      (setcdr (cdr body) (list fragment))
      (should-error (fanyi-parse-from (clone fanyi-etymon-provider) dom)
                    :type 'fanyi-parse-error))))

(ert-deftest fanyi-etymon-keeps-paragraph-and-inline-separators ()
  (let* ((dom (fanyi-test--read-fixture "etymon.html"))
         (body (car (dom-by-class dom "word__defination--2q7ZH"))))
    (setcdr (cdr body) '("first" (p nil) "second" (span nil " ") "word"))
    (fanyi-test--with-rendered-response fanyi-etymon-provider dom
      (should (string-match-p "first\n\nsecond word" (buffer-string))))))

(ert-deftest fanyi-etymon-preserves-valid-entry-next-to-empty-entry ()
  (let ((dom (fanyi-test--read-fixture "etymon.html")))
    (dom-append-child (car (dom-by-tag dom 'body))
                      '(section ((class . "word--C9UPa"))
                                (h1 ((class . "word__name--TTbAA")) "broken")
                                (div ((class . "word__defination--2q7ZH")) (p nil))))
    (fanyi-test--with-rendered-response fanyi-etymon-provider dom
      (should (= (length (oref instance definitions)) 1))
      (should-not (string-match-p "broken" (buffer-string))))))

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
    (dolist (provider '(fanyi-haici-provider fanyi-etymon-provider fanyi-longman-provider))
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
