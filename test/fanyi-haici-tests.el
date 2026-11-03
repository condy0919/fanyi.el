;;; fanyi-haici-tests.el --- Haici response edge cases -*- lexical-binding: t; -*-

(require 'fanyi-provider-tests)

(ert-deftest fanyi-haici-preserves-definitions-without-phonetics ()
  (let ((response (fanyi-test--read-fixture "haici.html")))
    (dom-remove-node response (car (dom-by-class response "phonetic")))
    (fanyi-test--with-rendered-response fanyi-haici-provider response
      (should (= (length (fanyi-haici-result-paraphrases result)) 2))
      (should (string-match-p "样本；例子" (buffer-string)))
      (should (string-match-p "取样；尝试" (buffer-string)))
      (should-not (fanyi-test--button-data #'fanyi-play-sound)))))

(ert-deftest fanyi-haici-labels-american-only-and-omits-missing-audio ()
  (let* ((response (fanyi-test--read-fixture "haici.html"))
         (phonetics (car (dom-by-class response "phonetic")))
         (pronunciations (dom-non-text-children phonetics))
         (american (cadr pronunciations)))
    (dom-remove-node phonetics (car pronunciations))
    ;; Class order is immaterial; the empty male identifier is not playable.
    (dom-set-attribute (dom-by-class american "fsound") 'class "fsound sound extra")
    (dom-set-attribute (dom-by-class american "^sound$") 'naudio "")
    (fanyi-test--with-rendered-response fanyi-haici-provider response
      (should (string-match-p (regexp-quote "美 [sæmpəl]") (buffer-string)))
      (should-not (string-match-p (regexp-quote "英 [") (buffer-string)))
      (should (equal (fanyi-test--button-data #'fanyi-play-sound)
                     '("https://audio.dict.cn/sample-us-f")))
      (should (string-match-p "取样；尝试" (buffer-string))))))

(ert-deftest fanyi-haici-keeps-compact-phonetics-without-audio ()
  (let* ((response (fanyi-test--read-fixture "haici.html"))
         (phonetics (car (dom-by-class response "phonetic"))))
    (setcdr (cdr phonetics) (dom-non-text-children phonetics))
    (dolist (node (dom-search phonetics (lambda (item) (dom-attr item 'naudio))))
      (dom-set-attribute node 'naudio nil))
    (fanyi-test--with-rendered-response fanyi-haici-provider response
      (should (string-match-p (regexp-quote "英 [sɑːmpəl]") (buffer-string)))
      (should (string-match-p (regexp-quote "美 [sæmpəl]") (buffer-string)))
      (should-not (fanyi-test--button-data #'fanyi-play-sound))
      (should (string-match-p "样本；例子" (buffer-string))))))

(ert-deftest fanyi-haici-keeps-last-definition-without-whitespace-or-placeholder ()
  (let ((response
         (with-temp-buffer
           (insert "<html><body><ul class='dict-basic-ul'><li><span>n.</span><strong>first meaning</strong></li><li><span>v.</span><strong>last <b>meaning</b></strong></li></ul></body></html>")
           (libxml-parse-html-region (point-min) (point-max)))))
    (fanyi-test--with-rendered-response fanyi-haici-provider response
      (should (equal (fanyi-haici-result-paraphrases result)
                     '(("n." "first meaning") ("v." "last meaning"))))
      (should (string-match-p "last meaning" (buffer-string))))))

(ert-deftest fanyi-haici-rejects-missing-or-empty-definitions ()
  (dolist (html '("<html><body>Service unavailable</body></html>"
                  "<html><body><ul class='dict-basic-ul'></ul></body></html>"
                  "<html><body><ul class='dict-basic-ul'><li><span>n.</span><strong> </strong></li></ul></body></html>"))
    (let ((response (with-temp-buffer
                      (insert html)
                      (libxml-parse-html-region (point-min) (point-max)))))
      (should-error (fanyi-parse-response (fanyi-make-request fanyi-haici-provider "sample") response)
                    :type 'fanyi-parse-error))))

(ert-deftest fanyi-haici-ignores-malformed-optional-sections ()
  (dolist (chart '("{broken" "[1,2]" "{\"1\":{\"percent\":\"bad\",\"sense\":\"noun\"}}"))
    (let ((response (fanyi-test--read-fixture "haici.html")))
      (dom-append-child (dom-by-tag response 'body)
                        `(div ((id . "dict-chart-basic") (data . ,chart))))
      (let ((shape (car (dom-by-class response "shape"))))
        (setcdr (cdr shape) '((span nil "orphan label"))))
      (dom-append-child (dom-by-tag response 'body)
                        '(div ((class . "layout etm")) (span nil) (p nil " ")))
      (fanyi-test--with-rendered-response fanyi-haici-provider response
        (should (= (length (fanyi-haici-result-paraphrases result)) 2))
        (should-not (fanyi-haici-result-distribution result))
        (should-not (fanyi-haici-result-related result))
        (should-not (fanyi-haici-result-etymons result))
        (should (string-match-p "样本；例子" (buffer-string)))))))

(ert-deftest fanyi-haici-reads-chart-fields-independent-of-json-key-order ()
  (let ((response (fanyi-test--read-fixture "haici.html")))
    (dom-append-child
     (dom-by-tag response 'body)
     '(div ((id . "dict-chart-basic")
            (data . "{\"1\":{\"sense\":\"noun\",\"percent\":75},\"2\":{\"percent\":25,\"sense\":\"verb\"}}"))))
    (fanyi-test--with-rendered-response fanyi-haici-provider response
      (should (equal (fanyi-haici-result-distribution result) '((75 "noun") (25 "verb"))))
      (should (string-match-p "Click to view the distribution chart" (buffer-string))))))

(provide 'fanyi-haici-tests)
;;; fanyi-haici-tests.el ends here
