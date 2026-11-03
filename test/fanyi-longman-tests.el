;;; fanyi-longman-tests.el --- Longman response robustness -*- lexical-binding: t; -*-

(require 'fanyi-provider-tests)

(defun fanyi-test--longman-html (html)
  "Parse a minimal Longman HTML response."
  (with-temp-buffer
    (insert html)
    (libxml-parse-html-region (point-min) (point-max))))

(ert-deftest fanyi-longman-pronunciations-are-independent ()
  (dolist (missing '("brefile" "amefile"))
    (dolist (empty '(nil "" "  "))
      (let ((dom (fanyi-test--read-fixture "longman.html")))
        (dom-set-attribute (car (dom-by-class dom missing)) 'data-src-mp3 empty)
        (fanyi-test--with-rendered-response fanyi-longman-provider dom
          (should (string-match-p "used for testing" (buffer-string)))
          (should (equal (fanyi-test--button-data #'fanyi-play-sound)
                         (list (if (equal missing "brefile")
                                   "https://example.invalid/sample-us.mp3"
                                 "https://example.invalid/sample-uk.mp3")
                               "https://example.invalid/sample-example.mp3"))))))))

(ert-deftest fanyi-longman-examples-without-audio-remain-readable ()
  (let ((dom (fanyi-test--read-fixture "longman.html")))
    (dom-remove-node dom (car (dom-by-class dom "speaker")))
    (fanyi-test--with-rendered-response fanyi-longman-provider dom
      (should (string-match-p "Take a sample\\." (buffer-string)))
      (should (equal (fanyi-test--button-data #'fanyi-play-sound)
                     '("https://example.invalid/sample-uk.mp3"
                       "https://example.invalid/sample-us.mp3"))))))

(ert-deftest fanyi-longman-rejects-missing-or-empty-core-content ()
  (dolist (html '("<html><body>Service unavailable</body></html>"
                  "<div class='dictionary'></div>"
                  "<div class='dictionary'><div class='dictentry'></div></div>"
                  "<div class='dictionary'><div class='dictentry'><span class='Sense'><span class='DEF'> </span></span></div></div>"
                  "<div class='dictionary'><div class='dictentry'><span class='Sense'><span class='Crossref'><a href='/help'>Help</a></span></span></div></div>"))
    (should-error (fanyi-parse-response (fanyi-make-request fanyi-longman-provider "sample")
                                        (fanyi-test--longman-html html))
                  :type 'fanyi-parse-error)))

(ert-deftest fanyi-longman-preserves-crossref-only-entries ()
  (dolist (href '("/dictionary/example#example__1"
                  "https://www.ldoceonline.com/dictionary/example#example__1"))
    (let ((dom (fanyi-test--longman-html
                (format "<div class='dictionary'><div class='dictentry'><span class='Sense'><span class='Crossref'><a href='%s'>See example</a></span></span></div></div>"
                        href))))
      ;; There is no Head or DEF: a working cross-reference is useful content.
      (fanyi-test--with-rendered-response fanyi-longman-provider dom
        (should (string-match-p "See example" (buffer-string)))
        (should (equal (fanyi-test--button-data #'fanyi-dwim) '("example")))
        (should-not (fanyi-test--button-data #'fanyi-play-sound))))))

(ert-deftest fanyi-longman-unknown-crossref-does-not-discard-definition ()
  (let ((dom (fanyi-test--read-fixture "longman.html")))
    (dom-set-attribute
     (dom-child-by-tag (dom-by-class dom "Crossref") 'a)
     'href "https://example.invalid/help")
    (fanyi-test--with-rendered-response fanyi-longman-provider dom
      (should (string-match-p "used for testing" (buffer-string)))
      (should (equal (fanyi-test--button-data #'fanyi-dwim) '("part"))))))

(ert-deftest fanyi-longman-preserves-nested-crossref-only-entries ()
  (let ((dom (fanyi-test--longman-html
              "<div class='dictionary'><div class='dictentry'><span class='Sense'><span class='Crossref'>See <span class='REFHWD'><a href='/dictionary/example#example__1'>example</a></span></span></span></div></div>")))
    (fanyi-test--with-rendered-response fanyi-longman-provider dom
      (should (string-match-p "See example" (buffer-string)))
      (should (equal (fanyi-test--button-data #'fanyi-dwim) '("example"))))))

(ert-deftest fanyi-longman-word-family-does-not-depend-on-whitespace ()
  (dolist (separator '("" "\n  "))
    (let* ((dom (fanyi-test--read-fixture "longman.html"))
           (family (fanyi-test--longman-html
                    (concat "<div class='wordfams'>" separator
                            "<span class='asset_intro'>Word family</span>" separator
                            "<span class='pos'>(noun)</span>" separator
                            "<a>sampler</a> sample "
                            "<span class='pos'>(verb)</span>" separator
                            "<a>sample</a></div>"))))
      (dom-append-child (car (dom-by-class dom "^dictionary$"))
                        (car (dom-by-class family "wordfams")))
      (fanyi-test--with-rendered-response fanyi-longman-provider dom
        (dolist (text '("Word family" "(noun)" "sampler" "(verb)"))
          (should (string-match-p (regexp-quote text) (buffer-string))))
        (should (string-match-p "used for testing" (buffer-string)))
        (should (member "sampler" (fanyi-test--button-data #'fanyi-dwim)))))))

(ert-deftest fanyi-longman-unrecognized-word-family-is-optional ()
  (let ((dom (fanyi-test--read-fixture "longman.html")))
    (dom-append-child (car (dom-by-class dom "^dictionary$"))
                      '(div ((class . "wordfams")) (span nil "Different layout")))
    (fanyi-test--with-rendered-response fanyi-longman-provider dom
      (should (string-match-p "Different layout" (buffer-string)))
      (should (string-match-p "used for testing" (buffer-string))))))

(provide 'fanyi-longman-tests)
;;; fanyi-longman-tests.el ends here
