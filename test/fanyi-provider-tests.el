;;; fanyi-provider-tests.el --- Offline dictionary fixtures -*- lexical-binding: t; -*-

(require 'fanyi-test-support)
(require 'fanyi-haici)
(require 'fanyi-longman)
(require 'fanyi-youdao-thesaurus)
(require 'fanyi-etymon)

(ert-deftest fanyi-provider-renderers-insert-at-point-without-selecting-a-buffer ()
  (let ((fanyi-buffer-name (generate-new-buffer-name " *fanyi-wrong-destination*"))
        (fanyi-use-glyphs nil))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create fanyi-buffer-name)
            (insert "Untouched destination")
            (setq buffer-read-only t))
          (dolist (entry `((,fanyi-haici-provider . "haici.html")
                           (,fanyi-longman-provider . "longman.html")
                           (,fanyi-youdao-thesaurus-provider . "youdao.json")
                           (,fanyi-etymon-provider)))
            (let* ((request (fanyi-make-request (car entry) "sample"))
                   (result (when (cdr entry)
                             (fanyi-parse-response request (fanyi-test--read-fixture (cdr entry))))))
              (with-temp-buffer
                (insert "Prefix\nSuffix")
                (goto-char 8)
                ;; Each callback must insert here, even with unrelated text
                ;; after point and a different buffer named by fanyi-buffer-name.
                (fanyi-render-result request result)
                (should (looking-at-p "Suffix"))
                (should (> (point) 8))
                (should (string-prefix-p "Prefix\n# " (buffer-string)))
                (should (string-suffix-p "Suffix" (buffer-string)))))
            (with-current-buffer fanyi-buffer-name
              (should buffer-read-only)
              (should (equal (buffer-string) "Untouched destination")))))
      (when (get-buffer fanyi-buffer-name) (kill-buffer fanyi-buffer-name)))))

(ert-deftest fanyi-provider-haici-fixture ()
  (fanyi-test--with-rendered-fixture fanyi-haici-provider "haici.html"
    (should (equal (fanyi-haici-result-syllable result) "sam·ple"))
    (should (= (fanyi-haici-result-star result) 3))
    (should (equal (fanyi-haici-result-paraphrases result)
                   '(("n." "样本；例子") ("vt." "取样；尝试"))))
    (dolist (text '("# 海词" "英 [sɑːmpəl]" "美 [sæmpəl]" "n. 样本；例子" "vt. 取样；尝试"))
      (should (string-match-p (regexp-quote text) (buffer-string))))
    (should (equal (fanyi-test--button-data #'fanyi-play-sound)
                   (mapcar (lambda (id) (format (fanyi-request-sound-url request) id))
                           '("sample-uk-f" "sample-uk-m" "sample-us-f" "sample-us-m"))))
    (should (equal (fanyi-test--button-data #'fanyi-dwim) '("sampler")))
    (should (equal (fanyi-test--button-data #'eww) '("https://dict.cn/sample")))))

(ert-deftest fanyi-provider-longman-fixture ()
  (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman.html"
    (dolist (text '("# Longman" "From the test dictionary" "sam‧ple" "noun"
                    "a small" "used for testing" "Take a sample." "From Latin exemplum."))
      (should (string-match-p (regexp-quote text) (buffer-string))))
    (should (equal (fanyi-test--button-data #'fanyi-play-sound)
                   '("https://example.invalid/sample-uk.mp3"
                     "https://example.invalid/sample-us.mp3"
                     "https://example.invalid/sample-example.mp3")))
    (should (equal (mapcar #'car (fanyi-test--button-data #'fanyi-longman--query)) '("part" "specimen" "example")))))

(ert-deftest fanyi-provider-youdao-fixture ()
  (fanyi-test--with-rendered-fixture fanyi-youdao-thesaurus-provider "youdao.json"
    (should (= (length result) 2))
    (dolist (text '("# 有道同义词词典" "sample" "n. 样本" "vt. 取样"
                    "specimen" "暂无释义"))
      (should (string-match-p (regexp-quote text) (buffer-string))))))

(defun fanyi-provider-test--parse-html (provider html)
  "Parse synthetic HTML with PROVIDER, preserving its source DOM."
  (with-temp-buffer
    (insert html)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           (before (copy-tree dom)))
      (unwind-protect
          (fanyi-parse-response (fanyi-make-request provider "sample") dom)
        (should (equal dom before))))))

(ert-deftest fanyi-provider-recognizes-explicit-missing-entry-pages ()
  ;; These are synthetic contracts, not complete downloaded pages.  Haici's
  ;; title and prompt come from its indexed missing-entry pages.  Longman's
  ;; didyoumean class is documented in the user-saved common CSS.
  (dolist (html '("<html><head><title>该词条未找到_海词词典</title></head><body>Sorry，没有找到与此相符的结果。</body></html>"
                  "<html><head><title> 该词条未找到_海词词典 </title></head><body><h2>您要查找的是不是：</h2><a href='/sample'>sample</a></body></html>"))
    (should-not
     (catch 'not-found
       (fanyi-provider-test--parse-html fanyi-haici-provider html)
       'unexpected-result)))
  (dolist (href '("/dictionary/sample" "https://www.ldoceonline.com/dictionary/sample"))
    (should-not
     (catch 'not-found
       (fanyi-provider-test--parse-html
        fanyi-longman-provider
        (format "<html><body><ul class='extra didyoumean'><li><a href='%s'>sample</a></li></ul></body></html>"
                href))
       'unexpected-result))))

(ert-deftest fanyi-provider-does-not-confuse-broken-pages-with-missing-entries ()
  (dolist (body '("" "Service unavailable" "<p hidden='hidden'>您要查找的是不是：</p>"
                  "<div style='display:none'>Sorry，没有找到与此相符的结果。</div>"
                  "<script>您要查找的是不是：</script>"))
    (should-error
     (fanyi-provider-test--parse-html
      fanyi-haici-provider
      (concat "<html><head><title>该词条未找到_海词词典</title></head><body>"
              body "</body></html>"))
     :type 'fanyi-parse-error))
  (dolist (html '("<html><head><title>Just a moment...</title></head><body>您要查找的是不是：</body></html>"
                  "<html><head><title>海词词典</title></head><body>Sorry，没有找到与此相符的结果。</body></html>"))
    (should-error (fanyi-provider-test--parse-html fanyi-haici-provider html)
                  :type 'fanyi-parse-error))
  (dolist (html '("<div class='didyoumean'></div>"
                  "<ul class='not-didyoumean'><li><a href='/dictionary/sample'>sample</a></li></ul>"
                  "<ul class='didyoumean'><li><a href='/help'>Help</a></li></ul>"
                  "<ul class='didyoumean'><li><a href='https://other.invalid/dictionary/sample'>sample</a></li></ul>"
                  "<ul class='didyoumean'><li><a href='/dictionary/sample'> </a></li></ul>"
                  "<ul class='didyoumean' hidden='hidden'><li><a href='/dictionary/sample'>sample</a></li></ul>"
                  "<div style='display:none'><ul class='didyoumean'><li><a href='/dictionary/sample'>sample</a></li></ul></div>"
                  "<div aria-hidden='true'><ul class='didyoumean'><li><a href='/dictionary/sample'>sample</a></li></ul></div>"
                  "<script><ul class='didyoumean'><li><a href='/dictionary/sample'>sample</a></li></ul></script>"))
    (should-error (fanyi-provider-test--parse-html fanyi-longman-provider html)
                  :type 'fanyi-parse-error)))

(ert-deftest fanyi-provider-usable-entries-take-precedence-over-suggestions ()
  (should
   (fanyi-haici-result-p
    (fanyi-provider-test--parse-html
     fanyi-haici-provider
     (concat "<html><head><title>该词条未找到_海词词典</title></head><body>"
             "<p>您要查找的是不是：</p><ul class='dict-basic-ul'>"
             "<li><span>n.</span><strong>样本</strong></li></ul></body></html>"))))
  (should
   (fanyi-provider-test--parse-html
    fanyi-longman-provider
    (concat "<div class='dictionary'><div class='dictentry'><span class='DEF'>A sample.</span></div></div>"
            "<ul class='didyoumean'><li><a href='/dictionary/other'>other</a></li></ul>"))))

(provide 'fanyi-provider-tests)
;;; fanyi-provider-tests.el ends here
