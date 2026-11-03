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
    (dolist (text '("# 海词" "英 [sɑːmpəl]" "美 [sæmpəl]" "- n. 样本；例子" "- vt. 取样；尝试"))
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
    (should (equal (mapcar #'car (fanyi-test--button-data #'fanyi-longman--query)) '("part" "example")))))

(ert-deftest fanyi-provider-youdao-fixture ()
  (fanyi-test--with-rendered-fixture fanyi-youdao-thesaurus-provider "youdao.json"
    (should (= (length result) 2))
    (dolist (text '("# 有道同义词词典" "sample" "n. 样本" "vt. 取样"
                    "specimen" "暂无释义"))
      (should (string-match-p (regexp-quote text) (buffer-string))))))

(provide 'fanyi-provider-tests)
;;; fanyi-provider-tests.el ends here
