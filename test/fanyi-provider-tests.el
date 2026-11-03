;;; fanyi-provider-tests.el --- Offline dictionary fixtures -*- lexical-binding: t; -*-

(require 'fanyi-test-support)
(require 'fanyi-haici)
(require 'fanyi-longman)
(require 'fanyi-youdao-thesaurus)

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
    (dolist (text '("# 有道同义词词典" "- sample" "n. 样本；vt. 取样"
                    "- specimen" "NO EXPLANATION"))
      (should (string-match-p (regexp-quote text) (buffer-string))))))

(provide 'fanyi-provider-tests)
;;; fanyi-provider-tests.el ends here
