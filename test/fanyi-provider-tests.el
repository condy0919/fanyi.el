;;; fanyi-provider-tests.el --- Offline dictionary fixtures -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'fanyi)
(require 'fanyi-haici)
(require 'fanyi-longman)
(require 'fanyi-youdao-thesaurus)

(defconst fanyi-test--fixture-directory
  (expand-file-name "fixtures"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun fanyi-test--read-fixture (file)
  "Parse the HTML or JSON fixture FILE using the real response parser."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file fanyi-test--fixture-directory))
    (if (string-suffix-p ".json" file)
        (progn (goto-char (point-min)) (json-parse-buffer))
      (libxml-parse-html-region (point-min) (point-max)))))

(defmacro fanyi-test--with-rendered-response (provider response &rest body)
  "Parse RESPONSE with PROVIDER and evaluate BODY in its result buffer.
Temporarily rename an existing result buffer so its text and mode survive."
  (declare (indent 2) (debug (form form body)))
  `(let* ((fanyi-use-glyphs nil)
          (existing (get-buffer fanyi-buffer-name))
          (original-name (and existing (buffer-name existing)))
          (instance (clone ,provider))
          result-buffer)
     (unwind-protect
         (progn
           (when existing
             (with-current-buffer existing
               (rename-buffer (generate-new-buffer-name " *fanyi-saved-result*"))))
           (setq result-buffer (get-buffer-create fanyi-buffer-name))
           (fanyi-set-query-word instance "sample")
           (fanyi-parse-from instance ,response)
           (fanyi-render instance)
           (with-current-buffer result-buffer
             ,@body))
       (when (buffer-live-p result-buffer)
         (kill-buffer result-buffer))
       (when (buffer-live-p existing)
         (with-current-buffer existing
           (rename-buffer original-name))))))

(defmacro fanyi-test--with-rendered-fixture (provider file &rest body)
  "Parse FILE with PROVIDER and evaluate BODY in its rendered result buffer."
  (declare (indent 2) (debug (form form body)))
  `(fanyi-test--with-rendered-response ,provider (fanyi-test--read-fixture ,file)
     ,@body))

(defun fanyi-test--button-data (action)
  "Return button data for buttons invoking ACTION in the current buffer."
  (let ((position (point-min)) data button)
    (while (setq button (next-button position t))
      (when (eq (button-get button 'action) action)
        (push (button-get button 'button-data) data))
      (setq position (button-end button)))
    (nreverse data)))

(ert-deftest fanyi-provider-haici-fixture ()
  (fanyi-test--with-rendered-fixture fanyi-haici-provider "haici.html"
    (should (equal (oref instance syllable) "sam·ple"))
    (should (= (oref instance star) 3))
    (should (equal (oref instance paraphrases)
                   '(("n." "样本；例子") ("vt." "取样；尝试"))))
    (dolist (text '("# 海词" "英 [sɑːmpəl]" "美 [sæmpəl]" "- n. 样本；例子" "- vt. 取样；尝试"))
      (should (string-match-p (regexp-quote text) (buffer-string))))
    (should (equal (fanyi-test--button-data #'fanyi-play-sound)
                   (mapcar (lambda (id) (format (oref instance sound-url) id))
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
    (should (equal (fanyi-test--button-data #'fanyi-dwim) '("part" "example")))))

(ert-deftest fanyi-provider-youdao-fixture ()
  (fanyi-test--with-rendered-fixture fanyi-youdao-thesaurus-provider "youdao.json"
    (should (= (length (oref instance entries)) 2))
    (dolist (text '("# 有道同义词词典" "- sample" "(n.) 样本；(vt.) 取样"
                    "- specimen" "NO EXPLANATION"))
      (should (string-match-p (regexp-quote text) (buffer-string))))))

(provide 'fanyi-provider-tests)
;;; fanyi-provider-tests.el ends here
