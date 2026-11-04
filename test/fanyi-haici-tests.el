;;; fanyi-haici-tests.el --- Haici response edge cases -*- lexical-binding: t; -*-

(require 'fanyi-test-support)
(require 'fanyi-haici)

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

(ert-deftest fanyi-haici-reads-phonetic-label-from-direct-text-only ()
  (let* ((response (fanyi-test--read-fixture "haici.html"))
         (phonetics (car (dom-by-class response "phonetic")))
         (british (car (dom-non-text-children phonetics))))
    ;; The explicit label takes precedence over a conflicting language hint,
    ;; even when pronunciation markup appears before the label.
    (setcdr (cdr british) '((span ((lang . "en-US")) "[sample]") " UK "))
    (fanyi-test--with-rendered-response fanyi-haici-provider response
      (should (equal (car (fanyi-haici-result-phonetics result))
                     '("英" "[sample]" nil nil)))
      (should (string-match-p (regexp-quote "英 [sample]") (buffer-string))))))

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
      (should (string-match-p "释义分布" (buffer-string))))))

(defun fanyi-haici-test--position (text)
  "Return the first position of TEXT in the result buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (match-beginning 0)))

(ert-deftest fanyi-haici-saved-dragon-includes-all-tabs-without-mutating-response ()
  (let* ((response (fanyi-test--read-fixture "haici-dragon-saved.html"))
         (original (copy-tree response t)))
    (fanyi-test--with-rendered-response fanyi-haici-provider response
      (should (equal response original))
      (should (= (length (dom-by-tag (fanyi-haici-result-content result)
                                     'fanyi-haici-section)) 8))
      (dolist (heading '("详尽释义" "双解释义" "英英释义" "例句" "词汇搭配"
                         "经典引文" "近反义词" "临近单词"))
        (let ((button (button-at (fanyi-haici-test--position heading))))
          (should button)
          (should (eq (button-get button 'action) #'fanyi-shr-toggle-section))))
      (should-not (string-match-p "## 释义\\|## 用法与例句\\|## 相关资料" (buffer-string)))
      (dolist (text '("“飞龙”（美国研制的一种轰炸机）" "龙的，像龙的"
                      "[C]凶恶的人,悍妇" "a fiercely vigilant and unpleasant woman"
                      "The contour of that mountain looks like a dragon."
                      "like a dragon" "They..faced the dragon's breath of fire."
                      "Draco" "Dragonair"))
        (should (string-match-p (regexp-quote text) (buffer-string))))
      (should (string-match-p "This jade dragon is a genuine antique\\.\n +这件玉龙是真正的古玩。"
                              (buffer-string)))
      (should (string-match-p "Saint George, that swing'd the dragon\\.\n +出自：King John"
                              (buffer-string)))
      (should-not (string-match-p "查看更多\\|收起\\|Highcharts\\|google_ad" (buffer-string)))
      (should (= (length (fanyi-test--button-data #'fanyi-dwim)) 36))
      (should (= (length (fanyi-test--button-data #'fanyi-play-sound)) 4)))))

(ert-deftest fanyi-haici-rich-definitions-survive-missing-basic-summary ()
  (let ((response (fanyi-test--read-fixture "haici-dragon-saved.html")))
    (dom-remove-node response (car (dom-by-class response "dict-basic-ul")))
    (dom-remove-node response (car (dom-by-class response "keyword")))
    (fanyi-test--with-rendered-response fanyi-haici-provider response
      (should-not (fanyi-haici-result-paraphrases result))
      (should (equal (fanyi-haici-result-syllable result) "sample"))
      (should (string-match-p "严格而警惕的女监护人" (buffer-string))))
    ;; Related links and example sentences alone do not constitute a definition.
    (dom-remove-node response (car (dom-by-class response "section def")))
    (should-error (fanyi-parse-response (fanyi-make-request fanyi-haici-provider "dragon")
                                        response)
                  :type 'fanyi-parse-error)))

(ert-deftest fanyi-haici-rich-tabs-filter-controls-and-keep-unfamiliar-content ()
  (let* ((response (fanyi-test--read-fixture "haici.html"))
         (extra (with-temp-buffer
                  (insert "<div class='section sent'>
<h3>例句</h3><div class='extra sort layout' style='display:none;height:1px'>
<ol><li>A <em>small</em> sample.<br>一个小样本。</li></ol>
<script>unwanted script</script><style>unwanted style</style>
<div class='dict-chart'>unwanted chart</div><div slot='25'>unwanted ad</div>
<div id='div-gpt-ad-1'>unwanted slot</div><div hidden=''>unwanted hidden</div>
<p style='DISPLAY: none'>unwanted display</p><p aria-hidden='true'>unwanted aria</p>
<a class='fold'>unwanted fold</a><a class='unfold'>unwanted unfold</a>
<img src='https://example.invalid/image.png'><iframe>unwanted frame</iframe>
</div><h3>常见句型</h3><div class='layout struc' style='display:none'>
<dl><dt>Use <em>sample</em></dt><dd><mark>Unfamiliar inline markup</mark>.</dd></dl>
</div><h3>空白栏目</h3><div class='layout empty'><script>unwanted</script></div></div>
<div class='section rel'><h3>词源解说</h3><div class='layout etm'><ul><li>Latin origin.</li></ul></div></div>")
                  (libxml-parse-html-region (point-min) (point-max)))))
    (dolist (group (dom-by-class extra "section"))
      (dom-append-child (dom-by-tag response 'body) group))
    (fanyi-test--with-rendered-response fanyi-haici-provider response
      (should-not (string-match-p "unwanted\\|空白栏目\\|image.png" (buffer-string)))
      (dolist (text '("A small sample." "一个小样本。" "常见句型" "Unfamiliar inline markup."))
        (should (string-match-p (regexp-quote text) (buffer-string))))
      (should (= (how-many "Latin origin\\." (point-min) (point-max)) 1))
      (should (= (length (dom-by-tag (fanyi-haici-result-content result)
                                     'fanyi-haici-section)) 3))
      (fanyi-test--fontify)
      (should (memq 'shadow (ensure-list (get-char-property (fanyi-haici-test--position "一个小样本。")
                                                            'face)))))))

(ert-deftest fanyi-haici-rich-links-query-full-words-and-retain-other-text ()
  (let ((response (fanyi-test--read-fixture "haici.html")) queries opened played)
    (dom-append-child
     (dom-by-tag response 'body)
     '(div ((class . "section sent"))
           (h3 nil "词汇搭配")
           (div ((class . "layout anno"))
                (ul nil
                    (li nil (a ((href . "/dragon%20boat%20race")) "dragon boat…"))
                    (li nil (a ((href . "//dict.cn/%E9%BE%99")) "龙"))
                    (li nil (a ((href . "https://example.invalid/reference")) "Reference"))
                    (li nil (a ((href . "javascript:alert(1)")) "Script label"))
                    (li nil (a ((href . "file:///tmp/example")) "File label"))
                    (li nil (a nil "Unlinked label")))
                (i ((class . "extra fsound sound") (naudio . "example.mp3"))))))
    (fanyi-test--with-rendered-response fanyi-haici-provider response
      (should (equal (fanyi-test--button-data #'fanyi-dwim) '("sampler" "dragon boat race" "龙")))
      (should (equal (sort (fanyi-test--button-data #'eww) #'string<)
                     '("https://dict.cn/sample" "https://example.invalid/reference")))
      (dolist (text '("Script label" "File label" "Unlinked label"))
        (should-not (button-at (fanyi-haici-test--position text))))
      (cl-letf (((symbol-function 'fanyi-dwim) (lambda (word) (push word queries)))
                ((symbol-function 'eww) (lambda (url &rest _) (push url opened)))
                ((symbol-function 'fanyi-play-sound) (lambda (url) (push url played))))
        (dolist (text '("dragon boat…" "龙" "Reference"))
          (button-activate (button-at (fanyi-haici-test--position text))))
        (goto-char (point-max))
        (search-backward "🔊")
        (button-activate (button-at (point))))
      (should (equal queries '("龙" "dragon boat race")))
      (should (equal opened '("https://example.invalid/reference")))
      (should (equal played '("https://audio.dict.cn/example.mp3"))))))

(ert-deftest fanyi-haici-rich-reflow-preserves-folds-selection-faces-and-buttons ()
  (fanyi-test--with-result-buffer
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (let* ((request (fanyi-make-request fanyi-haici-provider "dragon"))
             (result (fanyi-parse-response request (fanyi-test--read-fixture "haici-dragon-saved.html")))
             (content (copy-tree (fanyi-haici-result-content result) t))
             (fanyi-haici-expanded-sections t)
             (shr-width 80) (inhibit-read-only t))
        (insert "Earlier dictionary\n\n")
        (fanyi-haici-render result request)
        (insert "Later dictionary\n")
        (button-activate (button-at (fanyi-haici-test--position "详尽释义")))
        (should (invisible-p (fanyi-haici-test--position "龙骑枪")))
        (should-not (invisible-p (fanyi-haici-test--position "[C]龙")))
        (goto-char (fanyi-haici-test--position "这件玉龙是真正的古玩。"))
        (set-mark (point))
        (forward-char (length "这件玉龙是真正的古玩。"))
        (let ((region (car fanyi-shr--regions))
              (selected (buffer-substring-no-properties (mark) (point)))
              (queries (fanyi-test--button-data #'fanyi-dwim))
              (audio (fanyi-test--button-data #'fanyi-play-sound))
              (transient-mark-mode t) (mark-active t) (deactivate-mark nil))
          (cl-letf (((symbol-function 'url-retrieve) (lambda (&rest _) (ert-fail "No network")))
                    ((symbol-function 'url-retrieve-synchronously) (lambda (&rest _) (ert-fail "No network")))
                    ((symbol-function 'url-queue-retrieve) (lambda (&rest _) (ert-fail "No network")))
                    ((symbol-function 'fanyi-haici--prepare-node) (lambda (&rest _) (ert-fail "Reuse prepared DOM"))))
            (dolist (width '(36 74 42))
              (let ((shr-width width))
                (fanyi-shr-reflow)
                (fanyi-test--fontify)
                (should (equal content (fanyi-haici-result-content result)))
                (should (use-region-p))
                (should (equal selected (buffer-substring-no-properties (mark) (point))))
                (should (invisible-p (fanyi-haici-test--position "龙骑枪")))
                (should-not (invisible-p (fanyi-haici-test--position "[C]龙")))
                (should (equal queries (fanyi-test--button-data #'fanyi-dwim)))
                (should (equal audio (fanyi-test--button-data #'fanyi-play-sound)))
                (should (string-prefix-p "Earlier dictionary\n\n" (buffer-string)))
                (should (string-suffix-p "Later dictionary\n" (buffer-string)))
                (dolist (field '(("Noun:" . fanyi-pos-face)
                                 ("这件玉龙是真正的古玩。" . shadow)
                                 ("dragon boat race" . fanyi-haici-link-face)))
                  (let ((position (fanyi-haici-test--position (car field))))
                    (should (memq (cdr field) (ensure-list (get-char-property position 'face))))
                    (should (get-text-property position 'font-lock-face))))
                (save-restriction
                  (narrow-to-region (fanyi-shr--region-start region) (fanyi-shr--region-end region))
                  ;; SHR may keep final CJK punctuation with the preceding
                  ;; character, carrying it over the edge by two columns.
                  (should (<= (fanyi-shr-test--max-line-width) (+ width 2)))))))
          (button-activate (button-at (fanyi-haici-test--position "详尽释义")))
          (should-not (invisible-p (fanyi-haici-test--position "龙骑枪"))))))))

(ert-deftest fanyi-haici-layout-defaults-prioritize-definitions-and-usage ()
  (let ((fanyi-haici-expanded-sections '(dual sort anno nfo)))
    (fanyi-test--with-rendered-fixture fanyi-haici-provider "haici-dragon-saved.html"
      (dolist (text '("[C]龙" "This jade dragon is a genuine antique."
                      "dragon boat race" "firedrake"))
        (should-not (invisible-p (fanyi-haici-test--position text))))
      (dolist (text '("龙骑枪" "a fiercely vigilant and unpleasant woman"
                      "Saint George, that swing'd the dragon." "Dragonair"))
        (should (invisible-p (fanyi-haici-test--position text))))
      (dolist (heading '("详尽释义 · 13" "双解释义 · 2" "英英释义 · 4"
                         "例句 · 3" "词汇搭配 · 20" "经典引文 · 2"
                         "近反义词 · 4" "临近单词 · 12"))
        (let ((position (fanyi-haici-test--position heading)))
          (should-not (invisible-p position))
          (should (button-at position))))
      (button-activate (button-at (fanyi-haici-test--position "详尽释义")))
      (should-not (invisible-p (fanyi-haici-test--position "龙骑枪")))
      (should (invisible-p (fanyi-haici-test--position "Dragonair")))
      (should (string-match-p "\\[C\\]龙\n[ ]+a big dangerous animal" (buffer-string)))
      (should (string-match-p "这件玉龙是真正的古玩。\n\n[ ]*[^\n]*Dragons are described"
                              (buffer-string))))))

(ert-deftest fanyi-haici-layout-honors-initial-section-configuration ()
  (dolist (setting '(t (sort) nil))
    (let ((fanyi-haici-expanded-sections setting))
      (fanyi-test--with-rendered-fixture fanyi-haici-provider "haici-dragon-saved.html"
        (dolist (entry '((detail . "龙骑枪") (dual . "[C]龙")
                         (sort . "This jade dragon is a genuine antique.")
                         (anno . "dragon boat race") (nwd . "Dragonair")))
          (should (eq (not (invisible-p (fanyi-haici-test--position (cdr entry))))
                      (and (or (eq setting t) (memq (car entry) setting)) t)))))))
  (let ((response (fanyi-test--read-fixture "haici-dragon-saved.html"))
        (fanyi-haici-expanded-sections '(dual sort anno nfo)))
    (dom-remove-node response (car (dom-by-class response "layout dual")))
    (fanyi-test--with-rendered-response fanyi-haici-provider response
      (should-not (invisible-p (fanyi-haici-test--position "龙骑枪")))
      (should-not (string-match-p "双解释义" (buffer-string))))))

(ert-deftest fanyi-haici-layout-aligns-vocabulary-and-reflows-complete-header ()
  (fanyi-test--with-result-buffer
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (let* ((request (fanyi-make-request fanyi-haici-provider "dragon"))
             (result (fanyi-parse-response request (fanyi-test--read-fixture "haici-dragon-saved.html")))
             (fanyi-use-glyphs nil)
             (fanyi-haici-expanded-sections '(dual sort anno nfo))
             (shr-width 80) (inhibit-read-only t))
        (fanyi-haici-render result request)
        (should (string-match-p "dragon boat race[ ]+龙舟竞赛" (buffer-string)))
        (let ((columns (mapcar (lambda (text)
                                 (goto-char (fanyi-haici-test--position text))
                                 (current-column))
                               '("龙舟竞赛" "服用鸦片" "龙血树"))))
          (should (apply #'= columns)))
        (let* ((region (car fanyi-shr--regions))
               (dom (fanyi-shr--region-dom region))
               (audio (fanyi-test--button-data #'fanyi-play-sound))
               (words (fanyi-test--button-data #'fanyi-dwim))
               (selected "龙舟竞赛"))
          ;; The word header belongs to the same reflowable document as its tabs.
          (should (<= (fanyi-shr--region-start region) (fanyi-haici-test--position "drag·on")))
          (should (get-text-property (fanyi-haici-test--position "drag·on") 'fanyi-html))
          (button-activate (button-at (fanyi-haici-test--position "详尽释义")))
          (goto-char (fanyi-haici-test--position selected))
          (set-mark (point))
          (forward-char (length selected))
          (let ((transient-mark-mode t) (mark-active t) (deactivate-mark nil))
            (cl-letf (((symbol-function 'url-retrieve) (lambda (&rest _) (ert-fail "No network")))
                      ((symbol-function 'url-queue-retrieve) (lambda (&rest _) (ert-fail "No network"))))
              (dolist (width '(34 80))
                (let ((shr-width width))
                  (fanyi-shr-reflow)
                  (fanyi-test--fontify)
                  (should (eq dom (fanyi-shr--region-dom region)))
                  (should (use-region-p))
                  (should (equal selected (buffer-substring-no-properties (mark) (point))))
                  (should-not (invisible-p (fanyi-haici-test--position "龙骑枪")))
                  (should (invisible-p (fanyi-haici-test--position "Dragonair")))
                  (should (equal audio (fanyi-test--button-data #'fanyi-play-sound)))
                  (should (equal words (fanyi-test--button-data #'fanyi-dwim)))
                  ;; SHR keeps final CJK punctuation on the preceding line;
                  ;; snapshot's deeper block indentation can need two columns.
                  (should (<= (fanyi-shr-test--max-line-width) (+ width 2)))
                  (when (= width 34)
                    (should (string-match-p "dragon boat race\n[ ]+龙舟竞赛" (buffer-string)))))))))))))

(ert-deftest fanyi-haici-layout-level-tooltip-and-compact-nearby-words ()
  (let ((fanyi-haici-expanded-sections t))
    (fanyi-test--with-rendered-fixture fanyi-haici-provider "haici-dragon-saved.html"
      (let ((label (fanyi-haici-test--position "核心词汇")))
        (should (string-match-p "海词4星核心词汇，属常用3000词。"
                                (get-text-property label 'help-echo)))
        (should-not (get-text-property label 'display)))
      (should-not (string-match-p "海词4星核心词汇，属常用3000词。" (buffer-string)))
      (let ((line-a (line-number-at-pos (fanyi-haici-test--position "dragonfly")))
            (line-b (line-number-at-pos (fanyi-haici-test--position "Dragone"))))
        (should (= line-a line-b)))
      (should (button-at (fanyi-haici-test--position "原网页"))))))

(ert-deftest fanyi-haici-layout-aligns-numbered-meanings-and-hanging-explanations ()
  (dolist (width '(80 36))
    (let ((shr-width width)
          (fanyi-haici-expanded-sections t))
      (fanyi-test--with-rendered-fixture fanyi-haici-provider "haici-dragon-saved.html"
        (goto-char (fanyi-haici-test--position "详尽释义"))
        (re-search-forward "^ +1 +\\(龙\\)$")
        (goto-char (match-beginning 1))
        (let ((meaning-column (current-column)))
          (re-search-forward "^ +10 +\\(【天】\\)")
          (goto-char (match-beginning 1))
          (should (= (current-column) meaning-column)))
        (goto-char (fanyi-haici-test--position "[C]龙"))
        (let ((meaning-column (current-column))
              (lines 0))
          (forward-line 1)
          (should (looking-at-p " +a big dangerous animal"))
          ;; Every line of the English explanation starts underneath the
          ;; Chinese meaning, including continuations in a narrow window.
          (while (not (looking-at-p "^ +2 "))
            (should-not (eobp))
            (should (= (current-indentation) meaning-column))
            (end-of-line)
            (should (<= (current-column) width))
            (cl-incf lines)
            (forward-line 1))
          (when (= width 36) (should (> lines 1))))))))

(ert-deftest fanyi-haici-layout-keeps-unusual-list-text-and-numbering ()
  (let ((response (fanyi-test--read-fixture "haici.html"))
        (fanyi-haici-expanded-sections t))
    (dom-append-child
     (dom-by-tag response 'body)
     '(div ((class . "section sent"))
           (h3 nil "词汇搭配")
           (div ((class . "layout anno"))
                (ul nil "Context outside the entry."
                    (li nil (a ((href . "/sample")) "sample") " 样本")))
           (h3 nil "额外释义")
           (div ((class . "layout extra"))
                (ol ((start . "-1")) (li nil "negative") (li nil "zero") (li nil "positive")))))
    (fanyi-test--with-rendered-response fanyi-haici-provider response
      (should (string-match-p "Context outside the entry\\." (buffer-string)))
      (dolist (text '("-1 negative" " 0 zero" " 1 positive"))
        (should (string-match-p (regexp-quote text) (buffer-string))))
      (should (equal (get-text-property (fanyi-haici-test--position "★★★") 'help-echo)
                     "3 常用词")))))

(ert-deftest fanyi-haici-preserves-complete-syllables-and-falls-back-to-visible-word ()
  (dolist (word '("mother-in-law" "ice cream" "café" "rock’n’roll" "sam·ple"))
    (dolist (prefix '("" "音节划分：" "音节划分: "))
      (let* ((response (fanyi-test--read-fixture "haici.html"))
             (keyword (car (dom-by-class response "keyword")))
             (request (fanyi-make-request fanyi-haici-provider "sample")))
        (dom-set-attribute keyword 'tip (concat prefix word " "))
        (should (equal (fanyi-haici-result-syllable (fanyi-haici-parse response request)) word)))))
  (dolist (tip '(nil "" " \t " "音节划分： "))
    (let* ((response (fanyi-test--read-fixture "haici.html"))
           (keyword (car (dom-by-class response "keyword")))
           (request (fanyi-make-request fanyi-haici-provider "query")))
      (dom-set-attribute keyword 'tip tip)
      (should (equal (fanyi-haici-result-syllable (fanyi-haici-parse response request)) "sample"))
      (dom-remove-node response keyword)
      (should (equal (fanyi-haici-result-syllable (fanyi-haici-parse response request)) "query")))))

(ert-deftest fanyi-haici-counts-only-visible-outer-definition-items ()
  (let* ((response (fanyi-test--read-fixture "haici-dragon-saved.html"))
         (detail (car (dom-by-class response "layout detail")))
         (list (car (dom-by-tag detail 'ol))))
    (dom-append-child (car (dom-by-tag list 'li))
                      '(ul nil (li nil "Nested illustration.")))
    (dom-append-child list '(li ((style . "display:none")) "Hidden meaning."))
    (dom-append-child list '(li nil (script nil "Advertising script.")))
    (dom-append-child list '(li nil " \t "))
    (fanyi-test--with-rendered-response fanyi-haici-provider response
      (should (string-match-p "详尽释义 · 13" (buffer-string)))
      (should (string-match-p "Nested illustration\\." (buffer-string)))
      (should-not (string-match-p "Hidden meaning\\|Advertising script" (buffer-string))))))

(ert-deftest fanyi-haici-counts-origin-paragraphs-and-omits-uncertain-counts ()
  (let ((response (fanyi-test--read-fixture "haici.html")))
    (dom-append-child
     (dom-by-tag response 'body)
     '(div ((class . "section rel"))
           (h3 nil "词源解说")
           (div ((class . "layout etm"))
                (p nil "First origin.") (p nil "Second origin.")
                (p nil " ") (p ((hidden . "")) "Hidden origin.")
                (a ((class . "unfold")) "收起"))
           (h3 nil "词语辨析")
           (div ((class . "layout extra"))
                (p nil "Useful prose.") (a ((class . "fold")) "查看更多"))))
    (fanyi-test--with-rendered-response fanyi-haici-provider response
      (should (string-match-p "词源解说 · 2" (buffer-string)))
      (should (string-match-p "词语辨析\n" (buffer-string)))
      (should-not (string-match-p "词语辨析 ·" (buffer-string)))
      (should (string-match-p "Useful prose\\." (buffer-string))))))

(ert-deftest fanyi-haici-counts-queryable-nearby-words-after-filtering-controls ()
  (let* ((response (fanyi-test--read-fixture "haici-dragon-saved.html"))
         (nearby (car (dom-by-class response "layout nwd"))))
    (dom-append-child nearby '(a ((class . "fold") (href . "javascript:;")) "查看更多"))
    (dom-append-child nearby '(a ((hidden . "") (href . "/hidden")) "Hidden word"))
    (fanyi-test--with-rendered-response fanyi-haici-provider response
      (should (string-match-p "临近单词 · 12" (buffer-string)))
      (should (= (length (fanyi-test--button-data #'fanyi-dwim)) 36)))))

(provide 'fanyi-haici-tests)
;;; fanyi-haici-tests.el ends here
