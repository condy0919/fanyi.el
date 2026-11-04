;;; fanyi-longman-tests.el --- Longman response robustness -*- lexical-binding: t; -*-

(require 'fanyi-test-support)
(require 'fanyi-longman)

(defun fanyi-test--longman-html (html)
  "Parse a minimal Longman HTML response."
  (with-temp-buffer
    (insert html)
    (libxml-parse-html-region (point-min) (point-max))))

(ert-deftest fanyi-longman-keeps-headword-without-visible-hyphenation ()
  (dolist (hyphenation '("<span class='HYPHENATION'></span>"
                         "<span class='HYPHENATION'> \n\t </span>"
                         "<span class='HYPHENATION'>&nbsp;</span>"
                         "<span class='HYPHENATION' hidden='hidden'>sam‧ple</span>"
                         "<span style='display:none'><span class='HYPHENATION'>sam‧ple</span></span>"
                         "<span class='HYPHENATION'><span class='suppressed'>sam‧ple</span></span>"))
    (let* ((dom (fanyi-test--longman-html
                 (concat "<div class='dictionary'><div class='dictentry'><span class='Head'>"
                         "<span class='HWD'>sample</span>" hyphenation
                         "</span><span class='DEF'>A definition.</span></div></div>")))
           (before (copy-tree dom)))
      (fanyi-test--with-rendered-response fanyi-longman-provider dom
        (should (equal dom before))
        (should (= (how-many "\\bsample\\b" (point-min) (point-max)) 1))
        (should-not (string-match-p "sam‧ple" (buffer-string)))
        (should (string-match-p "A definition\\." (buffer-string)))))))

(ert-deftest fanyi-longman-prefers-usable-hyphenated-headword ()
  (dolist (hyphenation '("sam‧ple" "<b>sam</b>‧ple"))
    (let ((dom (fanyi-test--longman-html
                (concat "<div class='dictionary'><div class='dictentry'><span class='Head'>"
                        "<span class='HWD'>sample</span><span class='HYPHENATION'>"
                        hyphenation "</span></span><span class='DEF'>A definition.</span></div></div>"))))
      (fanyi-test--with-rendered-response fanyi-longman-provider dom
        (should (= (how-many "sam‧ple" (point-min) (point-max)) 1))
        (should-not (string-match-p "\\bsample\\b" (buffer-string)))
        (should-not (fanyi-longman--nodes result "HWD"))))))

(ert-deftest fanyi-longman-phrase-links-preserve-word-and-source-url ()
  (fanyi-test--with-query
    (let ((fanyi-providers (list fanyi-longman-provider (fanyi-test--provider))))
      (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-dragon-saved.html"
        (button-activate (fanyi-test--longman-style-button "chase the dragon"))
        (should (equal fanyi-current-word "chase the dragon"))
        (should (equal (car fanyi-history) "chase the dragon"))
        (let ((requests (mapcar #'fanyi--task-request (reverse fanyi--tasks))))
          (should (equal (mapcar #'fanyi-request-word requests)
                         '("chase the dragon" "chase the dragon")))
          (should (equal (mapcar #'fanyi-request-url requests)
                         '("https://www.ldoceonline.com/dictionary/chase-the-dragon"
                           "https://example.invalid/chase%20the%20dragon"))))
        ;; The clicked URL is scoped to that lookup, not future queries.
        (should (equal (fanyi-request-url (fanyi-make-request fanyi-longman-provider "next"))
                       "https://www.ldoceonline.com/dictionary/next"))))))

(ert-deftest fanyi-longman-local-anchor-opens-folds-after-reflow ()
  (let ((dom (fanyi-test--longman-html
              (concat "<div class='dictionary'><div class='dictentry'>"
                      "<a href='#target'>Jump</a><div class='ThesBox'>"
                      "<span class='heading'>THESAURUS</span>"
                      "<span class='DEF' id='target'>Destination definition.</span>"
                      "</div></div></div>"))))
    (fanyi-test--with-rendered-response fanyi-longman-provider dom
      (save-window-excursion
        (set-window-buffer nil (current-buffer))
        (button-activate (fanyi-test--longman-style-button "THESAURUS"))
        (goto-char (point-min))
        (outline-hide-subtree)
        (let ((shr-width 30)) (fanyi-shr-reflow))
        (fanyi-test--fontify)
        (let ((button (fanyi-test--longman-style-button "Jump")))
          (narrow-to-region (button-start button) (button-end button))
          (cl-letf (((symbol-function 'eww) (lambda (&rest _) (ert-fail "Anchor is local")))
                    ((symbol-function 'fanyi-dwim) (lambda (&rest _) (ert-fail "No new query"))))
            (button-activate button)))
        (should-not (buffer-narrowed-p))
        (should (looking-at-p "Destination"))
        (should-not (invisible-p (point)))))))

(ert-deftest fanyi-longman-missing-verb-table-opens-complete-url-in-eww ()
  (let* ((request (fanyi-make-request fanyi-longman-provider "explode"))
         (result (fanyi-parse-response request (fanyi-test--read-fixture "longman-explode-saved.html")))
         opened)
    (fanyi-test--with-result-buffer
      (let ((inhibit-read-only t)) (fanyi-render-result request result))
      (cl-letf (((symbol-function 'eww) (lambda (url &rest _) (setq opened url)))
                ((symbol-function 'fanyi-dwim) (lambda (&rest _) (ert-fail "No duplicate query"))))
        (button-activate (fanyi-test--longman-style-button "Verb table")))
      (should (equal opened "https://www.ldoceonline.com/dictionary/explode#explode__10")))))

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
        (should (equal (mapcar #'car (fanyi-test--button-data #'fanyi-longman--query)) '("example")))
        (should-not (fanyi-test--button-data #'fanyi-play-sound))))))

(ert-deftest fanyi-longman-unknown-crossref-does-not-discard-definition ()
  (let ((dom (fanyi-test--read-fixture "longman.html")))
    (dom-set-attribute
     (dom-child-by-tag (dom-by-class dom "Crossref") 'a)
     'href "https://example.invalid/help")
    (fanyi-test--with-rendered-response fanyi-longman-provider dom
      (should (string-match-p "used for testing" (buffer-string)))
      (should (equal (mapcar #'car (fanyi-test--button-data #'fanyi-longman--query)) '("part" "specimen"))))))

(ert-deftest fanyi-longman-preserves-nested-crossref-only-entries ()
  (let ((dom (fanyi-test--longman-html
              "<div class='dictionary'><div class='dictentry'><span class='Sense'><span class='Crossref'>See <span class='REFHWD'><a href='/dictionary/example#example__1'>example</a></span></span></span></div></div>")))
    (fanyi-test--with-rendered-response fanyi-longman-provider dom
      (should (string-match-p "See example" (buffer-string)))
      (should (equal (mapcar #'car (fanyi-test--button-data #'fanyi-longman--query)) '("example"))))))

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
        (should (member "sampler" (mapcar #'car (fanyi-test--button-data #'fanyi-longman--query))))))))

(ert-deftest fanyi-longman-unrecognized-word-family-is-optional ()
  (let ((dom (fanyi-test--read-fixture "longman.html")))
    (dom-append-child (car (dom-by-class dom "^dictionary$"))
                      '(div ((class . "wordfams")) (span nil "Different layout")))
    (fanyi-test--with-rendered-response fanyi-longman-provider dom
      (should (string-match-p "Different layout" (buffer-string)))
      (should (string-match-p "used for testing" (buffer-string))))))

(ert-deftest fanyi-longman-synonym-text-fallback-adds-only-a-missing-label ()
  (dolist (text '("specimen" "SYN specimen"))
    (let* ((dom (fanyi-test--read-fixture "longman.html"))
           (synonym (car (fanyi-longman--nodes dom "SYN"))))
      (setcdr (cdr synonym) (list text))
      (fanyi-test--with-rendered-response fanyi-longman-provider dom
        (should (= (how-many "\\bSYN\\b" (point-min) (point-max)) 1))
        (should (string-match-p "SYN specimen" (buffer-string)))
        (should (member "specimen" (mapcar #'car (fanyi-test--button-data #'fanyi-longman--query))))))))

(ert-deftest fanyi-longman-associations-retain-emphasis-labels-and-punctuation ()
  (dolist (case '(("SYN" "<span class='synopp'>SYN</span> <b>blow</b> up." "blow up" "SYN" bold ".")
                  ("RELATEDWD" "<span class='neutral'>, → </span><strong>explosion</strong>!" "explosion" "→" bold "!")
                  ("OPP" "<span class='neutral'>OPP</span> <em>shrink</em>;" "shrink" "OPP" italic ";")
                  ("OPP" "OPP contract" "contract" "OPP" nil nil)))
    (pcase-let ((`(,class ,html ,word ,label ,face ,punctuation) case))
      (let* ((dom (fanyi-test--longman-html
                   (concat "<div class='dictionary'><div class='dictentry'>"
                           "<span class='DEF'>A definition.</span><span class='" class "'>"
                           html "</span></div></div>")))
             (before (copy-tree dom)) queries)
        (fanyi-test--with-rendered-response fanyi-longman-provider dom
          (should (equal dom before))
          (fanyi-test--fontify)
          (should (equal (mapcar #'car (fanyi-test--button-data #'fanyi-longman--query)) (list word)))
          (goto-char (point-min))
          (search-forward label)
          (should-not (button-at (match-beginning 0)))
          (when (member class '("SYN" "OPP"))
            (should (fanyi-test--longman-style-has-face-p
                     (get-char-property (match-beginning 0) 'face) 'fanyi-longman-synopp-face)))
          (search-forward word)
          (let ((button (button-at (match-beginning 0))))
            (should (equal (button-get button 'button-data)
                           (cons word (concat "https://www.ldoceonline.com/dictionary/" (url-hexify-string word)))))
            (when face
              (should (fanyi-test--longman-style-has-face-p
                       (get-char-property (match-beginning 0) 'face) face)))
            (when punctuation
              (should (looking-at-p (regexp-quote punctuation)))
              (should-not (button-at (point))))
            (cl-letf (((symbol-function 'fanyi-dwim) (lambda (target) (push target queries))))
              (button-activate button)))
          (should (equal queries (list word))))))))

(ert-deftest fanyi-longman-associations-preserve-existing-anchors ()
  (dolist (html '("<span class='SYN'><span class='synopp'>SYN</span> <a href='/dictionary/rocket#rocket__2'><b>rocket</b></a></span>"
                  "<a href='/dictionary/rocket#rocket__2'><span class='SYN'><span class='synopp'>SYN</span> <b>rocket</b></span></a>"
                  "<a class='SYN' href='/dictionary/rocket#rocket__2'><b>rocket</b></a>"
                  "<span class='RELATEDWD'><span class='neutral'>→ </span>soar, <a href='/dictionary/rocket#rocket__2'><b>rocket</b></a></span>"))
    (let ((dom (fanyi-test--longman-html
                (concat "<div class='dictionary'><div class='dictentry'>"
                        "<span class='DEF'>A definition.</span>" html "</div></div>"))))
      (fanyi-test--with-rendered-response fanyi-longman-provider dom
        (should-not (dom-by-tag result 'fanyi-query))
        (should (equal (fanyi-test--button-data #'fanyi-longman--query)
                       '(("rocket" . "https://www.ldoceonline.com/dictionary/rocket#rocket__2"))))
        (should (= (how-many "\\brocket\\b" (point-min) (point-max)) 1))))))

(ert-deftest fanyi-longman-association-fallback-labels-use-svg ()
  (cl-letf (((symbol-function 'create-image) #'fanyi-test--create-svg-image)
            ((symbol-function 'display-images-p) (lambda (&rest _) t))
            ((symbol-function 'image-type-available-p) (lambda (type) (eq type 'svg))))
    (dolist (case '(("SYN" "rocket") ("SYN" "SYN rocket")
                    ("OPP" "OPP shrink") ("OPP" "<span class='neutral'>OPP</span> shrink")))
      (let* ((label (car case))
             (dom (fanyi-test--longman-html
                   (concat "<div class='dictionary'><div class='dictentry'>"
                           "<span class='DEF'>A definition.</span><span class='" label "'>"
                           (cadr case) "</span></div></div>"))))
        (fanyi-test--with-rendered-response fanyi-longman-provider dom
          (let ((fanyi-use-glyphs t) (inhibit-read-only t))
            (erase-buffer)
            (fanyi-render-result request result))
          (should (= (how-many label (point-min) (point-max)) 1))
          (goto-char (point-min))
          (search-forward label)
          (let ((display (get-char-property (match-beginning 0) 'display)))
            (should (eq (car-safe display) 'image))
            (should (string-match-p label (plist-get (cdr display) :data)))))))))

(ert-deftest fanyi-longman-associations-do-not-infer-queries-from-ambiguous-text ()
  (dolist (html '("rocket, soar" "rocket or soar" "rise and fall"
                  "to suddenly increase in strength" "rise (informal)"
                  "<span>SYN <b>rocket</b></span>"
                  "rocket <span class='REGISTERLAB'>informal</span>"))
    (let ((dom (fanyi-test--longman-html
                (concat "<div class='dictionary'><div class='dictentry'>"
                        "<span class='DEF'>A definition.</span><span class='SYN'>"
                        html "</span></div></div>"))))
      (fanyi-test--with-rendered-response fanyi-longman-provider dom
        (should-not (dom-by-tag result 'fanyi-query))
        (should-not (fanyi-test--button-data #'fanyi-longman--query))
        (should (= (how-many "\\bSYN\\b" (point-min) (point-max)) 1))
        (should (string-match-p "rocket\\|rise\\|increase" (buffer-string)))))))

(ert-deftest fanyi-longman-validates-visible-prepared-content ()
  (let* ((dom (fanyi-test--longman-html
               (concat "<div class='dictionary'>"
                       "<div class='dictentry'><span hidden='hidden'><span class='DEF'>Hidden definition.</span></span></div>"
                       "<div class='dictentry'><span class='suppressed Crossref'><a href='/dictionary/hidden'>Hidden reference.</a></span></div>"
                       "<div class='dictentry HWD'><span class='DEF'>Visible definition.</span></div>"
                       "</div>")))
         (before (copy-tree dom))
         (request (fanyi-make-request fanyi-longman-provider "sample"))
         (result (fanyi-parse-response request dom)))
    (should (equal dom before))
    ;; A face wrapper around the surviving entry must not hide its identity.
    (should (= (length (fanyi-longman--nodes result "dictentry")) 1))
    (should (string-match-p "Visible definition" (fanyi-shr-dom-text result)))
    (should-not (string-match-p "Hidden" (fanyi-shr-dom-text result)))))

(ert-deftest fanyi-longman-prepares-once-and-reuses-result ()
  (let* ((dom (fanyi-test--read-fixture "longman-sun-saved.html"))
         (before (copy-tree dom))
         (rules (copy-tree fanyi-longman--class-styles))
         (request (fanyi-make-request fanyi-longman-provider "sun"))
         (sections (fanyi-longman--sections
                    (car (fanyi-longman--nodes dom "dictionary"))))
         (prepare (symbol-function 'fanyi-longman--prepare-node))
         (section-counts (make-hash-table :test #'eq))
         result)
    (cl-letf (((symbol-function 'fanyi-longman--prepare-node)
               (lambda (node base &optional context)
                 (when (memq node sections)
                   (puthash node (1+ (gethash node section-counts 0)) section-counts))
                 (funcall prepare node base context))))
      (setq result (fanyi-parse-response request dom)))
    (dolist (section sections)
      (should (= (gethash section section-counts 0) 1)))
    (let ((prepared (copy-tree result))
          (fanyi-use-glyphs nil))
      (cl-letf (((symbol-function 'fanyi-longman--prepare-node)
                 (lambda (&rest _) (ert-fail "Prepared DOM must be reused"))))
        (with-temp-buffer
          (let ((fanyi-buffer-name (buffer-name))
                (shr-width 74))
            (fanyi-mode)
            (save-window-excursion
              (set-window-buffer nil (current-buffer))
              (let ((inhibit-read-only t)) (fanyi-render-result request result))
              (let ((first (buffer-string))
                    (inhibit-read-only t))
                (erase-buffer)
                (let ((inhibit-read-only t)) (fanyi-render-result request result))
                (should (equal first (buffer-string))))
              (let ((lines (count-lines (point-min) (point-max)))
                    (shr-width 44))
                (fanyi-shr-reflow)
                (should (> (count-lines (point-min) (point-max)) lines)))))))
      (should (equal result prepared)))
    (should (equal dom before))
    (should (equal fanyi-longman--class-styles rules))))

(provide 'fanyi-longman-tests)
;;; fanyi-longman-tests.el ends here
