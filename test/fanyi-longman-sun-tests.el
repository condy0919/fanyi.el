;;; fanyi-longman-sun-tests.el --- Saved sun page layout regressions -*- lexical-binding: t; -*-

(require 'fanyi-longman-style-tests)

(defun fanyi-test--sun-line (text &optional prefix)
  "Find a line containing TEXT and return its beginning.
Allow subsequent text on the line when PREFIX is non-nil."
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (should (re-search-forward
             (concat "^[ \t]*" (regexp-quote text)
                     (if prefix "\\(?:[ \t]\\|$\\)" "[ \t]*$"))
             nil t)))
  (line-beginning-position))

(defun fanyi-test--sun-blank-before-p (position)
  "Whether the line before POSITION is blank."
  (save-excursion
    (goto-char position)
    (forward-line -1)
    (looking-at-p "[ \t]*$")))

(defun fanyi-test--sun-example-indents ()
  "Return the indentation of the three recorded sun examples."
  (mapcar
   (lambda (text)
     (goto-char (point-min))
     (search-forward text)
     (current-indentation))
   '("Too much sun is bad for you."
     "We sat in the sun, eating ice cream."
     "the warmth of the afternoon sun")))

(ert-deftest fanyi-longman-sun-keeps-all-entries-and-hides-suppressed-text ()
  (let* ((dom (fanyi-test--read-fixture "longman-sun-saved.html"))
         (before (copy-tree dom)))
    ;; The newspaper, noun, verb and abbreviation all belong to this response.
    (should (= (length (dom-by-class dom "^dictentry$")) 4))
    (should (dom-by-class dom "suppressed"))
    (should-not (dom-by-tag dom 'script))
    (should-not (dom-by-tag dom 'iframe))
    (should-not (dom-by-class dom "am-dictionary"))
    (fanyi-test--with-rendered-response fanyi-longman-provider dom
      (should (equal dom before))
      (should (dom-by-class (oref instance content) "suppressed"))
      (let ((case-fold-search nil))
        (should-not (string-match-p
                     "The Sun" (buffer-substring-no-properties (point-min) (point-max)))))
      (fanyi-test--sun-line "Sun, The trademark" t)
      (fanyi-test--sun-line "sun1 /sʌn/" t)
      (fanyi-test--sun-line "sun2 verb (sunned, sunning)" t)
      (fanyi-test--sun-line "Sun. (also Sun British English)")
      (fanyi-test--sun-line "the written abbreviation of Sunday")
      (should (= (how-many "^[ \t]*• " (point-min) (point-max)) 27))
      (fanyi-test--sun-line "Origin sun1 Old English sunne"))))

(ert-deftest fanyi-longman-sun-crossrefs-follow-sense-numbers-and-keep-actions ()
  (let (queries)
    (cl-letf (((symbol-function 'fanyi-dwim) (lambda (word) (push word queries))))
      (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-sun-saved.html"
        (dolist (line '("1 → the sun/the Sun"
                        "4 → everything/anything etc under the sun"
                        "5 → catch the sun"))
          (fanyi-test--sun-line line))
        ;; Tail cross-references still start their own line.
        (fanyi-test--sun-line "→ make hay while the sun shines")
        (dolist (text '("the sun/the Sun" "everything/anything etc under the sun"
                        "catch the sun" "make hay while the sun shines"))
          (let ((button (fanyi-test--longman-style-button text)))
            (should button)
            (button-activate button)))
        (should (equal (nreverse queries)
                       '("the-sun-the-sun"
                         "everything-anything-etc-under-the-sun"
                         "catch-the-sun" "make-hay-while-the-sun-shines")))))))

(ert-deftest fanyi-longman-sun-collocations-have-readable-groups-and-examples ()
  (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-sun-saved.html"
    (fanyi-test--longman-style-fontify)
    (should (fanyi-test--sun-blank-before-p
             (fanyi-test--sun-line "COLLOCATIONS – Meanings 1 & 2")))
    (dolist (text '("COLLOCATIONS" "– Meanings 1 & 2"))
      (should (fanyi-test--longman-style-has-face-p
               (fanyi-test--longman-style-face text)
               'fanyi-longman-box-heading-face)))
    (dolist (heading '("VERBS" "ADJECTIVES/NOUN + SUN" "PHRASES" "NOUNS"))
      (should (fanyi-test--sun-blank-before-p (fanyi-test--sun-line heading)))
      (should (fanyi-test--longman-style-has-face-p
               (fanyi-test--longman-style-face heading)
               'fanyi-longman-group-heading-face)))
    ;; All 14 collocations have their own line and margin above, including
    ;; the ones whose inline explanatory gloss follows the bold phrase.
    (dolist (collocation '("the sun shines"
                           "the sun beats down/blazes down"
                           "the sun comes out"
                           "the sun rises/comes up"
                           "the sun sets/goes down"
                           "the sun sinks"
                           "the hot/warm sun"
                           "the blazing/burning sun"
                           "a bright sun"
                           "the morning/afternoon/evening sun"
                           "the midday/noonday sun"
                           "the rising/setting sun"
                           "the sun is high/low in the sky"
                           "the sun’s rays"))
      (let ((position (fanyi-test--sun-line collocation t)))
        (should (fanyi-test--sun-blank-before-p position))
        (goto-char position)
        (back-to-indentation)
        (should (fanyi-test--longman-style-has-face-p
                 (get-char-property (point) 'face) 'bold))))
    ;; Each collocation's example survives on a more deeply indented line.
    (dolist (node (dom-by-class (oref instance content) "^Collocate$"))
      (let* ((phrase (dom-texts (car (dom-by-class node "\\bCOLLOC\\b")) ""))
             (example (dom-texts (car (dom-by-class node "^EXAMPLE$")) ""))
             (phrase-start (fanyi-test--sun-line phrase t))
             (phrase-indent (current-indentation))
             (example-start (fanyi-test--sun-line example)))
        (should (> example-start phrase-start))
        (should (> (current-indentation) phrase-indent))
        (should (fanyi-test--longman-style-has-face-p
                 (fanyi-test--longman-style-face example)
                 'fanyi-longman-example-face))))))

(ert-deftest fanyi-longman-sun-audio-preserves-example-indent-and-actions ()
  (let* ((dom (fanyi-test--read-fixture "longman-sun-saved.html"))
         (without-audio (copy-tree dom))
         (urls (mapcar (lambda (node) (dom-attr node 'data-src-mp3))
                       (dom-by-class dom "\\bexafile\\b")))
         with-indents without-indents played)
    (dolist (node (dom-by-class without-audio "\\bexafile\\b"))
      (dom-remove-node without-audio node))
    (cl-letf (((symbol-function 'fanyi-play-sound) (lambda (url) (push url played))))
      (fanyi-test--with-rendered-response fanyi-longman-provider dom
        (setq with-indents (fanyi-test--sun-example-indents))
        (let ((position (point-min)) button)
          (while (setq button (next-button position t))
            (when (equal (button-get button 'help-echo) "Play example")
              (button-activate button))
            (setq position (button-end button)))))
      (fanyi-test--with-rendered-response fanyi-longman-provider without-audio
        (setq without-indents (fanyi-test--sun-example-indents))))
    (should (= (length urls) 3))
    (should (equal (nreverse played) urls))
    (should (equal with-indents without-indents))
    (should (cl-every (lambda (indent) (> indent 0)) with-indents))))

(ert-deftest fanyi-longman-sun-grammar-and-secondary-corpus-titles-have-context ()
  (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-sun-saved.html"
    (fanyi-test--longman-style-fontify)
    (let* ((grammar-title (fanyi-test--sun-line "in the sun"))
           (grammar-indent (current-indentation)))
      (should (fanyi-test--longman-style-has-face-p
               (fanyi-test--longman-style-face "in the sun\n") 'bold))
      (should (= grammar-indent (cadr (fanyi-test--sun-example-indents))))
      (goto-char grammar-title)
      (forward-line 1)
      (should (looking-at-p "[ \t]*🔊[ \t]+We sat in the sun")))
    (goto-char (point-min))
    (search-forward "## Examples from the Corpus sun")
    (should (re-search-forward "^[ \t]*in the sun[ \t]*$" nil t))
    (let ((corpus-title (line-beginning-position)))
      (should (fanyi-test--sun-blank-before-p corpus-title))
      (back-to-indentation)
      (should (fanyi-test--longman-style-has-face-p
               (get-char-property (point) 'face) 'fanyi-longman-heading-face))
      (should-not (looking-at-p "##")))
    (should (= (how-many "^## Examples from the Corpus sun$"
                         (point-min) (point-max)) 2))))

(ert-deftest fanyi-longman-sun-homonyms-and-frequency-labels-survive-font-lock ()
  (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-sun-saved.html"
    (fanyi-test--longman-style-fontify)
    (goto-char (point-min))
    (let ((count 0))
      ;; Include the origin's homonym number as well as noun and verb heads.
      (while (re-search-forward "sun[12]" nil t)
        (let ((display (get-char-property (1- (point)) 'display)))
          (should (eq (car-safe display) 'raise))
          (should (> (cadr display) 0)))
        (cl-incf count))
      (should (= count 3)))
    (dolist (label '(("S2" . "Top 2000 spoken words")
                     ("W1" . "Top 1000 written words")))
      (should (fanyi-test--longman-style-has-face-p
               (fanyi-test--longman-style-face (car label))
               'fanyi-longman-frequency-face))
      (should (equal (get-char-property (match-beginning 0) 'help-echo)
                     (cdr label))))
    (dolist (text '("sunned" "sunning" "Sunday"))
      (should (fanyi-test--longman-style-has-face-p
               (fanyi-test--longman-style-face text) 'bold)))))

(ert-deftest fanyi-longman-sun-reflow-keeps-styles-indentation-and-actions ()
  (let ((shr-width 74) queries played)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (&rest _) (ert-fail "Rendering must stay offline")))
              ((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _) (ert-fail "Rendering must stay offline")))
              ((symbol-function 'url-queue-retrieve)
               (lambda (&rest _) (ert-fail "Rendering must stay offline")))
              ((symbol-function 'fanyi-dwim) (lambda (word) (push word queries)))
              ((symbol-function 'fanyi-play-sound) (lambda (url) (push url played))))
      (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-sun-saved.html"
        ;; Initialize the major mode before rendering, as an actual query does,
        ;; so its cached DOM and font-lock state have the normal lifecycle.
        (fanyi-mode)
        (let ((inhibit-read-only t)) (erase-buffer))
        (save-window-excursion
          (set-window-buffer nil (current-buffer))
          (fanyi-render instance)
          (let ((noninteractive nil)) (font-lock-mode 1))
          (font-lock-ensure)
          (let ((indents (fanyi-test--sun-example-indents))
                (audio (fanyi-test--button-data #'fanyi-play-sound))
                (links (fanyi-test--button-data #'fanyi-dwim))
                (lines (count-lines (point-min) (point-max)))
                (shr-width 54))
            (fanyi-shr-reflow)
            (font-lock-flush)
            (font-lock-ensure)
            (should (> (count-lines (point-min) (point-max)) lines))
            (should (equal (fanyi-test--sun-example-indents) indents))
            (should (equal (fanyi-test--button-data #'fanyi-play-sound) audio))
            (should (equal (fanyi-test--button-data #'fanyi-dwim) links))
            (dolist (field '(("COLLOCATIONS" . fanyi-longman-box-heading-face)
                             ("VERBS" . fanyi-longman-group-heading-face)
                             ("S2" . fanyi-longman-frequency-face)
                             ("the hot/warm sun" . bold)))
              (should (fanyi-test--longman-style-has-face-p
                       (fanyi-test--longman-style-face (car field)) (cdr field))))
            (goto-char (point-min))
            (search-forward "sun1")
            (should (eq (car-safe (get-char-property (1- (point)) 'display)) 'raise))
            (should-not queries)
            (should-not played)
            (button-activate (fanyi-test--longman-style-button "catch the sun"))
            (should (equal queries '("catch-the-sun")))
            (goto-char (point-min))
            (search-forward "Too much sun is bad for you.")
            (beginning-of-line)
            (let ((button (next-button (point) t)))
              (should (equal (button-get button 'help-echo) "Play example"))
              (button-activate button)
              (should (equal played (list (button-get button 'button-data)))))))))))

(provide 'fanyi-longman-sun-tests)
;;; fanyi-longman-sun-tests.el ends here
