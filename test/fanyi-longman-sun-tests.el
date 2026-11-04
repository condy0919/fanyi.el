;;; fanyi-longman-sun-tests.el --- Saved sun page layout regressions -*- lexical-binding: t; -*-

(require 'fanyi-test-support)
(require 'fanyi-longman)

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
      (should-not (dom-by-class result "suppressed"))
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
                       '("the sun/the Sun"
                         "everything/anything etc under the sun"
                         "catch the sun" "make hay while the sun shines")))))))

(ert-deftest fanyi-longman-sun-collocations-have-readable-groups-and-examples ()
  (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-sun-saved.html"
    (fanyi-test--fontify)
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
    (dolist (node (dom-by-class result "^Collocate$"))
      (let* ((phrase (fanyi-shr-dom-text (car (dom-by-class node "\\bCOLLOC\\b"))))
             (example (fanyi-shr-dom-text (car (dom-by-class node "^EXAMPLE$"))))
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
    (fanyi-test--fontify)
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
    (fanyi-test--fontify)
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
        (let ((inhibit-read-only t)) (erase-buffer))
        (save-window-excursion
          (set-window-buffer nil (current-buffer))
          (let ((inhibit-read-only t)) (fanyi-render-result request result))
          (let ((noninteractive nil)) (font-lock-mode 1))
          (font-lock-ensure)
          (let ((indents (fanyi-test--sun-example-indents))
                (audio (fanyi-test--button-data #'fanyi-play-sound))
                (links (mapcar #'car (fanyi-test--button-data #'fanyi-longman--query)))
                (lines (count-lines (point-min) (point-max)))
                (shr-width 54))
            (fanyi-shr-reflow)
            (font-lock-flush)
            (font-lock-ensure)
            (should (> (count-lines (point-min) (point-max)) lines))
            (should (equal (fanyi-test--sun-example-indents) indents))
            (should (equal (fanyi-test--button-data #'fanyi-play-sound) audio))
            (should (equal (mapcar #'car (fanyi-test--button-data #'fanyi-longman--query)) links))
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
            (should (equal queries '("catch the sun")))
            (goto-char (point-min))
            (search-forward "Too much sun is bad for you.")
            (beginning-of-line)
            (let ((button (next-button (point) t)))
              (should (equal (button-get button 'help-echo) "Play example"))
              (button-activate button)
              (should (equal played (list (button-get button 'button-data)))))))))))

(defun fanyi-test--sun-check-frequency-display (svg)
  "Check that sun's frequency labels use SVG or their text fallback."
  (dolist (label '(("S2" . "Top 2000 spoken words")
                   ("W1" . "Top 1000 written words")))
    (goto-char (point-min))
    (search-forward (car label))
    (let ((start (match-beginning 0))
          (end (point)))
      ;; Keep both letters searchable/copyable and attach the tooltip to all
      ;; of the displayed label, rather than replacing its underlying text.
      (should (equal (buffer-substring-no-properties start end) (car label)))
      (dolist (position (number-sequence start (1- end)))
        (let ((display (get-char-property position 'display))
              (face (get-char-property position 'face)))
          (should (equal (get-char-property position 'help-echo) (cdr label)))
          (should (fanyi-test--longman-style-has-face-p
                   face 'fanyi-longman-frequency-face))
          (if svg
              (progn
                (should (eq (car-safe display) 'image))
                (should (eq (plist-get (cdr display) :type) 'svg))
                (should (string-match-p (regexp-quote (car label))
                                        (plist-get (cdr display) :data)))
                ;; The SVG owns its border; do not draw an Emacs face box
                ;; around it a second time.  Retain the named face beneath.
                (should (plist-member (car face) :box))
                (should-not (plist-get (car face) :box))
                (should (equal (plist-get (car face) :background)
                               (face-attribute 'fanyi-longman-body-face
                                               :background nil 'default))))
            (should-not display)
            (should-not (member '(:box nil) face)))))
      ;; Whitespace beside a label must not become part of the image.
      (should-not (get-char-property (1- start) 'display))
      (should-not (get-char-property end 'display)))))

(ert-deftest fanyi-longman-sun-frequency-svg-survives-font-lock-and-reflow ()
  (let ((shr-width 74) labels)
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
              ((symbol-function 'image-type-available-p) (lambda (type) (eq type 'svg)))
              ((symbol-function 'fanyi-longman-svg-tag-make)
               (lambda (text face)
                 (push (cons text face) labels)
                 `(image :type svg :data
                         ,(format "<svg xmlns=\"http://www.w3.org/2000/svg\"><text>%s</text></svg>"
                                  text)))))
      (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-sun-saved.html"
        ;; Fixture rendering normally disables glyphs.  Enable them inside
        ;; that binding before populating its cache again.
        (let ((fanyi-use-glyphs t))
          (let ((inhibit-read-only t)) (erase-buffer))
          (save-window-excursion
            (set-window-buffer nil (current-buffer))
            (let ((inhibit-read-only t)) (fanyi-render-result request result))
            (fanyi-test--sun-check-frequency-display t)
            (let ((noninteractive nil)) (font-lock-mode 1))
            (font-lock-ensure)
            (font-lock-flush)
            (font-lock-ensure)
            (fanyi-test--sun-check-frequency-display t)
            (let ((lines (count-lines (point-min) (point-max)))
                  (shr-width 54))
              (fanyi-shr-reflow)
              (font-lock-flush)
              (font-lock-ensure)
              (should (> (count-lines (point-min) (point-max)) lines))
              (fanyi-test--sun-check-frequency-display t))
            (dolist (label '("S2" "W1"))
              (should (member (cons label 'fanyi-longman-frequency-face) labels)))
            ;; Origin keeps its existing SVG, while group headings retain
            ;; text and their own face rather than becoming frequency badges.
            (should (member '("Origin" . fanyi-longman-svg-asset-face) labels))
            (goto-char (point-min))
            (search-forward "Origin sun1 Old English sunne")
            (should (eq (car-safe (get-char-property (match-beginning 0) 'display)) 'image))
            (dolist (heading '("VERBS" "ADJECTIVES/NOUN + SUN" "PHRASES" "NOUNS"))
              (fanyi-test--sun-line heading)
              (back-to-indentation)
              (should-not (get-char-property (point) 'display))
              (should (fanyi-test--longman-style-has-face-p
                       (get-char-property (point) 'face)
                       'fanyi-longman-group-heading-face)))))))))

(ert-deftest fanyi-longman-sun-frequency-svg-falls-back-to-styled-text ()
  ;; Respect the user setting, missing SVG support, and text-only displays
  ;; independently.  Do not mock `fanyi-display-glyphs-p' past these checks.
  (dolist (settings '((nil t t) (t nil t) (t t nil)))
    (ert-info ((format "Glyphs, SVG support, image display: %S" settings))
      (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) (nth 2 settings)))
                ((symbol-function 'image-type-available-p)
                 (lambda (type) (and (nth 1 settings) (eq type 'svg))))
                ((symbol-function 'fanyi-longman-svg-tag-make)
                 (lambda (&rest _) (ert-fail "SVG generation must be skipped"))))
        (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-sun-saved.html"
          ;; Explicitly set this inside the fixture macro, which has its own
          ;; default binding, before rendering the response under test.
          (let ((fanyi-use-glyphs (car settings)))
            (let ((inhibit-read-only t)) (erase-buffer))
            (let ((inhibit-read-only t)) (fanyi-render-result request result))
            (fanyi-test--fontify)
            (fanyi-test--sun-check-frequency-display nil)
            (should (face-attribute 'fanyi-longman-frequency-face :box nil t))
            (goto-char (point-min))
            (search-forward "Origin sun1 Old English sunne")
            (should-not (get-char-property (match-beginning 0) 'display))))))))

(ert-deftest fanyi-longman-sun-svg-preserves-origin-fill-and-frequency-outline ()
  ;; Exercise the real SVG builder.  An unspecified face box is truthy in
  ;; Lisp, but must not turn Origin's filled badge into a transparent outline.
  (should (eq (face-attribute 'fanyi-longman-svg-asset-face :box nil t) 'unspecified))
  (cl-letf (((symbol-function 'svg-image) (lambda (dom &rest _) dom)))
    (let* ((origin (fanyi-longman-svg-tag-make "Origin" 'fanyi-longman-svg-asset-face))
           (frequency (fanyi-longman-svg-tag-make "S2" 'fanyi-longman-frequency-face))
           (origin-rect (car (dom-by-tag origin 'rect)))
           (frequency-rect (car (dom-by-tag frequency 'rect))))
      (should origin-rect)
      (should frequency-rect)
      (should (equal (dom-attr origin-rect 'fill)
                     (face-attribute 'fanyi-longman-svg-asset-face :background nil t)))
      (should (equal (dom-attr origin-rect 'stroke) "none"))
      (should (equal (dom-attr frequency-rect 'fill) "none"))
      (should (equal (dom-attr frequency-rect 'stroke) "red")))))

(defun fanyi-test--sun-svg-data (text)
  "Return the actual SVG data for TEXT in the current result."
  (goto-char (point-min))
  (search-forward text)
  (plist-get (cdr (get-char-property (match-beginning 0) 'display)) :data))

(ert-deftest fanyi-longman-sun-refreshes-svg-and-settings-after-commands ()
  (let ((foreground (face-attribute 'fanyi-longman-frequency-face :foreground))
        (background (face-attribute 'fanyi-longman-svg-asset-face :background))
        (fanyi-longman-svg-border-radius 7)
        (fanyi-longman-example-indent 2))
    (unwind-protect
        (cl-letf (((symbol-function 'create-image) #'fanyi-test--create-svg-image)
                  ((symbol-function 'display-images-p) (lambda (&rest _) t))
                  ((symbol-function 'image-type-available-p) (lambda (type) (eq type 'svg))))
          (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-sun-saved.html"
            (let ((fanyi-use-glyphs t))
              (let ((inhibit-read-only t)) (erase-buffer))
              (save-window-excursion
                (set-window-buffer nil (current-buffer))
                (let ((inhibit-read-only t)) (fanyi-render-result request result))
                (let ((render (symbol-function 'fanyi-shr--render))
                      (renders 0))
                  (cl-letf (((symbol-function 'fanyi-shr--render)
                             (lambda (&rest arguments)
                               (cl-incf renders)
                               (apply render arguments))))
                    (run-hooks 'post-command-hook)
                    (should (= renders 0))
                    (set-face-attribute 'fanyi-longman-frequency-face nil :foreground "magenta")
                    (with-temp-buffer (run-hooks 'post-command-hook))
                    (should (= renders 0))
                    (run-hooks 'post-command-hook)
                    (should (= renders 1))
                    (should (string-match-p "magenta" (fanyi-test--sun-svg-data "S2")))
                    (set-face-attribute 'fanyi-longman-svg-asset-face nil :background "#123456")
                    (run-hooks 'post-command-hook)
                    (should (string-match-p "#123456" (fanyi-test--sun-svg-data "Origin")))
                    (setq fanyi-longman-svg-border-radius 3
                          fanyi-longman-example-indent 4)
                    (run-hooks 'post-command-hook)
                    (should (string-match-p "rx=\"3\"" (fanyi-test--sun-svg-data "W1")))
                    (should (equal (fanyi-test--sun-example-indents) '(6 6 6)))
                    (setq fanyi-use-glyphs nil)
                    (run-hooks 'post-command-hook)
                    (fanyi-test--sun-check-frequency-display nil)
                    (should-not (fanyi-test--sun-svg-data "Origin"))
                    (setq fanyi-use-glyphs t)
                    (run-hooks 'post-command-hook)
                    (fanyi-test--sun-check-frequency-display t)
                    (should (string-match-p "#123456" (fanyi-test--sun-svg-data "Origin")))
                    (should (= renders 5))
                    (run-hooks 'post-command-hook)
                    (should (= renders 5))))))))
      (set-face-attribute 'fanyi-longman-frequency-face nil :foreground foreground)
      (set-face-attribute 'fanyi-longman-svg-asset-face nil :background background))))

(ert-deftest fanyi-longman-svg-uses-remapped-window-font-metrics ()
  ;; Window metrics include buffer text scaling; frame metrics do not.
  (cl-letf (((symbol-function 'window-font-width) (lambda (&rest _) 20))
            ((symbol-function 'window-font-height) (lambda (&rest _) 40))
            ((symbol-function 'svg-image) (lambda (dom &rest _) dom)))
    (let* ((shr-use-fonts nil)
           (svg (fanyi-longman-svg-tag-make "S2" 'fanyi-longman-frequency-face))
           (text (car (dom-by-tag svg 'text))))
      (should (= (dom-attr svg 'width) 80))
      (should (= (dom-attr svg 'height) 40))
      (should (= (dom-attr text 'font-size) 30)))))

(provide 'fanyi-longman-sun-tests)
;;; fanyi-longman-sun-tests.el ends here
