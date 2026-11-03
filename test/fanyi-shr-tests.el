;;; fanyi-shr-tests.el --- Longman HTML rendering tests -*- lexical-binding: t; -*-

(require 'fanyi-test-support)
(require 'fanyi-longman)
(require 'shr)

(defun fanyi-test--rendered-text ()
  "Return current buffer text with insignificant whitespace normalized."
  (replace-regexp-in-string "[ \t\r\n]+" " " (buffer-string)))

(defun fanyi-test--shr-html (html)
  "Parse compact HTML used by the shr regression tests."
  (with-temp-buffer
    (insert html)
    (libxml-parse-html-region (point-min) (point-max))))

(defun fanyi-test--has-face-p (position expected)
  "Return whether POSITION has EXPECTED face before or after fontification."
  (cl-some (lambda (property)
             (let ((face (get-text-property position property)))
               (or (eq face expected) (and (listp face) (memq expected face)))))
           '(face font-lock-face)))

(defun fanyi-test--find-action-button (action data)
  "Find a button invoking ACTION with DATA in the current buffer."
  (let ((position (point-min)) found button)
    (while (and (not found) (setq button (next-button position t)))
      (when (and (eq (button-get button 'action) action)
                 (equal (button-get button 'button-data) data))
        (setq found button))
      (setq position (button-end button)))
    found))

(ert-deftest fanyi-shr-preserves-new-markup-and-document-structure ()
  (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-rich.html"
    (let ((text (fanyi-test--rendered-text)))
      (dolist (expected '("Nested bold italic and new field text."
                          "First paragraph." "Second paragraph."
                          "After the break." "First list item" "Second list item"
                          "Form" "Meaning" "sampled" "past tense"
                          "A newly formatted example." "From an older word."))
        (should (string-match-p (regexp-quote expected) text))))
    (should (string-match-p "^# Longman$" (buffer-string)))
    (should (string-match-p "^## Rich test dictionary$" (buffer-string)))
    ;; Paragraphs, br and list items should remain visibly separate.
    (should (string-match-p "First paragraph\\.\n" (buffer-string)))
    (should (string-match-p "Second paragraph\\.\n" (buffer-string)))
    (should (string-match-p "First list item\n" (buffer-string)))))

(ert-deftest fanyi-shr-keeps-formatting-properties ()
  (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-rich.html"
    (fanyi-test--fontify)
    (goto-char (point-min))
    (search-forward "bold")
    (let ((face (or (get-text-property (match-beginning 0) 'face)
                    (get-text-property (match-beginning 0) 'font-lock-face))))
      (should (or (eq face 'bold) (and (listp face) (memq 'bold face)))))
    (search-forward "italic")
    (let ((face (or (get-text-property (match-beginning 0) 'face)
                    (get-text-property (match-beginning 0) 'font-lock-face))))
      (should (or (eq face 'italic) (and (listp face) (memq 'italic face)))))))

(ert-deftest fanyi-shr-selects-entries-and-does-not-fetch-resources ()
  (cl-letf (((symbol-function 'url-retrieve)
             (lambda (&rest _) (ert-fail "Rendering must not fetch resources")))
            ((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _) (ert-fail "Rendering must not fetch resources")))
            ((symbol-function 'url-queue-retrieve)
             (lambda (&rest _) (ert-fail "Rendering must not fetch resources"))))
    (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-rich.html"
      (dolist (unexpected '("Site navigation" "Advertisement" "Site footer" "Script body"
                            "Invalid entry placeholder" "Unrelated entry"))
        (should-not (string-match-p (regexp-quote unexpected) (buffer-string)))))))

(ert-deftest fanyi-shr-preserves-input-dom ()
  (let* ((dom (fanyi-test--read-fixture "longman-rich.html"))
         (before (copy-tree dom)))
    (fanyi-test--with-rendered-response fanyi-longman-provider dom
      (should (equal dom before)))))

(ert-deftest fanyi-shr-audio-buttons-play-only-when-activated ()
  (let (played)
    (cl-letf (((symbol-function 'fanyi-play-sound)
               (lambda (url) (push url played))))
      (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-rich.html"
        (should-not played)
        (let ((button (fanyi-test--find-action-button
                       #'fanyi-play-sound "https://www.ldoceonline.com/media/sample-uk.mp3")))
          (should button)
          (button-activate button))
        (should (equal played '("https://www.ldoceonline.com/media/sample-uk.mp3")))))))

(ert-deftest fanyi-shr-dictionary-links-decode-query-words ()
  (let (queries)
    (cl-letf (((symbol-function 'fanyi-dwim)
               (lambda (word) (push word queries))))
      (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-rich.html"
        (should-not queries)
        (let ((button (fanyi-test--find-action-button #'fanyi-dwim "ice cream")))
          (should button)
          ;; Native button navigation and activation should remain usable.
          (should (keymapp (button-get button 'keymap)))
          (button-activate button))
        (should (equal queries '("ice cream")))))))

(ert-deftest fanyi-shr-resolves-relative-external-links ()
  (let (opened)
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url &rest _) (push url opened)))
              ((symbol-function 'browse-url-default-browser)
               (lambda (url &rest _) (push url opened)))
              ((symbol-function 'eww)
               (lambda (url &rest _) (push url opened))))
      (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-rich.html"
        (should-not opened)
        (goto-char (point-min))
        (search-forward "Usage help")
        (let ((button (button-at (match-beginning 0))))
          (should button)
          (button-activate button))
        (should (equal opened '("https://www.ldoceonline.com/help/usage")))))))

(ert-deftest fanyi-shr-does-not-change-earlier-provider-results ()
  (with-temp-buffer
    (let* ((fanyi-buffer-name (buffer-name))
           (fanyi-use-glyphs nil)
           (request (fanyi-make-request fanyi-longman-provider "sample"))
           (result (fanyi-parse-response
                    request (fanyi-test--read-fixture "longman-rich.html")))
           (prefix (propertize "# Other dictionary\n\nKeep this text.\n\n"
                               'face 'bold 'help-echo "original")))
      (insert prefix)
      (fanyi-render-result request result)
      (should (equal-including-properties
               (buffer-substring (point-min) (+ (point-min) (length prefix))) prefix))
      (should (string-match-p "# Longman" (buffer-string))))))

(ert-deftest fanyi-shr-does-not-repeat-nested-dictionary-sections ()
  (let ((dom (fanyi-test--shr-html
              (concat "<div class='dictionary'><div class='dictentry'>"
                      "<span class='DEF'>A small amount.</span>"
                      "<div class='wordfams'><span class='asset_intro'>Word family</span>"
                      "<a>sampler</a></div>"
                      "<div class='etym'>From an older word.</div>"
                      "</div></div>"))))
    (fanyi-test--with-rendered-response fanyi-longman-provider dom
      (dolist (text '("A small amount." "Word family" "sampler" "From an older word."))
        (should (= (how-many (regexp-quote text) (point-min) (point-max)) 1)))
      (should (equal (fanyi-test--button-data #'fanyi-dwim) '("sampler"))))))

(ert-deftest fanyi-shr-separates-semantic-fields-without-splitting-words ()
  (let ((dom (fanyi-test--shr-html
              (concat "<div class='dictionary'><div class='dictentry'><span class='Sense'>"
                      "<span class='SIGNPOST'>FOOD</span>"
                      "<span class='LEXUNIT'>small sample</span>"
                      "<span class='DEF'>A pre<span>fix</span>ed word with "
                      "<span>in</span><span>line</span> markup.</span>"
                      "<span class='SYN'><span class='EXPR'>specimen</span></span>"
                      "<span class='Crossref'><a href='/dictionary/example'>example</a></span>"
                      "</span></div></div>"))))
    (fanyi-test--with-rendered-response fanyi-longman-provider dom
      (let ((text (fanyi-test--rendered-text)))
        (dolist (expected '("FOOD small sample A prefixed word with inline markup."
                            "markup. SYN specimen example"))
          (should (string-match-p (regexp-quote expected) text)))))))

(ert-deftest fanyi-shr-audio-action-does-not-depend-on-source-tag ()
  (let ((dom (fanyi-test--shr-html
              (concat "<div class='dictionary'><div class='dictentry'>"
                      "<span class='DEF'>A small amount.</span>"
                      "<button class='speaker' data-src-mp3='/media/button.mp3'>Listen</button>"
                      "</div></div>")))
        played)
    (cl-letf (((symbol-function 'fanyi-play-sound)
               (lambda (url) (push url played))))
      (fanyi-test--with-rendered-response fanyi-longman-provider dom
        (should-not played)
        (let ((button (fanyi-test--find-action-button
                       #'fanyi-play-sound "https://www.ldoceonline.com/media/button.mp3")))
          (should button)
          (button-activate button))
        (should (equal played '("https://www.ldoceonline.com/media/button.mp3")))))))

(ert-deftest fanyi-shr-dragon-heading-headword-and-labels ()
  (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-dragon.html"
    (should (string-match-p "^## From Longman Dictionary of Contemporary English$"
                            (buffer-string)))
    (should (string-match-p "^drag‧on /ˈdræɡən/ ●●○ noun \\[countable\\][ \t]*$"
                            (buffer-string)))
    (should (string-match-p "BAD-TEMPERED" (buffer-string)))
    (dolist (removed '("dragon drag‧on" "(noun)" "RF"))
      (should-not (string-match-p (regexp-quote removed) (buffer-string))))))

(ert-deftest fanyi-shr-dragon-topic-and-cross-reference-actions ()
  (dolist (path '("/topic/folklore" "/Folklore-topic/"))
    (let ((dom (fanyi-test--read-fixture "longman-dragon.html"))
          (url (concat "https://www.ldoceonline.com" path))
          topics queries)
      (dom-set-attribute (car (dom-by-class dom "^topic$")) 'href path)
      (cl-letf (((symbol-function 'eww)
                 (lambda (url &rest _) (push url topics)))
                ((symbol-function 'fanyi-dwim)
                 (lambda (word) (push word queries))))
        (fanyi-test--with-rendered-response fanyi-longman-provider dom
          (fanyi-test--fontify)
          (should-not topics)
          (should-not queries)
          (let ((topic (fanyi-test--find-action-button #'eww url))
                (crossref (fanyi-test--find-action-button #'fanyi-dwim "chase-the-dragon")))
            (should topic)
            (should crossref)
            (should (string-match-p "→ chase the dragon" (buffer-string)))
            (button-activate topic)
            (button-activate crossref))
          (should (equal topics (list url)))
          (should (equal queries '("chase-the-dragon"))))))))

(ert-deftest fanyi-shr-dragon-highlighting-survives-font-lock ()
  (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-dragon.html"
    (fanyi-test--fontify)
    (dolist (field '(("## From Longman" . fanyi-sub-headline-face)
                     ("noun" . fanyi-pos-face)
                     ("[countable]" . fanyi-longman-grammar-face)
                     ("BAD-TEMPERED" . fanyi-longman-signpost-face)
                     ("There, a little way along" . fanyi-longman-example-face)))
      (goto-char (point-min))
      (search-forward (car field))
      (should (fanyi-test--has-face-p (match-beginning 0) (cdr field))))))

(ert-deftest fanyi-shr-dragon-corpus-examples-have-separate-lines ()
  (let ((shr-width 1000))
    (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-dragon.html"
      (goto-char (point-min))
      (re-search-forward "^## Examples from the Corpus dragon$")
      (let ((start (line-beginning-position 2)))
        (re-search-forward "^\\(?:## \\|Origin dragon\\)")
        (should
         (equal (split-string (buffer-substring-no-properties start (line-beginning-position))
                              "\n[ \t]*" t "[ \t]+")
                '("• There, a little way along, a dragon had been painted on the wall in green."
                  "• With the Sun in its present state of inactivity there are unlikely to be any dragons worth chasing."
                  "• Anyone between five and 11 can enter, and the task is to draw, paint or model a green dragon."
                  "• Is there anything in the dressing-up box that looks like dragon skin?"
                  "• Now the dragons are few and those that are left slumber deeply."
                  "• As the strength of the dragons waned so did the power of the Dragon Princes."
                  "• Word arrived from Caledor that the dragons had been roused.")))))))

(ert-deftest fanyi-shr-dragon-origin-is-one-paragraph-without-glyphs ()
  (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-dragon.html"
    (goto-char (point-min))
    (re-search-forward "^Origin dragon (1200-1300) Old French Greek drakon “large snake”[ \t]*$")
    (should-not (get-text-property (match-beginning 0) 'display))))

(ert-deftest fanyi-shr-origin-keeps-emphasis-after-font-lock ()
  (let ((dom (fanyi-test--read-fixture "longman-dragon.html")))
    (setcar (car (dom-by-class dom "Etymon")) 'em)
    (setcar (cadr (dom-by-class dom "LANG")) 'strong)
    (fanyi-test--with-rendered-response fanyi-longman-provider dom
      (fanyi-test--fontify)
      (goto-char (point-min))
      (re-search-forward "^Origin dragon (1200-1300) Old French Greek drakon “large snake”[ \t]*$")
      (dolist (field '(("Greek" . bold) ("drakon" . italic)))
        (goto-char (point-min))
        (search-forward (car field))
        (should (fanyi-test--has-face-p (match-beginning 0) (cdr field)))))))

(ert-deftest fanyi-shr-dragon-origin-svg-and-unavailable-svg-fallback ()
  (dolist (svg-available '(t nil))
    (let (labels)
      (cl-letf (((symbol-function 'fanyi-display-glyphs-p) (lambda () t))
                ((symbol-function 'image-type-available-p)
                 (lambda (type) (and svg-available (eq type 'svg))))
                ((symbol-function 'fanyi-longman-svg-tag-make)
                 (lambda (text face)
                   (push (cons text face) labels)
                   '(image :type svg :data "<svg>Origin</svg>"))))
        (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-dragon.html"
          (goto-char (point-min))
          (re-search-forward "^Origin dragon (1200-1300) Old French Greek drakon “large snake”[ \t]*$")
          (let ((display (get-text-property (match-beginning 0) 'display)))
            (if svg-available
                (progn
                  (should (member '("Origin" . fanyi-longman-svg-asset-face) labels))
                  (should (eq (car-safe display) 'image))
                  (should (eq (plist-get (cdr display) :type) 'svg)))
              (should-not labels)
              (should-not display))))))))

(provide 'fanyi-shr-tests)
;;; fanyi-shr-tests.el ends here
