;;; fanyi-longman-style-tests.el --- Saved Longman page presentation -*- lexical-binding: t; -*-

(require 'fanyi-test-support)
(require 'fanyi-longman)

(ert-deftest fanyi-longman-saved-page-keeps-source-and-renders-real-structure ()
  (let* ((dom (fanyi-test--read-fixture "longman-dragon-saved.html"))
         (before (copy-tree dom))
         (shr-width 1000))
    (fanyi-test--with-rendered-response fanyi-longman-provider dom
      ;; The saved page uses nested asset/yellow_box and assetlink/exaGroup
      ;; siblings, and an ACTIV label hidden by CSS, unlike the synthetic page.
      (should (equal dom before))
      (should (dom-by-class dom "^ACTIV$"))
      (should (= (length (dom-by-class dom "^HWD$")) 2))
      (should-not (dom-by-class result "^ACTIV$"))
      ;; The headword duplicate is removed, while Origin retains its HWD.
      (should (= (length (dom-by-class result "^HWD$")) 1))
      (should (string-match-p "^## From Longman Dictionary of Contemporary English$"
                              (buffer-string)))
      (should (string-match-p "^## Examples from the Corpus dragon$" (buffer-string)))
      (should (= (how-many "^[ \t]*• " (point-min) (point-max)) 7))
      (should (string-match-p "drag‧on /ˈdræɡən/ ●●○ noun \\[countable\\]"
                              (buffer-string)))
      (should (string-match-p "Origin dragon (1200-1300) Old French Greek drakon “large snake”"
                              (buffer-string)))
      ;; This page marks BAD-TEMPERED as hidden ACTIV, not a visible SIGNPOST.
      (dolist (hidden '("BAD-TEMPERED" "RF" "dragondrag‧on" "dragon drag‧on"))
        (should-not (string-match-p (regexp-quote hidden) (buffer-string)))))))

(ert-deftest fanyi-longman-saved-page-styles-survive-real-font-lock ()
  (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-dragon-saved.html"
    (fanyi-test--fontify)
    (dolist (field '(("drag‧on" . fanyi-longman-headword-face)
                     ("noun" . fanyi-longman-pos-face)
                     ("noun" . fanyi-pos-face)
                     ("[countable]" . fanyi-longman-grammar-face)
                     ("There, a little way along" . fanyi-longman-example-face)))
      (should (fanyi-test--longman-style-has-face-p
               (fanyi-test--longman-style-face (car field)) (cdr field))))
    (should (= (face-attribute 'fanyi-longman-headword-face :height nil t) 1.6))
    (should (equal (face-attribute 'fanyi-longman-grammar-face :foreground nil t) "#008000"))
    (should (equal (face-attribute 'fanyi-longman-example-face :foreground nil t) "#808080"))
    (goto-char (point-min))
    (search-forward "## Examples from the Corpus dragon")
    (should (eq (get-text-property (match-beginning 0) 'keymap) fanyi-mode-cycle-map))))

(ert-deftest fanyi-longman-saved-page-emphasis-and-link-actions ()
  (let (queries topics)
    (cl-letf (((symbol-function 'fanyi-dwim) (lambda (word) (push word queries)))
              ((symbol-function 'eww) (lambda (url &rest _) (push url topics))))
      (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-dragon-saved.html"
        (fanyi-test--fontify)
        (should (fanyi-test--longman-style-has-face-p
                 (fanyi-test--longman-style-face "dragon had been") 'bold))
        (let ((face (fanyi-test--longman-style-face "imaginary")))
          (should (fanyi-test--longman-style-has-face-p face 'fanyi-longman-definition-link-face))
          (should-not (fanyi-test--longman-style-has-face-p face 'shr-link)))
        (dolist (text '("imaginary" "chase the dragon" "Folklore"))
          (let ((button (fanyi-test--longman-style-button text)))
            (should button)
            (button-activate button)))
        (should (equal (nreverse queries) '("imaginary" "chase the dragon")))
        (should (equal topics '("https://www.ldoceonline.com/Folklore-topic/")))))))

(ert-deftest fanyi-longman-class-styles-preserve-native-tags-and-links ()
  (let* ((dom (fanyi-test--read-fixture "longman-dragon-saved.html"))
         (word (car (dom-by-class dom "^HYPHENATION$")))
         (link (car (dom-by-class dom "^defRef$")))
         queries)
    ;; Style-bearing nodes still retain their native HTML behavior.
    (setcar word 'em)
    (dom-set-attribute link 'class "defRef GRAM")
    (setcdr (cdr link) '((strong nil "imaginary")))
    (cl-letf (((symbol-function 'fanyi-dwim) (lambda (query) (push query queries))))
      (fanyi-test--with-rendered-response fanyi-longman-provider dom
        (fanyi-test--fontify)
        (let ((face (fanyi-test--longman-style-face "drag‧on")))
          (should (fanyi-test--longman-style-has-face-p face 'italic))
          (should (fanyi-test--longman-style-has-face-p face 'fanyi-longman-headword-face)))
        (let ((face (fanyi-test--longman-style-face "imaginary"))
              (button (fanyi-test--longman-style-button "imaginary")))
          (should (fanyi-test--longman-style-has-face-p face 'bold))
          (should (fanyi-test--longman-style-has-face-p face 'fanyi-longman-grammar-face))
          (should button)
          (button-activate button))
        (should (equal queries '("imaginary")))))))

(ert-deftest fanyi-longman-class-rule-order-controls-conflicting-styles ()
  (dolist (classes '("first second" "second first"))
    (let* ((fanyi-longman--class-styles
            '(("first" :tag fanyi-line :face fanyi-longman-grammar-face)
              ("second" :tag fanyi-heading :face fanyi-longman-headword-face :indent 2)))
           (rules (copy-tree fanyi-longman--class-styles))
           (dom `(div ((class . "dictionary"))
                      (div ((class . "dictentry"))
                           (span ((class . "DEF")) "before"
                                 (span ((class . ,classes)) "label") "after"))))
           (original (copy-tree dom)))
      (fanyi-test--with-rendered-response fanyi-longman-provider dom
        (fanyi-test--fontify)
        ;; The first rule supplies the tag and face; the second still supplies
        ;; its independent indentation, regardless of the HTML class order.
        (should (string-match-p "^  label$" (buffer-string)))
        (should-not (string-match-p "## label" (buffer-string)))
        (let ((face (fanyi-test--longman-style-face "label")))
          (should (fanyi-test--longman-style-has-face-p face 'fanyi-longman-grammar-face))
          (should-not (fanyi-test--longman-style-has-face-p face 'fanyi-longman-headword-face)))
        (should (equal dom original))
        (should (equal fanyi-longman--class-styles rules))))))

(ert-deftest fanyi-longman-explicit-nil-rule-values-preserve-native-inline-content ()
  (let* ((fanyi-longman--class-styles
          '(("first" :tag nil :face nil :space nil)
            ("second" :tag fanyi-line :face bold :space t)))
         (dom '(div ((class . "dictionary"))
                    (div ((class . "dictentry"))
                         (span ((class . "DEF")) "before"
                               (em ((class . "second first")) "label") "after")))))
    (fanyi-test--with-rendered-response fanyi-longman-provider dom
      (fanyi-test--fontify)
      (should (string-match-p "beforelabelafter" (buffer-string)))
      (let ((face (fanyi-test--longman-style-face "label")))
        (should (fanyi-test--longman-style-has-face-p face 'italic))
        (should-not (fanyi-test--longman-style-has-face-p face 'bold))))))

(ert-deftest fanyi-longman-context-overrides-keep-original-tags-for-origin-and-notes ()
  (let ((dom '(div ((class . "dictionary"))
                   (div ((class . "dictentry"))
                        (span ((class . "DEF")) "Definition.")
                        (span ((class . "F2NBox"))
                              (span ((class . "heading EXPL")) "Register") " inline qualifier"
                              (span ((class . "EXPL")) "Note body.")))
                   (div ((class . "etym"))
                        (span ((class . "asset_intro")) "Origin")
                        (span ((class . "HWD")) "term") " "
                        (span ((class . "Sense")) "root phrase") " "
                        (div ((class . "EXAMPLE")) "origin example")))))
    (fanyi-test--with-rendered-response fanyi-longman-provider dom
      (fanyi-test--fontify)
      ;; Context intentionally bypasses class tags here: origin flattens the
      ;; original HTML and note headings change only the semantic face.
      (should (string-match-p "^Origin term root phrase origin example$" (buffer-string)))
      (should (string-match-p "^[ \t]*Register inline qualifier$" (buffer-string)))
      (should (string-match-p "^[ \t]*Note body\\.$" (buffer-string)))
      (let ((face (fanyi-test--longman-style-face "Register")))
        (should (fanyi-test--longman-style-has-face-p face 'fanyi-longman-note-heading-face))
        (should-not (fanyi-test--longman-style-has-face-p face 'fanyi-longman-box-heading-face))))))

(provide 'fanyi-longman-style-tests)
;;; fanyi-longman-style-tests.el ends here
