;;; fanyi-longman-style-tests.el --- Saved Longman page presentation -*- lexical-binding: t; -*-

(require 'fanyi-provider-tests)

(defun fanyi-test--longman-style-fontify ()
  "Refresh font-lock with the same face aliases as an interactive result."
  (fanyi-mode)
  (let ((noninteractive nil))
    (font-lock-mode 1))
  (should font-lock-mode)
  (font-lock-ensure)
  (font-lock-flush)
  (font-lock-ensure))

(defun fanyi-test--longman-style-has-face-p (value face)
  "Whether displayed face VALUE contains FACE, including combined faces."
  (or (eq value face)
      (and (consp value)
           (cl-some (lambda (part) (fanyi-test--longman-style-has-face-p part face))
                    value))))

(defun fanyi-test--longman-style-face (text)
  "Return the effective display face at the first occurrence of TEXT."
  (goto-char (point-min))
  (search-forward text)
  (get-char-property (match-beginning 0) 'face))

(defun fanyi-test--longman-style-button (text)
  "Return the button at the first occurrence of TEXT."
  (goto-char (point-min))
  (search-forward text)
  (button-at (match-beginning 0)))

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
    (fanyi-test--longman-style-fontify)
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
        (fanyi-test--longman-style-fontify)
        (should (fanyi-test--longman-style-has-face-p
                 (fanyi-test--longman-style-face "dragon had been") 'bold))
        (let ((face (fanyi-test--longman-style-face "imaginary")))
          (should (fanyi-test--longman-style-has-face-p face 'fanyi-longman-definition-link-face))
          (should-not (fanyi-test--longman-style-has-face-p face 'shr-link)))
        (dolist (text '("imaginary" "chase the dragon" "Folklore"))
          (let ((button (fanyi-test--longman-style-button text)))
            (should button)
            (button-activate button)))
        (should (equal (nreverse queries) '("imaginary" "chase-the-dragon")))
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
        (fanyi-test--longman-style-fontify)
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

(provide 'fanyi-longman-style-tests)
;;; fanyi-longman-style-tests.el ends here
