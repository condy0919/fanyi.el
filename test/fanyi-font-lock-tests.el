;;; fanyi-font-lock-tests.el --- Text and HTML highlighting -*- lexical-binding: t; -*-

(require 'fanyi-test-support)

(ert-deftest fanyi-font-lock-keeps-html-literals-and-text-formatting-separate ()
  (fanyi-test--with-result-buffer
    (let ((inhibit-read-only t)
          (sample "> quote\n- list\n(noun) /slash/ *asterisk* _under_score_\n"))
      (insert "# HTML\n\n" (propertize sample 'fanyi-html t)
              "\n# Text\n\n" sample))
    (fanyi-test--fontify)
    (dolist (field '(("> quote" . fanyi-quote-face)
                     ("-" . fanyi-list-face)
                     ("(noun)" . fanyi-pos-face)
                     ("/slash/" . italic)
                     ("*asterisk*" . bold)
                     ("_under_score_" . underline)))
      (goto-char (point-min))
      (search-forward (car field))
      (should (get-text-property (match-beginning 0) 'fanyi-html))
      (should-not (get-text-property (match-beginning 0) 'face))
      (search-forward (car field))
      (should-not (get-text-property (match-beginning 0) 'fanyi-html))
      (should (eq (get-text-property (match-beginning 0) 'face) (cdr field))))))

(ert-deftest fanyi-font-lock-preserves-html-semantic-styles-and-heading-actions ()
  (fanyi-test--with-result-buffer
    (let ((inhibit-read-only t))
      (insert "# Longman\n\n"
              (propertize "## Corpus\n" 'fanyi-html t
                          'font-lock-face '(:height 1.2 :weight bold))
              (propertize "/literal/ *literal* _literal_ (noun)\n"
                          'fanyi-html t 'font-lock-face 'font-lock-string-face)
              "\n# Other dictionary\n\n## Text heading\n"))
    (fanyi-test--fontify)
    (goto-char (point-min))
    (search-forward "## Corpus")
    (let ((position (match-beginning 0)))
      (should-not (plist-get (text-properties-at position) 'face))
      (should (equal (get-text-property position 'font-lock-face)
                     '(:height 1.2 :weight bold)))
      (should (equal (get-char-property position 'face)
                     '(:height 1.2 :weight bold)))
      (should (eq (get-text-property position 'keymap) fanyi-mode-cycle-map)))
    (dolist (text '("/literal/" "*literal*" "_literal_" "(noun)"))
      (search-forward text)
      (should-not (plist-get (text-properties-at (match-beginning 0)) 'face))
      (should (eq (get-text-property (match-beginning 0) 'font-lock-face)
                  'font-lock-string-face))
      (should (eq (get-char-property (match-beginning 0) 'face)
                  'font-lock-string-face)))
    (search-forward "## Text heading")
    (should (eq (get-text-property (match-beginning 0) 'face) 'fanyi-sub-headline-face))
    (should (eq (get-text-property (match-beginning 0) 'keymap) fanyi-mode-cycle-map))
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'fanyi-dict-face))
    (should (eq (get-text-property (point) 'keymap) fanyi-mode-cycle-map))))

(ert-deftest fanyi-font-lock-does-not-style-matches-crossing-html-boundaries ()
  (fanyi-test--with-result-buffer
    (let ((inhibit-read-only t))
      (insert "/plain " (propertize "HTML" 'fanyi-html t) " plain/\n/italic/\n"))
    (fanyi-test--fontify)
    (goto-char (point-min))
    (should-not (get-text-property (point) 'face))
    (search-forward "HTML")
    (should-not (get-text-property (match-beginning 0) 'face))
    (search-forward "/italic/")
    (should (eq (get-text-property (match-beginning 0) 'face) 'italic))))

(ert-deftest fanyi-youdao-preserves-explanations-and-semantic-pos-faces ()
  (require 'fanyi-youdao-thesaurus)
  (let* ((explanation
          (concat "n. 名词；v. 动词；vt. 及物；vi. 不及物；pron. 代词；prep. 介词；"
                  "conj. 连词；abbr. 缩写；det. 限定词；adj. 形容词；adv. 副词；art. 冠词；"
                  "n.中文；(n.) 已有；abbreviation. Rev. n.example n._suffix N. 大写"))
         (result (list (cons "sample" explanation))))
    (fanyi-test--with-result-buffer
      (let ((fanyi-buffer-name (buffer-name)))
        (fanyi-youdao-thesaurus-render result nil))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     (concat "# 有道同义词词典 \n\n- sample\n" explanation "\n\n")))
      ;; Styling a rendered result must not add properties to its source text.
      (should-not (text-property-not-all 0 (length explanation) 'face nil explanation))
      (should-not (text-property-not-all 0 (length explanation) 'font-lock-face nil explanation))
      (cl-labels
          ((check-faces ()
             (goto-char (point-min))
             (dolist (label '("n." "v." "vt." "vi." "pron." "prep." "conj." "abbr."
                              "det." "adj." "adv." "art." "n.中文" "(n.)"))
               (search-forward label)
               (let ((begin (+ (match-beginning 0) (if (string-prefix-p "(" label) 1 0))))
                 (should (eq (get-char-property begin 'face) 'fanyi-pos-face))
                 (should (eq (get-text-property begin 'font-lock-face) 'fanyi-pos-face))))
             (dolist (text '("abbreviation." "Rev." "n.example" "n._suffix" "N."))
               (search-forward text)
               (cl-loop for position from (match-beginning 0) below (match-end 0)
                        do (should-not (eq (get-char-property position 'face) 'fanyi-pos-face))
                        do (should-not (get-text-property position 'font-lock-face))))))
        (check-faces)
        (fanyi-test--fontify)
        (check-faces)))))

(provide 'fanyi-font-lock-tests)
;;; fanyi-font-lock-tests.el ends here
