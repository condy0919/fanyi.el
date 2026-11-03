;;; fanyi-shr-tests.el --- Longman HTML rendering tests -*- lexical-binding: t; -*-

(require 'fanyi-provider-tests)
(require 'shr)

(defun fanyi-test--rendered-text ()
  "Return current buffer text with insignificant whitespace normalized."
  (replace-regexp-in-string "[ \t\r\n]+" " " (buffer-string)))

(defun fanyi-test--shr-html (html)
  "Parse compact HTML used by the shr regression tests."
  (with-temp-buffer
    (insert html)
    (libxml-parse-html-region (point-min) (point-max))))

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
           (instance (clone fanyi-longman-provider))
           (prefix (propertize "# Other dictionary\n\nKeep this text.\n\n"
                               'face 'bold 'help-echo "original")))
      (insert prefix)
      (fanyi-set-query-word instance "sample")
      (fanyi-parse-from instance (fanyi-test--read-fixture "longman-rich.html"))
      (fanyi-render instance)
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

(provide 'fanyi-shr-tests)
;;; fanyi-shr-tests.el ends here
