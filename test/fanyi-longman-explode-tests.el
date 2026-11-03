;;; fanyi-longman-explode-tests.el --- Saved explode page presentation -*- lexical-binding: t; -*-

(require 'fanyi-longman-style-tests)

(defun fanyi-test--explode-family-label ()
  "Return the start of the Word family label in the current result."
  (goto-char (point-min))
  (search-forward "Word family")
  (match-beginning 0))

(ert-deftest fanyi-longman-explode-family-label-and-query-actions ()
  (let ((shr-width 1000) queries)
    (cl-letf (((symbol-function 'fanyi-dwim) (lambda (word) (push word queries))))
      (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-explode-saved.html"
        (should (= (how-many "Word family" (point-min) (point-max)) 1))
        (should (string-match-p
                 (regexp-quote
                  (concat "Word family (noun) explosion explosive (adjective) exploding "
                          "explosive exploded unexploded (verb) explode (adverb) explosively"))
                 (buffer-string)))
        (should-not (string-match-p "^## Word family" (buffer-string)))
        (fanyi-test--longman-style-fontify)
        (should (fanyi-test--longman-style-has-face-p
                 (get-char-property (fanyi-test--explode-family-label) 'face)
                 'fanyi-longman-svg-asset-face))
        (should-not queries)
        (dolist (entry '(("explosion" . "explosion")
                         ("unexploded" . "unexploded")
                         ("explosively" . "explosive")))
          (let ((button (fanyi-test--longman-style-button (car entry))))
            (should button)
            (should (eq (button-get button 'action) #'fanyi-dwim))
            (should (equal (button-get button 'button-data) (cdr entry)))
            (button-activate button)))
        (should (equal (nreverse queries) '("explosion" "unexploded" "explosive")))))))

(ert-deftest fanyi-longman-explode-family-svg-and-styled-fallback ()
  ;; Cover enabled SVG, the user switch, missing SVG support, and a text display.
  (dolist (settings '((t t t) (nil t t) (t nil t) (t t nil)))
    (ert-info ((format "Glyphs, SVG support, image display: %S" settings))
      (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) (nth 2 settings)))
                ((symbol-function 'image-type-available-p)
                 (lambda (type) (and (nth 1 settings) (eq type 'svg)))))
        (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-explode-saved.html"
          (let ((fanyi-use-glyphs (car settings)))
            (let ((inhibit-read-only t)) (erase-buffer))
            (fanyi-render-result request result)
            (fanyi-test--longman-style-fontify)
            (let* ((position (fanyi-test--explode-family-label))
                   (display (get-char-property position 'display)))
              (should (equal (buffer-substring-no-properties position (+ position 11))
                             "Word family"))
              (should (fanyi-test--longman-style-has-face-p
                       (get-char-property position 'face) 'fanyi-longman-svg-asset-face))
              (if (cl-every #'identity settings)
                  (progn
                    (should (eq (car-safe display) 'image))
                    (should (string-match-p "Word family" (plist-get (cdr display) :data))))
                (should-not display)))))))))

(ert-deftest fanyi-longman-explode-family-refreshes-with-reflow-and-face-changes ()
  (let ((background (face-attribute 'fanyi-longman-svg-asset-face :background))
        (shr-width 74) queries)
    (unwind-protect
        (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
                  ((symbol-function 'image-type-available-p) (lambda (type) (eq type 'svg)))
                  ((symbol-function 'url-retrieve)
                   (lambda (&rest _) (ert-fail "Rendering must stay offline")))
                  ((symbol-function 'url-retrieve-synchronously)
                   (lambda (&rest _) (ert-fail "Rendering must stay offline")))
                  ((symbol-function 'url-queue-retrieve)
                   (lambda (&rest _) (ert-fail "Rendering must stay offline")))
                  ((symbol-function 'fanyi-dwim) (lambda (word) (push word queries))))
          (fanyi-test--with-rendered-fixture fanyi-longman-provider "longman-explode-saved.html"
            (let ((fanyi-use-glyphs t)
                  (prepared (copy-tree result)))
              (fanyi-mode)
              (let ((inhibit-read-only t)) (erase-buffer))
              (save-window-excursion
                (set-window-buffer nil (current-buffer))
                (fanyi-render-result request result)
                (let ((noninteractive nil)) (font-lock-mode 1))
                (font-lock-ensure)
                (cl-letf (((symbol-function 'fanyi-longman--prepare-node)
                           (lambda (&rest _) (ert-fail "Prepared DOM must be reused"))))
                  (let ((lines (count-lines (point-min) (point-max)))
                        (shr-width 44))
                    (fanyi-shr-reflow)
                    (font-lock-flush)
                    (font-lock-ensure)
                    (should (> (count-lines (point-min) (point-max)) lines))
                    (let* ((position (fanyi-test--explode-family-label))
                           (before (plist-get (cdr (get-char-property position 'display)) :data)))
                      (should (string-match-p "Word family" before))
                      (set-face-attribute 'fanyi-longman-svg-asset-face nil :background "#13579b")
                      (run-hooks 'post-command-hook)
                      (font-lock-flush)
                      (font-lock-ensure)
                      (let* ((position (fanyi-test--explode-family-label))
                             (after (plist-get (cdr (get-char-property position 'display)) :data)))
                        (should-not (equal before after))
                        (should (string-match-p "#13579b" after))
                        (should (fanyi-test--longman-style-has-face-p
                                 (get-char-property position 'face) 'fanyi-longman-svg-asset-face))))
                    (should-not queries)
                    (button-activate (fanyi-test--longman-style-button "explosion"))
                    (should (equal queries '("explosion"))))))
              (should (equal result prepared)))))
      (set-face-attribute 'fanyi-longman-svg-asset-face nil :background background))))

(provide 'fanyi-longman-explode-tests)
;;; fanyi-longman-explode-tests.el ends here
