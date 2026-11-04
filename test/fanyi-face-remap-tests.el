;;; fanyi-face-remap-tests.el --- Local image face remapping -*- lexical-binding: t; -*-

(require 'fanyi-test-support)
(require 'fanyi-longman)
(require 'fanyi-haici)

(defface fanyi-remap-test-label
  '((t :foreground "#102030" :background "#fafafa" :weight normal :box nil))
  "Face for testing local image style resolution.")
(defface fanyi-remap-test-parent
  '((t :foreground "#405060" :background "#eeeeee"))
  "Parent face for testing local image style resolution.")

(ert-deftest fanyi-face-remap-honors-relative-base-inheritance-and-removal ()
  (with-temp-buffer
    (let ((global (face-all-attributes 'fanyi-remap-test-label)))
      (should (equal (fanyi-face-attribute 'fanyi-remap-test-label :foreground) "#102030"))
      (let ((cookie (face-remap-add-relative 'fanyi-remap-test-label
                                             :foreground "#123abc" :box nil)))
        (should (equal (fanyi-face-attribute 'fanyi-remap-test-label :foreground) "#123abc"))
        (should-not (fanyi-face-attribute 'fanyi-remap-test-label :box))
        (face-remap-remove-relative cookie)
        (should (equal (fanyi-face-attribute 'fanyi-remap-test-label :foreground) "#102030")))
      (face-remap-set-base 'fanyi-remap-test-label :inherit 'fanyi-remap-test-parent)
      (should (equal (fanyi-face-attribute 'fanyi-remap-test-label :foreground) "#405060"))
      (face-remap-add-relative 'fanyi-remap-test-parent :foreground "#987654")
      (should (equal (fanyi-face-attribute 'fanyi-remap-test-label :foreground) "#987654"))
      (face-remap-set-base 'fanyi-remap-test-label :weight 'bold)
      (should (eq (fanyi-face-attribute 'fanyi-remap-test-label :foreground) 'unspecified))
      (should (equal (fanyi-face-attribute 'fanyi-remap-test-label :foreground nil
                                           'fanyi-remap-test-parent) "#987654"))
      (should (eq (fanyi-face-attribute 'fanyi-remap-test-label :weight) 'bold))
      (should (equal global (face-all-attributes 'fanyi-remap-test-label)))))
  (with-temp-buffer
    (should (equal (fanyi-face-attribute 'fanyi-remap-test-label :foreground) "#102030"))))

(ert-deftest fanyi-face-remap-uses-local-default-and-longman-body-fallback ()
  (with-temp-buffer
    (face-remap-set-base 'fanyi-longman-frequency-face :weight 'bold)
    (face-remap-set-base 'fanyi-longman-body-face :foreground "#112233" :background "#ddeeff")
    (face-remap-set-base 'default :foreground "#778899" :background "#f5f5f5")
    (should (equal (fanyi-longman--label-color 'fanyi-longman-frequency-face :foreground)
                   "#112233"))
    (should (equal (fanyi-longman--label-color 'fanyi-longman-frequency-face :background)
                   "#ddeeff"))
    (face-remap-set-base 'fanyi-longman-body-face :weight 'normal)
    (should (equal (fanyi-longman--label-color 'fanyi-longman-frequency-face :foreground)
                   "#778899"))
    (should (equal (fanyi-longman--label-color 'fanyi-longman-frequency-face :background)
                   "#f5f5f5"))
    (face-remap-set-base 'fanyi-haici-label-face :box nil)
    (should (equal (fanyi-face-attribute 'fanyi-haici-label-face :foreground nil 'default)
                   "#778899"))))

(ert-deftest fanyi-face-remap-handles-named-cycles-without-recursing-in-lisp ()
  (with-temp-buffer
    ;; A remapping can refer to its own global definition as its base, or to
    ;; another remapped face.  Emacs's native merge detects these cycles.
    (setq-local face-remapping-alist
                '((fanyi-remap-test-label fanyi-remap-test-parent)
                  (fanyi-remap-test-parent fanyi-remap-test-label)))
    (should (equal (fanyi-face-attribute 'fanyi-remap-test-label :foreground) "#102030"))))

(ert-deftest fanyi-face-remap-filters-follow-the-rendering-window ()
  (with-temp-buffer
    (save-window-excursion
      (delete-other-windows)
      (set-window-buffer nil (current-buffer))
      (let* ((first (selected-window))
             (second (split-window-right))
             (before (current-buffer))
             (face-remapping-alist
              '((fanyi-remap-test-label
                 (:filtered (:window fanyi-remap-test t) (:foreground "#abcdef"))
                 fanyi-remap-test-label))))
        (set-window-parameter second 'fanyi-remap-test t)
        (unwind-protect
            (progn
              (should (equal (fanyi-face-attribute 'fanyi-remap-test-label :foreground first)
                             "#102030"))
              (should (equal (fanyi-face-attribute 'fanyi-remap-test-label :foreground second)
                             "#abcdef"))
              (should (equal (fanyi-face-attribute 'fanyi-remap-test-label :foreground
                                                   (window-frame first)) "#102030"))
              ;; Native window filters distinguish a missing parameter from
              ;; one explicitly set to nil (xfaces.c's evaluate_face_filter).
              (let* ((parameter (make-symbol "fanyi-filter-nil"))
                     (face-remapping-alist
                      `((fanyi-remap-test-label
                         (:filtered (:window ,parameter nil) (:foreground "#fedcba"))
                         fanyi-remap-test-label))))
                (should (equal (fanyi-face-attribute 'fanyi-remap-test-label :foreground second)
                               "#102030"))
                (set-window-parameter second parameter nil)
                (should (equal (fanyi-face-attribute 'fanyi-remap-test-label :foreground second)
                               "#fedcba")))
              (should (eq (current-buffer) before))
              (should (eq (selected-window) first)))
          (set-window-parameter second 'fanyi-remap-test nil))))))

(defun fanyi-face-remap-test--svg (label)
  "Return the SVG image data at LABEL in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward label)
    (plist-get (cdr (get-text-property (match-beginning 0) 'display)) :data)))

(ert-deftest fanyi-face-remap-refreshes-longman-svg-without-changing-global-faces ()
  (let ((global (face-all-attributes 'fanyi-longman-frequency-face))
        (render (symbol-function 'fanyi-shr--render))
        (renders 0))
    (cl-letf (((symbol-function 'create-image) #'fanyi-test--create-svg-image)
              ((symbol-function 'display-images-p) (lambda (&rest _) t))
              ((symbol-function 'image-type-available-p) (lambda (type) (eq type 'svg)))
              ((symbol-function 'fanyi-shr--render)
               (lambda (&rest args) (cl-incf renders) (apply render args))))
      (fanyi-test--with-result-buffer
        (save-window-excursion
          (set-window-buffer nil (current-buffer))
          (let ((inhibit-read-only t) (fanyi-use-glyphs t) (shr-width 80))
            (fanyi-longman-render '(div nil (fanyi-label ((class . "FREQ")) "S2"))
                                  (fanyi-make-request fanyi-longman-provider "sample"))
            (let* ((original (fanyi-face-remap-test--svg "S2"))
                   (cookie (face-remap-add-relative 'fanyi-longman-frequency-face
                                                    :foreground "#123abc" :weight 'bold
                                                    :box '(:line-width 1 :color "#321cba"))))
              (run-hooks 'post-command-hook)
              (should (= renders 2))
              (should (string-match-p "#123abc" (fanyi-face-remap-test--svg "S2")))
              (should (string-match-p "#321cba" (fanyi-face-remap-test--svg "S2")))
              (run-hooks 'post-command-hook)
              (should (= renders 2))
              (face-remap-remove-relative cookie)
              (run-hooks 'post-command-hook)
              (should (= renders 3))
              (should (equal original (fanyi-face-remap-test--svg "S2"))))))))
    (should (equal global (face-all-attributes 'fanyi-longman-frequency-face)))))

(ert-deftest fanyi-face-remap-refreshes-haici-svg-and-both-dictionaries-speakers ()
  (cl-letf (((symbol-function 'create-image) #'fanyi-test--create-svg-image)
            ((symbol-function 'display-images-p) (lambda (&rest _) t))
            ((symbol-function 'image-type-available-p) (lambda (_) t))
            ((symbol-function 'find-image) (lambda (specs) (cons 'image (car specs)))))
    (fanyi-test--with-result-buffer
      (save-window-excursion
        (set-window-buffer nil (current-buffer))
        (let ((inhibit-read-only t) (fanyi-use-glyphs t) (shr-width 80))
          (fanyi-shr-insert '(fanyi-haici-label nil "Core")
                            '((fanyi-haici-label . fanyi-haici--shr-label))
                            #'fanyi-haici--render-style)
          (let ((original (fanyi-face-remap-test--svg "Core"))
                (cookie (face-remap-add-relative 'fanyi-haici-label-face :foreground "#654321")))
            (run-hooks 'post-command-hook)
            (should (string-match-p "#654321" (fanyi-face-remap-test--svg "Core")))
            (face-remap-remove-relative cookie)
            (run-hooks 'post-command-hook)
            (should (equal original (fanyi-face-remap-test--svg "Core"))))
          (dolist (entry '((fanyi-female-speaker-face . fanyi-haici--render-style)
                           (fanyi-male-speaker-face . fanyi-haici--render-style)
                           (fanyi-longman-british-speaker-face . fanyi-longman--render-style)
                           (fanyi-longman-american-speaker-face . fanyi-longman--render-style)))
            (let* ((before (funcall (cdr entry) (selected-window)))
                   (image (fanyi-speaker-image (car entry)))
                   (cookie (face-remap-add-relative (car entry) :foreground "#a1b2c3")))
              (should-not (equal before (funcall (cdr entry) (selected-window))))
              (should (equal (cdr (assoc "color" (plist-get (cdr (fanyi-speaker-image (car entry)))
                                                            :color-symbols))) "#a1b2c3"))
              (face-remap-remove-relative cookie)
              (should (equal before (funcall (cdr entry) (selected-window))))
              (should (equal image (fanyi-speaker-image (car entry)))))))))))

(provide 'fanyi-face-remap-tests)
;;; fanyi-face-remap-tests.el ends here
