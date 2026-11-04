;;; fanyi-org-tests.el --- Org link registration and derived modes -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'loaddefs-gen)
(require 'ol-fanyi)

(define-derived-mode fanyi-test-org-mode fanyi-mode "Fanyi Org test")

(ert-deftest fanyi-org-stores-links-in-derived-result-modes ()
  (with-temp-buffer
    (fanyi-test-org-mode)
    (setq fanyi-current-word "Ice Cream")
    (let (properties)
      (cl-letf (((symbol-function 'org-link-store-props)
                 (lambda (&rest values) (setq properties values))))
        (fanyi-org-store-link))
      (should (equal (plist-get properties :link) "fanyi:Ice Cream"))
      (should (equal (plist-get properties :description) "Ice Cream")))))

(ert-deftest fanyi-org-generated-autoloads-register-in-either-load-order ()
  (let* ((source (file-name-directory (locate-library "ol-fanyi")))
         (directory (make-temp-file "fanyi-org-autoload-" t))
         (autoloads (expand-file-name "fanyi-autoloads.el" directory)))
    (unwind-protect
        (progn
          (copy-file (expand-file-name "ol-fanyi.el" source)
                     (expand-file-name "ol-fanyi.el" directory))
          (loaddefs-generate directory autoloads)
          (dolist (org-first '(nil t))
            (ert-info ((format "Load Org first: %S" org-first))
              (with-temp-buffer
                (let ((form
                       `(progn
                          (require 'ert)
                          (when ,org-first (require 'ol))
                          (load ,autoloads nil t)
                          (require 'ol)
                          ;; Registration alone does not load dictionary code.
                          (should-not (featurep 'fanyi))
                          (should-not (featurep 'ol-fanyi))
                          (should (autoloadp (symbol-function 'fanyi-org-open-link)))
                          (should (eq (org-link-get-parameter "fanyi" :follow)
                                      'fanyi-org-open-link))
                          (require 'fanyi)
                          (let (queried)
                            (cl-letf (((symbol-function 'fanyi-dwim)
                                       (lambda (word) (setq queried word))))
                              (funcall (org-link-get-parameter "fanyi" :follow)
                                       "Ice Cream" nil))
                            (should (equal queried "Ice Cream")))
                          (should (featurep 'ol-fanyi)))))
                  (should
                   (equal 0 (call-process
                             (expand-file-name invocation-name invocation-directory)
                             nil (current-buffer) nil "-Q" "--batch"
                             "-L" source "-L" directory
                             "--eval" (prin1-to-string form)))))))))
      (delete-directory directory t))))

(provide 'fanyi-org-tests)
;;; fanyi-org-tests.el ends here
