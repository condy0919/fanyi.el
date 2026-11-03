;;; fanyi-config-tests.el --- Provider configuration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'fanyi-tests)

(defvar fanyi-youdao-thesaurus-max-count)

(defconst fanyi-test-first-provider
  (fanyi-test-service :word "dummy" :url "https://first.invalid/%s"
                      :api-type 'json))
(defconst fanyi-test-second-provider
  (fanyi-test-service :word "dummy" :url "https://second.invalid/%s"
                      :api-type 'json))

(defconst fanyi-test--project-directory
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(ert-deftest fanyi-config-setq-changes-enabled-providers ()
  (fanyi-test--with-query
    (let (urls)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (url &rest args)
                   (push url urls)
                   (apply #'fanyi-test--retrieve url args))))
        (setq fanyi-providers '(fanyi-test-first-provider))
        (fanyi-dwim "First")
        (setq fanyi-providers '(fanyi-test-second-provider))
        (fanyi-dwim "Second"))
      (should (equal (reverse urls)
                     '("https://first.invalid/first"
                       "https://second.invalid/second")))
      (should (equal fanyi-providers '(fanyi-test-second-provider)))
      (should (= (length fanyi--tasks) 1))
      (should-not (eq (fanyi--task-instance (car fanyi--tasks))
                      fanyi-test-second-provider))
      (should (equal (oref fanyi-test-first-provider word) "dummy"))
      (should (equal (oref fanyi-test-second-provider word) "dummy")))))

(ert-deftest fanyi-config-customize-keeps-provider-symbols ()
  (fanyi-test--with-query
    (let ((properties (copy-sequence (symbol-plist 'fanyi-providers))))
      (unwind-protect
          (progn
            (customize-set-variable 'fanyi-providers
                                    '(fanyi-test-first-provider
                                      fanyi-test-second-provider))
            (should (equal fanyi-providers
                           '(fanyi-test-first-provider
                             fanyi-test-second-provider)))
            (fanyi-dwim "word")
            (should (= (length fanyi-test--requests) 2)))
        (setplist 'fanyi-providers properties)))))

(ert-deftest fanyi-config-accepts-existing-service-instances ()
  (fanyi-test--with-query
    (let ((template (car fanyi-providers)))
      (fanyi-dwim "First")
      (let ((first-instance (fanyi--task-instance (car fanyi--tasks))))
        (fanyi-dwim "Second")
        (should-not (eq first-instance
                        (fanyi--task-instance (car fanyi--tasks)))))
      (should-not (eq template (fanyi--task-instance (car fanyi--tasks))))
      (should (equal (oref template word) "dummy"))
      (should (equal (oref (fanyi--task-instance (car fanyi--tasks)) word)
                     "second")))))

(ert-deftest fanyi-config-before-load-is-lazy-and-preserves-highlighting ()
  ;; A fresh Emacs proves neither loading fanyi nor setting its configuration
  ;; requires an unused provider.  It also exercises first-query highlighting.
  (with-temp-buffer
    (let* ((form
            '(progn
               (setq fanyi-providers '(fanyi-haici-provider))
               (require 'fanyi)
               (cl-assert (equal fanyi-providers '(fanyi-haici-provider)))
               (cl-assert (not (featurep 'fanyi-haici)))
               (cl-assert (not (featurep 'fanyi-longman)))
               (let ((fanyi-auto-select nil)
                     (fanyi-request-timeout nil)
                     urls)
                 (cl-letf (((symbol-function 'url-retrieve)
                            (lambda (url &rest _)
                              (push url urls)
                              (generate-new-buffer " *fanyi-config-response*")))
                           ((symbol-function 'display-buffer) #'ignore))
                   (fanyi-dwim "Sample"))
                 (cl-assert (equal urls '("https://dict.cn/sample")))
                 (cl-assert (featurep 'fanyi-haici))
                 (cl-assert (not (featurep 'fanyi-longman)))
                 (with-current-buffer fanyi-buffer-name
                   (let ((inhibit-read-only t)) (insert "★"))
                   (font-lock-ensure)
                   (cl-assert (eq (get-text-property (point-min) 'face)
                                  'fanyi-haici-star-face))))))
           (status
            (call-process
             (expand-file-name invocation-name invocation-directory)
             nil t nil "-Q" "--batch"
             "-L" fanyi-test--project-directory
             "-L" (file-name-directory (locate-library "s"))
             "--eval" (prin1-to-string form))))
      (ert-info ((buffer-string))
        (should (equal status 0))))))

(ert-deftest fanyi-config-youdao-count-changes-without-reloading ()
  (require 'fanyi-youdao-thesaurus)
  (fanyi-test--with-query
    (let ((fanyi-youdao-thesaurus-max-count 3)
          (template (symbol-value 'fanyi-youdao-thesaurus-provider))
          urls)
      (let ((original-url (oref template url)))
        (setq fanyi-providers '(fanyi-youdao-thesaurus-provider))
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (url &rest args)
                     (push url urls)
                     (apply #'fanyi-test--retrieve url args))))
          (fanyi-dwim "word")
          (setq fanyi-youdao-thesaurus-max-count 7)
          (fanyi-dwim "word"))
        (should (equal (reverse urls)
                       '("https://dict.youdao.com/suggest?q=word&doctype=json&num=3"
                         "https://dict.youdao.com/suggest?q=word&doctype=json&num=7")))
        (should (equal (oref template url) original-url))
        (should (equal (oref template word) "dummy"))))))

(ert-deftest fanyi-config-youdao-count-preserves-custom-endpoint ()
  (require 'fanyi-youdao-thesaurus)
  (let ((instance (fanyi-youdao-thesaurus-service
                   :word "dummy"
                   :url "https://proxy.invalid/suggest?q=%s&num=2&mode=brief#details"
                   :api-type 'json))
        (fanyi-youdao-thesaurus-max-count 7))
    (fanyi-set-query-word instance "first word")
    (should (equal (format (oref instance url) (oref instance word))
                   "https://proxy.invalid/suggest?q=first%20word&num=7&mode=brief#details"))
    (setq fanyi-youdao-thesaurus-max-count 9)
    (fanyi-set-query-word instance "second")
    (should (equal (format (oref instance url) (oref instance word))
                   "https://proxy.invalid/suggest?q=second&num=9&mode=brief#details"))))

(ert-deftest fanyi-config-eldoc-keeps-help-local-and-preserves-global-fallback ()
  (let* ((global-functions (list (lambda (_callback) "Other documentation")))
         (eldoc-documentation-functions global-functions)
         (eldoc-documentation-strategy #'eldoc-documentation-default)
         displayed
         (eldoc-display-functions
          (list (lambda (docs _interactive) (setq displayed (mapcar #'car docs))))))
    (with-temp-buffer
      (insert (propertize "🔊" 'help-echo "Play British pronunciation") " plain")
      (fanyi-mode)
      (goto-char (point-min))
      (should (local-variable-p 'eldoc-documentation-functions))
      (eldoc-print-current-symbol-info t)
      (should (equal displayed '("Play British pronunciation")))
      (goto-char (point-max))
      (eldoc-print-current-symbol-info t)
      (should (equal displayed '("Other documentation"))))
    (should (eq eldoc-documentation-functions global-functions))))

(provide 'fanyi-config-tests)
;;; fanyi-config-tests.el ends here
