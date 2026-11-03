;;; fanyi-config-tests.el --- Provider configuration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'fanyi-test-support)

(defvar fanyi-youdao-thesaurus-max-count)

(defconst fanyi-test-first-provider
  (fanyi-test--provider "https://first.invalid/%s"))
(defconst fanyi-test-second-provider
  (fanyi-test--provider "https://second.invalid/%s"))

(defconst fanyi-test--project-directory
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(ert-deftest fanyi-config-setq-changes-enabled-providers ()
  (fanyi-test--with-query
    (let ((first-before (copy-fanyi-provider fanyi-test-first-provider))
          (second-before (copy-fanyi-provider fanyi-test-second-provider))
          urls)
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
      (should (= (length (fanyi-test--query-state 'fanyi--tasks)) 1))
      (should (equal (fanyi-request-url (fanyi--task-request (car (fanyi-test--query-state 'fanyi--tasks))))
                     "https://second.invalid/second"))
      (should (equal fanyi-test-first-provider first-before))
      (should (equal fanyi-test-second-provider second-before)))))

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

(ert-deftest fanyi-config-requests-are-independent-of-provider-descriptor ()
  (fanyi-test--with-query
    (let* ((template (car fanyi-providers))
           (before (prin1-to-string template)))
      (fanyi-dwim "First")
      (let ((first-request (fanyi--task-request (car (fanyi-test--query-state 'fanyi--tasks)))))
        (fanyi-dwim "Second")
        (should-not (eq first-request (fanyi--task-request (car (fanyi-test--query-state 'fanyi--tasks)))))
        (should (equal (fanyi-request-word first-request) "first"))
        (should (equal (fanyi-request-url first-request) "https://example.invalid/first")))
      (should (equal (prin1-to-string template) before))
      (should (equal (fanyi-request-word (fanyi--task-request (car (fanyi-test--query-state 'fanyi--tasks))))
                     "second")))))

(ert-deftest fanyi-config-request-copies-mutable-http-settings ()
  (let* ((word (copy-sequence "Ice Cream?"))
         (headers (list (cons (copy-sequence "X-Token") (copy-sequence "original"))))
         (body (copy-sequence "payload"))
         (method (copy-sequence "POST"))
         (sound-url (copy-sequence "https://audio.invalid/%s"))
         (provider (fanyi-test--provider)))
    (setf (fanyi-provider-headers provider) headers
          (fanyi-provider-body provider) body
          (fanyi-provider-method provider) method
          (fanyi-provider-sound-url provider) sound-url)
    (let ((first (fanyi-make-request provider word))
          (second (fanyi-make-request provider word)))
      ;; Mutating strings inside an alist must not reach an active request.
      (aset word 0 ?X)
      (aset (caar headers) 0 ?Z)
      (aset (cdar headers) 0 ?X)
      (setcdr headers (list (cons "Extra" "new")))
      (aset body 0 ?X)
      (aset method 0 ?H)
      (aset sound-url 8 ?X)
      (dolist (request (list first second))
        (should (equal (fanyi-request-word request) "Ice Cream?"))
        (should (equal (fanyi-request-url request)
                       "https://example.invalid/Ice%20Cream%3F"))
        (should (equal (fanyi-request-headers request) '(("X-Token" . "original"))))
        (should (equal (fanyi-request-body request) "payload"))
        (should (equal (fanyi-request-method request) "POST"))
        (should (equal (fanyi-request-sound-url request) "https://audio.invalid/%s")))
      ;; Requests must not share their copied settings with one another.
      (aset (cdar (fanyi-request-headers first)) 0 ?Y)
      (aset (fanyi-request-body first) 0 ?Y)
      (should (equal (fanyi-request-headers second) '(("X-Token" . "original"))))
      (should (equal (fanyi-request-body second) "payload")))))

(ert-deftest fanyi-config-request-freezes-descriptor-and-callbacks ()
  (let ((provider (fanyi-test--provider)) rendered)
    (setf (fanyi-provider-parse provider) (lambda (_response _request) "old result")
          (fanyi-provider-render provider)
          (lambda (result request) (push (list 'old result (fanyi-request-url request)) rendered)))
    (let ((old (fanyi-make-request provider "first")))
      (setf (fanyi-provider-url provider) "https://changed.invalid/%s"
            (fanyi-provider-api-type provider) 'xml
            (fanyi-provider-method provider) "POST"
            (fanyi-provider-parse provider) (lambda (_response _request) "new result")
            (fanyi-provider-render provider)
            (lambda (result request) (push (list 'new result (fanyi-request-url request)) rendered)))
      (let ((new (fanyi-make-request provider "second")))
        (should (eq (fanyi-request-api-type old) 'json))
        (should (equal (fanyi-request-method old) "GET"))
        (should (eq (fanyi-request-api-type new) 'xml))
        (should (equal (fanyi-request-method new) "POST"))
        (fanyi-render-result old (fanyi-parse-response old nil))
        (fanyi-render-result new (fanyi-parse-response new nil))
        (should (equal (nreverse rendered)
                       '((old "old result" "https://example.invalid/first")
                         (new "new result" "https://changed.invalid/second"))))))))

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
             "--eval" (prin1-to-string form))))
      (ert-info ((buffer-string))
        (should (equal status 0))))))

(ert-deftest fanyi-config-youdao-count-changes-without-reloading ()
  (require 'fanyi-youdao-thesaurus)
  (fanyi-test--with-query
    (let ((fanyi-youdao-thesaurus-max-count 3)
          (template (symbol-value 'fanyi-youdao-thesaurus-provider))
          urls)
      (let ((before (prin1-to-string template)) first-request)
        (setq fanyi-providers '(fanyi-youdao-thesaurus-provider))
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (url &rest args)
                     (push url urls)
                     (apply #'fanyi-test--retrieve url args))))
          (fanyi-dwim "word")
          (setq first-request (fanyi--task-request (car (fanyi-test--query-state 'fanyi--tasks))))
          (setq fanyi-youdao-thesaurus-max-count 7)
          (fanyi-dwim "word"))
        (should (equal (reverse urls)
                       '("https://dict.youdao.com/suggest?q=word&doctype=json&num=3"
                         "https://dict.youdao.com/suggest?q=word&doctype=json&num=7")))
        (should (equal (fanyi-request-url first-request)
                       "https://dict.youdao.com/suggest?q=word&doctype=json&num=3"))
        (should (equal (prin1-to-string template) before))))))

(ert-deftest fanyi-config-youdao-count-preserves-custom-endpoint ()
  (require 'fanyi-youdao-thesaurus)
  (let ((provider (copy-fanyi-provider fanyi-youdao-thesaurus-provider))
        (fanyi-youdao-thesaurus-max-count 7))
    (setf (fanyi-provider-url provider)
          "https://proxy.invalid/suggest?q=%s&num=2&mode=brief#details")
    (let* ((before (prin1-to-string provider))
           (first (fanyi-make-request provider "first word")))
      (should (equal (fanyi-request-url first)
                     "https://proxy.invalid/suggest?q=first%20word&num=7&mode=brief#details"))
      (setq fanyi-youdao-thesaurus-max-count 9)
      (let ((second (fanyi-make-request provider "second")))
        (should (equal (fanyi-request-url second)
                       "https://proxy.invalid/suggest?q=second&num=9&mode=brief#details")))
      (should (equal (fanyi-request-url first)
                     "https://proxy.invalid/suggest?q=first%20word&num=7&mode=brief#details"))
      (should (equal (prin1-to-string provider) before)))))

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
