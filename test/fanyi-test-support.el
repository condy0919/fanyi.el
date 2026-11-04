;;; fanyi-test-support.el --- Shared offline test helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared fixtures and helpers only; loading this file registers no ERT tests
;; and does not load dictionary providers.  Suites require their own providers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'fanyi)
(require 'fanyi-shr)

(defun fanyi-test--create-svg-image (data type data-p &rest properties)
  "Build an SVG image spec without requiring native image support.
Use only in tests simulating SVG availability.  Keep the real SVG generation
and XML serialization before `create-image' receives DATA and PROPERTIES."
  (should (eq type 'svg))
  (should data-p)
  (should (stringp data))
  `(image :type svg :data ,data ,@properties))

(defun fanyi-test--parse (response _request)
  "Return the definition from the test RESPONSE."
  (gethash "definition" response))

(defun fanyi-test--render (result request)
  "Insert the test RESULT for REQUEST at point in the current buffer."
  (insert (fanyi-request-word request) ": " result "\n"))

(defun fanyi-test--provider (&optional url)
  "Return a test provider descriptor using URL."
  (fanyi-provider-create :url (or url "https://example.invalid/%s")
                         :api-type 'json
                         :parse #'fanyi-test--parse
                         :render #'fanyi-test--render))

(cl-defstruct fanyi-test-request callback buffer)

(defvar fanyi-test--requests nil)
(defvar fanyi-test--buffers nil)

(defun fanyi-test--response-buffer ()
  "Create a response buffer and record it for test cleanup."
  (let ((buffer (generate-new-buffer " *fanyi-test-response*")))
    (push buffer fanyi-test--buffers)
    buffer))

(defun fanyi-test--retrieve (_url callback &rest _args)
  "Record CALLBACK without accessing the network."
  (let* ((buffer (fanyi-test--response-buffer))
         (request (make-fanyi-test-request :callback callback :buffer buffer)))
    (setq fanyi-test--requests (append fanyi-test--requests (list request)))
    buffer))

(defun fanyi-test--deliver (request status body)
  "Deliver REQUEST with STATUS and raw response BODY, returning its buffer.
Recreate a cancelled response buffer to simulate a late callback,
including one delivered by a redirected request."
  (let ((buffer (fanyi-test-request-buffer request)))
    (unless (buffer-live-p buffer)
      (setq buffer (fanyi-test--response-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "\n" body))
    (with-current-buffer buffer
      (setq-local url-http-end-of-headers 1)
      (funcall (fanyi-test-request-callback request) status))
    buffer))

(defun fanyi-test--respond (request &optional status definition)
  "Deliver REQUEST with STATUS and DEFINITION, returning its buffer."
  (fanyi-test--deliver
   request status
   (json-encode `((definition . ,(or definition "definition"))))))

(defun fanyi-test--query-state (variable)
  "Read VARIABLE from the active result buffer, or its default before a query."
  (if (buffer-live-p fanyi--active-buffer)
      (buffer-local-value variable fanyi--active-buffer)
    (default-value variable)))

(defmacro fanyi-test--with-query (&rest body)
  "Evaluate BODY with isolated query state and mocked HTTP requests."
  (declare (indent 0) (debug t))
  `(let* ((fanyi-buffer-name (generate-new-buffer-name " *fanyi-test*"))
          (fanyi-providers (list (fanyi-test--provider)))
          (fanyi-auto-select nil)
          (fanyi-history nil)
          (fanyi--query-id 0)
          (fanyi--active-buffer nil)
          (fanyi-verbose t)
          (fanyi-log nil)
          (fanyi-request-timeout 15)
          (fanyi-test--requests nil)
          (fanyi-test--buffers nil)
          (fanyi-test--initial-timers (copy-sequence timer-list)))
     (unwind-protect
         (cl-letf (((symbol-function 'url-retrieve) #'fanyi-test--retrieve)
                   ((symbol-function 'display-buffer) #'ignore))
           ,@body)
       (dolist (timer (copy-sequence timer-list))
         (unless (memq timer fanyi-test--initial-timers)
           (cancel-timer timer)))
       (dolist (buffer (cons (get-buffer fanyi-buffer-name)
                             fanyi-test--buffers))
         (when (buffer-live-p buffer)
           (let ((process (get-buffer-process buffer)))
             (when process
               (set-process-sentinel process #'ignore)
               (delete-process process)))
           (kill-buffer buffer))))))

(defconst fanyi-shr-test--text
  (mapconcat (lambda (number) (format "word%02d" number))
             (number-sequence 1 45) " "))

(defmacro fanyi-shr-test--with-window (&rest body)
  "Evaluate BODY in a temporary buffer displayed in the selected window."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (save-window-excursion
       (delete-other-windows)
       (set-window-buffer nil (current-buffer))
       (let ((shr-width nil)
             (shr-use-fonts nil))
         ,@body))))

(defun fanyi-shr-test--text ()
  "Return rendered content with layout whitespace normalized."
  (replace-regexp-in-string "[ \t\r\n]+" " " (string-trim (buffer-string))))

(defun fanyi-shr-test--max-line-width ()
  "Return the largest line width in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((width 0))
      (while (not (eobp))
        (end-of-line)
        (setq width (max width (current-column)))
        (forward-line))
      width)))

(defconst fanyi-test--fixture-directory
  (expand-file-name "fixtures"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun fanyi-test--read-fixture (file)
  "Parse the HTML or JSON fixture FILE using the real response parser."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file fanyi-test--fixture-directory))
    (if (string-suffix-p ".json" file)
        (progn (goto-char (point-min)) (json-parse-buffer))
      (libxml-parse-html-region (point-min) (point-max)))))

(defmacro fanyi-test--with-result-buffer (&rest body)
  "Evaluate BODY in a fresh Fanyi buffer initialized before any rendering."
  (declare (indent 0) (debug t))
  `(let ((fanyi-buffer-name (generate-new-buffer-name "fanyi-test-result")))
     (with-temp-buffer
       ;; A leading space would keep Font Lock disabled, unlike real results.
       (rename-buffer fanyi-buffer-name)
       (fanyi-mode)
       (setq-local fanyi-current-word "sample")
       ,@body)))

(defmacro fanyi-test--with-rendered-response (provider response &rest body)
  "Parse RESPONSE with PROVIDER and evaluate BODY in its Fanyi result buffer."
  (declare (indent 2) (debug (form form body)))
  `(let* ((fanyi-use-glyphs nil)
          (request (fanyi-make-request ,provider "sample"))
          result)
     (fanyi-test--with-result-buffer
       (setq result (fanyi-parse-response request ,response))
       (let ((inhibit-read-only t)) (fanyi-render-result request result))
       ,@body)))

(defun fanyi-test--fontify ()
  "Fontify and refontify the current result without resetting its mode or state."
  (should (derived-mode-p 'fanyi-mode))
  (unless font-lock-mode
    (let ((noninteractive nil))
      (font-lock-mode 1)))
  (should font-lock-mode)
  (font-lock-ensure)
  (font-lock-flush)
  (font-lock-ensure))

(defmacro fanyi-test--with-rendered-fixture (provider file &rest body)
  "Parse FILE with PROVIDER and evaluate BODY in its rendered result buffer."
  (declare (indent 2) (debug (form form body)))
  `(fanyi-test--with-rendered-response ,provider (fanyi-test--read-fixture ,file)
     ,@body))

(defun fanyi-test--button-data (action)
  "Return button data for buttons invoking ACTION in the current buffer."
  (let ((position (point-min)) data button)
    (while (setq button (next-button position t))
      (when (eq (button-get button 'action) action)
        (push (button-get button 'button-data) data))
      (setq position (button-end button)))
    (nreverse data)))

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

(provide 'fanyi-test-support)
;;; fanyi-test-support.el ends here
