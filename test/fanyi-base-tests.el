;;; fanyi-base-tests.el --- Audio and diagnostic tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'fanyi-base)

(defmacro fanyi-test--with-audio (&rest body)
  "Evaluate BODY with a mocked download and player."
  (declare (indent 0) (debug t))
  `(let ((fanyi-sound-player "test-player")
         (fanyi-sound-player-support-https nil)
         (fanyi-log nil)
         (fanyi-verbose nil)
         (buffer (generate-new-buffer " *fanyi-test-audio*"))
         callback played sent eof)
     (unwind-protect
         (cl-letf (((symbol-function 'url-retrieve)
                    (lambda (_url fn &rest _args)
                      (setq callback fn)
                      buffer))
                   ((symbol-function 'make-process)
                    (lambda (&rest _args) (setq played t) 'test-player))
                   ((symbol-function 'process-send-region)
                    (lambda (_proc start end)
                      (setq sent (buffer-substring-no-properties start end))))
                   ((symbol-function 'process-live-p) (lambda (_proc) t))
                   ((symbol-function 'process-send-eof)
                    (lambda (_proc) (setq eof t))))
           (with-current-buffer buffer
             (insert "\naudio bytes")
             (setq-local url-http-end-of-headers 1))
           ,@body)
       (when (buffer-live-p buffer)
         (kill-buffer buffer)))))

(ert-deftest fanyi-audio-accepts-empty-status-and-cleans-response ()
  (fanyi-test--with-audio
    (fanyi-play-sound "https://example.invalid/audio")
    (with-current-buffer buffer
      (funcall callback nil))
    (should played)
    (should (equal sent "audio bytes"))
    (should eof)
    (should-not (buffer-live-p buffer))))

(ert-deftest fanyi-audio-error-does-not-play-response ()
  (fanyi-test--with-audio
    (fanyi-play-sound "https://example.invalid/audio")
    (with-current-buffer buffer
      (funcall callback '(:error (error http 404))))
    (should-not played)
    (should-not sent)
    (should-not (buffer-live-p buffer))))

(ert-deftest fanyi-audio-player-error-cleans-response ()
  (fanyi-test--with-audio
    (fanyi-play-sound "https://example.invalid/audio")
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _args) (error "Player unavailable"))))
      (with-current-buffer buffer
        (funcall callback nil)))
    (should-not sent)
    (should-not (buffer-live-p buffer))))

(ert-deftest fanyi-audio-interrupted-transfer-cleans-player-and-response ()
  ;; Both writing audio and closing stdin can fail or be interrupted.
  (dolist (operation '(send eof))
    (dolist (failure '(error quit))
      (let ((fanyi-sound-player "test-player")
            (fanyi-sound-player-support-https nil)
            (fanyi-log nil)
            (fanyi-verbose nil)
            (buffer (generate-new-buffer " *fanyi-test-audio*"))
            callback player)
        (unwind-protect
            (cl-letf (((symbol-function 'url-retrieve)
                       (lambda (_url fn &rest _args)
                         (setq callback fn)
                         buffer))
                      ((symbol-function 'make-process)
                       (lambda (&rest _args)
                         (setq player (make-pipe-process
                                       :name "fanyi-test-player"
                                       :buffer nil :noquery t))))
                      ((symbol-function 'process-send-region)
                       (lambda (&rest _args)
                         (when (eq operation 'send)
                           (signal failure nil))))
                      ((symbol-function 'process-send-eof)
                       (lambda (&rest _args)
                         (when (eq operation 'eof)
                           (signal failure nil)))))
              (with-current-buffer buffer
                (insert "\naudio bytes")
                (setq-local url-http-end-of-headers 1))
              (fanyi-play-sound "https://example.invalid/audio")
              (condition-case nil
                  (with-current-buffer buffer
                    (funcall callback nil))
                (quit nil))
              (should (processp player))
              (should-not (process-live-p player))
              (should-not (buffer-live-p buffer)))
          (when (and player (process-live-p player))
            (delete-process player))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest fanyi-audio-start-error-is-reported ()
  (let ((fanyi-sound-player "test-player")
        (fanyi-sound-player-support-https nil)
        (fanyi-log nil)
        (fanyi-verbose t)
        report)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (&rest _args) (error "Cannot connect")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq report (apply #'format fmt args)))))
      (fanyi-play-sound "https://example.invalid/audio"))
    (should (string-match-p "Cannot connect" report))))

(ert-deftest fanyi-audio-cleans-redirect-chain ()
  (fanyi-test--with-audio
    (let ((redirected (generate-new-buffer " *fanyi-test-audio-redirect*")))
      (unwind-protect
          (progn
            (fanyi-play-sound "https://example.invalid/audio")
            (with-current-buffer buffer
              (setq-local url-redirect-buffer redirected))
            (with-current-buffer redirected
              (insert "\nredirected audio")
              (setq-local url-http-end-of-headers 1)
              (funcall callback '(:redirect "https://example.invalid/new")))
            (should (equal sent "redirected audio"))
            (should-not (buffer-live-p buffer))
            (should-not (buffer-live-p redirected)))
        (when (buffer-live-p redirected)
          (kill-buffer redirected))))))

(ert-deftest fanyi-log-formats-multiple-arguments ()
  (let ((fanyi-log t)
        (fanyi-log-buffer-name (generate-new-buffer-name " *fanyi-test-log*")))
    (unwind-protect
        (progn
          (fanyi-log "%s: %d" "failure" 500)
          (with-current-buffer fanyi-log-buffer-name
            (should (string-match-p "failure: 500\n" (buffer-string)))))
      (when (get-buffer fanyi-log-buffer-name)
        (kill-buffer fanyi-log-buffer-name)))))

(ert-deftest fanyi-user-error-formats-message-and-log ()
  (let ((fanyi-log t)
        (fanyi-verbose t)
        (fanyi-log-buffer-name (generate-new-buffer-name " *fanyi-test-log*")))
    (unwind-protect
        (let ((err (should-error (fanyi-user-error "%s: %d" "failure" 500)
                                 :type 'user-error)))
          (should (equal (error-message-string err) "failure: 500"))
          (with-current-buffer fanyi-log-buffer-name
            (should (string-match-p "failure: 500\n" (buffer-string)))))
      (when (get-buffer fanyi-log-buffer-name)
        (kill-buffer fanyi-log-buffer-name)))))

(provide 'fanyi-base-tests)
;;; fanyi-base-tests.el ends here
