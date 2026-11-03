;;; fanyi-bookmark-tests.el --- Query bookmark identity -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'bookmark)
(require 'fanyi)

(ert-deftest fanyi-bookmarks-retain-the-query-through-custom-names-and-renaming ()
  (let ((bookmark-alist nil)
        (bookmark-save-flag nil)
        (bookmark-use-annotations nil)
        (bookmark-fringe-mark nil)
        (bookmark-alist-modification-count 0)
        (bookmark-current-bookmark nil)
        queries)
    (cl-letf (((symbol-function 'bookmark-maybe-load-default-file) #'ignore)
              ((symbol-function 'fanyi-dwim) (lambda (word) (push word queries))))
      (with-temp-buffer
        (fanyi-mode)
        (setq fanyi-current-word "explode")
        (bookmark-set "Vocabulary: explosions")
        (bookmark-handle-bookmark "Vocabulary: explosions")
        (bookmark-rename "Vocabulary: explosions" "Renamed vocabulary")
        (bookmark-handle-bookmark "Renamed vocabulary")
        ;; The explicit word survives serialization and unrelated later queries.
        (setq fanyi-current-word "sun")
        (fanyi-bookmark-jump (read (prin1-to-string (car bookmark-alist))))
        (should (equal queries '("explode" "explode" "explode")))))))

(ert-deftest fanyi-bookmarks-still-open-legacy-records ()
  (let (queried)
    (cl-letf (((symbol-function 'fanyi-dwim) (lambda (word) (setq queried word))))
      (fanyi-bookmark-jump '("dragon" (handler . fanyi-bookmark-jump)))
      (should (equal queried "dragon")))))

(provide 'fanyi-bookmark-tests)
;;; fanyi-bookmark-tests.el ends here
