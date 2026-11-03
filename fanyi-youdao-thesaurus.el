;;; fanyi-youdao-thesaurus.el --- Youdao thesaurus service -*- lexical-binding: t -*-

;; Copyright (C) 2021 Zhiwei Chen
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/condy0919/fanyi.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (s "1.12.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Youdao thesaurus service.

;;; Code:

(require 'fanyi-base)

(require 's)
(require 'subr-x)
(require 'cl-lib)

(defcustom fanyi-youdao-thesaurus-max-count 10
  "Max count of entries displayed."
  :type 'integer
  :group 'fanyi)

(defcustom fanyi-youdao-thesaurus-indent 2
  "Default indent for explanation."
  :type 'integer
  :group 'fanyi)

(defun fanyi-youdao-thesaurus-prepare-request (request)
  "Set the current result limit on REQUEST."
  (let* ((url (url-generic-parse-url (fanyi-request-url request)))
         (path (url-filename url))
         (limit (format "num=%d" fanyi-youdao-thesaurus-max-count)))
    ;; Preserve custom endpoints and other parameters on this request.
    (setf (url-filename url)
          (if (string-match-p "[?&]num=" path)
              (replace-regexp-in-string
               "[?&]num=[^&]*"
               (lambda (match) (concat (substring match 0 1) limit)) path t t)
            (concat path (if (string-match-p "?" path) "&" "?") limit)))
    (setf (fanyi-request-url request) (url-recreate-url url))))

(defun fanyi-youdao-thesaurus-parse (js _request)
  "Parse JS into entries, distinguishing empty results from invalid data."
  (unless (hash-table-p js)
    (signal 'fanyi-parse-error '("Youdao: expected a response object")))
  (let ((result (gethash "result" js))
        (data (gethash "data" js)))
    (unless (and (hash-table-p result) (equal (gethash "code" result) 200)
                 (hash-table-p data) (vectorp (gethash "entries" data)))
      (signal 'fanyi-parse-error '("Youdao: missing or invalid result entries")))
    (let ((entries (gethash "entries" data)))
      (when (= (length entries) 0)
        (throw 'not-found nil))
      (cl-loop for entry across entries
               collect
               (progn
                 (unless (and (hash-table-p entry)
                              (stringp (gethash "entry" entry))
                              (not (string-empty-p (string-trim (gethash "entry" entry)))))
                   (signal 'fanyi-parse-error '("Youdao: invalid entry text")))
                 (let ((explain (gethash "explain" entry)))
                   (cons (gethash "entry" entry)
                         (if (and (stringp explain)
                                  (not (string-empty-p (string-trim explain))))
                             explain
                           "NO EXPLANATION"))))))))

(defun fanyi-youdao-thesaurus-render (result _request)
  "Render RESULT in the buffer named `fanyi-buffer-name'.
It's NOT thread-safe, caller should hold `fanyi-buffer-mtx'
before calling this function."
  (fanyi-with-fanyi-buffer
    ;; The headline about Youdao thesaurus service.
    (insert "# 有道同义词词典 \n\n")
    (cl-loop for (ent . explain) in result
             do (insert "- " ent "\n")
             ;; Prettify POS, order matters. See `fanyi-mode-font-lock-keywords'.
             do (insert (propertize
                         (s-replace-all '(("pron." . "(pron.)")
                                          ("prep." . "(prep.)")
                                          ("conj." . "(conj.)")
                                          ("abbr." . "(abbr.)")
                                          ("det."  . "(det.)")
                                          ("adj."  . "(adj.)")
                                          ("adv."  . "(adv.)")
                                          ("art."  . "(art.)")
                                          ("vt."   . "(vt.)")
                                          ("vi."   . "(vi.)")
                                          ("n."    . "(n.)")
                                          ("v."    . "(v.)"))
                                        explain)
                         'line-prefix (s-repeat fanyi-youdao-thesaurus-indent " ")
                         'wrap-prefix (s-repeat fanyi-youdao-thesaurus-indent " ")))
             do (insert "\n"))
    (insert "\n")))

(defconst fanyi-youdao-thesaurus-provider
  (fanyi-provider-create :url "https://dict.youdao.com/suggest?q=%s&doctype=json"
                         :api-type 'json
                         :prepare #'fanyi-youdao-thesaurus-prepare-request
                         :parse #'fanyi-youdao-thesaurus-parse
                         :render #'fanyi-youdao-thesaurus-render)
  "Youdao thesaurus provider.")

(provide 'fanyi-youdao-thesaurus)
;;; fanyi-youdao-thesaurus.el ends here
