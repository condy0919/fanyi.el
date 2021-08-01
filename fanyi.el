;;; fanyi.el --- English-Chinese translator for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2021 Zhiwei Chen
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/condy0919/fanyi.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4")) ((s "1.12.0"))

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



;;; Code:

(require 's)

(require 'dom)
(require 'seq)
(require 'url)
(require 'json)
(require 'chart)
(require 'eieio)
(require 'button)
(require 'thingatpt)

;; Silence compile warnings.
(defvar url-http-end-of-headers)

(defgroup fanyi nil
  "English-Chinese translator for Emacs."
  :prefix "fanyi-"
  :group 'tools
  :link '(url-link "https://github.com/condy0919/fanyi.el"))

(defcustom fanyi-sound-player
  (or (executable-find "mpv")
      (executable-find "mplayer")
      (executable-find "mpg123"))
  "Program to play sound."
  :type 'string
  :group 'fanyi)

(defface fanyi-word-face
  '((t (:height 1.75 :weight bold :foreground "#d08770")))
  "Face used for user requested word."
  :group 'fanyi)

(defface fanyi-syllable-face
  '((t (:weight bold :foreground "#ff0000")))
  "Face used for syllable of word."
  :group 'fanyi)

(defface fanyi-star-face
  '((t (:foreground "#ffff00")))
  "Face used for star of word."
  :group 'fanyi)

(defface fanyi-female-speaker-face
  '((t (:foreground "#ec5e66")))
  "Face used for female speaker button."
  :group 'fanyi)

(defface fanyi-male-speaker-face
  '((t (:foreground "#57a7b7")))
  "Face used for male speaker button."
  :group 'fanyi)

(defface fanyi-word-paraphrase-face
  '((t (:weight bold)))
  "Face used for paraphrase of word."
  :group 'fanyi)

(defface fanyi-word-category-face
  '((((background dark)) :foreground "light slate gray")
    (((background light)) :foreground "dark slate gray"))
  "Face used for highlight the part of speech."
  :group 'fanyi)

(defun fanyi-play-sound (url)
  "Play URL via external program.
See `fanyi-sound-player'."
  (if (not fanyi-sound-player)
      (user-error "`fanyi-sound-player' is needed to play sound")
    (start-process fanyi-sound-player nil fanyi-sound-player url)))

(defconst fanyi-buffer-name "*fanyi*"
  "The default name of translation buffer.")

(defclass fanyi-service ()
  ((url :initarg :url
        :type string
        :protection :protected
        :documentation "Dictionary translation url.")
   (sound-url :initarg :sound-url
              :type string
              :protection :protected
              :documentation "Dictionary sound url."))
  "The base class of translation service."
  :abstract t)

;;
;; honor 音节划分 难度
;; 发音: 英式 女，男 美式 女，男
;; 翻译
;;
;; 释义常用度分布图
;;
;; 发音
;;
;; * 相关扩展链接，如异体字、名词复数等
;; (dom-texts (dom-by-class xxx "shape"))
;; ;;=> 名词: honorer 过去式: honored 过去分词: honored 现在分词: honoring 第三人称单数: honors
;; ;; => ((div ((class . "shape")) " "
;; ;;          (label nil "名词:") " " (a (...) " honorer				") " "
;; ;;          (label nil "过去式:") " " (a (...) " honored				") " "
;; ;;          (label nil "过去分词:") ...))
;;
;; 音节划分 词频 星级
;; 发音: 英 女，男 美 女，男
;; 释义
;; 分布图
;; 相关扩展
;; 其他
(defclass fanyi-haici-service (fanyi-service)
  ((syllable :initarg :syllable
             :type string
             :documentation "Syllable of the word.")
   (star :initarg :star
         :type number
         :documentation "Frequency of the word.")
   (level :initarg :level
          :type string
          :documentation "Level description of the word.")
   (phonetics :initarg :phonetics
              :type list
              :documentation "Phonetics of the word.
It could be either British pronunciation or American pronunciation.")
   (paraphrases :initarg :paraphrases
                :type list
                :documentation "List of (pos . paraphrase).")
   (distribution :initarg :distribution
                 :type list
                 :documentation "List of (percent . sense)."))
  "The HaiCi translation service.")

;; Silence unknown slots warning.
(eieio-declare-slots :url :sound-url :syllable :star :level :phonetics :paraphrases :distribution)

(cl-defmethod fanyi-parse-from ((this fanyi-haici-service) dom)
  "Complete the fields of THIS from DOM tree."
  ;; syllable.
  (let* ((str (dom-attr (dom-by-class dom "keyword") 'tip))
         (matches (s-match "\\([a-zA-Z·]+\\)" str)))
    (cl-assert matches)
    (oset this :syllable (nth 1 matches)))

  ;; star and level description.
  (let* ((str (dom-attr (dom-by-class dom "level-title") 'level))
         (matches (s-match "\\([12345]\\)" str)))
    (cl-assert matches)
    (oset this :star (string-to-number (nth 1 matches)))
    (oset this :level str))

  ;; phonetics, a list of (pronunciation, female sound url, male sound url)
  ;;
  ;; British: female, male
  ;; American: female, male
  (let ((phonetics (dom-children (dom-by-class dom "phonetic")))
        collection)
    ;; British is at index 1, American is at index 3.
    (dolist (idx (list 1 3))
      (let ((node (nth idx phonetics)))
        (cl-pushnew (list
                     ;; pronunciation
                     (dom-text
                      (dom-search node
                                  (lambda (x)
                                    (string= (dom-attr x 'lang) "EN-US"))))
                     ;; female sound url
                     (dom-attr
                      (dom-search node
                                  (lambda (x)
                                    (string= (dom-attr x 'class) "sound fsound")))
                      'naudio)
                     ;; male sound url
                     (dom-attr
                      (dom-search node
                                  (lambda (x)
                                    (string= (dom-attr x 'class) "sound")))
                      'naudio))
                    collection)))
    (oset this :phonetics (nreverse collection)))

  ;; paraphrases, list of (pos, paraphrase)
  (let ((paraphrases (butlast (dom-by-tag (dom-by-class dom "dict-basic-ul") 'li))))
    (oset this :paraphrases
          (cl-loop for p in paraphrases
                   collect (list (dom-text (nth 3 p)) (dom-text (nth 5 p))))))

  ;; distribution of paraphrases.
  (let* ((chart (dom-attr (dom-by-id dom "dict-chart-basic") 'data))
         (json (json-read-from-string (url-unhex-string chart))))
    (oset this :distribution
          ;; transform (\1 (percent . 55) (sense . "abc"))
          (cl-loop for j in json
                   collect (seq-map #'cdr (seq-drop j 1)))))
  )

;; 音节划分 词频 星级
;; 发音: 英 女，男 美 女，男
;; 释义
;; 分布图
;; 相关扩展
;; 其他
(cl-defmethod fanyi-render ((this fanyi-haici-service) buf)
  "Render THIS page into BUF and return it."
  ;; (chart-bar-quickie
  ;;  'horizontal
  ;;  "Eye Colors - Descending"
  ;;  (mapcar #'car eye-color-groups) "Colors"
  ;;  (mapcar #'cdr eye-color-groups) "Frequency"
  ;;  nil
  ;;  (on #'cdr #'>) ;; A compar

;;    (paraphrases :initarg :paraphrase
;;                 :type list
;;                 :documentation "List of (pos . paraphrase).")
;;    (distribution :initarg :distribution
;;                  :type list
;;                  :documentation "List of (percent . sense)."))
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      ;; Clear.
      (erase-buffer)
      ;; Syllable division and star/level description.
      (insert (format "Syllable division: %s %s %s\n"
                      (propertize (oref this :syllable) 'face 'fanyi-syllable-face)
                      (propertize (s-repeat (oref this :star) "★") 'face 'fanyi-star-face)
                      (oref this :level)))
      ;; Phonetics.
      ;; British: pronunciation, female sound url, male sound url
      ;; American: pronunciation, female sound url, male sound url
      (let ((phonetics (oref this :phonetics)))
        (cl-assert (equal (length phonetics) 2))
        (cl-loop
         for i from 0 to 1 do
         (cl-destructuring-bind (pronunciation female male) (nth i phonetics)
           (if (equal i 0)
               (insert "英")
             (insert "  美"))
           (insert (format " %s " pronunciation))
           (insert-button "♀"
                          'action (lambda (url) (fanyi-play-sound url))
                          'button-data (format (oref this :sound-url) female)
                          'face 'fanyi-female-speaker-face
                          'follow-link t)
           (insert " ")
           (insert-button "♂"
                          'action (lambda (url) (fanyi-play-sound url))
                          'button-data (format (oref this :sound-url) male)
                          'face 'fanyi-male-speaker-face
                          'follow-link t)
           ))
        )
    )))
  ;; (let* ((dist (oref this :distribution))
  ;;        (chart (chart-bar-quickie
  ;;                'vertical
  ;;                "fanyi-haici-render"
  ;;                (mapcar #'cadr dist) "Sense"
      ;;                (mapcar #'car dist) "Percent")))

(setq xxx-buf (get-buffer-create "*xxx*"))

(fanyi-render fanyi-haici-instance xxx-buf)
(oref fanyi-haici-instance :star)

(oref fanyi-haici-instance :distribution)

(defvar fanyi-haici-instance
  (fanyi-haici-service :url "https://dict.cn/%s"
                       :sound-url "https://audio.dict.cn/%s"))

(defun fanyi--insert-header (text)
  "The header about current TEXT."
  (insert (format "Translating %s\n\n\n" (propertize text 'face 'fanyi-word-face))))

;;;###autoload
(defun fanyi-dwim (word)
  "Translate WORD."
  (interactive (let* ((default (if (use-region-p)
                                   (buffer-substring-no-properties (region-beginning) (region-end))
                                 (thing-at-point 'word t)))
                      (prompt (if (stringp default)
                                  (format "Search Word (default \"%s\"): " default)
                                "Search Word: ")))
                 (list (read-string prompt nil nil default))))
  (let ((url (format (oref fanyi-haici-instance :url) word)))
    ;; libxml2 is required.
    (when (not (fboundp 'libxml-parse-html-region))
      (error "This function requires Emacs to be compiled with libxml2"))
    (url-retrieve url (lambda (status)
                        ;; Something went wrong.
                        (when (or (not status) (plist-member status :error))
                          (user-error "Something went wrong.\n\n%s" (pp-to-string (plist-get status :error))))
                        ;; Move point to the real http content.
                        (goto-char url-http-end-of-headers)
                        (let ((buf (get-buffer-create fanyi-buffer-name))
                              (dom (libxml-parse-html-region (point) (point-max) url)))
                          (with-current-buffer buf
                            (let ((inhibit-read-only t)
                                  (inhibit-point-motion-hooks t))
                              ;; Clear the previous search result.
                              (erase-buffer)
                              ;; Make the user searched WORD more perceptible.
                              (fanyi--insert-header word)

                              (fanyi-parse-from fanyi-haici-instance dom)
                              (insert (fanyi-render fanyi-haici-instance))

                              )
                            (pop-to-buffer buf))
                          (setq xxx dom))))))


(provide 'fanyi)

;;; fanyi.el ends here
