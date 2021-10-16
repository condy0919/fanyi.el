[![build](https://github.com/condy0919/fanyi.el/actions/workflows/build.yml/badge.svg)](https://github.com/condy0919/fanyi.el/actions/workflows/build.yml)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](LICENSE)
![Supports Emacs 27.1-28.x](https://img.shields.io/badge/Supports-Emacs_27.1_--_28.x-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)
[![MELPA](https://melpa.org/packages/fanyi-badge.svg)](https://melpa.org/#/fanyi)

# fanyi.el

`fanyi.el` is a simple yet powerful multi-dictionaries interface for Emacs, currently it includes:

- [`海词`](https://dict.cn/)
- [`有道翻译`](https://fanyi.youdao.com/translate?doctype=json&type=), **Unofficial** ~~outdated~~ API
- [`有道同义词`](https://dict.youdao.com/suggest?q=accumulate&doctype=json), **Unofficial** API
- [`etymonline`](https://www.etymonline.com/)
- [`Longman`](https://www.ldoceonline.com/)
- [`LibreTranslate`](https://libretranslate.com/), Free and Open Source Machine Translation API.
- [`American Heritage dictionary`](https://ahdictionary.com/), unusable in China due to high latency. **Not fully implemented**

`fanyi.el` has integrations with:

- `imenu`, <kbd>M-x imenu</kbd> to jump to dictionary section directly.
- `bookmark`, <kbd>C-x r m</kbd> to make a bookmark for current query word which turns `bookmark` into a vocabulary notebook.
- `outline-mode`, <kbd>TAB</kbd> to collapse on section.
- `ol` (`org-link`), `org-store-link` on a fanyi buffer, the link can be inserted later via `org-insert-link`. It's accomplished by introducing a `fanyi` link type.
  e.g., <kbd>C-c C-o</kbd> on `[[fanyi:happy][description of happy]]` will invoke a fanyi search.
  Since `ol` depends on `org` package, this integration needs an explicit `(require 'ol-fanyi)`.

## Installation

Install `fanyi.el` from [MELPA](https://melpa.org) with:

```
M-x package-install RET fanyi RET
```

If you're an `American Heritage Dictionary` user, make sure `Minion New` font is
installed. Otherwise the pronunciation may be displayed in tofu, or an
`all-the-icons` icon if you have `all-the-icons` installed.

``` shell
# family: Minion New Bold
# https://ahdictionary.com/application/resources/fonts/MinionNew-Bold.ttf
#
# family: Minion New Italic
# https://ahdictionary.com/application/resources/fonts/MinionNew-Italic.ttf
curl https://ahdictionary.com/application/resources/fonts/MININ___.TTF -o Minion.ttf

# linux
mv Minion.ttf ~/.local/share/fonts/

# macOS
mv Minion.ttf ~/Library/Fonts/
```

## Usage

Call `fanyi-dwim`, or `fanyi-dwim2` if you can confirm the word at point is
wanted. A new command `fanyi-from-history` is introduced to browse the search
history.

If you want <kbd>M-x fanyi</kbd> to list only `fanyi-dwim`, `fanyi-dwim2` and
`fanyi-from-history`, try

``` emacs-lisp
;; Emacs 28 only
(setq read-extended-command-predicate #'command-completion-default-include-p)
```

Don't customize `fanyi-providers` via `setq`, use the custom system instead.

``` emacs-lisp
;; If you want English-English dictionary only.
(use-package fanyi
  :ensure t
  :custom
  (fanyi-providers '(fanyi-etymon-provider
                     fanyi-longman-provider)))

;; Default, comment out the providers you don't need.
(use-package fanyi
  :ensure t
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道翻译
                     fanyi-youdao-translate-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider
                     ;; LibreTranslate
                     fanyi-libre-provider)))

;; For non-`use-package' users
(custom-set-variables
 '(fanyi-providers '(fanyi-haici-provider
                     fanyi-youdao-translate-provider
                     fanyi-youdao-thesaurus-provider
                     fanyi-etymon-provider
                     fanyi-longman-provider
                     fanyi-libre-provider)))
```

If you're a Windows user,

``` emacs-lisp
(setq fanyi-sound-player-support-https t)
```

may be helpful since `- is interpreted as stdin` is an UNIX convention. If
`fanyi-sound-player-support-https` is non-nil, the url will be passed to
`fanyi-sound-player` directly.

## Screenshots

![status indicator](https://user-images.githubusercontent.com/4024656/136776219-0ffd6d27-ce36-48ca-979c-87f163cd79fc.png)
![海词](https://user-images.githubusercontent.com/4024656/128582690-2af2bb4a-46aa-4241-bdc0-6a5bb5e2db38.png)
![分布](https://user-images.githubusercontent.com/4024656/128582703-3e62cd17-a778-4982-9872-98e8697e333e.png)
![Etymon](https://user-images.githubusercontent.com/4024656/128583142-dfd26d67-45c5-482a-9268-d7482dbe65f3.png)
![American Heritage](https://user-images.githubusercontent.com/4024656/129494115-02b1e344-4eb3-43ac-8ccc-08d75a3aeecd.png)
![Longman](https://user-images.githubusercontent.com/4024656/131615163-7b52a0eb-77a4-481f-b093-8f9c7fa91216.png)
![有道翻译英文](https://user-images.githubusercontent.com/4024656/137604778-f0b632b7-d585-4d96-b169-5e8386cf1056.png)
![有道翻译中文](https://user-images.githubusercontent.com/4024656/137604796-7fca0be0-2904-4139-947f-699bfdf136f7.png)
![有道同义词英文](https://user-images.githubusercontent.com/4024656/132447780-1bbd9a6e-805e-4a4c-9e0a-03f499864c6f.png)
![有道同义词中文](https://user-images.githubusercontent.com/4024656/132447809-af3785bd-fe6d-4e52-a107-71c89fb8a26b.png)

## Similar projects

- [youdao-dictionary](https://github.com/xuchunyang/youdao-dictionary.el)
- [bing-dict](https://github.com/cute-jumper/bing-dict.el)
