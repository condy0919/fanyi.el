[![build](https://github.com/condy0919/fanyi.el/actions/workflows/build.yml/badge.svg)](https://github.com/condy0919/fanyi.el/actions/workflows/build.yml)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](LICENSE)
![Supports Emacs 27.1-28.x](https://img.shields.io/badge/Supports-Emacs_27.1_--_28.x-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)
[![MELPA](https://melpa.org/packages/fanyi-badge.svg)](https://melpa.org/#/fanyi)

# fanyi.el

`fanyi.el` is a simple yet powerful multi-dictionaries interface for Emacs, currently it includes:

- [`海词`](https://dict.cn/)
- [`有道同义词`](https://dict.youdao.com/suggest?q=accumulate&doctype=json), **Unofficial** API
- [`etymonline`](https://www.etymonline.com/) (opens entries in an external browser)
- [`Longman`](https://www.ldoceonline.com/).

`fanyi.el` has integrations with:

- `imenu`, <kbd>M-x imenu</kbd> to jump to dictionary section directly.
- `bookmark`, <kbd>C-x r m</kbd> to make a bookmark for current query word which turns `bookmark` into a vocabulary notebook.
- `outline-mode`, <kbd>TAB</kbd> to collapse on section.
- `ol` (aka `org-link`), `org-store-link` on a fanyi buffer, the link can be inserted later via `org-insert-link`. It's accomplished by introducing a `fanyi` link type.
  e.g., <kbd>C-c C-o</kbd> on `[[fanyi:happy][description of happy]]` will invoke a fanyi search.
  Since `ol` depends on `org` package, this integration needs an explicit `(require 'ol-fanyi)`.

## Installation

Install `fanyi.el` from [MELPA](https://melpa.org) with:

```
M-x package-install RET fanyi RET
```

## Usage

<kbd>M-x</kbd> `fanyi-dwim`, or `fanyi-dwim2` if you can confirm the word at
point is wanted. A new command `fanyi-from-history` is introduced to browse the
search history.

If you want <kbd>M-x fanyi</kbd> to list only `fanyi-dwim`, `fanyi-dwim2` and
`fanyi-from-history`, try

``` emacs-lisp
;; Emacs 28+
(setq read-extended-command-predicate #'command-completion-default-include-p)
```

Set `fanyi-providers` with `setq`, Customize, or `use-package`. Changes take
effect on the next query; dictionary modules are loaded when needed.

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
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider)))

;; For non-`use-package' users
(setq fanyi-providers '(fanyi-haici-provider
                        fanyi-youdao-thesaurus-provider
                        fanyi-etymon-provider
                        fanyi-longman-provider))

;; Also takes effect on the next query, without reloading the package.
(setq fanyi-youdao-thesaurus-max-count 15)
```

By default, `M-x fanyi-dwim` will move the point to the new created `*fanyi* `buffer.

``` elisp
(setq fanyi-auto-select nil)
```

to keep the point.

Each dictionary request times out after 15 seconds by default. To change
the limit (or disable it with `nil`):

``` emacs-lisp
(setq fanyi-request-timeout 30)
```

Failed requests show their error in the results buffer without interrupting
other dictionaries. The `Retry query` button repeats the query using all
enabled dictionaries.

Missing pronunciation or audio does not hide available definitions. Responses
without usable dictionary content are reported as parsing failures; an explicit
empty result from Youdao is reported as no results found.

Longman uses Emacs's built-in `shr` renderer for dictionary HTML. Definitions,
lists, and inline formatting follow the document structure, with Fanyi headings,
separate example lines, and persistent highlighting for parts of speech and
grammar. Duplicate headwords, field codes, and signpost labels are omitted.
Audio buttons play pronunciations, dictionary links start Fanyi queries, and
related-topic links open in Emacs's EWW browser. The Origin label uses an SVG
badge when supported (controlled by `fanyi-use-glyphs` and
`fanyi-longman-svg-border-radius`), with a text fallback. Only dictionary content
is selected; rendering does not download images or other page resources.

Etymonline displays an `Open in browser` button immediately, without making an
HTTP request or parsing its pages. Click it to read the current entry and
complete any browser verification there.

When another dictionary returns a Cloudflare browser challenge, fanyi shows a
verification message and an `Open in browser` button instead of `Retry query`.
The button opens that entry in an external browser so you can complete any
verification and read it there. Other dictionaries continue normally. This
does not transfer browser cookies or import the browser's results into Emacs.

For windows users,

``` emacs-lisp
(setq fanyi-sound-player-support-https t)
```

if your sound player can be launched with `your-sound-player url`. Otherwise it will be called like `your-sound-player - <(the voice file)`, where `-` is interpreted as `stdin` which is an UNIX convention.

## FAQ

1. Audio button icon displayed in tofu.
   The icon is emoji, install the proper font to display it. If you use archlinux, `yay -S ttf-symbola` without additional settings. For MacOS users, add the following elisp to your Emacs config.
   ```elisp
   ;; "Apple Color Emoji" is bundled with MacOS.
   ;;
   ;; Emacs 29, 28
   (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend)

   ;; Emacs 27
   (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
   ```

2. A dictionary reports `Could not parse dictionary response`.
   The response did not contain usable dictionary content. Retry the query;
   if it keeps failing, file an issue with the dictionary, query word, and
   displayed error. Missing audio alone should not prevent definitions from
   being displayed.

## Screenshots

![status indicator](https://user-images.githubusercontent.com/4024656/136776219-0ffd6d27-ce36-48ca-979c-87f163cd79fc.png)
![海词](https://user-images.githubusercontent.com/4024656/128582690-2af2bb4a-46aa-4241-bdc0-6a5bb5e2db38.png)
![分布](https://user-images.githubusercontent.com/4024656/128582703-3e62cd17-a778-4982-9872-98e8697e333e.png)
![有道同义词英文](https://user-images.githubusercontent.com/4024656/132447780-1bbd9a6e-805e-4a4c-9e0a-03f499864c6f.png)
![有道同义词中文](https://user-images.githubusercontent.com/4024656/132447809-af3785bd-fe6d-4e52-a107-71c89fb8a26b.png)

## Development

Install development dependencies with `cask install`, then run `make test`
for the ERT suite and `make compile` for byte compilation. CI runs both.
Tests use mocked requests and the small HTML/JSON samples in `test/fixtures/`;
they do not contact dictionary websites. The fixtures exercise parsing and
rendering, but are not a check of the websites' current availability.

## Similar projects

- [youdao-dictionary](https://github.com/xuchunyang/youdao-dictionary.el)
- [bing-dict](https://github.com/cute-jumper/bing-dict.el)
