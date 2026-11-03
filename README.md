[![build](https://github.com/condy0919/fanyi.el/actions/workflows/build.yml/badge.svg)](https://github.com/condy0919/fanyi.el/actions/workflows/build.yml)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](LICENSE)
![Supports Emacs 29.1+](https://img.shields.io/badge/Supports-Emacs_29.1%2B-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)
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

Requires Emacs 29.1 or later.

Install `fanyi.el` from [MELPA](https://melpa.org) with:

```
M-x package-install RET fanyi RET
```

## Usage

<kbd>M-x</kbd> `fanyi-dwim`, or `fanyi-dwim2` if you can confirm the word at
point is wanted. A new command `fanyi-from-history` is introduced to browse the
search history.

Queries trim surrounding whitespace and reject empty input. All query entry
points, including dictionary buttons, bookmarks, and Org links, update the
same history, with duplicate words moved to the front.

If you want <kbd>M-x fanyi</kbd> to list only `fanyi-dwim`, `fanyi-dwim2` and
`fanyi-from-history`, try

``` emacs-lisp
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
other dictionaries. The `Retry dictionary` button retries only that dictionary,
using its original request settings. It replaces the failed section in place;
successful results, reading position, selections and folds remain intact.
Repeated clicks during a retry do not start duplicate requests. Renamed result
buffers can retry independently while a newer query is running.
Each result buffer retains its own query word and task counts, including when
you rename it to keep an earlier lookup. Starting a new query cancels pending
requests from the previous lookup without changing its saved word or results.

Missing pronunciation or audio does not hide available definitions. Responses
without usable dictionary content are reported as parsing failures; an explicit
empty result from Youdao is reported as no results found.

Youdao entries use a borderless layout with clickable, bold words on the left
and explanations on the right. Parts of speech start separate lines and retain
their highlighting. Long explanations wrap within their column. Narrow windows
automatically place each explanation below its word, and missing explanations
appear as subdued `暂无释义` text. Layout changes reuse cached content without
another request. To always use the stacked layout:

``` emacs-lisp
(setq fanyi-youdao-thesaurus-layout 'stacked)
```

`fanyi-youdao-thesaurus-indent` controls indentation in the stacked layout.

Haici includes detailed Chinese and English definitions, bilingual examples,
collocations, quotations and related words when supplied by the page. Its compact
header groups the headword, pronunciation and basic meanings; the vocabulary
level uses an SVG badge with a text fallback and the full explanation in its
tooltip. `释义分布` opens the distribution chart and `原网页` opens the page in EWW.

Website tabs become independent folding headings with item counts. Use `TAB` to
reach a heading and `RET` to toggle it. Bilingual definitions, examples,
collocations and synonyms start expanded; detailed definitions, English
definitions, quotations and nearby words start collapsed. With the default
configuration, detailed definitions expand when bilingual definitions are
unavailable. All available content, including inactive website tabs, remains
searchable in the buffer.

Chinese meanings and their English explanations occupy separate lines. Examples
keep English above subdued Chinese translations, with space between pairs.
Collocations and synonyms align words with their explanations in two columns;
narrow windows stack them. Nearby words form a compact wrapping list. Dictionary
links query the full word or phrase inside Emacs; other references open in EWW.
The header and sections use `shr` with `shr-use-fonts` disabled, retain semantic
highlighting, and reflow from cached content while preserving folds, selections
and reading position. Rendering does not fetch page scripts or images.

Customize the initially expanded tabs with `fanyi-haici-expanded-sections`:

``` emacs-lisp
(setq fanyi-haici-expanded-sections '(dual sort anno nfo)) ; Default
;; Or expand every tab on a new lookup:
(setq fanyi-haici-expanded-sections t)
```

The tab symbols are `detail` (detailed definitions), `dual` (bilingual definitions),
`en` (English definitions), `sort` (examples), `anno` (collocations), `auth`
(quotations), `nfo` (synonyms/antonyms), and `nwd` (nearby words). Manual folding
choices survive window resizing.

Longman uses Emacs's built-in `shr` renderer for dictionary HTML. Definitions,
lists, and inline formatting follow the document structure. A small set of
Longman class rules supplies headword sizes, colors, headings and example
indentation, based on a saved page and its CSS. This preserves native HTML
emphasis and persistent highlighting for parts of speech and grammar; it does
not implement a general CSS engine. Duplicate headwords and hidden field labels
are omitted. Visible signpost labels use the site's uppercase orange badges.
Synonym and antonym labels (`synopp`, such as SYN) use yellow SVG badges with
a styled text fallback; their following words retain their text and links.
Register and thesaurus sections keep separate headings, explanations, entries,
and examples, including the site's emphasis within those paragraphs.
Supplementary sections use headings, whitespace and indentation instead of
outer borders. Group headings follow the theme's keyword face; short Register
notes use a body-sized bold heading and remain expanded.
Thesaurus and collocation sections start expanded. Their headings are buttons:
use `TAB` to reach one, then `RET` or a mouse click to toggle its contents. Each
button folds only its own section, including when it appears inside a sense;
following definitions and other dictionaries remain visible. These local folds
are independent of outline headings and survive window and style changes.
Audio buttons play pronunciations. Dictionary links query the entry name while
retaining the original Longman URL, so phrases keep their spaces in other
dictionaries and query history. Links to the current page jump to a rendered
anchor and open its folds; targets outside the rendered content open in EWW
with their original URL. Related-topic links also open in EWW. Word family, Origin, and frequency
labels such as S2 and W1 use SVG badges when supported (controlled by `fanyi-use-glyphs` and
`fanyi-longman-svg-border-radius`), with a text fallback. Only dictionary content
is selected; rendering does not download images or other page resources.

Longman renders with `shr-use-fonts` locally disabled, using the buffer's default
font and character-based layout. It still respects `shr-use-colors`. HTML regions
reflow from their saved DOM when the displayed window changes width or Longman's
styles and glyph settings change, without another request. Visible results are
checked after each command; asynchronous setting changes are picked up by the next
command or window event. Unchanged settings do not trigger rendering. SVG badges
follow the window's font size, including text scaling. Reading position, folded
headings, and other dictionaries are retained. When the buffer appears in several
windows, its selected window determines the layout. `C-u M-x fanyi-shr-reflow`
still forces a refresh. Fanyi's text-formatting rules do not reinterpret the HTML
body as Markdown-style emphasis.

Etymonline displays an `Open in browser` button immediately, without making an
HTTP request or parsing its pages. Click it to read the current entry in your
browser.

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
   (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend)
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

Run `make test CASK=` for the ERT suite and `make compile CASK=` for byte
compilation; no external Lisp packages are required. If using Cask, run
`cask install`, then `make test` and `make compile`. CI runs both checks.
Tests use mocked requests and the small HTML/JSON samples in `test/fixtures/`;
they do not contact dictionary websites. The fixtures exercise parsing and
rendering, but are not a check of the websites' current availability.

Dictionary modules expose `fanyi-provider` records containing configuration
and ordinary callbacks. The existing provider symbols in `fanyi-providers`
remain the normal configuration interface. Direct records are also accepted;
use `copy-fanyi-provider` to customize an endpoint without changing a built-in
provider.

`fanyi-make-request` creates a separate snapshot for each lookup, copying the
HTTP settings and capturing the callbacks. Parsed data is stored on the task,
separately from both the provider and the request:

- `prepare(request)` is optional and adjusts only that request, for example
  to apply Youdao's current result limit.
- `parse(response, request)` returns dictionary data. Longman returns a prepared
  DOM that rendering and reflow reuse without repeating HTML conversion; the
  response DOM is left unchanged. Youdao returns word/explanation pairs, and
  Haici returns a `fanyi-haici-result` record with a prepared DOM in `content`.
  Empty results still use `(throw 'not-found nil)`;
  malformed responses signal `fanyi-parse-error`.
- `render(result, request)` inserts the result at point in the current buffer.
  It does not select a buffer or move to the end of it. The dispatcher supplies
  a writable region for that dictionary, preserves the reader's position, and
  rolls back partial text on failure. Browser providers skip parsing and receive
  a nil result. Direct callers of `fanyi-render-result` supply their own writable
  destination and insertion point.

The former EIEIO service classes and generic methods have been removed.
New providers use `fanyi-provider-create` with `:parse` and `:render` callbacks,
and optionally `:prepare`. Restart Emacs after upgrading from the EIEIO version
so previously loaded provider objects are replaced with the new records.

## Similar projects

- [youdao-dictionary](https://github.com/xuchunyang/youdao-dictionary.el)
- [bing-dict](https://github.com/cute-jumper/bing-dict.el)
