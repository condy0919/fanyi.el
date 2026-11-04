[![build](https://github.com/condy0919/fanyi.el/actions/workflows/build.yml/badge.svg)](https://github.com/condy0919/fanyi.el/actions/workflows/build.yml)
[![License](https://img.shields.io/:license-gpl3-blue.svg)](LICENSE)
![Supports Emacs 29.1+](https://img.shields.io/badge/Supports-Emacs_29.1%2B-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)
[![MELPA](https://melpa.org/packages/fanyi-badge.svg)](https://melpa.org/#/fanyi)

# fanyi.el

Look up words and phrases in multiple dictionaries from Emacs. Results include
clickable cross-references, pronunciation buttons, folding sections and layouts
that adapt to the window width.

All four providers are enabled by default, in the configuration order below.
Results appear as requests complete; Etymonline's browser button appears immediately.

| Dictionary | Provider | Content |
| --- | --- | --- |
| [海词 / Haici](https://dict.cn/) | `fanyi-haici-provider` | Chinese and English definitions, examples, collocations, quotations and related words |
| [Etymonline](https://www.etymonline.com/) | `fanyi-etymon-provider` | An `Open in browser` button for the entry |
| [Longman](https://www.ldoceonline.com/) | `fanyi-longman-provider` | English definitions, examples, word families, collocations, thesaurus and origins |
| [有道同义词 / Youdao](https://dict.youdao.com/) | `fanyi-youdao-thesaurus-provider` | Suggested words and explanations, using an unofficial API |

## Installation

Requires **Emacs 29.1 or later**. Runtime Lisp dependencies are built into Emacs.
Haici and Longman require HTML parsing support (`libxml-parse-html-region`);
Youdao requires JSON parsing support (`json-parse-buffer`). Etymonline's browser
button works without either parser. Audio playback uses an external player;
see [Pronunciation](#pronunciation).

With [MELPA](https://melpa.org/) configured, install with:

```text
M-x package-install RET fanyi RET
```

Or use `use-package`:

```emacs-lisp
(use-package fanyi
  :ensure t
  :commands (fanyi-dwim fanyi-dwim2 fanyi-from-history))
```

## Looking up words

| Command | Behavior |
| --- | --- |
| `M-x fanyi-dwim` | Prompt for a query, using the active region or word at point as the default |
| `M-x fanyi-dwim2` | Query the active region or word at point immediately; prompt if neither exists |
| `M-x fanyi-from-history` | Select and repeat a previous query |

Queries trim surrounding whitespace and reject empty input. Dictionary links,
bookmarks and Org links use the same query history; repeated queries move to
the front. The original query text is kept in the buffer and history, while
requests use lowercase text.

Results open in `*fanyi*`. The header shows the query followed by the counts of
pending, completed and failed dictionaries. Each buffer retains its own query
and request state, so you can rename a result buffer to keep an earlier lookup.
Starting another query cancels pending requests from the previous lookup.

### Result-buffer keys

| Key | Action |
| --- | --- |
| `s` | Start another query |
| `h` | Look up a word from history |
| `w` | Copy the current query |
| `q` | Quit the result window |
| `TAB` / `S-TAB` | Move to the next/previous visible button, wrapping at the ends |
| `RET` or mouse click on a button | Follow a word, play audio, toggle a section or activate another action |

On outline headings beginning with `#` or `##`, `TAB` cycles that section and
`S-TAB` cycles the buffer's outline. Haici tab headings and Longman thesaurus or
collocation headings are separate folding buttons: navigate to them with `TAB`,
then use `RET` to toggle their contents. Navigation skips buttons inside folded
sections; searching can reveal folded content.

### Imenu, bookmarks and Org links

- `M-x imenu` jumps to a dictionary heading.
- `C-x r m` creates a bookmark for the current query. Following it repeats the
  lookup, even if you rename the bookmark.
- Load the optional Org integration with `(require 'ol-fanyi)`. Use
  `org-store-link` in a result buffer and `org-insert-link` in an Org buffer.
  Following `[[fanyi:happy][happy]]` with `C-c C-o` starts a lookup.

## Configuration

Use `M-x customize-group RET fanyi RET`, `setq`, or `use-package`'s `:custom`.
Provider modules load when needed. Changes to `fanyi-providers` take effect on
the next query without reloading Fanyi.

```emacs-lisp
;; Default providers. Remove any you do not need.
(setq fanyi-providers '(fanyi-haici-provider
                        fanyi-etymon-provider
                        fanyi-longman-provider
                        fanyi-youdao-thesaurus-provider))
```

For English definitions and a browser link to etymology, use
`'(fanyi-longman-provider fanyi-etymon-provider)` instead.

| Option | Default | Effect |
| --- | --- | --- |
| `fanyi-auto-select` | `t` | Select the result window; use `nil` to keep focus in the current window |
| `fanyi-request-timeout` | `15` | Timeout in seconds for each dictionary request; `nil` disables it |
| `fanyi-headline-max-length` | `30` | Maximum query length displayed in the header |
| `fanyi-use-glyphs` | `t` | Use image-based labels and icons where supported |
| `fanyi-verbose` | `nil` | Report errors in the echo area |
| `fanyi-log` | `nil` | Record diagnostic messages in `*fanyi-log*` |

Failed dictionaries show an error and a `Retry dictionary` button. Retrying uses
the original request settings and replaces only that dictionary's result; other
results, reading position, selections and folds remain intact. Saved, renamed
result buffers can retry independently of a newer query. Missing optional audio
or pronunciation does not hide available definitions.

## Dictionary display

Haici, Longman and Youdao use Emacs's built-in `shr` rendering infrastructure.
`shr-use-fonts` is locally disabled to keep the buffer's default font and
character-based layout; semantic faces still supply highlighting and emphasis.
SVG labels have searchable, copyable text underneath and a text fallback when
images are disabled or unavailable.

Resizing the displayed window reflows cached content without another request.
Reflow preserves reading position, selections, narrowing and folded sections.
When several windows show the same buffer, the selected one determines its
layout, or the previous rendering window if none is selected. Use
`C-u M-x fanyi-shr-reflow` to force a refresh. Rendering does not fetch page
scripts, images or other remote assets.

If automatic reflow fails, the previous content remains available. Fanyi retries
when the layout or rendering settings change, or when you force a refresh,
instead of repeating the same failure after every command.

### Haici

The compact header groups the headword, star level, pronunciation and basic
meanings. The vocabulary level uses an SVG label with its full description in a
hover/ElDoc hint. `释义分布` opens the distribution chart; `原网页` opens the entry
in EWW.

Website tabs become independent folding sections with item counts. Chinese
meanings and their English explanations occupy separate lines. Examples put
English above subdued Chinese translations, with space between pairs.
Collocations and synonyms align words with their explanations in two columns;
narrow windows stack them. Nearby words form a compact wrapping list. Word and
phrase buttons start another lookup inside Emacs.

Choose which sections start expanded on a new lookup:

```emacs-lisp
(setq fanyi-haici-expanded-sections '(dual sort anno nfo)) ; Default
```

| Symbol | Section | Expanded by default |
| --- | --- | --- |
| `detail` | Detailed Chinese definitions | When bilingual definitions are unavailable |
| `dual` | Bilingual definitions | Yes |
| `en` | English definitions | No |
| `sort` | Examples | Yes |
| `anno` | Collocations | Yes |
| `auth` | Quotations | No |
| `nfo` | Synonyms and antonyms | Yes |
| `nwd` | Nearby words | No |
| `etm` | Origin | No |

Use `t` to expand all sections, or `nil` to collapse them all. The `detail`
fallback applies when `dual` is requested but unavailable. All available content,
including inactive website tabs, stays in the buffer. Changing this option
applies to new lookups; reflow preserves the reader's existing fold choices.

Set `fanyi-haici-chart-inhibit-same-window` to `t` to open distribution charts in
another window. Its default is `nil`.

### Longman

Definitions and examples retain their structure, parts of speech, grammar labels
and emphasis. British pronunciation uses a red speaker icon, and American
pronunciation uses a blue one. Word family, Origin, frequency markers such as
S2/W1, sense signposts and SYN/OPP labels use SVG badges when supported. Register
notes keep their own heading and explanation. Thesaurus and collocation sections
start expanded and can be folded individually.

Dictionary links query the word or phrase inside Emacs while retaining its
original Longman URL. Related-topic links open in EWW. Links to anchors in the
current entry jump to rendered content and reveal its folds; missing targets
open in EWW. External links open in a browser.

| Option | Default | Effect |
| --- | --- | --- |
| `fanyi-longman-example-indent` | `2` | Indentation for examples |
| `fanyi-longman-svg-border-radius` | `7` | Label corner radius in pixels |

The renderer maps dictionary-specific HTML classes to semantic styles; it does
not reproduce arbitrary website CSS. `shr-use-colors` controls SHR's color
handling, while Fanyi's semantic faces and SVG label colors remain customizable
through their faces.

### Youdao

Each entry has a clickable word and explanations grouped by part of speech.
The default borderless layout uses aligned columns when space permits, switching
to stacked entries in narrow windows. Missing explanations appear as subdued
`暂无释义` text.

| Option | Default | Effect |
| --- | --- | --- |
| `fanyi-youdao-thesaurus-layout` | `columns` | Use adaptive columns, or `stacked` to always put explanations below words |
| `fanyi-youdao-thesaurus-indent` | `2` | Explanation indentation in stacked entries |
| `fanyi-youdao-thesaurus-max-count` | `10` | Maximum entries requested; changes apply to the next query |

### Etymonline

Etymonline displays an `Open in browser` button without making an HTTP request
or parsing the page. Activating it opens the entry in an external browser.

## Pronunciation

Fanyi detects `mpv`, then `mplayer`, then `mpg123`. You can set
`fanyi-sound-player` to an executable name or full path; it is not a shell command
with arguments.

By default, Fanyi downloads the audio and sends its bytes to the player's standard
input through a binary pipe, passing `-` as the input argument. If your player
accepts HTTPS URLs directly, enable:

```emacs-lisp
(setq fanyi-sound-player-support-https t)
```

This option works across platforms. Audio is fetched or played only when a
pronunciation button is activated.

Starting another pronunciation cancels the previous audio download or player.
Use `M-x fanyi-stop-sound` to stop playback or a pending download explicitly.
Emacs-managed audio downloads time out after `fanyi-sound-timeout` seconds
(default `15`); set it to `nil` to disable the timeout. When the player fetches
HTTPS URLs directly, it handles its own network timeout.

## Troubleshooting

- **Missing player or no sound:** check `fanyi-sound-player`. Enable
  `fanyi-verbose` and/or `fanyi-log` to see download or playback errors.
- **Missing glyphs:** use a font with the required characters, or set
  `fanyi-use-glyphs` to `nil` to use text in place of supported image labels.
  Text speaker symbols still require a font that can display them.
- **`Could not parse dictionary response`:** the response did not contain usable
  dictionary content. Retry that dictionary. If the issue persists, report the
  dictionary, query and displayed error. Website changes or error pages can
  affect HTML providers; missing optional audio alone should not cause this.
- **Missing parser support:** check the functions listed under
  [Installation](#installation), or disable providers requiring the unavailable
  parser.

## Development

Run the source and byte-compiled checks with Emacs's built-in libraries:

```sh
make clean
make test
make compile
make test
```

`make` defaults to `make test`. Run `make clean` first to remove generated
bytecode when testing source files. The [CI workflow](.github/workflows/build.yml)
runs the sequence above on Emacs 29.1 and snapshot. Tests use mocked requests
and local HTML/JSON fixtures; they do not contact dictionary websites. See
[fixture documentation](test/fixtures/README.md) for saved-page references and
coverage.

Optional package metadata checks use `package-lint` through Cask. After
installing Cask, install the development dependency and run lint explicitly:

```sh
cask install
make lint
```

Cask and its package dependencies are used only by `make lint`; compilation
and ERT tests run directly in Emacs.

### Source layout

| File | Responsibility |
| --- | --- |
| [fanyi.el](fanyi.el) | Commands, result buffers, asynchronous tasks, retries and navigation |
| [fanyi-base.el](fanyi-base.el) | Provider/request records, callback dispatch, audio and logging |
| [fanyi-shr.el](fanyi-shr.el) | Cached rendering, reflow and local folding sections |
| [fanyi-haici.el](fanyi-haici.el) | Haici parsing, rendering and responsive word-list layouts |
| [fanyi-longman.el](fanyi-longman.el) | Longman HTML preparation, semantic styles and interactions |
| [fanyi-youdao-thesaurus.el](fanyi-youdao-thesaurus.el) | Youdao JSON parsing and adaptive entry layout |
| [fanyi-etymon.el](fanyi-etymon.el) | Etymonline browser button |
| [ol-fanyi.el](ol-fanyi.el) | Optional Org link integration |

### Provider interface

Providers are `fanyi-provider` records created with `fanyi-provider-create`.
`fanyi-providers` accepts the built-in symbols or provider records directly;
`copy-fanyi-provider` can be used to customize an existing provider.

Each lookup creates a separate `fanyi-request`, copying HTTP settings and
capturing callbacks before optional request preparation. Results belong to the
task, separately from the request and provider.

- `prepare(request)` optionally adjusts the new request, such as Youdao's result
  limit. Subsequent configuration changes affect future requests.
- `parse(response, request)` returns independent dictionary data. Longman returns
  prepared DOM, Haici a `fanyi-haici-result` record containing prepared content,
  and Youdao word/explanation pairs. Explicitly empty results use
  `(throw 'not-found nil)`; malformed responses signal `fanyi-parse-error`.
- `render(result, request)` inserts at point in the current buffer. The task
  supplies a writable destination and rolls back partial output on failure.
  Direct callers of `fanyi-render-result` supply their own writable region and
  transaction. Browser providers skip fetching/parsing and receive a nil result.

The old EIEIO service classes and generic methods have been removed. Custom
providers should use these records and callbacks. Restart Emacs when migrating
from the EIEIO version to replace previously loaded provider objects.

## Similar projects

- [youdao-dictionary](https://github.com/xuchunyang/youdao-dictionary.el)
- [bing-dict](https://github.com/cute-jumper/bing-dict.el)
