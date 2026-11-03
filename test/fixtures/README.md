# Dictionary response fixtures

Most files are hand-written, reduced examples of the HTML and JSON structures
consumed by the providers. These synthetic cases do not represent snapshots of
the current sites. `example.invalid` URLs must never be fetched. The saved
Longman references described below are exceptions, provided by the user.

- `haici.html`: two pronunciations, four audio identifiers, two definitions,
  and a related word. Whitespace and the final placeholder list item match the
  structure expected by the original parser.
- `longman.html`: a headword, British and American audio, a definition link,
  a cross-reference, an example with audio, and an origin section.
- `longman-rich.html`: nested and unfamiliar markup, lists, paragraphs, a table,
  relative audio and help links, an encoded cross-reference, and unrelated page
  content that must not be rendered.
- `longman-dragon.html`: reduced markup using the text reported by the user for
  dragon, including a dictionary heading, topic and dictionary links,
  duplicate headword markup, grammar labels, seven corpus examples, and origin.
- `longman-dragon-saved.html`: the actual dictionary subtree from a browser-saved
  https://www.ldoceonline.com/dictionary/dragon page supplied on 2026-11-03.
  The original inline whitespace, classes, links and audio URLs are retained;
  only the advertisement container and its script were removed from the subtree.
  The surrounding document adds a local stylesheet for visual inspection.
- `longman-dragon-saved.css`: relevant presentation rules from the accompanying
  `common.css`, plus the original base font and link treatment. Open the saved
  HTML in a browser to inspect it without fetching page resources. It excludes
  page chrome and remote icon fonts, so the browser reference has no audio icons.
- `longman-sun-saved.html`: the actual dictionary subtree from the browser-saved
  https://www.ldoceonline.com/dictionary/sun page supplied on 2026-11-03 as
  `/tmp/sun`. It retains all four entries (newspaper, noun, verb, abbreviation),
  the collocation box, both corpus sections, and origin. Only three advertising
  containers and their scripts were removed. Original whitespace, classes, links,
  and inactive audio URL attributes are preserved, including the hidden
  `suppressed LEXVAR` spelling that previously leaked into the rendered headword.
- `longman-sun-saved.css`: relevant rules from `/tmp/sun_files/common_VJuP.css`
  in their original declaration order, with the base font and link treatment.
  Unrelated selectors, responsive page chrome, remote icon fonts and resources
  are excluded. The HTML loads only this local stylesheet; dictionary links and
  audio data remain available to offline parser tests without fetching them.
- `longman-explode-saved.html`: the actual dictionary subtree from the user's
  browser-saved https://www.ldoceonline.com/dictionary/explode page at
  `/tmp/explode`. It retains the word family, six senses, five visible signposts,
  Register box, two thesaurus groups with ten entries, corpus, and origin.
  Only two advertising containers, their scripts, and adjacent advertising
  indentation were removed; inline dictionary whitespace, classes, links, and
  inactive audio attributes are retained.
  This includes the plain-text adjective `exploding` and the unlinked verb
  `span.w`, which exercise different word-family markup from the anchors.
- `longman-explode-saved.css`: relevant presentation rules from
  `/tmp/explode_files/common_4Vww.css`, preserving declaration order and recording
  source line numbers. It includes the original narrow-screen word-family rule
  that starts each part of speech on a new line below 762 pixels. Unrelated
  selectors, scripts, remote fonts, and resources are excluded; the HTML loads
  only this local stylesheet.
- `youdao.json`: two entries, including one without an explanation.
- `youdao-empty.json`: a successful API response explicitly containing no entries.
- `error-page.html`: a generic service error page with no dictionary structure.

The provider tests parse these files with Emacs's HTML/JSON parsers and render
results without network requests or image rendering. When extending coverage,
keep each example small enough that its purpose is clear.
Partial-response tests also vary the parsed fixture trees to remove audio,
pronunciation, or required content and to change optional formatting.

The saved reference exposed details absent from the synthetic dragon case:
`ACTIV` is hidden by CSS, the corpus label and title live in separate sibling
containers, and corpus `NodeW` spans are bold. CSS `green` and `gray` are
`#008000` and `#808080`; Emacs's named colors need not have those values.
The style tests check the effective displayed face after font-lock refreshes.
The reference has also been compared with an isolated graphical Emacs frame,
including reflow that preserves reading position and folded headings. Fanyi keeps
`##` headings, query/audio buttons, visible signposts, and the Origin SVG label.

The saved sun reference adds cross-reference-only senses, a collocation box
with four groups and 14 collocations, grammar example groups, multiple corpus
titles, frequency labels, and superscript homonym numbers. Its tests check
line boundaries and spacing, font-lock preservation, query/audio actions, and
consistent example indentation with and without audio buttons. These are
semantic layout checks rather than browser pixel comparisons; SHR continues to
use the user's fixed-width font preference.

The saved explode reference adds word-family grouping, visible orange signposts,
Register explanations with emphasized expressions, and a thesaurus with nested
groups and separate entries. `F2NBox` and `ThesBox` are block containers;
`EXPL` and `Exponent` supply line boundaries even though their HTML tags are
spans. `EXP`, `EXPR`, and `COLLOINEXA` mark bold semantic text, while `ACTIV`
remains hidden. The local CSS preserves these distinctions for offline review.
