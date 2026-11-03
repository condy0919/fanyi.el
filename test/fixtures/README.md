# Dictionary response fixtures

Most files are hand-written, reduced examples of the HTML and JSON structures
consumed by the providers. These synthetic cases do not represent snapshots of
the current sites. `example.invalid` URLs must never be fetched. The saved
Longman reference described below is an exception, provided by the user.

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
including proportional-font reflow. Fanyi intentionally keeps `##` headings,
query/audio buttons, hidden signposts, and the Origin SVG label.
