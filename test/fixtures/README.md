# Dictionary response fixtures

These are hand-written, reduced examples of the HTML and JSON structures
consumed by the providers. They are synthetic regression data, not downloaded
pages or snapshots of the current dictionary websites. Definitions and audio
identifiers are test data; `example.invalid` URLs must never be fetched.

- `haici.html`: two pronunciations, four audio identifiers, two definitions,
  and a related word. Whitespace and the final placeholder list item match the
  structure expected by the original parser.
- `longman.html`: a headword, British and American audio, a definition link,
  a cross-reference, an example with audio, and an origin section.
- `longman-rich.html`: nested and unfamiliar markup, lists, paragraphs, a table,
  relative audio and help links, an encoded cross-reference, and unrelated page
  content that must not be rendered.
- `youdao.json`: two entries, including one without an explanation.
- `youdao-empty.json`: a successful API response explicitly containing no entries.
- `error-page.html`: a generic service error page with no dictionary structure.

The provider tests parse these files with Emacs's HTML/JSON parsers and render
results without network requests or image rendering. When extending coverage,
keep each example small enough that its purpose is clear.
Partial-response tests also vary the parsed fixture trees to remove audio,
pronunciation, or required content and to change optional formatting.
