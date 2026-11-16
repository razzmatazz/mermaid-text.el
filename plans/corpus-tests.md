# Testing against other renderers' test suites

## Goal

Run mermaid-text against the diagrams other mermaid projects test with,
to find the cases it crashes on, draws wrongly or cannot draw yet, and
fix them over time.  The renderers' languages do not matter: only their
test inputs, and their expected drawings where they have them, are used.

## Sources

Every candidate found so far is MIT, which is GPL-compatible: the cases
can be copied into this repo as long as each source keeps its copyright
notice and license text.

| Project | Language | What its tests offer |
|---|---|---|
| [mermaid-js/mermaid](https://github.com/mermaid-js/mermaid) | TypeScript | Parser tests: many real-world sources, no text drawings |
| [AlexanderGrooff/mermaid-ascii](https://github.com/AlexanderGrooff/mermaid-ascii) | Go | Sources with expected text drawings |
| [kevinswiber/mmdflux](https://github.com/kevinswiber/mmdflux) | Rust | Sources with expected text drawings; flowchart, class, sequence, state |
| [lukilabs/beautiful-mermaid](https://github.com/lukilabs/beautiful-mermaid) | TypeScript | ASCII output; flowchart, state, sequence, class, ER |

Not surveyed yet: how many cases each has, how they are laid out, and
how many are edge cases (long labels, cycles, self-loops, parallel
edges, nesting, CJK and emoji) rather than basics.  That is step 1.
Check each project's license again when importing it.

## What is checked

Other renderers lay a diagram out their own way, so their drawings are
never compared to ours character for character.  Each case is checked
for what holds of any correct drawing:

1. **No error.**  `mermaid-text-render` returns lines or nil, and never
   signals.
2. **Width.**  No line is wider than the width given, at 40, 80 and 120.
3. **Labels.**  Every node, participant, edge and message label appears
   in the drawing, allowing for wrapping.
4. **Drawn when supported.**  A diagram of a kind mermaid-text claims to
   support is drawn, not nil, at a generous width.  Kinds it does not
   support (class, ER, …) are expected to be nil.
5. **Structure** (later).  For small flowcharts, the edges traced in the
   drawing join the right boxes.

## Known failures

Most cases are expected to fail at first.  They are recorded and fixed
gradually, as a ratchet:

- A case's id is its source and file, e.g. `mmdflux/flowchart/cycle-3`.
- `test/corpus/known-failures.eld` lists each failing case with the
  checks it fails.
- The corpus test fails when a case not listed fails (a regression), and
  when a listed case passes, so that a fix removes it from the list.
- `make corpus-baseline` rewrites the list from a run, for the first
  import and after a source is refreshed.

Grouping the list by check and diagram kind shows which hole to work on
next.

## Layout

```
test/corpus/
  known-failures.eld
  mmdflux/
    LICENSE         their MIT text, unchanged
    SOURCE.md       repo, commit, authors, files taken, changes
    flowchart/…     the cases, as .mmd (and expected .txt when given)
  mermaid-ascii/
    …
test/mermaid-text-corpus-tests.el
```

The cases are vendored, not fetched, so the tests run offline and the
same in CI as locally.  A few hundred cases at most to begin with.

## Credit

Each import credits the project and its authors in four places:

- **`test/corpus/<project>/LICENSE`**: their license, unchanged, with
  their copyright line, as MIT requires.
- **`test/corpus/<project>/SOURCE.md`**: the repo URL, the upstream
  commit, the authors as the copyright line names them, which files were
  taken, and what was changed (renamed files, dropped drawings).
- **`README.md`**: an "Acknowledgements" section with a line per
  project, naming it and its authors, linking it, and saying the tests
  use its cases under MIT.
- **`CHANGELOG.md`** and the import's commit message: e.g. "Tests now
  include cases from mmdflux by Kevin Swiber (MIT)", the commit naming
  the upstream commit.

Refreshing a source updates its `SOURCE.md` commit; the README line stays.

## Steps

1. Survey the sources' test suites: case counts, layout, diagram kinds,
   expected drawings, edge cases.  Choose what to import.
2. Write `test/mermaid-text-corpus-tests.el`: load the cases, run
   checks 1–4, compare with `known-failures.eld`.  Add `make
   corpus-baseline`.
3. Import the first source (likely mmdflux or mermaid-ascii, as they
   target text drawings) with its credits; generate the baseline.
4. Import the others, one commit each.
5. Work down `known-failures.eld`; add check 5 when the basics pass.
