# mermaid-text

Draws [mermaid](https://mermaid.js.org) diagrams as text, with
box-drawing characters, in plain Emacs Lisp.  It needs no mermaid CLI,
Node.js, browser or image support, so diagrams show in a terminal Emacs
too, and in any buffer that shows text.

```
      ┌──────┐
      │ Push │
      └───┬──┘
          │
          │
          ▼
   ┌─────────────┐
   < Tests pass? >
   └───┬─────┬───┘
     ╭─╯     ╰──╮
     │ yes      │ no
     ▼          ▼
┌────────┐   ┌─────┐
│ Deploy │   │ Fix │
└────────┘   └─────┘
```

It was written for [octocat.el](https://github.com/razzmatazz/octocat.el),
which draws the mermaid fences of GitHub comments and READMEs with it.

## Supported diagrams

- `flowchart` / `graph`: TD, TB, BT, LR and RL; box, rounded, circle
  and diamond nodes; solid, dotted and thick edges, with arrows and
  labels; `A & B --> C` fan-out; subgraphs.
- `sequenceDiagram`: participants, messages, notes, `loop`, `alt`,
  `opt` and `par` blocks, `autonumber`.
- `stateDiagram`: transitions only.
- `pie`.

Styling (`style`, `classDef`), comments and front matter are ignored.

## Usage

`mermaid-text-render` takes the diagram's source as a list of lines and
the width it may use, and returns the drawing as a list of lines:

```elisp
(mermaid-text-render (split-string "graph TD\nA --> B" "\n") 80)
```

It fits the drawing to the width: labels are wrapped, a left-to-right
flowchart is turned top-down, sequence diagrams are squeezed.  It
returns nil for what it cannot draw, or cannot draw within the width,
so the caller can show the source instead.

## Installation

Not yet on MELPA.  Until then, with `package-vc` (Emacs 29+):

```elisp
(package-vc-install "https://github.com/razzmatazz/mermaid-text.el")
```

## Development

`make test` runs the tests, `make compile` byte-compiles with warnings
as errors, and `make lint` runs checkdoc and package-lint.  They need
[Eldev](https://github.com/emacs-eldev/eldev).

## License

GPL-3.0-or-later; see [LICENSE](LICENSE).
