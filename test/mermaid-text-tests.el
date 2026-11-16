;;; mermaid-text-tests.el --- ERT tests for mermaid-text  -*- lexical-binding: t; -*-

;; Assisted-by: Claude:claude-opus-5-5
;; Assisted-by: Claude:claude-sonnet-5-5

;;; Commentary:

;; Run via `make test'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mermaid-text)

(defun mermaid-text-tests--draw (source &optional width)
  "Return the diagram SOURCE drawn for WIDTH, as a string, or nil."
  (when-let* ((lines (mermaid-text-render (split-string source "\n") width)))
    (string-join lines "\n")))

(defun mermaid-text-tests--rows (&rest rows)
  "Join ROWS with newlines, to write a drawing out row by row."
  (string-join rows "\n"))

(defun mermaid-text-tests--width (drawing)
  "Return the width of the widest row of DRAWING."
  (apply #'max (mapcar #'string-width (split-string drawing "\n"))))

;;; Flowcharts

(ert-deftest mermaid-text-test-flow-top-down ()
  "A chart runs downwards by default, with an arrow into the target."
  (should (equal (mermaid-text-tests--draw "graph TD\nA[Start] --> B[End]")
                 (mermaid-text-tests--rows
                  "┌───────┐"
                  "│ Start │"
                  "└───┬───┘"
                  "    │"
                  "    │"
                  "    ▼"
                  " ┌─────┐"
                  " │ End │"
                  " └─────┘")))
  ;; BT runs the other way, so the arrow points up.
  (should (equal (mermaid-text-tests--draw "graph BT\nA --> B")
                 (mermaid-text-tests--rows
                  "┌───┐"
                  "│ B │"
                  "└───┘"
                  "  ▲"
                  "  │"
                  "  │"
                  "┌─┴─┐"
                  "│ A │"
                  "└───┘"))))

(ert-deftest mermaid-text-test-flow-left-right ()
  "A left-to-right chart lays nodes side by side, joined by arrows."
  (should (equal (mermaid-text-tests--draw "graph LR\nA[Start] --> B[End]")
                 (mermaid-text-tests--rows
                  "┌───────┐     ┌─────┐"
                  "│ Start ├────►│ End │"
                  "└───────┘     └─────┘"))))

(ert-deftest mermaid-text-test-flow-edge-labels ()
  "Edge labels sit beside a vertical edge and on a horizontal one."
  (should (equal (mermaid-text-tests--draw "graph TD\nA -->|yes| B")
                 (mermaid-text-tests--rows
                  "┌───┐"
                  "│ A │"
                  "└─┬─┘"
                  "  │"
                  "  │ yes"
                  "  ▼"
                  "┌───┐"
                  "│ B │"
                  "└───┘")))
  (should (equal (mermaid-text-tests--draw "graph LR\nA -- go --> B")
                 (mermaid-text-tests--rows
                  "┌───┐      ┌───┐"
                  "│ A ├─ go ►│ B │"
                  "└───┘      └───┘"))))

(ert-deftest mermaid-text-test-flow-shapes ()
  "Circles and rounded nodes have round corners; a decision has < and > sides."
  (should (equal (mermaid-text-tests--draw "graph TD\nA((c)) --> B{d}\nB --> C(r)")
                 (mermaid-text-tests--rows
                  "╭───╮"
                  "│ c │"
                  "╰─┬─╯"
                  "  │"
                  "  │"
                  "  ▼"
                  "┌───┐"
                  "< d >"
                  "└─┬─┘"
                  "  │"
                  "  │"
                  "  ▼"
                  "╭───╮"
                  "│ r │"
                  "╰───╯"))))

(ert-deftest mermaid-text-test-flow-edge-styles ()
  "Dotted and thick edges use their own characters; ends can be marked."
  (let ((out (mermaid-text-tests--draw "graph TD\nA -.-> B\nA ==> C\nA --x D\nA <--> E")))
    (should (string-match-p "┆" out))
    (should (string-match-p "┃" out))
    (should (string-match-p "✕" out))
    (should (string-match-p "▲" out))))

(ert-deftest mermaid-text-test-flow-cycle ()
  "An edge back to an earlier node is drawn pointing up beside the other."
  (let ((out (mermaid-text-tests--draw "graph TD\nA --> B\nB --> A")))
    (should (string-match-p "▼" out))
    (should (string-match-p "▲" out))
    (should (string-match-p "A" out))
    (should (string-match-p "B" out))))

(ert-deftest mermaid-text-test-flow-fan-out-and-long-edge ()
  "Every node is drawn, with an arrow into each, and a skipping edge is routed."
  (let ((out (mermaid-text-tests--draw
              (string-join '("graph TB"
                             "A[Commit] --> B[Build] & C[Lint] & D[Test]"
                             "B --> E[Package]" "C --> E" "D --> E"
                             "A -- skips --> E"
                             "E == deploy ==> F[Production]")
                           "\n"))))
    (dolist (name '("Commit" "Build" "Lint" "Test" "Package" "Production" "skips" "deploy"))
      (should (string-match-p name out)))
    ;; A->B, A->C, A->D, B->E, C->E, D->E, A->E and E->F.
    (should (= 8 (cl-count ?▼ out)))))

(ert-deftest mermaid-text-test-flow-edges-turn-round ()
  "An edge turns on rounded corners, a box keeps its square ones."
  (let ((out (mermaid-text-tests--draw "graph TD\nA --> B & C")))
    (should (string-match-p "╭" out))
    (should (string-match-p "╮" out))
    (should (string-match-p "┌" out))))

(defun mermaid-text-tests--lines (&rest lines)
  "Return LINES, polylines of (X . Y), drawn on a canvas as a string."
  (let ((cv (mermaid-text--canvas)))
    (dolist (points lines)
      (mermaid-text--draw-line cv points))
    (mermaid-text--draw-hops cv)
    (string-join (mermaid-text--lines cv) "\n")))

(ert-deftest mermaid-text-test-flow-crossing-hops ()
  "Lines crossing are told from lines joining: the horizontal one stops short."
  (should (equal (mermaid-text-tests--lines '((3 . 0) (3 . 2)) '((0 . 1) (6 . 1)))
                 (mermaid-text-tests--rows "   │" "──╴│╶──" "   │")))
  ;; Drawn the other way round, too.
  (should (equal (mermaid-text-tests--lines '((0 . 1) (6 . 1)) '((3 . 0) (3 . 2)))
                 (mermaid-text-tests--rows "   │" "──╴│╶──" "   │")))
  ;; A line ending on another still joins it.
  (should (equal (mermaid-text-tests--lines '((3 . 0) (3 . 2)) '((0 . 1) (3 . 1)))
                 (mermaid-text-tests--rows "   │" "───┤" "   │"))))

(ert-deftest mermaid-text-test-flow-multiline-labels ()
  "<br> breaks a label into lines, which are centred."
  (should (equal (mermaid-text-tests--draw "graph TD\nA[one<br/>three]")
                 (mermaid-text-tests--rows
                  "┌───────┐"
                  "│  one  │"
                  "│ three │"
                  "└───────┘"))))

(ert-deftest mermaid-text-test-flow-ignores-styling ()
  "Styling lines, comments and front matter do not matter."
  (should (equal (mermaid-text-tests--draw
                  "---\ntitle: x\n---\n%% note\ngraph TD\nA --> B\nstyle A fill:#f9f\nclassDef c fill:#fff")
                 (mermaid-text-tests--draw "graph TD\nA --> B"))))

(ert-deftest mermaid-text-test-flow-big ()
  "A chart of 30 nodes and 110 edges, as dense as a large module graph, is drawn."
  (let ((out (mermaid-text-tests--draw
              (string-join
               (cons "graph TD"
                     (cl-loop for a below 30 nconc
                              (cl-loop for b from (1+ a) below (min 30 (+ a 5))
                                       collect (format "n%d --> n%d" a b))))
               "\n"))))
    (should out)
    (should (string-match-p "n29" out))
    (should (= 110 (cl-count ?▼ out)))))

(ert-deftest mermaid-text-test-flow-unsupported ()
  "What cannot be drawn faithfully gives nil, so the source is shown."
  (should-not (mermaid-text-tests--draw "graph TD\nsubgraph a\nsubgraph b\nX\nend\nend"))
  (should-not (mermaid-text-tests--draw "graph TD\nA --> A"))
  (should-not (mermaid-text-tests--draw "graph TD\nA ?? B"))
  (should-not (mermaid-text-tests--draw "gantt\ntitle x"))
  (should-not (mermaid-text-tests--draw "")))

;;; Subgraphs

(ert-deftest mermaid-text-test-subgraph-frame ()
  "A subgraph is a dashed frame, with its title, around its nodes."
  (should (equal (mermaid-text-tests--draw "graph TD\nsubgraph one\nA --> B\nend")
                 (mermaid-text-tests--rows
                  "┌╌ one ╌┐"
                  "╎       ╎"
                  "╎ ┌───┐ ╎"
                  "╎ │ A │ ╎"
                  "╎ └─┬─┘ ╎"
                  "╎   │   ╎"
                  "╎   │   ╎"
                  "╎   ▼   ╎"
                  "╎ ┌───┐ ╎"
                  "╎ │ B │ ╎"
                  "╎ └───┘ ╎"
                  "╎       ╎"
                  "└╌╌╌╌╌╌╌┘"))))

(ert-deftest mermaid-text-test-subgraph-title-and-crossing-edges ()
  "`id [Title]' sets the title, and edges cross the frame to reach outside."
  (let ((out (mermaid-text-tests--draw
              (string-join '("graph TD" "Dev --> PR"
                             "subgraph ci [Continuous integration]" "Build --> Test" "end"
                             "PR --> Build" "Test --> Deploy")
                           "\n"))))
    (should (string-match-p "┌╌+│? Continuous integration ╌+┐" out))
    (should-not (string-match-p "\\bci\\b" out))
    ;; Dev, PR and Deploy stay outside the frame.
    (let ((rows (split-string out "\n")))
      (dolist (name '("Dev" "PR" "Deploy"))
        (should (cl-find-if (lambda (r) (and (string-match-p name r)
                                             (not (string-match-p "╎" r))))
                            rows)))
      (dolist (name '("Build" "Test"))
        (should (cl-find-if (lambda (r) (and (string-match-p name r) (string-match-p "╎" r)))
                            rows))))
    ;; The edges are not cut by the frame.
    (should (= 4 (cl-count ?▼ out)))))

(ert-deftest mermaid-text-test-subgraph-two-side-by-side ()
  "Several subgraphs are framed separately."
  (let ((out (mermaid-text-tests--draw
              (string-join '("graph LR"
                             "subgraph front [Frontend]" "UI --> Store" "end"
                             "subgraph back [Backend]" "API --> DB" "end"
                             "Store --> API")
                           "\n"))))
    (should (string-match-p "┌╌ Frontend ╌+┐ ┌╌ Backend ╌+┐" out))
    (should (string-match-p "UI.*Store.*API.*DB" out))))

(ert-deftest mermaid-text-test-subgraph-width ()
  "Frames are cut short with their titles when the width is tight."
  (let ((out (mermaid-text-tests--draw
              (string-join '("graph TD" "Dev --> PR"
                             "subgraph ci [Continuous integration]" "Build --> Test[Run tests]"
                             "end" "PR --> Build" "Test --> Deploy")
                           "\n")
              20)))
    (should out)
    (should (<= (mermaid-text-tests--width out) 20))
    (should (string-match-p "┌╌ Con…" out))))

(ert-deftest mermaid-text-test-subgraph-unsupported ()
  "Nested subgraphs, edges to a subgraph and a subgraph with a foreign node
in its way are not drawn."
  (should-not (mermaid-text-tests--draw "graph TD\nsubgraph a\nsubgraph b\nX\nend\nend"))
  (should-not (mermaid-text-tests--draw "graph TD\nsubgraph one\nA\nend\nX --> one"))
  (should-not (mermaid-text-tests--draw "graph TD\nsubgraph s\nA\nB\nend\nA --> C --> B"))
  (should-not (mermaid-text-tests--draw "graph TD\nsubgraph s\nA\n")))

;;; Width

(ert-deftest mermaid-text-test-flow-width-keeps-left-right-when-it-fits ()
  "A left-to-right chart that fits the width stays on one row."
  (let ((out (mermaid-text-tests--draw "flowchart LR\nA[One] --> B[Two] --> C[Three]" 100)))
    (should (equal out (mermaid-text-tests--draw "flowchart LR\nA[One] --> B[Two] --> C[Three]")))
    (should (= 3 (length (split-string out "\n"))))))

(ert-deftest mermaid-text-test-flow-width-turns-left-right-top-down ()
  "A left-to-right chart too wide for the width is drawn top-down instead."
  (let ((out (mermaid-text-tests--draw "flowchart LR\nA[One] --> B[Two] --> C[Three]" 20)))
    (should (equal out (mermaid-text-tests--rows
                        " ┌─────┐"
                        " │ One │"
                        " └──┬──┘"
                        "    │"
                        "    │"
                        "    ▼"
                        " ┌─────┐"
                        " │ Two │"
                        " └──┬──┘"
                        "    │"
                        "    │"
                        "    ▼"
                        "┌───────┐"
                        "│ Three │"
                        "└───────┘")))
    (should (<= (mermaid-text-tests--width out) 20))))

(ert-deftest mermaid-text-test-flow-width-wraps-labels ()
  "Long labels are wrapped until the chart fits."
  (let* ((src "graph TD\nA[a rather long description of the first step] --> B[short]")
         (wide (mermaid-text-tests--draw src))
         (narrow (mermaid-text-tests--draw src 24)))
    (should (> (mermaid-text-tests--width wide) 24))
    (should (<= (mermaid-text-tests--width narrow) 24))
    (dolist (word '("rather" "description" "first" "step"))
      (should (string-match-p word narrow)))))

(ert-deftest mermaid-text-test-width-too-small ()
  "A diagram that cannot be made to fit gives nil."
  (should-not (mermaid-text-tests--draw "graph TD\nA[Start] --> B[End]" 5))
  (should-not (mermaid-text-tests--draw "sequenceDiagram\nAlice->>Bob: hello there" 10))
  (should-not (mermaid-text-tests--draw "pie\n\"a\" : 1" 6)))

;;; Sequence diagrams

(ert-deftest mermaid-text-test-sequence-messages ()
  "Participants are columns; solid and dashed arrows run between them."
  (should (equal (mermaid-text-tests--draw "sequenceDiagram\nA->>B: hi\nB-->>A: yo")
                 (mermaid-text-tests--rows
                  " ┌───┐  ┌───┐"
                  " │ A │  │ B │"
                  " └─┬─┘  └─┬─┘"
                  "   │  hi  │"
                  "   ├─────►┤"
                  "   │  yo  │"
                  "   ├◄┄┄┄┄┄┤"))))

(ert-deftest mermaid-text-test-sequence-autonumber-and-notes ()
  "Autonumber prefixes messages, `participant X as Y' renames, notes are boxes."
  (should (equal (mermaid-text-tests--draw
                  "sequenceDiagram\nautonumber\nparticipant A as Alice\nA->>B: hi\nNote over A,B: ok")
                 (mermaid-text-tests--rows
                  " ┌───────┐  ┌───┐"
                  " │ Alice │  │ B │"
                  " └───┬───┘  └─┬─┘"
                  "     │ 1. hi  │"
                  "     ├───────►┤"
                  "   ┌────────────┐"
                  "   │     ok     │"
                  "   └────────────┘"))))

(ert-deftest mermaid-text-test-sequence-blocks ()
  "loop/alt blocks are framed, with a dashed rule for else."
  (let ((out (mermaid-text-tests--draw
              (string-join '("sequenceDiagram" "A->>B: x"
                             "alt ok" "B->>A: y" "else bad" "B-xA: z" "end")
                           "\n"))))
    (should (string-match-p "┌─ alt ok ─[─┼]*┐" out))
    (should (string-match-p "┄ bad ┄" out))
    (should (string-match-p "└─+┼─+┼─+┘" out))
    (should (string-match-p "✕" out))))

(ert-deftest mermaid-text-test-sequence-self-message ()
  "A message to oneself loops back to the same lifeline."
  (let ((out (mermaid-text-tests--draw "sequenceDiagram\nA->>A: think")))
    (should (string-match-p "├───┐ think" out))
    (should (string-match-p "┤◄──┘" out))))

(ert-deftest mermaid-text-test-sequence-width ()
  "Labels are wrapped and the diagram squeezed to fit the width."
  (let* ((src "sequenceDiagram\nAlice->>Bob: a fairly long message to send over\nBob-->>Alice: ok")
         (wide (mermaid-text-tests--draw src))
         (narrow (mermaid-text-tests--draw src 30)))
    (should (> (mermaid-text-tests--width wide) 30))
    (should (<= (mermaid-text-tests--width narrow) 30))
    (dolist (word '("fairly" "message" "send" "over"))
      (should (string-match-p word narrow)))))

(ert-deftest mermaid-text-test-sequence-unsupported ()
  "A line it does not understand makes the whole diagram unsupported."
  (should-not (mermaid-text-tests--draw "sequenceDiagram\nA->>B: hi\nbox Blue\nend"))
  (should-not (mermaid-text-tests--draw "sequenceDiagram\nA->>B: hi\nend")))

;;; Pie charts and state diagrams

(ert-deftest mermaid-text-test-pie ()
  "Bars are scaled to the biggest slice and labelled with their share."
  (should (equal (mermaid-text-tests--draw "pie title T\n\"a\" : 3\n\"b\" : 1")
                 (mermaid-text-tests--rows
                  "T"
                  "a  ██████████████████████████████ 75.0%"
                  "b  ██████████                     25.0%")))
  (should (string-match-p "(386)" (mermaid-text-tests--draw "pie showData\n\"Dogs\" : 386"))))

(ert-deftest mermaid-text-test-pie-width ()
  "The bars shrink to the width."
  (should (equal (mermaid-text-tests--draw "pie\n\"alpha\" : 3\n\"beta\" : 1" 24)
                 (mermaid-text-tests--rows
                  "alpha  ███████████ 75.0%"
                  "beta   ███▋        25.0%"))))

(ert-deftest mermaid-text-test-state ()
  "A state diagram is drawn as rounded states joined by transitions."
  (should (equal (mermaid-text-tests--draw "stateDiagram-v2\n[*] --> A\nA --> [*]: done")
                 (mermaid-text-tests--rows
                  "╭───╮"
                  "│ ● │"
                  "╰─┬─╯"
                  "  │"
                  "  │"
                  "  ▼"
                  "╭───╮"
                  "│ A │"
                  "╰─┬─╯"
                  "  │"
                  "  │ done"
                  "  ▼"
                  "╭───╮"
                  "│ ◉ │"
                  "╰───╯")))
  (should-not (mermaid-text-tests--draw "stateDiagram-v2\nstate X {\nA --> B\n}")))

(provide 'mermaid-text-tests)
;;; mermaid-text-tests.el ends here
