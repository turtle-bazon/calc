# calc Improvement Plan — as cl-toolkit Field Exercise

*Baseline: 287/287 unit checks green · 62/62 integration · evaluator.lisp
1087 lines / 42 defuns · 8 functions ≥50 lines · uncommitted lambda-param
fix in evaluator.lisp*

Every mutation below routes through cl-toolkit; every phase ends with the
same gate chain: `validate` → `lint` → `make test` → `test-integration`.
Phases are ordered so each de-risks the next.

---

## Phase 0 — Housekeeping (15 min)

| Task | Toolkit angle |
|---|---|
| Commit the pending lambda-param validation fix | plain git, but verify the touched span first with `check-anchor` |
| Delete stray `calc.asd.bak` | — |
| Bump `calc.asd` version 0.2.0 → 0.3.0 (feature reality: arrays, strings, lambdas, macros, stats) | `patch-span --find-old` |
| Add `*.bak` / `build/` hygiene check to .gitignore if missing | — |

**Exit criteria:** clean tree, suite green, version honest.

## Phase 1 — process-expression decomposition (the redo, finally unblocked)

90 lines, 6-way cond with inline bodies (comment, assignment, defmacro,
defun, array-literal, default-eval). This failed twice manually; the
atomic tooling now exists.

Protocol per clause (repeat ×4 for the extractable ones):

```bash
# 1. Verify anchor uniqueness BEFORE anything
cl-toolkit check-anchor -f src/processor.lisp --text "<clause head>"

# 2. Atomic promotion — cond clause, so --when is mandatory;
#    compile gate rolls back if the generated helper is illegal
cl-toolkit extract-clause -f src/processor.lisp --name process-expression \
  --match "<clause>" --as <verb-noun> --lambda-list "(tr vars funcs)" \
  --call "(<verb-noun> tr vars funcs)" --when \
  --compile-check --compile-check-package calc --write

# 3. Gates
cl-toolkit lint -f src/processor.lisp
make test && make test-integration
```

Expected helpers: `handle-comment`, `handle-assignment`,
`handle-defmacro`, `handle-defun-form` (~15-25 lines each).
process-expression becomes a ~25-line dispatcher.

**Known risk from field data:** the unbound-variable anomaly was
reproduction-script-shaped, not edit-shaped — and `--compile-check`
now covers the write side. If the anomaly reappears in `make test`,
it's the fiveam-context bug, not the edit.

**Toolkit angle under test:** `extract-clause --when --compile-check
--compile-check-package` end-to-end on its real target for the first
time; `--occurrence` if any clause head repeats.

## Phase 2 — evaluator.lisp: the four big handlers

Order by risk (lowest first — build confidence before the scary ones):

| Function | Lines | Extractable structure |
|---|---|---|
| `handle-string` | 54 | sub-ops per string op → `extract-clause` or `--match` swaps |
| `handle-stack` | 52 | same shape |
| `make-unary-func` | 67 | the DEG/RAD/SIGNUM/TAND clause family (insert-in territory) |
| `handle-array` | **126** | the monster: per-op cond, likely 3-4 extractions |
| `make-binary-func` | **97** | op-table driven; candidate for data-driven refactor (table + one generic path) |
| `handle-stats` | 87 | stats-op cond |
| `dispatch-control-flow` | 132 | DO/FOR/IF/BEGIN families — the historical minefield; do LAST |

Target: no function >60 lines in evaluator.lisp. Each extraction is one
`extract-clause --when` (cond bodies) or `insert-in` (splicing into
existing bodies).

## Phase 3 — dispatch-token + dispatch-control-flow consolidation

After Phase 2, the two dispatchers likely shrink or merge patterns.
Evaluate whether dispatch-control-flow's BEGIN/FOR/IF families want the
same treatment or are genuinely cohesive. **Decision point, not a
commitment** — cohesion is a valid outcome.

## Phase 4 — error-handling consistency sweep

`calc-error` is raised ad hoc. Sweep: every user-facing error path
raises `calc-error` with a message; no raw `error` in handler code.
Toolkit angle: `find-forms --contains "(error "` to enumerate raw
raises; `--match` swaps to `(error 'calc-error ...)`.

## Phase 5 — polish

- README feature list vs reality (`functions` command, arrays, IN,
  REVERSE — recent features undocumented?)
- REPL: `help` command listing special commands (mirrors `functions`)

---

## Success metrics

| Metric | Start | Target |
|---|---|---|
| Suite | 287/287 | 287+ green every phase |
| process-expression | 90 lines | ≤30 |
| evaluator functions ≥50 lines | 8 | ≤2 |
| Corruption incidents | 0 (post-0.3.0) | 0 |

## Meta: what this exercises in cl-toolkit

`extract-clause --when --compile-check-package` (first real-target run),
`--occurrence` (repeated clause heads), `insert-in` (body splices),
`find-forms` sweeps (Phase 4), `patch-span --find-old` (version bump),
and the full gate chain as scripted contract. Any friction found is
0.5.x feedback with a live repro.
