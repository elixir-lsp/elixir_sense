# ElixirSense types integration — consolidated backlog (Fable)

Third review pass, 2026-06-11. This file is now the SINGLE prioritized backlog for the
elixir_sense side, consolidating:
- ELIXIR_SENSE_TYPES_GPT.md (second GPT review, same date)
- ELIXIR_SENSE_TYPES_GEMINI.md (Gemini review, lives in the main checkout)
- the two earlier Fable audit/fix rounds (full details in git history of this file:
  commits a4082a28, da4cba36)

Every inherited claim below was re-verified against the current worktree and the Elixir
1.20.1 sources (`~/elixir/lib/elixir/lib/module/types/`), empirically where possible.
Gates at review time: 1756/1756 tests · compile --warnings-as-errors · format ·
credo --all --strict all green.

## Verification verdicts on second-round claims

| Claim (source) | Verdict |
|---|---|
| `apply_strong_mirror` always dynamic-wraps returns; compiler `Apply.return/3` wraps only when an arg is gradual (GPT P0, Gemini P0) | **Confirmed but downgraded to P2.** Divergence is real (`elixir_types.ex` wrap_dynamic vs `apply.ex:1732-1741`), but Gemini's harm claim is refuted: every Binding consumer runs `to_shape/1`, which unwraps `dynamic(inner)` → `inner`, so shapes are fully preserved. Residual effect is cosmetic (`descr_to_string` shows `dynamic(:ok)` where the compiler would show `:ok`). |
| `flatten_unions/1` is one-level; spec-parsed `a \| b \| c` stays nested (Gemini P0) | **Refuted.** `spec_ast_to_shape` does produce right-nested unions, but `do_expand({:union, members})` recursively expands each member before `normalize_union`, so nesting collapses fully (verified empirically: 3-deep union flattens and dedupes; all other `normalize_union` callers receive pre-expanded members). |
| `build_local_sigs_map/2` recomputed per `Binding.from_env/3`, no cache (Gemini perf) | **Confirmed, P1 when combined with the LSP per-hint pattern.** Cost-model correction: it does NOT parse specs at request time (sigs are precomputed into `SpecInfo.elixir_types_sig` at metadata build); the per-call cost is an O(N) scan of `mods_funs_to_positions` + map build. The LSP provider calls `from_env` once per hint → O(hints × N) per request. |
| `elixir_types_sig_source` provenance is written but never consulted (GPT P0) | **Confirmed, P2.** No reader outside struct defs/write sites. Mitigating accident: spec sigs are `:infer` (never `:strong`), so the kind-based precedence in `build_local_sigs_map` already yields native-inferred-wins-then-spec — the intended trust order. The `{_, {:ok, {:strong, ...}}}` spec-precedence clause is dead code. |
| Fresh-bug sweep of the da4cba36 round | **No new bugs.** `coerce_var_type(nil)` unaffected by the new `coerce_static_descr` clauses; ExCk `:ensure_table` send fires only on the recovery path (no flood; stray messages drained); mismatched-arity clauses are excluded correctly. One observation: `args_compatible?` rescues an internal Descr `CaseClauseError` to `true` (conservatively keeps the clause) — robustness fallback, acceptable. |

## P1 — DONE (fix wave, 2026-06-11 evening)

All four P1 items shipped (commit follows this update):
- **1.1/1.2** `ElixirSense.Core.TypeHints` facade: `request_context/1`,
  `type_hint_for_var/4`, `effective_params/4` with request-scoped process-dict caching
  of the local-sigs map, env resolution, and params. `Binding.from_env/4` gained an
  additive `local_sigs:` option. elixir-ls now consumes ONLY this facade (no
  Binding/TypePresentation references remain in the provider).
- **1.3** Apply parity harness (`elixir_types_apply_parity_test.exs`): real ExCk chunks
  from `Code.compile_string` with `infer_signatures: true`; local-caller returns used as
  compiler ground truth; 10 scenarios, ZERO divergences found. Ground-truth correction
  discovered: `apply_infer` wraps unconditionally; only `apply_strong` routes through
  `return/3`'s conditional wrap.
- **1.4** `build_local_sigs_map` precedence is now explicit provenance rank
  (`:exck` > `:inferred` > `:spec`); dead `:strong`-spec clause deleted; regression test
  locks that a `:strong`-tagged spec sig cannot outrank native-inferred.
- **2.1** (pulled forward) strong-sig returns wrap in `dynamic()` only when an argument
  is gradual, mirroring `Apply.return/3`; infer-sig unconditional wrap retained
  (matches apply.ex:1827).

## P1 (historical) — Highest-value next work

### 1.1 Per-request local-sigs memoization (cross-repo perf, with LSP item 1.1)
`Binding.from_env/3` (`binding.ex:31-37`) rebuilds the local sigs map on every call and
the LSP provider calls it once per hint. Either memoize `build_local_sigs_map` keyed by
metadata identity (process-dictionary scoped to the request, or a field on Metadata) or
solve it structurally via the facade (1.2), which can build env+binding once per
request. [Gemini, verified]

### 1.2 LSP-facing facade: `type_hint_for_var(metadata, position, var, opts)`
The agreed target seam (GPT HP, Gemini P0, LSP-side audits all converge): one
elixir_sense entry point that assembles env + binding + render policy and returns
`:skip | {:ok, %{label, full, source}}`. Removes `Binding.from_env`/`TypePresentation`
knowledge from elixir-ls, gives a natural home for per-request caching (1.1), and is
the single swap point when Elixir ships a public typesystem API. Add sibling entry
points incrementally (`render_compiler_type`, descriptor-aware `fields_for_receiver`)
— do NOT attempt the full facade big-bang. [GPT + Gemini]

### 1.3 Apply-mirror parity test harness
`apply_signature/2` is a faithful but drift-prone hand mirror of private `Apply` rules.
Add parity tests that compile tiny fixture modules, read their real ExCk sigs, and
compare `apply_signature` outcomes against compiler-observed behavior (static args,
gradual args, zero matches, >16 clauses, unions, optional-key maps, funs; `:infer` and
`:strong`). This is the guard that lets the mirror survive Elixir minor releases. [GPT]

### 1.4 Consult provenance explicitly in sig precedence
Make `build_local_sigs_map` order by `elixir_types_sig_source` (`:exck` > `:inferred` >
`:spec`) instead of relying on the kind-tag accident; delete the dead `:strong`-spec
precedence clause. Optionally surface the richer source in `render_hint/3`
(`:native_exck | :native_inferred | :spec | :shape`) so the LSP `minimumTrust` can
filter precisely (LSP item 2.1). [GPT, verified P2 but cheap and unblocks LSP P2]

## P2 — Fidelity and architecture

- **2.1 Strong/infer return-wrapping parity** — track arg gradualness through
  `apply_signature` and only `wrap_dynamic` when an arg is gradual, mirroring
  `Apply.return/3`. Effect is display-only today (shapes unwrap), so do it together
  with 1.3's parity harness. [GPT + Gemini, verified P2]
- **2.2 Descr-backed algebra where descriptors exist** — delegate union/intersection/
  difference/disjointness to `Descr` when both operands carry descriptors; shapes stay
  the fallback. Start with the highest-traffic sites (`combine_intersection`,
  `difference`) rather than a wholesale swap. [GPT + Gemini]
- **2.3 Open/closed/partial map and struct metadata** — record which map shapes are
  known-closed so coercion can stop defaulting everything to open maps; required-key
  facts are currently lost. [GPT]
- **2.4 Improper-list modeling or explicit degradation** — affects `++`, cons patterns,
  remote arg selection, display (`non_empty_list(head, tail)` currently → nil). [GPT]
- **2.5 Module/function resolution consolidation** — `module_from_ast/2` vs
  `Introspection.actual_mod_fun/6` vs binding call expansion. One metadata-aware path.
  [GPT]
- **2.6 Descriptor-aware `fields_for_receiver/2`** — optional/required/domain keys from
  descriptors, shapes as fallback. [GPT]
- **2.7 Default-argument signature tests** — arity-keyed native sigs vs default-generated
  heads, local and remote. [GPT]
- **2.8 Version-matrix behavior tests** — 1.18/1.19 paths are probed but only 1.20 is
  behavior-tested locally; needs CI on multiple Elixir versions (the ci.yml matrix
  exists — gate native tests appropriately). [GPT]
- **2.9 Explicit top/unknown/dynamic/bottom/absent distinctions in shape data** — `nil`,
  `:none`, `:not_set`, `{:dynamic, _}` are still overloaded across expansion/rendering/
  suppression. Large; design doc first. [GPT]

## P3 — Performance, cleanup, docs

- **3.1 Benchmark native typing on large files** (parse/metadata paths pay the cost
  server-wide, not just inlay hints). [GPT + earlier Fable #39/#40: per-clause
  re-inference O(n²) and clause-AST retention in ModFunInfo are still open]
- **3.2 `spec_ast_to_shape` stdlib typespec coverage** — keep expanding the well-known
  table or (better) fold into the long-term plan to retire the string-reparse path
  (earlier Fable #32). [Gemini P2]
- **3.3 Docs/comments policy sync** — spec sigs are provenance-tagged fallbacks;
  `render_hint/3` widens literals for hints only; `apply_signature/2` is a mirror, not
  a public API. [GPT]
- **3.4 Generated-var filter refinement** (earlier Fable #38) and `declare_var`
  delegation (earlier Fable #34) — unchanged, still open.

## Closed this pass (no action needed)

- Gemini "flatten_unions" P0 — refuted with empirical evidence (see verdicts).
- Gemini "ExCk :ensure_table flood" — refuted (send only on recovery path).
- da4cba36 fresh-bug sweep — clean.
- All items listed as Done in the status blocks of ELIXIR_SENSE_TYPES_GPT.md (apply
  mirror, spec-sig downgrade + provenance fields, pattern-refinement inversion,
  optional-key preservation incl. if_set coercion, literal widening, source-aware
  render_hint/3, compiler-parity + failure-mode suites, ExCk version pinning/padding/
  ETS owner with on-demand table recreation, subtraction precision gates, conservative
  intersection fallback, descr_to_string rendering seam).
