# ElixirSense Types Audit Tasks

Fifth review date: 2026-06-12.

Worktree: `/Users/lukaszsamson/elixir_sense/.claude/worktrees/trusting-wu-d1f603`.

Inputs consolidated:
- `ELIXIR_SENSE_TYPES_GPT.md` fourth review.
- `ELIXIR_SENSE_TYPES_FABLE.md` 2026-06-12 architectural/deep-architecture waves.
- `/Users/lukaszsamson/elixir_sense/ELIXIR_SENSE_TYPES_GEMINI.md`.
- Elixir private typesystem under `/Users/lukaszsamson/elixir/lib/elixir/lib/module`.

Goal: use binding shapes as a starting point, progressively enrich them with Elixir inferred types, and display types in the same representation used by Elixir type warnings while keeping private `Module.Types` details behind a replaceable abstraction.

## Status

The backlog has moved from core correctness into release evidence and long-term compatibility. No inherited GPT/Fable/Gemini P0 correctness bug remains open in the current worktree.

Done in current code:
- `ElixirSense.Core.TypeHints` is the LSP-facing facade: `request_context/1`, `discard/1`, `type_hint_for_var/4`, flow-sensitive `type_hint_at/4`, `effective_params/4`, and `trust_rank/1`.
- Type hint provenance is explicit: `:native_exck` > `:native_inferred` > `:spec` > `:shape`.
- Request-scoped caches cover env lookup, local signatures, effective params, and flow-sensitive read hints; long-lived callers have an explicit `discard/1` lifecycle.
- Strong/infer apply behavior is parity-tested against compiler-observed ExCk behavior; strong returns wrap in `dynamic()` only for gradual args.
- Private `Module.Types` usage is capability-gated, drift-canary-tested, and memoizes immutable capability probes in `:persistent_term`.
- Map shapes now have the three-marker tail model: `:closed` literal-complete, `nil` partial, and `:open` open/unknown-base.
- Improper lists are represented as `{:nonempty_list, elem, tail}` and render with compiler spelling `non_empty_list(elem, tail)`.
- Spec fallback no longer reparses strings; it consumes compiler-expanded quoted spec AST.
- `fields_for_receiver/2` merges descriptor-derived fields with structural fields.
- `ElixirSense.Core.ModuleResolver` consolidates the pure AST module resolver paths over the canonical alias-expansion engine.
- Shape vocabulary and sentinel meanings are documented and pinned by contract tests.
- Guard/rescue precision improved for `is_function(x, arity)`, `is_exception/1,2`, and rescue variables.
- Large-file benchmark data exists: native type hints are faster on the measured hint path than the structural fallback.

Gemini/Fable reconciliation:
- Fixed: strong return wrapping, request-scoped local signature caching, facade boundary, richer trust levels, private API drift canaries, map open/closed fidelity, improper lists, descriptor-aware fields, flow-sensitive read API, string-reparse spec fallback, and local inference O(n²) work.
- Refuted: Gemini's nested-union P0 does not reproduce on the current expansion path.
- Still relevant: shape algebra drift from compiler `Descr`, release evidence across supported Elixir versions, and minimizing private API leakage until Elixir ships a public typesystem API.

## P0 - Release Gates

- [ ] Run and record the full supported-version behavior matrix before publishing ElixirSense for ElixirLS.
  - Current local gates are strong, but release confidence needs recorded behavior results on each supported Elixir minor.
  - Cover `Expr.of_expr`, `Pattern.of_match`, `Pattern.of_head`, `Pattern.of_domain`, `Descr.to_quoted_string`, ExCk decoding, signature application, map closedness, improper lists, and hint rendering.
  - Keep 1.20-only paths version-gated, especially machinery requiring `Pattern.of_domain` / `:previous`.

- [ ] Keep private API drift fail-closed as a hard invariant.
  - Any new `apply/3` against `Module.Types.*` needs a capability probe, guarded fallback, and drift test.
  - If an arity, return shape, or descriptor contract changes, disable only that native capability and return structural best effort.
  - Never emit `:native_exck` or `:native_inferred` from unverified descriptors or signatures.

- [ ] Preserve `TypeHints` as the only editor-facing boundary.
  - ElixirLS consumes the facade; keep new inlay/hover/completion type features behind it.
  - Do not expose `Binding`, raw ExCk chunks, `TypePresentation`, native signature tuples, or private descriptor data to ElixirLS.
  - This is the abstraction to delete or replace when Elixir exposes a public typesystem API.

## P1 - Highest-Value Next Work

- [ ] Finish module-resolution consolidation decisions.
  - `ModuleResolver` covers the pure AST resolver overlap and has equivalence tests.
  - Remaining deliberate decision: whether to unify `Binding.resolve_same_root_alias` with the canonical fallback, since it can change semantics.
  - `TypeHints` receiver consolidation is still blocked on passing enough raw AST/context through Binding.

- [ ] Extend descriptor-backed algebra only where compiler primitives are semantically safe.
  - `combine_intersection` is partially delegated; subtraction/difference remains custom because dynamic-wrapper distribution is not equivalent.
  - Continue auditing map/list/tuple union, `covers?`, disjointness, compatibility, and difference against `Descr` behavior before delegating.
  - Keep structural fallback for shapes without verified descriptors.

- [ ] Keep map-tail semantics coherent across all producers and consumers.
  - `:closed`, `nil` partial, and `:open` are now distinct internally, but `:closed` and partial intentionally render the same for now.
  - Any new map producer must choose a tail based on construction vs pattern/guard semantics, not display convenience.
  - Add regression tests for new `Map.*`, struct, guard, and pattern paths as they appear.

- [ ] Keep spec fallback small and lower-trust.
  - The reparse bug is fixed, but `spec_ast_to_shape` is still a lossy compatibility layer.
  - Add common remote/user type patterns only where they are safe.
  - Spec-derived results must stay at `:spec` trust and never outrank ExCk/native facts.

## P2 - Fidelity And Coverage

- [ ] Expand compiler-comparison fixtures.
  - Add more behavior fixtures for guards, `case`, `with`, `try`, `receive`, `for`, structs, optional maps, binaries, captures, functions, generated code, and remote dependency calls.
  - Compare through `Descr.to_quoted_string/2` or actual compiler warning output where possible.

- [ ] Keep local inference explicitly best-effort.
  - Coverage is broad, but this is still not the compiler's full `Module.Types` local flow.
  - Keep non-ExCk local inference at `:native_inferred` or lower as appropriate.
  - Document cases where source-position metadata cannot represent all generated/default heads.

- [ ] Decide whether first-class gradual/dynamic shapes are needed.
  - Native `dynamic()` is currently unwrapped at the shape boundary; `{:dynamic, _}` is display-only legacy.
  - If hover/completion eventually need gradual semantics, introduce a real algebra-aware dynamic shape deliberately.
  - Do not let accidental `{:dynamic, _}` shapes silently fall through to unknown.

- [ ] Decide whether nested provenance aggregation is worth the cost.
  - `TypeHints` intentionally attributes only top-level call thunks.
  - Containers that include native-backed calls still report structural provenance.
  - Keep this policy unless a user-facing trust/filtering case justifies recursive attribution.

## P3 - Performance And Cleanup

- [ ] Turn benchmark data into a repeatable regression check.
  - Round-4 data closed the immediate benchmark question, but a reusable fixture/threshold would catch future regressions.
  - Include metadata build cost and inlay request cost with native typing enabled/disabled.

- [ ] Review non-LSP internal callers for repeated local inference work.
  - The LSP request path is cached through `TypeHints`.
  - Hover, completion, and other callers may still invoke `Binding.from_env` or local signature construction repeatedly.

- [ ] Keep adapter policy documented near new code.
  - Private compiler mirrors must stay guarded and tested.
  - Spec-derived signatures are lower-trust fallbacks.
  - Inlay hints widen literal leaves for readability while hover/completion may keep structural precision.

## Closed Or Refuted

- [x] Strong-signature return wrapping bug: fixed.
- [x] Per-hint `build_local_sigs_map` hot path for LSP: fixed through `TypeHints.request_context/1`.
- [x] Richer trust/source levels: fixed with `:native_exck`, `:native_inferred`, `:spec`, `:shape`, and `trust_rank/1`.
- [x] Private API drift canaries and fail-closed guards: added; keep invariant for future calls.
- [x] Pattern AST refinement overriding native results: fixed; native results are authoritative and AST refinement fills gaps only.
- [x] Tooltip/source rendering and literal widening for hints: fixed.
- [x] Nested union flattening Gemini P0: refuted for the current expansion path.
- [x] Open/closed/partial map semantics: fixed with `:closed` / `nil` / `:open` map tails.
- [x] Map update on unknown base collapsing to closed map: fixed by producing open maps.
- [x] Improper-list modeling: fixed with `{:nonempty_list, elem, tail}`.
- [x] Descriptor-aware `fields_for_receiver/2`: fixed.
- [x] Flow-sensitive read-position type API: fixed with `TypeHints.type_hint_at/4`.
- [x] Spec fallback string reparse: removed.
- [x] Shape vocabulary documentation and contract tests: added.
- [x] Local inference O(n²) clause reinference: fixed with one module-completion pass.
- [x] TypeHints cache lifecycle: fixed with `discard/1`.
- [x] Capability probe hot path: fixed with `:persistent_term` memoization.
- [x] Guard/rescue precision wave: fixed for `is_function/2`, `is_exception/1,2`, and verified rescue cases.
- [x] Initial large-file benchmark question: closed with measured data.
