# ElixirSense Types Audit Tasks

Fourth review date: 2026-06-11.

Worktree: `/Users/lukaszsamson/elixir_sense/.claude/worktrees/trusting-wu-d1f603`.

Inputs consolidated:
- `ELIXIR_SENSE_TYPES_GPT.md` third review.
- `ELIXIR_SENSE_TYPES_FABLE.md` latest fix-wave/backlog.
- `/Users/lukaszsamson/elixir_sense/ELIXIR_SENSE_TYPES_GEMINI.md`.
- Elixir private typesystem under `/Users/lukaszsamson/elixir/lib/elixir/lib/module`.

Goal: use binding shapes as a starting point, progressively enrich them with Elixir inferred types, and display types in the same representation used by Elixir type warnings while keeping private `Module.Types` details behind a replaceable abstraction.

## Status

No confirmed P0 ElixirSense correctness bug remains from the GPT/Fable/Gemini findings in the current worktree. The remaining release risk is evidence and compatibility: prove the adapter behaves across supported Elixir versions, keep private API drift fail-closed, and do not let low-trust structural guesses look like compiler facts.

Done in current code:
- `ElixirSense.Core.TypeHints` is the LSP-facing facade: `request_context/1`, `type_hint_for_var/4`, `effective_params/4`, and `trust_rank/1`.
- `Binding.from_env/4` accepts precomputed `local_sigs`, and `TypeHints.request_context/1` avoids rebuilding local signatures per hint.
- Source/trust ordering is explicit: `:native_exck` > `:native_inferred` > `:spec` > `:shape`.
- `build_local_sigs_map/2` now ranks provenance explicitly: `:exck` > `:inferred` > `:spec`.
- `apply_signature/2` now mirrors strong-signature return wrapping: strong returns are wrapped in `dynamic()` only when at least one argument is gradual; inferred returns still wrap unconditionally.
- Private API drift is guarded by capability probes and drift-canary tests, including the `to_shape_eager` non-map guard.
- `combine_intersection` delegates to `Module.Types.Descr.intersection` for an exact-shape whitelist. Maps, scalar literals, and subtraction still deliberately fall back.
- Local inference best-effort behavior now has focused tests for recursion, mutual recursion, default heads, super/overridable, private reachability, disjoint guards, macro-generated heads, remote defaults, guard refinement, `with`, structs, captures, and pipelines.

Gemini/Fable reconciliation:
- Fixed: strong return wrapping, request-scoped local signature caching, facade boundary, richer trust levels, private API drift canaries.
- Refuted: Gemini's nested-union P0 does not reproduce on the current expansion path.
- Still relevant: shape algebra drift from compiler `Descr`, open/closed map fidelity, complex spec fallback, and multi-version behavior evidence.

## P0 - Release Gates

- [ ] Run and record the full supported-version matrix before publishing ElixirSense for ElixirLS.
  - Capability probes exist, but release confidence needs behavior results on each supported Elixir minor.
  - Cover `Expr.of_expr`, `Pattern.of_match`, `Pattern.of_head`, `Pattern.of_domain`, `Descr.to_quoted_string`, ExCk decoding, signature application, and hint rendering.
  - Keep 1.20-only checks version-gated, especially paths requiring `Pattern.of_domain` / `:previous`.

- [ ] Keep private API drift fail-closed as a hard invariant.
  - The current audit and canaries are good. Treat any new `apply/3` against `Module.Types.*` as requiring a capability probe, a guarded fallback, and a drift test.
  - If an arity, return shape, or descriptor contract changes, disable only that native capability and return structural best effort.
  - Never emit a `:native_exck` or `:native_inferred` source from unverified descriptors or signatures.

- [ ] Preserve `TypeHints` as the only editor-facing boundary.
  - ElixirLS now consumes the facade; keep new inlay/hover/completion type features behind it.
  - Do not expose `Binding`, raw ExCk chunks, `TypePresentation`, native signature tuples, or private descriptor data to ElixirLS.
  - This is the abstraction to delete or replace when Elixir exposes a public typesystem API.

## P1 - Highest-Value Next Work

- [ ] Record open/closed/partial map and struct semantics in shapes.
  - Current coercion defaults many maps to open and loaded structs to closed full-field maps.
  - That avoids false absence, but loses required-key and partial-map facts used by the compiler.
  - Add explicit metadata for known-open, known-closed, and partial maps/structs, then preserve it through conversion, intersection, presentation, and completions.

- [ ] Extend descriptor-backed algebra where compiler primitives are semantically safe.
  - `combine_intersection` is partially delegated; `difference`, map operations, scalar literals, `covers?`, disjointness, compatibility, and union normalization remain custom approximations.
  - Delegate only where properties match the shape operation. Do not delegate subtraction until the dynamic-wrapper behavior is proven equivalent for the intended operation.
  - Keep structural fallback for shapes without verified descriptors.

- [ ] Improve complex spec fallback, or narrow it behind compiler data.
  - `spec_ast_to_shape` remains a lossy fallback for user and stdlib remote types.
  - Add only safe, common patterns and keep all spec-derived results at `:spec` trust.
  - Prefer ExCk/native inferred signatures whenever available; specs must not outrank compiler facts.

- [ ] Make `fields_for_receiver/2` descriptor-aware.
  - Completion currently depends on expanded shapes.
  - Native descriptors can retain optional keys, required keys, domain keys, and struct fields that shapes may drop.
  - Route field extraction through the same checked adapter layer rather than adding completion-specific descriptor parsing.

- [ ] Consolidate module/function resolution.
  - `ElixirTypes.module_from_ast/2`, binding call expansion, completion, hover, and LSP parameter hints still overlap.
  - Route alias/module/function resolution through one metadata-aware path so local inference, remote calls, and editor providers agree.

## P2 - Fidelity And Coverage

- [ ] Model improper lists or explicitly degrade them.
  - Elixir distinguishes `list(t)`, `non_empty_list(t)`, and improper `non_empty_list(head, tail)`.
  - Shapes still mostly degrade improper list tails.
  - This affects cons patterns, `++`, remote argument selection, and displayed return types.

- [ ] Add more compiler-comparison fixtures.
  - Expand beyond the new local inference tests into guards, `case`, `with`, structs, optional maps, binaries, captures, function values, generated code, and remote dependency calls.
  - Compare through `Descr.to_quoted_string/2` or actual compiler warning output where possible.

- [ ] Keep local inference explicitly best-effort.
  - The coverage is much better, but this is still not the compiler's full `Module.Types` local flow.
  - Keep non-ExCk local inference at `:native_inferred` or lower as appropriate, and keep recursion-disabled cases locked down by tests.
  - Document cases where source-position metadata cannot represent all generated/default heads.

- [ ] Define the shape vocabulary for `top`, `unknown`, `dynamic`, `bottom`, and absent fields.
  - The bridge currently mixes `nil`, `:none`, `:dynamic`, open maps, and omitted keys across different operations.
  - Make those meanings explicit before expanding descriptor-backed algebra further.

## P3 - Performance And Cleanup

- [ ] Benchmark native typing on large files.
  - Measure modules with many functions, many variables, large `case`/`with` expressions, many remote calls, and native typing enabled/disabled.
  - Include both metadata build cost and per-request `TypeHints` cost.

- [ ] Review non-LSP internal callers for repeated local inference work.
  - The LSP request path is cached through `TypeHints`.
  - Hover, completion, and other callers may still invoke `Binding.from_env` or local signature construction repeatedly.

- [ ] Document adapter policy near the code.
  - `apply_signature/2` is a guarded mirror of private compiler behavior, not a public API.
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
- [x] Local inference best-effort tests: added; remaining work is fidelity and documented trust.
