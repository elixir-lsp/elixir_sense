# ElixirSense Types Audit Tasks

## Status after the 2026-06-11 fix pass (Fable)

Gates: 1756/1756 tests, compile --warnings-as-errors, format, credo --strict all green.

**Done:**
- HP "native apply wrapper" — `ElixirTypes.apply_signature/2` mirrors `apply_infer`/
  `apply_strong` (per-position disjointness, @max_clauses=16, dynamic-wrapped unions,
  `:error` on zero matches — the union-of-all-clauses fallback is gone). Selection uses
  STATIC arg coercion (dynamic-wrapped args are only_gradual and can never narrow);
  unknown args stay gradual.
- HP "spec sigs not :strong" — spec-derived sigs are `{:infer, ...}` with dynamic-wrapped
  returns; provenance recorded in `elixir_types_sig_source` (`:spec`/`:inferred`,
  `:exck` reserved) on SpecInfo/ModFunInfo.
- HP "rendering policy" — descr-backed display goes through `descr_to_string`
  (`to_quoted_string` parity verified by a 30-descr golden corpus, exact match);
  `render_hint/3` widens literal leaves to compiler spellings (hover/completion keep
  literals as a documented extension); `source: :native | :shape` exposed.
- Algebra "optional map keys" — `{:optional, _}` preserved through Binding expansion,
  rendered `if_set(...)`, coerced via `Descr.if_set/1` in field positions (round-trip
  soundness now holds for optional-key maps).
- Pattern "limit post-Pattern refinement" — native `of_match` descriptors are
  authoritative; AST refinement only fills native-less vars; its residual bugs fixed.
- Pattern "synthetic versions" — reserved ranges documented; synthetic-keyed entries
  filtered from VarInfo-bound results.
- Tests — `elixir_types_compiler_parity_test.exs` (rendering parity, round-trip
  soundness property, ExCk parity) and `elixir_types_failure_mode_test.exs` (graceful
  degradation incl. version mismatch, garbage input, disabled mode). ETS owner now
  recreates its table on demand (receive loop) — cache survives external deletes.

**Partial:** facade (apply_signature + sourced render_hint are the seam; full opaque
facade with type_expr/type_match entry points still open); seed-trust tracking
(provenance recorded, not yet consulted in merge decisions); open/closed coercion
(open default + if_set, but known-closed shapes are not recorded).

**Deferred:** descr-backed Binding algebra swap; improper-list modeling; local
inference full `local_handler` parity (mitigated by :infer downgrade + apply mirror);
module-resolution consolidation; default-arg signature tests; descriptor-aware
`fields_for_receiver`; full version-matrix behavior tests (1.18/1.19 paths are probed
but only 1.20 is behavior-tested locally).

---

Audit target: branch/worktree `/Users/lukaszsamson/elixir_sense/.claude/worktrees/trusting-wu-d1f603`.

Reference checked: `/Users/lukaszsamson/elixir/lib/elixir/lib/module/types.ex` and `/Users/lukaszsamson/elixir/lib/elixir/lib/module/types/*.ex`.

Goal: use binding shapes as the starting point, progressively enrich them with Elixir's inferred types, and keep the public ElixirSense surface close to the compiler's type semantics and warning presentation without exposing unstable private `Module.Types` details to ElixirLS.

## High Priority

- [ ] Replace custom remote-signature return filtering with a native apply wrapper.
  - Current code reads ExCk signatures but `ElixirTypes.extract_return_type_from_sig/2` reimplements compatibility by coercing binding shapes and checking `Descr.intersection/2`, then unions return types.
  - This diverges from `Module.Types.Apply.remote_apply/3`, `apply_infer/2`, and `apply_strong/4`, which handle `:infer` vs `:strong`, gradual arguments, domain compatibility, `dynamic(return)`, `@max_clauses`, and bad-remote behavior.
  - Add a narrow adapter API such as `ElixirTypes.apply_signature(sig, arg_shapes, mode)` that delegates to Elixir's application semantics when an internal callable API exists, or mirrors it in one place with tests copied from compiler behavior. Then make `Binding.expand_call*` and `maybe_refine_local_call/4` use it.

- [ ] Stop treating spec signatures as compiler-native strong signatures until they are produced by Elixir's own spec/type translator.
  - `Compiler.State.build_elixir_types_spec_sig/4` parses rendered spec strings with `Code.string_to_quoted/2`, converts spec AST to ElixirSense shapes, then coerces them into `Descr` values.
  - This reimplements typespec semantics and loses/changes constructs such as remote types, type variables, bounded variables, unions/intersections/negations, maps with required vs optional keys, structs, improper lists, literals, and callbacks.
  - Prefer the real ExCk/signature data emitted by Elixir whenever available. If local specs must be used before compilation, isolate this as a fallback with an explicit `:lossy_spec` status and do not mark it `:strong`.

- [ ] Rework local function inference to track Elixir's `Module.Types.local_handler/7` behavior more closely.
  - `ElixirTypes.infer_local_signature/5` calls `Pattern.of_head` and `Expr.of_expr` per clause, but it does not reproduce compiler handling of `previous`, `head_no_previous_args_types`, `compute_domain/4`, clause mapping, `add_inferred/5`, `group_clauses_by_return/1`, default/super clauses, recursion, local-call cache state, private reachability, or warning-mode behavior.
  - This can produce signatures that look native but do not behave like compiler inferred signatures.
  - Either call a public or deliberately wrapped compiler inference entry point when possible, or downgrade local inference output to best-effort metadata and avoid feeding it back as authoritative `{:infer, domain, clauses}`.

- [ ] Make the adapter boundary explicit and stable.
  - ElixirLS should not know about `Module.Types.Descr` maps, ExCk chunk layouts, or `{kind, domain, clauses}` tuples.
  - Introduce an internal ElixirSense facade with stable structs/opaque terms for: `available?`, `type_expr`, `type_match`, `type_remote_call`, `type_local_call`, `render_compiler_type`, and `fields_for_receiver`.
  - Keep all private API probing, version dispatch, ExCk validation, and fallback behavior behind that facade so it can be removed when Elixir exposes a public typesystem API.

- [ ] Align rendering policy with compiler warning output for every displayed type.
  - `TypePresentation.render_descr/1` correctly tries `Module.Types.Descr.to_quoted_string/2`, but the structural renderer can still display non-compiler spellings: literal integers and strings, `not_set()`, `if_set(...)`, `struct()`, `number()`, open tuple `{...,}`, and ad-hoc function arrows.
  - Decide which structural-only constructs may be shown in LSP. For inlay hints, prefer compiler spellings or suppress/normalize constructs that the compiler would not show in warnings.
  - Add golden tests comparing displayed strings against `Descr.to_quoted_string/2` for remote calls and native descriptors.

## Binding Shape Algebra Gaps

- [ ] Separate "unknown", "top", "dynamic", "bottom", and "known absent" in the shape representation.
  - Today `nil`, `:none`, `:not_set`, `:empty`, `:empty_list`, `{:dynamic, _}`, and `term()`-rendering shapes are mixed across binding expansion, union normalization, optional fields, and hint suppression.
  - Elixir's typesystem distinguishes `term()`, `dynamic()`, `none()`, and `not_set()`; shape algebra should preserve those distinctions where they influence refinement and rendering.

- [ ] Replace custom `union`, `intersection`, `difference`, and `covers?` rules with descriptor-backed operations where possible.
  - `Binding.normalize_union/1`, `combine_intersection/2`, `difference/2`, `covers?/2`, list/map/tuple merge rules, and case feasibility are hand-built approximations.
  - These cannot model negation, optional map keys, open/closed maps, improper lists, dynamic wrappers, function intersections, remote type aliases, or structural subtyping the same way `Descr` does.
  - Use shapes for lightweight binding provenance, but perform algebra on `Descr` when descriptors are present. Fall back to existing shapes only when descriptor algebra is unavailable.

- [ ] Preserve optional map key semantics instead of unwrapping them during expansion.
  - `ElixirTypes.to_shape/1` can produce `{:optional, inner}`, but `Binding.do_expand/3` unwraps it to `inner`, losing `if_set(...)`/`not_set()` information from maps.
  - This diverges from compiler map typing, where optional fields matter for pattern matching, map updates, and rendered warnings.

- [ ] Audit map/struct coercion for open vs closed map semantics.
  - `coerce_map_descr/2` always builds `Descr.open_map/1`; `coerce_struct_descr/2` builds closed maps only for loaded structs and open maps otherwise.
  - That is safer than asserting absent keys, but it also loses required-key information from binding shapes and can make pattern compatibility too permissive.
  - Record whether a shape is known-open, known-closed, or partial, and coerce accordingly.

- [ ] Fix list and improper-list modeling.
  - The compiler distinguishes `empty_list()`, `list(t)`, `non_empty_list(t)`, and improper `non_empty_list(head, tail)`.
  - Current shapes mostly collapse improper lists to unknown or proper lists. This affects `++`, cons patterns, list hints, and remote call argument filtering.

- [ ] Keep literals out of compiler-shaped display unless the compiler descriptor is literal-aware for that value.
  - Elixir's current warning text generally widens integer/string literals to `integer()`/`binary()` in many places, while the structural path preserves `5` and `"x"`.
  - Literal preservation may be useful internally for binding refinements, but the inlay-hint display layer should not present a type style that differs from compiler warnings.

## Pattern And Occurrence Typing

- [ ] Remove or strictly limit post-`Module.Types.Pattern` AST refinement in `ElixirTypes.apply_pattern_refinements/5`.
  - After calling native `of_match`, the adapter refines variables again from AST literals for structs, maps, tuples, lists, and binary segments.
  - This risks overriding the compiler's own guard/pattern precision rules, previous-clause subtraction, dynamic wrappers, map optionality, and binary segment semantics.
  - Prefer the variables/descriptors returned by native `Pattern.of_match`; only use AST refinement when native typing is unavailable, and mark it best-effort.

- [ ] Do not synthesize variable versions that can be mistaken for compiler versions.
  - `ensure_body_var_versions/1` stamps unversioned variables with high integer versions. This prevents crashes but can create descriptors keyed to versions that do not exist in metadata.
  - Keep synthetic versions in a separate namespace or wrapper so they never leak into `VarInfo`, hover, inlay hints, or cross-statement seeds.

- [ ] Revisit case/with difference logic against `Pattern.init_previous/0` and `Pattern.of_head/8`.
  - Current cross-clause occurrence typing uses custom `precise_pattern_type/1`, `difference/2`, and feasibility checks.
  - Elixir computes previous-clause information in `Module.Types.Pattern` and uses precise/head-no-previous data when building inferred domains. The custom approximation should be tested against real compiler examples, especially guards, maps, binaries, pinned vars, structs, and non-empty lists.

- [ ] Seed native contexts with descriptors without asserting false certainty.
  - `coerce_var_type/1` dynamic-wraps shape-derived seeds, which is the right direction, but already-built descriptors are accepted as authoritative.
  - Track whether each descriptor came from compiler inference, ExCk/specs, or shape coercion; only compiler-native descriptors should be treated as certain.

## Remote Calls And External Modules

- [ ] Make remote calls to external modules accurate before local best-effort hints.
  - Use ExCk exports where available and version-compatible.
  - Fall back to legacy specs only when ExCk is missing, but avoid converting lossy legacy specs into native-looking strong signatures.
  - Add coverage for stdlib calls with overloaded signatures, callbacks, protocol implementations, structs, maps, functions, and calls whose arguments select different return clauses.

- [ ] Normalize alias/module resolution through existing compiler metadata.
  - `ElixirTypes.module_from_ast/2` partially resolves aliases, `__MODULE__`, attributes, and variables. This duplicates `Introspection.actual_mod_fun/6` and normalized env expansion.
  - Consolidate module resolution so binding expansion, remote-call typing, completion, hover, and LSP inlay hints use the same path.

- [ ] Handle default arguments and generated heads consistently.
  - Binding call expansion already has default-arity logic via docs/spec metadata, but native signatures are keyed by concrete arity.
  - Add tests for `def f(a, b \\ ...)`, remote defaults, and specs with multiple heads to make sure the selected signature corresponds to the actual call arity.

## Presentation And Consumer API

- [ ] Split APIs for internal algebra vs user-facing presentation.
  - `to_shape/1` is lossy and useful for binding algebra. `descr_to_string/2` is the compiler-facing presentation path. Keep consumers from accidentally calling `render/1` on lossy structural shapes when a descriptor is available.

- [ ] Add an LSP-facing `type_hint_for_var/3` API in ElixirSense.
  - It should return `:skip | {:ok, %{label: binary(), full: binary(), source: :compiler | :shape | :spec | :unknown}}`.
  - This lets ElixirLS avoid knowing how to combine `VarInfo.type`, `VarInfo.elixir_types_descr`, binding expansion, descriptor rendering, and suppression policy.

- [ ] Make `fields_for_receiver/2` descriptor-aware.
  - Current field completion uses expanded shapes only. Native descriptors can know optional keys, required keys, struct fields, and domain keys that shapes may drop.
  - Add a descriptor-backed field extraction path where possible, falling back to shapes.

## Compatibility And Tests

- [ ] Add version-matrix tests for every private `Module.Types` API used.
  - The adapter probes 1.18/1.19/1.20-era arities. Tests should assert behavior, not just availability, for `Expr.of_expr`, `Pattern.of_match`, `Pattern.of_head`, `Pattern.of_domain`, `Descr.to_quoted_string`, and ExCk decoding.

- [ ] Add compiler-comparison fixtures.
  - For selected code snippets, compile with Elixir's checker and compare the adapter's returned text/signatures against compiler warning text or ExCk signatures.
  - Include remote calls, local calls, pattern matches, guards, case/with, structs, maps with optional keys, lists, binaries, and function captures.

- [ ] Fail closed on private API drift.
  - If a probed internal API changes shape, the adapter should disable only the affected capability and return best-effort structural types, not crash or produce native-looking but wrong types.

- [ ] Document trust levels on stored metadata.
  - Add metadata fields or status atoms distinguishing `:compiler_native`, `:exck`, `:local_inferred_best_effort`, `:legacy_spec_lossy`, and `:shape_only`.
  - Use those trust levels in merge and presentation decisions.
