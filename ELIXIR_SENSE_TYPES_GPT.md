# ElixirSense Types Audit Tasks

Second review date: 2026-06-11.

Worktree: `/Users/lukaszsamson/elixir_sense/.claude/worktrees/trusting-wu-d1f603`.

References reviewed:
- Current branch code and tests.
- `ELIXIR_SENSE_TYPES_FABLE.md`.
- Elixir private typesystem under `/Users/lukaszsamson/elixir/lib/elixir/lib/module`.

Goal: use binding shapes as a starting point, progressively enrich them with Elixir inferred types, and display types in the same representation used by Elixir type warnings while keeping private `Module.Types` details behind a replaceable abstraction.

## Status

The first review's largest P0s are no longer open in their original form:

- Done: ExCk version pinning, BEAM chunk padding, ETS owner hardening, stale checker-cache validation, optional map field preservation, open tuple conversion, unconvertible union/intersection degradation, dynamic wrapping for shape seeds, synthetic version ranges, guarded/imprecise subtraction fixes, conservative intersection fallback, `++` correction, literal widening for inlay hints, `descr_to_string/2` compiler-style rendering, and Fable/GPT regression suites.
- Done with caveat: `ElixirTypes.apply_signature/2` centralizes signature application and mirrors `apply_infer`/`apply_strong`, but it is still a hand mirror of private Elixir rules.
- Done with caveat: spec-derived signatures are downgraded to `:infer` and provenance fields exist, but provenance is not yet used enough to drive merge/trust decisions.
- Still architectural: binding algebra remains shape-first and approximate; the adapter boundary is still broad; local inference is still a best-effort reproduction of `Module.Types.local_handler`.

## P0 - Correctness Before Trusting Hints

- [ ] Fix strong-signature return wrapping in `ElixirTypes.apply_signature/2`.
  - Current `apply_strong_mirror/2` wraps every matched return in `dynamic(...)`.
  - Elixir's `Module.Types.Apply.return/3` only wraps in non-static modes when at least one actual argument is gradual; with fully static known args, strong arrows return the static OUT.
  - Track whether coerced call arguments are gradual, and mirror `return(type, args_types, stack)` instead of always calling `wrap_dynamic/1`.
  - Add tests for a strong signature with static arg selection versus unknown/dynamic args.

- [ ] Treat `apply_signature/2` as a drift-prone mirror until it is backed by compiler-comparison tests.
  - The centralization is good, but it still reimplements private `Apply` logic: disjointness, compatibility, only-gradual behavior, max-clause fallback, and return wrapping.
  - Add parity tests against real `Module.Types.Apply` behavior where possible, or compile tiny modules and compare ExCk/application outcomes.
  - Include `:infer` and `:strong`, zero matches, too many clauses, gradual args, static args, unions, optional map keys, and functions.

- [ ] Keep remote calls to external modules as the highest-confidence path.
  - The stated LSP goal starts with accurate remote call types. Make ExCk/native remote exports the preferred source and make lossy legacy/spec fallback visibly lower trust.
  - Add coverage for stdlib and dependency calls whose return varies by argument type, not just simple single-clause returns.

- [ ] Use provenance in merge and presentation decisions.
  - `elixir_types_sig_source` exists (`:spec`, `:inferred`, `:exck` reserved), and `render_hint/3` exposes only `:native | :shape`.
  - Add a trust model that distinguishes compiler-native descriptors, ExCk, local best-effort inference, lossy spec conversion, and shape-only data.
  - Do not let lossy spec/local inference override more reliable structural flow facts or present as equally authoritative.

- [ ] Rework local function inference or label it explicitly best-effort everywhere.
  - `infer_local_signature/5` still approximates `Module.Types.local_handler`: previous-clause state, domain computation, default/super clauses, recursion, private reachability, mapping, and warning-mode behavior are incomplete.
  - Either move closer to the real compiler flow or prevent local inferred sigs from being consumed as if they were compiler `{:infer, domain, clauses}`.

## P1 - Architecture And Abstraction

- [ ] Introduce a narrow ElixirSense types facade for consumers.
  - Current consumers still know too much: `VarInfo.elixir_types_descr`, native sig tuples, ExCk-backed fields, `Binding.from_env`, and `TypePresentation.render_hint`.
  - Add stable APIs such as `type_hint_for_var(metadata, position, var, opts)`, `type_remote_call`, `type_local_call`, `render_compiler_type`, and `fields_for_receiver`.
  - Keep private API probing, ExCk decoding, descriptor conversion, and fallback policy behind that facade so it can be removed when Elixir exposes a public API.

- [ ] Split internal algebra from display.
  - `to_shape/1` is lossy and should be used for binding provenance/algebra only.
  - Display should prefer `Descr.to_quoted_string/2` whenever a native descriptor exists.
  - Ensure every caller that displays text goes through the presentation API rather than `Binding.expand -> render` directly.

- [ ] Make shape algebra descriptor-backed where descriptors are available.
  - `Binding.normalize_union`, `combine_intersection`, `difference`, `covers?`, case feasibility, list/map/tuple merging, and optional handling are still custom approximations.
  - Use `Descr.union/intersection/difference/disjoint?/compatible?` for descriptor-backed values and fall back to shapes only when no descriptor exists.

- [ ] Represent top/unknown/dynamic/bottom/absent explicitly in shape data.
  - `nil`, `:none`, `:not_set`, `:empty`, `:empty_list`, `{:dynamic, _}`, and `term()` text are still overloaded across expansion, rendering, and hint suppression.
  - Preserve Elixir distinctions between `term()`, `dynamic()`, `none()`, and `not_set()` where they affect refinement and map optionality.

- [ ] Record open/closed/partial map and struct semantics.
  - Current coercion defaults to open maps and loaded-struct closed maps, which is safer than false absence but loses required-key facts.
  - Add metadata on shapes for known-open, known-closed, and partial maps; coerce each differently.

## P2 - Fidelity And Coverage

- [ ] Add version-matrix behavior tests, not only capability probes.
  - The adapter supports/probes Elixir 1.18/1.19/1.20-era private APIs, but local behavior testing is mostly on the current runtime.
  - Test `Expr.of_expr`, `Pattern.of_match`, `Pattern.of_head`, `Pattern.of_domain`, `Descr.to_quoted_string`, ExCk decoding, and signature application across supported versions.

- [ ] Expand compiler-comparison fixtures.
  - Include remote calls, local calls, pattern matches, guards, case/with, structs, optional maps, lists including improper lists, binaries, captures, and default arguments.
  - Compare returned hint text to `Descr.to_quoted_string/2` or compiler warnings when possible.

- [ ] Model improper lists or explicitly degrade them.
  - The compiler distinguishes `list(t)`, `non_empty_list(t)`, and improper `non_empty_list(head, tail)`.
  - Shapes still mostly collapse improper lists to unknown/proper-list approximations. This affects `++`, cons patterns, remote arg selection, and display.

- [ ] Consolidate module/function resolution.
  - `ElixirTypes.module_from_ast/2`, binding call expansion, completion, hover, and LSP parameter hints use overlapping resolution logic.
  - Route alias/module/function resolution through one metadata-aware path to avoid inconsistent remote-call typing.

- [ ] Add default-argument signature tests.
  - Native signatures are arity-keyed while Elixir source has default-generated heads.
  - Cover local and remote `def f(a, b \\ ..., c)` calls and multiple spec heads.

- [ ] Make `fields_for_receiver/2` descriptor-aware.
  - Field completion currently uses expanded shapes.
  - Native descriptors can retain optional keys, required keys, domain keys, and struct fields that shapes may lose.

- [ ] Fail closed on private API drift.
  - If an internal arity or return contract changes, disable only the affected capability and return structural best effort.
  - Never produce native-looking types from a descriptor/signature whose version or contract is unverified.

## P3 - Performance And Cleanup

- [ ] Memoize local signature maps and avoid repeated clause inference per request.
  - `Binding.from_env/3` may build local sig maps repeatedly for hover, completion, and inlay hints.
  - Cache per metadata/module where safe and invalidate with metadata changes.

- [ ] Benchmark native typing on large files.
  - With native typing enabled, parse/metadata paths can pay extra cost outside inlay hints.
  - Measure large modules, many local functions, large case/with constructs, and many remote calls.

- [ ] Update documentation/comments to match current policy.
  - Specs are fallback/provenance-tagged, not compiler-strong.
  - `render_hint/3` widens literals for hints only.
  - `apply_signature/2` is centralized but still a compatibility mirror, not a public Elixir API.
