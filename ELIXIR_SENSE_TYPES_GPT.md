# ElixirSense Types Audit Tasks

Third review date: 2026-06-11.

Worktree: `/Users/lukaszsamson/elixir_sense/.claude/worktrees/trusting-wu-d1f603`.

Inputs consolidated:
- `ELIXIR_SENSE_TYPES_GPT.md` second review.
- `ELIXIR_SENSE_TYPES_FABLE.md` third consolidated backlog.
- `/Users/lukaszsamson/elixir_sense/ELIXIR_SENSE_TYPES_GEMINI.md`.
- Elixir private typesystem under `/Users/lukaszsamson/elixir/lib/elixir/lib/module`.

Goal: use binding shapes as a starting point, progressively enrich them with Elixir inferred types, and display types in the same representation used by Elixir type warnings while keeping private `Module.Types` details behind a replaceable abstraction.

## Status

No confirmed P0 correctness bug remains from the previous GPT/Gemini pass in the current worktree.

Done in the latest code:
- `ElixirSense.Core.TypeHints` facade exists: `request_context/1`, `type_hint_for_var/4`, and `effective_params/4`.
- `Binding.from_env/4` accepts precomputed `local_sigs`, enabling request-scoped caching.
- `apply_signature/2` has parity tests against compiler-observed ExCk behavior and now handles strong-signature return wrapping like `Apply.return/3`: strong returns wrap in `dynamic()` only when an argument is gradual; infer returns still wrap unconditionally.
- Signature provenance precedence is explicit: `:exck` > `:inferred` > `:spec`.
- Spec-derived signatures no longer masquerade as compiler-strong facts.
- Optional fields, open tuples, literal widening for inlay hints, compiler-style descriptor rendering, ExCk version/padding/ETS hardening, synthetic-version filtering, and pattern precision fixes are already covered by tests.

Gemini findings incorporated:
- Confirmed and fixed: strong return wrapping, local-sigs hot-path caching via the facade.
- Refuted by current behavior/Fable verification: non-recursive nested unions causing incorrect normalization.
- Still relevant: shape algebra drift from `Descr`, private API drift risk, complex spec fallback coverage.

## Fix-wave status (2026-06-11 evening, Fable)

Addressed this wave:
- P0 "fail closed on private API drift" — full audit table produced (every apply/3 /
  Module.Types call site: guard + failure behavior verified); one gap fixed
  (to_shape_eager non-map guard); 19 drift-canary tests added incl. a pinned
  capabilities() key set that fails loudly on upstream drift.
- P0 "parity harness across versions" — native tests version-guard reviewed: the
  cross-clause subtraction test now gates on available?(:previous) (1.20-only
  machinery), capability checks cover all 6 keys; the CI matrix runs the suite per
  Elixir version.
- P1 "richer trust levels" — TypeHints now classifies
  :native_exck > :native_inferred > :spec > :shape with trust_rank/1; elixir-ls
  minimumTrust maps compiler/native/bestEffort onto the ranks. Documented gap: the
  native engine collapses many resolvable remote-call thunks to descrs (those report
  :native_inferred), though e.g. Enum.map attributes :native_exck in practice.
- P1 "descr-backed algebra" — combine_intersection delegates to Descr.intersection
  behind an exact-shape whitelist (atoms/booleans/numerics/tuples/lists/unions; maps
  and scalar literals excluded by design); verified dynamic(a) ∩ dynamic(b) =
  dynamic(a ∩ b) makes the wrapped path exact. difference NOT delegated — the
  property does not hold for subtraction (documented).
- P1 "local inference best-effort tests" — 19 tests: recursion (sig deliberately
  absent via maybe_disable_local_handler — locked in), mutual recursion, default
  heads (only full arity recorded in mods_funs_to_positions), super/overridable,
  private reachability, disjoint guard domains, macro-generated heads, remote
  default-arg fixtures, guard-refined/with/struct/capture pipeline hints.

Still open: P1 open/closed map metadata, spec fallback expansion; P2 improper lists,
resolution consolidation, descriptor-aware fields_for_receiver, more compiler
fixtures, true multi-version behavior runs; P3 benchmarks, internal caller review.

## P0 - Merge Gates

- [ ] Keep the apply parity harness in CI across supported Elixir versions.
  - Current parity coverage is good for the local runtime, but the implementation is still a mirror of private `Module.Types.Apply` rules.
  - Run the parity suite on every supported Elixir minor version where native typing is enabled.
  - Include `:infer`, `:strong`, zero matches, >16 matched clauses, static args, gradual args, unions, optional maps, lists, functions, and ill-typed calls.

- [ ] Fail closed on private API drift.
  - Audit every private `Module.Types` call reached through `apply/3`.
  - If an arity, return shape, or descriptor contract changes, disable only that capability and return structural best effort.
  - Never produce native-looking output from unverified descriptors or signatures.

- [ ] Preserve the TypeHints facade as the only LSP-facing entry point.
  - ElixirLS has moved to the facade; keep that boundary stable.
  - New editor-facing features should go through `TypeHints` rather than exposing `Binding`, `TypePresentation`, ExCk chunks, or native signature tuples.

## P1 - Highest-Value Next Work

- [ ] Surface richer trust/source levels from type hints.
  - Today `TypeHints.type_hint_for_var/4` returns `source: :native | :shape`.
  - ElixirSense already records more provenance for signatures: `:exck`, `:inferred`, and `:spec`.
  - Expose useful trust levels such as `:native_exck`, `:native_inferred`, `:spec`, and `:shape` so ElixirLS can implement precise `minimumTrust` behavior.

- [ ] Make descriptor-backed algebra the default when descriptors are available.
  - Shape algebra remains approximate in `combine_intersection`, `difference`, `covers?`, union normalization, case feasibility, and map/list/tuple merging.
  - Start with high-traffic operations where both sides have descriptors and delegate to `Descr.union`, `Descr.intersection`, `Descr.disjoint?`, `Descr.compatible?`, and related primitives.
  - Keep shape fallback for values without descriptors.

- [ ] Treat local function inference as best-effort unless it can delegate to compiler flow.
  - `infer_local_signature/5` is much better covered now, but it is still not the real `Module.Types.local_handler` pipeline.
  - Keep its provenance lower than ExCk/compiler-native facts.
  - Add tests for recursion, default/super clauses, private reachability, multi-clause domains, and generated heads.

- [ ] Record open/closed/partial map and struct semantics in shapes.
  - Current coercion defaults many maps to open and loaded structs to closed maps with full fields.
  - This avoids false absence but loses required-key facts.
  - Add explicit shape metadata for known-open, known-closed, and partial maps/structs, then coerce accordingly.

- [ ] Improve complex specs fallback or retire it behind compiler data.
  - `spec_ast_to_shape` remains a lossy fallback.
  - Add common stdlib/user remote type patterns only where they are safe.
  - Keep spec-derived results marked lower trust than ExCk or native inferred signatures.

## P2 - Fidelity And Coverage

- [ ] Model improper lists or explicitly degrade them.
  - Elixir distinguishes `list(t)`, `non_empty_list(t)`, and improper `non_empty_list(head, tail)`.
  - Shapes still mostly degrade improper list tails.
  - This affects `++`, cons patterns, remote argument selection, and display.

- [ ] Consolidate module/function resolution.
  - `ElixirTypes.module_from_ast/2`, binding call expansion, completion, hover, and LSP parameter hints still contain overlapping resolution logic.
  - Route alias/module/function resolution through one metadata-aware path.

- [ ] Make `fields_for_receiver/2` descriptor-aware.
  - Completion currently relies on expanded shapes.
  - Native descriptors can retain optional keys, required keys, domain keys, and struct fields that shapes may lose.

- [ ] Add default-argument signature fixtures.
  - Native signatures are arity-keyed while source definitions can generate multiple default heads.
  - Cover local and remote `def f(a, b \\ ..., c)` cases and multiple spec heads.

- [ ] Expand compiler-comparison fixtures.
  - Add more examples for guards, case/with, structs, optional maps, binaries, captures, functions, and remote dependency calls.
  - Compare text to `Descr.to_quoted_string/2` or compiler warning output when possible.

- [ ] Add multi-version behavior tests for private API adapters.
  - Capability probes are not enough.
  - Test behavior for `Expr.of_expr`, `Pattern.of_match`, `Pattern.of_head`, `Pattern.of_domain`, `Descr.to_quoted_string`, ExCk decoding, and signature application across supported Elixir versions.

## P3 - Performance And Cleanup

- [ ] Benchmark native typing on large files.
  - Native typing affects parse/metadata paths beyond inlay hints.
  - Measure modules with many functions, many variables, large case/with expressions, and many remote calls.

- [ ] Continue reducing repeated local inference work.
  - The TypeHints request context handles LSP request caching.
  - Review hover/completion/other internal callers for repeated `Binding.from_env` or local sig map work.

- [ ] Document current policy.
  - `apply_signature/2` is a private-API mirror guarded by parity tests, not a public Elixir API.
  - Spec-derived signatures are lower-trust fallbacks.
  - Inlay hints widen literal integer/float/binary leaves, while hover/completion may keep structural literal precision.

## Closed Or Refuted

- [x] Strong-signature return wrapping bug: fixed.
- [x] Per-hint `build_local_sigs_map` hot path: fixed for LSP through `TypeHints.request_context/1`.
- [x] Nested union flattening Gemini P0: refuted for current expansion path.
- [x] Pattern AST refinement overriding native results: fixed; native results are authoritative and AST refinement fills gaps only.
- [x] Tooltip/source rendering and literal widening for hints: fixed.
