Overall

  - Direction is right: a thin, gated adaptor over Module.Types,
  conservative conversions, and surgical integration points, with
  fallbacks to existing logic.
  - M1 is effectively done; a good chunk of M2 is in place (local
  signature capture/inference, ExCk reader, basic pattern refinement
  hooks).
  - Implementation is careful (guards + rescues) and largely non-invasive
  to existing behavior when disabled.

  What Landed

  - Adaptor: ElixirSense.Core.ElixirTypes with availability/enabled
  gates, stack/context builders, of_expr/…, of_match/…, to_shape/…,
  merge_shapes/2, perf metrics and a remote handler. lib/elixir_sense/
  core/elixir_types.ex:1
  - Fallback typing: TypeInference calls adaptor when it can’t type an
  AST and merges list/tuple detail when possible. lib/elixir_sense/core/
  type_inference.ex:148, lib/elixir_sense/core/type_inference.ex:165,
  lib/elixir_sense/core/type_inference.ex:233
  - Pattern refinement hooks: = in compiler and clauses enhancer for
  case/cond. lib/elixir_sense/core/compiler.ex:137, lib/elixir_sense/
  core/compiler.ex:155, lib/elixir_sense/core/compiler/clauses.ex:691
  - Local signature capture + storage: raw clause AST and inferred
  signature slots on ModFunInfo with helpers to persist them. lib/
  elixir_sense/core/compiler/state.ex:1435, lib/elixir_sense/core/
  compiler/state.ex:1450, lib/elixir_sense/core/state/mod_fun_info.ex:29
  - ExCk reader: chunk fetch + cache, adapter remote handler uses
  it. lib/elixir_sense/core/exck_reader.ex:1, lib/elixir_sense/core/
  elixir_types.ex:1255
  - Shape conversion: conservative basics, plus some M2 extras (union,
  struct, function, integer ranges, string literal heuristics). lib/
  elixir_sense/core/elixir_types.ex:579, lib/elixir_sense/core/
  elixir_types.ex:742

  Gaps vs. M1/M2

  - Local handler not wired into normal flows: you can build a local sigs
  map and of_expr/… accepts it, but the core code path doesn’t pass it
  yet. Only tests exercise it. Needs propagation from metadata/env at
  call sites (e.g., Binding.expand local calls).
  - Env not threaded: TypeInference calls adaptor with nil module/
  function/file by default, limiting precision and blocking future diags.
  Thread env/file to of_expr/… when available.
  - Pattern refinement: of_match/… is implemented, but real refinements
  for map/list/tuple are mostly TODOs (stubbed methods). Ensure these
  return concrete improvements for simple patterns.
  - Remote signatures: current remote handler extracts only a union of
  return types from ExCk; it ignores domains/args. Useful but leaves
  precision on the table for call-site checking.
  - Config: feature flag assumed via Application.get_env, but there’s
  no default key in repo config (config/config.exs). TTL for ExCk
  uses :exck_cache_ttl separately; planned :elixir_types_opts not used.
  - Descriptor heuristics: union/struct/function/range/string-literal
  extractors are intentionally conservative but may not match all real
  descriptor forms. They fall back to nil, which is safe but limits wins.

  Implementation Quality

  - Gated + defensive: availability and enable checks are consistently
  used; external calls are wrapped in try/rescue/catch to avoid editor
  crashes.
  - Non-invasive: existing typing continues to work; merges prefer more
  specific shapes and keep :none intact.
  - Good scaffolding for M2: clause capture, inference, ExCk integration,
  closure-based handlers are ready; only wiring remains.
  - Minor style nit: SPDX headers added in ExCkReader may not match repo
  conventions; consider aligning with repo’s no-header pattern.

  Tests

  - Coverage is broad: basic adaptor behavior, gating, shape conversion,
  fallback typing, ExCk fetch/cache, local signature capture, remote
  handler, and pattern refinement paths are all covered.
      - Examples: test/elixir_sense/core/elixir_types_test.exs:14,
  test/elixir_sense/core/elixir_types_real_test.exs:76, test/
  elixir_sense/core/exck_reader_test.exs:43, test/elixir_sense/core/
  elixir_types_m2_test.exs:80
  - Many tests accept :error/nil as valid outcomes to keep things robust
  across versions; this avoids flakiness but weakens guarantees for the
  cases you intend to support.
  - Missing a few assertive checks:
      - Demonstrate improved var types are merged in compiler paths
  (e.g., = and case clauses) by asserting on actual refined shapes in
  State after expansion.
      - Assert specific to_shape results for supported patterns (simple
  lists, tuples, maps) with available?() true.
      - End-to-end test that local signatures actually influence typing
  of local calls once wiring is added.

  Recommendations

  - Wire local handler end-to-end:
      - Build map via ElixirTypes.build_local_sigs_map/2 and pass into
  TypeInference.type_of_with_elixir_types/4 at local call sites in
  Binding. Then thread env.module/function/file so init_stack/… can set
  a meaningful context. See lib/elixir_sense/core/binding.ex local call
  handling around the {:local_call, …} branch.
  - Thread env/file:
      - Where TypeInference.type_of/2 is used inside the compiler/
  binding, pass module/function/file into the new 4‑arity helper to
  improve precision for both locals and remotes.
  - Flesh out of_match refiners:
      - Implement tuple/map/list refinements for simple, closed cases.
  Keep conservative; only add clearly inferrable types.
  - Improve test assertiveness where supported:
      - Add a couple of non-optional assertions: e.g., {:{}, [], [1,:ok]}
  → exact tuple shapes, %{name: n} = %{name: "x"} refines n to {:binary,
  nil}.
      - Add an integration test that an inferred local signature changes
  the type seen at a local call site once wired.
  - Tighten config:
      - Add use_elixir_types: false default in config/config.exs and
  unify caching flags into :elixir_types_opts (or keep the current key
  but document it).
  - Keep conversion conservative:
      - Retain “return nil when unsure”. It’s better to under-report than
  to feed wrong shapes into completions.
  - Optional: Expand remote return typing:
      - Consider looking at clause domains when readily available to
  narrow return type based on simple literal arguments.
