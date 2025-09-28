Next Steps

  - Review and confirm plan scope
  - Plumb env into adaptor calls
      - Thread module/function/file into TypeInference →
  ElixirTypes.of_expr/… use sites for more precise stacks. Start with
  call sites in TypeInference.type_of/2 and points where env is available
  in compiler flows. lib/elixir_sense/core/type_inference.ex:233
  - Wire local_handler into local calls
      - Build local sigs map from metadata and pass it for local calls
  (so Module.Types can type locals). Hook in Binding’s local call path.
  lib/elixir_sense/core/binding.ex:200+
  - Expand of_match refinements
      - Implement concrete refiners for tuples, lists, and maps
  (currently TODOs) so = and case produce tangible var types. lib/
  elixir_sense/core/elixir_types.ex:520–760
  - Add refinement hooks for other constructs
      - Mirror the case enhancement for cond, with, try, receive, and
  simple for subjects where pattern vars are extracted. lib/elixir_sense/
  core/compiler/clauses.ex:660–740
  - Standardize struct shapes
      - Align to_shape/1 struct output to Binding’s shape {:struct,
  fields, {:atom, Module}, nil} to ensure seamless expansion and
  intersections. lib/elixir_sense/core/elixir_types.ex:616; lib/
  elixir_sense/core/binding.ex:84
  - Improve merge algorithms
      - Make merge_shapes/2 element-wise for tuples and maps (field-
  wise merge) and list element narrowing when both sides are present;
  keep :none semantics.
  - Enhance remote signatures
      - In remote_handler_from/1, consider argument descriptors (when
  available) to choose clause returns instead of plain unions from ExCk.
  lib/elixir_sense/core/elixir_types.ex:1255
  - Unify config and docs
      - Add defaults for :use_elixir_types in config/config.exs and
  consolidate cache knobs under :elixir_types_opts with docs.
  - Strengthen tests
      - Add assertive tests for refined vars in = and case; add
  local-call influence tests once wired; add element-wise merge
  tests; stabilize to_shape struct mapping. test/elixir_sense/core/
  elixir_types_m2_test.exs:160
  - Profile and tune
      - Measure cost of adaptor paths; selectively use :traversal
  vs :dynamic; keep small caches hot; keep feature gated.

  Coverage Map (Module.Types usage)

  - Expression fallback: Yes, via TypeInference catch‑all and tuple/
  list merges; env currently nil by default. lib/elixir_sense/core/
  type_inference.ex:233
  - Function definitions: Yes, clauses captured and signatures inferred;
  stored on ModFunInfo. lib/elixir_sense/core/compiler.ex:2000+, lib/
  elixir_sense/core/compiler/state.ex:1435,1450
  - Match =: Yes, merged refinements from of_match. lib/elixir_sense/
  core/compiler.ex:137,155,2958
  - Case clauses: Yes, enhanced refinement hook present. lib/
  elixir_sense/core/compiler/clauses.ex:691,705
  - cond/with/try/receive/for: Not yet integrated; planned above.
  - Remote calls: ExCk reader present; remote handler installed in
  stacks, but core flows rarely call of_expr for remote call ASTs
  yet. lib/elixir_sense/core/exck_reader.ex:1, lib/elixir_sense/core/
  elixir_types.ex:1255
  - Protocols: Module.Types supports them; no special integration added
  here (falls under remote/local inference).
  - Guards: Existing ElixirSense guard refinements remain; integrating
  Module.Types guard typing is a future step.

  Bottom line: Good coverage for =/case/function defs and expression
  fallback; not yet for cond/with/try/receive/for; locals/remotes not yet
  feeding call‑site types in normal paths.

  Binding Compatibility

  - Shapes: lists/tuples/maps produced by to_shape/1 match Binding’s
  expectations. Structs need alignment: Binding expects {:struct, fields,
  {:atom, Module}, nil} while to_shape/1 currently returns {:struct,
  module, fields}. Standardizing avoids surprises when refined var
  types flow into state and later Binding.expand/3 consumes them. lib/
  elixir_sense/core/binding.ex:84
  - Context operators: Binding’s evaluation uses contextual wrappers
  like {:map_key, …}, {:tuple_nth, …}, {:list_head, …} to propagate
  constraints. Current of_match/… returns final shapes; that’s fine as
  an augmentation, but adding simple pattern‑aware refinements that mimic
  those contexts would complement existing flows (optional).
  - Data model: ModFunInfo additions (clauses/sig/status) fit the state
  well. build_local_sigs_map/2 aligns with Module.Types local handler
  contract. lib/elixir_sense/core/state/mod_fun_info.ex:29

  Conclusion: Mostly compatible today; fix struct shape and you’re in
  good shape. Keep conversions conservative.

  Merging Algorithms

  - TypeInference.intersect/2 builds an intersection envelope; safe
  and non‑destructive, but can accumulate and stay unresolved. It is
  acceptable for now because Binding can expand variants; consider
  normalizing duplicates to keep intersections shallow. lib/elixir_sense/
  core/type_inference.ex:604
  - ElixirTypes.merge_shapes/2 is conservative and
  specificity‑preferring. It:
      - Preserves :none
      - Prefers specific integer literals
      - Prefers lists with element types
      - Chooses tuple/map by “more concrete elements/fields”
      - It does not merge element‑wise; safe but lossy. lib/elixir_sense/
  core/elixir_types.ex:742
  - Recommendation: For tuples and maps, attempt element/field‑wise
  merge when both sides have compatible arity/keys; fall back to current
  “prefer more specific” rule otherwise. For lists, prefer the narrower
  element type when both are non‑nil. Keep :none absorbing.
