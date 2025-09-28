Here’s a focused compatibility audit of the expansion algorithms in
  Binding, TypeInference, and Guard with Elixir’s set-theoretic type
  system (Module.Types), plus concrete adjustments to minimize impedance.

  High-Level Model

  - Binding is a partial evaluator: it carries “shapes” and propagates
  constraints via special projections (e.g., tuple_nth, map_key) to
  expand types during analysis. lib/elixir_sense/core/binding.ex:361
  - TypeInference computes coarse shapes from AST; when enabled,
  it defers to Module.Types for a subset of nodes (fallback). lib/
  elixir_sense/core/type_inference.ex:233
  - Guard computes variable refinements from guard expressions with a
  custom algebra over shapes (union for or, intersection for and). lib/
  elixir_sense/core/type_inference/guard.ex:16

  Types — Missing (vs. Module.Types)

  - Top type: no explicit “term” (top). nil currently doubles as
  “unknown”. Module.Types uses Descr.term(). Suggest introducing :term.
  - Empty/non-empty list: shape has {:list, type} and {:list, :empty},
  but no explicit non-empty list. Module.Types distinguishes list(T) vs
  non_empty_list(T, …).
  - Map openness: shapes do not explicitly encode open/closed maps.
  Module.Types has closed/open maps and dynamic/static parts. Binding’s
  third map element is “updated_map” origin, not openness.
  - Structs as maps: Module.Types models structs via a map
  with :__struct__. Shapes use {:struct, fields, {:atom, Module}, nil}
  (OK if standardized), but often mixed with map semantics.
  - Function types: no shape for functions beyond a placeholder in
  adaptor. Module.Types has function types with arity.
  - Special types: :pid, :reference, :port are absent. Guard has TODOs
  for these (lib/elixir_sense/core/type_inference/guard.ex:255).
  - Numeric detail: integers supported as {:integer, value|nil}, floats
  not modeled by TypeInference; Module.Types has separate integer() and
  float() and can encode bounded integer ranges.

  Types — Excess/Non-type Constructs

  - Evaluation projections in Binding:
      - {:map_key, map_shape, key_shape}; {:tuple_nth, tuple_shape, n};
  {:list_head, list_shape}; {:list_tail, list_shape}; {:for_expression,
  list}; etc. lib/elixir_sense/core/binding.ex:200, lib/elixir_sense/
  core/binding.ex:361
      - These are not set-theoretic types; they’re partial-evaluation
  thunks. Keep them Binding-internal and don’t surface to adaptor or
  guard algebra.
  - “Updated” attachments in map/struct shapes encode data-flow (map
  update), not set shapes; they confuse open/closed semantics.

  Union/Intersection Semantics

  - Union
      - Represented as {:union, list} (Binding normalizes some unions
  when expanding; union merging appears in guard). lib/elixir_sense/core/
  type_inference/guard.ex:23
      - No simplification/dedup/normal form across the codebase;
  acceptable but drifts from set-theoretic union without normalizing
  duplicates.
  - Intersection
      - Implemented as a bag: builds {:intersection, list} with dedup for
  nested intersections, but no element-wise semantics (no tuple/map/list
  intersection rules). lib/elixir_sense/core/type_inference.ex:499
      - Effectively accumulates constraints; not a true set-theoretic
  intersection. This is fine for Binding’s constraint passing, but
  diverges from Module.Types expectations.

  Guard Compatibility

  - And/Or/Not effect
      - andalso → merges var maps via TypeInference.intersect
  (accumulation, not set-theoretic). lib/elixir_sense/core/
  type_inference/guard.ex:31
      - orelse → unions only when both sides constrain the same variable;
  otherwise drops. lib/elixir_sense/core/type_inference/guard.ex:41
      - not → clears constraints (sets var → nil), not true complement.
  lib/elixir_sense/core/type_inference/guard.ex:19
  - Predicates
      - Maps many :erlang guards to coarse shapes
  (e.g., :is_list, :hd, :tl, :tuple_size, :map_get, etc.). lib/
  elixir_sense/core/type_inference/guard.ex:145
      - Some oddities: {:list, :boolean} used as a type for hd/tl
  guards (that’s not a real list element type). lib/elixir_sense/core/
  type_inference/guard.ex:76
  - Module.Types guards (present/coming) will expect unions/
  intersections/complements in set terms; current approach is a pragmatic
  approximation that won’t translate 1:1.

  Assumptions & Mismatch

  - Unknown vs top: nil is “unknown” everywhere (top), and :none is
  bottom. This maps to term() and empty set respectively, but conflates
  “not inferred” and “top”. lib/elixir_sense/core/type_inference.ex:499
  - Map keys: Binding often assumes atom/binary keys only; Module.Types
  allows general key types.
  - Struct semantics: Binding relies on Struct.is_struct/2 lookups;
  Module.Types treats it as map with :__struct__. Keep shape aligned to
  that map form for easier conversion.
  - Binaries: TypeInference explicitly does not support binaries
  (comment), although the compiler has a Bitstring module for AST
  expansion. lib/elixir_sense/core/type_inference.ex:185
  - Booleans: Mixed representations {:atom, true|false}, :boolean (Guard)
  — choose one consistent representation and maintain conversion to/from
  Module.Types atom sets.

  Concrete Gaps to Close (near term)

  - Add a top sentinel and normalize algebra
      - Introduce :term as top; keep :none as bottom. Teach intersection/
  union helpers to flatten and dedupe so intersections don’t grow
  unbounded (still fine to keep simple).
  - Lists: track emptiness
      - Shape: {:list, :empty} | {:list, elem} | {:list, {:non_empty,
  elem}}. Convert Module.Types empty_list/0 and non_empty_list/2
  accordingly.
      - In Binding, treat {:non_empty, T} as head T, tail {:list, T}.
  - Maps: encode openness
      - Reuse the third element to signal openness: {:map, fields, :term}
  for open; {:map, fields, nil} for closed. Stop using the third slot for
  “updated origin” across expansion (keep that internal).
  - Struct shape standardization
      - Always use {:struct, fields, {:atom, Module}, nil}; encode as map
  with __struct__ atom key when converting to descriptor.
  - Fill in special types
      - Add :pid | :reference | :port | {:fun, arity | :any} shapes and
  consistent conversions (to/from Module.Types).
  - Numbers
      - Add {:float, nil} and optional integer ranges {:integer,
  {min,max}} where easily available (only when Module.Types provided
  precise info).

  Where Union/Intersection Should Evolve

  - Keep today’s inert intersection for control-flow (works for Binding).
  - Add element-wise semantics where safe:
      - Tuples: same arity → intersect fields positionally.
      - Maps: intersect values of shared atom-key fields; drop
  conflicting keys; treat openness conservatively (closed ∩ open = closed
  with shared keys, else open).
      - Lists: unify element types; :empty ∩ {:non_empty, _} = :none.
      - Atoms: identical atom wins; {:atom, a} ∩ {:atom, b} with
  a≠b → :none; :boolean ∩ {:atom, true} → {:atom, true} (if you
  keep :boolean, or model as union of atoms).
  - Longer term: implement intersection/union by converting shapes →
  Module.Types descriptors (from_shape/1), applying Descr.intersection/2
  or Descr.union/2, then converting back (to_shape/1). Start with tuples/
  lists/maps/atoms only.

  Binding/Guard Specific Adjustments

  - Restrict non-type placeholders
      - Keep {:map_key, …}, {:tuple_nth, …}, {:list_head, …},
  {:list_tail, …} Binding-internal; avoid leaking them into guard algebra
  or adaptor results.
  - Guard shapes cleanup
      - Replace {:list, :boolean} placeholder with either {:list, {:atom,
  true}|{:atom, false}} union or a normalized :boolean shape (and teach
  to_shape/1 to collapse boolean atom unions into :boolean).
      - For not guard, explicitly forget constraints (current behavior
  is compatible if you treat it as losing refinements). lib/elixir_sense/
  core/type_inference/guard.ex:19

  Summary Answers

  - Missing types: top :term, non-empty lists, open/closed maps, function
  types, PIDs/ports/refs, floats, integer ranges, structured unions (atom
  sets). Binaries are not covered by TypeInference.
  - Excessive types: Binding’s projection thunks (map_key, tuple_nth,
  list_head/tail, for_expression) and “updated” attachments — keep
  strictly internal to Binding expansion and don’t treat them as set-
  theoretic types.
  - Union/intersection: Union is an unnormalized list; Intersection is
  a constraint bag, not a semantic intersection. Good enough for current
  propagation but not set-theoretic. Introduce element-wise rules where
  safe, and plan a descriptor-backed path for correctness.
  - Assumptions compatibility: Using nil for top and :none for bottom
  mostly maps to term() vs. empty set, but conflation of “unknown” and
  “top” will cause mismatch in corner cases. Map key/type assumptions
  (atoms/binaries only) are stricter than Module.Types but acceptable as
  a pragmatic completion heuristic.

  If you want, I can:

  - Standardize struct/list/map openness in to_shape/1 and adjust Binding
  to treat {:map, fields, :term} as open.
  - Add :term and adjust TypeInference.intersect/2 to normalize
  intersections.
  - Implement a minimal from_shape/1 to power tuple/list/map
  intersections via Module.Types for improved fidelity.
