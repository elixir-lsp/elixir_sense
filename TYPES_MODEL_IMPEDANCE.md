Here’s a concrete path to minimize impedance between ElixirSense shapes
  and Module.Types descriptors now, while setting up a clean future
  migration.

  Near‑Term Alignment

  - Standardize struct shape
      - Always use {:struct, fields, {:atom, Module}, nil} so Binding
  consumes Module.Types results without extra adapters. Binding expects
  this form (lib/elixir_sense/core/binding.ex:52, lib/elixir_sense/core/
  binding.ex:473).
      - Ensure ElixirTypes.to_shape/1 returns this exact shape (today it
  sometimes returns {:struct, module, fields}). lib/elixir_sense/core/
  elixir_types.ex:616
  - Introduce explicit top type
      - Add a :term sentinel (top) to distinguish “unknown but valid”
  from “not computed”. Keep :none as bottom. Gradually replace nil usages
  that currently serve as top with :term, while continuing to accept nil
  | :term in readers for compatibility.
  - Lists: model emptiness
      - Extend list shape to capture emptiness: {:list, :empty} | {:list,
  elem} | {:list, {:non_empty, elem}}.
      - Convert Module.Types empty_list() → {:list, :empty} and
  non_empty_list(T, _) → {:list, {:non_empty, to_shape(T)}}.
      - Binding list logic already reasons about heads/tails
  (lib/elixir_sense/core/binding.ex:361, lib/elixir_sense/core/
  binding.ex:463); teaching it {:non_empty, _} is a local extension (drop
  to elem when present).
  - Maps: model openness
      - Extend {:map, fields, updated} to allow updated to be :term
  (open map) besides nil (closed/no-update). Map Module.Types “open
  map” → {:map, fields, :term}; closed map → {:map, fields, nil}.
  Binding’s map handling will pass these through (lib/elixir_sense/core/
  binding.ex:440).
  - Add basic “specials”
      - Introduce shapes: :pid | :reference | :port | {:fun, arity
  | :any} to mirror Module.Types. Treat them as atoms in Binding (pass-
  through) until used for completions or guards.

  Algebra/Merge Semantics

  - Keep union/intersection, but tighten rules
      - Provide helpers Shape.union([shape]) and
  Shape.intersection([shape]) that flatten and dedupe, so we don’t
  accumulate nested {:intersection, …} and noisy {:union, …}. Today
  intersect/2 grows lists (lib/elixir_sense/core/type_inference.ex:604).
  - Tuple/map element‑wise merge
      - Evolve ElixirTypes.merge_shapes/2 to attempt element‑wise merge
  for tuples (same arity) and per‑key merge for maps (shared atom keys);
  fall back to “prefer more specific” if merging fails. This mirrors
  Module.Types Descr intersection behavior at a coarser granularity. lib/
  elixir_sense/core/elixir_types.ex:742

  Conversion Layer (bridge both ways)

  - to_shape (Descr → shape)
      - Make it precise for the aligned cases above:
          - ints (incl. ranges) → {:integer, {min,max}} | {:integer, nil}
          - lists (empty/non_empty) → see lists above
          - tuples (closed) → {:tuple, n, [shapes]}
          - maps (closed/open) → {:map, fields, nil | :term}
          - struct → {:struct, fields, {:atom, Module}, nil}
          - atom singletons → {:atom, a}; unions → {:union, …} of atom
  singletons
          - funs → {:fun, arity | :any}
          - pid/ref/port → :pid | :reference | :port
      - You already have most of this; standardize struct output
  and list empty/non_empty to reduce loss. lib/elixir_sense/core/
  elixir_types.ex:579
  - from_shape (shape → Descr)
      - Add a best‑effort ElixirTypes.from_shape/1:
          - {:atom, a} → Descr.atom([a])
          - {:integer, {min,max}} → ranged integer (or integer/0 if not
  supported)
          - {:list, :empty} → empty_list/0; {:list, {:non_empty, T}} →
  non_empty_list(from_shape(T)); {:list, T} → list(from_shape(T))
          - {:tuple, n, elems} → closed tuple of from_shape/1 elems
          - {:map, fields, :term} → open map; nil → closed map; struct →
  map with :__struct__ atom field
          - unions/intersections → fold with Descr.union/2/
  Descr.intersection/2 (when available)
      - This unlocks arg‑aware clause selection in local/remote handlers
  without requiring AST (nice for M3).

  Impact and Code Hotspots

  - Binding
      - Struct shape alignment is the most important immediate change
  to avoid mismatches (lib/elixir_sense/core/binding.ex:52, lib/
  elixir_sense/core/binding.ex:473).
      - List {:non_empty, _} handling: tweak head/tail helpers
  to treat {:non_empty, T} as T for heads and keep list for tails
  (lib/elixir_sense/core/binding.ex:361, lib/elixir_sense/core/
  binding.ex:463).
  - TypeInference
      - Continue to use adaptor fallback; with from_shape/1 we can
  start evaluating intersections/unions via Descr to improve precision
  incrementally (opt‑in per call site). lib/elixir_sense/core/
  type_inference.ex:233
  - ElixirTypes adaptor
      - Standardize to_shape/1 struct mapping, add list non_empty, add
  specials, add from_shape/1, and expose a small Shape helper module for
  unions/intersections.
      - Remote/local handlers can use from_shape/1 for arg‑aware clause
  picking (M3).

  Suggested Step‑Down Plan

  - Step 1: Align output shapes
      - Fix to_shape/1 for struct + list emptiness, add specials.
  Add tests that assert Binding consumes them correctly (simple maps/
  structs/lists). Files: lib/elixir_sense/core/elixir_types.ex:579; test/
  elixir_sense/core/elixir_types_m2_test.exs:526+
  - Step 2: Introduce :term and normalize algebra
      - Add :term as top; teach intersect/2/merge_shapes/2 to flatten/
  normalize. lib/elixir_sense/core/type_inference.ex:604; lib/
  elixir_sense/core/elixir_types.ex:742
  - Step 3: Implement from_shape/1
      - Add best‑effort shape→descr; unit tests around round‑tripping the
  common patterns.
  - Step 4: Arg‑aware selection (M3)
      - Use from_shape(args) in local/remote handlers to choose
  compatible clauses; keep current union behavior as fallback. lib/
  elixir_sense/core/elixir_types.ex:1219, lib/elixir_sense/core/
  elixir_types.ex:1255
  - Step 5: Broaden map openness
      - Start populating {:map, fields, :term} from open Module.Types
  maps; add a couple of Binding tests to ensure benign consumption.

  Long‑Term Migration Path

  - Keep generating shapes for UI/completion while increasingly computing
  with Module.Types.Descr under the hood:
      - Evaluate merges/intersections via Descr (using from/to
  conversion) for semantic correctness, then project back to shapes.
      - Gradually reduce places where shapes are “authored” manually in
  TypeInference/Binding; prefer deriving them from descriptors.
  - When Elixir’s engine is mature:
      - Replace ElixirSense’s type inference for expressions/patterns
  with Module.Types calls, using shapes only as a presentation layer.
      - Keep Dialyzer/spec/guards/pragmatic extras as a thin augmentation
  where Module.Types is silent.

  This keeps present performance and behavior predictable while reducing
  impedance enough to unlock arg‑aware clause selection and more precise
  remotes — and sets you up to flip the engine later with limited
  fallout.
