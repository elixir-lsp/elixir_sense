Great question — I meant making the local/remote return selection
  clause-aware based on the actual argument types, instead of
  unconditionally unioning all clause returns. Today, our local_handler
  refinement in Binding uses merge_clause_returns to union the return
  types of all clauses because we don’t surface argument descriptors at
  that call site.

  - What it is
      - Use actual argument descriptors to pick the most specific
  matching clause(s) and return those types, instead of a blanket union.
  The selection uses Module.Types.Descr operations (e.g., subtype?/2,
  disjoint?/2, intersection/2).
  - Where it belongs
      - It’s a follow-on to M2 local signature work (present implicitly
  in TYPES_M2_REFINEMENT.md — we store {:infer, domain, clauses} but
  currently ignore domain at call sites).
      - It maps to the “Enhance remote handler with arg-aware returns”
  item I listed in the Next Steps plan — this is an M3-level enhancement,
  not something that was signed off in an “M2_review.md” (no such file in
  the repo).
  - Why not in Binding right now
      - Binding’s local-call path doesn’t have Module.Types descriptors
  for call arguments; we only have ElixirSense shapes. Converting shapes
  back to descriptors would be lossy and brittle.
      - The right place is inside the adaptor/local_handler when we
  already have a stack/context and can type the call (or its args) via
  Module.Types.
  - Minimal algorithm (adaptor/local_handler)
      - Given {:infer, domain, clauses} and args_descrs:
          - If domain is present, check each clause’s arg types against
  args_descrs using subtype?/2 or “not disjoint?” per position.
          - Pick:
              - One exact match → return that clause’s return type.
              - Multiple compatible → union their return types.
              - No compatible → fall back to union of all returns
  (current behavior).
      - This reuses and generalizes the current merge_clause_returns
  helper into a pick-or-union function with arg filtering.
  - Practical placement
      - Prefer implementing this in ElixirTypes.local_handler_from/1 (and
  remote handler) since stacks know how to type arguments.
      - For local calls where we only have AST (TypeInference path),
  ElixirTypes.of_expr/… can already produce descriptors — that path is
  a better candidate for arg-aware selection than the Binding-only post-
  merge.

  Short answer: it’s not an item from a separate “M2_review.md”; it’s
  the natural next step from M2’s signature storage (documented in
  TYPES_M2_REFINEMENT.md) and sits in the M3 bucket of our next-steps
  plan.
