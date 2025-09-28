Short answer: don’t wire the remote handler inside Binding — wire it
  where we still have the original AST: in TypeInference. Yes, this is
  part of the next-steps (M3) plan.

  - Where remote handler lives today
      - It’s already installed on the Module.Types stack in
  ElixirTypes.init_stack/…, and it uses ExCkReader to read ExCk
  signatures.
  - Why not Binding
      - Binding’s call evaluation works on shapes (not AST). At
  that point we’ve lost the full call AST and can’t reliably invoke
  Module.Types.Expr.of_expr/5 to leverage the remote handler.
      - Binding continues to use specs + introspection; adaptor results
  should be merged earlier.
  - Where to wire it
      - In TypeInference’s remote-call clause (the {{:., …}, _, args}
  form), call ElixirTypes.of_expr/… with the original AST plus env
  (module/function/file) and merge the returned shape with the existing
  result, preferring the more specific one.
      - This naturally engages the remote handler (since of_expr builds a
  stack that includes it).
  - Plan alignment (M3)
      - Plumb env: Thread module/function/file (and metadata when
  helpful) into TypeInference.type_of_with_elixir_types/… calls.
      - Remote calls: Add a merge step in the remote-call clause to
  consult ElixirTypes.of_expr/… (which uses the remote handler) and merge
  shapes.
      - Tests: Add assertive e2e for common remotes (e.g., Enum.map/2,
  String.length/1), verifying improved types when enabled and graceful
  fallback when disabled.
      - Optional later (arg-aware): Extend the remote handler to pick
  clause returns based on argument descriptors rather than a plain union
  — that’s the “arg-aware returns” enhancement in M3.

  So, no extra Binding wiring needed. The right place to “turn on”
  Module.Types’ remote typing is in TypeInference where the AST is
  preserved.  
