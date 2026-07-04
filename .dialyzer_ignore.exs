# Dialyzer warnings suppressed for the ElixirSense type engine.
#
# Entries are `{file, warning_type}` (no line) **on purpose**: the adapter
# supports multiple Elixir versions, and the warnings below move between lines
# from one version to the next (version-gating dead branches flip; the vendored
# tokenizer/parser differ). Pinning exact lines made the file break on every
# `mix format` and every Elixir switch. The trade-off is that a *new* warning of
# the same type in the same file would also be hidden — acceptable for these
# pre-existing, well-understood buckets. New type-engine code is dialyzer-clean
# and is NOT covered by a blanket entry.
#
# Buckets:
#   1. Version/capability gating — `Version.match?(System.version(), ...)` and
#      similar compile-time booleans bake one branch dead on any given Elixir;
#      dialyzer reports it as a `:pattern_match` (`false` vs `true`). Which line
#      is dead flips with the version.
#   2. Defensive fallback clauses — `_ -> ...` heads kept for robustness against
#      unexpected runtime values; dialyzer proves them unreachable
#      (`:pattern_match_cov`) but they stay on purpose.
#   3. Pre-existing peripheral logic in the vendored tokenizer/parser and the
#      normalized Macro.Env / Binding helpers.
#   4. ETS opaque-type boundary: `:ets.tid()` is an opaque reference, but
#      `Process.put/get` erases its opaqueness (the round-trip types the value as
#      `reference() | integer()`, not `:ets.tid()`). A liveness guard
#      (`is_reference/is_integer`) then narrows it to a non-opaque type, and
#      `:ets.info/1` reports `call_without_opaque`. The code is correct at
#      runtime and is wrapped in `rescue _ -> false` for safety; the opaqueness
#      loss is unavoidable through the process-dictionary boundary.
[
  # 1. Version/capability gating
  {"lib/elixir_sense/core/compiler.ex", :pattern_match},
  {"lib/elixir_sense/core/compiler/clauses.ex", :pattern_match},
  {"lib/elixir_sense/core/compiler/fn.ex", :pattern_match},
  {"lib/elixir_sense/core/compiler/state.ex", :pattern_match},
  {"lib/elixir_sense/core/elixir_types.ex", :pattern_match},
  {"lib/elixir_sense/core/type_inference.ex", :pattern_match},

  # 2. Defensive fallback clauses (unreachable per success typing, kept on purpose)
  {"lib/elixir_sense/core/compiler/state.ex", :pattern_match_cov},
  {"lib/elixir_sense/core/elixir_types.ex", :pattern_match_cov},

  # 3. Pre-existing peripheral logic (vendored tokenizer/parser, normalized helpers)
  {"lib/elixir_sense/core/binding.ex", :pattern_match},
  {"lib/elixir_sense/core/normalized/macro/env.ex", :pattern_match},
  {"lib/elixir_sense/core/normalized/tokenizer.ex", :contract_supertype},
  {"lib/elixir_sense/core/normalized/tokenizer.ex", :pattern_match},
  {"lib/elixir_sense/core/parser.ex", :guard_fail},

  # 3b. Completion engine ported wholesale from elixir-ls (commit 98e983d). The unreachable `:error`
  #     clauses in expand_dot_path/expand_aliases are upstream-faithful defensive fallbacks.
  {"lib/elixir_sense/providers/completion/completion_engine.ex", :pattern_match},

  # 4. ETS opaque-type boundary (see note above)
  {"lib/elixir_sense/core/elixir_types.ex", :call_without_opaque}
]
