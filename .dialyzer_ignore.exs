# Dialyzer warnings suppressed for ElixirSense.
#
# Entries are `{file, warning_type}` (no line) **on purpose**: the compiler
# adapter supports multiple Elixir versions (~> 1.16), and these warnings move
# between lines from one version to the next (version-gating dead branches flip;
# the vendored tokenizer/parser differ). Pinning exact lines made the file break
# on every `mix format` and every Elixir switch. The trade-off is that a *new*
# warning of the same type in the same file would also be hidden — acceptable
# for these pre-existing, well-understood buckets.
#
# Buckets:
#   1. Version/capability gating — `Version.match?(System.version(), ...)`
#      compile-time booleans (`@stamp_version`, `@stop_generated_on_args`) bake
#      one branch dead on any given Elixir; dialyzer reports it as a
#      `:pattern_match` (`false` vs `true`). Which line is dead flips with the
#      version.
#   2. Defensive fallback clauses kept for robustness (vendored tokenizer/parser
#      and the normalized Macro.Env helpers); dialyzer proves them unreachable
#      under the current host's success typing but they stay on purpose.
[
  # 1. Version/capability gating
  {"lib/elixir_sense/core/compiler.ex", :pattern_match},
  {"lib/elixir_sense/core/compiler/clauses.ex", :pattern_match},
  {"lib/elixir_sense/core/compiler/fn.ex", :pattern_match},
  {"lib/elixir_sense/core/normalized/macro/env.ex", :pattern_match},

  # 2. Pre-existing peripheral logic (vendored tokenizer/parser, normalized helpers)
  {"lib/elixir_sense/core/normalized/tokenizer.ex", :contract_supertype},
  {"lib/elixir_sense/core/normalized/tokenizer.ex", :pattern_match}
]
