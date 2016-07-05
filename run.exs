requires = [
  "lib/elixir_sense/core/introspection.ex",
  "lib/elixir_sense/core/ast.ex",
  "lib/elixir_sense/core/state.ex",
  "lib/elixir_sense/core/metadata_builder.ex",
  "lib/elixir_sense/core/metadata.ex",
  "lib/elixir_sense/core/parser.ex",
  "lib/elixir_sense/core/source.ex",
  "lib/alchemist/helpers/module_info.ex",
  "lib/alchemist/helpers/complete.ex",
  "lib/elixir_sense/providers/definition.ex",
  "lib/elixir_sense/providers/docs.ex",
  "lib/elixir_sense/providers/suggestion.ex",
  "lib/elixir_sense/providers/signature.ex",
  "lib/elixir_sense.ex",
  "lib/alchemist/api/comp.ex",
  "lib/alchemist/api/defl.ex",
  "lib/alchemist/api/docl.ex",
  "lib/alchemist/api/eval.ex",
]

requires |> Enum.each(fn file ->
  Code.require_file(file, __DIR__)
end)
