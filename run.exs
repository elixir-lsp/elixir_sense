requires = [
  "elixir_sense/core/builtin_attributes.ex",
  "elixir_sense/core/applications.ex",
  "elixir_sense/core/state.ex",
  "elixir_sense/core/introspection.ex",
  "elixir_sense/core/edoc_reader.ex",
  "elixir_sense/core/erlang_html.ex",
  "elixir_sense/core/ast.ex",
  "elixir_sense/core/metadata_builder.ex",
  "elixir_sense/core/metadata.ex",
  "elixir_sense/core/parser.ex",
  "elixir_sense/core/source.ex",
  "elixir_sense/core/type_ast.ex",
  "elixir_sense/core/type_info.ex",
  "elixir_sense/core/builtin_functions.ex",
  "elixir_sense/core/builtin_types.ex",
  "elixir_sense/core/normalized/code.ex",
  "elixir_sense/core/normalized/code/cursor_context.ex",
  "elixir_sense/core/normalized/tokenizer.ex",
  "elixir_sense/core/normalized/typespec.ex",
  "elixir_sense/core/struct.ex",
  "elixir_sense/core/binding.ex",
  "elixir_sense/location.ex",
  "elixir_sense/providers/suggestion/complete.ex",
  "elixir_sense/providers/definition.ex",
  "elixir_sense/providers/docs.ex",
  "elixir_sense/providers/suggestion.ex",
  "elixir_sense/providers/signature.ex",
  "elixir_sense/providers/expand.ex",
  "elixir_sense/providers/references.ex",
  "elixir_sense/providers/eval.ex",
  "elixir_sense/server/request_handler.ex",
  "elixir_sense/server/context_loader.ex",
  "elixir_sense/server/tcp_server.ex",
  "elixir_sense.ex",
  "elixir_sense/server.ex"
]

requires
|> Enum.each(fn file ->
  Code.require_file("lib/#{file}", __DIR__)
end)

ElixirSense.Server.start(System.argv())
