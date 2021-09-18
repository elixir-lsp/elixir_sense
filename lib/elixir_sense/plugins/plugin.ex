defmodule ElixirSense.Plugins.Plugin do
  alias ElixirSense.Core.State
  @type suggestion :: ElixirSense.Providers.Suggestion.generic()

  @type acc :: %{context: term, result: list(suggestion())}
  @type cursor_context :: %{
          text_before: String.t(),
          text_after: String.t(),
          at_module_body?: boolean
        }

  @callback reduce(
              hint :: String,
              env :: State.Env.t(),
              buffer_metadata :: Metadata.t(),
              cursor_context,
              acc
            ) :: {:cont, acc} | {:halt, acc}

  @callback decorate(suggestion) :: suggestion

  @optional_callbacks decorate: 1, reduce: 5
end
