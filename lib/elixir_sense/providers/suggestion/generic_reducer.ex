defmodule ElixirSense.Providers.Suggestion.GenericReducer do
  @moduledoc """
  A generic behaviour for reducers that customize suggestions
  according to the cursor's position in a function call.
  """

  alias ElixirSense.Plugins.Util

  @type func_call :: {module, fun :: atom, arg :: non_neg_integer, any}
  @type suggestion :: ElixirSense.Providers.Suggestion.generic()
  @type reducer_name :: atom()

  @callback suggestions(hint :: String.t(), func_call, [func_call], opts :: map) ::
              :ignore
              | {:add | :override, [suggestion]}
              | {:add | :override, [suggestion], [reducer_name]}

  defmacro __using__(_) do
    quote do
      @behaviour unquote(__MODULE__)

      def reduce(hint, env, buffer_metadata, cursor_context, acc) do
        unquote(__MODULE__).reduce(__MODULE__, hint, env, buffer_metadata, cursor_context, acc)
      end
    end
  end

  def reduce(reducer, hint, env, buffer_metadata, cursor_context, acc) do
    text_before = cursor_context.text_before

    case Util.func_call_chain(text_before, env, buffer_metadata) do
      [func_call | _] = chain ->
        if :erlang.function_exported(reducer, :suggestions, 4) do
          reducer.suggestions(hint, func_call, chain, acc) |> handle_suggestions(acc)
        else
          {:cont, acc}
        end

      [] ->
        if :erlang.function_exported(reducer, :suggestions, 2) do
          reducer.suggestions(hint, acc) |> handle_suggestions(acc)
        else
          {:cont, acc}
        end
    end
  end

  def handle_suggestions(:ignore, acc) do
    {:cont, acc}
  end

  def handle_suggestions({:add, suggestions}, acc) do
    {:cont, %{acc | result: suggestions ++ acc.result}}
  end

  def handle_suggestions({:add, suggestions, reducers}, acc) do
    {:cont, %{acc | result: suggestions ++ acc.result, reducers: reducers}}
  end

  def handle_suggestions({:override, suggestions}, acc) do
    {:halt, %{acc | result: suggestions}}
  end

  def handle_suggestions({:override, suggestions, reducers}, acc) do
    {:cont, %{acc | result: suggestions, reducers: reducers}}
  end
end
