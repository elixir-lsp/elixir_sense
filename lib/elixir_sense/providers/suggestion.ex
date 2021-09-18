defmodule ElixirSense.Providers.Suggestion do
  @moduledoc """
  Provider responsible for finding suggestions for auto-completing.

  It provides suggestions based on a list of pre-defined reducers.

  ## Reducers

  A reducer is a function with the following spec:

      @spec reducer(
        String.t(),
        String.t(),
        State.Env.t(),
        Metadata.t(),
        acc()
      ) :: {:cont | :halt, acc()}

  ## Examples

  Adding suggestions:

      def my_reducer(hint, prefix, env, buffer_metadata, acc) do
        suggestions = ...
        {:cont, %{acc | result: acc.result ++ suggestions}}
      end

  Defining the only set of suggestions to be provided:

      def my_reducer(hint, prefix, env, buffer_metadata, acc) do
        suggestions = ...
        {:halt, %{acc | result: suggestions}}
      end

  Defining a list of suggestions to be provided and allow an extra
  limited set of additional reducers to run next:

      def my_reducer(hint, prefix, env, buffer_metadata, acc) do
        suggestions = ...
        {:cont, %{acc | result: fields, reducers: [:populate_common, :variables]}}
      end
  """

  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.State
  alias ElixirSense.Plugins.Util
  alias ElixirSense.Providers.Suggestion.Reducers

  @type generic :: %{
          type: :generic,
          label: String.t(),
          detail: String.t() | nil,
          documentation: String.t() | nil,
          insert_text: String.t() | nil,
          filter_text: String.t() | nil,
          snippet: String.t() | nil,
          priority: integer() | nil,
          kind: atom(),
          command: map()
        }

  @type suggestion ::
          generic()
          | Reducers.Common.attribute()
          | Reducers.Common.variable()
          | Reducers.Struct.field()
          | Reducers.Returns.return()
          | Reducers.Callbacks.callback()
          | Reducers.Protocol.protocol_function()
          | Reducers.Common.func()
          | Reducers.Common.mod()
          | Reducers.Params.param_option()
          | Reducers.TypeSpecs.type_spec()

  @type acc :: %{result: [suggestion], reducers: [atom], context: map}
  @type cursor_context :: %{
          text_before: String.t(),
          text_after: String.t(),
          at_module_body?: boolean()
        }

  @reducers [
    structs_fields: &Reducers.Struct.add_fields/5,
    returns: &Reducers.Returns.add_returns/5,
    callbacks: &Reducers.Callbacks.add_callbacks/5,
    protocol_functions: &Reducers.Protocol.add_functions/5,
    overridable: &Reducers.Overridable.add_overridable/5,
    param_options: &Reducers.Params.add_options/5,
    typespecs: &Reducers.TypeSpecs.add_types/5,
    populate_common: &Reducers.Common.populate/5,
    variables: &Reducers.Common.add_variables/5,
    modules: &Reducers.Common.add_modules/5,
    functions: &Reducers.Common.add_functions/5,
    macros: &Reducers.Common.add_macros/5,
    variable_fields: &Reducers.Common.add_fields/5,
    attributes: &Reducers.Common.add_attributes/5,
    docs_snippets: &Reducers.DocsSnippets.add_snippets/5
  ]

  @doc """
  Finds all suggestions for a hint based on context information.
  """
  @spec find(String.t(), State.Env.t(), Metadata.t(), cursor_context) :: [suggestion()]
  def find(hint, env, buffer_metadata, cursor_context) do
    plugins = Util.plugins()

    plugin_reducers =
      plugins
      |> Enum.filter(&:erlang.function_exported(&1, :reduce, 5))
      |> Enum.map(fn plugin ->
        {plugin, &plugin.reduce/5}
      end)

    plugin_decorators =
      plugins
      |> Enum.filter(&:erlang.function_exported(&1, :decorate, 1))
      |> Enum.map(fn plugin ->
        &plugin.decorate/1
      end)

    reducers = plugin_reducers ++ @reducers

    acc = %{result: [], reducers: Keyword.keys(reducers), context: %{}}

    %{result: result} =
      Enum.reduce_while(reducers, acc, fn
        {key, fun}, acc ->
          if key in acc.reducers do
            fun.(hint, env, buffer_metadata, cursor_context, acc)
          else
            {:cont, acc}
          end
      end)

    for item <- result do
      Enum.reduce(plugin_decorators, item, fn d, item -> d.(item) end)
    end
  end
end
