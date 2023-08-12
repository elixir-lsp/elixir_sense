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
  alias ElixirSense.Core.ModuleStore
  alias ElixirSense.Core.State
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
          | Reducers.Bitstring.bitstring_option()

  @type acc :: %{result: [suggestion], reducers: [atom], context: map}
  @type cursor_context :: %{
          text_before: String.t(),
          text_after: String.t(),
          at_module_body?: boolean(),
          cursor_position: {pos_integer, pos_integer}
        }

  @reducers [
    structs_fields: &Reducers.Struct.add_fields/5,
    returns: &Reducers.Returns.add_returns/5,
    callbacks: &Reducers.Callbacks.add_callbacks/5,
    protocol_functions: &Reducers.Protocol.add_functions/5,
    overridable: &Reducers.Overridable.add_overridable/5,
    param_options: &Reducers.Params.add_options/5,
    typespecs: &Reducers.TypeSpecs.add_types/5,
    populate_common: &Reducers.Common.populate/6,
    variables: &Reducers.Common.add_variables/5,
    modules: &Reducers.Common.add_modules/5,
    functions: &Reducers.Common.add_functions/5,
    macros: &Reducers.Common.add_macros/5,
    variable_fields: &Reducers.Common.add_fields/5,
    attributes: &Reducers.Common.add_attributes/5,
    docs_snippets: &Reducers.DocsSnippets.add_snippets/5,
    bitstring_options: &Reducers.Bitstring.add_bitstring_options/5
  ]

  @add_opts_for [:populate_common]

  @doc """
  Finds all suggestions for a hint based on context information.
  """
  @spec find(String.t(), State.Env.t(), Metadata.t(), cursor_context, ModuleStore.t(), keyword()) ::
          [suggestion()]
  def find(
        hint,
        env,
        buffer_metadata,
        cursor_context,
        %{plugins: plugins} = module_store,
        opts \\ []
      ) do
    reducers =
      plugins
      |> Enum.filter(&function_exported?(&1, :reduce, 5))
      |> Enum.map(fn module ->
        {module, &module.reduce/5}
      end)
      |> Enum.concat(@reducers)
      |> maybe_add_opts(opts)

    context =
      plugins
      |> Enum.filter(&function_exported?(&1, :setup, 1))
      |> Enum.reduce(%{module_store: module_store}, fn plugin, context ->
        plugin.setup(context)
      end)

    acc = %{result: [], reducers: Keyword.keys(reducers), context: context}

    %{result: result} =
      Enum.reduce_while(reducers, acc, fn {key, fun}, acc ->
        if key in acc.reducers do
          fun.(hint, env, buffer_metadata, cursor_context, acc)
        else
          {:cont, acc}
        end
      end)

    for item <- result do
      plugins
      |> Enum.filter(&function_exported?(&1, :decorate, 1))
      |> Enum.reduce(item, fn module, item -> module.decorate(item) end)
    end
  end

  defp maybe_add_opts(reducers, opts) do
    Enum.map(reducers, fn {name, reducer} ->
      if name in @add_opts_for do
        {name, reducer_with_opts(reducer, opts)}
      else
        {name, reducer}
      end
    end)
  end

  defp reducer_with_opts(fun, opts) do
    fn a, b, c, d, e -> fun.(a, b, c, d, e, opts) end
  end
end
