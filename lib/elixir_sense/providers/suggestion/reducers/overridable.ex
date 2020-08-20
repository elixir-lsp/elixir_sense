defmodule ElixirSense.Providers.Suggestion.Reducers.Overridable do
  @moduledoc false

  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.State

  @type protocol_function :: %{
          type: :protocol_function,
          name: String.t(),
          arity: non_neg_integer,
          args: String.t(),
          origin: String.t(),
          summary: String.t(),
          spec: String.t(),
          metadata: map
        }

  @doc """
  A reducer that adds suggestions of overridable functions.
  """
  def add_functions(_hint, %State.Env{scope: {_f, _a}}, _metadata, _cursor_context, acc),
    do: {:cont, acc}

  def add_functions(hint, env, metadata, _cursor_context, acc) do
    %State.Env{protocol: protocol, behaviours: behaviours, module: module} = env

    # overridable behaviour callbacks are returned by Reducers.Callbacks
    behaviour_callbacks =
      Enum.flat_map(behaviours, fn
        mod when is_atom(mod) and (protocol == nil or mod != elem(protocol, 0)) ->
          for %{
                name: name,
                arity: arity
              } <-
                Introspection.get_callbacks_with_docs(mod) do
            {mod, name, arity}
          end

        _ ->
          []
      end)

    list =
      for {{^module, name, arity}, %State.ModFunInfo{overridable: {true, origin}} = info}
          when is_integer(arity) <- metadata.mods_funs_to_positions,
          def_prefix?(hint, info.type) or String.starts_with?("#{name}", hint),
          {origin, name, arity} not in behaviour_callbacks do
        spec =
          case metadata.specs[{module, name, arity}] do
            %State.SpecInfo{specs: specs} -> specs |> Enum.join("\n")
            nil -> ""
          end

        args = info.params |> hd |> Enum.map_join(", ", &(&1 |> elem(0) |> Atom.to_string()))

        %{
          type: :callback,
          name: Atom.to_string(name),
          arity: arity,
          args: args,
          origin: inspect(origin),
          summary: "",
          metadata: %{},
          spec: spec
        }
      end

    {:cont, %{acc | result: acc.result ++ Enum.sort(list)}}
  end

  defp def_prefix?(hint, type) when type in [:defmacro, :defmacrop] do
    String.starts_with?("defmacro", hint)
  end

  defp def_prefix?(hint, type) when type in [:def, :defp] do
    String.starts_with?("def", hint)
  end
end
