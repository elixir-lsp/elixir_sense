defmodule ElixirSense.Providers.Suggestion.Reducers.Callbacks do
  @moduledoc false

  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.State

  @type callback :: %{
          type: :callback,
          name: String.t(),
          arity: non_neg_integer,
          args: String.t(),
          origin: String.t(),
          summary: String.t(),
          spec: String.t(),
          metadata: map
        }

  @doc """
  A reducer that adds suggestions of callbacks.
  """
  def add_callbacks(_hint, _text_before, %State.Env{scope: {_f, _a}}, _buffer_metadata, acc),
    do: {:cont, acc}

  def add_callbacks(hint, _text_before, env, _buffer_metadata, acc) do
    %State.Env{protocol: protocol, behaviours: behaviours} = env

    list =
      Enum.flat_map(behaviours, fn
        mod when is_atom(mod) and (protocol == nil or mod != elem(protocol, 0)) ->
          mod_name = inspect(mod)

          for %{
                name: name,
                arity: arity,
                callback: spec,
                signature: signature,
                doc: doc,
                metadata: metadata
              } <-
                Introspection.get_callbacks_with_docs(mod),
              hint == "" or String.starts_with?("#{name}", hint) do
            desc = Introspection.extract_summary_from_docs(doc)
            [_, args_str] = Regex.run(Regex.recompile!(~r/.\((.*)\)/), signature)
            args = args_str |> String.replace(Regex.recompile!(~r/\s/), "")

            %{
              type: :callback,
              name: Atom.to_string(name),
              arity: arity,
              args: args,
              origin: mod_name,
              summary: desc,
              spec: spec,
              metadata: metadata
            }
          end

        _ ->
          []
      end)

    {:cont, %{acc | result: acc.result ++ Enum.sort(list)}}
  end
end
