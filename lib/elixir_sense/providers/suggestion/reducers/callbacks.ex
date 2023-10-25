defmodule ElixirSense.Providers.Suggestion.Reducers.Callbacks do
  @moduledoc false

  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.State
  alias ElixirSense.Providers.Suggestion.Matcher

  @type callback :: %{
          type: :callback,
          subtype: :callback | :macrocallback,
          name: String.t(),
          arity: non_neg_integer,
          args: String.t(),
          args_list: [String.t()],
          origin: String.t(),
          summary: String.t(),
          spec: String.t(),
          metadata: map
        }

  @doc """
  A reducer that adds suggestions of callbacks.
  """
  def add_callbacks(hint, env, _buffer_metadata, context, acc) do
    text_before = context.text_before

    %State.Env{protocol: protocol, behaviours: behaviours, scope: scope} = env

    list =
      Enum.flat_map(behaviours, fn
        mod when is_atom(mod) and (protocol == nil or mod != elem(protocol, 0)) ->
          mod_name = inspect(mod)

          for %{
                name: name,
                arity: arity,
                kind: kind,
                callback: spec,
                signature: signature,
                doc: doc,
                metadata: metadata
              } <-
                Introspection.get_callbacks_with_docs(mod),
              def_prefix?(hint, spec) or Matcher.match?("#{name}", hint) do
            desc = Introspection.extract_summary_from_docs(doc)
            [_, args_str] = Regex.run(~r/.\(([^\)]*)\)/u, signature)

            args_list =
              args_str
              |> String.split(",")
              |> Enum.map(&String.trim/1)

            %{
              type: :callback,
              subtype: kind,
              name: Atom.to_string(name),
              arity: arity,
              args:
                args_str
                |> String.replace("\n", " ")
                |> String.split(",")
                |> Enum.map_join(", ", &String.trim/1),
              args_list: args_list,
              origin: mod_name,
              summary: desc,
              spec: spec,
              metadata: metadata
            }
          end

        _ ->
          []
      end)

    list = Enum.sort(list)

    cond do
      Regex.match?(~r/\s(def|defmacro)\s+([_\p{Ll}\p{Lo}][\p{L}\p{N}_]*[?!]?)?$/u, text_before) ->
        {:halt, %{acc | result: list}}

      match?({_f, _a}, scope) ->
        {:cont, acc}

      true ->
        {:cont, %{acc | result: acc.result ++ list}}
    end
  end

  defp def_prefix?(hint, spec) do
    if String.starts_with?(spec, "@macrocallback") do
      String.starts_with?("defmacro", hint)
    else
      String.starts_with?("def", hint)
    end
  end
end
