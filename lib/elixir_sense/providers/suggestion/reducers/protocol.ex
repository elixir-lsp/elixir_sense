defmodule ElixirSense.Providers.Suggestion.Reducers.Protocol do
  @moduledoc false

  alias ElixirSense.Core.{Introspection, State}

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
  A reducer that adds suggestions of protocol functions.
  """
  def add_functions(_hint, _text_before, %State.Env{scope: {_f, _a}}, _buffer_metadata, acc),
    do: {:cont, acc}

  def add_functions(_hint, _text_before, %State.Env{protocol: nil}, _buffer_metadata, acc),
    do: {:cont, acc}

  def add_functions(hint, _text_before, env, _buffer_metadata, acc) do
    %State.Env{protocol: {protocol, _implementations}} = env

    list =
      for {{name, arity}, {_type, args, docs, metadata, spec}} <-
            Introspection.module_functions_info(protocol),
          hint == "" or String.starts_with?("#{name}", hint) do
        %{
          type: :protocol_function,
          name: Atom.to_string(name),
          arity: arity,
          args: args,
          origin: inspect(protocol),
          summary: docs,
          metadata: metadata,
          spec: spec
        }
      end
    {:cont, %{acc | result: acc.result ++ Enum.sort(list)}}
  end
end
