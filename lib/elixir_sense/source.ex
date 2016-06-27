defmodule ElixirSense.Source do

  def get_prefix(code, line, col) do
    line = code |> String.split("\n") |> Enum.at(line-1)
    line |> String.slice(0, col-1)
  end

  def which_func(prefix) do
    tokens =
      case prefix |> String.to_char_list |> :elixir_tokenizer.tokenize(1, []) do
        {:ok, _, _, tokens} ->
          tokens |> Enum.reverse
        {:error, {_line, error_prefix, _token}, _rest, sofar} ->
          :elixir_utils.characters_to_binary(error_prefix)
          sofar
      end

    %{candidate: candidate, npar: npar} = scan(tokens, %{npar: 0, count: 0, candidate: []})

    case candidate do
      []          -> :none
      [func]      -> {nil, func, npar}
      [mod, func] -> {mod, func, npar}
      list        ->
        [func|mods] = Enum.reverse(list)
        {Module.concat(Enum.reverse(mods)), func, npar}
    end
  end

  defp scan([{:",", _}|_], %{count: 1} = state), do: state
  defp scan([{:",", _}|tokens], %{count: 0} = state) do
    scan(tokens, %{state | npar: state.npar + 1, candidate: []})
  end
  defp scan([{:"(", _}|_], %{count: 1} = state), do: state
  defp scan([{:"(", _}|tokens], state) do
    scan(tokens, %{state | count: state.count + 1, candidate: []})
  end
  defp scan([{:")", _}|tokens], state) do
    scan(tokens, %{state | count: state.count - 1, candidate: []})
  end
  defp scan([{:paren_identifier, _, value}|tokens], %{count: 1} = state) do
    scan(tokens, %{state | candidate: [value|state.candidate]})
  end
  defp scan([{:aliases, _, [value]}|tokens], %{count: 1} = state) do
    scan(tokens, %{state | candidate: [Module.concat([value])|state.candidate]})
  end
  defp scan([{:atom, _, value}|tokens], %{count: 1} = state) do
    scan(tokens, %{state | candidate: [value|state.candidate]})
  end
  defp scan([{:., _}|tokens], state), do: scan(tokens, state)
  defp scan([_|_], %{count: 1} = state), do: state
  defp scan([_token|tokens], state), do: scan(tokens, state)
  defp scan([], state), do: state

end
