defmodule ElixirSense.Core.Source do
  @moduledoc """
  Source parsing
  """

  alias ElixirSense.Core.Normalized.Tokenizer

  @empty_graphemes [" ", "\n", "\r\n"]
  @stop_graphemes ~w/{ } ( ) [ ] < > + - * & ^ , ; ~ % = " ' \\ \/ $ ! ?`#/ ++ @empty_graphemes

  def split_module_and_hint(hint, aliases \\ []) do
    if String.ends_with?(hint, ".") do
      {mod, _} =
        hint
        |> String.slice(0..-2)
        |> split_module_and_func(aliases)
      {mod, ""}
    else
      {mod, new_hint} =
        hint
        |> split_module_and_func(aliases)
      {mod, to_string(new_hint)}
    end
  end

  def split_module_and_func(call, aliases \\ []) do
    case Code.string_to_quoted(call) do
      {:error, _} ->
        {nil, nil}
      {:ok, quoted} when is_atom(quoted) ->
        {quoted, nil}
      {:ok, quoted} ->
        split_mod_quoted_fun_call(quoted, aliases)
    end
  end

  def prefix(code, line, col) do
    line = code |> String.split("\n") |> Enum.at(line - 1, "")
    line = if String.length(line) < col do
      line_padding = for _ <- 1..(String.length(line) - col), into: "", do: " "
      line <> line_padding
    else
      line
    end
    line_str = line |> String.slice(0, col - 1)
    case Regex.run(Regex.recompile!(~r/[\w0-9\._!\?\:@]+$/), line_str) do
      nil -> ""
      [prefix] -> prefix
    end
  end

  def text_before(code, line, col) do
    pos = find_position(code, line, col, {0, 1, 1})
    {text, _rest} = String.split_at(code, pos)
    text
  end

  def text_after(code, line, col) do
    pos = find_position(code, line, col, {0, 1, 1})
    {_, rest} = String.split_at(code, pos)
    rest
  end

  def subject(code, line, col) do
    acc = %{line: line, col: col, pos_found: false, candidate: [], pos: nil}
    case walk_text(code, acc, &find_subject/5) do
      %{candidate: []} ->
        nil
      %{candidate: candidate} ->
        candidate |> Enum.reverse |> Enum.join
    end
  end

  def subject_with_position(code, line, col) do
    acc = %{line: line, col: col, pos_found: false, candidate: [], pos: nil}
    case walk_text(code, acc, &find_subject/5) do
      %{candidate: []} ->
        nil
      %{candidate: candidate, pos: {line, col}} ->
        subject = candidate |> Enum.reverse |> Enum.join
        last_part = subject |> String.reverse() |> String.split(".", parts: 2) |> Enum.at(0) |> String.reverse()
        {subject, {line, col - String.length(last_part)}}
    end
  end

  def find_next_word(code) do
    walk_text(code, nil, fn
      grapheme, rest, _, _, _ when grapheme in @empty_graphemes ->
        {rest, nil}
      _grapheme, rest, line, col, _ ->
        {"", {rest, line - 1, col - 1}}
    end)
  end

  def which_struct(text_before) do
    code = text_before |> String.reverse()
    case walk_text(code, %{buffer: [], count_open: 0, result: nil}, &find_struct/5) do
      %{result: nil} ->
        nil
      %{result: result} ->
        result
        |> Enum.join
        |> Kernel.<>("_: _}")
        |> Code.string_to_quoted()
        |> extract_struct_module()
    end
  end

  defp extract_struct_module({:ok, {:%, _, [{:__aliases__, _, module_list}, {:%{},_, fields}]}}) do
    fields_names = Keyword.keys(fields) |> Enum.slice(0..-2)
    {Module.concat(module_list), fields_names}
  end
  defp extract_struct_module(_) do
    nil
  end

  defp find_struct("%" = grapheme, _rest, _line, _col, %{buffer: buffer, count_open: 1} = acc) do
    {"", %{acc | result: [grapheme|buffer]}}
  end
  defp find_struct("{" = grapheme, rest, _line, _col, %{buffer: buffer, count_open: count_open} = acc) do
    {rest, %{acc | buffer: [grapheme|buffer], count_open: count_open + 1}}
  end
  defp find_struct("}" = grapheme, rest, _line, _col, %{buffer: buffer, count_open: count_open} = acc) do
    {rest, %{acc | buffer: [grapheme|buffer], count_open: count_open - 1}}
  end
  defp find_struct(grapheme, rest, _line, _col, %{buffer: buffer} = acc) do
    {rest, %{acc | buffer: [grapheme|buffer]}}
  end

  defp find_subject(grapheme, rest, line, col, %{pos_found: false, line: line, col: col} = acc) do
    find_subject(grapheme, rest, line, col, %{acc | pos_found: true})
  end
  defp find_subject("." = grapheme, rest, _line, _col, %{pos_found: false} = acc) do
    {rest, %{acc | candidate: [grapheme|acc.candidate]}}
  end
  defp find_subject(".", _rest, line, col, %{pos_found: true} = acc) do
    {"", %{acc | pos: {line, col-1}}}
  end
  defp find_subject(grapheme, rest, _line, _col, %{candidate: [_|_]} = acc) when grapheme in ["!", "?"] do
    {rest, %{acc | candidate: [grapheme|acc.candidate]}}
  end
  defp find_subject(grapheme, rest, _line, _col, %{candidate: ["."|_]} = acc) when grapheme in @stop_graphemes do
    {rest, acc}
  end
  defp find_subject(grapheme, rest, _line, _col, %{pos_found: false} = acc) when grapheme in @stop_graphemes do
    {rest, %{acc | candidate: []}}
  end
  defp find_subject(grapheme, _rest, line, col, %{pos_found: true} = acc) when grapheme in @stop_graphemes do
    {"", %{acc | pos: {line, col-1}}}
  end
  defp find_subject(grapheme, rest, _line, _col, acc) do
    {rest, %{acc | candidate: [grapheme|acc.candidate]}}
  end

  defp walk_text(text, acc, func) do
    do_walk_text(text, func, 1, 1, acc)
  end

  defp do_walk_text(text, func, line, col, acc) do
    case String.next_grapheme(text) do
      nil ->
        acc
      {grapheme, rest} ->
        {new_rest, new_acc} = func.(grapheme, rest, line, col, acc)
        {new_line, new_col} =
          if grapheme in ["\n", "\r\n"] do
            {line + 1, 1}
          else
            {line, col + 1}
          end

        do_walk_text(new_rest, func, new_line, new_col, new_acc)
    end
  end

  defp find_position(_text, line, col, {pos, line, col}) do
    pos
  end

  defp find_position(text, line, col, {pos, current_line, current_col}) do
    case String.next_grapheme(text) do
      {grapheme, rest} ->
        {new_pos, new_line, new_col} =
          if grapheme in ["\n", "\r\n"] do
            if current_line == line do
              # this is the line we're lookin for
              # but it's shorter than expected
              {pos, current_line, col}
            else
              {pos + 1, current_line + 1, 1}
            end
          else
            {pos + 1, current_line, current_col + 1}
          end
          find_position(rest, line, col, {new_pos, new_line, new_col})
      nil ->
        pos
    end
  end

  def which_func(prefix) do
    tokens = Tokenizer.tokenize(prefix)

    pattern = %{npar: 0, count: 0, count2: 0, candidate: [], pos: nil, pipe_before: false}
    result = scan(tokens, pattern)
    %{candidate: candidate, npar: npar, pipe_before: pipe_before, pos: pos} = result

    %{
      candidate: normalize_candidate(candidate),
      npar: normalize_npar(npar, pipe_before),
      pipe_before: pipe_before,
      pos: pos
    }
  end

  defp normalize_candidate(candidate) do
    case candidate do
      []          -> :none
      [func]      -> {nil, func}
      [mod, func] -> {mod, func}
      list        ->
        [func|mods] = Enum.reverse(list)
        {Module.concat(Enum.reverse(mods)), func}
    end
  end

  defp normalize_npar(npar, true), do: npar + 1
  defp normalize_npar(npar, _pipe_before), do: npar

  defp scan([{:kw_identifier, _, _}|tokens], %{npar: 1} = state) do
    scan(tokens, %{state | npar: 0})
  end
  defp scan([{:",", _}|_], %{count: 1} = state), do: state
  defp scan([{:",", _}|tokens], %{count: 0, count2: 0} = state) do
    scan(tokens, %{state | npar: state.npar + 1, candidate: []})
  end
  defp scan([{:"(", _}|_], %{count: 1} = state), do: state
  defp scan([{:"(", _}|tokens], state) do
    scan(tokens, %{state | count: state.count + 1, candidate: []})
  end
  defp scan([{:")", _}|tokens], state) do
    scan(tokens, %{state | count: state.count - 1, candidate: []})
  end
  defp scan([{token, _}|tokens], %{count2: 0} = state) when token in [:"[", :"{"] do
    scan(tokens, %{state | npar: 0, count2: 0})
  end
  defp scan([{token, _}|tokens], state) when token in [:"[", :"{"] do
    scan(tokens, %{state | count2: state.count2 + 1})
  end
  defp scan([{token, _}|tokens], state) when token in [:"]", :"}"]do
    scan(tokens, %{state | count2: state.count2 - 1})
  end
  defp scan([{:paren_identifier, pos, value}|tokens], %{count: 1} = state) do
    scan(tokens, %{state | candidate: [value|state.candidate], pos: update_pos(pos, state.pos)})
  end
  defp scan([{:aliases, pos, [value]}|tokens], %{count: 1} = state) do
    updated_pos = update_pos(pos, state.pos)
    scan(tokens, %{state | candidate: [Module.concat([value])|state.candidate], pos: updated_pos})
  end
  defp scan([{:alias, pos, value}|tokens], %{count: 1} = state) do
    updated_pos = update_pos(pos, state.pos)
    scan(tokens, %{state | candidate: [Module.concat([value])|state.candidate], pos: updated_pos})
  end
  defp scan([{:atom, pos, value}|tokens], %{count: 1} = state) do
    scan(tokens, %{state | candidate: [value|state.candidate], pos: update_pos(pos, state.pos)})
  end
  defp scan([{:fn, _}|tokens], %{count: 1} = state) do
    scan(tokens, %{state | npar: 0, count: 0})
  end
  defp scan([{:., _}|tokens], state), do: scan(tokens, state)
  defp scan([{:arrow_op, _, :|>}|_], %{count: 1} = state), do: pipe_before(state)
  defp scan([_|_], %{count: 1} = state), do: state
  defp scan([_token|tokens], state), do: scan(tokens, state)
  defp scan([], state), do: state

  defp update_pos({line, init_col, end_col}, nil) do
    {{line, init_col}, {line, end_col}}
  end
  defp update_pos({new_init_line, new_init_col, _}, {{_, _}, {end_line, end_col}}) do
    {{new_init_line, new_init_col}, {end_line, end_col}}
  end

  defp pipe_before(state) do
    %{state | pipe_before: true}
  end

  defp split_mod_quoted_fun_call(quoted, aliases) do
    case Macro.decompose_call(quoted) do
      {{:__aliases__, _, mod_parts}, fun, _args} ->
        case concat_module_parts(mod_parts, aliases) do
          {:ok, concated} -> {concated, fun}
          :error -> {nil, nil}
        end
      {:__aliases__, mod_parts} ->
        case concat_module_parts(mod_parts, aliases) do
          {:ok, concated} -> {concated, nil}
          :error -> {nil, nil}
        end
      {mod, func, []} when is_atom(mod) and is_atom(func) ->
        {mod, func}
      {func, []} when is_atom(func) ->
        {nil, func}
      _ -> {nil, nil}
    end
  end

  def concat_module_parts([name | rest], aliases) when is_atom(name) do
    case Keyword.fetch(aliases, Module.concat(Elixir, name)) do
      {:ok, name} when rest == [] -> {:ok, name}
      {:ok, name} -> {:ok, Module.concat([name | rest])}
      :error -> {:ok, Module.concat([name | rest])}
    end
  end
  def concat_module_parts([_ | _], _), do: :error

end
