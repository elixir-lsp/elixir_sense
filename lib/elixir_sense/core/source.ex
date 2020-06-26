defmodule ElixirSense.Core.Source do
  @moduledoc """
  Source parsing
  """

  alias ElixirSense.Core.Normalized.Tokenizer

  @line_break ["\n", "\r\n"]
  @empty_graphemes [" "] ++ @line_break
  @stop_graphemes [
                    "{",
                    "}",
                    "(",
                    ")",
                    "[",
                    "]",
                    "<",
                    ">",
                    "+",
                    "-",
                    "*",
                    "&",
                    "^",
                    ",",
                    ";",
                    "~",
                    "%",
                    "=",
                    "\\",
                    "\/",
                    "$",
                    "!",
                    "?",
                    "`",
                    "#"
                  ] ++ @empty_graphemes

  @spec split_module_and_hint(String.t(), module | nil, [{module, module}]) ::
          {nil | module, String.t()}
  def split_module_and_hint(hint, current_module \\ nil, aliases \\ []) do
    if String.ends_with?(hint, ".") do
      {mod, _} =
        hint
        |> String.slice(0..-2)
        |> split_module_and_func(current_module, aliases)

      {mod, ""}
    else
      {mod, new_hint} =
        hint
        |> split_module_and_func(current_module, aliases)

      {mod, to_string(new_hint)}
    end
  end

  @doc ~S"""
  Splits function call into module and function

  ## Examples

      iex> ElixirSense.Core.Source.split_module_and_func("MyMod.my_func", CurrentMod)
      {MyMod, :my_func}
      iex> ElixirSense.Core.Source.split_module_and_func("MyMod.Sub.my_func", CurrentMod)
      {MyMod.Sub, :my_func}
      iex> ElixirSense.Core.Source.split_module_and_func("MyAlias.my_func", CurrentMod, [{MyAlias, My.Mod}])
      {My.Mod, :my_func}
      iex> ElixirSense.Core.Source.split_module_and_func("my_func", CurrentMod)
      {nil, :my_func}
      iex> ElixirSense.Core.Source.split_module_and_func(":erlang_mod.my_func", CurrentMod)
      {:erlang_mod, :my_func}
      iex> ElixirSense.Core.Source.split_module_and_func("Elixir.MyMod.my_func", CurrentMod)
      {MyMod, :my_func}
      iex> ElixirSense.Core.Source.split_module_and_func(":\"Elixir.MyMod\".my_func", CurrentMod)
      {MyMod, :my_func}
      iex> ElixirSense.Core.Source.split_module_and_func("__MODULE__.my_func", CurrentMod)
      {CurrentMod, :my_func}
      iex> ElixirSense.Core.Source.split_module_and_func("__MODULE__.Sub.my_func", CurrentMod)
      {CurrentMod.Sub, :my_func}
      iex> ElixirSense.Core.Source.split_module_and_func("MyModule", CurrentMod)
      {MyModule, nil}
      iex> ElixirSense.Core.Source.split_module_and_func("__MODULE__", CurrentMod)
      {CurrentMod, nil}
      iex> ElixirSense.Core.Source.split_module_and_func("__MODULE__.Sub", CurrentMod)
      {CurrentMod.Sub, nil}
      iex> ElixirSense.Core.Source.split_module_and_func(":erlang_module", CurrentMod)
      {:erlang_module, nil}
      iex> ElixirSense.Core.Source.split_module_and_func("Elixir.MyMod", CurrentMod)
      {MyMod, nil}
      iex> ElixirSense.Core.Source.split_module_and_func(":\"Elixir.MyMod\"", CurrentMod)
      {MyMod, nil}
      iex> ElixirSense.Core.Source.split_module_and_func("MyAlias", CurrentMod, [{MyAlias, My.Mod}])
      {My.Mod, nil}
      iex> ElixirSense.Core.Source.split_module_and_func("", CurrentMod)
      {nil, nil}
      iex> ElixirSense.Core.Source.split_module_and_func("Elixir.Keyword.A", CurrentMod, [{Keyword, My.Mod}])
      {Keyword.A, nil}
      iex> ElixirSense.Core.Source.split_module_and_func("Elixir.Keyword", CurrentMod, [{Keyword, My.Mod}])
      {Keyword, nil}

  """
  @spec split_module_and_func(String.t()) :: {module | nil, atom | nil}
  @spec split_module_and_func(String.t(), module | nil) :: {module | nil, atom | nil}
  @spec split_module_and_func(String.t(), module | nil, [{module, module}]) ::
          {module | nil, atom | nil}
  def split_module_and_func(call, current_module \\ nil, aliases \\ [])
  def split_module_and_func("", _current_module, _aliases), do: {nil, nil}

  def split_module_and_func(call, current_module, aliases) do
    case Code.string_to_quoted(call) do
      {:error, _} ->
        {nil, nil}

      {:ok, quoted} when is_atom(quoted) ->
        {quoted, nil}

      {:ok, quoted} ->
        split_mod_quoted_fun_call(quoted, current_module, aliases)
    end
  end

  @spec prefix(String.t(), pos_integer, pos_integer) :: String.t()
  def prefix(code, line, col) do
    line = code |> split_lines |> Enum.at(line - 1, "")

    line =
      if String.length(line) < col do
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

  @spec text_before(String.t(), pos_integer, pos_integer) :: String.t()
  def text_before(code, line, col) do
    pos = find_position(code, line, col, {0, 1, 1})
    {text, _rest} = String.split_at(code, pos)
    text
  end

  @spec text_after(String.t(), pos_integer, pos_integer) :: String.t()
  def text_after(code, line, col) do
    pos = find_position(code, line, col, {0, 1, 1})
    {_, rest} = String.split_at(code, pos)
    rest
  end

  @spec subject(String.t(), pos_integer, pos_integer) :: nil | String.t()
  def subject(code, line, col) do
    acc = %{line: line, col: col, pos_found: false, candidate: [], pos: nil}

    code =
      code
      |> split_lines
      |> Enum.map_join("\n", fn line ->
        # this is a naive comment strip - it will not honour # in strings, chars etc
        Regex.replace(~r/[^<]\#.*$/, line, "")
      end)

    case walk_text(code, acc, &find_subject/5) do
      %{candidate: []} ->
        nil

      %{candidate: candidate} ->
        candidate |> Enum.reverse() |> Enum.join()
    end
  end

  @spec subject_with_position(String.t(), pos_integer, pos_integer) ::
          nil | {String.t(), {pos_integer, pos_integer}}
  def subject_with_position(code, line, col) do
    acc = %{line: line, col: col, pos_found: false, candidate: [], pos: nil}

    case walk_text(code, acc, &find_subject/5) do
      %{candidate: []} ->
        nil

      %{candidate: candidate, pos: {line, col}} ->
        subject = candidate |> Enum.reverse() |> Enum.join()

        last_part =
          subject
          |> String.reverse()
          |> String.split(".", parts: 2)
          |> Enum.at(0)
          |> String.reverse()

        {subject, {line, col - String.length(last_part) + 1}}
    end
  end

  @doc ~S"""
  Finds next word offset

  ## Examples

      iex> ElixirSense.Core.Source.find_next_word("my_funcs sds")
      {"y_funcs sds", 0, 0}
      iex> ElixirSense.Core.Source.find_next_word(" my_funcs sds")
      {"y_funcs sds", 0, 1}
      iex> ElixirSense.Core.Source.find_next_word("\nmy_funcs sds")
      {"y_funcs sds", 1, 0}
      iex> ElixirSense.Core.Source.find_next_word("")
      nil
      iex> ElixirSense.Core.Source.find_next_word(" ")
      nil
  """
  @spec find_next_word(String.t()) :: nil | {String.t(), non_neg_integer, non_neg_integer}
  def find_next_word(code) do
    walk_text(code, nil, fn
      grapheme, rest, _, _, _ when grapheme in @empty_graphemes ->
        {rest, nil}

      _grapheme, rest, line, col, _ ->
        {"", {rest, line - 1, col - 1}}
    end)
  end

  @type var_or_attr_t :: {:variable, atom} | {:attribute, atom} | nil

  @spec which_struct(String.t(), nil | module) ::
          nil
          | {{:atom, atom} | {:attribute, atom} | nil, [atom], boolean, var_or_attr_t}
          | {:map, [atom], var_or_attr_t}
  def which_struct(text_before, current_module) do
    code = text_before |> String.reverse()

    with %{result: result} when result != nil <-
           walk_text(code, %{buffer: [], count_open: 0, result: nil}, &find_struct/5),
         {:ok, ast} <-
           result
           |> Enum.join()
           |> Kernel.<>("_: _}")
           |> Code.string_to_quoted() do
      extract_struct_module(ast, current_module)
    else
      _ -> nil
    end
  end

  @spec get_v12_module_prefix(String.t(), module | nil) :: nil | String.t()
  def get_v12_module_prefix(text_before, current_module) do
    with %{"module" => module_str} <-
           Regex.named_captures(
             ~r/(alias|require|import|use)\s+(?<module>[^\s^\{^\}]+?)\.\{[^\}]*?$/,
             text_before
           ),
         {:ok, ast} <- Code.string_to_quoted(module_str),
         {:ok, module, elixir_prefix} <- extract_module(ast, current_module) do
      if elixir_prefix and module != Elixir do
        "Elixir." <> inspect(module)
      else
        inspect(module)
      end
    else
      _ ->
        nil
    end
  end

  @doc """
  Walks through `text` updating `acc` until a criteria is reached.
  """
  @spec walk_text(
          text :: binary,
          acc :: any(),
          (binary, binary, integer, integer, any -> {binary, any})
        ) :: any
  def walk_text(text, acc, func) do
    do_walk_text(text, func, 1, 1, acc)
  end

  defp extract_module({:__aliases__, _, module_list}, current_module) do
    module_list =
      case module_list do
        [{:__MODULE__, _, nil} | rest] -> [current_module | rest]
        otherwise -> otherwise
      end

    {:ok, Module.concat(module_list), hd(module_list) == Elixir}
  end

  defp extract_module({:__MODULE__, _, nil}, current_module) do
    {:ok, current_module, false}
  end

  defp extract_module(module, _current_module) when is_atom(module) do
    {:ok, module, false}
  end

  defp extract_module(_, _) do
    :error
  end

  defp get_field_names(fields) do
    Keyword.keys(fields) |> Enum.slice(0..-2)
  end

  defp get_var_or_attr({var, _, nil}) when is_atom(var) and var != :__MODULE__ do
    {:variable, var}
  end

  defp get_var_or_attr({:@, _, [{attr, _, nil}]}) when is_atom(attr) do
    {:attribute, attr}
  end

  defp get_var_or_attr(_), do: nil

  defp do_extract_struct_module({var, _, nil}, fields, _current_module, updated_var)
       when is_atom(var) and var != :__MODULE__ do
    # variable struct type is not supported
    {nil, get_field_names(fields), false, updated_var}
  end

  defp do_extract_struct_module({:@, _, [{attr, _, nil}]}, fields, _current_module, updated_var)
       when is_atom(attr) do
    {{:attribute, attr}, get_field_names(fields), false, updated_var}
  end

  defp do_extract_struct_module(module, fields, current_module, updated_var) do
    case extract_module(module, current_module) do
      {:ok, extracted_module, elixir_prefix} ->
        {{:atom, extracted_module}, get_field_names(fields), elixir_prefix, updated_var}

      _ ->
        {nil, get_field_names(fields), false, updated_var}
    end
  end

  defp extract_struct_module({:%{}, _, [{:|, _, [expr, fields]}]}, _) do
    {:map, get_field_names(fields), get_var_or_attr(expr)}
  end

  defp extract_struct_module({:%{}, _, fields}, _) do
    {:map, get_field_names(fields), nil}
  end

  defp extract_struct_module(
         {:%, _, [module, {:%{}, _, [{:|, _, [expr, fields]}]}]},
         current_module
       ) do
    do_extract_struct_module(module, fields, current_module, get_var_or_attr(expr))
  end

  defp extract_struct_module({:%, _, [module, {:%{}, _, fields}]}, current_module) do
    do_extract_struct_module(module, fields, current_module, nil)
  end

  defp extract_struct_module(_, _) do
    nil
  end

  defp find_struct("%" = grapheme, _rest, _line, _col, %{buffer: buffer, count_open: 1} = acc) do
    {"", %{acc | result: [grapheme | buffer]}}
  end

  defp find_struct(
         "{" = grapheme,
         rest,
         _line,
         _col,
         %{buffer: buffer, count_open: count_open} = acc
       ) do
    {rest, %{acc | buffer: [grapheme | buffer], count_open: count_open + 1}}
  end

  defp find_struct(
         "}" = grapheme,
         rest,
         _line,
         _col,
         %{buffer: buffer, count_open: count_open} = acc
       ) do
    {rest, %{acc | buffer: [grapheme | buffer], count_open: count_open - 1}}
  end

  defp find_struct(grapheme, rest, _line, _col, %{buffer: buffer} = acc) do
    {rest, %{acc | buffer: [grapheme | buffer]}}
  end

  defp find_subject(grapheme, rest, line, col, %{pos_found: false, line: line, col: col} = acc) do
    find_subject(grapheme, rest, line, col, %{acc | pos_found: true})
  end

  defp find_subject("." = grapheme, rest, _line, _col, %{pos_found: false} = acc) do
    {rest, %{acc | candidate: [grapheme | acc.candidate]}}
  end

  defp find_subject(".", _rest, line, col, %{pos_found: true} = acc) do
    {"", %{acc | pos: {line, col - 1}}}
  end

  defp find_subject(grapheme, rest, _line, _col, %{candidate: [_ | _]} = acc)
       when grapheme in ["!", "?"] do
    {rest, %{acc | candidate: [grapheme | acc.candidate]}}
  end

  defp find_subject(grapheme, rest, _line, _col, %{candidate: ["." | _]} = acc)
       when grapheme in @stop_graphemes do
    {rest, acc}
  end

  defp find_subject(grapheme, rest, _line, _col, %{pos_found: false} = acc)
       when grapheme in @stop_graphemes do
    {rest, %{acc | candidate: []}}
  end

  defp find_subject(grapheme, _rest, line, col, %{pos_found: true} = acc)
       when grapheme in @stop_graphemes do
    {"", %{acc | pos: {line, col - 1}}}
  end

  defp find_subject(grapheme, rest, _line, _col, acc) do
    {rest, %{acc | candidate: [grapheme | acc.candidate]}}
  end

  defp do_walk_text(text, func, line, col, acc) do
    case String.next_grapheme(text) do
      nil ->
        acc

      {grapheme, rest} ->
        {new_rest, new_acc} = func.(grapheme, rest, line, col, acc)

        {new_line, new_col} =
          if grapheme in @line_break do
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
          if grapheme in @line_break do
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

  @spec which_func(String.t(), nil | module) :: %{
          candidate: :none | {nil | module, atom},
          elixir_prefix: boolean,
          npar: non_neg_integer,
          pipe_before: boolean,
          unfinished_parm: boolean,
          cursor_at_option: true | false | :maybe,
          options_so_far: [atom],
          pos:
            nil | {{non_neg_integer, non_neg_integer}, {non_neg_integer, nil | non_neg_integer}}
        }
  def which_func(prefix, current_module \\ nil) do
    tokens = Tokenizer.tokenize(prefix)

    pattern = %{
      npar: [],
      count: 0,
      count2: 0,
      count3: 0,
      options_so_far: [],
      candidate: [],
      pos: nil,
      pipe_before: false
    }

    result = scan(tokens, pattern)
    %{candidate: candidate, npar: npar, pipe_before: pipe_before, pos: pos} = result

    {cursor_at_option, options_so_far} = check_cursor_at_option(tokens, result)
    {normalized_candidate, elixir_prefix} = normalize_candidate(candidate, current_module)

    unfinished_parm =
      case npar |> Enum.at(-1) do
        nil ->
          case {tokens, normalized_candidate} do
            {_, :none} -> false
            {[{:"(", _}, {:paren_identifier, _, _} | _], _} -> false
            _ -> true
          end

        token ->
          Enum.find_index(tokens, fn t -> t == token end) != 0
      end

    %{
      candidate: normalized_candidate,
      elixir_prefix: elixir_prefix,
      npar: normalize_npar(length(npar), pipe_before),
      unfinished_parm: unfinished_parm,
      pipe_before: pipe_before,
      cursor_at_option: cursor_at_option,
      options_so_far: options_so_far,
      pos: pos
    }
  end

  defp check_cursor_at_option(tokens, scan_result) do
    comma_before_token =
      case tokens do
        [{:",", _} = token | _] ->
          token

        [{:identifier, _, _}, {:",", _} = token | _] ->
          token

        _ ->
          nil
      end

    same_level = scan_result.count3 == 0
    first_arg? = scan_result.npar == []

    cond do
      !same_level ->
        {false, []}

      comma_before_token in scan_result.npar ->
        {:maybe, []}

      comma_before_token ->
        {true, Enum.reverse(scan_result.options_so_far)}

      first_arg? ->
        {:maybe, []}

      true ->
        {false, []}
    end
  end

  defp normalize_candidate(candidate, current_module) do
    case candidate do
      [] ->
        {:none, false}

      [func] ->
        {{nil, func}, false}

      [:__MODULE__, func] ->
        {{current_module, func}, false}

      [mod, func] ->
        {{mod, func}, false}

      list ->
        [func | mods] = Enum.reverse(list)

        module_parts =
          case mods |> Enum.reverse() do
            [:__MODULE__ | rest] ->
              [current_module | rest]

            rest ->
              rest
          end

        {{Module.concat(module_parts), func}, hd(module_parts) == Elixir}
    end
  end

  defp normalize_npar(npar, true), do: npar + 1
  defp normalize_npar(npar, _pipe_before), do: npar

  defp maybe_update_options_so_far(%{count: 0, count2: 0, count3: 0} = state, key) do
    %{state | options_so_far: [key | state.options_so_far]}
  end

  defp maybe_update_options_so_far(state, _key) do
    state
  end

  defp scan([{:kw_identifier, _, key} | tokens], %{npar: [_]} = state) do
    state = maybe_update_options_so_far(state, key)
    scan(tokens, %{state | npar: []})
  end

  defp scan([{:kw_identifier, _, key} | tokens], state) do
    state = maybe_update_options_so_far(state, key)
    scan(tokens, state)
  end

  defp scan([{:",", _} | _], %{count: 1} = state), do: state

  defp scan([{:",", _pos} = t | tokens], %{count: 0, count2: 0} = state) do
    scan(tokens, %{state | npar: [t | state.npar], candidate: []})
  end

  defp scan([{:"(", _} | tokens], %{count: 1, candidate: []} = state), do: scan(tokens, state)
  defp scan([{:"(", _} | _tokens], %{count: 1} = state), do: state

  defp scan([{:"(", _} | tokens], state) do
    scan(tokens, %{state | count: state.count + 1, candidate: []})
  end

  defp scan([{:")", _} | tokens], state) do
    scan(tokens, %{state | count: state.count - 1, candidate: []})
  end

  defp scan([{token, _} | tokens], %{count2: 0} = state) when token in [:"[", :"{"] do
    scan(tokens, %{state | npar: [], count2: 0, count3: state.count3 + 1})
  end

  defp scan([{token, _} | tokens], state) when token in [:"[", :"{"] do
    scan(tokens, %{state | count2: state.count2 + 1, count3: state.count3 + 1})
  end

  defp scan([{token, _} | tokens], state) when token in [:"]", :"}"] do
    scan(tokens, %{state | count2: state.count2 - 1, count3: state.count3 - 1})
  end

  defp scan([{:paren_identifier, pos, value} | tokens], %{count: 1} = state) do
    scan(tokens, %{state | candidate: [value | state.candidate], pos: update_pos(pos, state.pos)})
  end

  defp scan([{:aliases, pos, [value]} | tokens], %{count: 1} = state) do
    updated_pos = update_pos(pos, state.pos)

    scan(tokens, %{
      state
      | candidate: [Module.concat([value]) | state.candidate],
        pos: updated_pos
    })
  end

  defp scan([{:alias, pos, value} | tokens], %{count: 1} = state) do
    updated_pos = update_pos(pos, state.pos)

    scan(tokens, %{
      state
      | candidate: [Module.concat([value]) | state.candidate],
        pos: updated_pos
    })
  end

  defp scan([{:identifier, pos, :__MODULE__} | tokens], %{count: 1} = state) do
    updated_pos = update_pos(pos, state.pos)

    scan(tokens, %{
      state
      | candidate: [:__MODULE__ | state.candidate],
        pos: updated_pos
    })
  end

  defp scan([{:atom, pos, value} | tokens], %{count: 1} = state) do
    scan(tokens, %{state | candidate: [value | state.candidate], pos: update_pos(pos, state.pos)})
  end

  defp scan([{:fn, _} | tokens], %{count: 1} = state) do
    scan(tokens, %{state | npar: [], count: 0})
  end

  defp scan([{:., _} | tokens], state), do: scan(tokens, state)
  defp scan([{:arrow_op, _, :|>} | _], %{count: 1} = state), do: pipe_before(state)
  defp scan([_ | _], %{count: 1} = state), do: state
  defp scan([_token | tokens], state), do: scan(tokens, state)
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

  defp split_mod_quoted_fun_call(quoted, current_module, aliases) do
    case Macro.decompose_call(quoted) do
      {{:__aliases__, _, mod_parts}, fun, _args} ->
        case concat_module_parts(mod_parts, current_module, aliases) do
          {:ok, concated} -> {concated, fun}
          :error -> {nil, nil}
        end

      {:__aliases__, mod_parts} ->
        case concat_module_parts(mod_parts, current_module, aliases) do
          {:ok, concated} -> {concated, nil}
          :error -> {nil, nil}
        end

      {:__MODULE__, []} ->
        {current_module, nil}

      {mod, func, []} when is_atom(mod) and is_atom(func) ->
        {mod, func}

      {{:__MODULE__, _, nil}, func, []} when is_atom(func) ->
        {current_module, func}

      {func, []} when is_atom(func) ->
        {nil, func}

      _ ->
        {nil, nil}
    end
  end

  def concat_module_parts([{:__MODULE__, _, nil} | rest], current_module, aliases)
      when is_atom(current_module) and current_module != nil do
    case concat_module_parts(rest, current_module, aliases) do
      {:ok, module} ->
        {:ok, Module.concat(current_module, module)}

      :error ->
        :error
    end
  end

  def concat_module_parts([name | rest], _current_module, aliases) when is_atom(name) do
    case Keyword.fetch(aliases, Module.concat(Elixir, name)) do
      {:ok, name} when rest == [] -> {:ok, name}
      {:ok, name} -> {:ok, Module.concat([name | rest])}
      :error -> {:ok, Module.concat([name | rest])}
    end
  end

  def concat_module_parts([_ | _], _, _), do: :error

  def concat_module_parts([], _, _), do: :error

  def split_lines(src, opts \\ []) do
    String.split(src, ["\r\n", "\r", "\n"], opts)
  end
end
