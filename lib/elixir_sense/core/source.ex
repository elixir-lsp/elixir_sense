defmodule ElixirSense.Core.Source do
  @moduledoc """
  Source parsing
  """

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode
  alias ElixirSense.Core.Normalized.Tokenizer

  @line_break ["\n", "\r\n", "\r"]
  @empty_graphemes [" ", "\t"] ++ @line_break

  @spec split_module_and_hint(String.t(), module | nil, [{module, module}]) ::
          {nil | module | {:attribute, atom}, String.t()}
  def split_module_and_hint(hint, current_module \\ nil, aliases \\ []) do
    if String.ends_with?(hint, ".") do
      {mod, _} =
        hint
        |> String.slice(0..-2//1)
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
      iex> ElixirSense.Core.Source.split_module_and_func("@attr", CurrentMod)
      {{:attribute, :attr}, nil}
      iex> ElixirSense.Core.Source.split_module_and_func("@attr.my_func", CurrentMod)
      {{:attribute, :attr}, :my_func}
      iex> ElixirSense.Core.Source.split_module_and_func("__MODULE__.Sub", CurrentMod)
      {CurrentMod.Sub, nil}

  """
  @spec split_module_and_func(String.t(), module | nil, [{module, module}]) ::
          {module | {:attribute, atom} | nil, atom | nil}
  def split_module_and_func(call, current_module \\ nil, aliases \\ [])
  def split_module_and_func("", _current_module, _aliases), do: {nil, nil}

  def split_module_and_func(call, current_module, aliases) do
    case Code.string_to_quoted(call, emit_warnings: false) do
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

    length = String.length(line)

    line =
      if length < col do
        line_padding = for _ <- 1..(col - length)//1, into: "", do: " "
        line <> line_padding
      else
        line
      end

    line_str = line |> String.slice(0, col - 1)

    case Regex.run(~r/[\p{L}\p{N}\.\_\!\?\:\@\&\^\~\+\-\<\>\=\*\/\|\\]+$/u, line_str) do
      nil -> ""
      [prefix] when is_binary(prefix) -> prefix
    end
  end

  @spec prefix_suffix(String.t(), pos_integer, pos_integer) :: {String.t(), String.t()}
  def prefix_suffix(code, line, col) do
    line = code |> split_lines() |> Enum.at(line - 1, "")

    line =
      if String.length(line) < col do
        line_padding = for _ <- 1..(col - String.length(line)), into: "", do: " "
        line <> line_padding
      else
        line
      end

    # Extract the prefix
    line_str = line |> String.slice(0, col - 1)

    prefix =
      case Regex.run(~r/[\p{L}\p{N}\.\_\!\?\:\@\&\^\~\+\-\<\>\=\*\/\|\\]+$/u, line_str) do
        nil -> ""
        [prefix] when is_binary(prefix) -> prefix
      end

    # Extract the suffix
    suffix =
      line
      |> String.slice((col - 1)..-1//1)
      |> case do
        str ->
          case Regex.run(~r/^[\p{L}\p{N}\.\_\!\?\:\@\&\^\~\+\-\<\>\=\*\/\|\\]+/u, str) do
            nil -> ""
            [suffix] when is_binary(suffix) -> suffix
          end
      end

    {prefix, suffix}
  end

  @doc """
  Splits the given source `src` into a list of lines using all common newline forms.
  """
  def split_lines(src, opts \\ []) do
    String.split(src, ["\r\n", "\r", "\n"], opts)
  end

  @doc """
  Splits `src` at the given `line` and `col`.

  If the specified column is past the end of the line,
  the text before the split is padded with spaces.
  """
  @spec split_at(String.t(), pos_integer, pos_integer) :: {String.t(), String.t()}
  def split_at(src, line, col) do
    # Ensure minimum line and column are 1.
    line = max(line, 1)
    col = max(col, 1)
    lines = split_lines(src, trim: false)
    {prefix, target, suffix} = extract_line(lines, line)

    target_len = String.length(target)

    {before_target, after_target} =
      if col - 1 <= target_len do
        {
          String.slice(target, 0, col - 1),
          String.slice(target, col - 1, target_len - (col - 1))
        }
      else
        pad = String.duplicate(" ", col - 1 - target_len)
        {target <> pad, ""}
      end

    before_part =
      case prefix do
        [] -> before_target
        _ -> Enum.join(prefix, "\n") <> "\n" <> before_target
      end

    after_part =
      if suffix == [] do
        after_target
      else
        after_target <> "\n" <> Enum.join(suffix, "\n")
      end

    {before_part, after_part}
  end

  # If the requested line is beyond the number of lines,
  # we use the last line as the target.
  defp extract_line(lines, line) do
    if line > length(lines) do
      {Enum.slice(lines, 0, length(lines) - 1), List.last(lines) || "", []}
    else
      {prefix, [target | suffix]} = Enum.split(lines, line - 1)
      {prefix, target, suffix}
    end
  end

  @doc """
  Splits `src` into segments at multiple positions.

  The positions should be given as a list of `{line, col}` tuples.
  Splitting is performed from the end of the source backwards,
  so the final result is a list of segments.
  """
  @spec split_at(String.t(), list({pos_integer, pos_integer})) :: list(String.t())
  def split_at(src, positions) when is_list(positions) do
    do_split_at(src, Enum.reverse(positions), [])
  end

  defp do_split_at(src, [], acc), do: [src | acc]

  defp do_split_at(src, [{line, col} | rest], acc) do
    {before, after_cursor} = split_at(src, line, col)
    do_split_at(before, rest, [after_cursor | acc])
  end

  @spec text_before(String.t(), pos_integer, pos_integer) :: String.t()
  def text_before(code, line, col) do
    {text, _rest} = split_at(code, line, col)
    text
  end

  @spec text_after(String.t(), pos_integer, pos_integer) :: String.t()
  def text_after(code, line, col) do
    {_, rest} = split_at(code, line, col)
    rest
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

  @type var_or_attr_t :: {:variable, atom, non_neg_integer | :any} | {:attribute, atom} | nil

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
           |> Code.string_to_quoted(emit_warnings: false) do
      extract_struct_module(ast, current_module)
    else
      _ -> nil
    end
  end

  @spec get_v12_module_prefix(String.t(), module | nil) :: nil | String.t()
  def get_v12_module_prefix(text_before, current_module) do
    with %{"module" => module_str} <-
           Regex.named_captures(
             ~r/(alias|require|import|use)\s+(?<module>[^\s^\{^\}]+?)\.\{[^\}]*?$/u,
             text_before
           ),
         {:ok, ast} <- Code.string_to_quoted(module_str, emit_warnings: false),
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

    try do
      {:ok, Module.concat(module_list), hd(module_list) == Elixir}
    rescue
      _ -> :error
    end
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
    if Keyword.keyword?(fields) do
      Keyword.keys(fields) |> Enum.slice(0..-2//1)
    else
      []
    end
  end

  defp get_var_or_attr({var, meta, context})
       when is_atom(var) and is_atom(context) and
              var not in [
                :__MODULE__,
                :__DIR__,
                :__ENV__,
                :__CALLER__,
                :__STACKTRACE__,
                :_
              ] do
    {:variable, var, Keyword.get(meta, :version, :any)}
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

  # since elixir 1.15 previous lines are kept in blocks
  # https://github.com/elixir-lang/elixir/commit/faf81cd92c7d6668d2e8115744cc8d06f9bfecba
  # skip as __block__ is not a call we are interested in
  @excluded_funs [:__block__]

  @spec which_func(String.t(), nil | %Binding{}) ::
          nil
          | %{
              candidate: {nil | module, atom},
              elixir_prefix: boolean,
              params: any,
              npar: non_neg_integer,
              cursor_at_option: true | false | :maybe,
              option: atom() | nil,
              options_so_far: [atom],
              pos: {{non_neg_integer, non_neg_integer}, {non_neg_integer, nil | non_neg_integer}}
            }
  def which_func(prefix, binding_env \\ nil) do
    binding_env = binding_env || %Binding{}

    # TODO refactor to use Macro.path on elixir 1.14
    with {:ok, ast} <- NormalizedCode.Fragment.container_cursor_to_quoted(prefix, columns: true),
         {_, {:ok, call_info}} <- Macro.prewalk(ast, nil, &find_call_pre/2),
         {{m, elixir_prefix}, f} when f not in @excluded_funs <-
           get_mod_fun(call_info.call, binding_env) do
      %{
        candidate: {m, f},
        elixir_prefix: elixir_prefix,
        params: call_info.params,
        npar: call_info.npar,
        pos: {{call_info.meta[:line], call_info.meta[:column]}, {call_info.meta[:line], nil}},
        cursor_at_option: call_info.cursor_at_option,
        options_so_far: call_info.options,
        option: call_info.option
      }
    else
      _ -> nil
    end
  end

  def find_call_pre(ast, {:ok, call_info}),
    do: {ast, {:ok, call_info}}

  # transform `a |> b(c)` calls into `b(a, c)`
  def find_call_pre({:|>, _, [params_1, {call, meta, params_rest}]}, state) do
    params = [params_1 | params_rest || []]
    find_call_pre({call, meta, params}, state)
  end

  def find_call_pre({{:., meta, call}, _, params} = ast, _state) when is_list(params) do
    {ast, find_cursor_in_params(params, call, meta)}
  end

  def find_call_pre({atom, meta, params} = ast, _state)
      when is_atom(atom) and is_list(params) and atom not in [:{}, :%{}] do
    {ast, find_cursor_in_params(params, atom, meta)}
  end

  def find_call_pre(ast, state), do: {ast, state}

  defp find_cursor_in_params(params, call, meta) do
    case Enum.reverse(params) do
      [{:__cursor__, _, list} | rest] when is_list(list) ->
        {:ok,
         %{
           call: call,
           params: Enum.reverse(rest),
           npar: length(rest),
           meta: meta,
           options: [],
           cursor_at_option: :maybe,
           option: nil
         }}

      [keyword_list | rest] when is_list(keyword_list) ->
        case Enum.reverse(keyword_list) do
          [{:__cursor__, _, list} | kl_rest] when is_list(list) ->
            {:ok,
             %{
               call: call,
               params: Enum.reverse(rest),
               npar: length(rest),
               meta: meta,
               options: take_options(kl_rest),
               cursor_at_option: true,
               option: nil
             }}

          [{atom, {:__cursor__, _, list}} | kl_rest] when is_atom(atom) and is_list(list) ->
            {:ok,
             %{
               call: call,
               params: Enum.reverse(rest),
               npar: length(rest),
               meta: meta,
               options: take_options(kl_rest),
               cursor_at_option: false,
               option: atom
             }}

          _ ->
            nil
        end

      _ ->
        nil
    end
  end

  defp take_options(kl_rest) do
    Enum.reduce_while(kl_rest, [], fn
      {atom, _value}, acc when is_atom(atom) ->
        {:cont, [atom | acc]}

      _, acc ->
        {:halt, acc}
    end)
  end

  def get_mod_fun(atom, _binding_env) when is_atom(atom), do: {{nil, false}, atom}

  def get_mod_fun([{:__aliases__, _, list}, fun], binding_env) do
    mod = get_mod(list, binding_env)

    if mod do
      {mod, fun}
    end
  end

  def get_mod_fun([{:__MODULE__, _, nil}, fun], binding_env) do
    if binding_env.module != nil do
      {{binding_env.module, false}, fun}
    end
  end

  def get_mod_fun([{:@, _, [{name, _, nil}]}, fun], binding_env) when is_atom(name) do
    case Binding.expand(binding_env, {:attribute, name}) do
      {:atom, atom} ->
        {{atom, false}, fun}

      _ ->
        nil
    end
  end

  def get_mod_fun([{name, meta, context}, fun], binding_env)
      when is_atom(name) and is_atom(context) and
             name not in [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__, :_] do
    case Binding.expand(binding_env, {:variable, name, Keyword.get(meta, :version, :any)}) do
      {:atom, atom} ->
        {{atom, false}, fun}

      _ ->
        nil
    end
  end

  def get_mod_fun([atom, fun], _binding_env) when is_atom(atom), do: {{atom, false}, fun}
  def get_mod_fun(_, _binding_env), do: nil

  def get_mod([{:__aliases__, _, list} | _rest], binding_env) do
    get_mod(list, binding_env)
  end

  def get_mod([{:__MODULE__, _, nil} | rest], binding_env) do
    if binding_env.module != nil do
      mod =
        binding_env.module
        |> Module.split()
        |> Kernel.++(rest)
        |> Module.concat()

      {mod, false}
    end
  end

  def get_mod([{:@, _, [{name, _, nil}]} | rest], binding_env) when is_atom(name) do
    case Binding.expand(binding_env, {:attribute, name}) do
      {:atom, atom} ->
        if ElixirSense.Core.Introspection.elixir_module?(atom) do
          mod =
            atom
            |> Module.split()
            |> Kernel.++(rest)
            |> Module.concat()

          {mod, false}
        else
          nil
        end

      _ ->
        nil
    end
  end

  def get_mod([head | _rest] = list, _binding_env) when is_atom(head) do
    {Module.concat(list), head == Elixir}
  end

  def get_mod(_list, _binding_env), do: nil

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

      {:@, [{attribute, _, nil}]} when is_atom(attribute) ->
        {{:attribute, attribute}, nil}

      {{:@, _, [{attribute, _, nil}]}, func, []} when is_atom(func) and is_atom(attribute) ->
        {{:attribute, attribute}, func}

      {{:__MODULE__, _, nil}, func, []} when is_atom(func) ->
        {current_module, func}

      {func, []} when is_atom(func) ->
        {nil, func}

      _ ->
        {nil, nil}
    end
  end

  # TODO remove
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

  def bitstring_options(prefix) do
    tokens = Tokenizer.tokenize(prefix)

    case scan_bitstring(tokens, nil) do
      nil ->
        nil

      {line, column, _} ->
        prefix
        |> split_lines
        |> Enum.at(line - 1, "")
        |> String.slice((column + 1)..-1//1)
    end
  end

  defp scan_bitstring([], acc), do: acc

  defp scan_bitstring([{:type_op, candidate, :"::"}, {:identifier, _, _} | rest], _acc) do
    scan_bitstring(rest, candidate)
  end

  defp scan_bitstring([{:"<<", _} | _rest], acc) when not is_nil(acc) do
    acc
  end

  defp scan_bitstring([{:",", _} | _rest], acc) when not is_nil(acc) do
    acc
  end

  defp scan_bitstring([{:">>", _} | _rest], _acc) do
    nil
  end

  defp scan_bitstring([_token | rest], acc) do
    scan_bitstring(rest, acc)
  end
end
