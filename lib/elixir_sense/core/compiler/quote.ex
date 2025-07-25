defmodule ElixirSense.Core.Compiler.Quote do
  alias ElixirSense.Core.Compiler.Dispatch
  alias ElixirSense.Core.Normalized.Macro.Env, as: NormalizedMacroEnv

  defstruct line: false,
            file: nil,
            context: nil,
            op: :none,
            aliases_hygiene: nil,
            imports_hygiene: nil,
            unquote: true,
            generated: false,
            shallow_validate: false

  def fun_to_quoted(function) do
    {:module, module} = :erlang.fun_info(function, :module)
    {:name, name} = :erlang.fun_info(function, :name)
    {:arity, arity} = :erlang.fun_info(function, :arity)

    {:&, [], [{:/, [], [{{:., [], [module, name]}, [{:no_parens, true}], []}, arity]}]}
  end

  def has_unquotes(ast), do: has_unquotes(ast, 0)

  def has_unquotes({:quote, _, [child]}, quote_level) do
    has_unquotes(child, quote_level + 1)
  end

  def has_unquotes({:quote, _, [quote_opts, child]}, quote_level) do
    case disables_unquote(quote_opts) do
      true -> false
      _ -> has_unquotes(child, quote_level + 1)
    end
  end

  def has_unquotes({unquote, _, [child]}, quote_level)
      when unquote in [:unquote, :unquote_splicing] do
    case quote_level do
      0 -> true
      _ -> has_unquotes(child, quote_level - 1)
    end
  end

  def has_unquotes({{:., _, [_, :unquote]}, _, [_]}, _), do: true
  def has_unquotes({var, _, ctx}, _) when is_atom(var) and is_atom(ctx), do: false

  def has_unquotes({name, _, args}, quote_level) when is_list(args) do
    has_unquotes(name) or Enum.any?(args, fn child -> has_unquotes(child, quote_level) end)
  end

  def has_unquotes({left, right}, quote_level) do
    has_unquotes(left, quote_level) or has_unquotes(right, quote_level)
  end

  def has_unquotes(list, quote_level) when is_list(list) do
    Enum.any?(list, fn child -> has_unquotes(child, quote_level) end)
  end

  def has_unquotes(_other, _), do: false

  defp disables_unquote([{:unquote, false} | _]), do: true
  defp disables_unquote([{:bind_quoted, _} | _]), do: true
  defp disables_unquote([_h | t]), do: disables_unquote(t)
  defp disables_unquote(_), do: false

  def build(meta, line, file, context, unquote, generated, e) do
    acc0 = []

    {v_line, acc1} = validate_compile(meta, :line, line, acc0)
    {v_file, acc2} = validate_compile(meta, :file, file, acc1)
    {v_context, acc3} = validate_compile(meta, :context, context, acc2)

    unquote = validate_runtime(:unquote, unquote)
    generated = validate_runtime(:generated, generated)

    q = %__MODULE__{
      op: :add_context,
      aliases_hygiene: e,
      imports_hygiene: e,
      line: v_line,
      file: v_file,
      unquote: unquote,
      context: v_context,
      generated: generated,
      shallow_validate: true
    }

    {q, v_context, acc3}
  end

  defp validate_compile(_meta, :line, value, acc) when is_boolean(value) do
    {value, acc}
  end

  defp validate_compile(_meta, :file, nil, acc) do
    {nil, acc}
  end

  defp validate_compile(meta, key, value, acc) do
    case is_valid(key, value) do
      true ->
        {value, acc}

      false ->
        var = {key, meta, __MODULE__}
        call = {{:., meta, [__MODULE__, :validate_runtime]}, meta, [key, value]}
        {var, [{:=, meta, [var, call]} | acc]}
    end
  end

  defp validate_runtime(key, value) do
    case is_valid(key, value) do
      true ->
        value

      false ->
        # elixir raises here invalid runtime value for option
        default(key)
    end
  end

  defp is_valid(:line, line), do: is_integer(line)
  defp is_valid(:file, file), do: is_binary(file)
  defp is_valid(:context, context), do: is_atom(context) and context != nil
  defp is_valid(:generated, generated), do: is_boolean(generated)
  defp is_valid(:unquote, unquote), do: is_boolean(unquote)
  defp default(:unquote), do: true
  defp default(:generated), do: false

  def escape(expr, op, unquote, state) do
    do_quote(
      expr,
      %__MODULE__{
        line: true,
        file: nil,
        op: op,
        unquote: unquote
      },
      state
    )
  end

  def quote({:unquote_splicing, _, [_]} = expr, %__MODULE__{unquote: true} = q, state) do
    # elixir raises here unquote_splicing only works inside arguments and block contexts
    # try to recover from error by wrapping it in block
    __MODULE__.quote({:__block__, [], [expr]}, q, state)
  end

  def quote(expr, q, state) do
    do_quote(expr, q, state)
  end

  # quote/unquote

  defp do_quote({:quote, meta, [arg]}, %__MODULE__{} = q, state) when is_list(meta) do
    {t_arg, state} = do_quote(arg, %{q | unquote: false}, state)

    new_meta =
      case q do
        %__MODULE__{op: :add_context, context: context} ->
          keystore(:context, meta, context)

        _ ->
          meta
      end

    {{:{}, [], [:quote, meta(new_meta, q), [t_arg]]}, state}
  end

  defp do_quote({:quote, meta, [opts, arg]}, %__MODULE__{} = q, state) when is_list(meta) do
    {t_opts, state} = do_quote(opts, q, state)
    {t_arg, state} = do_quote(arg, %{q | unquote: false}, state)

    new_meta =
      case q do
        %__MODULE__{op: :add_context, context: context} ->
          keystore(:context, meta, context)

        _ ->
          meta
      end

    {{:{}, [], [:quote, meta(new_meta, q), [t_opts, t_arg]]}, state}
  end

  defp do_quote(
         {:unquote, meta, [expr]},
         %__MODULE__{unquote: true, shallow_validate: validate},
         state
       )
       when is_list(meta) do
    if validate do
      {{{:., meta, [:elixir_quote, :shallow_validate_ast]}, meta, [expr]}, state}
    else
      {expr, state}
    end
  end

  # Aliases

  defp do_quote(
         {:__aliases__, meta, [h | t] = list},
         %__MODULE__{aliases_hygiene: e = %{}} = q,
         state
       )
       when is_atom(h) and h != Elixir and is_list(meta) do
    annotation =
      case NormalizedMacroEnv.expand_alias(e, meta, list, trace: true) do
        {:alias, atom} ->
          atom

        :error ->
          false
      end

    alias_meta = keystore(:alias, Keyword.delete(meta, :counter), annotation)
    do_quote_tuple(:__aliases__, alias_meta, [h | t], q, state)
  end

  # Vars

  defp do_quote({name, meta, nil}, %__MODULE__{op: :add_context} = q, state)
       when is_atom(name) and is_list(meta) do
    import_meta =
      case q.imports_hygiene do
        nil -> meta
        e -> import_meta(meta, name, 0, q, e)
      end

    {{:{}, [], [name, meta(import_meta, q), q.context]}, state}
  end

  # cursor

  defp do_quote(
         {:__cursor__, meta, args},
         %__MODULE__{unquote: _},
         state
       )
       when is_list(args) do
    # emit cursor as is regardless of unquote
    # TODO capture env?
    {{:__cursor__, meta, args}, state}
  end

  # Unquote

  defp do_quote(
         {{{:., meta, [left, :unquote]}, _, [expr]}, _, args},
         %__MODULE__{unquote: true} = q,
         state
       )
       when is_list(meta) do
    do_quote_call(left, meta, expr, args, q, state)
  end

  defp do_quote({{:., meta, [left, :unquote]}, _, [expr]}, %__MODULE__{unquote: true} = q, state)
       when is_list(meta) do
    do_quote_call(left, meta, expr, nil, q, state)
  end

  # Imports

  defp do_quote(
         {:&, meta, [{:/, _, [{f, _, c}, a]}] = args},
         %__MODULE__{imports_hygiene: e = %{}} = q,
         state
       )
       when is_atom(f) and is_integer(a) and is_atom(c) and is_list(meta) do
    {new_meta, state} =
      case Dispatch.find_import(meta, f, a, e) do
        false ->
          {meta, state}

        receiver ->
          # import capture is precise
          # elixir emits function/macro_imported in Dispatch.find_import
          meta = keystore(:context, keystore(:imports, meta, [{a, receiver}]), q.context)
          {meta, state}
      end

    do_quote_tuple(:&, new_meta, args, q, state)
  end

  defp do_quote({name, meta, args_or_context}, %__MODULE__{imports_hygiene: e = %{}} = q, state)
       when is_atom(name) and is_list(meta) and
              (is_list(args_or_context) or is_atom(args_or_context)) do
    arity =
      case args_or_context do
        args when is_list(args) -> length(args)
        context when is_atom(context) -> 0
      end

    import_meta = import_meta(meta, name, arity, q, e)
    annotated = annotate({name, import_meta, args_or_context}, q.context)

    do_quote_tuple(annotated, q, state)
  end

  # Two-element tuples

  defp do_quote({left, right}, %__MODULE__{unquote: true} = q, state)
       when is_tuple(left) and elem(left, 0) == :unquote_splicing and
              is_tuple(right) and elem(right, 0) == :unquote_splicing do
    do_quote({:{}, [], [left, right]}, q, state)
  end

  defp do_quote({left, right}, q, state) do
    {t_left, state} = do_quote(left, q, state)
    {t_right, state} = do_quote(right, q, state)
    {{t_left, t_right}, state}
  end

  # Everything else

  defp do_quote(other, q = %{op: op}, state) when op != :add_context do
    do_escape(other, q, state)
  end

  defp do_quote({_, _, _} = tuple, q, state) do
    annotated = annotate(tuple, q.context)
    do_quote_tuple(annotated, q, state)
  end

  defp do_quote([], _, state), do: {[], state}

  defp do_quote([h | t], %__MODULE__{unquote: false} = q, state) do
    {head_quoted, state} = do_quote(h, q, state)
    do_quote_simple_list(t, head_quoted, q, state)
  end

  defp do_quote([h | t], q, state) do
    do_quote_tail(:lists.reverse(t, [h]), q, state)
  end

  defp do_quote(other, _, state), do: {other, state}

  defp import_meta(meta, name, arity, q, e) do
    case Keyword.get(meta, :imports, false) == false &&
           Dispatch.find_imports(meta, name, e) do
      [_ | _] = imports ->
        trace_import_quoted(imports, meta, name, e)
        keystore(:imports, keystore(:context, meta, q.context), imports)

      _ ->
        case arity == 1 && Keyword.fetch(meta, :ambiguous_op) do
          {:ok, nil} ->
            keystore(:ambiguous_op, meta, q.context)

          _ ->
            meta
        end
    end
  end

  defp do_quote_call(left, meta, expr, args, q, state) do
    all = [left, {:unquote, meta, [expr]}, args, q.context]

    {tall_reverse, state} =
      Enum.reduce(all, {[], state}, fn x, {acc, state} ->
        {tx, state} = do_quote(x, q, state)
        {[tx | acc], state}
      end)

    # elixir does not emit remote_function in quoted as AST is meaningless
    {{{:., meta, [:elixir_quote, :dot]}, meta, [meta(meta, q) | Enum.reverse(tall_reverse)]},
     state}
  end

  defp do_quote_tuple({left, meta, right}, q, state) do
    do_quote_tuple(left, meta, right, q, state)
  end

  defp do_quote_tuple(left, meta, right, q, state) do
    {t_left, state} = do_quote(left, q, state)
    {t_right, state} = do_quote(right, q, state)
    {{:{}, [], [t_left, meta(meta, q), t_right]}, state}
  end

  defp do_quote_simple_list([], prev, _, state), do: {[prev], state}

  defp do_quote_simple_list([h | t], prev, q, state) do
    {head_quoted, state} = do_quote(h, q, state)
    {tail_quoted, state} = do_quote_simple_list(t, head_quoted, q, state)
    {[prev | tail_quoted], state}
  end

  defp do_quote_simple_list(other, prev, q, state) do
    {quoted_other, state} = do_quote(other, q, state)
    {[{:|, [], [prev, quoted_other]}], state}
  end

  defp do_quote_tail(
         [{:|, meta, [{:unquote_splicing, _, [left]}, right]} | t],
         %__MODULE__{unquote: true} = q,
         state
       ) do
    {tt, state} = do_quote_splice(t, q, [], [], state)
    {tr, state} = do_quote(right, q, state)
    {do_runtime_list(meta, :tail_list, [left, tr, tt]), state}
  end

  defp do_quote_tail(list, q, state) do
    do_quote_splice(list, q, [], [], state)
  end

  defp do_quote_splice(
         [{:unquote_splicing, meta, [expr]} | t],
         %__MODULE__{unquote: true} = q,
         buffer,
         acc,
         state
       ) do
    runtime = do_runtime_list(meta, :list, [expr, do_list_concat(buffer, acc)])
    do_quote_splice(t, q, [], runtime, state)
  end

  defp do_quote_splice([h | t], q, buffer, acc, state) do
    {th, state} = do_quote(h, q, state)
    do_quote_splice(t, q, [th | buffer], acc, state)
  end

  defp do_quote_splice([], _q, buffer, acc, state) do
    {do_list_concat(buffer, acc), state}
  end

  defp do_list_concat(left, []), do: left
  defp do_list_concat([], right), do: right

  defp do_list_concat(left, right) do
    {{:., [], [:erlang, :++]}, [], [left, right]}
  end

  defp do_runtime_list(meta, fun, args) do
    {{:., meta, [:elixir_quote, fun]}, meta, args}
  end

  defp meta(meta, q) do
    generated(keep(Keyword.delete(meta, :column), q), q)
  end

  defp generated(meta, %__MODULE__{generated: true}), do: [{:generated, true} | meta]
  defp generated(meta, %__MODULE__{generated: false}), do: meta

  defp keep(meta, %__MODULE__{file: nil, line: line}) do
    line(meta, line)
  end

  defp keep(meta, %__MODULE__{file: file, line: true}) do
    case Keyword.pop(meta, :line) do
      {nil, _} ->
        [{:keep, {file, 0}} | meta]

      {line, meta_no_line} ->
        [{:keep, {file, line}} | meta_no_line]
    end
  end

  defp keep(meta, %__MODULE__{file: file, line: false}) do
    [{:keep, {file, 0}} | Keyword.delete(meta, :line)]
  end

  defp keep(meta, %__MODULE__{file: file, line: line}) do
    [{:keep, {file, line}} | Keyword.delete(meta, :line)]
  end

  defp line(meta, true), do: meta

  defp line(meta, false) do
    Keyword.delete(meta, :line)
  end

  defp line(meta, line) do
    keystore(:line, meta, line)
  end

  defguardp defs(kind) when kind in [:def, :defp, :defmacro, :defmacrop, :@]
  defguardp lexical(kind) when kind in [:import, :alias, :require]

  defp annotate({def, meta, [h | t]}, context) when defs(def) do
    {def, meta, [annotate_def(h, context) | t]}
  end

  defp annotate({{:., _, [_, def]} = target, meta, [h | t]}, context) when defs(def) do
    {target, meta, [annotate_def(h, context) | t]}
  end

  defp annotate({lexical, meta, [_ | _] = args}, context) when lexical(lexical) do
    new_meta = keystore(:context, Keyword.delete(meta, :counter), context)
    {lexical, new_meta, args}
  end

  defp annotate(tree, _context), do: tree

  defp annotate_def({:when, meta, [left, right]}, context) do
    {:when, meta, [annotate_def(left, context), right]}
  end

  defp annotate_def({fun, meta, args}, context) do
    {fun, keystore(:context, meta, context), args}
  end

  defp annotate_def(other, _context), do: other

  defp do_escape({left, meta, right}, q = %{op: :prune_metadata}, state) when is_list(meta) do
    tm = for {k, v} <- meta, k == :no_parens or k == :line or k == :delimiter, do: {k, v}
    {tl, state} = do_quote(left, q, state)
    {tr, state} = do_quote(right, q, state)
    {{:{}, [], [tl, tm, tr]}, state}
  end

  defp do_escape(tuple, q, state) when is_tuple(tuple) do
    {tt, state} = do_quote(Tuple.to_list(tuple), q, state)
    {{:{}, [], tt}, state}
  end

  defp do_escape(bitstring, _, state) when is_bitstring(bitstring) do
    {case Bitwise.band(bit_size(bitstring), 7) do
       0 ->
         bitstring

       size ->
         <<bits::size(size), bytes::binary>> = bitstring

         {:<<>>, [],
          [{:"::", [], [bits, {size, [], [size]}]}, {:"::", [], [bytes, {:binary, [], nil}]}]}
     end, state}
  end

  defp do_escape(map, q, state) when is_map(map) do
    {tt, state} = do_quote(Enum.sort(Map.to_list(map)), q, state)
    {{:%{}, [], tt}, state}
  end

  defp do_escape([], _, state), do: {[], state}

  defp do_escape([h | t], %__MODULE__{unquote: false} = q, state) do
    {ht, state} = do_quote(h, q, state)
    do_quote_simple_list(t, ht, q, state)
  end

  defp do_escape([h | t], q, state) do
    # The improper case is inefficient, but improper lists are rare.
    try do
      l = Enum.reverse(t, [h])
      do_quote_tail(l, q, state)
    catch
      _ ->
        {l, r} = reverse_improper(t, [h])
        {tl, state} = do_quote_splice(l, q, [], [], state)
        {tr, state} = do_quote(r, q, state)
        {update_last(tl, fn x -> {:|, [], [x, tr]} end), state}
    end
  end

  defp do_escape(other, _, state) when is_number(other) or is_atom(other) or is_pid(other),
    do: {other, state}

  defp do_escape(fun, _, state) when is_function(fun) do
    case {Function.info(fun, :env), Function.info(fun, :type)} do
      {{:env, []}, {:type, :external}} ->
        {fun_to_quoted(fun), state}

      _ ->
        # elixir raises here ArgumentError
        {nil, state}
    end
  end

  defp do_escape(_other, _, state) do
    # elixir raises here ArgumentError
    {nil, state}
  end

  defp reverse_improper([h | t], acc), do: reverse_improper(t, [h | acc])
  defp reverse_improper([], acc), do: acc
  defp reverse_improper(t, acc), do: {acc, t}
  defp update_last([], _), do: []
  defp update_last([h], f), do: [f.(h)]
  defp update_last([h | t], f), do: [h | update_last(t, f)]

  defp keystore(_key, meta, value) when value == nil do
    meta
  end

  defp keystore(key, meta, value) do
    :lists.keystore(key, 1, meta, {key, value})
  end

  defp trace_import_quoted([{arity, mod} | imports], meta, name, e) do
    {rest, arities} = collect_trace_import_quoted(imports, mod, [], [arity])
    :elixir_env.trace({:imported_quoted, meta, mod, name, arities}, e)
    trace_import_quoted(rest, meta, name, e)
  end

  defp trace_import_quoted([], _meta, _name, _e) do
    :ok
  end

  defp collect_trace_import_quoted([{arity, mod} | imports], mod, acc, arities) do
    collect_trace_import_quoted(imports, mod, acc, [arity | arities])
  end

  defp collect_trace_import_quoted([import | imports], mod, acc, arities) do
    collect_trace_import_quoted(imports, mod, [import | acc], arities)
  end

  defp collect_trace_import_quoted([], _mod, acc, arities) do
    {Enum.reverse(acc), Enum.reverse(arities)}
  end
end
