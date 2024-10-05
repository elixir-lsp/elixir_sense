defmodule ElixirSense.Core.Compiler.Fn do
  alias ElixirSense.Core.Compiler, as: ElixirExpand
  alias ElixirSense.Core.Compiler.Clauses, as: ElixirClauses
  alias ElixirSense.Core.Compiler.Dispatch, as: ElixirDispatch
  alias ElixirSense.Core.Compiler.Utils, as: ElixirUtils
  alias ElixirSense.Core.State

  def expand(meta, clauses, s, e) when is_list(clauses) do
    transformer = fn
      {:->, _, [_left, _right]} = clause, sa ->
        # elixir raises defaults_in_args
        s_reset = State.new_vars_scope(sa)

        # no point in doing type inference here, we have no idea what the fn will be called with
        {e_clause, s_acc, _e_acc} =
          ElixirClauses.clause(&ElixirClauses.head/3, clause, s_reset, e)

        {e_clause, State.remove_vars_scope(s_acc, sa)}
    end

    {e_clauses, se} = Enum.map_reduce(clauses, s, transformer)

    {{:fn, meta, e_clauses}, se, e}
  end

  # Capture

  def capture(meta, {:/, _, [{{:., _, [_m, f]} = dot, require_meta, []}, a]}, s, e)
      when is_atom(f) and is_integer(a) do
    args = args_from_arity(meta, a)

    capture_require({dot, require_meta, args}, s, e, true)
  end

  def capture(meta, {:/, _, [{f, import_meta, c}, a]}, s, e)
      when is_atom(f) and is_integer(a) and is_atom(c) do
    args = args_from_arity(meta, a)
    capture_import({f, import_meta, args}, s, e, true)
  end

  def capture(_meta, {{:., _, [_, fun]}, _, args} = expr, s, e)
      when is_atom(fun) and is_list(args) do
    capture_require(expr, s, e, is_sequential_and_not_empty(args))
  end

  def capture(meta, {{:., _, [_]}, _, args} = expr, s, e) when is_list(args) do
    capture_expr(meta, expr, s, e, false)
  end

  def capture(meta, {:__block__, _, [expr]}, s, e) do
    capture(meta, expr, s, e)
  end

  def capture(meta, {:__block__, _, expr}, s, e) do
    # elixir raises block_expr_in_capture
    # try to recover from error
    expr =
      case expr do
        [] ->
          {:"&1", meta, e.module}

        list ->
          ElixirUtils.select_with_cursor(list) || hd(list)
      end

    capture(meta, expr, s, e)
  end

  def capture(_meta, {atom, _, args} = expr, s, e) when is_atom(atom) and is_list(args) do
    capture_import(expr, s, e, is_sequential_and_not_empty(args))
  end

  def capture(meta, {left, right}, s, e) do
    capture(meta, {:{}, meta, [left, right]}, s, e)
  end

  def capture(meta, list, s, e) when is_list(list) do
    capture_expr(meta, list, s, e, is_sequential_and_not_empty(list))
  end

  def capture(meta, integer, s, e) when is_integer(integer) do
    # elixir raises here capture_arg_outside_of_capture
    # emit fake capture
    capture(meta, [{:&, meta, [1]}], s, e)
  end

  def capture(meta, arg, s, e) do
    # elixir raises invalid_args_for_capture
    # we try to transform the capture to local fun capture
    case arg do
      {var, _, context} when is_atom(var) and is_atom(context) ->
        capture(meta, {:/, meta, [arg, 0]}, s, e)

      _ ->
        # try to wrap it in list
        capture(meta, [arg], s, e)
    end
  end

  defp capture_import({atom, import_meta, args} = expr, s, e, sequential) do
    res =
      if sequential do
        ElixirDispatch.import_function(import_meta, atom, length(args), s, e)
      else
        false
      end

    handle_capture(res, import_meta, import_meta, expr, s, e, sequential)
  end

  defp capture_require({{:., dot_meta, [left, right]}, require_meta, args}, s, e, sequential) do
    case escape(left, []) do
      {esc_left, []} ->
        {e_left, se, ee} = ElixirExpand.expand(esc_left, s, e)

        res =
          if sequential do
            case e_left do
              {name, _, context} when is_atom(name) and is_atom(context) ->
                {:remote, e_left, right, length(args)}

              _ when is_atom(e_left) ->
                ElixirDispatch.require_function(
                  require_meta,
                  e_left,
                  right,
                  length(args),
                  s,
                  ee
                )

              _ ->
                false
            end
          else
            false
          end

        dot = {{:., dot_meta, [e_left, right]}, require_meta, args}
        handle_capture(res, require_meta, dot_meta, dot, se, ee, sequential)

      {esc_left, escaped} ->
        dot = {{:., dot_meta, [esc_left, right]}, require_meta, args}
        capture_expr(require_meta, dot, s, e, escaped, sequential)
    end
  end

  defp handle_capture(false, meta, _dot_meta, expr, s, e, sequential) do
    capture_expr(meta, expr, s, e, sequential)
  end

  defp handle_capture(local_or_remote, meta, dot_meta, _expr, s, e, _sequential) do
    {local_or_remote, meta, dot_meta, s, e}
  end

  defp capture_expr(meta, expr, s, e, sequential) do
    capture_expr(meta, expr, s, e, [], sequential)
  end

  defp capture_expr(meta, expr, s, e, escaped, sequential) do
    case escape(expr, escaped) do
      {e_expr, []} when not sequential ->
        # elixir raises here invalid_args_for_capture
        # we emit fn without args
        fn_expr = {:fn, meta, [{:->, meta, [[], e_expr]}]}
        {:expand, fn_expr, s, e}

      {e_expr, e_dict} ->
        # elixir raises capture_arg_without_predecessor here
        # if argument vars are not consecutive
        e_vars = Enum.map(e_dict, &elem(&1, 1))
        fn_expr = {:fn, meta, [{:->, meta, [e_vars, e_expr]}]}
        {:expand, fn_expr, s, e}
    end
  end

  defp escape({:&, meta, [pos]}, dict) when is_integer(pos) and pos > 0 do
    # This might pollute user space but is unlikely because variables
    # named :"&1" are not valid syntax.
    var = {:"&#{pos}", meta, nil}
    {var, :orddict.store(pos, var, dict)}

    case :orddict.find(pos, dict) do
      {:ok, var} ->
        {var, dict}

      :error ->
        # elixir uses here elixir_module:next_counter(?key(E, module))
        # but we are not compiling and do not need to keep count in module scope
        # elixir 1.17 also renames the var to `capture`
        next = System.unique_integer()
        var = {:"&#{pos}", [{:counter, next} | meta], nil}
        {var, :orddict.store(pos, var, dict)}
    end
  end

  defp escape({:&, meta, [pos]}, dict) when is_integer(pos) do
    # elixir raises here invalid_arity_for_capture
    # we substitute arg number
    escape({:&, meta, [1]}, dict)
  end

  defp escape({:&, _meta, args}, dict) do
    # elixir raises here nested_capture
    # try to recover from error by dropping &
    escape(args, dict)
  end

  defp escape({left, meta, right}, dict0) do
    {t_left, dict1} = escape(left, dict0)
    {t_right, dict2} = escape(right, dict1)
    {{t_left, meta, t_right}, dict2}
  end

  defp escape({left, right}, dict0) do
    {t_left, dict1} = escape(left, dict0)
    {t_right, dict2} = escape(right, dict1)
    {{t_left, t_right}, dict2}
  end

  defp escape(list, dict) when is_list(list) do
    Enum.map_reduce(list, dict, fn x, acc -> escape(x, acc) end)
  end

  defp escape(other, dict) do
    {other, dict}
  end

  defp args_from_arity(_meta, 0), do: []

  defp args_from_arity(meta, a) when is_integer(a) and a >= 1 and a <= 255 do
    for x <- 1..a do
      {:&, meta, [x]}
    end
  end

  defp args_from_arity(_meta, _a) do
    # elixir raises invalid_arity_for_capture
    []
  end

  defp is_sequential_and_not_empty([]), do: false
  defp is_sequential_and_not_empty(list), do: is_sequential(list, 1)

  defp is_sequential([{:&, _, [int]} | t], int), do: is_sequential(t, int + 1)
  defp is_sequential([], _int), do: true
  defp is_sequential(_, _int), do: false
end
