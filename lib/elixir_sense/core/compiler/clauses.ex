defmodule ElixirSense.Core.Compiler.Clauses do
  alias ElixirSense.Core.Compiler
  alias ElixirSense.Core.Compiler.Utils
  alias ElixirSense.Core.Compiler.State
  alias ElixirSense.Core.TypeInference

  def match(fun, expr, after_s, _before_s, %{context: :match} = e) do
    fun.(expr, after_s, e)
  end

  def match(fun, expr, after_s, before_s, e) do
    %{vars: current, unused: unused} = after_s
    %{vars: {read, _write}, prematch: prematch} = before_s

    call_s = %{
      before_s
      | prematch: {read, unused, :none},
        unused: unused,
        vars: current,
        calls: after_s.calls,
        lines_to_env: after_s.lines_to_env,
        vars_info: after_s.vars_info,
        cursor_env: after_s.cursor_env,
        closest_env: after_s.closest_env
    }

    call_e = Map.put(e, :context, :match)
    {e_expr, %{vars: new_current, unused: new_unused} = s_expr, ee} = fun.(expr, call_s, call_e)

    end_s = %{
      after_s
      | prematch: prematch,
        unused: new_unused,
        vars: new_current,
        calls: s_expr.calls,
        lines_to_env: s_expr.lines_to_env,
        vars_info: s_expr.vars_info,
        cursor_env: s_expr.cursor_env,
        closest_env: s_expr.closest_env
    }

    end_e = Map.put(ee, :context, Map.get(e, :context))
    {e_expr, end_s, end_e}
  end

  def clause(fun, {:->, clause_meta, [_, _]} = clause, s, e)
      when is_function(fun, 4) do
    clause(fn x, sa, ea -> fun.(clause_meta, x, sa, ea) end, clause, s, e)
  end

  def clause(fun, {:->, meta, [left, right]}, s, e) do
    {e_left, sl, el} = fun.(left, s, e)
    {e_right, sr, er} = Compiler.expand(right, sl, el)
    {{:->, meta, [e_left, e_right]}, sr, er}
  end

  def clause(fun, expr, s, e) do
    # try to recover from error by wrapping the expression in clause
    # elixir raises here bad_or_missing_clauses
    clause(fun, {:->, [], [[expr], :ok]}, s, e)
  end

  def head([{:when, meta, [_ | _] = all}], s, e) do
    {args, guard} = Utils.split_last(all)
    prematch = s.prematch

    {{e_args, e_guard}, sg, eg} =
      match(
        fn _ok, sm, em ->
          {e_args, sa, ea} = Compiler.expand_args(args, sm, em)

          {e_guard, sg, eg} =
            guard(guard, %{sa | prematch: prematch}, %{ea | context: :guard})

          type_info = TypeInference.Guard.type_information_from_guards(e_guard)

          sg = State.merge_inferred_types(sg, type_info)

          {{e_args, e_guard}, sg, eg}
        end,
        :ok,
        s,
        s,
        e
      )

    {[{:when, meta, e_args ++ [e_guard]}], sg, eg}
  end

  def head(args, s, e) do
    match(&Compiler.expand_args/3, args, s, s, e)
  end

  def guard({:when, meta, [left, right]}, s, e) do
    {e_left, sl, el} = guard(left, s, e)
    {e_right, sr, er} = guard(right, sl, el)
    {{:when, meta, [e_left, e_right]}, sr, er}
  end

  def guard(guard, s, e) do
    {e_guard, sg, eg} = Compiler.expand(guard, s, e)
    {e_guard, sg, eg}
  end

  # case

  @valid_case_opts [:do]

  def case(e_expr, [], s, e) do
    # elixir raises here missing_option
    # emit a fake do block
    case(e_expr, [do: []], s, e)
  end

  def case(_e_expr, opts, s, e) when not is_list(opts) do
    # elixir raises here invalid_args
    # there may be cursor
    Compiler.expand(opts, s, e)
  end

  def case(e_expr, opts, s, e) do
    # expand invalid opts in case there's cursor
    {_ast, s, _e} = Compiler.expand(opts |> Keyword.drop(@valid_case_opts), s, e)

    opts = sanitize_opts(opts, @valid_case_opts)

    match_context = TypeInference.type_of(e_expr, e.context)

    {case_clauses, sa} =
      Enum.map_reduce(opts, s, fn x, sa ->
        expand_case(x, match_context, sa, e)
      end)

    {case_clauses, sa, e}
  end

  defp expand_case({:do, _} = do_clause, match_context, s, e) do
    expand_clauses(
      fn c, s, e ->
        case head(c, s, e) do
          {[h | _] = c, s, e} ->
            clause_vars_with_inferred_types =
              TypeInference.find_typed_vars(h, match_context, :match)

            s = State.merge_inferred_types(s, clause_vars_with_inferred_types)

            {c, s, e}

          other ->
            other
        end
      end,
      do_clause,
      s,
      e
    )
  end

  # cond

  @valid_cond_opts [:do]

  def cond([], s, e) do
    # elixir raises here missing_option
    # emit a fake do block
    cond([do: []], s, e)
  end

  def cond(opts, s, e) when not is_list(opts) do
    # elixir raises here invalid_args
    # there may be cursor
    Compiler.expand(opts, s, e)
  end

  def cond(opts, s, e) do
    # expand invalid opts in case there's cursor
    {_ast, s, _e} = Compiler.expand(opts |> Keyword.drop(@valid_cond_opts), s, e)

    opts = sanitize_opts(opts, @valid_cond_opts)

    {cond_clauses, sa} =
      Enum.map_reduce(opts, s, fn x, sa ->
        expand_cond(x, sa, e)
      end)

    {cond_clauses, sa, e}
  end

  defp expand_cond({:do, _} = do_clause, s, e) do
    expand_clauses(&Compiler.expand_args/3, do_clause, s, e)
  end

  # receive

  @valid_receive_opts [:do, :after]

  def receive([], s, e) do
    # elixir raises here missing_option
    # emit a fake do block
    receive([do: []], s, e)
  end

  def receive(opts, s, e) when not is_list(opts) do
    # elixir raises here invalid_args
    # there may be cursor
    Compiler.expand(opts, s, e)
  end

  def receive(opts, s, e) do
    # expand invalid opts in case there's cursor
    {_ast, s, _e} = Compiler.expand(opts |> Keyword.drop(@valid_receive_opts), s, e)

    opts = sanitize_opts(opts, @valid_receive_opts)

    {receive_clauses, sa} =
      Enum.map_reduce(opts, s, fn x, sa ->
        expand_receive(x, sa, e)
      end)

    {receive_clauses, sa, e}
  end

  defp expand_receive({:do, {:__block__, _, []}} = do_block, s, _e) do
    {do_block, s}
  end

  defp expand_receive({:do, _} = do_clause, s, e) do
    # no point in doing type inference here, we have no idea what message we may get
    expand_clauses(&head/3, do_clause, s, e)
  end

  defp expand_receive({:after, [_ | _]} = after_clause, s, e) do
    expand_clauses(&Compiler.expand_args/3, after_clause, s, e)
  end

  defp expand_receive({:after, expr}, s, e) when not is_list(expr) do
    # elixir raises here multiple_after_clauses_in_receive
    case expr do
      expr when not is_list(expr) ->
        # try to recover from error by wrapping the expression in list
        expand_receive({:after, [expr]}, s, e)

      [first | discarded] ->
        # try to recover from error by taking first clause only
        # expand other in case there's cursor
        {_ast, s, _e} = Compiler.expand(discarded, s, e)
        expand_receive({:after, [first]}, s, e)

      [] ->
        # try to recover from error by inserting a fake clause
        expand_receive({:after, [{:->, [], [[0], :ok]}]}, s, e)
    end
  end

  # with

  @valid_with_opts [:do, :else]

  def with(meta, args, s, e) do
    {exprs, opts0} = Utils.split_opts(args)

    # expand invalid opts in case there's cursor
    {_ast, s, _e} = Compiler.expand(opts0 |> Keyword.drop(@valid_with_opts), s, e)

    opts0 = sanitize_opts(opts0, @valid_with_opts)
    s0 = State.new_vars_scope(s)
    {e_exprs, {s1, e1}} = Enum.map_reduce(exprs, {s0, e}, &expand_with/2)
    {e_do, opts1, s2} = expand_with_do(meta, opts0, s, s1, e1)
    {e_opts, _opts2, s3} = expand_with_else(opts1, s2, e)

    {{:with, meta, e_exprs ++ [[{:do, e_do} | e_opts]]}, s3, e}
  end

  defp expand_with({:<-, meta, [left, right]}, {s, e}) do
    {e_right, sr, er} = Compiler.expand(right, s, e)
    sm = State.reset_read(sr, s)
    {[e_left], sl, el} = head([left], sm, er)

    match_context_r = TypeInference.type_of(e_right, e.context)
    vars_l_with_inferred_types = TypeInference.find_typed_vars(e_left, match_context_r, :match)

    sl = State.merge_inferred_types(sl, vars_l_with_inferred_types)

    {{:<-, meta, [e_left, e_right]}, {sl, el}}
  end

  defp expand_with(expr, {s, e}) do
    {e_expr, se, ee} = Compiler.expand(expr, s, e)
    {e_expr, {se, ee}}
  end

  defp expand_with_do(_meta, opts, s, acc, e) do
    {expr, rest_opts} = Keyword.pop(opts, :do)
    # elixir raises here missing_option
    # we return empty expression
    expr = expr || []

    {e_expr, s_acc, _e_acc} = Compiler.expand(expr, acc, e)

    {e_expr, rest_opts, State.remove_vars_scope(s_acc, s)}
  end

  defp expand_with_else(opts, s, e) do
    case Keyword.pop(opts, :else) do
      {nil, _} ->
        {[], opts, s}

      {expr, rest_opts} ->
        pair = {:else, expr}

        # no point in doing type inference here, we have no idea what data we are matching against
        {e_pair, se} = expand_clauses(&head/3, pair, s, e)
        {[e_pair], rest_opts, se}
    end
  end

  # try

  @valid_try_opts [:do, :rescue, :catch, :else, :after]

  def try([], s, e) do
    # elixir raises here missing_option
    # emit a fake do block
    try([do: []], s, e)
  end

  def try(opts, s, e) when not is_list(opts) do
    # elixir raises here invalid_args
    # there may be cursor
    Compiler.expand(opts, s, e)
  end

  def try(opts, s, e) do
    # expand invalid opts in case there's cursor
    {_ast, s, _e} = Compiler.expand(opts |> Keyword.drop(@valid_try_opts), s, e)

    opts = sanitize_opts(opts, @valid_try_opts)

    {try_clauses, sa} =
      Enum.map_reduce(opts, s, fn x, sa ->
        expand_try(x, sa, e)
      end)

    {try_clauses, sa, e}
  end

  defp expand_try({:do, expr}, s, e) do
    {e_expr, se, _ee} = Compiler.expand(expr, State.new_vars_scope(s), e)
    {{:do, e_expr}, State.remove_vars_scope(se, s)}
  end

  defp expand_try({:after, expr}, s, e) do
    {e_expr, se, _ee} = Compiler.expand(expr, State.new_vars_scope(s), e)
    {{:after, e_expr}, State.remove_vars_scope(se, s)}
  end

  defp expand_try({:else, _} = else_clause, s, e) do
    # TODO we could try to infer type from last try block expression
    expand_clauses(&head/3, else_clause, s, e)
  end

  defp expand_try({:catch, _} = catch_clause, s, e) do
    expand_clauses_with_stacktrace(&expand_catch/4, catch_clause, s, e)
  end

  defp expand_try({:rescue, _} = rescue_clause, s, e) do
    expand_clauses_with_stacktrace(&expand_rescue/4, rescue_clause, s, e)
  end

  defp expand_clauses_with_stacktrace(fun, clauses, s, e) do
    old_stacktrace = s.stacktrace
    ss = %{s | stacktrace: true}
    {ret, se} = expand_clauses(fun, clauses, ss, e)
    {ret, %{se | stacktrace: old_stacktrace}}
  end

  defp expand_catch(meta, [{:when, when_meta, [a1, a2, a3, dh | dt]}], s, e) do
    # elixir raises here wrong_number_of_args_for_clause
    {_, s, _} = Compiler.expand([dh | dt], s, e)
    expand_catch(meta, [{:when, when_meta, [a1, a2, a3]}], s, e)
  end

  defp expand_catch(_meta, args = [_], s, e) do
    # no point in doing type inference here, we have no idea what throw we caught
    head(args, s, e)
  end

  defp expand_catch(_meta, args = [_, _], s, e) do
    # TODO is it worth to infer type of the first arg? :error | :exit | :throw | {:EXIT, pid()}
    head(args, s, e)
  end

  defp expand_catch(meta, [a1, a2 | d], s, e) do
    # attempt to recover from error by taking 2 first args
    # elixir raises here wrong_number_of_args_for_clause
    {_, s, _} = Compiler.expand(d, s, e)
    expand_catch(meta, [a1, a2], s, e)
  end

  defp expand_rescue(_meta, [arg], s, e) do
    # elixir is strict here and raises invalid_rescue_clause on invalid args
    {e_arg, sa, ea} = expand_rescue(arg, s, e)
    {[e_arg], sa, ea}
  end

  defp expand_rescue(meta, [a1 | d], s, e) do
    # try to recover from error by taking first argument only
    # elixir raises here wrong_number_of_args_for_clause
    {_, s, _} = Compiler.expand(d, s, e)
    expand_rescue(meta, [a1], s, e)
  end

  # rescue var
  defp expand_rescue({name, _, atom} = var, s, e) when is_atom(name) and is_atom(atom) do
    {e_left, sl, el} = match(&Compiler.expand/3, var, s, s, e)

    match_context = {:struct, [], {:atom, Exception}, nil}

    vars_with_inferred_types = TypeInference.find_typed_vars(e_left, match_context, :match)
    sl = State.merge_inferred_types(sl, vars_with_inferred_types)

    {e_left, sl, el}
  end

  # rescue Alias => _ in [Alias]
  defp expand_rescue({:__aliases__, _, [_ | _]} = alias, s, e) do
    expand_rescue({:in, [], [{:_, [], e.module}, alias]}, s, e)
  end

  # rescue var in _
  defp expand_rescue(
         {:in, _, [{name, _, var_context} = var, {:_, _, underscore_context}]},
         s,
         e
       )
       when is_atom(name) and is_atom(var_context) and is_atom(underscore_context) do
    {e_left, sl, el} = match(&Compiler.expand/3, var, s, s, e)

    match_context = {:struct, [], {:atom, Exception}, nil}

    vars_with_inferred_types = TypeInference.find_typed_vars(e_left, match_context, :match)
    sl = State.merge_inferred_types(sl, vars_with_inferred_types)

    {e_left, sl, el}
  end

  # rescue var in (list() or atom())
  defp expand_rescue({:in, meta, [left, right]}, s, e) do
    {e_left, sl, el} = match(&Compiler.expand/3, left, s, s, e)
    {e_right, sr, er} = Compiler.expand(right, sl, el)

    case e_left do
      {name, _, atom} when is_atom(name) and is_atom(atom) ->
        normalized = normalize_rescue(e_right, e)

        match_context =
          for exception <- normalized, reduce: nil do
            nil -> {:struct, [], {:atom, exception}, nil}
            other -> {:union, [other, {:struct, [], {:atom, exception}, nil}]}
          end

        match_context =
          if match_context == nil do
            {:struct, [], {:atom, Exception}, nil}
          else
            match_context
          end

        vars_with_inferred_types = TypeInference.find_typed_vars(e_left, match_context, :match)
        sr = State.merge_inferred_types(sr, vars_with_inferred_types)

        {{:in, meta, [e_left, normalized]}, sr, er}

      _ ->
        # elixir rejects this case, we normalize to underscore
        {{:in, meta, [{:_, [], e.module}, normalize_rescue(e_right, e)]}, sr, er}
    end
  end

  # rescue expr() => rescue expanded_expr()
  defp expand_rescue({_, meta, _} = arg, s, e) do
    # TODO how to check for cursor here?
    case Compiler.Macro.expand_once(arg, %{e | line: Utils.get_line(meta)}) do
      ^arg ->
        # elixir rejects this case
        # try to recover from error by generating fake expression
        expand_rescue({:in, meta, [arg, {:_, [], e.module}]}, s, e)

      new_arg ->
        expand_rescue(new_arg, s, e)
    end
  end

  # rescue list() or atom() => _ in (list() or atom())
  defp expand_rescue(arg, s, e) do
    expand_rescue({:in, [], [{:_, [], e.module}, arg]}, s, e)
  end

  defp normalize_rescue(atom, _e) when is_atom(atom) do
    [atom]
  end

  defp normalize_rescue(other, e) do
    # elixir is strict here, we reject invalid nodes
    res =
      if is_list(other) do
        Enum.filter(other, &is_atom/1)
      else
        []
      end

    if res == [] do
      [{:_, [], e.module}]
    else
      res
    end
  end

  defp expand_clauses(fun, {key, [_ | _] = clauses}, s, e) do
    transformer = fn clause, sa ->
      {e_clause, s_acc, _e_acc} =
        clause(fun, clause, State.new_vars_scope(sa), e)

      {e_clause, State.remove_vars_scope(s_acc, sa)}
    end

    {values, se} = Enum.map_reduce(clauses, s, transformer)
    {{key, values}, se}
  end

  defp expand_clauses(fun, {key, expr}, s, e) do
    # try to recover from error by wrapping the expression in a clauses list
    # elixir raises here bad_or_missing_clauses
    expand_clauses(fun, {key, [expr]}, s, e)
  end

  # helpers

  defp sanitize_opt(opts, opt) do
    case Keyword.fetch(opts, opt) do
      :error -> []
      {:ok, value} -> [{opt, value}]
    end
  end

  defp sanitize_opts(opts, allowed) do
    Enum.flat_map(allowed, fn opt -> sanitize_opt(opts, opt) end)
  end
end