defmodule ElixirSense.Core.Compiler.Clauses do
  @moduledoc false
  require Logger
  alias ElixirSense.Core.Compiler
  alias ElixirSense.Core.Compiler.Utils
  alias ElixirSense.Core.Compiler.State
  alias ElixirSense.Core.TypeInference
  alias ElixirSense.Core.TypeInference.Occurrence

  # See ElixirSense.Core.Compiler.@stamp_version — gates version stamping
  # on `with` to host Elixir 1.20+ (commit 603602e67 — reverse arrows).
  @stamp_version Version.match?(System.version(), ">= 1.20.0-dev")

  def parallel_match(meta, expr, s, %{context: :match} = e) do
    %{vars: {_read, write}} = s

    matches = unpack_match(expr, meta, [])

    # {[{_, e_head} | e_tail], e_writes, sm, em} =
    #   Enum.reduce(matches, {[], [], s, e}, fn {e_meta, match}, {acc_matches, acc_writes, si, ei} ->
    #     # #elixir_ex{vars={Read, _Write}} = SI
    #     %{vars: {read, _write}} = si

    #     # {EMatch, SM, EM} = elixir_expand:expand(Match, SI#elixir_ex{vars={Read, #{}}}, EI)
    #     {e_match, s_m, e_m} =
    #       Compiler.expand(match, %{si | vars: {read, %{}}}, ei)

    #     # #elixir_ex{vars={_, EWrite}} = SM
    #     %{vars: {_, e_write}} = s_m

    #     # {[{EMeta, EMatch} | AccMatches], [EWrite | AccWrites], SM, EM}
    #     {
    #       [{e_meta, e_match} | acc_matches],
    #       [e_write | acc_writes],
    #       s_m,
    #       e_m
    #     }
    #   end)

    {[{_, e_head} | e_tail], e_writes, sm, em} =
      :lists.foldl(
        fn {e_meta, match}, {acc_matches, acc_writes, si, ei} ->
          %{vars: {read, _write}} = si
          {e_match, sm, em} = Compiler.expand(match, %{si | vars: {read, %{}}}, ei)
          %{vars: {_, e_write}} = sm
          {[{e_meta, e_match} | acc_matches], [e_write | acc_writes], sm, em}
        end,
        {[], [], s, e},
        matches
      )

    # EMatch =
    #   lists:foldl(fun({EMeta, EMatch}, Acc) ->
    #     {'=', EMeta, [EMatch, Acc]}
    #   end, EHead, ETail),
    # e_match =
    #   Enum.reduce(e_tail, e_head, fn {e_meta, e_m}, acc ->
    #     {:'=', e_meta, [e_m, acc]}
    #   end)

    e_match =
      :lists.foldl(
        fn {e_meta, e_match}, acc ->
          {:=, e_meta, [e_match, acc]}
        end,
        e_head,
        e_tail
      )

    %{vars: {v_read, _}, prematch: {p_read, cycles, p_info}} = sm

    {p_cycles, p_writes} = store_cycles(e_writes, cycles, %{})

    v_write =
      write != false && State.merge_vars(write, p_writes)

    updated_sm = %{
      sm
      | vars: {v_read, v_write},
        prematch: {p_read, p_cycles, p_info}
    }

    {e_match, updated_sm, em}
  end

  defp unpack_match({:=, meta, [{_, _var_meta, _} = node, node]}, _meta, acc) do
    # TODO: remove this clause on Elixir v1.23
    # elixir warns here on duplicate_match
    unpack_match(node, meta, acc)
  end

  defp unpack_match({:=, meta, [left, right]}, _meta, acc),
    do: unpack_match(left, meta, unpack_match(right, meta, acc))

  defp unpack_match(node, meta, acc) do
    [{meta, node} | acc]
  end

  defp store_cycles([write | writes], {cycles, skip_list}, acc) do
    # Compute the variables this parallel pattern depends on
    # depends_on =
    #   Enum.reduce(writes, acc, fn w, a ->
    #     Map.merge(a, w)
    #   end)

    depends_on = :lists.foldl(&:maps.merge/2, acc, writes)

    # For each variable on a sibling, store it inside the graph (Cycles)
    # acc_cycles =
    #   Enum.reduce(write, cycles, fn {pair, _}, acc_cycles ->
    #     Map.update(acc_cycles, pair, depends_on, fn current ->
    #       # maps:merge_with(fun(_, _, _) -> error end, Current, DependsOn)
    #       Map.merge(current, depends_on, fn _, _, _ -> :error end)
    #     end)
    #   end)

    acc_cycles =
      :maps.fold(
        fn pair, _, acc_cycles ->
          :maps.update_with(
            pair,
            fn current ->
              Map.merge(current, depends_on, fn _, _, _ -> :error end)
            end,
            depends_on,
            acc_cycles
          )
        end,
        cycles,
        write
      )

    # The SkipList logic
    acc_skip_list =
      case map_size(depends_on) > 1 do
        true -> [depends_on | skip_list]
        false -> skip_list
      end

    # store_cycles(writes, {acc_cycles, acc_skip_list}, Map.merge(acc, write))
    store_cycles(writes, {acc_cycles, acc_skip_list}, :maps.merge(acc, write))
  end

  defp store_cycles([], cycles, acc) do
    {cycles, acc}
  end

  def match(fun, expr, after_s, before_s, e) do
    # elixir validates if context is not guard
    %{vars: current, unused: counter} = after_s
    %{vars: {read, _write}, prematch: prematch} = before_s

    call_s = %{
      before_s
      | prematch: {read, {%{}, []}, counter},
        unused: counter,
        vars: current,
        calls: after_s.calls,
        lines_to_env: after_s.lines_to_env,
        vars_info: after_s.vars_info,
        cursor_env: after_s.cursor_env,
        closest_env: after_s.closest_env
    }

    call_e = Map.put(e, :context, :match)
    {e_expr, %{vars: new_current, unused: new_unused} = s_expr, ee} = fun.(expr, call_s, call_e)

    # elixir calls validate_cycles here

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

  def clause(fun, {:->, meta, [left, right]}, s, e) do
    {e_left, sl, el} =
      if is_function(fun, 4) do
        fun.(meta, left, s, e)
      else
        fun.(left, s, e)
      end

    {e_right, sr, er} = Compiler.expand(right, sl, el)
    {{:->, meta, [e_left, e_right]}, sr, er}
  end

  def clause(fun, expr, s, e) do
    # try to recover from error by wrapping the expression in clause
    # elixir raises here bad_or_missing_clauses
    clause(fun, {:->, [], [[expr], :ok]}, s, e)
  end

  def head([{:when, when_meta, [_ | _] = all}], s, e) do
    {args, guard} = Utils.split_last(all)
    guarded_head(when_meta, args, guard, s, e)
  end

  def head(args, s, e) do
    match(&Compiler.expand_args/3, args, s, s, e)
  end

  defp guarded_head(when_meta, args, guard, s, e) do
    {e_args, sa, ea} = match(&Compiler.expand_args/3, args, s, s, e)

    prematch =
      if Version.match?(System.version(), ">= 1.18.0-dev") do
        sa.prematch
      else
        s.prematch
      end

    {e_guard, sg, eg} = guard(guard, %{sa | prematch: prematch}, %{ea | context: :guard})

    type_info = TypeInference.Guard.type_information_from_guards(e_guard)
    sg = State.merge_inferred_types(sg, type_info)

    {[{:when, when_meta, e_args ++ [e_guard]}], sg, %{eg | context: nil}}
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

    match_context = TypeInference.type_of(e_expr, e.context, s, e)

    {case_clauses, sa} =
      Enum.map_reduce(opts, s, fn x, sa ->
        expand_case(x, e_expr, match_context, sa, e)
      end)

    {case_clauses, sa, e}
  end

  defp expand_case({:do, _} = do_clause, scrutinee_ast, match_context, s, e) do
    expand_clauses(
      fn c, s, e ->
        case head(c, s, e) do
          {[h | _] = c, s, e} ->
            clause_vars_with_inferred_types =
              TypeInference.find_typed_vars(h, match_context, :match)

            # Enhanced pattern matching refinement with ElixirTypes
            {enhanced_vars, var_descrs} =
              enhance_case_pattern_vars(clause_vars_with_inferred_types, h, match_context, s)

            s = State.merge_inferred_types(s, enhanced_vars)
            s = State.merge_inferred_elixir_types(s, var_descrs)

            # L1 occurrence typing: within this branch, narrow the scrutinee
            # variable itself to the type the clause pattern matched.
            s = State.merge_inferred_types(s, Occurrence.scrutinee_refinements(scrutinee_ast, h))

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
    expand_clauses(&expand_cond_head/3, do_clause, s, e)
  end

  # A cond clause head is a single-element arg list `[condition]`. In the branch
  # where the condition holds, refine the tested variables (L1 occurrence
  # typing) so the clause body sees the narrowed types.
  defp expand_cond_head([_condition] = args, s, e) do
    {e_args, s, e} = Compiler.expand_args(args, s, e)
    s = State.merge_inferred_types(s, Occurrence.condition_refinements(hd(e_args)))
    {e_args, s, e}
  end

  defp expand_cond_head(args, s, e) do
    Compiler.expand_args(args, s, e)
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
    # try to recover from error by wrapping the expression in list
    expand_receive({:after, [expr]}, s, e)
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

    # Stamp a {:version, N} on `with` (1.20+ only — commit 603602e67).
    {meta, s3} =
      if @stamp_version do
        counter = s3.unused
        {[{:version, counter} | meta], %{s3 | unused: counter + 1}}
      else
        {meta, s3}
      end

    {{:with, meta, e_exprs ++ [[{:do, e_do} | e_opts]]}, s3, e}
  end

  defp expand_with({:<-, meta, [left, right]}, {s, e}) do
    {e_right, sr, er} = Compiler.expand(right, s, e)
    sm = State.reset_read(sr, s)
    {[e_left], sl, el} = head([left], sm, er)

    match_context_r = TypeInference.type_of(e_right, e.context, sr, e)
    vars_l_with_inferred_types = TypeInference.find_typed_vars(e_left, match_context_r, :match)

    # Enhanced pattern matching refinement with ElixirTypes for with expressions
    {enhanced_vars, var_descrs} =
      enhance_with_pattern_vars(vars_l_with_inferred_types, e_left, match_context_r, sl)

    sl = State.merge_inferred_types(sl, enhanced_vars)
    sl = State.merge_inferred_elixir_types(sl, var_descrs)

    # L1 occurrence typing: in the success continuation, narrow the right-hand
    # side variable to the type the left pattern matched.
    sl = State.merge_inferred_types(sl, Occurrence.scrutinee_refinements(e_right, e_left))

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

  defp expand_catch(_meta, [{:when, when_meta, [arg1, arg2, guard]}], s, e),
    do: guarded_head(when_meta, [arg1, arg2], guard, s, e)

  defp expand_catch(_meta, [{:when, when_meta, [arg1, guard]}], s, e),
    do: guarded_head(when_meta, [:throw, arg1], guard, s, e)

  defp expand_catch(_meta, [arg], s, e), do: head([:throw, arg], s, e)

  defp expand_catch(_meta, [_, _] = args, s, e) do
    # Enhanced catch pattern with type refinement for error/exit/throw
    {[e_kind, e_value], sl, el} = head(args, s, e)

    # Infer types for catch patterns
    kind_context = {:union, [{:atom, :error}, {:atom, :exit}, {:atom, :throw}]}
    kind_vars = TypeInference.find_typed_vars(e_kind, kind_context, :match)

    # Value could be anything - for :error it's often an exception
    value_vars = TypeInference.find_typed_vars(e_value, nil, :match)

    # Enhanced catch pattern refinement with ElixirTypes
    {enhanced_kind_vars, kind_descrs} =
      enhance_catch_pattern_vars(kind_vars, e_kind, kind_context, sl)

    {enhanced_value_vars, value_descrs} = enhance_catch_pattern_vars(value_vars, e_value, nil, sl)

    sl = State.merge_inferred_types(sl, enhanced_kind_vars ++ enhanced_value_vars)
    sl = State.merge_inferred_elixir_types(sl, Map.merge(kind_descrs, value_descrs))

    {[e_kind, e_value], sl, el}
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

    # Enhanced rescue pattern refinement with ElixirTypes
    {enhanced_vars, var_descrs} =
      enhance_rescue_pattern_vars(vars_with_inferred_types, e_left, match_context, sl)

    sl = State.merge_inferred_types(sl, enhanced_vars)
    sl = State.merge_inferred_elixir_types(sl, var_descrs)

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

    # Enhanced rescue pattern refinement with ElixirTypes
    {enhanced_vars, var_descrs} =
      enhance_rescue_pattern_vars(vars_with_inferred_types, e_left, match_context, sl)

    sl = State.merge_inferred_types(sl, enhanced_vars)
    sl = State.merge_inferred_elixir_types(sl, var_descrs)

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

        # Enhanced rescue pattern refinement with ElixirTypes
        {enhanced_vars, var_descrs} =
          enhance_rescue_pattern_vars(vars_with_inferred_types, e_left, match_context, sr)

        sr = State.merge_inferred_types(sr, enhanced_vars)
        sr = State.merge_inferred_elixir_types(sr, var_descrs)

        {{:in, meta, [e_left, normalized]}, sr, er}

      _ ->
        # elixir rejects this case, we normalize to underscore
        {{:in, meta, [{:_, [], e.module}, normalize_rescue(e_right, e)]}, sr, er}
    end
  end

  # rescue expr() => rescue expanded_expr()
  defp expand_rescue({_, meta, _} = arg, s, e) do
    # TODO how to check for cursor here?
    expanded = Compiler.Macro.expand_once(arg, %{e | line: Utils.get_line(meta)})

    case expanded do
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

  # Enhanced pattern matching refinement for case expressions
  defp enhance_case_pattern_vars(clause_vars, pattern_ast, match_context, state) do
    enhance_pattern_vars(clause_vars, pattern_ast, match_context, :__case_subject__, state)
  end

  # Convert TypeInference match context to Module.Types descriptor.
  # Delegates to coerce_var_type which handles all ElixirSense shape types.
  defp match_context_to_descr(nil), do: nil
  defp match_context_to_descr(:none), do: nil

  defp match_context_to_descr(match_context) do
    if ElixirSense.Core.ElixirTypes.enabled?() do
      try do
        descr = ElixirSense.Core.ElixirTypes.coerce_var_type_public(match_context)

        # coerce_var_type returns dynamic() for unrecognized shapes — treat as nil
        # to avoid passing uninformative constraints to native pattern matching
        if descr == Module.Types.Descr.dynamic() do
          nil
        else
          descr
        end
      rescue
        e ->
          Logger.debug(
            "match_context_to_descr failed: #{Exception.format(:error, e, __STACKTRACE__)}"
          )

          nil
      end
    else
      nil
    end
  end

  # Merge clause variables with ElixirTypes refinements
  defp merge_clause_vars_with_elixir_types(clause_vars, refined_vars) do
    clause_map = Map.new(clause_vars)

    enhanced_map =
      Enum.reduce(refined_vars, clause_map, fn {key, elixir_type}, acc ->
        case Map.fetch(acc, key) do
          {:ok, existing_type} ->
            # Merge existing type with ElixirTypes refinement
            merged_type = TypeInference.intersect(existing_type, elixir_type)
            Map.put(acc, key, merged_type)

          :error ->
            # Add new refined variable
            Map.put(acc, key, elixir_type)
        end
      end)

    # Maintain original ordering but include all variables
    original_keys = Enum.map(clause_vars, &elem(&1, 0))
    ordered = Enum.map(original_keys, fn key -> {key, Map.get(enhanced_map, key)} end)

    additional =
      enhanced_map
      |> Enum.reject(fn {key, _} -> key in original_keys end)

    ordered ++ additional
  end

  # Helper functions for environment extraction (adapted from compiler.ex)
  defp env_for_meta_clauses(state, ast) do
    meta = extract_meta_from_ast(ast)
    line = Keyword.get(meta, :line, 0)

    cond do
      is_integer(line) and line > 0 ->
        Map.get(state.lines_to_env, line) || closest_env_clauses(state)

      true ->
        closest_env_clauses(state)
    end
  end

  defp extract_meta_from_ast({_, meta, _}) when is_list(meta), do: meta
  defp extract_meta_from_ast(_), do: []

  defp file_for_meta(ast) do
    meta = extract_meta_from_ast(ast)
    Keyword.get(meta, :file)
  end

  defp closest_env_clauses(%ElixirSense.Core.Compiler.State{closest_env: {{_, _}, _dist, env}})
       when not is_nil(env), do: env

  defp closest_env_clauses(_), do: nil

  defp module_file_clauses(nil), do: nil

  defp module_file_clauses(module) when is_atom(module) do
    case :code.which(module) do
      path when is_list(path) -> List.to_string(path)
      _ -> nil
    end
  end

  # Enhanced pattern matching refinement for with expressions
  defp enhance_with_pattern_vars(clause_vars, pattern_ast, match_context, state) do
    enhance_pattern_vars(clause_vars, pattern_ast, match_context, :__with_expression__, state)
  end

  # Enhanced pattern matching refinement for rescue expressions
  defp enhance_rescue_pattern_vars(clause_vars, pattern_ast, match_context, state) do
    enhance_pattern_vars(clause_vars, pattern_ast, match_context, :__rescue_exception__, state)
  end

  # Enhanced pattern matching refinement for catch expressions
  defp enhance_catch_pattern_vars(clause_vars, pattern_ast, match_context, state) do
    enhance_pattern_vars(clause_vars, pattern_ast, match_context, :__catch_value__, state)
  end

  # Common implementation for pattern matching refinement via ElixirTypes
  defp enhance_pattern_vars(clause_vars, pattern_ast, match_context, match_label, state) do
    if ElixirSense.Core.ElixirTypes.enabled?() and clause_vars != [] do
      try do
        env = env_for_meta_clauses(state, pattern_ast)
        module = env && env.module
        function = env && env.function
        file = file_for_meta(pattern_ast) || module_file_clauses(module)

        expected_descr = match_context_to_descr(match_context)
        target_keys = Enum.map(clause_vars, &elem(&1, 0))

        case ElixirSense.Core.ElixirTypes.of_match(
               pattern_ast,
               expected_descr,
               {:variable_match, match_label},
               module,
               function,
               file,
               :dynamic,
               target_keys: target_keys
             ) do
          {:ok, refined_vars, var_descrs} when map_size(refined_vars) > 0 ->
            merged = merge_clause_vars_with_elixir_types(clause_vars, refined_vars)
            {merged, var_descrs}

          _ ->
            {clause_vars, %{}}
        end
      rescue
        e ->
          Logger.debug(
            "enhance_pattern_vars failed: #{Exception.format(:error, e, __STACKTRACE__)}"
          )

          {clause_vars, %{}}
      catch
        kind, payload ->
          Logger.debug(
            "enhance_pattern_vars failed: #{Exception.format(kind, payload, __STACKTRACE__)}"
          )

          {clause_vars, %{}}
      end
    else
      {clause_vars, %{}}
    end
  end
end
