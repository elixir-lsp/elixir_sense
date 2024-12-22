defmodule ElixirSense.Core.Compiler do
  alias ElixirSense.Core.Compiler.State
  require Logger
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.TypeInfo
  alias ElixirSense.Core.TypeInference
  alias ElixirSense.Core.TypeInference.Guard
  alias ElixirSense.Core.Normalized.Macro.Env, as: NormalizedMacroEnv
  alias ElixirSense.Core.State.ModFunInfo

  @env :elixir_env.new()
  def env, do: @env

  def expand(ast, state, env) do
    try do
      state =
        case ast do
          {_, meta, _} when is_list(meta) ->
            state
            |> State.add_current_env_to_line(meta, env)
            |> State.update_closest_env(meta, env)

          # state
          _ ->
            state
        end

      do_expand(ast, state, env)
    catch
      kind, payload ->
        Logger.warning(
          "Unable to expand ast node #{inspect(ast)}: #{Exception.format(kind, payload, __STACKTRACE__)}"
        )

        {ast, state, env}
    end
  end

  # =/2

  defp do_expand({:=, meta, [_, _]} = expr, s, e = %{context: :match}) do
    {e_expr, se, ee} = __MODULE__.Clauses.parallel_match(meta, expr, s, e)

    vars_with_inferred_types = TypeInference.find_typed_vars(e_expr, nil, :match)

    se = State.merge_inferred_types(se, vars_with_inferred_types)

    {e_expr, se, ee}
  end

  defp do_expand({:=, meta, [left, right]}, s, e) do
    # elixir validates we are not in guard context
    {e_right, sr, er} = expand(right, s, e)
    {e_left, sl, el} = __MODULE__.Clauses.match(&expand/3, left, sr, s, er)

    e_expr = {:=, meta, [e_left, e_right]}

    vars_with_inferred_types = TypeInference.find_typed_vars(e_expr, nil, el.context)

    sl = State.merge_inferred_types(sl, vars_with_inferred_types)

    {e_expr, sl, el}
  end

  # Literal operators

  defp do_expand({:{}, meta, args}, state, env) do
    {args, state, env} = expand_args(args, state, env)
    {{:{}, meta, args}, state, env}
  end

  defp do_expand({:%{}, meta, args}, state, env) do
    __MODULE__.Map.expand_map(meta, args, state, env)
  end

  defp do_expand({:%, meta, [left, right]}, state, env) do
    __MODULE__.Map.expand_struct(meta, left, right, state, env)
  end

  defp do_expand({:<<>>, meta, args}, state, env) do
    __MODULE__.Bitstring.expand(meta, args, state, env, false)
  end

  defp do_expand({:->, meta, [left, right]}, s, e) do
    # elixir raises here unhandled_arrow_op
    expand({:"__->__", meta, [left, right]}, s, e)
  end

  defp do_expand({:"::", meta, [left, right]}, s, e) do
    # elixir raises here unhandled_type_op
    expand({:"__::__", meta, [left, right]}, s, e)
  end

  defp do_expand({:|, meta, [left, right]}, s, e) do
    # elixir raises here unhandled_cons_op
    expand({:"__|__", meta, [left, right]}, s, e)
  end

  defp do_expand({:"\\\\", meta, [left, right]}, s, e) do
    # elixir doesn't match on naked default args operator
    expand({:"__\\\\__", meta, [left, right]}, s, e)
  end

  # __block__

  defp do_expand({:__block__, _meta, []}, s, e), do: {nil, s, e}

  defp do_expand({:__block__, _meta, [arg]}, s, e) do
    expand(arg, s, e)
  end

  defp do_expand({:__block__, meta, args}, s, e) when is_list(args) do
    {e_args, sa, ea} = expand_block(args, [], meta, s, e)
    {{:__block__, meta, e_args}, sa, ea}
  end

  # __aliases__

  defp do_expand({:__aliases__, meta, [head | tail] = list}, state, env) do
    case NormalizedMacroEnv.expand_alias(env, meta, list, trace: false) do
      {:alias, alias} ->
        # TODO track alias
        {alias, state, env}

      :error ->
        {head, state, env} = expand(head, state, env)

        if is_atom(head) do
          # TODO track alias
          {Module.concat([head | tail]), state, env}
        else
          # elixir raises here invalid_alias
          {{:__aliases__, meta, [head | tail]}, state, env}
        end
    end
  end

  # require, alias, import

  defp do_expand({form, meta, [{{:., _, [base, :{}]}, _, refs} | rest]}, state, env)
       when form in [:require, :alias, :import] do
    case rest do
      [] ->
        expand_multi_alias_call(form, meta, base, refs, [], state, env)

      [opts] ->
        # elixir raises if there is :as in opts, we omit it
        opts = Keyword.delete(opts, :as)

        expand_multi_alias_call(form, meta, base, refs, opts, state, env)
    end
  end

  defp do_expand({form, meta, [arg]}, state, env) when form in [:require, :alias, :import] do
    expand({form, meta, [arg, []]}, state, env)
  end

  defp do_expand({:alias, meta, [arg, opts]}, state, env) do
    state =
      state
      |> State.add_first_alias_positions(env, meta)
      |> State.add_current_env_to_line(meta, env)

    # no need to call expand_without_aliases_report - we never report
    {arg, state, env} = expand(arg, state, env)
    {opts, state, env} = expand_opts([:as, :warn], no_alias_opts(opts), state, env)

    if is_atom(arg) do
      case NormalizedMacroEnv.define_alias(env, meta, arg, [trace: false] ++ opts) do
        {:ok, env} ->
          {arg, state, env}

        {:error, _} ->
          # elixir_aliases
          {arg, state, env}
      end
    else
      # expected_compile_time_module
      {arg, state, env}
    end
  end

  defp do_expand({:require, meta, [arg, opts]}, state, env) do
    state =
      state
      |> State.add_current_env_to_line(meta, env)

    # no need to call expand_without_aliases_report - we never report
    {arg, state, env} = expand(arg, state, env)

    {opts, state, env} =
      expand_opts([:as, :warn], no_alias_opts(opts), state, env)

    # elixir handles special meta key :defined in the require call.
    # It is only set by defmodule and we handle it there

    if is_atom(arg) do
      # elixir calls here :elixir_aliases.ensure_loaded(meta, e_ref, et)
      # and optionally waits until required module is compiled
      case NormalizedMacroEnv.define_require(env, meta, arg, [trace: false] ++ opts) do
        {:ok, env} ->
          {arg, state, env}

        {:error, _} ->
          # elixir_aliases
          {arg, state, env}
      end
    else
      # expected_compile_time_module
      {arg, state, env}
    end
  end

  defp do_expand({:import, meta, [arg, opts]}, state, env) do
    state =
      state
      |> State.add_current_env_to_line(meta, env)

    # no need to call expand_without_aliases_report - we never report
    {arg, state, env} = expand(arg, state, env)
    {opts, state, env} = expand_opts([:only, :except, :warn], opts, state, env)

    if is_atom(arg) do
      opts =
        opts
        |> Keyword.merge(
          trace: false,
          emit_warnings: false,
          info_callback: import_info_callback(arg, state)
        )

      case NormalizedMacroEnv.define_import(env, meta, arg, opts) do
        {:ok, env} ->
          {arg, state, env}

        _ ->
          {arg, state, env}
      end
    else
      # expected_compile_time_module
      {arg, state, env}
    end
  end

  # Compilation environment macros

  defp do_expand({:__MODULE__, meta, ctx}, state, env) when is_atom(ctx) do
    state = State.add_current_env_to_line(state, meta, env)

    {env.module, state, env}
  end

  defp do_expand({:__DIR__, meta, ctx}, state, env) when is_atom(ctx) do
    state = State.add_current_env_to_line(state, meta, env)

    {Path.dirname(env.file), state, env}
  end

  defp do_expand({:__CALLER__, meta, ctx} = caller, state, env) when is_atom(ctx) do
    # elixir checks if context is not match and if caller is allowed
    state = State.add_current_env_to_line(state, meta, env)

    {caller, state, env}
  end

  defp do_expand({:__STACKTRACE__, meta, ctx} = stacktrace, state, env) when is_atom(ctx) do
    # elixir checks if context is not match and if stacktrace is allowed
    state = State.add_current_env_to_line(state, meta, env)

    {stacktrace, state, env}
  end

  defp do_expand({:__ENV__, meta, ctx}, state, env) when is_atom(ctx) do
    # elixir checks if context is not match
    state = State.add_current_env_to_line(state, meta, env)

    {escape_map(escape_env_entries(meta, state, env)), state, env}
  end

  defp do_expand({{:., dot_meta, [{:__ENV__, meta, atom}, field]}, call_meta, []}, s, e)
       when is_atom(atom) and is_atom(field) do
    # elixir checks if context is not match
    s = State.add_current_env_to_line(s, call_meta, e)

    env = escape_env_entries(meta, s, e)

    case Map.fetch(env, field) do
      {:ok, value} -> {value, s, e}
      :error -> {{{:., dot_meta, [escape_map(env), field]}, call_meta, []}, s, e}
    end
  end

  # Quote

  defp do_expand({unquote_call, meta, [arg]}, s, e)
       when unquote_call in [:unquote, :unquote_splicing] do
    # elixir raises here unquote_outside_quote
    # we may have cursor there
    {arg, s, e} = expand(arg, s, e)
    s = s |> State.add_current_env_to_line(meta, e)
    {{unquote_call, meta, [arg]}, s, e}
  end

  defp do_expand({:quote, meta, [opts]}, s, e) when is_list(opts) do
    case Keyword.pop(opts, :do) do
      {nil, _} ->
        # elixir raises here missing_option
        # generate a fake do block
        expand({:quote, meta, [opts, [{:do, {:__block__, [], []}}]]}, s, e)

      {do_block, new_opts} ->
        expand({:quote, meta, [new_opts, [{:do, do_block}]]}, s, e)
    end
  end

  defp do_expand({:quote, meta, [arg]}, s, e) do
    # elixir raises here invalid_args
    # we may have cursor there
    {arg, s, e} = expand(arg, s, e)
    s = s |> State.add_current_env_to_line(meta, e)
    {{:quote, meta, [arg]}, s, e}
  end

  defp do_expand({:quote, meta, [opts, do_block]}, s, e) when is_list(do_block) do
    exprs =
      case Keyword.fetch(do_block, :do) do
        {:ok, expr} ->
          expr

        :error ->
          # elixir raises here missing_option
          # try to recover from error by generating a fake do block
          {:__block__, [], [do_block]}
      end

    valid_opts = [:context, :location, :line, :file, :unquote, :bind_quoted, :generated]
    {e_opts, st, et} = expand_opts(valid_opts, opts, s, e)

    context = Keyword.get(e_opts, :context, e.module || :"Elixir")

    {file, line} =
      case Keyword.fetch(e_opts, :location) do
        {:ok, :keep} -> {e.file, true}
        :error -> {Keyword.get(e_opts, :file, nil), Keyword.get(e_opts, :line, false)}
      end

    {binding, default_unquote} =
      case Keyword.fetch(e_opts, :bind_quoted) do
        {:ok, bq} ->
          if is_list(bq) do
            # safe to drop, opts already expanded
            bq = Enum.filter(bq, &match?({key, _} when is_atom(key), &1))
            {bq, false}
          else
            {[], false}
          end

        :error ->
          {[], true}
      end

    unquote_opt = Keyword.get(e_opts, :unquote, default_unquote)
    generated = Keyword.get(e_opts, :generated, false)

    # alternative implementation
    # res = expand_quote(exprs, st, et)
    # res |> elem(0) |> IO.inspect
    # res
    {q, q_context, q_prelude} =
      __MODULE__.Quote.build(meta, line, file, context, unquote_opt, generated, et)

    {e_prelude, sp, ep} = expand(q_prelude, st, et)
    {e_context, sc, ec} = expand(q_context, sp, ep)
    {quoted, sc} = __MODULE__.Quote.quote(exprs, q, sc)
    {e_quoted, es, eq} = expand(quoted, sc, ec)

    es = es |> State.add_current_env_to_line(meta, eq)

    e_binding =
      for {k, v} <- binding do
        {:{}, [], [:=, [], [{:{}, [], [k, meta, e_context]}, v]]}
      end

    e_binding_quoted =
      case e_binding do
        [] -> e_quoted
        _ -> {:{}, [], [:__block__, [], e_binding ++ [e_quoted]]}
      end

    case e_prelude do
      [] -> {e_binding_quoted, es, eq}
      _ -> {{:__block__, [], e_prelude ++ [e_binding_quoted]}, es, eq}
    end
  end

  defp do_expand({:quote, meta, [arg1, arg2]}, s, e) do
    # elixir raises here invalid_args
    # try to recover from error by wrapping arg in a do block
    expand({:quote, meta, [arg1, [{:do, {:__block__, [], [arg2]}}]]}, s, e)
  end

  # Functions

  defp do_expand({:&, meta, [{:super, super_meta, args} = expr]}, s, e) when is_list(args) do
    case resolve_super(meta, length(args), s, e) do
      {kind, name, _} when kind in [:def, :defp] ->
        expand_fn_capture(meta, {name, super_meta, args}, s, e)

      _ ->
        expand_fn_capture(meta, expr, s, e)
    end
  end

  defp do_expand(
         {:&, meta, [{:/, arity_meta, [{:super, super_meta, context}, arity]} = expr]},
         s,
         e
       )
       when is_atom(context) and is_integer(arity) do
    case resolve_super(meta, arity, s, e) do
      {kind, name, _} when kind in [:def, :defp] ->
        s =
          s
          |> State.add_call_to_line({nil, name, arity}, super_meta)
          |> State.add_current_env_to_line(super_meta, e)

        {{:&, meta, [{:/, arity_meta, [{name, super_meta, context}, arity]}]}, s, e}

      _ ->
        expand_fn_capture(meta, expr, s, e)
    end
  end

  defp do_expand({:&, meta, [arg]}, s, e) do
    expand_fn_capture(meta, arg, s, e)
  end

  defp do_expand({:fn, meta, pairs}, s, e) do
    __MODULE__.Fn.expand(meta, pairs, s, e)
  end

  # case/cond/try/receive

  defp do_expand({:cond, meta, [opts]}, s, e) do
    # elixir raises underscore_in_cond if the last clause is _
    {e_clauses, sc, ec} = __MODULE__.Clauses.cond(opts, s, e)
    {{:cond, meta, [e_clauses]}, sc, ec}
  end

  defp do_expand({:case, meta, [expr, options]}, s, e) do
    expand_case(meta, expr, options, s, e)
  end

  defp do_expand({:receive, meta, [opts]}, s, e) do
    {e_clauses, sc, ec} = __MODULE__.Clauses.receive(opts, s, e)
    {{:receive, meta, [e_clauses]}, sc, ec}
  end

  defp do_expand({:try, meta, [opts]}, s, e) do
    {e_clauses, sc, ec} = __MODULE__.Clauses.try(opts, s, e)
    {{:try, meta, [e_clauses]}, sc, ec}
  end

  defp do_expand({:for, _, [_ | _]} = expr, s, e), do: expand_for(expr, s, e, true)

  defp do_expand({:with, meta, [_ | _] = args}, s, e) do
    __MODULE__.Clauses.with(meta, args, s, e)
  end

  # Cursor

  defp do_expand({:__cursor__, meta, args}, s, e) when is_list(args) do
    s =
      unless s.cursor_env do
        s
        |> State.add_cursor_env(meta, e)
      else
        s
      end

    case args do
      [h | _] ->
        expand(h, s, e)

      [] ->
        {nil, s, e}
    end
  end

  # Super

  defp do_expand({:super, meta, args}, s, e) when is_list(args) do
    arity = length(args)

    case resolve_super(meta, arity, s, e) do
      {kind, name, _} ->
        {e_args, sa, ea} = expand_args(args, s, e)

        sa =
          sa
          |> State.add_call_to_line({nil, name, arity}, meta)
          |> State.add_current_env_to_line(meta, ea)

        {{:super, [{:super, {kind, name}} | meta], e_args}, sa, ea}

      _ ->
        # elixir does not allow this branch
        expand_local(meta, :super, args, s, e)
    end
  end

  # Vars

  # Pin operator
  # It only appears inside match and it disables the match behaviour.

  defp do_expand({:^, meta, [arg]}, %{prematch: {prematch, _, _}, vars: {_, write}} = s, e) do
    no_match_s = %{s | prematch: :pin, vars: {prematch, write}}

    case expand(arg, no_match_s, %{e | context: nil}) do
      {{name, _var_meta, kind} = var, %{unused: unused}, _}
      when is_atom(name) and is_atom(kind) ->
        s = State.add_var_read(s, var)
        {{:^, meta, [var]}, %{s | unused: unused}, e}

      {arg, s, _e} ->
        # elixir raises here invalid_arg_for_pin
        # we may have cursor in arg
        {{:^, meta, [arg]}, s, e}
    end
  end

  defp do_expand({:^, _meta, [arg]}, s, e) do
    # elixir raises here pin_outside_of_match
    # try to recover from error by dropping the pin and expanding arg
    expand(arg, s, e)
  end

  defp do_expand({:_, _meta, kind} = var, s, e) when is_atom(kind) do
    # elixir raises unbound_underscore if context is not match
    {var, s, e}
  end

  defp do_expand({name, meta, kind}, s, %{context: :match} = e)
       when is_atom(name) and is_atom(kind) do
    %{
      prematch: {_, _, prematch_version},
      unused: version,
      vars: {read, write}
    } = s

    pair = {name, var_context(meta, kind)}

    case read do
      # Variable was already overridden
      %{^pair => var_version} when var_version >= prematch_version ->
        new_write = write != false && Map.put(write, pair, version)
        var = {name, [{:version, var_version} | meta], kind}
        # it's a write but for simplicity treat it as read
        s = State.add_var_read(s, var)
        {var, %{s | vars: {read, new_write}, unused: version}, e}

      # Variable is being overridden now
      %{^pair => _} ->
        new_read = Map.put(read, pair, version)
        new_write = if write != false, do: Map.put(write, pair, version), else: write
        var = {name, [{:version, version} | meta], kind}
        s = State.add_var_write(s, var)
        {var, %{s | vars: {new_read, new_write}, unused: version + 1}, e}

      # Variable defined for the first time
      _ ->
        new_read = Map.put(read, pair, version)
        new_write = if write != false, do: Map.put(write, pair, version), else: write
        var = {name, [{:version, version} | meta], kind}
        s = State.add_var_write(s, var)
        {var, %{s | vars: {new_read, new_write}, unused: version + 1}, e}
    end
  end

  defp do_expand({name, meta, kind}, s, e) when is_atom(name) and is_atom(kind) do
    %{vars: {read, _write}, prematch: prematch} = s
    pair = {name, var_context(meta, kind)}

    result =
      case read do
        %{^pair => current_version} ->
          case prematch do
            {pre, _cycle, {_bitsize, original}} ->
              cond do
                Map.get(pre, pair) != current_version ->
                  {:ok, current_version}

                Map.has_key?(pre, pair) ->
                  # elixir plans to remove this case on 2.0
                  {:ok, current_version}

                not Map.has_key?(original, pair) ->
                  {:ok, current_version}

                true ->
                  :raise
              end

            _ ->
              {:ok, current_version}
          end

        _ ->
          # prematch

          case e do
            %{context: :guard} ->
              :raise

            %{} when s.prematch == :pin ->
              :pin

            _ ->
              # TODO
              :elixir_config.get(:on_undefined_variable)
          end
      end

    case result do
      {:ok, pair_version} ->
        var = {name, [{:version, pair_version} | meta], kind}
        s = State.add_var_read(s, var)
        {var, s, e}

      error ->
        case Keyword.fetch(meta, :if_undefined) do
          {:ok, :apply} ->
            # convert to local call
            expand({name, meta, []}, s, e)

          # elixir plans to remove this clause on v2.0
          {:ok, :raise} ->
            # elixir raises here undefined_var
            {{name, meta, kind}, s, e}

          # elixir plans to remove this clause on v2.0
          _ when error == :warn ->
            # convert to local call and add if_undefined meta
            expand({name, [{:if_undefined, :warn} | meta], []}, s, e)

          _ when error == :pin ->
            # elixir raises here undefined_var_pin
            {{name, meta, kind}, s, e}

          _ ->
            # elixir raises here undefined_var and attaches span meta
            {{name, meta, kind}, s, e}
        end
    end
  end

  # Local calls

  defp do_expand({fun, meta, args}, state, env)
       when is_atom(fun) and is_list(meta) and is_list(args) do
    # elixir checks here id fall is not ambiguous
    arity = length(args)

    # If we are inside a function, we support reading from locals.
    allow_locals = match?({n, a} when fun != n or arity != a, env.function)

    case NormalizedMacroEnv.expand_import(env, meta, fun, arity,
           trace: false,
           allow_locals: allow_locals,
           check_deprecations: false
         ) do
      {:macro, module, callback} ->
        # NOTE there is a subtle difference - callback will call expander with state derived from env via
        # :elixir_env.env_to_ex(env) possibly losing some details. Jose Valim is convinced this is not a problem
        state =
          state
          |> State.add_call_to_line({module, fun, length(args)}, meta)
          |> State.add_current_env_to_line(meta, env)

        expand_macro(meta, module, fun, args, callback, state, env)

      {:function, module, fun} ->
        {ar, af} =
          case __MODULE__.Rewrite.inline(module, fun, arity) do
            {ar, an} ->
              {ar, an}

            false ->
              {module, fun}
          end

        expand_remote(ar, meta, af, meta, args, state, State.prepare_write(state, env), env)

      {:error, :not_found} ->
        expand_local(meta, fun, args, state, env)

      {:error, {:conflict, _module}} ->
        # elixir raises here, expand args to look for cursor
        {_, state, _e} = expand_args(args, state, env)
        {{fun, meta, args}, state, env}

      {:error, {:ambiguous, _module}} ->
        # elixir raises here, expand args to look for cursor
        {_, state, _e} = expand_args(args, state, env)
        {{fun, meta, args}, state, env}
    end
  end

  # Remote call

  defp do_expand({{:., dot_meta, [module, fun]}, meta, args}, state, env)
       when (is_tuple(module) or is_atom(module)) and is_atom(fun) and is_list(meta) and
              is_list(args) do
    # dbg({module, fun, args})
    {module, state_l, env} = expand(module, State.prepare_write(state, env), env)
    arity = length(args)

    if is_atom(module) do
      case __MODULE__.Rewrite.inline(module, fun, arity) do
        {ar, an} ->
          expand_remote(ar, dot_meta, an, meta, args, state, state_l, env)

        false ->
          case NormalizedMacroEnv.expand_require(env, meta, module, fun, arity,
                 trace: false,
                 check_deprecations: false
               ) do
            {:macro, module, callback} ->
              # NOTE there is a subtle difference - callback will call expander with state derived from env via
              # :elixir_env.env_to_ex(env) possibly losing some details. Jose Valim is convinced this is not a problem
              state =
                state
                |> State.add_call_to_line({module, fun, length(args)}, meta)
                |> State.add_current_env_to_line(meta, env)

              expand_macro(meta, module, fun, args, callback, state, env)

            :error ->
              expand_remote(module, dot_meta, fun, meta, args, state, state_l, env)
          end
      end
    else
      expand_remote(module, dot_meta, fun, meta, args, state, state_l, env)
    end
  end

  # Anonymous calls

  defp do_expand({{:., dot_meta, [expr]}, meta, args}, s, e) when is_list(args) do
    {[e_expr | e_args], sa, ea} = expand_args([expr | args], s, e)

    # elixir validates if e_expr is not atom and raises invalid_function_call

    # for remote calls we emit position of right side of .
    # to make it consistent we shift dot position here
    dot_meta = dot_meta |> Keyword.put(:column_correction, 1)

    sa =
      sa
      |> State.add_call_to_line({nil, e_expr, length(e_args)}, dot_meta)
      |> State.add_current_env_to_line(meta, e)

    {{{:., dot_meta, [e_expr]}, meta, e_args}, sa, ea}
  end

  # Invalid calls

  defp do_expand({other, meta, args}, s, e) when is_list(meta) and is_list(args) do
    # elixir raises invalid_call, we may have cursor in other
    {other_exp, s, e} = expand(other, s, e)

    if other_exp != other do
      expand(other_exp, s, e)
    else
      {args, s, e} = expand_args(args, s, e)
      {{other, meta, args}, s, e}
    end
  end

  # Literals

  defp do_expand({left, right}, state, env) do
    {[e_left, e_right], state, env} = expand_args([left, right], state, env)
    {{e_left, e_right}, state, env}
  end

  defp do_expand(list, s, %{context: :match} = e) when is_list(list) do
    expand_list(list, &expand/3, s, e, [])
  end

  defp do_expand(list, s, e) when is_list(list) do
    {e_args, {se, _}, ee} =
      expand_list(list, &expand_arg/3, {State.prepare_write(s), s}, e, [])

    {e_args, State.close_write(se, s), ee}
  end

  defp do_expand(other, s, e) when is_number(other) or is_atom(other) or is_binary(other) do
    {other, s, e}
  end

  defp do_expand(function, s, e) when is_function(function) do
    type_info = :erlang.fun_info(function, :type)
    env_info = :erlang.fun_info(function, :env)

    case {type_info, env_info} do
      {{:type, :external}, {:env, []}} ->
        {__MODULE__.Quote.fun_to_quoted(function), s, e}

      _other ->
        # elixir raises here invalid_quoted_expr
        {nil, s, e}
    end
  end

  defp do_expand(pid, s, e) when is_pid(pid) do
    case e.function do
      nil ->
        {pid, s, e}

      _function ->
        # elixir plans to error here invalid_pid_in_function on 2.0
        {pid, s, e}
    end
  end

  defp do_expand(_other, s, e) do
    # elixir raises here invalid_quoted_expr
    {nil, s, e}
  end

  # Macro handling

  defp expand_macro(
         meta,
         Kernel,
         :defdelegate,
         [funs, opts],
         _callback,
         state,
         env = %{module: module}
       )
       when module != nil do
    {opts, state, env} = expand(opts, state, env)
    # elixir does validation here
    target = Keyword.get(opts, :to, :__unknown__)

    # TODO Remove List.wrap when multiple funs are no longer supported by elixir
    state =
      funs
      |> List.wrap()
      |> Enum.reduce(state, fn fun, state ->
        state_orig = state

        {fun, state, has_unquotes} =
          if __MODULE__.Quote.has_unquotes(fun) do
            state = State.new_vars_scope(state)
            # dynamic defdelegate - replace unquote expression with fake call
            case fun do
              {{:unquote, _, unquote_args}, meta, args} ->
                {_, state, _} = expand(unquote_args, state, env)
                {{:__unknown__, meta, args}, state, true}

              _ ->
                {fun, state, true}
            end
          else
            state = State.new_func_vars_scope(state)
            {fun, state, false}
          end

        {name, args, as, as_args} = __MODULE__.Utils.defdelegate_each(fun, opts)
        arity = length(args)

        # no need to reset versioned_vars - we never update it
        env_for_expand = %{env | function: {name, arity}}

        # expand defaults and pass args without defaults to expand_args
        {args_no_defaults, args, state} =
          expand_defaults(args, state, %{env_for_expand | context: nil}, [], [])

        # based on :elixir_clauses.def
        {e_args_no_defaults, state, _env_for_expand} =
          expand_args(args_no_defaults, %{state | prematch: {%{}, {%{}, []}, 0}}, %{
            env_for_expand
            | context: :match
          })

        args =
          Enum.zip(args, e_args_no_defaults)
          |> Enum.map(fn
            {{:"\\\\", meta, [_, expanded_default]}, expanded_arg} ->
              {:"\\\\", meta, [expanded_arg, expanded_default]}

            {_, expanded_arg} ->
              expanded_arg
          end)

        state =
          unless has_unquotes do
            # restore module vars
            State.remove_func_vars_scope(state, state_orig)
          else
            # remove scope
            State.remove_vars_scope(state, state_orig)
          end

        state
        |> State.add_current_env_to_line(meta, %{env | context: nil, function: {name, arity}})
        |> State.add_func_to_index(
          env,
          name,
          args,
          State.extract_range(meta),
          :defdelegate,
          target: {target, as, length(as_args)}
        )
      end)

    {[], state, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :@,
         [{:__cursor__, _meta, list} = arg],
         _callback,
         state,
         env
       )
       when is_list(list) do
    {arg, state, _env} = expand(arg, state, env)
    {{:@, meta, [arg]}, state, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :@,
         [{:behaviour, _meta, [arg]}],
         _callback,
         state,
         env = %{module: module}
       )
       when module != nil do
    state =
      state
      |> State.add_current_env_to_line(meta, env)

    {arg, state, env} = expand(arg, state, env)
    State.add_behaviour(arg, state, env)
  end

  defp expand_macro(
         meta,
         Kernel,
         :@,
         [{:moduledoc, doc_meta, [arg]}],
         _callback,
         state,
         env = %{module: module}
       )
       when module != nil do
    state =
      state
      |> State.add_current_env_to_line(meta, env)

    {arg, state, env} = expand(arg, state, env)

    state =
      state
      |> State.add_moduledoc_positions(
        env,
        meta
      )
      |> State.register_doc(env, :moduledoc, arg)

    {{:@, meta, [{:moduledoc, doc_meta, [arg]}]}, state, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :@,
         [{doc, doc_meta, [arg]}],
         _callback,
         state,
         env = %{module: module}
       )
       when doc in [:doc, :typedoc] and module != nil do
    state =
      state
      |> State.add_current_env_to_line(meta, env)

    {arg, state, env} = expand(arg, state, env)

    state =
      state
      |> State.register_doc(env, doc, arg)

    {{:@, meta, [{doc, doc_meta, [arg]}]}, state, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :@,
         [{:impl, doc_meta, [arg]}],
         _callback,
         state,
         env = %{module: module}
       )
       when module != nil do
    state =
      state
      |> State.add_current_env_to_line(meta, env)

    {arg, state, env} = expand(arg, state, env)

    # impl adds sets :hidden by default
    state =
      state
      |> State.register_doc(env, :doc, :impl)

    {{:@, meta, [{:impl, doc_meta, [arg]}]}, state, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :@,
         [{:optional_callbacks, doc_meta, [arg]}],
         _callback,
         state,
         env = %{module: module}
       )
       when module != nil do
    state =
      state
      |> State.add_current_env_to_line(meta, env)

    {arg, state, env} = expand(arg, state, env)

    state =
      state
      |> State.register_optional_callbacks(arg)

    {{:@, meta, [{:optional_callbacks, doc_meta, [arg]}]}, state, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :@,
         [{:deprecated, doc_meta, [arg]}],
         _callback,
         state,
         env = %{module: module}
       )
       when module != nil do
    state =
      state
      |> State.add_current_env_to_line(meta, env)

    {arg, state, env} = expand(arg, state, env)

    state =
      state
      |> State.register_doc(env, :doc, deprecated: arg)

    {{:@, meta, [{:deprecated, doc_meta, [arg]}]}, state, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :@,
         [{:derive, doc_meta, [derived_protos]}],
         _callback,
         state,
         env = %{module: module}
       )
       when module != nil do
    state =
      List.wrap(derived_protos)
      |> Enum.map(fn
        {proto, _opts} -> proto
        proto -> proto
      end)
      |> Enum.reduce(state, fn proto, acc ->
        case expand(proto, acc, env) do
          {proto_module, acc, _env} when is_atom(proto_module) ->
            # protocol implementation module for Any
            mod_any = Module.concat(proto_module, Any)

            # protocol implementation module built by @derive
            mod = Module.concat(proto_module, module)

            case acc.mods_funs_to_positions[{mod_any, nil, nil}] do
              nil ->
                # implementation for: Any not detected (is in other file etc.)
                acc
                |> State.add_module_to_index(mod, State.extract_range(meta), generated: true)

              _any_mods_funs ->
                # copy implementation for: Any
                copied_mods_funs_to_positions =
                  for {{module, fun, arity}, val} <- acc.mods_funs_to_positions,
                      module == mod_any,
                      into: %{},
                      do: {{mod, fun, arity}, val}

                %{
                  acc
                  | mods_funs_to_positions:
                      acc.mods_funs_to_positions |> Map.merge(copied_mods_funs_to_positions)
                }
            end

          _other ->
            acc
        end
      end)

    {{:@, meta, [{:derive, doc_meta, [derived_protos]}]}, state, env}
  end

  defp expand_macro(
         attr_meta,
         Kernel,
         :@,
         [{kind, kind_meta, [expr | _]}],
         _callback,
         state,
         env = %{module: module}
       )
       when kind in [:type, :typep, :opaque] and module != nil do
    cursor_before? = state.cursor_env != nil
    {expr, state, env} = __MODULE__.Typespec.expand_type(expr, state, env)

    {name, type_args} = __MODULE__.Typespec.type_to_signature(expr)
    type_args = type_args || []

    name =
      cond do
        name in [:required, :optional] ->
          # elixir raises here type #{name}/#{1} is a reserved type and it cannot be defined
          :"__#{name}__"

        __MODULE__.Typespec.built_in_type?(name, length(type_args)) ->
          # elixir raises here type #{name}/#{length(type_args)} is a built-in type and it cannot be redefined
          :"__#{name}__"

        true ->
          name
      end

    cursor_after? = state.cursor_env != nil

    spec = TypeInfo.typespec_to_string(kind, expr)

    state =
      state
      |> State.add_type(env, name, type_args, spec, kind, State.extract_range(attr_meta))
      |> State.with_typespec({name, length(type_args)})
      |> State.add_current_env_to_line(attr_meta, env)
      |> State.with_typespec(nil)

    state =
      if not cursor_before? and cursor_after? do
        {meta, env} = state.cursor_env
        env = %{env | typespec: {name, length(type_args)}}
        %{state | cursor_env: {meta, env}}
      else
        state
      end

    {{:@, attr_meta, [{kind, kind_meta, [expr]}]}, state, env}
  end

  defp expand_macro(
         attr_meta,
         Kernel,
         :@,
         [{kind, kind_meta, [expr | _]}],
         _callback,
         state,
         env = %{module: module}
       )
       when kind in [:callback, :macrocallback, :spec] and module != nil do
    cursor_before? = state.cursor_env != nil
    {expr, state, env} = __MODULE__.Typespec.expand_spec(expr, state, env)

    {name, type_args} = __MODULE__.Typespec.spec_to_signature(expr)
    cursor_after? = state.cursor_env != nil
    spec = TypeInfo.typespec_to_string(kind, expr)

    range = State.extract_range(attr_meta)

    state =
      if kind in [:callback, :macrocallback] do
        state
        |> State.add_func_to_index(
          env,
          :behaviour_info,
          [{:atom, attr_meta, nil}],
          range,
          :def,
          generated: true
        )
      else
        state
      end

    type_args = type_args || []

    state =
      state
      |> State.add_spec(env, name, type_args, spec, kind, range)
      |> State.with_typespec({name, length(type_args)})
      |> State.add_current_env_to_line(attr_meta, env)
      |> State.with_typespec(nil)

    state =
      if not cursor_before? and cursor_after? do
        {meta, env} = state.cursor_env
        env = %{env | typespec: {name, length(type_args)}}
        %{state | cursor_env: {meta, env}}
      else
        state
      end

    {{:@, attr_meta, [{kind, kind_meta, [expr]}]}, state, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :@,
         [{name, name_meta, args}],
         _callback,
         state,
         env = %{module: module}
       )
       when is_atom(name) and module != nil do
    {is_definition, {e_args, state, env}} =
      case args do
        arg when is_atom(arg) ->
          # @attribute
          {false, {nil, state, env}}

        [] ->
          # deprecated @attribute()
          {false, {nil, state, env}}

        [_] ->
          # @attribute(arg)
          # elixir validates env.function is nil
          # elixir forbids behavior name
          {true, expand_args(args, state, env)}

        args ->
          # elixir raises "invalid @ call #{inspect(args)}"
          {e_args, state, env} = expand_args(args, state, env)
          {true, {[hd(e_args)], state, env}}
      end

    inferred_type =
      case e_args do
        nil -> nil
        [arg] -> TypeInference.type_of(arg, env.context)
      end

    state =
      state
      |> State.add_attribute(env, name, meta, e_args, inferred_type, is_definition)
      |> State.add_current_env_to_line(meta, env)

    {{:@, meta, [{name, name_meta, e_args}]}, state, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :defoverridable,
         [arg],
         _callback,
         state,
         env = %{module: module}
       )
       when module != nil do
    {arg, state, env} = expand(arg, state, env)

    case arg do
      keyword when is_list(keyword) ->
        {nil, State.make_overridable(state, env, keyword, meta[:context]), env}

      behaviour_module when is_atom(behaviour_module) ->
        if Code.ensure_loaded?(behaviour_module) and
             function_exported?(behaviour_module, :behaviour_info, 1) do
          keyword =
            behaviour_module.behaviour_info(:callbacks)
            |> Enum.map(&Introspection.drop_macro_prefix/1)

          {nil, State.make_overridable(state, env, keyword, meta[:context]), env}
        else
          {nil, state, env}
        end

      _ ->
        {nil, state, env}
    end
  end

  defp expand_macro(
         meta,
         Kernel,
         type,
         [fields],
         _callback,
         state,
         env = %{module: module}
       )
       when type in [:defstruct, :defexception] and module != nil do
    if Map.has_key?(state.structs, module) do
      raise ArgumentError,
            "defstruct has already been called for " <>
              "#{inspect(module)}, defstruct can only be called once per module"
    end

    {fields, state, env} = expand(fields, state, env)

    fields =
      case fields do
        fs when is_list(fs) ->
          fs

        _other ->
          # elixir raises ArgumentError here
          []
      end

    fields =
      fields
      |> Enum.filter(fn
        field when is_atom(field) -> true
        {field, _} when is_atom(field) -> true
        _ -> false
      end)
      |> Enum.map(fn
        field when is_atom(field) -> {field, nil}
        {field, value} when is_atom(field) -> {field, value}
      end)

    state =
      state
      |> State.add_struct_or_exception(env, type, fields, State.extract_range(meta))

    {{type, meta, [fields]}, state, env}
  end

  defp expand_macro(
         meta,
         Record,
         call,
         [_name, _fields] = args,
         _callback,
         state,
         env = %{module: module}
       )
       when call in [:defrecord, :defrecordp] and module != nil do
    range = State.extract_range(meta)
    {[name, fields] = args, state, env} = expand(args, state, env)

    fields =
      if Keyword.keyword?(fields) do
        fields
      else
        []
      end

    type =
      case call do
        :defrecord -> :defmacro
        :defrecordp -> :defmacrop
      end

    options = [generated: true]

    state =
      state
      |> State.add_func_to_index(
        env,
        name,
        [{:\\, [], [{:args, [], nil}, []]}],
        range,
        type,
        options
      )
      |> State.add_func_to_index(
        env,
        name,
        [{:record, [], nil}, {:args, [], nil}],
        range,
        type,
        options
      )
      |> State.add_record(env, call, name, fields)
      |> State.add_current_env_to_line(meta, env)

    {{{:., meta, [Record, call]}, meta, args}, state, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :defprotocol,
         [_alias, [do: _block]] = args,
         callback,
         state,
         env
       ) do
    original_env = env
    # expand the macro normally
    {ast, state, env} =
      expand_macro_callback!(meta, Kernel, :defprotocol, args, callback, state, env)

    [module] = env.context_modules -- original_env.context_modules
    # add behaviour_info builtin
    # generate callbacks as macro expansion currently fails
    state =
      state
      |> State.add_func_to_index(
        %{env | module: module},
        :behaviour_info,
        [:atom],
        State.extract_range(meta),
        :def,
        generated: true
      )
      |> State.generate_protocol_callbacks(%{env | module: module})

    {ast, state, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :defimpl,
         [name, do_block],
         callback,
         state,
         env
       ) do
    expand_macro(
      meta,
      Kernel,
      :defimpl,
      [name, [], do_block],
      callback,
      state,
      env
    )
  end

  defp expand_macro(
         meta,
         Kernel,
         :defimpl,
         [name, opts, do_block],
         callback,
         state,
         env
       ) do
    opts = Keyword.merge(opts, do_block)

    {for, opts} =
      Keyword.pop_lazy(opts, :for, fn ->
        env.module ||
          raise ArgumentError, "defimpl/3 expects a :for option when declared outside a module"
      end)

    for =
      __MODULE__.Macro.expand_literals(for, %{
        env
        | module: env.module || Elixir,
          function: {:__impl__, 1}
      })

    {for, state} =
      if is_atom(for) or (is_list(for) and Enum.all?(for, &is_atom/1)) do
        {for, state}
      else
        {_, state, _} = expand(for, state, env)
        {:"Elixir.__Unknown__", state}
      end

    {protocol, state, _env} = expand(name, state, env)

    impl = fn protocol, for, block, state, env ->
      name = Module.concat(protocol, for)

      expand_macro(
        meta,
        Kernel,
        :defmodule,
        [name, [do: block]],
        callback,
        state,
        env
      )
    end

    block =
      case opts do
        [] ->
          # elixir raises here
          nil

        [do: block] ->
          block

        _ ->
          raise ArgumentError, "unknown options given to defimpl, got: #{Macro.to_string(opts)}"
      end

    for_wrapped =
      for
      |> List.wrap()

    {ast, state, env} =
      for_wrapped
      |> Enum.reduce({[], state, env}, fn for, {acc, state, env} ->
        {ast, state, env} =
          impl.(protocol, for, block, %{state | protocol: {protocol, for_wrapped}}, env)

        {[ast | acc], state, env}
      end)

    {Enum.reverse(ast), %{state | protocol: nil}, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :defmodule,
         [alias, [do: block]] = _args,
         _callback,
         state,
         env
       ) do
    state_orig = state
    original_env = env

    {expanded, _state, _env} = expand(alias, state, env)

    {full, env} =
      if is_atom(expanded) do
        alias_defmodule(alias, expanded, env)
      else
        # elixir raises here
        {:"Elixir.__Unknown__", env}
      end

    # elixir emits a special require directive with :defined key set in meta
    # require expand does alias, updates context_modules and runtime_modules
    # we do it here instead

    env = %{env | context_modules: [full | env.context_modules]}

    state =
      case original_env do
        %{function: nil} ->
          state

        _ ->
          %{state | runtime_modules: [full | state.runtime_modules]}
      end

    range = State.extract_range(meta)

    module_functions =
      case state.protocol do
        nil -> []
        _ -> [{:__impl__, [:atom], :def}]
      end

    state =
      state
      |> State.add_module_to_index(full, range, [])
      |> State.add_module()
      |> State.add_current_env_to_line(meta, %{env | module: full})
      |> State.add_module_functions(%{env | module: full}, module_functions, range)
      |> State.new_vars_scope()
      |> State.new_attributes_scope()

    {state, _env} = State.maybe_add_protocol_behaviour(state, %{env | module: full})

    {_result, state, e_env} = expand(block, state, %{env | module: full})

    # here we handle module callbacks. Only before_compile macro callbacks are expanded as they
    # affect module body. Func before_compile callbacks are not executed. after_compile and after_verify
    # are not executed as we do not preform a real compilation
    {state, _e_env} =
      for args <- Map.get(state.attribute_store, {full, :before_compile}, []) do
        case args do
          {module, fun} -> [module, fun]
          module -> [module, :__before_compile__]
        end
      end
      |> Enum.reduce({state, e_env}, fn target, {state, env} ->
        # module vars are not accessible in module callbacks
        env = %{env | versioned_vars: %{}, line: meta[:line]}
        state_orig = state
        state = State.new_func_vars_scope(state)

        # elixir dispatches callbacks by raw dispatch and eval_forms
        # instead we expand a bock with require and possibly expand macros
        # we do not attempt to exec function callbacks
        ast =
          {:__block__, [],
           [
             {:require, [], [hd(target)]},
             {{:., [], target}, [], [env]}
           ]}

        {_result, state, env} = expand(ast, state, env)
        {State.remove_func_vars_scope(state, state_orig), env}
      end)

    # restore vars from outer scope
    # restore version counter
    state =
      state
      |> State.apply_optional_callbacks(%{env | module: full})
      |> State.remove_vars_scope(state_orig, true)
      |> State.remove_attributes_scope()
      |> State.remove_module()

    # in elixir the result of defmodule expansion is
    # require (a module atom) and :elixir_module.compile dot call in block
    # we don't need that

    {{:__block__, [], []}, state, env}
  end

  defp expand_macro(
         meta,
         Protocol,
         :def,
         [{name, _, _args = [_ | _]} = call],
         callback,
         state,
         env
       )
       when is_atom(name) do
    # transform protocol def to def with empty body
    {ast, state, env} =
      expand_macro(meta, Kernel, :def, [call, nil], callback, state, env)

    {ast, state, env}
  end

  defp expand_macro(meta, Kernel, def_kind, [call], callback, state, env)
       when def_kind in [:def, :defp, :defmacro, :defmacrop, :defguard, :defguardp] do
    # transform guard and function head to def with empty body
    expand_macro(meta, Kernel, def_kind, [call, nil], callback, state, env)
  end

  defp expand_macro(
         meta,
         Kernel,
         def_kind,
         [call, expr],
         _callback,
         state,
         env = %{module: module}
       )
       when module != nil and
              def_kind in [:def, :defp, :defmacro, :defmacrop, :defguard, :defguardp] do
    state =
      case call do
        {:__cursor__, _, list} when is_list(list) ->
          {_, state, _} = expand(call, state, %{env | function: {:__unknown__, 0}})
          state

        _ ->
          state
      end

    state_orig = state

    unquoted_call = __MODULE__.Quote.has_unquotes(call)
    unquoted_expr = __MODULE__.Quote.has_unquotes(expr)
    has_unquotes = unquoted_call or unquoted_expr

    # if there are unquote fragments in either call or body elixir escapes both and evaluates
    # if unquoted_expr or unquoted_call, do: __MODULE__.Quote.escape({call, expr}, :none, true)
    # instead we try to expand the call and body ignoring the unquotes
    # 

    {name_and_args, guards} = __MODULE__.Utils.extract_guards(call)

    # elixir raises here if def is invalid, we try to continue with unknown
    # especially, we return unknown for calls with unquote fragments
    {{name, _meta_1, args}, state} =
      case name_and_args do
        {n, m, a} when is_atom(n) and is_atom(a) ->
          {{n, m, []}, state}

        {n, m, a} when is_atom(n) and is_list(a) ->
          {{n, m, a}, state}

        {{:unquote, _, unquote_args}, m, a} when is_atom(a) ->
          {_, state, _} = expand(unquote_args, state, env)
          {{:__unknown__, m, []}, state}

        {{:unquote, _, unquote_args}, m, a} when is_list(a) ->
          {_, state, _} = expand(unquote_args, state, env)
          {{:__unknown__, m, a}, state}

        {_n, m, a} when is_atom(a) ->
          {{:__unknown__, m, []}, state}

        {_n, m, a} when is_list(a) ->
          {{:__unknown__, m, a}, state}

        _ ->
          {{:__unknown__, [], []}, state}
      end

    arity = length(args)

    # based on :elixir_def.env_for_expansion
    state =
      unless has_unquotes do
        # module vars are not accessible in def body
        %{
          state
          | caller: def_kind in [:defmacro, :defmacrop, :defguard, :defguardp]
        }
        |> State.new_func_vars_scope()
      else
        # make module variables accessible if there are unquote fragments in def body
        %{
          state
          | caller: def_kind in [:defmacro, :defmacrop, :defguard, :defguardp]
        }
        |> State.new_vars_scope()
      end

    # no need to reset versioned_vars - we never update it
    env_for_expand = %{env | function: {name, arity}}

    # expand defaults and pass args without defaults to expand_args
    {args_no_defaults, args, state} =
      expand_defaults(args, state, %{env_for_expand | context: nil}, [], [])

    # based on :elixir_clauses.def
    {e_args_no_defaults, state, env_for_expand} =
      expand_args(args_no_defaults, %{state | prematch: {%{}, {%{}, []}, 0}}, %{
        env_for_expand
        | context: :match
      })

    # elixir calls validate_cycles here

    args =
      Enum.zip(args, e_args_no_defaults)
      |> Enum.map(fn
        {{:"\\\\", meta, [_, expanded_default]}, expanded_arg} ->
          {:"\\\\", meta, [expanded_arg, expanded_default]}

        {_, expanded_arg} ->
          expanded_arg
      end)

    prematch =
      if Version.match?(System.version(), ">= 1.15.0-dev") do
        if Version.match?(System.version(), ">= 1.18.0-dev") do
          :none
        else
          Code.get_compiler_option(:on_undefined_variable)
        end
      else
        :warn
      end

    {e_guard, state, env_for_expand} =
      __MODULE__.Clauses.guard(
        guards,
        %{state | prematch: prematch},
        %{env_for_expand | context: :guard}
      )

    type_info = Guard.type_information_from_guards(e_guard)

    state = State.merge_inferred_types(state, type_info)

    env_for_expand = %{env_for_expand | context: nil}

    state =
      state
      |> State.add_current_env_to_line(meta, env_for_expand)
      |> State.add_func_to_index(
        env,
        name,
        args,
        State.extract_range(meta),
        def_kind
      )

    expr =
      case expr do
        nil ->
          # function head
          nil

        [do: do_block] ->
          # do block only
          do_block

        _ ->
          if is_list(expr) and Keyword.has_key?(expr, :do) do
            # do block with receive/catch/else/after
            # wrap in try
            # NOTE origin kind may be not correct here but origin is not used and
            # elixir uses it only for error messages in elixir_clauses module
            {:try, [{:origin, def_kind} | meta], [expr]}
          else
            # elixir raises here
            expr
          end
      end

    {_e_body, state, _env_for_expand} =
      expand(expr, state, env_for_expand)

    # restore vars from outer scope
    state =
      %{state | caller: false}

    state =
      unless has_unquotes do
        # restore module vars
        State.remove_func_vars_scope(state, state_orig)
      else
        # remove scope
        State.remove_vars_scope(state, state_orig)
      end

    # result of def expansion is fa tuple
    {{name, arity}, state, env}
  end

  defp expand_macro(
         meta,
         ExUnit.Case,
         :test,
         [name | rest],
         callback,
         state,
         env = %{module: module}
       )
       when module != nil and is_binary(name) do
    {args, do_block} =
      case rest do
        [] -> {[{:_, [], nil}], [do: {:__block__, [], []}]}
        [do_block] -> {[{:_, [], nil}], do_block}
        [context, do_block | _] -> {[context], do_block}
      end

    call = {ex_unit_test_name(state, name), meta, args}
    expand_macro(meta, Kernel, :def, [call, do_block], callback, state, env)
  end

  defp expand_macro(
         meta,
         ExUnit.Callbacks,
         setup,
         rest,
         callback,
         state,
         env = %{module: module}
       )
       when module != nil and setup in [:setup, :setup_all] do
    {args, do_block} =
      case rest do
        [] -> {[{:_, [], nil}], [do: {:__block__, [], []}]}
        [do_block] -> {[{:_, [], nil}], do_block}
        [context, do_block | _] -> {[context], do_block}
      end

    line = __MODULE__.Utils.get_line(meta)

    # NOTE this name is not 100% correct - ex_unit uses counters instead of line but it's too complicated
    call = {:"__ex_unit_#{setup}_#{line}", meta, args}
    expand_macro(meta, Kernel, :def, [call, do_block], callback, state, env)
  end

  defp expand_macro(
         _meta,
         ExUnit.Case,
         :describe,
         [name, [{:do, block}]],
         _callback,
         state,
         env = %{module: module}
       )
       when module != nil and is_binary(name) do
    state = %{state | ex_unit_describe: name}
    {ast, state, _env} = expand(block, state, env)
    state = %{state | ex_unit_describe: nil}
    {{:__block__, [], [ast]}, state, env}
  end

  defp expand_macro(meta, module, fun, args, callback, state, env) do
    expand_macro_callback(meta, module, fun, args, callback, state, env)
  end

  defp expand_macro_callback(meta, module, fun, args, callback, state, env) do
    # dbg({module, fun, args})
    try do
      callback.(meta, args)
    catch
      # If expanding the macro fails, we just give up.
      kind, payload ->
        Logger.warning(Exception.format(kind, payload, __STACKTRACE__))
        # look for cursor in args
        {_ast, state, _env} = expand(args, state, env)

        {{{:., meta, [module, fun]}, meta, args}, state, env}
    else
      ast ->
        state =
          if __MODULE__.Utils.has_cursor?(args) and not __MODULE__.Utils.has_cursor?(ast) do
            # in case there was cursor in the original args but it's not present in macro result
            # expand a fake node
            {_ast, state, _env} = expand({:__cursor__, [], []}, state, env)
            state
          else
            state
          end

        {ast, state, env} = expand(ast, state, env)
        {ast, state, env}
    end
  end

  defp expand_macro_callback!(meta, _module, _fun, args, callback, state, env) do
    ast = callback.(meta, args)
    {ast, state, env} = expand(ast, state, env)
    {ast, state, env}
  end

  defp ex_unit_test_name(state, name) do
    case state.ex_unit_describe do
      nil -> "test #{name}"
      describe -> "test #{describe} #{name}"
    end
    |> String.to_atom()
  end

  defp expand_defaults([{:"\\\\", meta, [expr, default]} | args], s, e, acc_no_defaults, acc) do
    {expanded_default, se, _} = expand(default, s, e)

    expand_defaults(args, se, e, [expr | acc_no_defaults], [
      {:"\\\\", meta, [expr, expanded_default]} | acc
    ])
  end

  defp expand_defaults([arg | args], s, e, acc_no_defaults, acc),
    do: expand_defaults(args, s, e, [arg | acc_no_defaults], [arg | acc])

  defp expand_defaults([], s, _e, acc_no_defaults, acc),
    do: {Enum.reverse(acc_no_defaults), Enum.reverse(acc), s}

  # defmodule helpers
  # defmodule automatically defines aliases, we need to mirror this feature here.

  # defmodule Elixir.Alias
  if Version.match?(System.version(), "< 1.16.0-dev") do
    # see https://github.com/elixir-lang/elixir/pull/12451#issuecomment-1461393633
    defp alias_defmodule({:__aliases__, meta, [:"Elixir", t] = x}, module, env) do
      alias = String.to_atom("Elixir." <> Atom.to_string(t))
      {:ok, env} = NormalizedMacroEnv.define_alias(env, meta, alias, as: alias, trace: false)
      {module, env}
    end
  end

  defp alias_defmodule({:__aliases__, _, [:"Elixir", _ | _]}, module, env), do: {module, env}

  # defmodule Alias in root
  defp alias_defmodule({:__aliases__, _, _}, module, %{module: nil} = env),
    do: {module, env}

  # defmodule Alias nested
  defp alias_defmodule({:__aliases__, meta, [h | t]}, _module, env) when is_atom(h) do
    module = Module.concat([env.module, h])
    alias = String.to_atom("Elixir." <> Atom.to_string(h))
    {:ok, env} = NormalizedMacroEnv.define_alias(env, meta, module, as: alias, trace: false)

    case t do
      [] -> {module, env}
      _ -> {String.to_atom(Enum.join([module | t], ".")), env}
    end
  end

  # defmodule _
  defp alias_defmodule(_raw, module, env) do
    {module, env}
  end

  # Helpers

  defp expand_remote(receiver, dot_meta, right, meta, args, s, sl, %{context: context} = e)
       when is_atom(receiver) or is_tuple(receiver) do
    cond do
      context == :guard and is_tuple(receiver) ->
        # elixir raises parens_map_lookup unless no_parens is set in meta
        # look for cursor in discarded args
        {_ast, sl, _env} = expand(args, sl, e)

        sl =
          sl
          |> State.add_call_to_line({receiver, right, length(args)}, meta)
          |> State.add_current_env_to_line(meta, e)

        {{{:., dot_meta, [receiver, right]}, meta, []}, sl, e}

      context == nil ->
        attached_meta = attach_runtime_module(receiver, meta, s, e)
        {e_args, {sa, _}, ea} = map_fold(&expand_arg/3, {sl, s}, e, args)

        case __MODULE__.Rewrite.rewrite(
               context,
               receiver,
               dot_meta,
               right,
               attached_meta,
               e_args,
               s
             ) do
          {:ok, rewritten} ->
            s =
              State.close_write(sa, s)
              |> State.add_call_to_line({receiver, right, length(e_args)}, meta)
              |> State.add_current_env_to_line(meta, e)

            {rewritten, s, ea}

          {:error, _error} ->
            # elixir raises here elixir_rewrite
            s =
              State.close_write(sa, s)
              |> State.add_call_to_line({receiver, right, length(e_args)}, meta)
              |> State.add_current_env_to_line(meta, e)

            {{{:., dot_meta, [receiver, right]}, attached_meta, e_args}, s, ea}
        end

      true ->
        case {receiver, right, args} do
          {:erlang, :+, [arg]} when is_number(arg) ->
            {+arg, sl, e}

          {:erlang, :-, [arg]} when is_number(arg) ->
            {-arg, sl, e}

          _ ->
            {e_args, sa, ea} = map_fold(&expand/3, sl, e, args)

            case __MODULE__.Rewrite.rewrite(context, receiver, dot_meta, right, meta, e_args, s) do
              {:ok, rewritten} ->
                {rewritten, sa, ea}

              {:error, _error} ->
                # elixir raises here elixir_rewrite
                s =
                  sa
                  |> State.add_call_to_line({receiver, right, length(e_args)}, meta)
                  |> State.add_current_env_to_line(meta, e)

                {{{:., dot_meta, [receiver, right]}, meta, e_args}, s, ea}
            end
        end
    end
  end

  defp expand_remote(receiver, dot_meta, right, meta, args, s, sl, e) do
    # elixir raises here invalid_call
    {e_args, {sa, _}, ea} = map_fold(&expand_arg/3, {sl, s}, e, args)

    s =
      State.close_write(sa, s)
      |> State.add_call_to_line({receiver, right, length(e_args)}, meta)
      |> State.add_current_env_to_line(meta, e)

    {{{:., dot_meta, [receiver, right]}, meta, e_args}, s, ea}
  end

  defp attach_runtime_module(receiver, meta, s, _e) do
    if receiver in s.runtime_modules do
      [{:runtime_module, true} | meta]
    else
      meta
    end
  end

  defp expand_local(meta, :when, [_, _] = args, state, env = %{context: nil}) do
    # naked when, try to transform into a case
    ast =
      {:case, meta,
       [
         {:_, meta, nil},
         [
           do: [
             {:->, meta,
              [
                [
                  {:when, meta, args}
                ],
                :ok
              ]}
           ]
         ]
       ]}

    expand(ast, state, env)
  end

  defp expand_local(meta, fun, args, state, env) do
    # elixir check if there are no clauses
    # elixir raises here invalid_local_invocation if context is match or guard
    # elixir compiler raises here undefined_function if env.function is nil

    state =
      state
      |> State.add_call_to_line({nil, fun, length(args)}, meta)
      |> State.add_current_env_to_line(meta, env)

    {args, state, env} = expand_args(args, state, env)
    {{fun, meta, args}, state, env}
  end

  defp expand_opts(allowed, opts, s, e) do
    {e_opts, se, ee} = expand(opts, s, e)
    # safe to drop after expand
    e_opts = sanitize_opts(allowed, e_opts)
    {e_opts, se, ee}
  end

  defp no_alias_opts(opts) when is_list(opts) do
    case Keyword.fetch(opts, :as) do
      {:ok, as} -> Keyword.put(opts, :as, no_alias_expansion(as))
      :error -> opts
    end
  end

  defp no_alias_opts(opts), do: opts

  defp no_alias_expansion({:__aliases__, _, [h | t]} = _aliases) when is_atom(h) do
    Module.concat([h | t])
  end

  defp no_alias_expansion(other), do: other

  defp expand_list([{:|, meta, args} = _head], fun, s, e, list) do
    {e_args, s_acc, e_acc} = map_fold(fun, s, e, args)
    expand_list([], fun, s_acc, e_acc, [{:|, meta, e_args} | list])
  end

  defp expand_list([h | t], fun, s, e, list) do
    {e_arg, s_acc, e_acc} = fun.(h, s, e)
    expand_list(t, fun, s_acc, e_acc, [e_arg | list])
  end

  defp expand_list([], _fun, s, e, list) do
    {Enum.reverse(list), s, e}
  end

  defp expand_block([], acc, _meta, s, e), do: {Enum.reverse(acc), s, e}

  defp expand_block([h], acc, meta, s, e) do
    # s = s |> State.add_current_env_to_line(meta, e)
    {eh, se, ee} = expand(h, s, e)
    expand_block([], [eh | acc], meta, se, ee)
  end

  defp expand_block([{:for, _, [_ | _]} = h | t], acc, meta, s, e) do
    {eh, se, ee} = expand_for(h, s, e, false)
    {eh, se, ee} = expand_block(t, [eh | acc], meta, se, ee)
    {eh, se, ee}
  end

  defp expand_block([{:=, _, [{:_, _, ctx}, {:for, _, [_ | _]} = h]} | t], acc, meta, s, e)
       when is_atom(ctx) do
    {eh, se, ee} = expand_for(h, s, e, false)
    expand_block(t, [eh | acc], meta, se, ee)
  end

  defp expand_block([h | t], acc, meta, s, e) do
    # s = s |> State.add_current_env_to_line(meta, e)
    {eh, se, ee} = expand(h, s, e)
    expand_block(t, [eh | acc], meta, se, ee)
  end

  # defp expand_quote(ast, state, env) do
  #   {_, {state, env}} =
  #     Macro.prewalk(ast, {state, env}, fn
  #       # We need to traverse inside unquotes
  #       {unquote, _, [expr]}, {state, env} when unquote in [:unquote, :unquote_splicing] ->
  #         {_expr, state, env} = expand(expr, state, env)
  #         {:ok, {state, env}}

  #       # If we find a quote inside a quote, we stop traversing it
  #       {:quote, _, [_]}, acc ->
  #         {:ok, acc}

  #       {:quote, _, [_, _]}, acc ->
  #         {:ok, acc}

  #       # Otherwise we go on
  #       node, acc ->
  #         {node, acc}
  #     end)

  #   {ast, state, env}
  # end

  defp expand_multi_alias_call(kind, meta, base, refs, opts, state, env) do
    {base_ref, state, env} = expand(base, state, env)

    fun = fn
      {:__aliases__, _, ref}, state, env ->
        expand({kind, meta, [Module.concat([base_ref | ref]), opts]}, state, env)

      ref, state, env when is_atom(ref) ->
        expand({kind, meta, [Module.concat([base_ref, ref]), opts]}, state, env)

      other, s, e ->
        # elixir raises here
        # expected_compile_time_module
        # we search for cursor
        {_, s, _} = expand(other, s, e)
        {other, s, e}
    end

    map_fold(fun, state, env, refs)
  end

  defp overridable_name(name, count) when is_integer(count), do: :"#{name} (overridable #{count})"

  defp resolve_super(_meta, _arity, _state, %{module: module, function: function})
       when module == nil or function == nil do
    # elixir asserts scope is function
    nil
  end

  defp resolve_super(_meta, arity, state, %{module: module, function: function}) do
    case function do
      {name, ^arity} ->
        state.mods_funs_to_positions

        case state.mods_funs_to_positions[{module, name, arity}] do
          %ModFunInfo{overridable: {true, _}} = info ->
            kind =
              case info.type do
                :defdelegate -> :def
                :defguard -> :defmacro
                :defguardp -> :defmacrop
                other -> other
              end

            hidden = Map.get(info.meta, :hidden, false)
            # def meta is not used anyway so let's pass empty
            meta = []
            # we hardcode count to 1
            count = 1

            case hidden do
              false ->
                {kind, name, meta}

              true when kind in [:defmacro, :defmacrop] ->
                {:defmacrop, overridable_name(name, count), meta}

              true ->
                {:defp, overridable_name(name, count), meta}
            end

          _ ->
            # elixir raises here no_super
            nil
        end

      _ ->
        # elixir raises here wrong_number_of_args_for_super
        nil
    end
  end

  defp expand_fn_capture(meta, arg, s, e) do
    case __MODULE__.Fn.capture(meta, arg, s, e) do
      {{:remote, remote, fun, arity}, require_meta, dot_meta, se, ee} ->
        attached_meta = attach_runtime_module(remote, require_meta, s, e)

        se =
          se
          |> State.add_call_to_line({remote, fun, arity}, attached_meta)
          |> State.add_current_env_to_line(attached_meta, ee)

        {{:&, meta, [{:/, [], [{{:., dot_meta, [remote, fun]}, attached_meta, []}, arity]}]}, se,
         ee}

      {{:local, fun, arity}, local_meta, _, se, ee} ->
        # elixir raises undefined_local_capture if ee.function is nil

        se =
          se
          |> State.add_call_to_line({nil, fun, arity}, local_meta)
          |> State.add_current_env_to_line(local_meta, ee)

        {{:&, meta, [{:/, [], [{fun, local_meta, nil}, arity]}]}, se, ee}

      {:expand, expr, se, ee} ->
        expand(expr, se, ee)
    end
  end

  defp expand_for({:for, meta, [_ | _] = args}, s, e, return) do
    {cases, block} = __MODULE__.Utils.split_opts(args)

    {expr, opts} =
      case Keyword.pop(block, :do) do
        {nil, do_opts} ->
          # elixir raises missing_option here
          {[], do_opts}

        {do_expr, do_opts} ->
          {do_expr, do_opts}
      end

    {e_opts, so, eo} = expand(opts, State.new_vars_scope(s), e)
    {e_cases, sc, ec} = map_fold(&expand_for_generator/3, so, eo, cases)
    # elixir raises here for_generator_start on invalid start generator

    # safe to drop after expand
    e_opts = sanitize_opts([:into, :uniq, :reduce], e_opts)

    {maybe_reduce, normalized_opts} =
      sanitize_for_options(e_opts, false, false, false, return, meta, e, [])

    {e_expr, se, _ee} = expand_for_do_block(expr, sc, ec, maybe_reduce)

    {{:for, meta, e_cases ++ [[{:do, e_expr} | normalized_opts]]}, State.remove_vars_scope(se, s),
     e}
  end

  defp expand_for_do_block([{:->, _, _} | _] = clauses, s, e, false) do
    # elixir raises here for_without_reduce_bad_block
    # try to recover from error by emitting fake reduce
    expand_for_do_block(clauses, s, e, {:reduce, []})
  end

  defp expand_for_do_block(expr, s, e, false), do: expand(expr, s, e)

  defp expand_for_do_block([{:->, _, _} | _] = clauses, s, e, {:reduce, _}) do
    transformer = fn
      {:->, clause_meta, [args, right]}, sa ->
        # elixir checks here that clause has exactly 1 arg by matching against {_, _, [[_], _]}
        # we drop excessive or generate a fake arg

        {args, discarded_args} =
          case args do
            [] ->
              {[{:_, [], e.module}], []}

            [{:when, meta, [head | rest]}] ->
              [last | rest_reversed] = Enum.reverse(rest)
              {[{:when, meta, [head, last]}], Enum.reverse(rest_reversed)}

            [head | rest] ->
              {[head], rest}
          end

        # check if there is cursor in dropped arg
        {_ast, sa, _e} = expand(discarded_args, sa, e)

        clause = {:->, clause_meta, [args, right]}
        s_reset = State.new_vars_scope(sa)

        # no point in doing type inference here, we are only certain of the initial value of the accumulator
        {e_clause, s_acc, _e_acc} =
          __MODULE__.Clauses.clause(&__MODULE__.Clauses.head/3, clause, s_reset, e)

        {e_clause, State.remove_vars_scope(s_acc, sa)}
    end

    {do_expr, sa} = Enum.map_reduce(clauses, s, transformer)
    {do_expr, sa, e}
  end

  defp expand_for_do_block(expr, s, e, {:reduce, _} = reduce) do
    # elixir raises here for_with_reduce_bad_block
    case expr do
      [] ->
        # try to recover from error by emitting a fake clause
        expand_for_do_block([{:->, [], [[{:_, [], e.module}], :ok]}], s, e, reduce)

      _ ->
        # try to recover from error by wrapping the expression in clause
        expand_for_do_block([{:->, [], [[expr], :ok]}], s, e, reduce)
    end
  end

  defp expand_for_generator({:<-, meta, [left, right]}, s, e) do
    {e_right, sr, er} = expand(right, s, e)
    sm = State.reset_read(sr, s)
    {[e_left], sl, el} = __MODULE__.Clauses.head([left], sm, er)

    match_context_r = TypeInference.type_of(e_right, e.context)

    vars_l_with_inferred_types =
      TypeInference.find_typed_vars(e_left, {:for_expression, match_context_r}, :match)

    sl = State.merge_inferred_types(sl, vars_l_with_inferred_types)

    {{:<-, meta, [e_left, e_right]}, sl, el}
  end

  defp expand_for_generator({:<<>>, meta, args} = x, s, e) when is_list(args) do
    case __MODULE__.Utils.split_last(args) do
      {left_start, {:<-, op_meta, [left_end, right]}} ->
        {e_right, sr, er} = expand(right, s, e)
        sm = State.reset_read(sr, s)

        {e_left, sl, el} =
          __MODULE__.Clauses.match(
            fn barg, bs, be ->
              __MODULE__.Bitstring.expand(meta, barg, bs, be, true)
            end,
            left_start ++ [left_end],
            sm,
            sm,
            er
          )

        # no point in doing type inference here, we're only going to find integers and binaries

        {{:<<>>, meta, [{:<-, op_meta, [e_left, e_right]}]}, sl, el}

      _ ->
        expand(x, s, e)
    end
  end

  defp expand_for_generator(x, s, e) do
    {x, s, e} = expand(x, s, e)
    {x, s, e}
  end

  defp sanitize_for_options([{:into, _} = pair | opts], _into, uniq, reduce, return, meta, e, acc) do
    sanitize_for_options(opts, pair, uniq, reduce, return, meta, e, [pair | acc])
  end

  defp sanitize_for_options(
         [{:uniq, _} = pair | opts],
         into,
         _uniq,
         reduce,
         return,
         meta,
         e,
         acc
       ) do
    # elixir checks if uniq value is boolean
    # we do not care - there may be cursor in it
    sanitize_for_options(opts, into, pair, reduce, return, meta, e, [pair | acc])
  end

  defp sanitize_for_options(
         [{:reduce, _} = pair | opts],
         into,
         uniq,
         _reduce,
         return,
         meta,
         e,
         acc
       ) do
    # elixir raises for_conflicting_reduce_into_uniq when reduce, uniq and true is enabled
    sanitize_for_options(opts, into, uniq, pair, return, meta, e, [pair | acc])
  end

  defp sanitize_for_options([], false, uniq, false, true, meta, e, acc) do
    pair = {:into, []}
    sanitize_for_options([pair], pair, uniq, false, true, meta, e, acc)
  end

  defp sanitize_for_options([], false, {:uniq, true}, false, false, meta, e, acc) do
    # safe to drop here even if there's a cursor options are already expanded
    acc_without_uniq = Keyword.delete(acc, :uniq)
    sanitize_for_options([], false, false, false, false, meta, e, acc_without_uniq)
  end

  defp sanitize_for_options([], _into, _uniq, reduce, _return, _meta, _e, acc) do
    {reduce, Enum.reverse(acc)}
  end

  defp sanitize_opts(allowed, opts) when is_list(opts) do
    for {key, value} <- opts, Enum.member?(allowed, key), do: {key, value}
  end

  defp sanitize_opts(_allowed, _opts), do: []

  defp escape_env_entries(meta, %{vars: {read, _}}, env) do
    env =
      case env.function do
        nil -> env
        _ -> %{env | lexical_tracker: nil, tracers: []}
      end

    %{env | versioned_vars: escape_map(read), line: __MODULE__.Utils.get_line(meta)}
  end

  defp escape_map(map) do
    {:%{}, [], Enum.sort(Map.to_list(map))}
  end

  defp map_fold(fun, s, e, list), do: map_fold(fun, s, e, list, [])

  defp map_fold(fun, s, e, [h | t], acc) do
    {rh, rs, re} = fun.(h, s, e)
    map_fold(fun, rs, re, t, [rh | acc])
  end

  defp map_fold(_fun, s, e, [], acc), do: {Enum.reverse(acc), s, e}

  defp var_context(meta, kind) do
    case Keyword.fetch(meta, :counter) do
      {:ok, counter} -> counter
      :error -> kind
    end
  end

  defp expand_case(meta, expr, opts, s, e) do
    {e_expr, se, ee} = expand(expr, s, e)

    r_opts =
      if Keyword.get(meta, :optimize_boolean, false) and :elixir_utils.returns_boolean(e_expr) do
        rewrite_case_clauses(opts)
      else
        opts
      end

    {e_opts, so, eo} = __MODULE__.Clauses.case(e_expr, r_opts, se, ee)
    {{:case, meta, [e_expr, e_opts]}, so, eo}
  end

  def rewrite_case_clauses(
        do: [
          {:->, false_meta,
           [
             [{:when, _, [var, {{:., _, [Kernel, :in]}, _, [var, [false, nil]]}]}],
             false_expr
           ]},
          {:->, true_meta,
           [
             [{:_, _, _}],
             true_expr
           ]}
        ]
      ) do
    rewrite_case_clauses(false_meta, false_expr, true_meta, true_expr)
  end

  def rewrite_case_clauses(
        do: [
          {:->, false_meta, [[false], false_expr]},
          {:->, true_meta, [[true], true_expr]} | _
        ]
      ) do
    rewrite_case_clauses(false_meta, false_expr, true_meta, true_expr)
  end

  def rewrite_case_clauses(opts), do: opts

  defp rewrite_case_clauses(false_meta, false_expr, true_meta, true_expr) do
    [
      do: [
        {:->, false_meta, [[false], false_expr]},
        {:->, true_meta, [[true], true_expr]}
      ]
    ]
  end

  def expand_arg(arg, acc, e)
      when is_number(arg) or is_atom(arg) or is_binary(arg) or is_pid(arg) do
    {arg, acc, e}
  end

  def expand_arg(arg, {acc, s}, e) do
    {e_arg, s_acc, e_acc} = expand(arg, State.reset_read(acc, s), e)
    {e_arg, {s_acc, s}, e_acc}
  end

  def expand_args([arg], s, e) do
    {e_arg, se, ee} = expand(arg, s, e)
    {[e_arg], se, ee}
  end

  def expand_args(args, s, %{context: :match} = e) do
    map_fold(&expand/3, s, e, args)
  end

  def expand_args(args, s, e) do
    {e_args, {sa, _}, ea} = map_fold(&expand_arg/3, {State.prepare_write(s), s}, e, args)
    {e_args, State.close_write(sa, s), ea}
  end

  if Version.match?(System.version(), ">= 1.15.0-dev") do
    @internals [{:behaviour_info, 1}, {:module_info, 1}, {:module_info, 0}]
  else
    @internals [{:module_info, 1}, {:module_info, 0}]
  end

  defp import_info_callback(module, state) do
    fn kind ->
      if Map.has_key?(state.mods_funs_to_positions, {module, nil, nil}) do
        category = if kind == :functions, do: :function, else: :macro

        for {{^module, fun, arity}, info} when fun != nil <- state.mods_funs_to_positions,
            {fun, arity} not in @internals,
            ModFunInfo.get_category(info) == category,
            not ModFunInfo.private?(info) do
          {fun, arity}
        end
      else
        # this branch is based on implementation in :elixir_import
        if Code.ensure_loaded?(module) do
          try do
            module.__info__(kind)
          rescue
            UndefinedFunctionError ->
              if kind == :functions do
                module.module_info(:exports) -- @internals
              else
                []
              end
          end
        else
          []
        end
      end
    end
  end
end
