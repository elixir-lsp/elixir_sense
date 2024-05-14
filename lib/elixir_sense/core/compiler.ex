defmodule ElixirSense.Core.Compiler do
  import ElixirSense.Core.State, except: [expand: 2, expand: 3, no_alias_expansion: 1]
  alias ElixirSense.Core.State
  require Logger
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.TypeInfo
  alias ElixirSense.Core.TypeInference

  @env :elixir_env.new()
  def env, do: @env

  def expand(ast, state, env) do
    try do
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

  defp do_expand({:=, meta, [left, right]}, s, e) do
    assert_no_guard_scope(e.context, "=/2")
    {e_right, sr, er} = expand(right, s, e)
    # dbg(sr)
    # dbg(e_right)
    {e_left, sl, el} = __MODULE__.Clauses.match(&expand/3, left, sr, s, er)
    # IO.inspect(sl.vars_info, label: "left")
    # dbg(e_left)
    # dbg(el.versioned_vars)
    # dbg(sl.vars)
    refute_parallel_bitstring_match(e_left, e_right, e, Map.get(e, :context) == :match)
    # {{:=, meta, [e_left, e_right]}, sl, el |> Map.from_struct()} |> dbg(limit: :infinity)
    # el = el |> :elixir_env.with_vars(sl.vars |> elem(0))
    # sl = sl |> add_current_env_to_line(Keyword.fetch!(meta, :line), el)
    # dbg(sl)
    {{:=, meta, [e_left, e_right]}, sl, el}
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

  defp do_expand({:->, _meta, [_, _]}, _s, _e), do: raise("unhandled_arrow_op")

  defp do_expand({:"::", _meta, [_, _]}, _s, _e), do: raise("unhandled_type_op")

  defp do_expand({:|, _meta, [_, _]}, _s, _e), do: raise("unhandled_cons_op")

  # __block__

  defp do_expand({:__block__, _meta, []}, s, e), do: {nil, s, e}

  defp do_expand({:__block__, _meta, [arg]}, s, e) do
    # s = s |> add_current_env_to_line(Keyword.fetch!(meta, :line), e)
    expand(arg, s, e)
  end

  defp do_expand({:__block__, meta, args}, s, e) when is_list(args) do
    {e_args, sa, ea} = expand_block(args, [], meta, s, e)
    {{:__block__, meta, e_args}, sa, ea}
  end

  # __aliases__

  defp do_expand({:__aliases__, meta, [head | tail] = list}, state, env) do
    case Macro.Env.expand_alias(env, meta, list, trace: false) do
      {:alias, alias} ->
        # TODO?
        # A compiler may want to emit a :local_function trace in here.
        # Elixir also warns on easy to confuse aliases, such as True/False/Nil.
        {alias, state, env}

      :error ->
        {head, state, env} = expand(head, state, env)

        if is_atom(head) do
          # TODO?
          # A compiler may want to emit a :local_function trace in here.
          {Module.concat([head | tail]), state, env}
        else
          raise "invalid_alias"
          # {{:__aliases__, meta, [head | tail]}, state, env}
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
    assert_no_match_or_guard_scope(env.context, "alias")

    state =
      state
      |> add_first_alias_positions(env, meta)
      |> add_current_env_to_line(Keyword.fetch!(meta, :line), env)

    # no need to call expand_without_aliases_report - we never report
    {arg, state, env} = expand(arg, state, env)
    {opts, state, env} = expand_opts([:as, :warn], no_alias_opts(opts), state, env)

    if is_atom(arg) do
      # TODO check difference with
      # elixir_aliases:alias(Meta, Ref, IncludeByDefault, Opts, E, true)
      # TODO PR to elixir with is_atom(module) check?
      case Macro.Env.define_alias(env, meta, arg, [trace: false] ++ opts) do
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
    assert_no_match_or_guard_scope(env.context, "require")
    original_env = env

    state =
      state
      |> add_current_env_to_line(Keyword.fetch!(meta, :line), env)

    # no need to call expand_without_aliases_report - we never report
    {arg, state, env} = expand(arg, state, env)

    {opts, state, env} =
      expand_opts([:as, :warn], no_alias_opts(opts), state, env)

    case Keyword.fetch(meta, :defined) do
      {:ok, mod} when is_atom(mod) ->
        env = %{env | context_modules: [mod | env.context_modules]}

        state =
          case original_env do
            %{function: nil} -> state
            _ -> %{state | runtime_modules: [mod | state.runtime_modules]}
          end

        # TODO how to test that case?
        # TODO remove this case - in elixir this is a hack used only by special require call emitted by defmodule
        # Macro.Env.define_alias is not fully equivalent - it calls alias with IncludeByDefault set to true
        # we counter it with only calling it if :as option is set
        # {arg, state, alias(meta, e_ref, false, e_opts, ea)}
        if Keyword.has_key?(opts, :as) do
          case Macro.Env.define_alias(env, meta, arg, [trace: false] ++ opts) do
            {:ok, env} ->
              {arg, state, env}

            {:error, _} ->
              # elixir_aliases
              {arg, state, env}
          end
        else
          {arg, state, env}
        end

      :error when is_atom(arg) ->
        # TODO check differences
        # TODO ensure loaded?
        # ElixirAliases.ensure_loaded(meta, e_ref, et)
        # re = ElixirAliases.require(meta, e_ref, e_opts, et, true)
        # {e_ref, st, alias(meta, e_ref, false, e_opts, re)}
        case Macro.Env.define_require(env, meta, arg, [trace: false] ++ opts) do
          {:ok, env} ->
            {arg, state, env}

          {:error, _} ->
            # elixir_aliases
            {arg, state, env}
        end

      :error ->
        # expected_compile_time_module
        {arg, state, env}
    end
  end

  defp do_expand({:import, meta, [arg, opts]}, state, env) do
    assert_no_match_or_guard_scope(env.context, "import")

    state =
      state
      |> add_current_env_to_line(Keyword.fetch!(meta, :line), env)

    # no need to call expand_without_aliases_report - we never report
    {arg, state, env} = expand(arg, state, env)
    {opts, state, env} = expand_opts([:only, :except, :warn], opts, state, env)

    if is_atom(arg) do
      # TODO check difference
      # elixir_aliases:ensure_loaded(Meta, ERef, ET)
      # elixir_import:import(Meta, ERef, EOpts, ET, true, true)
      # TODO this does not work for context modules
      with true <- Code.ensure_loaded?(arg),
           {:ok, env} <- Macro.Env.define_import(env, meta, arg, [trace: false] ++ opts) do
        {arg, state, env}
      else
        _ ->
          # elixir_import
          {arg, state, env}
      end
    else
      # expected_compile_time_module
      {arg, state, env}
    end
  end

  # Compilation environment macros

  defp do_expand({:__MODULE__, meta, ctx}, state, env) when is_atom(ctx) do
    line = Keyword.get(meta, :line, 0)
    state = if line > 0, do: add_current_env_to_line(state, line, env), else: state

    {env.module, state, env}
  end

  defp do_expand({:__DIR__, meta, ctx}, state, env) when is_atom(ctx) do
    line = Keyword.get(meta, :line, 0)
    state = if line > 0, do: add_current_env_to_line(state, line, env), else: state

    {Path.dirname(env.file), state, env}
  end

  defp do_expand({:__CALLER__, meta, ctx} = caller, s, e) when is_atom(ctx) do
    assert_no_match_scope(e.context, "__CALLER__")
    # unless s.caller do
    #   function_error(meta, e, __MODULE__, :caller_not_allowed)
    # end
    line = Keyword.get(meta, :line, 0)
    s = if line > 0, do: add_current_env_to_line(s, line, e), else: s

    {caller, s, e}
  end

  defp do_expand({:__STACKTRACE__, meta, ctx} = stacktrace, s, e) when is_atom(ctx) do
    assert_no_match_scope(e.context, "__STACKTRACE__")
    # unless s.stacktrace do
    #   function_error(meta, e, __MODULE__, :stacktrace_not_allowed)
    # end
    line = Keyword.get(meta, :line, 0)
    s = if line > 0, do: add_current_env_to_line(s, line, e), else: s

    {stacktrace, s, e}
  end

  defp do_expand({:__ENV__, meta, ctx}, s, e) when is_atom(ctx) do
    assert_no_match_scope(e.context, "__ENV__")

    line = Keyword.get(meta, :line, 0)
    s = if line > 0, do: add_current_env_to_line(s, line, e), else: s

    {escape_map(escape_env_entries(meta, s, e)), s, e}
  end

  defp do_expand({{:., dot_meta, [{:__ENV__, meta, atom}, field]}, call_meta, []}, s, e)
       when is_atom(atom) and is_atom(field) do
    assert_no_match_scope(e.context, "__ENV__")

    line = Keyword.get(call_meta, :line, 0)
    s = if line > 0, do: add_current_env_to_line(s, line, e), else: s

    env = escape_env_entries(meta, s, e)

    case Map.fetch(env, field) do
      {:ok, value} -> {value, s, e}
      :error -> {{{:., dot_meta, [escape_map(env), field]}, call_meta, []}, s, e}
    end
  end

  # Quote

  defp do_expand({unquote_call, _meta, [_]}, _s, _e)
       when unquote_call in [:unquote, :unquote_splicing],
       do: raise("unquote_outside_quote")

  defp do_expand({:quote, meta, [opts]}, s, e) when is_list(opts) do
    case Keyword.fetch(opts, :do) do
      {:ok, do_block} ->
        new_opts = Keyword.delete(opts, :do)
        expand({:quote, meta, [new_opts, [{:do, do_block}]]}, s, e)

      :error ->
        raise "missing_option"
    end
  end

  defp do_expand({:quote, _meta, [_]}, _s, _e), do: raise("invalid_args")

  defp do_expand({:quote, meta, [opts, do_block]}, s, e) when is_list(do_block) do
    exprs =
      case Keyword.fetch(do_block, :do) do
        {:ok, expr} -> expr
        :error -> raise "missing_option"
      end

    valid_opts = [:context, :location, :line, :file, :unquote, :bind_quoted, :generated]
    {e_opts, st, et} = expand_opts(valid_opts, opts, s, e)

    context = Keyword.get(e_opts, :context, e.module || :"Elixir")

    {file, line} =
      case Keyword.fetch(e_opts, :location) do
        {:ok, :keep} -> {e.file, false}
        :error -> {Keyword.get(e_opts, :file, nil), Keyword.get(e_opts, :line, false)}
      end

    {binding, default_unquote} =
      case Keyword.fetch(e_opts, :bind_quoted) do
        {:ok, bq} ->
          if is_list(bq) and Enum.all?(bq, &match?({key, _} when is_atom(key), &1)) do
            {bq, false}
          else
            raise "invalid_bind_quoted_for_quote"
          end

        :error ->
          {[], true}
      end

    unquote_opt = Keyword.get(e_opts, :unquote, default_unquote)
    generated = Keyword.get(e_opts, :generated, false)

    # TODO this is a stub only
    # res = expand_quote(exprs, st, et)
    # res |> elem(0) |> IO.inspect
    # res
    {q, prelude} =
      __MODULE__.Quote.build(meta, line, file, context, unquote_opt, generated)

    quoted = __MODULE__.Quote.quote(meta, exprs, binding, q, prelude, et)
    expand(quoted, st, et)
  end

  defp do_expand({:quote, _meta, [_, _]}, _s, _e), do: raise("invalid_args")

  # Functions

  defp do_expand({:&, meta, [{:super, super_meta, args} = expr]}, s, e) when is_list(args) do
    assert_no_match_or_guard_scope(e.context, "&")

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
    assert_no_match_or_guard_scope(e.context, "&")

    case resolve_super(meta, arity, s, e) do
      {kind, name, _} when kind in [:def, :defp] ->

        line = Keyword.get(super_meta, :line, 0)
        column = Keyword.get(super_meta, :column, nil)

        s =
          s
          |> add_call_to_line({nil, name, arity}, {line, column})
          |> add_current_env_to_line(line, e)

        {{:&, meta, [{:/, arity_meta, [{name, super_meta, context}, arity]}]}, s, e}

      _ ->
        expand_fn_capture(meta, expr, s, e)
    end
  end

  defp do_expand({:&, meta, [arg]}, s, e) do
    assert_no_match_or_guard_scope(e.context, "&")
    expand_fn_capture(meta, arg, s, e)
  end

  defp do_expand({:fn, meta, pairs}, s, e) do
    assert_no_match_or_guard_scope(e.context, "fn")
    __MODULE__.Fn.expand(meta, pairs, s, e)
  end

  # case/cond/try/receive

  defp do_expand({:cond, meta, [opts]}, s, e) do
    assert_no_match_or_guard_scope(e.context, "cond")
    assert_no_underscore_clause_in_cond(opts, e)
    {e_clauses, sc, ec} = __MODULE__.Clauses.cond(meta, opts, s, e)
    {{:cond, meta, [e_clauses]}, sc, ec}
  end

  defp do_expand({:case, meta, [expr, options]}, s, e) do
    assert_no_match_or_guard_scope(e.context, "case")
    expand_case(meta, expr, options, s, e)
  end

  defp do_expand({:receive, meta, [opts]}, s, e) do
    assert_no_match_or_guard_scope(e.context, "receive")
    {e_clauses, sc, ec} = __MODULE__.Clauses.receive(meta, opts, s, e)
    {{:receive, meta, [e_clauses]}, sc, ec}
  end

  defp do_expand({:try, meta, [opts]}, s, e) do
    assert_no_match_or_guard_scope(e.context, "try")
    {e_clauses, sc, ec} = __MODULE__.Clauses.try(meta, opts, s, e)
    {{:try, meta, [e_clauses]}, sc, ec}
  end

  defp do_expand({:for, _, [_ | _]} = expr, s, e), do: expand_for(expr, s, e, true)

  defp do_expand({:with, meta, [_ | _] = args}, s, e) do
    assert_no_match_or_guard_scope(e.context, "with")
    __MODULE__.Clauses.with(meta, args, s, e)
  end

  # Cursor

  defp do_expand({:__cursor__, meta, []}, s, e) do
    s = unless s.cursor_env do
      s
      |> add_cursor_env(meta, e)
    else
      s
    end

    {{:__cursor__, meta, []}, s, e}
  end

  # Super

  defp do_expand({:super, meta, args}, s, e) when is_list(args) do
    assert_no_match_or_guard_scope(e.context, "super")
    arity = length(args)
    {kind, name, _} = resolve_super(meta, arity, s, e)
    {e_args, sa, ea} = expand_args(args, s, e)
    
    line = Keyword.get(meta, :line, 0)
    column = Keyword.get(meta, :column, nil)

    sa =
      sa
      |> add_call_to_line({nil, name, arity}, {line, column})
      |> add_current_env_to_line(line, ea)

    {{:super, [{:super, {kind, name}} | meta], e_args}, sa, ea}
  end

  # Vars

  # Pin operator
  # It only appears inside match and it disables the match behaviour.

  defp do_expand({:^, meta, [arg]}, %{prematch: {prematch, _, _}, vars: {_, write}} = s, e) do
    no_match_s = %{s | prematch: :pin, vars: {prematch, write}}

    case expand(arg, no_match_s, %{e | context: nil}) do
      {{name, _var_meta, kind} = var, %{unused: unused}, _}
      when is_atom(name) and is_atom(kind) ->
        s = add_var_read(s, var)
        {{:^, meta, [var]}, %{s | unused: unused}, e}

      {arg, s, _e} ->
        # elixir raises here invalid_arg_for_pin
        # we may have cursor in arg
        {{:^, meta, [arg]}, s, e}
    end
  end

  defp do_expand({:^, meta, [arg]}, s, e) do
    # elixir raises here pin_outside_of_match
    # try to recover from error by dropping the pin and expanding arg
    expand(arg, s, e)
  end

  defp do_expand({:_, _meta, kind} = var, s, %{context: _context} = e) when is_atom(kind) do
    # if context != :match, do: function_error(meta, e, __MODULE__, :unbound_underscore)
    {var, s, e}
  end

  defp do_expand({:_, _meta, kind} = var, s, %{context: :match} = e) when is_atom(kind) do
    {var, s, e}
  end

  defp do_expand({name, meta, kind}, s, %{context: :match} = e)
       when is_atom(name) and is_atom(kind) do
    %{
      prematch: {_, prematch_version, _},
      unused: version,
      vars: {read, write}
    } = s

    pair = {name, var_context(meta, kind)}

    case read do
      # Variable was already overridden
      %{^pair => var_version} when var_version >= prematch_version ->
        var = {name, [{:version, var_version} | meta], kind}
        # it's a write but for simplicity treat it as read
        s = add_var_read(s, var)
        {var, %{s | unused: version}, e}

      # Variable is being overridden now
      %{^pair => _} ->
        new_read = Map.put(read, pair, version)
        new_write = if write != false, do: Map.put(write, pair, version), else: write
        var = {name, [{:version, version} | meta], kind}
        s = add_var_write(s, var)
        {var, %{s | vars: {new_read, new_write}, unused: version + 1}, e}

      # Variable defined for the first time
      _ ->
        new_read = Map.put(read, pair, version)
        new_write = if write != false, do: Map.put(write, pair, version), else: write
        var = {name, [{:version, version} | meta], kind}
        s = add_var_write(s, var)
        {var, %{s | vars: {new_read, new_write}, unused: version + 1}, e}
    end
  end

  defp do_expand({name, meta, kind}, s, e) when is_atom(name) and is_atom(kind) do
    %{vars: {read, _write}, unused: version, prematch: prematch} = s
    pair = {name, var_context(meta, kind)}

    result =
      case read do
        %{^pair => current_version} ->
          case prematch do
            {pre, _counter, {_bitsize, original}} ->
              cond do
                Map.get(pre, pair) != current_version ->
                  {:ok, current_version}

                Map.has_key?(pre, pair) ->
                  # TODO: Enable this warning on Elixir v1.19
                  # TODO: Remove me on Elixir 2.0
                  # warn about unpinned bitsize var
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
          prematch
      end

    case result do
      {:ok, pair_version} ->
        var = {name, [{:version, pair_version} | meta], kind}
        s = add_var_read(s, var)
        {var, %{s | unused: version}, e}

      error ->
        case Keyword.fetch(meta, :if_undefined) do
          {:ok, :apply} ->
            # TODO check if this can happen
            expand({name, meta, []}, s, e)

          # TODO: Remove this clause on v2.0 as we will raise by default
          {:ok, :raise} ->
            # TODO is it worth registering var access
            # function_error(meta, e, __MODULE__, {:undefined_var, name, kind})
            {{name, meta, kind}, s, e}

          # TODO: Remove this clause on v2.0 as we will no longer support warn
          _ when error == :warn ->
            # TODO is it worth registering var access
            # Warn about undefined var to call
            # elixir_errors:file_warn(Meta, E, ?MODULE, {undefined_var_to_call, Name}),
            expand({name, [{:if_undefined, :warn} | meta], []}, s, e)

          _ when error == :pin ->
            # TODO is it worth registering var access
            # function_error(meta, e, __MODULE__, {:undefined_var_pin, name, kind})
            {{name, meta, kind}, s, e}

          _ ->
            # TODO is it worth registering var access
            span_meta = __MODULE__.Env.calculate_span(meta, name)
            # function_error(span_meta, e, __MODULE__, {:undefined_var, name, kind})
            {{name, span_meta, kind}, s, e}
        end
    end
  end

  # Local calls

  defp do_expand({fun, meta, args}, state, env)
       when is_atom(fun) and is_list(meta) and is_list(args) do
    assert_no_ambiguous_op(fun, meta, args, state, env)
    arity = length(args)

    # TODO check if it works in our case
    # If we are inside a function, we support reading from locals.
    allow_locals = match?({n, a} when fun != n or arity != a, env.function)

    case Macro.Env.expand_import(env, meta, fun, arity,
           trace: false,
           allow_locals: allow_locals,
           check_deprecations: false
         ) do
      {:macro, module, callback} ->
        # TODO there is a subtle difference - callback will call expander with state derrived from env via
        # :elixir_env.env_to_ex(env) possibly losing some details
        # line = Keyword.get(meta, :line, 0)
        # column = Keyword.get(meta, :column, nil)
        # state = state
        # |> add_call_to_line({module, fun, length(args)}, {line, column})
        # |> add_current_env_to_line(line, env)
        expand_macro(meta, module, fun, args, callback, state, env)

      {:function, module, fun} ->
        # Transform to remote call - we may need to do rewrites
        expand({{:., meta, [module, fun]}, meta, args}, state, env)

      :error ->
        expand_local(meta, fun, args, state, env)
    end
  end

  # Remote call

  defp do_expand({{:., dot_meta, [module, fun]}, meta, args}, state, env)
       when (is_tuple(module) or is_atom(module)) and is_atom(fun) and is_list(meta) and
              is_list(args) do
    # dbg({module, fun, args})
    {module, state_l, env} = expand(module, __MODULE__.Env.prepare_write(state), env)
    arity = length(args)

    if is_atom(module) do
      case :elixir_rewrite.inline(module, fun, arity) do
        {ar, an} ->
          expand_remote(ar, dot_meta, an, meta, args, state, state_l, env)

        false ->
          case Macro.Env.expand_require(env, meta, module, fun, arity,
                 trace: false,
                 check_deprecations: false
               ) do
            {:macro, module, callback} ->
              # TODO there is a subtle difference - callback will call expander with state derived from env via
              # :elixir_env.env_to_ex(env) possibly losing some details
              expand_macro(meta, module, fun, args, callback, state, env)

            :error ->
              # expand_remote(meta, module, fun, args, state, env)
              expand_remote(module, dot_meta, fun, meta, args, state, state_l, env)
          end
      end
    else
      expand_remote(module, dot_meta, fun, meta, args, state, state_l, env)
    end
  end

  # Anonymous calls

  defp do_expand({{:., dot_meta, [expr]}, meta, args}, s, e) when is_list(args) do
    assert_no_match_or_guard_scope(e.context, "anonymous call")
    {[e_expr | e_args], sa, ea} = expand_args([expr | args], s, e)

    sa =
      if is_atom(e_expr) do
        # function_error(meta, e, __MODULE__, {:invalid_function_call, e_expr})
        sa
      else
        line = Keyword.get(dot_meta, :line, 0)
        column = Keyword.get(dot_meta, :column, nil)

        column =
          if column do
            # for remote calls we emit position of right side of .
            # to make it consistent we shift dot position here
            column + 1
          else
            column
          end

        sa
        |> add_call_to_line({nil, e_expr, length(e_args)}, {line, column})
        |> add_current_env_to_line(line, e)
      end

    {{{:., dot_meta, [e_expr]}, meta, e_args}, sa, ea}
  end

  # Invalid calls

  defp do_expand({_, meta, args} = invalid, _s, _e) when is_list(meta) and is_list(args) do
    raise "invalid_call #{inspect(invalid)}"
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
      expand_list(list, &expand_arg/3, {__MODULE__.Env.prepare_write(s), s}, e, [])

    {e_args, __MODULE__.Env.close_write(se, s), ee}
  end

  defp do_expand(function, s, e) when is_function(function) do
    type_info = :erlang.fun_info(function, :type)
    env_info = :erlang.fun_info(function, :env)

    case {type_info, env_info} do
      {{:type, :external}, {:env, []}} ->
        {__MODULE__.Quote.fun_to_quoted(function), s, e}

      other ->
        raise "invalid_quoted_expr when expanding fun #{inspect(other)}"
    end
  end

  defp do_expand(pid, s, e) when is_pid(pid) do
    case e.function do
      nil ->
        {pid, s, e}

      _function ->
        # TODO: Make me an error on v2.0
        # ElixirErrors.file_warn([], e, __MODULE__, {:invalid_pid_in_function, pid, function})
        {pid, s, e}
    end
  end

  # defp do_expand(0.0 = zero, s, %{context: :match} = e) when is_float(zero) do
  #   # ElixirErrors.file_warn([], e, __MODULE__, :invalid_match_on_zero_float)
  #   {zero, s, e}
  # end

  defp do_expand(other, s, e) when is_number(other) or is_atom(other) or is_binary(other) do
    {other, s, e}
  end

  defp do_expand(other, _s, _e) do
    raise "invalid_quoted_expr #{inspect(other)}"
  end

  # Macro handling

  defp expand_macro(
         meta,
         Kernel,
         :defdelegate,
         [funs, opts],
         _callback,
         state,
         env
       ) do
    assert_no_match_or_guard_scope(env.context, :"def/2")
    module = assert_module_scope(env, :def, 2)

    {position, end_position} = extract_range(meta)
    {line, _} = position

    {opts, state, env} = expand(opts, state, env)
    target = Kernel.Utils.defdelegate_all(funs, opts, env)
    # TODO: Remove List.wrap when multiple funs are no longer supported
    state =
      funs
      |> List.wrap()
      |> Enum.reduce(state, fn fun, state ->
        # TODO expand args?
        {name, args, as, as_args} = Kernel.Utils.defdelegate_each(fun, opts)
        arity = length(args)

        state
        |> add_current_env_to_line(line, %{env | context: nil, function: {name, arity}})
        |> add_mod_fun_to_position(
          {module, name, arity},
          position,
          end_position,
          args,
          :defdelegate,
          "",
          # doc,
          %{delegate_to: {target, as, length(as_args)}},
          # meta
          target: {target, as}
        )
      end)

    {[], state, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :@,
         [{:behaviour, _meta, [arg]}],
         _callback,
         state,
         env
       ) do
    assert_module_scope(env, :@, 1)
    unless env.function, do: assert_no_match_or_guard_scope(env.context, "@/1")
    line = Keyword.fetch!(meta, :line)

    state =
      state
      |> add_current_env_to_line(line, env)

    {arg, state, env} = expand(arg, state, env)
    add_behaviour(arg, state, env)
  end

  defp expand_macro(
         meta,
         Kernel,
         :@,
         [{:moduledoc, doc_meta, [arg]}],
         _callback,
         state,
         env
       ) do
    assert_module_scope(env, :@, 1)
    unless env.function, do: assert_no_match_or_guard_scope(env.context, "@/1")
    line = Keyword.fetch!(meta, :line)

    state =
      state
      |> add_current_env_to_line(line, env)

    {arg, state, env} = expand(arg, state, env)

    state =
      state
      |> add_moduledoc_positions(
        env,
        meta
      )
      |> register_doc(env, :moduledoc, arg)

    {{:@, meta, [{:moduledoc, doc_meta, [arg]}]}, state, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :@,
         [{doc, doc_meta, [arg]}],
         _callback,
         state,
         env
       )
       when doc in [:doc, :typedoc] do
    assert_module_scope(env, :@, 1)
    unless env.function, do: assert_no_match_or_guard_scope(env.context, "@/1")
    line = Keyword.fetch!(meta, :line)

    state =
      state
      |> add_current_env_to_line(line, env)

    {arg, state, env} = expand(arg, state, env)

    state =
      state
      |> register_doc(env, doc, arg)

    {{:@, meta, [{doc, doc_meta, [arg]}]}, state, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :@,
         [{:impl, doc_meta, [arg]}],
         _callback,
         state,
         env
       ) do
    assert_module_scope(env, :@, 1)
    unless env.function, do: assert_no_match_or_guard_scope(env.context, "@/1")
    line = Keyword.fetch!(meta, :line)

    state =
      state
      |> add_current_env_to_line(line, env)

    {arg, state, env} = expand(arg, state, env)

    # impl adds sets :hidden by default
    state =
      state
      |> register_doc(env, :doc, :impl)

    {{:@, meta, [{:impl, doc_meta, [arg]}]}, state, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :@,
         [{:optional_callbacks, doc_meta, [arg]}],
         _callback,
         state,
         env
       ) do
    assert_module_scope(env, :@, 1)
    unless env.function, do: assert_no_match_or_guard_scope(env.context, "@/1")
    line = Keyword.fetch!(meta, :line)

    state =
      state
      |> add_current_env_to_line(line, env)

    {arg, state, env} = expand(arg, state, env)

    state =
      state
      |> register_optional_callbacks(arg)

    {{:@, meta, [{:optional_callbacks, doc_meta, [arg]}]}, state, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :@,
         [{:deprecated, doc_meta, [arg]}],
         _callback,
         state,
         env
       ) do
    assert_module_scope(env, :@, 1)
    unless env.function, do: assert_no_match_or_guard_scope(env.context, "@/1")
    line = Keyword.fetch!(meta, :line)

    state =
      state
      |> add_current_env_to_line(line, env)

    {arg, state, env} = expand(arg, state, env)

    state =
      state
      |> register_doc(env, :doc, deprecated: arg)

    {{:@, meta, [{:deprecated, doc_meta, [arg]}]}, state, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :@,
         [{:derive, doc_meta, [derived_protos]}],
         _callback,
         state,
         env
       ) do
    current_module = assert_module_scope(env, :@, 1)
    unless env.function, do: assert_no_match_or_guard_scope(env.context, "@/1")

    line = Keyword.fetch!(meta, :line)
    column = Keyword.fetch!(meta, :column)

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
            mod = Module.concat(proto_module, current_module)

            case acc.mods_funs_to_positions[{mod_any, nil, nil}] do
              nil ->
                # implementation for: Any not detected (is in other file etc.)
                acc
                |> add_module_to_index(mod, {line, column}, nil, generated: true)

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

          :error ->
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
         env
       )
       when kind in [:type, :typep, :opaque] do
    assert_module_scope(env, :@, 1)
    unless env.function, do: assert_no_match_or_guard_scope(env.context, "@/1")

    {expr, state, env} = __MODULE__.Typespec.expand_type(expr, state, env)

    case __MODULE__.Typespec.type_to_signature(expr) do
      {name, [_type_arg]} when name in [:required, :optional] ->
        raise "type #{name}/#{1} is a reserved type and it cannot be defined"

      {name, type_args} ->
        if __MODULE__.Typespec.built_in_type?(name, length(type_args)) do
          raise"type #{name}/#{length(type_args)} is a built-in type and it cannot be redefined"
        end
        # TODO elixir does Macro.escape with unquote: true

        spec = TypeInfo.typespec_to_string(kind, expr)

        {position = {line, _column}, end_position} = extract_range(attr_meta)

        state =
          state
          |> add_type(env, name, type_args, spec, kind, position, end_position)
          |> with_typespec({name, length(type_args)})
          |> add_current_env_to_line(line, env)
          |> with_typespec(nil)

        {{:@, attr_meta, [{kind, kind_meta, [expr]}]}, state, env}

      :error ->
        {{:@, attr_meta, [{kind, kind_meta, [expr]}]}, state, env}
    end
  end

  defp expand_macro(
         attr_meta,
         Kernel,
         :@,
         [{kind, kind_meta, [expr | _]}],
         _callback,
         state,
         env
       )
       when kind in [:callback, :macrocallback, :spec] do
    assert_module_scope(env, :@, 1)
    unless env.function, do: assert_no_match_or_guard_scope(env.context, "@/1")

    {expr, state, env} = __MODULE__.Typespec.expand_spec(expr, state, env)

    case __MODULE__.Typespec.spec_to_signature(expr) do
      {name, type_args} ->
        spec = TypeInfo.typespec_to_string(kind, expr)

        {position = {line, _column}, end_position} = extract_range(attr_meta)

        state =
          if kind in [:callback, :macrocallback] do
            state
            |> add_func_to_index(
              env,
              :behaviour_info,
              [{:atom, attr_meta, nil}],
              position,
              end_position,
              :def,
              generated: true
            )
          else
            state
          end

        state =
          state
          |> add_spec(env, name, type_args, spec, kind, position, end_position,
            generated: state.generated
          )
          |> with_typespec({name, length(type_args)})
          |> add_current_env_to_line(line, env)
          |> with_typespec(nil)

        {{:@, attr_meta, [{kind, kind_meta, [expr]}]}, state, env}

      :error ->
        {{:@, attr_meta, [{kind, kind_meta, [expr]}]}, state, env}
    end
  end

  defp expand_macro(
         meta,
         Kernel,
         :@,
         [{name, name_meta, args}],
         _callback,
         state,
         env
       )
       when is_atom(name) do
    assert_module_scope(env, :@, 1)
    unless env.function, do: assert_no_match_or_guard_scope(env.context, "@/1")
    line = Keyword.fetch!(meta, :line)
    column = Keyword.get(meta, :column, 1)

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
          if env.function, do: raise("cannot set attribute @#{name} inside function/macro")
          if name == :behavior, do: raise("@behavior attribute is not supported")
          {true, expand_args(args, state, env)}

        args ->
          raise "invalid @ call #{inspect(args)}"
      end

    inferred_type =
      case e_args do
        nil -> nil
        [arg] -> TypeInference.get_binding_type(state, arg)
      end

    state =
      state
      |> add_attribute(name, inferred_type, is_definition, {line, column})
      |> add_current_env_to_line(line, env)

    {{:@, meta, [{name, name_meta, e_args}]}, state, env}
  end

  defp expand_macro(
         meta,
         Kernel,
         :defoverridable,
         [arg],
         _callback,
         state,
         env
       ) do
    assert_module_scope(env, :defoverridable, 1)
    {arg, state, env} = expand(arg, state, env)

    case arg do
      keyword when is_list(keyword) ->
        {nil, make_overridable(state, env, keyword, meta[:context]), env}

      behaviour_module when is_atom(behaviour_module) ->
        if Code.ensure_loaded?(behaviour_module) and
             function_exported?(behaviour_module, :behaviour_info, 1) do
          keyword =
            behaviour_module.behaviour_info(:callbacks)
            |> Enum.map(&Introspection.drop_macro_prefix/1)

          {nil, make_overridable(state, env, keyword, meta[:context]), env}
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
         env
       )
       when type in [:defstruct, :defexception] do
    module = assert_module_scope(env, type, 1)

    if Map.has_key?(state.structs, module) do
      raise ArgumentError,
            "defstruct has already been called for " <>
              "#{inspect(module)}, defstruct can only be called once per module"
    end

    case fields do
      fs when is_list(fs) ->
        :ok

      other ->
        raise ArgumentError, "struct fields definition must be list, got: #{inspect(other)}"
    end

    {position, end_position} = extract_range(meta)

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
      |> add_struct_or_exception(env, type, fields, position, end_position)

    {{type, meta, [fields]}, state, env}
  end

  defp expand_macro(
         meta,
         Record,
         call,
         [name, _] = args,
         _callback,
         state,
         env
       )
       when call in [:defrecord, :defrecordp] do
    assert_no_match_or_guard_scope(env.context, :"{call}/2")
    module = assert_module_scope(env, call, 2)

    {position = {line, column}, end_position} = extract_range(meta)

    type =
      case call do
        :defrecord -> :defmacro
        :defrecordp -> :defmacrop
      end

    options = [generated: true]

    state =
      state
      |> add_func_to_index(
        env,
        name,
        [{:\\, [], [{:args, [], nil}, []]}],
        position,
        end_position,
        type,
        options
      )
      |> add_func_to_index(
        env,
        name,
        [{:record, [], nil}, {:args, [], nil}],
        position,
        end_position,
        type,
        options
      )
      |> add_call_to_line({module, call, length(args)}, {line, column})
      |> add_current_env_to_line(line, env)

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
    {position, end_position} = extract_range(meta)
    original_env = env
    # expand the macro normally
    {ast, state, env} =
      expand_macro_callback!(meta, Kernel, :defprotocol, args, callback, state, env)

    [module] = env.context_modules -- original_env.context_modules
    # add behaviour_info builtin
    # generate callbacks as macro expansion currently fails
    state =
      state
      |> add_func_to_index(
        %{env | module: module},
        :behaviour_info,
        [:atom],
        position,
        end_position,
        :def,
        generated: true
      )
      |> generate_protocol_callbacks(%{env | module: module})

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

    # TODO elixir uses expand_literals here
    {for, state, _env} =
      expand(for, state, %{env | module: env.module || Elixir, function: {:__impl__, 1}})

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
          raise ArgumentError, "defimpl expects a do-end block"

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
    %{vars: vars, unused: unused} = state
    original_env = env
    assert_no_match_or_guard_scope(env.context, "defmodule/2")

    {expanded, _state, _env} = expand(alias, state, env)

    # {expanded, with_alias} =
    #   case is_atom(expanded) do
    #     true ->
    #       {full, old, opts} = alias_defmodule(alias, expanded, env)
    #       # Expand the module considering the current environment/nesting
    #       meta = [defined: full] ++ alias_meta(alias)
    #       {full, {:require, meta, [old, opts]}}

    #     false ->
    #       {expanded, nil}
    #   end

    # The env inside the block is discarded
    {_result, state, env} =
      if is_atom(expanded) do
        {full, env} = alias_defmodule(alias, expanded, env)

        # in elixir context_modules and runtime_modules are handled via special require expansion
        # with :defined key set in meta
        env = %{env | context_modules: [full | env.context_modules]}

        state =
          case original_env do
            %{function: nil} ->
              state

            _ ->
              # TODO how to test that? quote do defmodule?
              %{state | runtime_modules: [full | state.runtime_modules]}
          end

        {position, end_position} = extract_range(meta)

        line = Keyword.fetch!(meta, :line)

        module_functions =
          case state.protocol do
            nil -> []
            _ -> [{:__impl__, [:atom], :def}]
          end

        state =
          state
          |> add_module_to_index(full, position, end_position, [])
          |> add_module
          |> add_current_env_to_line(line, %{env | module: full})
          |> add_module_functions(%{env | module: full}, module_functions, position, end_position)
          |> new_vars_scope
          |> new_attributes_scope

        # TODO magic with ElixirEnv instead of new_vars_scope?

        {state, _env} = maybe_add_protocol_behaviour(state, %{env | module: full})

        {result, state, _env} = expand(block, state, %{env | module: full})

        state =
          state
          |> apply_optional_callbacks(%{env | module: full})

        {result, state, env}
      else
        raise "unable to expand module alias #{inspect(expanded)}"
        # alias |> dbg
        # keys = state |> Map.from_struct() |> Map.take([:vars, :unused])
        # keys |> dbg(limit: :infinity)
        # block |> dbg
        # # If we don't know the module name, do we still want to expand it here?
        # # Perhaps it would be useful for dealing with local functions anyway?
        # # But note that __MODULE__ will return nil.

        # # TODO
        # expand(block, state, %{env | module: nil})
      end

    # restore vars from outer scope
    state =
      %{state | vars: vars, unused: unused}
      |> maybe_move_vars_to_outer_scope
      |> remove_vars_scope
      |> remove_attributes_scope
      |> remove_module

    # TODO hardcode expansion?
    # to result of require (a module atom) and :elixir_module.compile dot call in block

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
      expand_macro(meta, Kernel, :def, [call, {:__block__, [], []}], callback, state, env)

    {ast, state, env}
  end

  defp expand_macro(meta, Kernel, def_kind, [call], callback, state, env)
       when def_kind in [:def, :defp, :defmacro, :defmacrop, :defguard, :defguardp] do
    # transform guard to def with empty body
    expand_macro(meta, Kernel, def_kind, [call, {:__block__, [], []}], callback, state, env)
  end

  defp expand_macro(meta, Kernel, def_kind, [call, expr], _callback, state, env)
       when def_kind in [:def, :defp, :defmacro, :defmacrop, :defguard, :defguardp] do
    # dbg(call)
    # dbg(expr)
    assert_no_match_or_guard_scope(env.context, :"{def_kind}/2")
    _module = assert_module_scope(env, def_kind, 2)

    %{vars: vars, unused: unused} = state

    # unquoted_call = :elixir_quote.has_unquotes(call)
    # unquoted_expr = :elixir_quote.has_unquotes(expr)
    # TODO expand the call and expression.
    # TODO store mod_fun_to_pos
    line = Keyword.fetch!(meta, :line)

    unquoted_call = __MODULE__.Quote.has_unquotes(call)
    unquoted_expr = __MODULE__.Quote.has_unquotes(expr)

    {call, expr} =
      if unquoted_expr or unquoted_call do
        {call, expr} = __MODULE__.Quote.escape({call, expr}, :none, true)

        try do
          # TODO binding?
          {{call, expr}, _} = Code.eval_quoted({call, expr}, [], env)
          {call, expr}
        rescue
          _ -> raise "unable to eval #{inspect({call, expr})}"
        end
      else
        {call, expr}
      end

    # dbg(call)
    # dbg(expr)

    # state =
    #   state
    #   |> add_current_env_to_line(line, env)

    state =
      %{state | vars: {%{}, false}, unused: 0}
      |> new_func_vars_scope

    {name_and_args, guards} = __MODULE__.Utils.extract_guards(call)
    # dbg(name_and_args)

    {name, _meta_1, args} =
      case name_and_args do
        {n, m, a} when is_atom(n) and is_atom(a) -> {n, m, []}
        {n, m, a} when is_atom(n) and is_list(a) -> {n, m, a}
        _ -> raise "invalid_def"
      end

    {_e_args, state, a_env} =
      expand_args(args, %{state | prematch: {%{}, 0, :none}}, %{env | context: :match})

    {_e_guard, state, g_env} =
      __MODULE__.Clauses.guard(
        guards,
        %{state | prematch: :raise},
        Map.put(a_env, :context, :guard)
      )

    # The env inside the block is discarded.
    # TODO name_arity from call
    # TODO expand call
    # TODO what should it be for macros?
    # TODO how to handle guards?
    # {call, e_call, state, env} = case call do
    #   {:when, meta_2, [call, guard]} ->
    #     {name, meta_1, args} = call
    #     {e_args, state, env} = expand_args(args, %{state | prematch: {%{}, 0, :none}}, %{env | context: :match})
    #     {e_guard, state, env} = expand(guard, state, %{env | context: :guard})
    #     {{name, meta_1, e_args}, {:when, meta_2, [{name, meta_1, e_args}, e_guard]}, state, env}
    #   call ->
    #     {name, meta_1, args} = call
    #     {e_args, state, _env} = expand_args(args, %{state | prematch: {%{}, 0, :none}}, %{env | context: :match})
    #     {{name, meta_1, e_args}, {name, meta_1, e_args}, state, env}
    # end

    # {name, _meta_1, args} = call
    arity = length(args)

    {position, end_position} = extract_range(meta)

    state =
      state
      |> add_current_env_to_line(line, %{g_env | context: nil, function: {name, arity}})
      |> add_func_to_index(
        env,
        name,
        args,
        position,
        end_position,
        def_kind
      )

    # expand_macro_callback(meta, Kernel, def_kind, [call, expr], callback, state, env)
    # %{state | prematch: :warn}
    # TODO not sure vars scope is needed
    state = state |> new_vars_scope

    {_e_body, state, _env} =
      expand(expr, state, %{g_env | context: nil, function: {name, arity}})

    # restore vars from outer scope
    state =
      %{state | vars: vars, unused: unused}
      |> maybe_move_vars_to_outer_scope
      |> remove_vars_scope
      |> remove_func_vars_scope

    # result of def expansion is fa tuple
    {{name, arity}, state, env}
  end

  defp expand_macro(meta, module, fun, args, callback, state, env) do
    expand_macro_callback(meta, module, fun, args, callback, state, env)
  end

  defp expand_macro_callback(meta, module, fun, args, callback, state, env) do
    line = Keyword.get(meta, :line, 0)
    column = Keyword.get(meta, :column)

    state =
      state
      |> add_call_to_line({module, fun, length(args)}, {line, column})

    # dbg({module, fun, args})
    try do
      callback.(meta, args)
    catch
      # TODO raise?
      # For language servers, if expanding the macro fails, we just give up.
      _kind, _payload ->
        # IO.inspect(payload, label: inspect(fun))
        {{{:., meta, [module, fun]}, meta, args}, state, env}
    else
      ast ->
        {ast, state, env} = expand(ast, state, env)
        {ast, state, env}
    end
  end

  defp expand_macro_callback!(meta, module, fun, args, callback, state, env) do
    line = Keyword.get(meta, :line, 0)
    column = Keyword.get(meta, :column)

    state =
      state
      |> add_call_to_line({module, fun, length(args)}, {line, column})

    # dbg({module, fun, args})
    ast = callback.(meta, args)
    {ast, state, env} = expand(ast, state, env)
    {ast, state, env}
  end

  defp extract_range(meta) do
    line = Keyword.get(meta, :line, 0)

    if line == 0 do
      {nil, nil}
    else
      position = {
        line,
        Keyword.get(meta, :column, 1)
      }

      end_position =
        case meta[:end] do
          nil ->
            case meta[:end_of_expression] do
              nil ->
                nil

              end_of_expression_meta ->
                {
                  Keyword.fetch!(end_of_expression_meta, :line),
                  Keyword.fetch!(end_of_expression_meta, :column)
                }
            end

          end_meta ->
            {
              Keyword.fetch!(end_meta, :line),
              Keyword.fetch!(end_meta, :column) + 3
            }
        end

      {position, end_position}
    end
  end

  # defmodule helpers
  # defmodule automatically defines aliases, we need to mirror this feature here.

  # defmodule Elixir.Alias
  defp alias_defmodule({:__aliases__, _, [:"Elixir", _ | _]}, module, env),
    do: {module, env}

  # defmodule Alias in root
  defp alias_defmodule({:__aliases__, _, _}, module, %{module: nil} = env),
    do: {module, env}

  # defmodule Alias nested
  defp alias_defmodule({:__aliases__, meta, [h | t]}, _module, env) when is_atom(h) do
    module = Module.concat([env.module, h])
    alias = String.to_atom("Elixir." <> Atom.to_string(h))
    {:ok, env} = Macro.Env.define_alias(env, meta, module, as: alias, trace: false)

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
    assert_no_clauses(right, meta, args, e)

    line = Keyword.get(meta, :line, 0)
    column = Keyword.get(meta, :column, nil)

    sl =
      if line > 0 do
        sl
        |> add_current_env_to_line(line, e)
      else
        sl
      end

    if context == :guard and is_tuple(receiver) do
      if Keyword.get(meta, :no_parens) != true do
        raise "parens_map_lookup"
      end

      {{{:., dot_meta, [receiver, right]}, meta, []}, sl, e}
    else
      attached_meta = attach_runtime_module(receiver, meta, s, e)
      {e_args, {sa, _}, ea} = map_fold(&expand_arg/3, {sl, s}, e, args)

      case rewrite(context, receiver, dot_meta, right, attached_meta, e_args, s) do
        {:ok, rewritten} ->
          s =
            __MODULE__.Env.close_write(sa, s)
            |> add_call_to_line({receiver, right, length(e_args)}, {line, column})
            |> add_current_env_to_line(line, e)

          {rewritten, s, ea}

        {:error, _error} ->
          raise "elixir_rewrite"
      end
    end
  end

  defp expand_remote(receiver, dot_meta, right, meta, args, _, _, _e),
    do: raise("invalid_call remote #{inspect({{:., dot_meta, [receiver, right]}, meta, args})}")

  defp attach_runtime_module(receiver, meta, s, _e) do
    if receiver in s.runtime_modules do
      [{:runtime_module, true} | meta]
    else
      meta
    end
  end

  defp rewrite(_, :erlang, _, :+, _, [arg], _s) when is_number(arg), do: {:ok, arg}

  defp rewrite(_, :erlang, _, :-, _, [arg], _s) when is_number(arg), do: {:ok, -arg}

  defp rewrite(:match, receiver, dot_meta, right, meta, e_args, _s) do
    :elixir_rewrite.match_rewrite(receiver, dot_meta, right, meta, e_args)
  end

  defp rewrite(:guard, receiver, dot_meta, right, meta, e_args, s) do
    :elixir_rewrite.guard_rewrite(receiver, dot_meta, right, meta, e_args, guard_context(s))
  end

  defp rewrite(_, receiver, dot_meta, right, meta, e_args, _s) do
    {:ok, :elixir_rewrite.rewrite(receiver, dot_meta, right, meta, e_args)}
  end

  # This fixes exactly 1 test...
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

  defp expand_local(meta, fun, args, state, env = %{function: function}) when function != nil do
    assert_no_clauses(fun, meta, args, env)

    if env.context in [:match, :guard] do
      raise "invalid_local_invocation"
    end

    line = Keyword.get(meta, :line, 0)
    column = Keyword.get(meta, :column, nil)

    state =
      state
      |> add_call_to_line({nil, fun, length(args)}, {line, column})
      |> add_current_env_to_line(line, env)

    # state = update_in(state.locals, &[{fun, length(args)} | &1])
    {args, state, env} = expand_args(args, state, env)
    {{fun, meta, args}, state, env}
  end

  defp expand_local(meta, fun, args, state, env) do
    # elixir compiler raises here
    # raise "undefined_function"
    line = Keyword.get(meta, :line, 0)
    column = Keyword.get(meta, :column, nil)

    state =
      state
      |> add_call_to_line({nil, fun, length(args)}, {line, column})
      |> add_current_env_to_line(line, env)

    {args, state, env} = expand_args(args, state, env)

    {{fun, meta, args}, state, env}
  end

  defp expand_opts(allowed, opts, s, e) do
    {e_opts, se, ee} = expand(opts, s, e)
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
    # s = s |> add_current_env_to_line(Keyword.fetch!(meta, :line), e)
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
    # s = s |> add_current_env_to_line(Keyword.fetch!(meta, :line), e)
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
        {other, s, e}
    end

    map_fold(fun, state, env, refs)
  end

  defp overridable_name(name, count) when is_integer(count), do: :"#{name} (overridable #{count})"

  defp resolve_super(_meta, arity, state, e) do
    module = assert_module_scope(e)
    function = assert_function_scope(e)

    case function do
      {name, ^arity} ->
        state.mods_funs_to_positions

        case state.mods_funs_to_positions[{module, name, arity}] do
          %State.ModFunInfo{overridable: {true, _}} = info ->
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
            # TODO count 1 hardcoded but that's probably OK
            count = 1

            case hidden do
              false ->
                {kind, name, meta}

              true when kind in [:defmacro, :defmacrop] ->
                {:defmacrop, overridable_name(name, count), meta}

              true ->
                {:defp, overridable_name(name, count), meta}
            end

          nil ->
            raise "no_super"
        end

      _ ->
        raise "wrong_number_of_args_for_super"
    end
  end

  defp expand_fn_capture(meta, arg, s, e) do
    case __MODULE__.Fn.capture(meta, arg, s, e) do
      {{:remote, remote, fun, arity}, require_meta, dot_meta, se, ee} ->
        # if is_atom(remote) do
        #   ElixirEnv.trace({:remote_function, require_meta, remote, fun, arity}, e)
        # end
        attached_meta = attach_runtime_module(remote, require_meta, s, e)

        line = Keyword.get(attached_meta, :line, 0)
        column = Keyword.get(attached_meta, :column, nil)

        se =
          se
          |> add_call_to_line({remote, fun, arity}, {line, column})
          |> add_current_env_to_line(line, ee)

        {{:&, meta, [{:/, [], [{{:., dot_meta, [remote, fun]}, attached_meta, []}, arity]}]}, se,
         ee}

      {{:local, _fun, _arity}, _, _, _se, %{function: nil}} ->
        # TODO register call?
        raise "undefined_local_capture"

      {{:local, fun, arity}, local_meta, _, se, ee} ->
        line = Keyword.get(local_meta, :line, 0)
        column = Keyword.get(local_meta, :column, nil)

        se =
          se
          |> add_call_to_line({nil, fun, arity}, {line, column})
          |> add_current_env_to_line(line, ee)

        {{:&, meta, [{:/, [], [{fun, local_meta, nil}, arity]}]}, se, ee}

      {:expand, expr, se, ee} ->
        expand(expr, se, ee)
    end
  end

  defp expand_for({:for, meta, [_ | _] = args}, s, e, return) do
    assert_no_match_or_guard_scope(e.context, "for")
    {cases, block} = __MODULE__.Utils.split_opts(args)
    block = sanitize_opts([:do, :into, :uniq, :reduce], block)

    {expr, opts} =
      case Keyword.pop(block, :do) do
        {nil, do_opts} ->
          # elixir raises missing_option here
          {[], do_opts}
        {do_expr, do_opts} -> {do_expr, do_opts}
      end

    {e_opts, so, eo} = expand(opts, __MODULE__.Env.reset_vars(s), e)
    {e_cases, sc, ec} = map_fold(&expand_for_generator/3, so, eo, cases)
    # elixir raises here for_generator_start on invalid start generator

    {maybe_reduce, normalized_opts} = sanitize_for_options(e_opts, false, false, false, return, meta, e, [])

     # TODO not sure new vars scope is actually needed
     sc = sc |> new_vars_scope
     {e_expr, se, ee} = expand_for_do_block(meta, expr, sc, ec, maybe_reduce)

     se =
      se
       |> maybe_move_vars_to_outer_scope
       |> remove_vars_scope

    {{:for, meta, e_cases ++ [[{:do, e_expr} | normalized_opts]]},
     __MODULE__.Env.merge_vars(se, s, ee), e}
  end

  defp expand_for_do_block(meta, [{:->, _, _} | _] = clauses, s, e, false) do
    # elixir raises here for_without_reduce_bad_block
    # try to recover from error by emitting fake reduce
    expand_for_do_block(meta, clauses, s, e, {:reduce, []})
  end

  defp expand_for_do_block(_meta, expr, s, e, false), do: expand(expr, s, e)

  defp expand_for_do_block(meta, [{:->, _, _} | _] = clauses, s, e, {:reduce, _}) do
    transformer = fn
      {:->, clause_meta, [args, right]} = clause, sa ->
        # elixir checks here that clause has exactly 1 arg by matching against {_, _, [[_], _]}
        # we drop excessive or generate a fake arg
        # TODO check if there is cursor in dropped arg?
        args = case args do
          [] -> [{:_, [], e.module}]
          [head | _] -> [head]
        end
        clause = {:->, clause_meta, [args, right]}
        s_reset = __MODULE__.Env.reset_vars(sa)

        {e_clause, s_acc, e_acc} =
          __MODULE__.Clauses.clause(meta, :fn, &__MODULE__.Clauses.head/3, clause, s_reset, e)

        {e_clause, __MODULE__.Env.merge_vars(s_acc, sa, e_acc)}
    end

    {do_expr, sa} = Enum.map_reduce(clauses, s, transformer)
    {do_expr, sa, e}
  end

  defp expand_for_do_block(meta, expr, s, e, {:reduce, _} = reduce) do
    # elixir raises here for_with_reduce_bad_block
    case expr do
      [] ->
        # try to recover from error by emitting a fake clause
        expand_for_do_block(meta, [{:->, meta, [[{:_, [], e.module}], :ok]}], s, e, reduce)
      _ ->
        # try to recover from error by wrapping the expression in clause
        expand_for_do_block(meta, [{:->, meta, [[expr], :ok]}], s, e, reduce)
    end
  end

  defp expand_for_generator({:<-, meta, [left, right]}, s, e) do
    {e_right, sr, er} = expand(right, s, e)
    sm = __MODULE__.Env.reset_read(sr, s)
    {[e_left], sl, el} = __MODULE__.Clauses.head([left], sm, er)
    {{:<-, meta, [e_left, e_right]}, sl, el}
  end

  defp expand_for_generator({:<<>>, meta, args} = x, s, e) when is_list(args) do
    case __MODULE__.Utils.split_last(args) do
      {left_start, {:<-, op_meta, [left_end, right]}} ->
        {e_right, sr, er} = expand(right, s, e)
        sm = __MODULE__.Env.reset_read(sr, s)

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
    # TODO check if there is cursor  in dropped unique
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

  defp assert_no_clauses(_name, _meta, [], _e), do: :ok

  defp assert_no_clauses(name, meta, args, e) do
    assert_arg_with_no_clauses(name, meta, List.last(args), e)
  end

  defp assert_arg_with_no_clauses(name, meta, [{key, value} | rest], e) when is_atom(key) do
    case value do
      [{:->, _, _} | _] ->
        raise "invalid_clauses"

      _ ->
        assert_arg_with_no_clauses(name, meta, rest, e)
    end
  end

  defp assert_arg_with_no_clauses(_name, _meta, _arg, _e), do: :ok

  defp assert_module_scope(env, fun, arity) do
    case env.module do
      nil -> raise ArgumentError, "cannot invoke #{fun}/#{arity} outside module"
      mod -> mod
    end
  end

  defp assert_module_scope(%{module: nil}), do: raise("invalid_expr_in_scope")
  defp assert_module_scope(%{module: module}), do: module
  defp assert_function_scope(%{function: nil}), do: raise("invalid_expr_in_scope")
  defp assert_function_scope(%{function: function}), do: function

  defp assert_no_match_scope(context, _exp) do
    case context do
      :match ->
        raise "invalid_pattern_in_match"

      _ ->
        :ok
    end
  end

  defp assert_no_guard_scope(context, exp) do
    case context do
      :guard ->
        raise ArgumentError,
              "invalid expression in guard, #{exp} is not allowed in guards. " <>
                "To learn more about guards, visit: https://hexdocs.pm/elixir/patterns-and-guards.html"

      _ ->
        :ok
    end
  end

  defp assert_no_match_or_guard_scope(context, exp) do
    case context do
      :match ->
        invalid_match!(exp)

      :guard ->
        raise ArgumentError,
              "invalid expression in guard, #{exp} is not allowed in guards. " <>
                "To learn more about guards, visit: https://hexdocs.pm/elixir/patterns-and-guards.html"

      _ ->
        :ok
    end
  end

  defp invalid_match!(exp) do
    raise ArgumentError,
          "invalid expression in match, #{exp} is not allowed in patterns " <>
            "such as function clauses, case clauses or on the left side of the = operator"
  end

  defp assert_no_underscore_clause_in_cond([{:do, clauses}], _e) when is_list(clauses) do
    case List.last(clauses) do
      {:->, _meta, [[{:_, _, atom}], _]} when is_atom(atom) ->
        raise ArgumentError, "underscore_in_cond"

      _other ->
        :ok
    end
  end

  defp assert_no_underscore_clause_in_cond(_other, _e), do: :ok

  defp assert_no_ambiguous_op(name, meta, [_arg], s, _e) do
    case Keyword.fetch(meta, :ambiguous_op) do
      {:ok, kind} ->
        pair = {name, kind}

        case Map.get(s.vars, pair) do
          nil ->
            :ok

          _ ->
            raise "op_ambiguity"
        end

      _ ->
        :ok
    end
  end

  defp assert_no_ambiguous_op(_atom, _meta, _args, _s, _e), do: :ok

  defp refute_parallel_bitstring_match({:<<>>, _, _}, {:<<>>, _meta, _} = _arg, _e, true) do
    # file_error(meta, e, __MODULE__, {:parallel_bitstring_match, arg})
    raise ArgumentError, "parallel_bitstring_match"
  end

  defp refute_parallel_bitstring_match(left, {:=, _meta, [match_left, match_right]}, e, parallel) do
    refute_parallel_bitstring_match(left, match_left, e, true)
    refute_parallel_bitstring_match(left, match_right, e, parallel)
  end

  defp refute_parallel_bitstring_match(left = [_ | _], right = [_ | _], e, parallel) do
    refute_parallel_bitstring_match_each(left, right, e, parallel)
  end

  defp refute_parallel_bitstring_match({left1, left2}, {right1, right2}, e, parallel) do
    refute_parallel_bitstring_match_each([left1, left2], [right1, right2], e, parallel)
  end

  defp refute_parallel_bitstring_match({:tuple, _, args1}, {:tuple, _, args2}, e, parallel) do
    refute_parallel_bitstring_match_each(args1, args2, e, parallel)
  end

  defp refute_parallel_bitstring_match({:%{}, _, args1}, {:%{}, _, args2}, e, parallel) do
    refute_parallel_bitstring_match_map_field(Enum.sort(args1), Enum.sort(args2), e, parallel)
  end

  defp refute_parallel_bitstring_match({:%, _, [_, args]}, right, e, parallel) do
    refute_parallel_bitstring_match(args, right, e, parallel)
  end

  defp refute_parallel_bitstring_match(left, {:%, _, [_, args]}, e, parallel) do
    refute_parallel_bitstring_match(left, args, e, parallel)
  end

  defp refute_parallel_bitstring_match(_left, _right, _e, _parallel), do: :ok

  defp refute_parallel_bitstring_match_each([arg1 | rest1], [arg2 | rest2], e, parallel) do
    refute_parallel_bitstring_match(arg1, arg2, e, parallel)
    refute_parallel_bitstring_match_each(rest1, rest2, e, parallel)
  end

  defp refute_parallel_bitstring_match_each(_list1, _list2, _e, _parallel), do: :ok

  defp refute_parallel_bitstring_match_map_field(
         [{key, val1} | rest1],
         [{key, val2} | rest2],
         e,
         parallel
       ) do
    refute_parallel_bitstring_match(val1, val2, e, parallel)
    refute_parallel_bitstring_match_map_field(rest1, rest2, e, parallel)
  end

  defp refute_parallel_bitstring_match_map_field(
         [field1 | rest1] = args1,
         [field2 | rest2] = args2,
         e,
         parallel
       ) do
    cond do
      field1 > field2 -> refute_parallel_bitstring_match_map_field(args1, rest2, e, parallel)
      true -> refute_parallel_bitstring_match_map_field(rest1, args2, e, parallel)
    end
  end

  defp refute_parallel_bitstring_match_map_field(_args1, _args2, _e, _parallel), do: :ok

  defp var_context(meta, kind) do
    case Keyword.fetch(meta, :counter) do
      {:ok, counter} -> counter
      :error -> kind
    end
  end

  # TODO probably we can remove it/hardcode, used only for generating error message
  defp guard_context(%{prematch: {_, _, {:bitsize, _}}}), do: "bitstring size specifier"
  defp guard_context(_), do: "guard"

  defp expand_case(meta, expr, opts, s, e) do
    {e_expr, se, ee} = expand(expr, s, e)

    r_opts = opts
    # if proplists.get_value(:optimize_boolean, meta, false) do
    #   if ElixirUtils.returns_boolean(e_expr) do
    #     rewrite_case_clauses(opts)
    #   else
    #     generated_case_clauses(opts)
    #   end
    # else
    #   opts
    # end

    {e_opts, so, eo} = __MODULE__.Clauses.case(meta, r_opts, se, ee)
    {{:case, meta, [e_expr, e_opts]}, so, eo}
  end

  def expand_arg(arg, acc, e)
      when is_number(arg) or is_atom(arg) or is_binary(arg) or is_pid(arg) do
    {arg, acc, e}
  end

  def expand_arg(arg, {acc, s}, e) do
    {e_arg, s_acc, e_acc} = expand(arg, __MODULE__.Env.reset_read(acc, s), e)
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
    {e_args, {sa, _}, ea} = map_fold(&expand_arg/3, {__MODULE__.Env.prepare_write(s), s}, e, args)
    {e_args, __MODULE__.Env.close_write(sa, s), ea}
  end

  defmodule Env do
    alias ElixirSense.Core.Compiler.Utils, as: ElixirUtils

    def reset_vars(s) do
      s |> new_vars_scope
    end

    def reset_read(%{vars: {_, write}} = s, %{vars: {read, _}}) do
      %{s | vars: {read, write}}
    end

    def prepare_write(%{vars: {read, _}} = s) do
      %{s | vars: {read, read}}
    end

    def close_write(%{vars: {_read, write}} = s, %{vars: {_, false}}) do
      %{s | vars: {write, false}}
    end

    def close_write(%{vars: {_read, write}} = s, %{vars: {_, upper_write}}) do
      %{s | vars: {write, merge_vars(upper_write, write)}}
    end

    defp merge_vars(v, v), do: v

    defp merge_vars(v1, v2) do
      :maps.fold(
        fn k, m2, acc ->
          case Map.fetch(acc, k) do
            {:ok, m1} when m1 >= m2 -> acc
            _ -> Map.put(acc, k, m2)
          end
        end,
        v1,
        v2
      )
    end

    def merge_vars(s, %{vars: {read, write}}, _e) do
      # dbg(s.vars_info)
      # dbg({read, write})
      s =
        %{s | vars: {read, write}}
        |> maybe_move_vars_to_outer_scope
        |> remove_vars_scope

      # dbg(s.vars_info)
      # dbg(s.vars_info_per_scope_id)
      s
    end

    def calculate_span(meta, name) do
      case Keyword.fetch(meta, :column) do
        {:ok, column} ->
          span = {ElixirUtils.get_line(meta), column + String.length(Atom.to_string(name))}
          [{:span, span} | meta]

        _ ->
          meta
      end
    end
  end

  defmodule Utils do
    def split_last([]), do: {[], []}

    def split_last(list), do: split_last(list, [])

    defp split_last([h], acc), do: {Enum.reverse(acc), h}

    defp split_last([h | t], acc), do: split_last(t, [h | acc])

    def split_opts(args) do
      case split_last(args) do
        {outer_cases, outer_opts} when is_list(outer_opts) ->
          case split_last(outer_cases) do
            {inner_cases, inner_opts} when is_list(inner_opts) ->
              {inner_cases, inner_opts ++ outer_opts}

            _ ->
              {outer_cases, outer_opts}
          end

        _ ->
          {args, []}
      end
    end

    def get_line(opts) when is_list(opts) do
      case Keyword.fetch(opts, :line) do
        {:ok, line} when is_integer(line) -> line
        _ -> 0
      end
    end

    def extract_guards({:when, _, [left, right]}), do: {left, extract_or_guards(right)}
    def extract_guards(term), do: {term, []}

    def extract_or_guards({:when, _, [left, right]}), do: [left | extract_or_guards(right)]
    def extract_or_guards(term), do: [term]

    def select_with_cursor(ast_list) do
      Enum.find(ast_list, &has_cursor?/1)
    end

    def has_cursor?(ast) do
      # TODO rewrite to lazy prewalker
      {_, result} = Macro.prewalk(ast, false, fn
        {:__cursor__, _, list} = node, state when is_list(list) ->
          {node, true}
        node, state ->
          {node, state}
      end)
      result
    end
  end

  defmodule Clauses do
    alias ElixirSense.Core.Compiler, as: ElixirExpand
    alias ElixirSense.Core.Compiler.Env, as: ElixirEnv
    alias ElixirSense.Core.Compiler.Utils, as: ElixirUtils

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
          cursor_env: after_s.cursor_env
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
          cursor_env: s_expr.cursor_env
      }

      end_e = Map.put(ee, :context, Map.get(e, :context))
      {e_expr, end_s, end_e}
    end

    def clause(meta, kind, fun, {:->, clause_meta, [_, _]} = clause, s, e)
        when is_function(fun, 4) do
      clause(meta, kind, fn x, sa, ea -> fun.(clause_meta, x, sa, ea) end, clause, s, e)
    end

    def clause(_meta, _kind, fun, {:->, meta, [left, right]}, s, e) do
      {e_left, sl, el} = fun.(left, s, e)
      {e_right, sr, er} = ElixirExpand.expand(right, sl, el)
      {{:->, meta, [e_left, e_right]}, sr, er}
    end

    def clause(meta, kind, fun, expr, s, e) do
      # try to recover from error by wrapping the expression in clause
      # elixir raises here bad_or_missing_clauses
      clause(meta, kind, fun, {:->, meta, [[expr], :ok]}, s, e)
    end

    def head([{:when, meta, [_ | _] = all}], s, e) do
      {args, guard} = ElixirUtils.split_last(all)
      prematch = s.prematch

      {{e_args, e_guard}, sg, eg} =
        match(
          fn _ok, sm, em ->
            {e_args, sa, ea} = ElixirExpand.expand_args(args, sm, em)

            {e_guard, sg, eg} =
              guard(guard, %{sa | prematch: prematch}, %{ea | context: :guard})

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
      match(&ElixirExpand.expand_args/3, args, s, s, e)
    end

    def guard({:when, meta, [left, right]}, s, e) do
      {e_left, sl, el} = guard(left, s, e)
      {e_right, sr, er} = guard(right, sl, el)
      {{:when, meta, [e_left, e_right]}, sr, er}
    end

    def guard(guard, s, e) do
      {e_guard, sg, eg} = ElixirExpand.expand(guard, s, e)
      # warn_zero_length_guard(e_guard, eg)
      {e_guard, sg, eg}
    end

    # case

    def case(_meta, [], _s, _e) do
      raise ArgumentError, "missing_option"
    end

    def case(_meta, opts, _s, _e) when not is_list(opts) do
      raise ArgumentError, "invalid_args"
    end

    def case(meta, opts, s, e) do
      opts = sanitize_opts(opts, [:do])

      {case_clauses, sa} =
        Enum.map_reduce(opts, s, fn x, sa ->
          expand_case(meta, x, sa, e)
        end)

      {case_clauses, sa, e}
    end

    defp expand_case(meta, {:do, _} = do_clause, s, e) do
      fun = expand_head(meta, :case, :do)
      expand_clauses(meta, :case, fun, do_clause, s, e)
    end

    # cond

    def cond(_meta, [], _s, _e) do
      raise ArgumentError, "missing_option"
    end

    def cond(_meta, opts, _s, _e) when not is_list(opts) do
      raise ArgumentError, "invalid_args"
    end

    def cond(meta, opts, s, e) do
      opts = sanitize_opts(opts, [:do])

      {cond_clauses, sa} =
        Enum.map_reduce(opts, s, fn x, sa ->
          expand_cond(meta, x, sa, e)
        end)

      {cond_clauses, sa, e}
    end

    defp expand_cond(meta, {:do, _} = do_clause, s, e) do
      fun = expand_one(meta, :cond, :do, &ElixirExpand.expand_args/3)
      expand_clauses(meta, :cond, fun, do_clause, s, e)
    end

    # receive

    def receive(_meta, [], _s, _e) do
      raise ArgumentError, "missing_option"
    end

    def receive(_meta, opts, _s, _e) when not is_list(opts) do
      raise ArgumentError, "invalid_args"
    end

    def receive(meta, opts, s, e) do
      opts = sanitize_opts(opts, [:do, :after])

      {receive_clauses, sa} =
        Enum.map_reduce(opts, s, fn x, sa ->
          expand_receive(meta, x, sa, e)
        end)

      {receive_clauses, sa, e}
    end

    defp expand_receive(_meta, {:do, {:__block__, _, []}} = do_block, s, _e) do
      {do_block, s}
    end

    defp expand_receive(meta, {:do, _} = do_clause, s, e) do
      fun = expand_head(meta, :receive, :do)
      expand_clauses(meta, :receive, fun, do_clause, s, e)
    end

    defp expand_receive(meta, {:after, [_]} = after_clause, s, e) do
      fun = expand_one(meta, :receive, :after, &ElixirExpand.expand_args/3)
      expand_clauses(meta, :receive, fun, after_clause, s, e)
    end

    defp expand_receive(meta, {:after, expr}, s, e) when not is_list(expr) do
      # elixir raises here multiple_after_clauses_in_receive
      case expr do
        expr when not is_list(expr) ->
          # try to recover from error by wrapping the expression in list
          expand_receive(meta, {:after, [expr]}, s, e)
        [first | _] ->
          # try to recover from error by taking first clause only
          # TODO maybe search for clause with cursor?
          expand_receive(meta, {:after, [first]}, s, e)
        [] ->
          # try to recover from error by inserting a fake clause
          expand_receive(meta, {:after, [{:->, meta, [[0], :ok]}]}, s, e)
      end
    end

    # with

    def with(meta, args, s, e) do
      {exprs, opts0} = ElixirUtils.split_opts(args)
      opts0 = sanitize_opts(opts0, [:do, :else])
      s0 = ElixirEnv.reset_vars(s)
      {e_exprs, {s1, e1}} = Enum.map_reduce(exprs, {s0, e}, &expand_with/2)
      {e_do, opts1, s2} = expand_with_do(meta, opts0, s, s1, e1)
      {e_opts, opts2, s3} = expand_with_else(meta, opts1, s2, e)

      {{:with, meta, e_exprs ++ [[{:do, e_do} | e_opts]]}, s3, e}
    end

    defp expand_with({:<-, meta, [left, right]}, {s, e}) do
      {e_right, sr, er} = ElixirExpand.expand(right, s, e)
      sm = ElixirEnv.reset_read(sr, s)
      {[e_left], sl, el} = head([left], sm, er)

      {{:<-, meta, [e_left, e_right]}, {sl, el}}
    end

    defp expand_with(expr, {s, e}) do
      {e_expr, se, ee} = ElixirExpand.expand(expr, s, e)
      {e_expr, {se, ee}}
    end

    defp expand_with_do(_meta, opts, s, acc, e) do
      {expr, rest_opts} = Keyword.pop(opts, :do)
      # elixir raises here missing_option
      # we return empty expression
      expr = expr || []

      # TODO not sure new vars scope is needed
      acc = acc |> new_vars_scope
      {e_expr, s_acc, e_acc} = ElixirExpand.expand(expr, acc, e)

      s_acc =
        s_acc
        |> maybe_move_vars_to_outer_scope
        |> remove_vars_scope

      {e_expr, rest_opts, ElixirEnv.merge_vars(s_acc, s, e_acc)}
    end

    defp expand_with_else(meta, opts, s, e) do
      case Keyword.pop(opts, :else) do
        {nil, _} ->
          {[], opts, s}

        {expr, rest_opts} ->
          pair = {:else, expr}
          fun = expand_head(meta, :with, :else)
          {e_pair, se} = expand_clauses(meta, :with, fun, pair, s, e)
          {[e_pair], rest_opts, se}
      end
    end

    # try

    def try(_meta, [], _s, _e), do: raise("missing_option")
    def try(_meta, opts, _s, _e) when not is_list(opts), do: raise("invalid_args")

    def try(meta, opts, s, e) do
      opts = sanitize_opts(opts, [:do, :rescue, :catch, :else, :after])

      {try_clauses, sa} =
        Enum.map_reduce(opts, s, fn x, sa ->
          expand_try(meta, x, sa, e)
        end)

      {try_clauses, sa, e}
    end

    defp expand_try(_meta, {:do, expr}, s, e) do
      {e_expr, se, ee} = ElixirExpand.expand(expr, ElixirEnv.reset_vars(s), e)
      {{:do, e_expr}, ElixirEnv.merge_vars(se, s, ee)}
    end

    defp expand_try(_meta, {:after, expr}, s, e) do
      {e_expr, se, ee} = ElixirExpand.expand(expr, ElixirEnv.reset_vars(s), e)
      {{:after, e_expr}, ElixirEnv.merge_vars(se, s, ee)}
    end

    defp expand_try(meta, {:else, _} = else_clause, s, e) do
      fun = expand_head(meta, :try, :else)
      expand_clauses(meta, :try, fun, else_clause, s, e)
    end

    defp expand_try(meta, {:catch, _} = catch_clause, s, e) do
      expand_clauses_with_stacktrace(meta, &expand_catch/4, catch_clause, s, e)
    end

    defp expand_try(meta, {:rescue, _} = rescue_clause, s, e) do
      expand_clauses_with_stacktrace(meta, &expand_rescue/4, rescue_clause, s, e)
    end

    defp expand_clauses_with_stacktrace(meta, fun, clauses, s, e) do
      old_stacktrace = s.stacktrace
      ss = %{s | stacktrace: true}
      {ret, se} = expand_clauses(meta, :try, fun, clauses, ss, e)
      {ret, %{se | stacktrace: old_stacktrace}}
    end

    defp expand_catch(_meta, args = [_], s, e) do
      head(args, s, e)
    end

    defp expand_catch(_meta, args = [_, _], s, e) do
      head(args, s, e)
    end

    defp expand_catch(meta, [a1, a2 | _], s, e) do
      # attempt to recover from error by taking 2 first args
      # elixir raises here wrong_number_of_args_for_clause
      expand_catch(meta, [a1, a2], s, e)
    end

    defp expand_rescue(_meta, [arg], s, e) do
      # elixir is strict here and raises invalid_rescue_clause on invalid args
      {e_arg, sa, ea} = expand_rescue(arg, s, e)
      {[e_arg], sa, ea}
    end

    defp expand_rescue(meta, [a1 | _], s, e) do
      # try to recover from error by taking first argument only
      # elixir raises here wrong_number_of_args_for_clause
      expand_rescue(meta, [a1], s, e)
    end

    # rescue var
    defp expand_rescue({name, _, atom} = var, s, e) when is_atom(name) and is_atom(atom) do
      match(&ElixirExpand.expand/3, var, s, s, e)
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
      match(&ElixirExpand.expand/3, var, s, s, e)
    end

    # rescue var in (list() or atom())
    defp expand_rescue({:in, meta, [left, right]}, s, e) do
      {e_left, sl, el} = match(&ElixirExpand.expand/3, left, s, s, e)
      {e_right, sr, er} = ElixirExpand.expand(right, sl, el)

      case e_left do
        {name, _, atom} when is_atom(name) and is_atom(atom) ->
          {{:in, meta, [e_left, normalize_rescue(e_right, e)]}, sr, er}

        _ ->
          # elixir rejects this case, we normalize to underscore
          {{:in, meta, [{:_, [], e.module}, normalize_rescue(e_right, e)]}, sr, er}
      end
    end

    # rescue expr() => rescue expanded_expr()
    defp expand_rescue({_, meta, _} = arg, s, e) do
      # TODO wut?
      case Macro.expand_once(arg, %{e | line: line(meta)}) do
        ^arg ->
          # elixir rejects this case
          # try to recover from error by generating fake expression
          expand_rescue({:in, meta, [arg, {:_, [], e.module}]}, s, e)
        new_arg -> expand_rescue(new_arg, s, e)
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
      res = if is_list(other) do
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

    defp expand_head(_meta, _kind, _key) do
      fn
        [{:when, _, [args, _, _ | _]}], _, _e ->
          raise ArgumentError, "wrong_number_of_args_for_clause #{inspect(args)}"

        [_] = args, s, e ->
          head(args, s, e)

        args, _, _e ->
          raise ArgumentError, "wrong_number_of_args_for_clause #{inspect(args)}"
      end
    end

    defp expand_one(_meta, _kind, _key, fun) do
      fn
        [_] = args, s, e ->
          fun.(args, s, e)

        _, _, _e ->
          raise ArgumentError, "wrong_number_of_args_for_clause"
      end
    end

    defp expand_clauses(meta, kind, fun, clauses, s, e) do
      new_kind = origin(meta, kind)
      expand_clauses_origin(meta, new_kind, fun, clauses, s, e)
    end

    defp expand_clauses_origin(meta, kind, fun, {key, [_ | _] = clauses}, s, e) do
      transformer = fn clause, sa ->
        {e_clause, s_acc, e_acc} =
          clause(meta, {kind, key}, fun, clause, ElixirEnv.reset_vars(sa), e)

        {e_clause, ElixirEnv.merge_vars(s_acc, sa, e_acc)}
      end

      {values, se} = Enum.map_reduce(clauses, s, transformer)
      {{key, values}, se}
    end

    defp expand_clauses_origin(meta, kind, fun, {key, expr}, s, e) do
      # try to recover from error by wrapping the expression in a clauses list
      # elixir raises here bad_or_missing_clauses
      expand_clauses_origin(meta, kind, fun, {key, [expr]}, s, e)
    end

    # helpers

    defp sanitize_opt(opts, opt) do
      # TODO look for opt with cursor?
      case Keyword.fetch(opts, opt) do
        :error -> []
        {:ok, value} -> [{opt, value}]
      end
    end

    defp sanitize_opts(opts, allowed) do
      Enum.flat_map(allowed, fn opt -> sanitize_opt(opts, opt) end)
    end

    defp origin(meta, default) do
      Keyword.get(meta, :origin, default)
    end

    defp line(opts) when is_list(opts) do
      case Keyword.fetch(opts, :line) do
        {:ok, line} when is_integer(line) -> line
        _ -> 0
      end
    end
  end

  defmodule Bitstring do
    alias ElixirSense.Core.Compiler, as: ElixirExpand
    alias ElixirSense.Core.Compiler.Env, as: ElixirEnv
    alias ElixirSense.Core.Compiler.Utils, as: ElixirUtils

    defp expand_match(expr, {s, original_s}, e) do
      {e_expr, se, ee} = ElixirExpand.expand(expr, s, e)
      {e_expr, {se, original_s}, ee}
    end

    def expand(meta, args, s, e, require_size) do
      case Map.get(e, :context) do
        :match ->
          {e_args, alignment, {sa, _}, ea} =
            expand(meta, &expand_match/3, args, [], {s, s}, e, 0, require_size)

          case find_match(e_args) do
            false ->
              :ok

            _match ->
              raise "nested_match"
          end

          {{:<<>>, [{:alignment, alignment} | meta], e_args}, sa, ea}

        _ ->
          pair_s = {ElixirEnv.prepare_write(s), s}

          {e_args, alignment, {sa, _}, ea} =
            expand(meta, &ElixirExpand.expand_arg/3, args, [], pair_s, e, 0, require_size)

          {{:<<>>, [{:alignment, alignment} | meta], e_args}, ElixirEnv.close_write(sa, s), ea}
      end
    end

    def expand(_bitstr_meta, _fun, [], acc, s, e, alignment, _require_size) do
      {Enum.reverse(acc), alignment, s, e}
    end

    def expand(
          bitstr_meta,
          fun,
          [{:"::", meta, [left, right]} | t],
          acc,
          s,
          e,
          alignment,
          require_size
        ) do
      {e_left, {sl, original_s}, el} = expand_expr(meta, left, fun, s, e)

      match_or_require_size = require_size or is_match_size(t, el)
      e_type = expr_type(e_left)

      expect_size =
        case e_left do
          _ when not match_or_require_size -> :optional
          {:^, _, [{_, _, _}]} -> {:infer, e_left}
          _ -> :required
        end

      {e_right, e_alignment, ss, es} =
        expand_specs(e_type, meta, right, sl, original_s, el, expect_size)

      e_acc = concat_or_prepend_bitstring(meta, e_left, e_right, acc, es, match_or_require_size)

      expand(
        bitstr_meta,
        fun,
        t,
        e_acc,
        {ss, original_s},
        es,
        alignment(alignment, e_alignment),
        require_size
      )
    end

    def expand(bitstr_meta, fun, [h | t], acc, s, e, alignment, require_size) do
      meta = extract_meta(h, bitstr_meta)
      {e_left, {ss, original_s}, es} = expand_expr(meta, h, fun, s, e)

      match_or_require_size = require_size or is_match_size(t, es)
      e_type = expr_type(e_left)
      e_right = infer_spec(e_type, meta)

      inferred_meta = [{:inferred_bitstring_spec, true} | meta]

      e_acc =
        concat_or_prepend_bitstring(
          inferred_meta,
          e_left,
          e_right,
          acc,
          es,
          match_or_require_size
        )

      expand(meta, fun, t, e_acc, {ss, original_s}, es, alignment, require_size)
    end

    defp expand_expr(
           _meta,
           {{:., _, [mod, :to_string]}, _, [arg]} = ast,
           fun,
           s,
           %{context: context} = e
         )
         when context != nil and (mod == Kernel or mod == String.Chars) do
      case fun.(arg, s, e) do
        {ebin, se, ee} when is_binary(ebin) -> {ebin, se, ee}
        _ -> fun.(ast, s, e)
      end
    end

    defp expand_expr(_meta, component, fun, s, e) do
      case fun.(component, s, e) do
        {e_component, _, _error_e} when is_list(e_component) or is_atom(e_component) ->
          raise "invalid_literal"

        expanded ->
          expanded
      end
    end

    defp expand_specs(expr_type, meta, info, s, original_s, e, expect_size) do
      default =
        %{size: :default, unit: :default, sign: :default, type: :default, endianness: :default}

      {specs, ss, es} =
        expand_each_spec(meta, unpack_specs(info, []), default, s, original_s, e)

      merged_type = type(meta, expr_type, specs.type, e)
      validate_size_required(meta, expect_size, expr_type, merged_type, specs.size, es)
      size_and_unit = size_and_unit(meta, expr_type, specs.size, specs.unit, es)
      alignment = compute_alignment(merged_type, specs.size, specs.unit)

      maybe_inferred_size =
        case {expect_size, merged_type, size_and_unit} do
          {{:infer, pinned_var}, :binary, []} ->
            [{:size, meta, [{{:., meta, [:erlang, :byte_size]}, meta, [pinned_var]}]}]

          {{:infer, pinned_var}, :bitstring, []} ->
            [{:size, meta, [{{:., meta, [:erlang, :bit_size]}, meta, [pinned_var]}]}]

          _ ->
            size_and_unit
        end

      [h | t] =
        build_spec(
          meta,
          specs.size,
          specs.unit,
          merged_type,
          specs.endianness,
          specs.sign,
          maybe_inferred_size,
          es
        )

      {Enum.reduce(t, h, fn i, acc -> {:-, meta, [acc, i]} end), alignment, ss, es}
    end

    defp type(_, :default, :default, _), do: :integer
    defp type(_, expr_type, :default, _), do: expr_type

    defp type(_, :binary, type, _) when type in [:binary, :bitstring, :utf8, :utf16, :utf32],
      do: type

    defp type(_, :bitstring, type, _) when type in [:binary, :bitstring], do: type

    defp type(_, :integer, type, _) when type in [:integer, :float, :utf8, :utf16, :utf32],
      do: type

    defp type(_, :float, :float, _), do: :float
    defp type(_, :default, type, _), do: type

    defp type(_meta, _other, type, _e) do
      # function_error(meta, e, __MODULE__, {:bittype_mismatch, type, other, :type})
      type
    end

    defp expand_each_spec(meta, [{expr, meta_e, args} = h | t], map, s, original_s, e)
         when is_atom(expr) do
      case validate_spec(expr, args) do
        {key, arg} ->
          # if args != [], do: :ok, else: file_warn(meta, e, __MODULE__, {:parens_bittype, expr})

          {value, se, ee} = expand_spec_arg(arg, s, original_s, e)
          validate_spec_arg(meta, key, value, se, original_s, ee)

          case Map.get(map, key, :default) do
            :default ->
              :ok

            ^value ->
              :ok

            _other ->
              # function_error(meta, e, __MODULE__, {:bittype_mismatch, value, _other, key})
              :ok
          end

          expand_each_spec(meta, t, Map.put(map, key, value), se, original_s, ee)

        :none ->
          ha =
            if args == nil do
              # file_warn(meta, e, __MODULE__, {:unknown_bittype, expr})
              {expr, meta_e, []}
            else
              h
            end

          # TODO not call it here
          case Macro.expand(ha, Map.put(e, :line, ElixirUtils.get_line(meta))) do
            ^ha ->
              # function_error(meta, e, __MODULE__, {:undefined_bittype, h})
              expand_each_spec(meta, t, map, s, original_s, e)

            new_types ->
              expand_each_spec(meta, unpack_specs(new_types, []) ++ t, map, s, original_s, e)
          end
      end
    end

    defp expand_each_spec(meta, [_expr | tail], map, s, original_s, e) do
      # function_error(meta, e, __MODULE__, {:undefined_bittype, expr})
      expand_each_spec(meta, tail, map, s, original_s, e)
    end

    defp expand_each_spec(_meta, [], map, s, _original_s, e), do: {map, s, e}

    defp compute_alignment(_, size, unit) when is_integer(size) and is_integer(unit),
      do: rem(size * unit, 8)

    defp compute_alignment(:default, size, unit), do: compute_alignment(:integer, size, unit)
    defp compute_alignment(:integer, :default, unit), do: compute_alignment(:integer, 8, unit)
    defp compute_alignment(:integer, size, :default), do: compute_alignment(:integer, size, 1)
    defp compute_alignment(:bitstring, size, :default), do: compute_alignment(:bitstring, size, 1)
    defp compute_alignment(:binary, size, :default), do: compute_alignment(:binary, size, 8)
    defp compute_alignment(:binary, _, _), do: 0
    defp compute_alignment(:float, _, _), do: 0
    defp compute_alignment(:utf32, _, _), do: 0
    defp compute_alignment(:utf16, _, _), do: 0
    defp compute_alignment(:utf8, _, _), do: 0
    defp compute_alignment(_, _, _), do: :unknown

    defp alignment(left, right) when is_integer(left) and is_integer(right) do
      rem(left + right, 8)
    end

    defp alignment(_, _), do: :unknown

    defp extract_meta({_, meta, _}, _), do: meta
    defp extract_meta(_, meta), do: meta

    defp infer_spec(:bitstring, meta), do: {:bitstring, meta, nil}
    defp infer_spec(:binary, meta), do: {:binary, meta, nil}
    defp infer_spec(:float, meta), do: {:float, meta, nil}
    defp infer_spec(:integer, meta), do: {:integer, meta, nil}
    defp infer_spec(:default, meta), do: {:integer, meta, nil}

    defp expr_type(integer) when is_integer(integer), do: :integer
    defp expr_type(float) when is_float(float), do: :float
    defp expr_type(binary) when is_binary(binary), do: :binary
    defp expr_type({:<<>>, _, _}), do: :bitstring
    defp expr_type(_), do: :default

    defp concat_or_prepend_bitstring(_meta, {:<<>>, _, []}, _e_right, acc, _e, _require_size),
      do: acc

    defp concat_or_prepend_bitstring(
           meta,
           {:<<>>, parts_meta, parts} = e_left,
           e_right,
           acc,
           e,
           require_size
         ) do
      case e do
        %{context: :match} when require_size ->
          case List.last(parts) do
            {:"::", _spec_meta, [bin, {:binary, _, nil}]} when not is_binary(bin) ->
              # function_error(spec_meta, e, __MODULE__, :unsized_binary)
              :ok

            {:"::", _spec_meta, [_, {:bitstring, _, nil}]} ->
              # function_error(spec_meta, e, __MODULE__, :unsized_binary)
              :ok

            _ ->
              :ok
          end

        _ ->
          :ok
      end

      case e_right do
        {:binary, _, nil} ->
          {alignment, alignment} = Keyword.fetch!(parts_meta, :alignment)

          if is_integer(alignment) and alignment != 0 do
            # function_error(meta, e, __MODULE__, {:unaligned_binary, e_left})
            Enum.reverse(parts, acc)
          else
            [{:"::", meta, [e_left, e_right]} | acc]
          end

        {:bitstring, _, nil} ->
          Enum.reverse(parts, acc)
      end
    end

    defp concat_or_prepend_bitstring(meta, e_left, e_right, acc, _e, _require_size) do
      [{:"::", meta, [e_left, e_right]} | acc]
    end

    defp unpack_specs({:-, _, [h, t]}, acc), do: unpack_specs(h, unpack_specs(t, acc))

    defp unpack_specs({:*, _, [{:_, _, atom}, unit]}, acc) when is_atom(atom),
      do: [{:unit, [], [unit]} | acc]

    defp unpack_specs({:*, _, [size, unit]}, acc),
      do: [{:size, [], [size]}, {:unit, [], [unit]} | acc]

    defp unpack_specs(size, acc) when is_integer(size), do: [{:size, [], [size]} | acc]

    defp unpack_specs({expr, meta, args}, acc) when is_atom(expr) do
      list_args =
        cond do
          is_atom(args) -> nil
          is_list(args) -> args
          true -> args
        end

      [{expr, meta, list_args} | acc]
    end

    defp unpack_specs(other, acc), do: [other | acc]

    defp validate_spec(spec, []), do: validate_spec(spec, nil)
    defp validate_spec(:big, nil), do: {:endianness, :big}
    defp validate_spec(:little, nil), do: {:endianness, :little}
    defp validate_spec(:native, nil), do: {:endianness, :native}
    defp validate_spec(:size, [size]), do: {:size, size}
    defp validate_spec(:unit, [unit]), do: {:unit, unit}
    defp validate_spec(:integer, nil), do: {:type, :integer}
    defp validate_spec(:float, nil), do: {:type, :float}
    defp validate_spec(:binary, nil), do: {:type, :binary}
    defp validate_spec(:bytes, nil), do: {:type, :binary}
    defp validate_spec(:bitstring, nil), do: {:type, :bitstring}
    defp validate_spec(:bits, nil), do: {:type, :bitstring}
    defp validate_spec(:utf8, nil), do: {:type, :utf8}
    defp validate_spec(:utf16, nil), do: {:type, :utf16}
    defp validate_spec(:utf32, nil), do: {:type, :utf32}
    defp validate_spec(:signed, nil), do: {:sign, :signed}
    defp validate_spec(:unsigned, nil), do: {:sign, :unsigned}
    defp validate_spec(_, _), do: :none

    defp expand_spec_arg(expr, s, _original_s, e) when is_atom(expr) or is_integer(expr) do
      {expr, s, e}
    end

    defp expand_spec_arg(expr, s, original_s, %{context: :match} = e) do
      %{prematch: {pre_read, pre_counter, _} = old_pre} = s
      %{vars: {original_read, _}} = original_s
      new_pre = {pre_read, pre_counter, {:bitsize, original_read}}

      {e_expr, se, ee} =
        ElixirExpand.expand(expr, %{s | prematch: new_pre}, %{e | context: :guard})

      {e_expr, %{se | prematch: old_pre}, %{ee | context: :match}}
    end

    defp expand_spec_arg(expr, s, original_s, e) do
      ElixirExpand.expand(expr, ElixirEnv.reset_read(s, original_s), e)
    end

    defp validate_spec_arg(_meta, :unit, value, _s, _original_s, _e) when not is_integer(value) do
      # function_error(meta, e, __MODULE__, {:bad_unit_argument, value})
      :ok
    end

    defp validate_spec_arg(_meta, _key, _value, _s, _original_s, _e), do: :ok

    defp validate_size_required(_meta, :required, :default, type, :default, _e)
         when type in [:binary, :bitstring] do
      # function_error(meta, e, __MODULE__, :unsized_binary)
      :ok
    end

    defp validate_size_required(_, _, _, _, _, _), do: :ok

    defp size_and_unit(_meta, :bitstring, size, unit, _e)
         when size != :default or unit != :default do
      # function_error(meta, e, __MODULE__, :bittype_literal_bitstring)
      []
    end

    defp size_and_unit(_meta, :binary, size, unit, _e)
         when size != :default or unit != :default do
      # function_error(meta, e, __MODULE__, :bittype_literal_string)
      []
    end

    defp size_and_unit(_meta, _expr_type, size, unit, _e) do
      add_arg(:unit, unit, add_arg(:size, size, []))
    end

    defp build_spec(_meta, size, unit, type, endianness, sign, spec, _e)
         when type in [:utf8, :utf16, :utf32] do
      cond do
        size != :default or unit != :default ->
          # function_error(meta, e, __MODULE__, :bittype_utf)
          :ok

        sign != :default ->
          # function_error(meta, e, __MODULE__, :bittype_signed)
          :ok

        true ->
          :ok
      end

      add_spec(type, add_spec(endianness, spec))
    end

    defp build_spec(_meta, _size, unit, type, _endianness, sign, spec, _e)
         when type in [:binary, :bitstring] do
      cond do
        type == :bitstring and unit != :default and unit != 1 ->
          # function_error(meta, e, __MODULE__, {:bittype_mismatch, unit, 1, :unit})
          :ok

        sign != :default ->
          # function_error(meta, e, __MODULE__, :bittype_signed)
          :ok

        true ->
          :ok
      end

      add_spec(type, spec)
    end

    defp build_spec(_meta, size, unit, type, endianness, sign, spec, _e)
         when type in [:integer, :float] do
      number_size = number_size(size, unit)

      cond do
        type == :float and is_integer(number_size) ->
          if valid_float_size(number_size) do
            add_spec(type, add_spec(endianness, add_spec(sign, spec)))
          else
            # function_error(meta, e, __MODULE__, {:bittype_float_size, number_size})
            []
          end

        size == :default and unit != :default ->
          # function_error(meta, e, __MODULE__, :bittype_unit)
          []

        true ->
          add_spec(type, add_spec(endianness, add_spec(sign, spec)))
      end
    end

    defp add_spec(:default, spec), do: spec
    defp add_spec(key, spec), do: [{key, [], nil} | spec]

    defp number_size(size, :default) when is_integer(size), do: size
    defp number_size(size, unit) when is_integer(size), do: size * unit
    defp number_size(size, _), do: size

    defp valid_float_size(16), do: true
    defp valid_float_size(32), do: true
    defp valid_float_size(64), do: true
    defp valid_float_size(_), do: false

    defp add_arg(_key, :default, spec), do: spec
    defp add_arg(key, arg, spec), do: [{key, [], [arg]} | spec]

    defp is_match_size([_ | _], %{context: :match}), do: true
    defp is_match_size(_, _), do: false

    defp find_match([{:=, _, [_left, _right]} = expr | _rest]), do: expr

    defp find_match([{_, _, args} | rest]) when is_list(args) do
      case find_match(args) do
        false -> find_match(rest)
        match -> match
      end
    end

    defp find_match([_arg | rest]), do: find_match(rest)

    defp find_match([]), do: false
  end

  defmodule Fn do
    alias ElixirSense.Core.Compiler, as: ElixirExpand
    alias ElixirSense.Core.Compiler.Env, as: ElixirEnv
    alias ElixirSense.Core.Compiler.Clauses, as: ElixirClauses
    alias ElixirSense.Core.Compiler.Dispatch, as: ElixirDispatch
    alias ElixirSense.Core.Compiler.Utils, as: ElixirUtils

    def expand(meta, clauses, s, e) when is_list(clauses) do
      transformer = fn
        {:->, _, [left, _right]} = clause, sa ->
          # elixir raises defaults_in_args
          left = sanitize_fn_arg(left)

          s_reset = ElixirEnv.reset_vars(sa)

          {e_clause, s_acc, e_acc} =
            ElixirClauses.clause(meta, :fn, &ElixirClauses.head/3, clause, s_reset, e)

          {e_clause, ElixirEnv.merge_vars(s_acc, sa, e_acc)}
      end

      {e_clauses, se} = Enum.map_reduce(clauses, s, transformer)

      {{:fn, meta, e_clauses}, se, e}
    end

    # TODO check if there is cursor in default
    defp sanitize_fn_arg({:"\\\\", _, [value, _default]}), do: value
    defp sanitize_fn_arg(value), do: value

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
      expr = case expr do
        [] -> {:"&1", meta, e.module}
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
      # TODO check similarity to macro expand_import
      res = sequential && ElixirDispatch.import_function(import_meta, atom, length(args), e)
      handle_capture(res, import_meta, import_meta, expr, s, e, sequential)
    end

    defp capture_require({{:., dot_meta, [left, right]}, require_meta, args}, s, e, sequential) do
      case escape(left, []) do
        {esc_left, []} ->
          {e_left, se, ee} = ElixirExpand.expand(esc_left, s, e)

          res =
            sequential &&
              case e_left do
                {name, _, context} when is_atom(name) and is_atom(context) ->
                  {:remote, e_left, right, length(args)}

                _ when is_atom(e_left) ->
                  # TODO check similarity to macro expand_require
                  ElixirDispatch.require_function(require_meta, e_left, right, length(args), ee)

                _ ->
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
          e_vars = Enum.map(e_dict, & elem(&1, 1))
          fn_expr = {:fn, meta, [{:->, meta, [e_vars, e_expr]}]}
          {:expand, fn_expr, s, e}
      end
    end

    defp escape({:&, meta, [pos]}, dict) when is_integer(pos) and pos > 0 do
      # Using a nil context here to emit warnings when variable is unused.
      # This might pollute user space but is unlikely because variables
      # named :"&1" are not valid syntax.
      var = {:"&#{pos}", meta, nil}
      {var, :orddict.store(pos, var, dict)}
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

    defp args_from_arity(_meta, a) do
      # elixir raises invalid_arity_for_capture
      []
    end

    defp is_sequential_and_not_empty([]), do: false
    defp is_sequential_and_not_empty(list), do: is_sequential(list, 1)
    # TODO need to understand if we need it
    defp is_sequential([{:&, _, [int]} | t], int), do: is_sequential(t, int + 1)
    defp is_sequential([], _int), do: true
    defp is_sequential(_, _int), do: false
  end

  defmodule Quote do
    alias ElixirSense.Core.Compiler.Dispatch, as: ElixirDispatch

    defstruct line: false,
              file: nil,
              context: nil,
              vars_hygiene: true,
              aliases_hygiene: true,
              imports_hygiene: true,
              unquote: true,
              generated: false

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

    def build(meta, line, file, context, unquote, generated) do
      acc0 = []

      {e_line, acc1} = validate_compile(meta, :line, line, acc0)
      {e_file, acc2} = validate_compile(meta, :file, file, acc1)
      {e_context, acc3} = validate_compile(meta, :context, context, acc2)

      validate_runtime(:unquote, unquote)
      validate_runtime(:generated, generated)

      q = %__MODULE__{
        line: e_line,
        file: e_file,
        unquote: unquote,
        context: e_context,
        generated: generated
      }

      {q, acc3}
    end

    def validate_compile(_meta, :line, value, acc) when is_boolean(value) do
      {value, acc}
    end

    def validate_compile(_meta, :file, nil, acc) do
      {nil, acc}
    end

    def validate_compile(meta, key, value, acc) do
      case is_valid(key, value) do
        true ->
          {value, acc}

        false ->
          var = {key, meta, __MODULE__}
          call = {{:., meta, [__MODULE__, :validate_runtime]}, meta, [key, value]}
          {var, [{:=, meta, [var, call]} | acc]}
      end
    end

    def validate_runtime(key, value) do
      case is_valid(key, value) do
        true ->
          value

        false ->
          raise ArgumentError,
                "invalid runtime value for option :#{Atom.to_string(key)} in quote, got: #{inspect(value)}"
      end
    end

    def is_valid(:line, line), do: is_integer(line)
    def is_valid(:file, file), do: is_binary(file)
    def is_valid(:context, context), do: is_atom(context) and context != nil
    def is_valid(:generated, generated), do: is_boolean(generated)
    def is_valid(:unquote, unquote), do: is_boolean(unquote)

    def escape(expr, kind, unquote) do
      do_quote(
        expr,
        %__MODULE__{
          line: true,
          file: nil,
          vars_hygiene: false,
          aliases_hygiene: false,
          imports_hygiene: false,
          unquote: unquote
        },
        kind
      )
    end

    def quote(_meta, {:unquote_splicing, _, [_]}, _binding, %__MODULE__{unquote: true}, _, _),
      do: raise("unquote_splicing only works inside arguments and block contexts")

    def quote(meta, expr, binding, q, prelude, e) do
      context = q.context

      vars =
        Enum.map(binding, fn {k, v} ->
          {:{}, [], [:=, [], [{:{}, [], [k, meta, context]}, v]]}
        end)

      quoted = do_quote(expr, q, e)

      with_vars =
        case vars do
          [] -> quoted
          _ -> {:{}, [], [:__block__, [], vars ++ [quoted]]}
        end

      case prelude do
        [] -> with_vars
        _ -> {:__block__, [], prelude ++ [with_vars]}
      end
    end

    # quote/unquote

    defp do_quote({:quote, meta, [arg]}, q, e) do
      t_arg = do_quote(arg, %__MODULE__{q | unquote: false}, e)

      new_meta =
        case q do
          %__MODULE__{vars_hygiene: true, context: context} ->
            keystore(:context, meta, context)

          _ ->
            meta
        end

      {:{}, [], [:quote, meta(new_meta, q), [t_arg]]}
    end

    defp do_quote({:quote, meta, [opts, arg]}, q, e) do
      t_opts = do_quote(opts, q, e)
      t_arg = do_quote(arg, %__MODULE__{q | unquote: false}, e)

      new_meta =
        case q do
          %__MODULE__{vars_hygiene: true, context: context} ->
            keystore(:context, meta, context)

          _ ->
            meta
        end

      {:{}, [], [:quote, meta(new_meta, q), [t_opts, t_arg]]}
    end

    defp do_quote({:unquote, _meta, [expr]}, %__MODULE__{unquote: true}, _), do: expr

    # Aliases

    defp do_quote({:__aliases__, meta, [h | t] = list}, %__MODULE__{aliases_hygiene: true} = q, e)
         when is_atom(h) and h != :"Elixir" do
      annotation =
        case Macro.Env.expand_alias(e, meta, list, trace: false) do
          {:alias, atom} -> atom
          :error -> false
        end

      alias_meta = keystore(:alias, Keyword.delete(meta, :counter), annotation)
      do_quote_tuple(:__aliases__, alias_meta, [h | t], q, e)
    end

    # Vars

    defp do_quote({name, meta, nil}, %__MODULE__{vars_hygiene: true} = q, e)
         when is_atom(name) and is_list(meta) do
      import_meta =
        if q.imports_hygiene do
          import_meta(meta, name, 0, q, e)
        else
          meta
        end

      {:{}, [], [name, meta(import_meta, q), q.context]}
    end

    # Unquote

    defp do_quote(
           {{{:., meta, [left, :unquote]}, _, [expr]}, _, args},
           %__MODULE__{unquote: true} = q,
           e
         ) do
      do_quote_call(left, meta, expr, args, q, e)
    end

    defp do_quote({{:., meta, [left, :unquote]}, _, [expr]}, %__MODULE__{unquote: true} = q, e) do
      do_quote_call(left, meta, expr, nil, q, e)
    end

    # Imports

    defp do_quote(
           {:&, meta, [{:/, _, [{f, _, c}, a]}] = args},
           %__MODULE__{imports_hygiene: true} = q,
           e
         )
         when is_atom(f) and is_integer(a) and is_atom(c) do
      new_meta =
        case ElixirDispatch.find_import(meta, f, a, e) do
          false ->
            meta

          receiver ->
            keystore(:context, keystore(:imports, meta, [{a, receiver}]), q.context)
        end

      do_quote_tuple(:&, new_meta, args, q, e)
    end

    defp do_quote({name, meta, args_or_context}, %__MODULE__{imports_hygiene: true} = q, e)
         when is_atom(name) and is_list(meta) and
                (is_list(args_or_context) or is_atom(args_or_context)) do
      arity =
        case args_or_context do
          args when is_list(args) -> length(args)
          _context when is_atom(args_or_context) -> 0
        end

      import_meta = import_meta(meta, name, arity, q, e)
      annotated = annotate({name, import_meta, args_or_context}, q.context)
      do_quote_tuple(annotated, q, e)
    end

    # Two-element tuples

    defp do_quote({left, right}, %__MODULE__{unquote: true} = q, e)
         when is_tuple(left) and elem(left, 0) == :unquote_splicing and
                is_tuple(right) and elem(right, 0) == :unquote_splicing do
      do_quote({:{}, [], [left, right]}, q, e)
    end

    defp do_quote({left, right}, q, e) do
      t_left = do_quote(left, q, e)
      t_right = do_quote(right, q, e)
      {t_left, t_right}
    end

    # Everything else

    defp do_quote(other, q, e) when is_atom(e) do
      do_escape(other, q, e)
    end

    defp do_quote({_, _, _} = tuple, q, e) do
      annotated = annotate(tuple, q.context)
      do_quote_tuple(annotated, q, e)
    end

    defp do_quote([], _, _), do: []

    defp do_quote([h | t], %__MODULE__{unquote: false} = q, e) do
      head_quoted = do_quote(h, q, e)
      do_quote_simple_list(t, head_quoted, q, e)
    end

    defp do_quote([h | t], q, e) do
      do_quote_tail(:lists.reverse(t, [h]), q, e)
    end

    defp do_quote(other, _, _), do: other

    defp import_meta(meta, name, arity, q, e) do
      case Keyword.get(meta, :import, false) == false &&
             ElixirDispatch.find_imports(meta, name, e) do
        [] ->
          case arity == 1 && Keyword.fetch(meta, :ambiguous_op) do
            {:ok, nil} ->
              keystore(:ambiguous_op, meta, q.context)

            _ ->
              meta
          end

        imports ->
          keystore(:imports, keystore(:context, meta, q.context), imports)
      end
    end

    defp do_quote_call(left, meta, expr, args, q, e) do
      all = [left, {:unquote, meta, [expr]}, args, q.context]
      tall = Enum.map(all, fn x -> do_quote(x, q, e) end)
      {{:., meta, [:elixir_quote, :dot]}, meta, [meta(meta, q) | tall]}
    end

    defp do_quote_tuple({left, meta, right}, q, e) do
      do_quote_tuple(left, meta, right, q, e)
    end

    defp do_quote_tuple(left, meta, right, q, e) do
      t_left = do_quote(left, q, e)
      t_right = do_quote(right, q, e)
      {:{}, [], [t_left, meta(meta, q), t_right]}
    end

    defp do_quote_simple_list([], prev, _, _), do: [prev]

    defp do_quote_simple_list([h | t], prev, q, e) do
      [prev | do_quote_simple_list(t, do_quote(h, q, e), q, e)]
    end

    defp do_quote_simple_list(other, prev, q, e) do
      [{:|, [], [prev, do_quote(other, q, e)]}]
    end

    defp do_quote_tail(
           [{:|, meta, [{:unquote_splicing, _, [left]}, right]} | t],
           %__MODULE__{unquote: true} = q,
           e
         ) do
      tt = do_quote_splice(t, q, e, [], [])
      tr = do_quote(right, q, e)
      do_runtime_list(meta, :tail_list, [left, tr, tt])
    end

    defp do_quote_tail(list, q, e) do
      do_quote_splice(list, q, e, [], [])
    end

    defp do_quote_splice(
           [{:unquote_splicing, meta, [expr]} | t],
           %__MODULE__{unquote: true} = q,
           e,
           buffer,
           acc
         ) do
      runtime = do_runtime_list(meta, :list, [expr, do_list_concat(buffer, acc)])
      do_quote_splice(t, q, e, [], runtime)
    end

    defp do_quote_splice([h | t], q, e, buffer, acc) do
      th = do_quote(h, q, e)
      do_quote_splice(t, q, e, [th | buffer], acc)
    end

    defp do_quote_splice([], _q, _e, buffer, acc) do
      do_list_concat(buffer, acc)
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

    defp keep(meta, %__MODULE__{file: file}) do
      case Keyword.pop(meta, :line) do
        {nil, _} ->
          [{:keep, {file, 0}} | meta]

        {line, meta_no_line} ->
          [{:keep, {file, line}} | meta_no_line]
      end
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

    defp do_escape({left, meta, right}, q, e = :prune_metadata) do
      tm = for {k, v} <- meta, k == :no_parens or k == :line, do: {k, v}
      tl = do_quote(left, q, e)
      tr = do_quote(right, q, e)
      {:{}, [], [tl, tm, tr]}
    end

    defp do_escape(tuple, q, e) when is_tuple(tuple) do
      tt = do_quote(Tuple.to_list(tuple), q, e)
      {:{}, [], tt}
    end

    defp do_escape(bitstring, _, _) when is_bitstring(bitstring) do
      case Bitwise.band(bit_size(bitstring), 7) do
        0 ->
          bitstring

        size ->
          <<bits::size(size), bytes::binary>> = bitstring

          {:<<>>, [],
           [{:"::", [], [bits, {size, [], [size]}]}, {:"::", [], [bytes, {:binary, [], nil}]}]}
      end
    end

    defp do_escape(map, q, e) when is_map(map) do
      tt = do_quote(Enum.sort(Map.to_list(map)), q, e)
      {:%{}, [], tt}
    end

    defp do_escape([], _, _), do: []

    defp do_escape([h | t], %__MODULE__{unquote: false} = q, e) do
      do_quote_simple_list(t, do_quote(h, q, e), q, e)
    end

    defp do_escape([h | t], q, e) do
      # The improper case is inefficient, but improper lists are rare.
      try do
        l = Enum.reverse(t, [h])
        do_quote_tail(l, q, e)
      catch
        _ ->
          {l, r} = reverse_improper(t, [h])
          tl = do_quote_splice(l, q, e, [], [])
          tr = do_quote(r, q, e)
          update_last(tl, fn x -> {:|, [], [x, tr]} end)
      end
    end

    defp do_escape(other, _, _) when is_number(other) or is_pid(other) or is_atom(other),
      do: other

    defp do_escape(fun, _, _) when is_function(fun) do
      case {Function.info(fun, :env), Function.info(fun, :type)} do
        {{:env, []}, {:type, :external}} ->
          fun_to_quoted(fun)

        _ ->
          raise ArgumentError
      end
    end

    defp do_escape(_other, _, _), do: raise(ArgumentError)

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
  end

  defmodule Dispatch do
    import :ordsets, only: [is_element: 2]

    def find_import(meta, name, arity, e) do
      tuple = {name, arity}

      case find_import_by_name_arity(meta, tuple, [], e) do
        {:function, receiver} ->
          # ElixirEnv.trace({:imported_function, meta, receiver, name, arity}, e)
          receiver

        {:macro, receiver} ->
          # ElixirEnv.trace({:imported_macro, meta, receiver, name, arity}, e)
          receiver

        _ ->
          false
      end
    end

    def find_imports(meta, name, e) do
      funs = e.functions
      macs = e.macros

      acc0 = %{}
      acc1 = find_imports_by_name(funs, acc0, name, meta, e)
      acc2 = find_imports_by_name(macs, acc1, name, meta, e)

      imports = acc2 |> Map.to_list() |> Enum.sort()
      # trace_import_quoted(imports, meta, name, e)
      imports
    end

    def import_function(meta, name, arity, e) do
      tuple = {name, arity}

      case find_import_by_name_arity(meta, tuple, [], e) do
        {:function, receiver} ->
          # ElixirEnv.trace({:imported_function, meta, receiver, name, arity}, e)
          # ElixirLocals.record_import(tuple, receiver, e.module, e.function)
          remote_function(meta, receiver, name, arity, e)

        {:macro, _receiver} ->
          false

        {:import, receiver} ->
          require_function(meta, receiver, name, arity, e)

        false ->
          if Macro.special_form?(name, arity) do
            false
          else
            function = e.function

            # TODO the condition has this at the end
            # and not ElixirDef.local_for(meta, name, arity, [:defmacro, :defmacrop], e)
            if function != nil and function != tuple do
              # ElixirEnv.trace({:local_function, meta, name, arity}, e)
              # ElixirLocals.record_local(tuple, e.module, function, meta, false)
              # TODO we may want to record
              {:local, name, arity}
            else
              false
            end
          end
      end
    end

    def require_function(meta, receiver, name, arity, e) do
      required = receiver in e.requires

      case is_macro(name, arity, receiver, required) do
        true ->
          false

        false ->
          # ElixirEnv.trace({:remote_function, meta, receiver, name, arity}, e)
          remote_function(meta, receiver, name, arity, e)
      end
    end

    defp remote_function(_meta, receiver, name, arity, _e) do
      # check_deprecated(:function, meta, receiver, name, arity, e)

      # TODO rewrite is safe to use as it does not emit traces and does not have side effects
      # but we may need to translate it anyway
      case :elixir_rewrite.inline(receiver, name, arity) do
        {ar, an} -> {:remote, ar, an, arity}
        false -> {:remote, receiver, name, arity}
      end
    end

    def find_imports_by_name([{mod, imports} | mod_imports], acc, name, meta, e) do
      new_acc = find_imports_by_name(name, imports, acc, mod, meta, e)
      find_imports_by_name(mod_imports, new_acc, name, meta, e)
    end

    def find_imports_by_name([], acc, _name, _meta, _e), do: acc

    def find_imports_by_name(name, [{name, arity} | imports], acc, mod, meta, e) do
      case Map.get(acc, arity) do
        nil ->
          find_imports_by_name(name, imports, Map.put(acc, arity, mod), mod, meta, e)

        _other_mod ->
          raise "ambiguous_call"
      end
    end

    def find_imports_by_name(name, [{import_name, _} | imports], acc, mod, meta, e)
        when name > import_name do
      find_imports_by_name(name, imports, acc, mod, meta, e)
    end

    def find_imports_by_name(_name, _imports, acc, _mod, _meta, _e), do: acc

    defp find_import_by_name_arity(meta, {_name, arity} = tuple, extra, e) do
      case is_import(meta, arity) do
        {:import, _} = import_res ->
          import_res

        false ->
          funs = e.functions
          macs = extra ++ e.macros
          fun_match = find_import_by_name_arity(tuple, funs)
          mac_match = find_import_by_name_arity(tuple, macs)

          case {fun_match, mac_match} do
            {[], [receiver]} ->
              {:macro, receiver}

            {[receiver], []} ->
              {:function, receiver}

            {[], []} ->
              false

            _ ->
              raise "ambiguous_call"
          end
      end
    end

    defp find_import_by_name_arity(tuple, list) do
      for {receiver, set} <- list, is_element(tuple, set), do: receiver
    end

    defp is_import(meta, arity) do
      with {:ok, imports} <- Keyword.fetch(meta, :imports),
           {:ok, _} <- Keyword.fetch(meta, :context),
           {:ok, receiver} <- Keyword.fetch(imports, arity) do
        {:import, receiver}
      else
        _ -> false
      end
    end

    defp is_macro(_name, _arity, _module, false), do: false

    defp is_macro(name, arity, receiver, true) do
      try do
        # TODO is it OK for local requires?
        macros = receiver.__info__(:macros)
        {name, arity} in macros
      rescue
        _error -> false
      end
    end
  end

  defmodule Map do
    alias ElixirSense.Core.Compiler, as: ElixirExpand

    def expand_struct(meta, left, {:%{}, map_meta, map_args}, s, %{context: context} = e) do
      clean_map_args = clean_struct_key_from_map_args(map_args)

      {[e_left, e_right], se, ee} =
        ElixirExpand.expand_args([left, {:%{}, map_meta, clean_map_args}], s, e)

      case validate_struct(e_left, context) do
        true when is_atom(e_left) ->
          case extract_struct_assocs(e_right) do
            {:expand, map_meta, assocs} when context != :match ->
              assoc_keys = Enum.map(assocs, fn {k, _} -> k end)
              struct = load_struct(e_left, assocs, se, ee)
              keys = [:__struct__ | assoc_keys]
              without_keys = Elixir.Map.drop(struct, keys)
              # TODO is escape safe?
              struct_assocs = Macro.escape(Enum.sort(Elixir.Map.to_list(without_keys)))
              {{:%, meta, [e_left, {:%{}, map_meta, struct_assocs ++ assocs}]}, se, ee}

            {_, _, _assocs} ->
              # elixir validates assocs against struct keys
              # we don't need to validate keys
              {{:%, meta, [e_left, e_right]}, se, ee}
          end

        _ ->
          # elixir raises invalid_struct_name if validate_struct returns false
          {{:%, meta, [e_left, e_right]}, se, ee}
      end
    end

    def expand_struct(meta, left, right, s, e) do
      # elixir raises here non_map_after_struct
      # try to recover from error by wrapping the expression in map
      expand_struct(meta, left, wrap_in_fake_map(right), s, e)
    end

    defp wrap_in_fake_map(right) do
      map_args = case right do
        list when is_list(list) ->
          if Keyword.keyword?(list) do
            list
          else
            [__fake_key__: list]
          end
        _ ->
          [__fake_key__: right]
      end
      {:%{}, [], map_args}
    end

    def expand_map(meta, [{:|, update_meta, [left, right]}], s, e) do
      # elixir raises update_syntax_in_wrong_context if e.context is not nil
      {[e_left | e_right], se, ee} = ElixirExpand.expand_args([left | right], s, e)
      e_right = sanitize_kv(e_right, e)
      {{:%{}, meta, [{:|, update_meta, [e_left, e_right]}]}, se, ee}
    end

    def expand_map(meta, args, s, e) do
      {e_args, se, ee} = ElixirExpand.expand_args(args, s, e)
      e_args = sanitize_kv(e_args, e)
      {{:%{}, meta, e_args}, se, ee}
    end

    defp clean_struct_key_from_map_args([{:|, pipe_meta, [left, map_assocs]}]) do
      [{:|, pipe_meta, [left, delete_struct_key(map_assocs)]}]
    end

    defp clean_struct_key_from_map_args(map_assocs) do
      delete_struct_key(map_assocs)
    end

    defp sanitize_kv(kv, %{context: context}) do
      Enum.filter(kv, fn
        {k, _v} ->
          if context == :match do
            validate_match_key(k)
          else
            true
          end

        _ ->
          false
      end)
    end

    defp validate_match_key({name, _, context})
         when is_atom(name) and is_atom(context) do
      # elixir raises here invalid_variable_in_map_key_match
      false
    end

    defp validate_match_key({:"::", _, [left, _]}) do
      validate_match_key(left)
    end

    defp validate_match_key({:^, _, [{name, _, context}]})
         when is_atom(name) and is_atom(context),
         do: true

    defp validate_match_key({:%{}, _, [_ | _]}), do: true

    defp validate_match_key({left, _, right}) do
      validate_match_key(left) and validate_match_key(right)
    end

    defp validate_match_key({left, right}) do
      validate_match_key(left) and validate_match_key(right)
    end

    defp validate_match_key(list) when is_list(list) do
      Enum.all?(list, &validate_match_key/1)
    end

    defp validate_match_key(_), do: true

    defp validate_struct({:^, _, [{var, _, ctx}]}, :match) when is_atom(var) and is_atom(ctx),
      do: true

    defp validate_struct({var, _meta, ctx}, :match) when is_atom(var) and is_atom(ctx), do: true
    defp validate_struct(atom, _) when is_atom(atom), do: true
    defp validate_struct(_, _), do: false

    defp sanitize_assocs(list) do
      Enum.filter(list, &match?({k, _} when is_atom(k), &1))
    end

    defp extract_struct_assocs({:%{}, meta, [{:|, _, [_, assocs]}]}) do
      {:update, meta, delete_struct_key(sanitize_assocs(assocs))}
    end

    defp extract_struct_assocs({:%{}, meta, assocs}) do
      {:expand, meta, delete_struct_key(sanitize_assocs(assocs))}
    end

    defp extract_struct_assocs(right) do
      # elixir raises here non_map_after_struct
      # try to recover from error by wrapping the expression in map
      extract_struct_assocs(wrap_in_fake_map(right))
    end

    defp delete_struct_key(assocs) do
      Keyword.delete(assocs, :__struct__)
    end

    defp load_struct(name, assocs, s, _e) do
      case s.structs[name] do
        nil ->
          try do
            apply(name, :__struct__, [assocs])
          else
            %{:__struct__ => ^name} = struct ->
              struct

            _ ->
              # recover from invalid return value
              [__struct__: name] |> Keyword.merge(assocs) |> Elixir.Map.new()
          rescue
            _ ->
              # recover from error by building the fake struct
              [__struct__: name] |> Keyword.merge(assocs) |> Elixir.Map.new()
          end

        info ->
          info.fields |> Keyword.merge(assocs) |> Elixir.Map.new()
      end
    end
  end

  defmodule Typespec do
    alias ElixirSense.Core.Compiler, as: ElixirExpand
    def spec_to_signature({:when, _, [spec, _]}), do: type_to_signature(spec)
    def spec_to_signature(other), do: type_to_signature(other)

    def type_to_signature({:"::", _, [{name, _, context}, _]})
        when is_atom(name) and name != :"::" and is_atom(context),
        do: {name, []}

    def type_to_signature({:"::", _, [{name, _, args}, _]})
        when is_atom(name) and name != :"::",
      do: {name, args}

    def type_to_signature(_), do: :error

    def expand_spec(ast, state, env) do
      # TODO not sure this is correct. Are module vars accessible?
      state = state
      |> new_func_vars_scope

      {ast, state, env} = do_expand_spec(ast, state, env)

      state = state
      |> remove_func_vars_scope

      {ast, state, env}
    end

    defp do_expand_spec({:when, meta, [spec, guard]}, state, env) when is_list(guard) do
      {spec, guard, state, env} = do_expand_spec(spec, guard, meta, state, env)
      {{:when, meta, [spec, guard]}, state, env}
    end
    defp do_expand_spec(spec, state, env) do
      {spec, guard, state, env} = do_expand_spec(spec, [], [], state, env)
      {spec, state, env}
    end

    defp do_expand_spec({:"::", meta, [{name, name_meta, args}, return]}, guard, guard_meta, state, env)
      when is_atom(name) and name != :"::" do
        args = if is_atom(args) do
          []
        else
          args
        end
        |> sanitize_args()

      guard = if Keyword.keyword?(guard), do: guard, else: []

      state = Enum.reduce(guard, state, fn {name, val}, state ->
        # guard is a keyword list so we don't have exact meta on keys
        add_var_write(state, {name, guard_meta, nil})
      end)

      {args_reverse, state, env} = Enum.reduce(args, {[], state, env}, fn arg, {acc, state, env} ->
        {arg, state, env} = expand_typespec(arg, state, env)
        {[arg | acc], state, env}
      end)
      args = Enum.reverse(args_reverse)

      {return, state, env} = expand_typespec(return, state, env)

      {guard_reverse, state, env} = Enum.reduce(guard, {[], state, env}, fn
        {_name, {:var, _, context}} = pair, {acc, state, env} when is_atom(context) ->
          # special type var
          {[pair | acc], state, env}
        {name, type}, {acc, state, env} ->
          {type, state, env} = expand_typespec(type, state, env)
          {[{name, type} | acc], state, env}
      end)
      guard = Enum.reverse(guard_reverse)

      {{:"::", meta, [{name, name_meta, args}, return]}, guard, state, env}
    end

    defp do_expand_spec(other, guard, _guard_meta, state, env) do
      # invalid or incomplete spec
      # TODO try to wrap in :: expression
      {other, guard, state, env}
    end

    defp sanitize_args(args) do
      Enum.map(args, fn
        {:"::", meta, [left, right]} ->
          {:"::", meta, [remove_default(left), remove_default(right)]}
  
        other ->
          remove_default(other)
      end)
    end
  
    defp remove_default({:\\, _, [left, _]}), do: left
    defp remove_default(other), do: other


    def expand_type(ast, state, env) do
      state = state
      |> new_func_vars_scope

      {ast, state, env} = do_expand_type(ast, state, env)

      state = state
      |> remove_func_vars_scope

      {ast, state, env}
    end

    defp do_expand_type({:"::", meta, [{name, name_meta, args}, definition]}, state, env) do
      args = if is_atom(args) do
        []
      else
        args
      end

      state = Enum.reduce(args, state, fn
        {name, meta, context}, state when is_atom(name) and is_atom(context) and name != :_ ->
          add_var_write(state, {name, meta, context})
         _, state ->
          # silently skip invalid typespec params
          state
      end)

      {definition, state, env} = expand_typespec(definition, state, env)
      {{:"::", meta, [{name, name_meta, args}, definition]}, state, env}
    end

    defp do_expand_type(other, state, env) do
      # invalid or incomplete spec
      # TODO try to wrap in :: expression
      {other, state, env}
    end

    @special_forms [
      :|, :<<>>, :%{}, :%, :.., :->, :"::", :+, :-, :., :{}, :__block__, :...
    ]

    defp expand_typespec(ast, state, env) do
      # TODO this should handle remote calls, attributes unquotes?
      # TODO attribute remote call should expand attribute
      # {{:., meta, [{:@, _, [{attr, _, _}]}, name]}, _, args}
      # TODO remote call should expand remote
      # {{:., meta, [remote, name]}, _, args}
      # TODO expand struct module
      # {:%, _, [name, {:%{}, meta, fields}]}
      {ast, {state, env}} =
        Macro.traverse(ast, {state, env}, fn
          {:__aliases__, _meta, list} = node, {state, env} when is_list(list) ->
            {node, state, env} = ElixirExpand.expand(node, state, env)
            {node, {state, env}}

          {:__MODULE__, _meta, ctx} = node, {state, env} when is_atom(ctx) ->
            {node, state, env} = ElixirExpand.expand(node, state, env)
            {node, {state, env}}
          
          {:"::", meta, [{var_name, var_meta, context}, expr]}, {state, env} when is_atom(var_name) and is_atom(context) ->
            # mark as annotation
          {{:"::", meta, [{var_name, [{:annotation, true} | var_meta], context}, expr]}, {state, env}}

          {name, meta, args}, {state, env} when is_atom(name) and is_atom(args) and name not in @special_forms and hd(meta) != {:annotation, true} ->
            [vars_from_scope | _other_vars] = state.vars_info
            ast = case Elixir.Map.get(vars_from_scope, {name, nil}) do
              nil ->
                # add parens to no parens local call
                {name, meta, []}
              _ ->
                {name, meta, args}
            end

            {ast, {state, env}}

          other, acc ->
            {other, acc}
        end, fn
          {{:., dot_meta, [remote, name]}, meta, args}, {state, env} when is_atom(remote) ->
            line = Keyword.get(meta, :line, 0)
            column = Keyword.get(meta, :column, nil)
            args = if is_atom(args) do
              []
            else
              args
            end

            state = add_call_to_line(state, {remote, name, length(args)}, {line, column})

            {{{:., dot_meta, [remote, name]}, meta, args}, {state, env}}

          {name, meta, args}, {state, env} when is_atom(name) and is_list(args) and name not in @special_forms ->
            line = Keyword.get(meta, :line, 0)
            column = Keyword.get(meta, :column, nil)

            state = add_call_to_line(state, {nil, name, length(args)}, {line, column})

            {{name, meta, args}, {state, env}}
          {name, meta, context} = var, {state, env} when is_atom(name) and is_atom(context) and hd(meta) != {:annotation, true} ->
            state = add_var_read(state, var)
            {var, {state, env}}
          other, acc ->
            {other, acc}
        end)

      {ast, state, env}
    end
      # TODO: Remove char_list type by v2.0
    def built_in_type?(:char_list, 0), do: true
    def built_in_type?(:charlist, 0), do: true
    def built_in_type?(:as_boolean, 1), do: true
    def built_in_type?(:struct, 0), do: true
    def built_in_type?(:nonempty_charlist, 0), do: true
    def built_in_type?(:keyword, 0), do: true
    def built_in_type?(:keyword, 1), do: true
    def built_in_type?(:var, 0), do: true
    def built_in_type?(name, arity), do: :erl_internal.is_type(name, arity)
  end
end
