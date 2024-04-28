defmodule ElixirSense.Core.Compiler do
  import ElixirSense.Core.State, except: [expand: 2, expand: 3, no_alias_expansion: 1]
  alias ElixirSense.Core.State
  require Logger
  alias ElixirSense.Core.Introspection

  @env :elixir_env.new()
  def env, do: @env

  def expand(ast, state, env) do
    try do
      do_expand(ast, state, env)
    catch
      kind, payload ->
        Logger.warning("Unable to expand ast node #{inspect(ast)}: #{Exception.format(kind, payload, __STACKTRACE__)}")
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
    # IO.inspect(sl.scope_vars_info, label: "left")
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
        if Keyword.has_key?(opts, :as) do
          raise "as_in_multi_alias_call"
        end

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
      |> add_current_env_to_line(Keyword.fetch!(meta, :line), env)

    # no need to call expand_without_aliases_report - we never report
    {arg, state, env} = expand(arg, state, env)
    {opts, state, env} = expand_opts(meta, :alias, [:as, :warn], no_alias_opts(opts), state, env)

    if is_atom(arg) do
      # TODO check difference with
      # elixir_aliases:alias(Meta, Ref, IncludeByDefault, Opts, E, true)
      # TODO PR to elixir with is_atom(module) check?
      case Macro.Env.define_alias(env, meta, arg, [trace: false] ++ opts) do
        {:ok, env} ->
          {arg, state, env}

        {:error, _} ->
          raise "elixir_aliases"
      end
    else
      raise "expected_compile_time_module"
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
      expand_opts(meta, :require, [:as, :warn], no_alias_opts(opts), state, env)

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
              raise "elixir_aliases"
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
            raise "elixir_aliases"
        end

      :error ->
        raise "expected_compile_time_module"
    end
  end

  defp do_expand({:import, meta, [arg, opts]}, state, env) do
    assert_no_match_or_guard_scope(env.context, "import")

    state =
      state
      |> add_current_env_to_line(Keyword.fetch!(meta, :line), env)

    # no need to call expand_without_aliases_report - we never report
    {arg, state, env} = expand(arg, state, env)
    {opts, state, env} = expand_opts(meta, :import, [:only, :except, :warn], opts, state, env)

    if is_atom(arg) do
      # TODO check difference
      # elixir_aliases:ensure_loaded(Meta, ERef, ET)
      # elixir_import:import(Meta, ERef, EOpts, ET, true, true)
      with true <- Code.ensure_loaded?(arg),
           {:ok, env} <- Macro.Env.define_import(env, meta, arg, [trace: false] ++ opts) do
        {arg, state, env}
      else
        _ ->
          raise "elixir_import"
      end
    else
      raise "expected_compile_time_module"
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
    {e_opts, st, et} = expand_opts(meta, :quote, valid_opts, opts, s, e)

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
      __MODULE__.Quote.build(meta, line, file, context, unquote_opt, generated) |> dbg

    quoted = __MODULE__.Quote.quote(meta, exprs |> dbg, binding, q, prelude, et) |> dbg
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

  defp do_expand({:&, meta, [{:/, arity_meta, [{:super, super_meta, context}, arity]} = expr]}, s, e)
      when is_atom(context) and is_integer(arity) do
    assert_no_match_or_guard_scope(e.context, "&")

    case resolve_super(meta, arity, s, e) do
      {kind, name, _} when kind in [:def, :defp] ->
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

  # Super

  defp do_expand({:super, meta, args}, s, e) when is_list(args) do
    assert_no_match_or_guard_scope(e.context, "super")
    {kind, name, _} = resolve_super(meta, length(args), s, e)
    {e_args, sa, ea} = expand_args(args, s, e)
    {{:super, [{:super, {kind, name}} | meta], e_args}, sa, ea}
  end

  # Vars

  # Pin operator
  # It only appears inside match and it disables the match behaviour.

  defp do_expand({:^, meta, [arg]}, %{prematch: {prematch, _, _}, vars: {_, write}} = s, e) do
    no_match_s = %{s | prematch: :pin, vars: {prematch, write}}

    case expand(arg, no_match_s, %{e | context: nil}) do
      {{name, var_meta, kind} = var, %{unused: unused}, _} when is_atom(name) and is_atom(kind) ->
        line = var_meta[:line]
        column = var_meta[:column]
        s = if kind == nil, do: add_var(s, %State.VarInfo{name: name, is_definition: false, positions: [{line, column}]}, false), else: s
        {{:^, meta, [var]}, %{s | unused: unused}, e}

      _ ->
        # function_error(meta, e, __MODULE__, {:invalid_arg_for_pin, arg})
        {{:^, meta, [arg]}, s, e}
    end
  end

  defp do_expand({:^, meta, [arg]}, s, e) do
    # function_error(meta, e, __MODULE__, {:pin_outside_of_match, arg})
    {{:^, meta, [arg]}, s, e}
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
      unused: {unused, version},
      vars: {read, write}
    } = s

    pair = {name, var_context(meta, kind)} |> dbg

    case read |> dbg do
      # Variable was already overridden
      %{^pair => var_version} when var_version >= prematch_version ->
        # maybe_warn_underscored_var_repeat(meta, name, kind, e)
        new_unused = var_used(meta, pair, var_version, unused)
        var = {name, [{:version, var_version} | meta], kind}
        line = meta[:line]
        column = meta[:column]
        s = if kind == nil, do: add_var(s, %State.VarInfo{name: name, is_definition: true, positions: [{line, column}]}, true), else: s
        {var, %{s | unused: {new_unused, version}}, e}

      # Variable is being overridden now
      %{^pair => _} ->
        new_unused = var_unused(pair, meta, version, unused, true)
        new_read = Map.put(read, pair, version)
        new_write = if write != false, do: Map.put(write, pair, version), else: write
        var = {name, [{:version, version} | meta], kind}
        line = meta[:line]
        column = meta[:column]
        s = if kind == nil, do: add_var(s, %State.VarInfo{name: name, is_definition: true, positions: [{line, column}]}, true), else: s
        {var, %{s | vars: {new_read, new_write}, unused: {new_unused, version + 1}}, e}

      # Variable defined for the first time
      _ ->
        new_unused = var_unused(pair, meta, version, unused, false)
        new_read = Map.put(read, pair, version)
        new_write = if write != false, do: Map.put(write, pair, version), else: write
        var = {name, [{:version, version} | meta], kind}
        line = meta[:line]
        column = meta[:column]
        s = if kind == nil, do: add_var(s, %State.VarInfo{name: name, is_definition: true, positions: [{line, column}]}, true), else: s
        {var, %{s | vars: {new_read, new_write}, unused: {new_unused, version + 1}}, e}
    end
  end

  defp do_expand({name, meta, kind}, s, e) when is_atom(name) and is_atom(kind) do
    %{vars: {read, _write}, unused: {unused, version}, prematch: prematch} = s
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

    case result |> dbg do
      {:ok, pair_version} ->
        # maybe_warn_underscored_var_access(meta, name, kind, e)
        var = {name, [{:version, pair_version} | meta], kind}
        line = meta[:line]
        column = meta[:column]
        s = if kind == nil, do: add_var(s, %State.VarInfo{name: name, is_definition: false, positions: [{line, column}]}, false), else: s
        {var, %{s | unused: {var_used(meta, pair, pair_version, unused), version}}, e}

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

    # if is_atom(e_expr) do
    #   function_error(meta, e, __MODULE__, {:invalid_function_call, e_expr})
    # end

    {{{:., dot_meta, [e_expr]}, meta, e_args}, sa, ea}
  end

  # Invalid calls

  defp do_expand({_, meta, args} = _invalid, _s, _e) when is_list(meta) and is_list(args) do
    raise "invalid_call"
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

      _ ->
        raise "invalid_quoted_expr"
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
         callback,
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
      state = funs
      |> List.wrap
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
            [target: {target, as}]
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
          |> add_current_env_to_line(line)
    
        {arg, state, env} = expand(arg, state, env)
        add_behaviour(arg, state, env)
  end

  defp expand_macro(
         meta,
         Kernel,
         :@,
         [{name, _meta, args}],
         _callback,
         state,
         env
       ) when is_atom(name) do
        assert_module_scope(env, :@, 1)
        unless env.function, do: assert_no_match_or_guard_scope(env.context, "@/1")
        line = Keyword.fetch!(meta, :line)
        column = Keyword.get(meta, :column, 1)

        {is_definition, {e_args, state, env}} = case args do
          arg when is_atom(arg) ->
            # @attribute
            {false, {nil, state, env}}
          [] ->
            # deprecated @attribute()
            {false, {nil, state, env}}
          [_] ->
            # @attribute(arg)
            if env.function, do: raise "cannot set attribute @#{name} inside function/macro"
            if name == :behavior, do: raise "@behavior attribute is not supported"
            {true, expand_args(args, state, env)}
          _ -> raise "invalid @ call"
        end

        state =
          state
          |> add_attribute(name, nil, is_definition, {line, column})
          |> add_current_env_to_line(line)
    
        
        {e_args, state, env}
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

        state =
          state
          |> add_module_to_index(full, position, end_position, [])
          |> add_current_env_to_line(line, %{env | module: full})
          |> add_module_functions(%{env | module: full}, [], position, end_position)
          |> new_vars_scope
          |> new_attributes_scope
          # TODO magic with ElixirEnv instead of new_vars_scope?

        {result, state, _env} = expand(block, state, %{env | module: full})
        {result, state, env}
      else
        raise "unable to expand module alias"
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
    state = %{state | vars: vars, unused: unused}
    |> maybe_move_vars_to_outer_scope
    |> remove_vars_scope
    |> remove_attributes_scope

    # TODO hardcode expansion?
    # to result of require (a module atom) and :elixir_module.compile dot call in block

    {{:__block__, [], []}, state, env}
  end

  defp expand_macro(meta, Kernel, def_kind, [call], _callback, state, env)
       when def_kind in [:defguard, :defguardp] do
    # transform guard to def with empty body
    expand_macro(meta, Kernel, def_kind, [call, {:__block__, [], []}], _callback, state, env)
  end

  defp expand_macro(meta, Kernel, def_kind, [call, expr], _callback, state, env)
       when def_kind in [:def, :defp, :defmacro, :defmacrop, :defguard, :defguardp] do
    dbg(call)
    dbg(expr)
    assert_no_match_or_guard_scope(env.context, :"{def_kind}/2")
    module = assert_module_scope(env, def_kind, 2)

    %{vars: vars, unused: unused} = state

    # unquoted_call = :elixir_quote.has_unquotes(call)
    # unquoted_expr = :elixir_quote.has_unquotes(expr)
    # TODO expand the call and expression.
    # TODO store mod_fun_to_pos
    line = Keyword.fetch!(meta, :line)

    # state =
    #   state
    #   |> add_current_env_to_line(line, env)

    state = %{state | vars: {%{}, false}, unused: {%{}, 0}}
    |> new_func_vars_scope

    {name_and_args, guards} = __MODULE__.Utils.extract_guards(call)

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
      |> add_mod_fun_to_position(
        {module, name, arity},
        position,
        end_position,
        args,
        def_kind,
        "",
        # doc,
        %{}
        # meta
      )

    # expand_macro_callback(meta, Kernel, def_kind, [call, expr], callback, state, env)
    # %{state | prematch: :warn}
    # TODO not sure vars scope is needed
    state = state |> new_vars_scope
    {_e_body, state, _env} =
      expand(expr, state, %{g_env | context: nil, function: {name, arity}})

    # restore vars from outer scope
    # TODO maybe_move_vars_to_outer_scope?
    state = %{state | vars: vars, unused: unused}
    |> remove_vars_scope
    |> remove_func_vars_scope

    # result of def expansion is fa tuple
    {{name, arity}, state, env}
  end

  defp expand_macro(meta, module, fun, args, callback, state, env) do
    expand_macro_callback(meta, module, fun, args, callback, state, env)
  end

  defp expand_macro_callback(meta, module, fun, args, callback, state, env) do
    dbg({module, fun, args})
    try do
      callback.(meta, args)
    catch
      # TODO raise?
      # For language servers, if expanding the macro fails, we just give up.
      kind, payload ->
        # IO.inspect(payload, label: inspect(fun))
        {{{:., meta, [module, fun]}, meta, args}, state, env}
    else
      ast ->
        {ast, state, env} = expand(ast, state, env)
        {ast, state, env}
    end
  end

  defp extract_range(meta) do
    line = Keyword.get(meta, :line, 0)

    if line == 0 do
      {nil, nil}
    else
      position = {
        Keyword.get(meta, :line, 0),
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
    # TODO register call
    sl = if line > 0 do
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
          s = __MODULE__.Env.close_write(sa, s)
          |> add_current_env_to_line(line, e)
          {rewritten, s, ea}

        {:error, _error} ->
          raise "elixir_rewrite"
      end
    end
  end

  defp expand_remote(_receiver, _dot_meta, _right, _meta, _args, _, _, _e),
    do: raise("invalid_call")

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

  defp expand_local(meta, fun, args, state, env = %{function: function}) when function != nil do
    assert_no_clauses(fun, meta, args, env)

    if env.context in [:match, :guard] do
      raise "invalid_local_invocation"
    end

    # A compiler may want to emit a :local_function trace in here.
    # TODO register call
    line = Keyword.fetch!(meta, :line)

    state =
      state
      |> add_current_env_to_line(line, env)

    # state = update_in(state.locals, &[{fun, length(args)} | &1])
    {args, state, env} = expand_args(args, state, env)
    {{fun, meta, args}, state, env}
  end

  defp expand_local(meta, fun, args, state, env) do
    # elixir compiler raises here
    # raise "undefined_function"
    line = Keyword.fetch!(meta, :line)

    state =
      state
      |> add_current_env_to_line(line, env)

    {args, state, env} = expand_args(args, state, env)

    {{fun, meta, args}, state, env}
  end

  defp expand_opts(meta, kind, allowed, opts, s, e) do
    {e_opts, se, ee} = expand(opts, s, e)
    validate_opts(meta, kind, allowed, e_opts, ee)
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

  defp expand_quote(ast, state, env) do
    {_, {state, env}} =
      Macro.prewalk(ast, {state, env}, fn
        # We need to traverse inside unquotes
        {unquote, _, [expr]}, {state, env} when unquote in [:unquote, :unquote_splicing] ->
          {_expr, state, env} = expand(expr, state, env)
          {:ok, {state, env}}

        # If we find a quote inside a quote, we stop traversing it
        {:quote, _, [_]}, acc ->
          {:ok, acc}

        {:quote, _, [_, _]}, acc ->
          {:ok, acc}

        # Otherwise we go on
        node, acc ->
          {node, acc}
      end)

    {ast, state, env}
  end

  defp expand_multi_alias_call(kind, meta, base, refs, opts, state, env) do
    {base_ref, state, env} = expand(base, state, env)

    fun = fn
      {:__aliases__, _, ref}, state, env ->
        expand({kind, meta, [Module.concat([base_ref | ref]), opts]}, state, env)

      ref, state, env when is_atom(ref) ->
        expand({kind, meta, [Module.concat([base_ref, ref]), opts]}, state, env)

      _other, _s, _e ->
        raise "expected_compile_time_module"
    end

    map_fold(fun, state, env, refs)
  end

  defp overridable_name(name, count) when is_integer(count), do: :"#{name} (overridable #{count})"

  defp resolve_super(_meta, arity, state, e) do
    module = assert_module_scope(e)
    function = assert_function_scope(e)

    case function do
      {name, ^arity} ->
        state.mods_funs_to_positions |> dbg

        case state.mods_funs_to_positions[{module, name, arity} |> dbg] do
          %State.ModFunInfo{overridable: {true, _}} = info ->
            kind = case info.type do
              :defdelegate -> :def
              :defguard -> :defmacro
              :defguardp -> :defmacrop
              other -> other
            end
            hidden = Map.get(info.meta |> dbg, :hidden, false)
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
          nil -> raise "no_super"
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

        {{:&, meta, [{:/, [], [{{:., dot_meta, [remote, fun]}, attached_meta, []}, arity]}]}, se,
         ee}

      {{:local, _fun, _arity}, _, _, _se, %{function: nil}} ->
        raise "undefined_local_capture"

      {{:local, fun, arity}, local_meta, _, se, ee} ->
        {{:&, meta, [{:/, [], [{fun, local_meta, nil}, arity]}]}, se, ee}

      {:expand, expr, se, ee} ->
        expand(expr, se, ee)
    end
  end

  defp expand_for({:for, meta, [_ | _] = args}, s, e, return) do
    assert_no_match_or_guard_scope(e.context, "for")
    {cases, block} = __MODULE__.Utils.split_opts(args)
    validate_opts(meta, :for, [:do, :into, :uniq, :reduce], block, e)

    {expr, opts} =
      case Keyword.pop(block, :do) do
        {do_expr, do_opts} -> {do_expr, do_opts}
        nil -> raise "missing_option"
      end

    {e_opts, so, eo} = expand(opts, __MODULE__.Env.reset_unused_vars(s), e)
    {e_cases, sc, ec} = map_fold(&expand_for_generator/3, so, eo, cases)
    assert_generator_start(meta, e_cases, e)

    {{e_expr, se, ee}, normalized_opts} =
      case validate_for_options(e_opts, false, false, false, return, meta, e, []) do
        {:ok, maybe_reduce, nopts} ->
          # TODO not sure new vars scope is actually needed
          sc = sc |> new_vars_scope
          {ed, sd, envd} = expand_for_do_block(meta, expr, sc, ec, maybe_reduce)
          sd = sd
          |> maybe_move_vars_to_outer_scope
          |> remove_vars_scope
          {{ed, sd, envd}, nopts}

        {:error, _error} ->
          # {file_error(meta, e, __MODULE__, error), e_opts}
          raise "invalid_option"
      end

    {{:for, meta, e_cases ++ [[{:do, e_expr} | normalized_opts]]},
     __MODULE__.Env.merge_and_check_unused_vars(se, s, ee), e}
  end

  defp expand_for_do_block(_meta, [{:->, _, _} | _], _s, _e, false),
    do: raise("for_without_reduce_bad_block")

  defp expand_for_do_block(_meta, expr, s, e, false), do: expand(expr, s, e)

  defp expand_for_do_block(meta, [{:->, _, _} | _] = clauses, s, e, {:reduce, _}) do
    transformer = fn
      {_, _, [[_], _]} = clause, sa ->
        s_reset = __MODULE__.Env.reset_unused_vars(sa)

        {e_clause, s_acc, e_acc} =
          __MODULE__.Clauses.clause(meta, :fn, &__MODULE__.Clauses.head/3, clause, s_reset, e)

        {e_clause, __MODULE__.Env.merge_and_check_unused_vars(s_acc, sa, e_acc)}

      _, _ ->
        raise "for_with_reduce_bad_block"
    end

    {do_expr, sa} = Enum.map_reduce(clauses, s, transformer)
    {do_expr, sa, e}
  end

  defp expand_for_do_block(_meta, _expr, _s, _e, {:reduce, _}),
    do: raise("for_with_reduce_bad_block")

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

  defp assert_generator_start(_, [{:<-, _, [_, _]} | _], _), do: :ok
  defp assert_generator_start(_, [{:<<>>, _, [{:<-, _, [_, _]}]} | _], _), do: :ok
  defp assert_generator_start(_meta, _, _e), do: raise("for_generator_start")

  defp validate_for_options([{:into, _} = pair | opts], _into, uniq, reduce, return, meta, e, acc) do
    validate_for_options(opts, pair, uniq, reduce, return, meta, e, [pair | acc])
  end

  defp validate_for_options(
         [{:uniq, boolean} = pair | opts],
         into,
         _uniq,
         reduce,
         return,
         meta,
         e,
         acc
       )
       when is_boolean(boolean) do
    validate_for_options(opts, into, pair, reduce, return, meta, e, [pair | acc])
  end

  defp validate_for_options([{:uniq, value} | _], _, _, _, _, _, _, _) do
    {:error, {:for_invalid_uniq, value}}
  end

  defp validate_for_options(
         [{:reduce, _} = pair | opts],
         into,
         uniq,
         _reduce,
         return,
         meta,
         e,
         acc
       ) do
    validate_for_options(opts, into, uniq, pair, return, meta, e, [pair | acc])
  end

  defp validate_for_options([], into, uniq, {:reduce, _}, _return, _meta, _e, _acc)
       when into != false or uniq != false do
    {:error, :for_conflicting_reduce_into_uniq}
  end

  defp validate_for_options([], false, uniq, false, true, meta, e, acc) do
    pair = {:into, []}
    validate_for_options([pair], pair, uniq, false, true, meta, e, acc)
  end

  defp validate_for_options([], false, {:uniq, true}, false, false, meta, e, acc) do
    # file_warn(meta, e, __MODULE__, :for_with_unused_uniq)
    acc_without_uniq = Keyword.delete(acc, :uniq)
    validate_for_options([], false, false, false, false, meta, e, acc_without_uniq)
  end

  defp validate_for_options([], _into, _uniq, reduce, _return, _meta, _e, acc) do
    {:ok, reduce, Enum.reverse(acc)}
  end

  defp validate_opts(_meta, _kind, allowed, opts, _e) when is_list(opts) do
    for {key, _} <- opts, not Enum.member?(allowed, key), do: raise("unsupported_option")
  end

  defp validate_opts(_meta, _kind, _allowed, _opts, _e) do
    raise "options_are_not_keyword"
  end

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

  defp var_unused({_, kind} = pair, meta, version, unused, override) do
    if kind == nil and should_warn(meta) do
      Map.put(unused, {pair, version}, {meta, override})
    else
      unused
    end
  end

  defp var_used(meta, {_, kind} = pair, version, unused) do
    keep_unused = Keyword.has_key?(meta, :keep_unused)

    if keep_unused do
      unused
    else
      if is_atom(kind) do
        Map.put(unused, {pair, version}, false)
      else
        unused
      end
    end
  end

  defp should_warn(meta) do
    Keyword.get(meta, :generated) != true
  end

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

    def reset_unused_vars(%{unused: {_unused, version}} = s) do
      %{s | unused: {%{}, version}} |> new_vars_scope
    end

    def reset_read(%{vars: {_, write}} = s, %{vars: {read, _}}) do
      %{s | vars: {read, write}}
    end

    def prepare_write(%{vars: {read, _}} = s) do
      %{s | vars: {read, read}}
    end

    def close_write(%{vars: {_read, write}} = s, %{vars: {_, false}} = s1) do
      %{s | vars: {write, false}}
    end

    def close_write(%{vars: {_read, write}} = s, %{vars: {_, upper_write}} = s1) do
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

    def merge_and_check_unused_vars(s, s1 = %{vars: {read, write}, unused: {unused, _version}}, e) do
      %{unused: {clause_unused, version}} = s
      new_unused = merge_and_check_unused_vars(read, unused, clause_unused, e)
      # dbg(s.scope_vars_info)
      # dbg({read, write})
      s = %{s | unused: {new_unused, version}, vars: {read, write}}
      |> maybe_move_vars_to_outer_scope
      |> remove_vars_scope

      # dbg(s.scope_vars_info)
      # dbg(s.vars_info_per_scope_id)
      s
    end

    def merge_and_check_unused_vars(current, unused, clause_unused, _e) do
      :maps.fold(
        fn
          {var, count} = key, false, acc ->
            case Map.fetch(current, var) do
              {:ok, current_count} when count <= current_count ->
                Map.put(acc, key, false)

              _ ->
                acc
            end

          {{_name, _kind}, _count}, {_meta, _overridden}, acc ->
            # if kind == nil and is_unused_var(name) do
            #   warn = {:unused_var, name, overridden}
            #   file_warn(meta, e, __MODULE__, warn)
            # end
            acc
        end,
        unused,
        clause_unused
      )
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
  end

  defmodule Clauses do
    alias ElixirSense.Core.Compiler, as: ElixirExpand
    alias ElixirSense.Core.Compiler.Env, as: ElixirEnv
    alias ElixirSense.Core.Compiler.Utils, as: ElixirUtils

    def match(fun, expr, after_s, _before_s, %{context: :match} = e) do
      fun.(expr, after_s, e)
    end

    def match(fun, expr, after_s, before_s, e) do
      %{vars: current, unused: {_counter, unused} = unused_tuple} = after_s
      %{vars: {read, _write}, prematch: prematch} = before_s

      call_s = %{before_s | prematch: {read, unused, :none}, unused: unused_tuple, vars: current}

      call_e = Map.put(e, :context, :match)
      {e_expr, %{vars: new_current, unused: new_unused} = s_expr, ee} = fun.(expr, call_s, call_e)

      # TODO elixir does it like that, is it a bug? we lose state
      # end_s = %{after_s | prematch: prematch, unused: new_unused, vars: new_current}
      end_s = %{s_expr | prematch: prematch, unused: new_unused, vars: new_current}

      dbg(hd(before_s.scope_vars_info))
      dbg(hd(after_s.scope_vars_info))
      dbg(hd(end_s.scope_vars_info))

      dbg(current)
      dbg(read)
      dbg(new_current)

      # TODO I'm not sure this is correct
      merged_vars = (hd(end_s.scope_vars_info) -- hd(after_s.scope_vars_info))
      |> merge_same_name_vars()
      
      merged_vars = merged_vars ++ hd(after_s.scope_vars_info)

      end_s = %{end_s |
      scope_vars_info: [merged_vars | tl(end_s.scope_vars_info)],
      lines_to_env: Map.merge(after_s.lines_to_env, end_s.lines_to_env)
      }

      # dbg(Map.keys(end_s.lines_to_env))
      # dbg(Map.keys(after_s.lines_to_env))
      # dbg(Map.keys(before_s.lines_to_env))
      # dbg(Map.keys(before_s.lines_to_env))
      # dbg(before_s.scope_vars_info)
      # dbg(after_s.scope_vars_info)
      # dbg(end_s.scope_vars_info)

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

    def clause(_meta, _kind, _fun, _, _, _e) do
      raise ArgumentError, "bad_or_missing_clauses"
    end

    def head([{:when, meta, [_ | _] = all}], s, e) do
      {args, guard} = ElixirUtils.split_last(all)
      prematch = s.prematch

      {{e_args, e_guard}, sg, eg} =
        match(
          fn _ok, sm, em ->
            {e_args, sa, ea} = ElixirExpand.expand_args(args, sm, em)

            {e_guard, sg, eg} =
              guard(guard, %{sa | prematch: prematch}, Map.put(ea, :context, :guard))

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
      :ok =
        assert_at_most_once(:do, opts, 0, fn _key ->
          raise ArgumentError, "duplicated_clauses"
        end)

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

    defp expand_case(_meta, {_key, _}, _s, _e) do
      raise ArgumentError, "unexpected_option"
    end

    # cond

    def cond(_meta, [], _s, _e) do
      raise ArgumentError, "missing_option"
    end

    def cond(_meta, opts, _s, _e) when not is_list(opts) do
      raise ArgumentError, "invalid_args"
    end

    def cond(meta, opts, s, e) do
      :ok =
        assert_at_most_once(:do, opts, 0, fn _key ->
          raise ArgumentError, "duplicated_clauses"
        end)

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

    defp expand_cond(_meta, {_key, _}, _s, _e) do
      raise ArgumentError, "unexpected_option"
    end

    # receive

    def receive(_meta, [], _s, _e) do
      raise ArgumentError, "missing_option"
    end

    def receive(_meta, opts, _s, _e) when not is_list(opts) do
      raise ArgumentError, "invalid_args"
    end

    def receive(meta, opts, s, e) do
      raise_error = fn _key ->
        raise ArgumentError, "duplicated_clauses"
      end

      :ok = assert_at_most_once(:do, opts, 0, raise_error)
      :ok = assert_at_most_once(:after, opts, 0, raise_error)

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

    defp expand_receive(_meta, {:after, _}, _s, _e) do
      raise ArgumentError, "multiple_after_clauses_in_receive"
    end

    defp expand_receive(_meta, {_key, _}, _s, _e) do
      raise ArgumentError, "unexpected_option"
    end

    # with

    def with(meta, args, s, e) do
      {exprs, opts0} = ElixirUtils.split_opts(args)
      s0 = ElixirEnv.reset_unused_vars(s)
      {e_exprs, {s1, e1, has_match}} = Enum.map_reduce(exprs, {s0, e, false}, &expand_with/2)
      {e_do, opts1, s2} = expand_with_do(meta, opts0, s, s1, e1)
      {e_opts, opts2, s3} = expand_with_else(meta, opts1, s2, e, has_match)

      case opts2 do
        [{_key, _} | _] ->
          raise "unexpected_option"

        [] ->
          :ok
      end

      {{:with, meta, e_exprs ++ [[{:do, e_do} | e_opts]]}, s3, e}
    end

    defp expand_with({:<-, meta, [left, right]}, {s, e, has_match}) do
      {e_right, sr, er} = ElixirExpand.expand(right, s, e)
      sm = ElixirEnv.reset_read(sr, s)
      {[e_left], sl, el} = head([left], sm, er)

      new_has_match =
        case e_left do
          {var, _, ctx} when is_atom(var) and is_atom(ctx) -> has_match
          _ -> true
        end

      {{:<-, meta, [e_left, e_right]}, {sl, el, new_has_match}}
    end

    defp expand_with(expr, {s, e, has_match}) do
      {e_expr, se, ee} = ElixirExpand.expand(expr, s, e)
      {e_expr, {se, ee, has_match}}
    end

    defp expand_with_do(_meta, opts, s, acc, e) do
      case Keyword.pop(opts, :do) do
        {nil, _} ->
          raise "missing_option"

        {expr, rest_opts} ->
          # TODO not sure new vars scope is needed
          acc = acc |> new_vars_scope
          {e_expr, s_acc, e_acc} = ElixirExpand.expand(expr, acc, e)
          s_acc = s_acc
          |> maybe_move_vars_to_outer_scope
          |> remove_vars_scope
          {e_expr, rest_opts, ElixirEnv.merge_and_check_unused_vars(s_acc, s, e_acc)}
      end
    end

    defp expand_with_else(meta, opts, s, e, _has_match) do
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
    def try(_meta, [{:do, _}], _s, _e), do: raise("missing_option")
    def try(_meta, opts, _s, _e) when not is_list(opts), do: raise("invalid_args")

    def try(meta, opts, s, e) do
      # TODO: Make this an error on v2.0
      # case opts do
      #   [{:do, _}, {:else, _}] ->
      #     file_warn(meta, Map.get(e, :file), __MODULE__, {:try_with_only_else_clause, origin(meta, :try)})
      #   _ ->
      #     :ok
      # end

      raise_error = fn _key ->
        raise "duplicated_clauses"
      end

      :ok = assert_at_most_once(:do, opts, 0, raise_error)
      :ok = assert_at_most_once(:rescue, opts, 0, raise_error)
      :ok = assert_at_most_once(:catch, opts, 0, raise_error)
      :ok = assert_at_most_once(:else, opts, 0, raise_error)
      :ok = assert_at_most_once(:after, opts, 0, raise_error)
      # :ok = warn_catch_before_rescue(opts, meta, e, false)

      {try_clauses, sa} =
        Enum.map_reduce(opts, s, fn x, sa ->
          expand_try(meta, x, sa, e)
        end)

      {try_clauses, sa, e}
    end

    defp expand_try(_meta, {:do, expr}, s, e) do
      {e_expr, se, ee} = ElixirExpand.expand(expr, ElixirEnv.reset_unused_vars(s), e)
      {{:do, e_expr}, ElixirEnv.merge_and_check_unused_vars(se, s, ee)}
    end

    defp expand_try(_meta, {:after, expr}, s, e) do
      {e_expr, se, ee} = ElixirExpand.expand(expr, ElixirEnv.reset_unused_vars(s), e)
      {{:after, e_expr}, ElixirEnv.merge_and_check_unused_vars(se, s, ee)}
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

    defp expand_try(_meta, {_key, _}, _s, _e) do
      raise ArgumentError, "unexpected_option"
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

    defp expand_catch(_meta, _, _, _e) do
      raise ArgumentError, "wrong_number_of_args_for_clause"
    end

    defp expand_rescue(_meta, [arg], s, e) do
      case expand_rescue(arg, s, e) do
        {e_arg, sa, ea} ->
          {[e_arg], sa, ea}

        false ->
          raise ArgumentError, "invalid_rescue_clause"
      end
    end

    defp expand_rescue(_meta, _, _, _e) do
      raise ArgumentError, "wrong_number_of_args_for_clause"
    end

    # rescue var
    defp expand_rescue({name, _, atom} = var, s, e) when is_atom(name) and is_atom(atom) do
      match(&ElixirExpand.expand/3, var, s, s, e)
    end

    # rescue Alias => _ in [Alias]
    defp expand_rescue({:__aliases__, _, [_ | _]} = alias, s, e) do
      expand_rescue({:in, [], [{:_, [], Map.get(e, :module)}, alias]}, s, e)
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
          case normalize_rescue(e_right) do
            false -> false
            other -> {{:in, meta, [e_left, other]}, sr, er}
          end

        _ ->
          false
      end
    end

    # rescue expr() => rescue expanded_expr()
    defp expand_rescue({_meta, meta, _} = arg, s, e) do
      # TODO wut?
      case Macro.expand_once(arg, Map.put(e, :line, line(meta))) do
        ^arg -> false
        new_arg -> expand_rescue(new_arg, s, e)
      end
    end

    # rescue list() or atom() => _ in (list() or atom())
    defp expand_rescue(arg, s, e) do
      expand_rescue({:in, [], [{:_, [], Map.get(e, :module)}, arg]}, s, e)
    end

    defp normalize_rescue(atom) when is_atom(atom) do
      [atom]
    end

    defp normalize_rescue(other) do
      if is_list(other) and Enum.all?(other, &is_atom/1), do: other, else: false
    end

    defp expand_head(_meta, _kind, _key) do
      fn
        [{:when, _, [_, _, _ | _]}], _, _e ->
          raise ArgumentError, "wrong_number_of_args_for_clause"

        [_] = args, s, e ->
          head(args, s, e)

        _, _, _e ->
          raise ArgumentError, "wrong_number_of_args_for_clause"
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
          clause(meta, {kind, key}, fun, clause, ElixirEnv.reset_unused_vars(sa), e)

        {e_clause, ElixirEnv.merge_and_check_unused_vars(s_acc, sa, e_acc)}
      end

      {values, se} = Enum.map_reduce(clauses, s, transformer)
      {{key, values}, se}
    end

    defp expand_clauses_origin(_meta, _kind, _fun, {_key, _}, _, _e) do
      raise ArgumentError, "bad_or_missing_clauses"
    end

    # helpers

    defp assert_at_most_once(_kind, [], _count, _fun), do: :ok

    defp assert_at_most_once(kind, [{kind, _} | _], 1, error_fun) do
      error_fun.(kind)
    end

    defp assert_at_most_once(kind, [{kind, _} | rest], count, fun) do
      assert_at_most_once(kind, rest, count + 1, fun)
    end

    defp assert_at_most_once(kind, [_ | rest], count, fun) do
      assert_at_most_once(kind, rest, count, fun)
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

    def expand(meta, clauses, s, e) when is_list(clauses) do
      transformer = fn
        {:->, _, [left, _right]} = clause, sa ->
          if Enum.any?(left, &is_invalid_arg/1) do
            raise "defaults_in_args"
          else
            s_reset = ElixirEnv.reset_unused_vars(sa)

            {e_clause, s_acc, e_acc} =
              ElixirClauses.clause(meta, :fn, &ElixirClauses.head/3, clause, s_reset, e)

            {e_clause, ElixirEnv.merge_and_check_unused_vars(s_acc, sa, e_acc)}
          end
      end

      {e_clauses, se} = Enum.map_reduce(clauses, s, transformer)
      e_arities = Enum.map(e_clauses, fn {:->, _, [args, _]} -> fn_arity(args) end)

      case Enum.uniq(e_arities) do
        [_] ->
          {{:fn, meta, e_clauses}, se, e}

        _ ->
          raise "clauses_with_different_arities"
      end
    end

    defp is_invalid_arg({:"\\\\", _, _}), do: true
    defp is_invalid_arg(_), do: false

    defp fn_arity([{:when, _, args}]), do: length(args) - 1
    defp fn_arity(args), do: length(args)

    # Capture

    def capture(meta, {:/, _, [{{:., _, [_m, f]} = dot, require_meta, []}, a]}, s, e)
        when is_atom(f) and is_integer(a) do
      args = args_from_arity(meta, a, e)
      # handle_capture_possible_warning(meta, require_meta, m, f, a, e)
      capture_require({dot, require_meta, args}, s, e, true)
    end

    def capture(meta, {:/, _, [{f, import_meta, c}, a]}, s, e)
        when is_atom(f) and is_integer(a) and is_atom(c) do
      args = args_from_arity(meta, a, e)
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

    def capture(_meta, {:__block__, _, _} = _expr, _s, _e) do
      raise "block_expr_in_capture"
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

    def capture(_meta, integer, _s, _e) when is_integer(integer) do
      raise "capture_arg_outside_of_capture"
    end

    def capture(_meta, _arg, _s, _e) do
      raise "invalid_args_for_capture"
    end

    defp capture_import({atom, import_meta, args} = expr, s, e, sequential) do
      # TODO check similarity to macro expand_import
      res = sequential && ElixirDispatch.import_function(import_meta, atom, length(args), e)
      handle_capture(res, import_meta, import_meta, expr, s, e, sequential)
    end

    defp capture_require({{:., dot_meta, [left, right]}, require_meta, args}, s, e, sequential) do
      case escape(left, e, []) do
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
      case escape(expr, e, escaped) do
        {_, []} when not sequential ->
          raise "invalid_args_for_capture"

        {e_expr, e_dict} ->
          e_vars = validate(meta, e_dict, 1, e)
          fn_expr = {:fn, meta, [{:->, meta, [e_vars, e_expr]}]}
          {:expand, fn_expr, s, e}
      end
    end

    defp validate(meta, [{pos, var} | t], pos, e) do
      [var | validate(meta, t, pos + 1, e)]
    end

    defp validate(_meta, [{_pos, _} | _], _expected, _e) do
      raise "capture_arg_without_predecessor"
    end

    defp validate(_meta, [], _pos, _e), do: []

    defp escape({:&, meta, [pos]}, _e, dict) when is_integer(pos) and pos > 0 do
      # Using a nil context here to emit warnings when variable is unused.
      # This might pollute user space but is unlikely because variables
      # named :"&1" are not valid syntax.
      var = {:"&#{pos}", meta, nil}
      {var, :orddict.store(pos, var, dict)}
    end

    defp escape({:&, _meta, [pos]}, _e, _dict) when is_integer(pos) do
      raise "invalid_arity_for_capture"
    end

    defp escape({:&, _meta, _} = _arg, _e, _dict) do
      raise "nested_capture"
    end

    defp escape({left, meta, right}, e, dict0) do
      {t_left, dict1} = escape(left, e, dict0)
      {t_right, dict2} = escape(right, e, dict1)
      {{t_left, meta, t_right}, dict2}
    end

    defp escape({left, right}, e, dict0) do
      {t_left, dict1} = escape(left, e, dict0)
      {t_right, dict2} = escape(right, e, dict1)
      {{t_left, t_right}, dict2}
    end

    defp escape(list, e, dict) when is_list(list) do
      Enum.map_reduce(list, dict, fn x, acc -> escape(x, e, acc) end)
    end

    defp escape(other, _e, dict) do
      {other, dict}
    end

    defp args_from_arity(_meta, a, _e) when is_integer(a) and a >= 0 and a <= 255 do
      Enum.map(1..a, fn x -> {:&, [], [x]} end)
    end

    defp args_from_arity(_meta, _a, _e) do
      raise "invalid_arity_for_capture"
    end

    defp is_sequential_and_not_empty([]), do: false
    defp is_sequential_and_not_empty(list), do: is_sequential(list, 1)

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

    def quote(_meta, {:unquote_splicing, _, [_]}, _binding, %__MODULE__{unquote: true}, _, _),
      do: raise("unquote_splicing only works inside arguments and block contexts")

    def quote(meta, expr, binding, q, prelude, e) do
      context = q.context

      vars =
        Enum.map(binding, fn {k, v} ->
          {:{}, [], {:=, [], {:{}, [], [k, meta, context]}, v}}
        end)

      quoted = do_quote(expr, q, e) |> dbg

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
      runtime = do_runtime_list(meta, :list, [expr, do_list_concat(buffer, acc)]) |> dbg
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
      clean_map_args = clean_struct_key_from_map_args(meta, map_args, e)

      {[e_left, e_right], se, ee} =
        ElixirExpand.expand_args([left, {:%{}, map_meta, clean_map_args}], s, e)

      case validate_struct(e_left, context) do
        true when is_atom(e_left) ->
          case extract_struct_assocs(meta, e_right, e) do
            {:expand, map_meta, assocs} when context != :match ->
              assoc_keys = Enum.map(assocs, fn {k, _} -> k end)
              struct = load_struct(meta, e_left, [assocs], assoc_keys, ee)
              keys = [:__struct__ | assoc_keys]
              without_keys = Elixir.Map.drop(struct, keys)
              struct_assocs = Macro.escape(Enum.sort(Elixir.Map.to_list(without_keys)))
              {{:%, meta, [e_left, {:%{}, map_meta, struct_assocs ++ assocs}]}, se, ee}

            {_, _, assocs} ->
              _ = load_struct(meta, e_left, [], Enum.map(assocs, fn {k, _} -> k end), ee)
              {{:%, meta, [e_left, e_right]}, se, ee}
          end

        true ->
          {{:%, meta, [e_left, e_right]}, se, ee}

        false when context == :match ->
          raise "invalid_struct_name_in_match"

        false ->
          raise "invalid_struct_name"
      end
    end

    def expand_struct(_meta, _left, _right, _s, _e), do: raise("non_map_after_struct")

    def expand_map(meta, [{:|, update_meta, [left, right]}], s, %{context: nil} = e) do
      {[e_left | e_right], se, ee} = ElixirExpand.expand_args([left | right], s, e)
      validate_kv(meta, e_right, right, e)
      {{:%{}, meta, [{:|, update_meta, [e_left, e_right]}]}, se, ee}
    end

    def expand_map(_meta, [{:|, _, [_, _]}] = _args, _s, _e) do
      raise "update_syntax_in_wrong_context"
    end

    def expand_map(meta, args, s, e) do
      {e_args, se, ee} = ElixirExpand.expand_args(args, s, e)
      validate_kv(meta, e_args, args, e)
      {{:%{}, meta, e_args}, se, ee}
    end

    defp clean_struct_key_from_map_args(meta, [{:|, pipe_meta, [left, map_assocs]}], e) do
      [{:|, pipe_meta, [left, clean_struct_key_from_map_assocs(meta, map_assocs, e)]}]
    end

    defp clean_struct_key_from_map_args(meta, map_assocs, e) do
      clean_struct_key_from_map_assocs(meta, map_assocs, e)
    end

    defp clean_struct_key_from_map_assocs(_meta, assocs, _e) do
      case Keyword.pop(assocs, :__struct__) do
        {nil, cleaned_assocs} ->
          cleaned_assocs

        {_struct_value, cleaned_assocs} ->
          # file_warn(meta, Map.get(e, :file), __MODULE__, :ignored_struct_key_in_struct)
          cleaned_assocs
      end
    end

    defp validate_kv(meta, kv, _original, %{context: context} = e) do
      Enum.reduce(kv, {1, %{}}, fn
        {k, _v}, {index, used} ->
          if context == :match do
            validate_match_key(meta, k, e)
          end

          new_used = validate_not_repeated(meta, k, used, e)
          {index + 1, new_used}

        _, {_index, _used} ->
          raise "not_kv_pair"
      end)
    end

    defp validate_not_repeated(_meta, key, used, e) do
      if is_literal(key) and Elixir.Map.has_key?(used, key) do
        case e do
          %{context: :match} ->
            # raise "repeated_key"
            # function_error(meta, Map.get(e, :file), __MODULE__, {:repeated_key, key})
            :ok

          _ ->
            # file_warn(meta, Map.get(e, :file), __MODULE__, {:repeated_key, key})
            :ok
        end

        used
      else
        Elixir.Map.put(used, key, true)
      end
    end

    defp validate_match_key(_meta, {name, _, context}, _e)
         when is_atom(name) and is_atom(context) do
      raise "invalid_variable_in_map_key_match"
    end

    defp validate_match_key(meta, {:"::", _, [left, _]}, e) do
      validate_match_key(meta, left, e)
    end

    defp validate_match_key(_, {:^, _, [{name, _, context}]}, _)
         when is_atom(name) and is_atom(context),
         do: :ok

    defp validate_match_key(_, {:%{}, _, [_ | _]}, _), do: :ok

    defp validate_match_key(meta, {left, _, right}, e) do
      validate_match_key(meta, left, e)
      validate_match_key(meta, right, e)
    end

    defp validate_match_key(meta, {left, right}, e) do
      validate_match_key(meta, left, e)
      validate_match_key(meta, right, e)
    end

    defp validate_match_key(meta, list, e) when is_list(list) do
      for each <- list do
        validate_match_key(meta, each, e)
      end
    end

    defp validate_match_key(_, _, _), do: :ok

    defp is_literal({_, _, _}), do: false

    defp is_literal({left, right}), do: is_literal(left) and is_literal(right)

    defp is_literal(list) when is_list(list), do: Enum.all?(list, &is_literal/1)

    defp is_literal(_), do: true

    defp validate_struct({:^, _, [{var, _, ctx}]}, :match) when is_atom(var) and is_atom(ctx),
      do: true

    defp validate_struct({var, _meta, ctx}, :match) when is_atom(var) and is_atom(ctx), do: true
    defp validate_struct(atom, _) when is_atom(atom), do: true
    defp validate_struct(_, _), do: false

    defp extract_struct_assocs(_, {:%{}, meta, [{:|, _, [_, assocs]}]}, _) do
      {:update, meta, delete_struct_key(assocs)}
    end

    defp extract_struct_assocs(_, {:%{}, meta, assocs}, _) do
      {:expand, meta, delete_struct_key(assocs)}
    end

    defp extract_struct_assocs(_meta, _other, _e) do
      raise "non_map_after_struct"
    end

    defp delete_struct_key(assocs) do
      Keyword.delete(assocs, :__struct__)
    end

    defp load_struct(meta, name, args, keys, e) do
      module = e.module
      in_context = name in [module | e.context_modules]

      _arity = length(args)
      # TODO
      # or (not ensure_loaded(name) and wait_for_struct(name))
      external = in_context

      try do
        # TODO the condition includes
        # and ElixirDef.external_for(meta, name, :__struct__, arity, [:def])
        case external do
          false when module == name ->
            raise UndefinedFunctionError

          false ->
            apply(name, :__struct__, args)

          external_fun ->
            try do
              apply(external_fun, args)
            rescue
              UndefinedFunctionError -> apply(name, :__struct__, args)
            end
        end
      rescue
        UndefinedFunctionError ->
          cond do
            in_context and e.function == nil ->
              raise "inaccessible_struct"

            true ->
              raise "undefined_struct"
          end
      else
        %{:__struct__ => struct_name} = struct when is_atom(struct_name) ->
          assert_struct_keys(meta, name, struct, keys, e)
          # ElixirEnv.trace({:struct_expansion, meta, name, keys}, e)
          struct

        %{:__struct__ => struct_name} when is_atom(struct_name) ->
          raise "struct_name_mismatch"

        _other ->
          raise "invalid_struct_return_value"
      end
    end

    defp assert_struct_keys(_meta, _name, struct, keys, _e) do
      for key <- keys, not Elixir.Map.has_key?(struct, key) do
        raise "unknown_key_for_struct"
      end
    end
  end
end