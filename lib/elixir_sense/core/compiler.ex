defmodule ElixirSense.Core.Compiler do
  import ElixirSense.Core.State, except: [expand: 2, expand: 3, no_alias_expansion: 1]
  alias ElixirSense.Core.State
  require Logger
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.TypeInfo
  alias ElixirSense.Core.TypeInference
  alias ElixirSense.Core.Guard
  alias ElixirSense.Core.Normalized.Macro.Env, as: NormalizedMacroEnv

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
    # elixir validates we are not in guard context
    {e_right, sr, er} = expand(right, s, e)
    {e_left, sl, el} = __MODULE__.Clauses.match(&expand/3, left, sr, s, er)

    match_context_r = TypeInference.get_binding_type(e_right, e.context)
    vars_l_with_inferred_types = TypeInference.find_vars(e_left, match_context_r, :match)

    expressions_to_refine = TypeInference.find_refinable(e_right, [], e.context)

    vars_r_with_inferred_types =
      if expressions_to_refine != [] do
        # we are in match context and the right side is also a pattern, we can refine types
        # on the right side using the inferred type of the left side
        match_context_l = TypeInference.get_binding_type(e_left, :match)

        for expr <- expressions_to_refine, reduce: [] do
          acc ->
            vars_in_expr_with_inferred_types =
              TypeInference.find_vars(expr, match_context_l, :match)

            acc ++ vars_in_expr_with_inferred_types
        end
      else
        []
      end

    sl =
      merge_inferred_types(sl, vars_l_with_inferred_types ++ vars_r_with_inferred_types)

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
    case NormalizedMacroEnv.expand_alias(env, meta, list, trace: false) do
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
      |> add_first_alias_positions(env, meta)
      |> add_current_env_to_line(Keyword.fetch!(meta, :line), env)

    # no need to call expand_without_aliases_report - we never report
    {arg, state, env} = expand(arg, state, env)
    {opts, state, env} = expand_opts([:as, :warn], no_alias_opts(opts), state, env)

    if is_atom(arg) do
      # TODO check difference with
      # elixir_aliases:alias(Meta, Ref, IncludeByDefault, Opts, E, true)
      # TODO PR to elixir with is_atom(module) check?
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
          case NormalizedMacroEnv.define_alias(env, meta, arg, [trace: false] ++ opts) do
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
        case NormalizedMacroEnv.define_require(env, meta, arg, [trace: false] ++ opts) do
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
    state =
      state
      |> add_current_env_to_line(Keyword.fetch!(meta, :line), env)

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
    # elixir checks if context is not match and if caller is allowed
    line = Keyword.get(meta, :line, 0)
    s = if line > 0, do: add_current_env_to_line(s, line, e), else: s

    {caller, s, e}
  end

  defp do_expand({:__STACKTRACE__, meta, ctx} = stacktrace, s, e) when is_atom(ctx) do
    # elixir checks if context is not match and if stacktrace is allowed
    line = Keyword.get(meta, :line, 0)
    s = if line > 0, do: add_current_env_to_line(s, line, e), else: s

    {stacktrace, s, e}
  end

  defp do_expand({:__ENV__, meta, ctx}, s, e) when is_atom(ctx) do
    # elixir checks if context is not match
    line = Keyword.get(meta, :line, 0)
    s = if line > 0, do: add_current_env_to_line(s, line, e), else: s

    {escape_map(escape_env_entries(meta, s, e)), s, e}
  end

  defp do_expand({{:., dot_meta, [{:__ENV__, meta, atom}, field]}, call_meta, []}, s, e)
       when is_atom(atom) and is_atom(field) do
    # elixir checks if context is not match
    line = Keyword.get(call_meta, :line, 0)
    s = if line > 0, do: add_current_env_to_line(s, line, e), else: s

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
            # TODO check if there's cursor?
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

    # TODO this is a stub only
    # res = expand_quote(exprs, st, et)
    # res |> elem(0) |> IO.inspect
    # res
    {q, q_context, q_prelude} =
      __MODULE__.Quote.build(meta, line, file, context, unquote_opt, generated, et)

    {e_prelude, sp, ep} = expand(q_prelude, st, et)
    {e_context, sc, ec} = expand(q_context, sp, ep)
    quoted = __MODULE__.Quote.quote(exprs, q)
    {e_quoted, es, eq} = expand(quoted, sc, ec)

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
    expand_fn_capture(meta, arg, s, e)
  end

  defp do_expand({:fn, meta, pairs}, s, e) do
    __MODULE__.Fn.expand(meta, pairs, s, e)
  end

  # case/cond/try/receive

  defp do_expand({:cond, meta, [opts]}, s, e) do
    # elixir raises underscore_in_cond if the last clause is _
    {e_clauses, sc, ec} = __MODULE__.Clauses.cond(meta, opts, s, e)
    {{:cond, meta, [e_clauses]}, sc, ec}
  end

  defp do_expand({:case, meta, [expr, options]}, s, e) do
    expand_case(meta, expr, options, s, e)
  end

  defp do_expand({:receive, meta, [opts]}, s, e) do
    {e_clauses, sc, ec} = __MODULE__.Clauses.receive(meta, opts, s, e)
    {{:receive, meta, [e_clauses]}, sc, ec}
  end

  defp do_expand({:try, meta, [opts]}, s, e) do
    {e_clauses, sc, ec} = __MODULE__.Clauses.try(meta, opts, s, e)
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
        |> add_cursor_env(meta, e)
      else
        s
      end

    {{:__cursor__, meta, args}, s, e}
  end

  # Super

  defp do_expand({:super, meta, args}, s, e) when is_list(args) do
    arity = length(args)

    case resolve_super(meta, arity, s, e) do
      {kind, name, _} ->
        {e_args, sa, ea} = expand_args(args, s, e)

        line = Keyword.get(meta, :line, 0)
        column = Keyword.get(meta, :column, nil)

        sa =
          sa
          |> add_call_to_line({nil, name, arity}, {line, column})
          |> add_current_env_to_line(line, ea)

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
        s = add_var_read(s, var)
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

          # elixir plans to remove this clause on v2.0
          {:ok, :raise} ->
            # TODO is it worth registering var access
            # function_error(meta, e, __MODULE__, {:undefined_var, name, kind})
            {{name, meta, kind}, s, e}

          # elixir plans to remove this clause on v2.0
          _ when error == :warn ->
            # TODO is it worth registering var access?
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
    # elixir checks here id fall is not ambiguous
    arity = length(args)

    # TODO check if it works in our case
    # If we are inside a function, we support reading from locals.
    allow_locals = match?({n, a} when fun != n or arity != a, env.function)

    # TODO this crashes with CompileError ambiguous_call
    case NormalizedMacroEnv.expand_import(env, meta, fun, arity,
           trace: false,
           allow_locals: allow_locals,
           check_deprecations: false
         ) do
      {:macro, module, callback} ->
        # TODO there is a subtle difference - callback will call expander with state derived from env via
        # :elixir_env.env_to_ex(env) possibly losing some details
        # line = Keyword.get(meta, :line, 0)
        # column = Keyword.get(meta, :column, nil)
        # state = state
        # |> add_call_to_line({module, fun, length(args)}, {line, column})
        # |> add_current_env_to_line(line, env)
        expand_macro(meta, module, fun, args, callback, state, env)

      {:function, module, fun} ->
        {ar, af} =
          case __MODULE__.Rewrite.inline(module, fun, arity) do
            {ar, an} ->
              {ar, an}

            false ->
              {module, fun}
          end

        expand_remote(ar, meta, af, meta, args, state, __MODULE__.Env.prepare_write(state), env)

      {:error, :not_found} ->
        expand_local(meta, fun, args, state, env)

      {:error, {:conflict, _module}} ->
        raise "conflict"

      {:error, {:ambiguous, _module}} ->
        raise "ambiguous"
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
      case __MODULE__.Rewrite.inline(module, fun, arity) do
        {ar, an} ->
          expand_remote(ar, dot_meta, an, meta, args, state, state_l, env)

        false ->
          case NormalizedMacroEnv.expand_require(env, meta, module, fun, arity,
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
    {[e_expr | e_args], sa, ea} = expand_args([expr | args], s, e)

    # elixir validates if e_expr is not atom and raises invalid_function_call

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

    sa =
      sa
      |> add_call_to_line({nil, e_expr, length(e_args)}, {line, column})
      |> add_current_env_to_line(line, e)

    {{{:., dot_meta, [e_expr]}, meta, e_args}, sa, ea}
  end

  # Invalid calls

  defp do_expand({other, meta, args}, s, e) when is_list(meta) and is_list(args) do
    # elixir raises invalid_call
    {args, s, e} = expand_args(args, s, e)
    {{other, meta, args}, s, e}
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

  defp do_expand(other, s, e) when is_number(other) or is_atom(other) or is_binary(other) do
    {other, s, e}
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
    {position, end_position} = extract_range(meta)
    {line, _} = position

    {opts, state, env} = expand(opts, state, env)
    # elixir does validation here
    target = Keyword.get(opts, :to, :__unknown__)

    # TODO Remove List.wrap when multiple funs are no longer supported by elixir
    state =
      funs
      |> List.wrap()
      |> Enum.reduce(state, fn fun, state ->
        fun =
          if __MODULE__.Quote.has_unquotes(fun) do
            # dynamic defdelegate - replace unquote expression with fake call
            {:__unknown__, [], []}
          else
            fun
          end

        {name, args, as, as_args} = Kernel.Utils.defdelegate_each(fun, opts)
        arity = length(args)

        state
        |> add_current_env_to_line(line, %{env | context: nil, function: {name, arity}})
        |> add_func_to_index(
          env,
          name,
          args,
          position,
          end_position,
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
         [{:behaviour, _meta, [arg]}],
         _callback,
         state,
         env = %{module: module}
       )
       when module != nil do
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
         env = %{module: module}
       )
       when module != nil do
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
         env = %{module: module}
       )
       when doc in [:doc, :typedoc] and module != nil do
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
         env = %{module: module}
       )
       when module != nil do
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
         env = %{module: module}
       )
       when module != nil do
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
         env = %{module: module}
       )
       when module != nil do
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
         env = %{module: module}
       )
       when module != nil do
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
            mod = Module.concat(proto_module, module)

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
         env = %{module: module}
       )
       when kind in [:type, :typep, :opaque] and module != nil do
    {expr, state, env} = __MODULE__.Typespec.expand_type(expr, state, env)

    case __MODULE__.Typespec.type_to_signature(expr) do
      {name, [_type_arg]} when name in [:required, :optional] ->
        raise "type #{name}/#{1} is a reserved type and it cannot be defined"

      {name, type_args} ->
        if __MODULE__.Typespec.built_in_type?(name, length(type_args)) do
          raise "type #{name}/#{length(type_args)} is a built-in type and it cannot be redefined"
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
         env = %{module: module}
       )
       when kind in [:callback, :macrocallback, :spec] and module != nil do
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
          |> add_spec(env, name, type_args, spec, kind, position, end_position)
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
         env = %{module: module}
       )
       when is_atom(name) and module != nil do
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
        [arg] -> TypeInference.get_binding_type(arg, env.context)
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
         env = %{module: module}
       )
       when module != nil do
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
         env = %{module: module}
       )
       when type in [:defstruct, :defexception] and module != nil do
    if Map.has_key?(state.structs, module) do
      raise ArgumentError,
            "defstruct has already been called for " <>
              "#{inspect(module)}, defstruct can only be called once per module"
    end

    fields =
      case fields do
        fs when is_list(fs) ->
          fs

        _other ->
          # elixir raises ArgumentError here
          []
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
         env = %{module: module}
       )
       when call in [:defrecord, :defrecordp] and module != nil do
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
    # transform guard and function head to def with empty body
    expand_macro(meta, Kernel, def_kind, [call, {:__block__, [], []}], callback, state, env)
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
    %{vars: vars, unused: unused} = state

    line = Keyword.fetch!(meta, :line)

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
    {name, _meta_1, args} =
      case name_and_args do
        {n, m, a} when is_atom(n) and is_atom(a) -> {n, m, []}
        {n, m, a} when is_atom(n) and is_list(a) -> {n, m, a}
        {n, m, a} when is_atom(a) -> {:__unknown__, m, []}
        {n, m, a} when is_list(a) -> {:__unknown__, m, a}
        _ -> {:__unknown__, [], []}
      end

    arity = length(args)

    # based on :elixir_def.env_for_expansion
    state =
      unless has_unquotes do
        # module vars are not accessible in def body
        %{
          state
          | vars: {%{}, false},
            unused: 0,
            caller: def_kind in [:defmacro, :defmacrop, :defguard, :defguardp]
        }
        |> new_func_vars_scope()
      else
        # make module variables accessible if there are unquote fragments in def body
        %{
          state
          | caller: def_kind in [:defmacro, :defmacrop, :defguard, :defguardp]
        }
      end

    env_for_expand = %{env | function: {name, arity}}

    # based on :elixir_clauses.def
    {_e_args, state, env_for_expand} =
      expand_args(args, %{state | prematch: {%{}, 0, :none}}, %{env_for_expand | context: :match})

    {e_guard, state, env_for_expand} =
      __MODULE__.Clauses.guard(
        guards,
        %{state | prematch: :raise},
        %{env_for_expand | context: :guard}
      )

    type_info = Guard.type_information_from_guards(e_guard)

    state = merge_inferred_types(state, type_info)

    env_for_expand = %{env_for_expand | context: nil}

    {position, end_position} = extract_range(meta)

    state =
      state
      |> add_current_env_to_line(line, env_for_expand)
      |> add_func_to_index(
        env,
        name,
        args,
        position,
        end_position,
        def_kind
      )

    # TODO not sure vars scope is needed
    state = state |> new_vars_scope

    {_e_body, state, _env_for_expand} =
      expand(expr, state, env_for_expand)

    # restore vars from outer scope
    state =
      %{state | vars: vars, unused: unused, caller: false}
      |> maybe_move_vars_to_outer_scope
      |> remove_vars_scope

    state =
      unless has_unquotes do
        # restore module vars
        remove_func_vars_scope(state)
      else
        # no need to do anything
        state
      end

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
      # elixir raises parens_map_lookup unless no_parens is set in meta
      # TODO there may be cursor in discarded args
      {{{:., dot_meta, [receiver, right]}, meta, []}, sl, e}
    else
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
            __MODULE__.Env.close_write(sa, s)
            |> add_call_to_line({receiver, right, length(e_args)}, {line, column})
            |> add_current_env_to_line(line, e)

          {rewritten, s, ea}

        {:error, _error} ->
          # elixir raises here elixir_rewrite
          s =
            __MODULE__.Env.close_write(sa, s)
            |> add_call_to_line({receiver, right, length(e_args)}, {line, column})
            |> add_current_env_to_line(line, e)

          {{{:., dot_meta, [receiver, right]}, attached_meta, e_args}, s, ea}
      end
    end
  end

  defp expand_remote(receiver, dot_meta, right, meta, args, s, sl, e) do
    # elixir raises here invalid_call
    {e_args, {sa, _}, ea} = map_fold(&expand_arg/3, {sl, s}, e, args)

    line = Keyword.get(meta, :line, 0)
    column = Keyword.get(meta, :column, nil)

    s =
      __MODULE__.Env.close_write(sa, s)
      |> add_call_to_line({receiver, right, length(e_args)}, {line, column})
      |> add_current_env_to_line(line, e)

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

        line = Keyword.get(attached_meta, :line, 0)
        column = Keyword.get(attached_meta, :column, nil)

        se =
          se
          |> add_call_to_line({remote, fun, arity}, {line, column})
          |> add_current_env_to_line(line, ee)

        {{:&, meta, [{:/, [], [{{:., dot_meta, [remote, fun]}, attached_meta, []}, arity]}]}, se,
         ee}

      {{:local, fun, arity}, local_meta, _, se, ee} ->
        # elixir raises undefined_local_capture if ee.function is nil
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
    {cases, block} = __MODULE__.Utils.split_opts(args)
    block = sanitize_opts([:do, :into, :uniq, :reduce], block)

    {expr, opts} =
      case Keyword.pop(block, :do) do
        {nil, do_opts} ->
          # elixir raises missing_option here
          {[], do_opts}

        {do_expr, do_opts} ->
          {do_expr, do_opts}
      end

    {e_opts, so, eo} = expand(opts, __MODULE__.Env.reset_vars(s), e)
    {e_cases, sc, ec} = map_fold(&expand_for_generator/3, so, eo, cases)
    # elixir raises here for_generator_start on invalid start generator

    {maybe_reduce, normalized_opts} =
      sanitize_for_options(e_opts, false, false, false, return, meta, e, [])

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
      {:->, clause_meta, [args, right]}, sa ->
        # elixir checks here that clause has exactly 1 arg by matching against {_, _, [[_], _]}
        # we drop excessive or generate a fake arg
        # TODO check if there is cursor in dropped arg?
        args =
          case args do
            [] -> [{:_, [], e.module}]
            [head | _] -> [head]
          end

        clause = {:->, clause_meta, [args, right]}
        s_reset = __MODULE__.Env.reset_vars(sa)

        # no point in doing type inference here, we are only certain of the initial value of the accumulator
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

    match_context_r = TypeInference.get_binding_type(e_right, e.context)

    vars_l_with_inferred_types =
      TypeInference.find_vars(e_left, {:for_expression, match_context_r}, :match)

    sl = State.merge_inferred_types(sl, vars_l_with_inferred_types)

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
    # TODO check if there is cursor  in dropped unique
    acc_without_uniq = Keyword.delete(acc, :uniq)
    sanitize_for_options([], false, false, false, false, meta, e, acc_without_uniq)
  end

  defp sanitize_for_options([], _into, _uniq, reduce, _return, _meta, _e, acc) do
    {reduce, Enum.reverse(acc)}
  end

  defp sanitize_opts(allowed, opts) when is_list(opts) do
    # TODO check if there's cursor
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

    {e_opts, so, eo} = __MODULE__.Clauses.case(meta, e_expr, r_opts, se, ee)
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

  @internals [{:behaviour_info, 1}, {:module_info, 1}, {:module_info, 0}]
  defp import_info_callback(module, state) do
    fn kind ->
      if Map.has_key?(state.mods_funs_to_positions, {module, nil, nil}) do
        category = if kind == :functions, do: :function, else: :macro

        for {{^module, fun, arity}, info} when fun != nil <- state.mods_funs_to_positions,
            {fun, arity} not in @internals,
            State.ModFunInfo.get_category(info) == category,
            not State.ModFunInfo.private?(info) do
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
      {_, result} =
        Macro.prewalk(ast, false, fn
          {:__cursor__, _, list} = node, _state when is_list(list) ->
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
    alias ElixirSense.Core.State
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

            type_info = Guard.type_information_from_guards(e_guard)

            sg = merge_inferred_types(sg, type_info)

            {{e_args, e_guard}, sg, eg}
          end,
          :ok,
          s,
          s,
          e
        )

      # TODO infer type from guard here

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
      {e_guard, sg, eg}
    end

    # case

    def case(meta, e_expr, [], s, e) do
      # elixir raises here missing_option
      # emit a fake do block
      case(meta, e_expr, [do: []], s, e)
    end

    def case(_meta, _e_expr, opts, s, e) when not is_list(opts) do
      # elixir raises here invalid_args
      # there may be cursor
      ElixirExpand.expand(opts, s, e)
    end

    def case(meta, e_expr, opts, s, e) do
      opts = sanitize_opts(opts, [:do])

      match_context = TypeInference.get_binding_type(e_expr, e.context)

      {case_clauses, sa} =
        Enum.map_reduce(opts, s, fn x, sa ->
          expand_case(meta, x, match_context, sa, e)
        end)

      {case_clauses, sa, e}
    end

    defp expand_case(meta, {:do, _} = do_clause, match_context, s, e) do
      expand_clauses(
        meta,
        :case,
        fn c, s, e ->
          case head(c, s, e) do
            {[h | _] = c, s, e} ->
              clause_vars_with_inferred_types =
                TypeInference.find_vars(h, match_context, :match)

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

    def cond(meta, [], s, e) do
      # elixir raises here missing_option
      # emit a fake do block
      cond(meta, [do: []], s, e)
    end

    def cond(_meta, opts, s, e) when not is_list(opts) do
      # elixir raises here invalid_args
      # there may be cursor
      ElixirExpand.expand(opts, s, e)
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
      expand_clauses(meta, :cond, &ElixirExpand.expand_args/3, do_clause, s, e)
    end

    # receive

    def receive(meta, [], s, e) do
      # elixir raises here missing_option
      # emit a fake do block
      receive(meta, [do: []], s, e)
    end

    def receive(_meta, opts, s, e) when not is_list(opts) do
      # elixir raises here invalid_args
      # there may be cursor
      ElixirExpand.expand(opts, s, e)
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
      # no point in doing type inference here, we have no idea what message we may get
      expand_clauses(meta, :receive, &head/3, do_clause, s, e)
    end

    defp expand_receive(meta, {:after, [_ | _]} = after_clause, s, e) do
      expand_clauses(meta, :receive, &ElixirExpand.expand_args/3, after_clause, s, e)
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
      {e_opts, _opts2, s3} = expand_with_else(meta, opts1, s2, e)

      {{:with, meta, e_exprs ++ [[{:do, e_do} | e_opts]]}, s3, e}
    end

    defp expand_with({:<-, meta, [left, right]}, {s, e}) do
      {e_right, sr, er} = ElixirExpand.expand(right, s, e)
      sm = ElixirEnv.reset_read(sr, s)
      {[e_left], sl, el} = head([left], sm, er)

      match_context_r = TypeInference.get_binding_type(e_right, e.context)
      vars_l_with_inferred_types = TypeInference.find_vars(e_left, match_context_r, :match)

      sl = State.merge_inferred_types(sl, vars_l_with_inferred_types)

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

          # no point in doing type inference here, we have no idea what data we are matching against
          {e_pair, se} = expand_clauses(meta, :with, &head/3, pair, s, e)
          {[e_pair], rest_opts, se}
      end
    end

    # try

    def try(meta, [], s, e) do
      # elixir raises here missing_option
      # emit a fake do block
      try(meta, [do: []], s, e)
    end

    def try(_meta, opts, s, e) when not is_list(opts) do
      # elixir raises here invalid_args
      # there may be cursor
      ElixirExpand.expand(opts, s, e)
    end

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
      # TODO we could try to infer type from last try block expression
      expand_clauses(meta, :try, &head/3, else_clause, s, e)
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
      # no point in doing type inference here, we have no idea what throw we caught
      head(args, s, e)
    end

    defp expand_catch(_meta, args = [_, _], s, e) do
      # TODO is it worth to infer type of the first arg? :error | :exit | :throw | {:EXIT, pid()}
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
      {e_left, sl, el} = match(&ElixirExpand.expand/3, var, s, s, e)

      match_context = {:struct, [], {:atom, Exception}, nil}

      vars_with_inferred_types = TypeInference.find_vars(e_left, match_context, :match)
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
      {e_left, sl, el} = match(&ElixirExpand.expand/3, var, s, s, e)

      match_context = {:struct, [], {:atom, Exception}, nil}

      vars_with_inferred_types = TypeInference.find_vars(e_left, match_context, :match)
      sl = State.merge_inferred_types(sl, vars_with_inferred_types)

      {e_left, sl, el}
    end

    # rescue var in (list() or atom())
    defp expand_rescue({:in, meta, [left, right]}, s, e) do
      {e_left, sl, el} = match(&ElixirExpand.expand/3, left, s, s, e)
      {e_right, sr, er} = ElixirExpand.expand(right, sl, el)

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

          vars_with_inferred_types = TypeInference.find_vars(e_left, match_context, :match)
          sr = State.merge_inferred_types(sr, vars_with_inferred_types)

          {{:in, meta, [e_left, normalized]}, sr, er}

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

          # elixir validates if there is no nested match

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

      e_acc = concat_or_prepend_bitstring(meta, e_left, e_right, acc)

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

      e_type = expr_type(e_left)
      e_right = infer_spec(e_type, meta)

      inferred_meta = [{:inferred_bitstring_spec, true} | meta]

      e_acc =
        concat_or_prepend_bitstring(
          inferred_meta,
          e_left,
          e_right,
          acc
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
        {e_component, s, e} when is_list(e_component) or is_atom(e_component) ->
          # elixir raises here invalid_literal
          # try to recover from error by replacing it with ""
          {"", s, e}

        expanded ->
          expanded
      end
    end

    defp expand_specs(expr_type, meta, info, s, original_s, e, expect_size) do
      default =
        %{size: :default, unit: :default, sign: :default, type: :default, endianness: :default}

      {specs, ss, es} =
        expand_each_spec(meta, unpack_specs(info, []), default, s, original_s, e)

      merged_type = type(expr_type, specs.type)

      # elixir validates if unsized binary is not on the end

      size_and_unit = size_and_unit(expr_type, specs.size, specs.unit)
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
          specs.size,
          specs.unit,
          merged_type,
          specs.endianness,
          specs.sign,
          maybe_inferred_size
        )

      {Enum.reduce(t, h, fn i, acc -> {:-, meta, [acc, i]} end), alignment, ss, es}
    end

    defp type(:default, :default), do: :integer
    defp type(expr_type, :default), do: expr_type

    defp type(:binary, type) when type in [:binary, :bitstring, :utf8, :utf16, :utf32],
      do: type

    defp type(:bitstring, type) when type in [:binary, :bitstring], do: type

    defp type(:integer, type) when type in [:integer, :float, :utf8, :utf16, :utf32],
      do: type

    defp type(:float, :float), do: :float
    defp type(:default, type), do: type

    defp type(_other, _type) do
      # elixir raises here bittype_mismatch
      type(:default, :default)
    end

    defp expand_each_spec(meta, [{:__cursor__, _, args} = h | t], map, s, original_s, e)
         when is_list(args) do
      {_, s, e} = ElixirExpand.expand(h, s, e)
      expand_each_spec(meta, t, map, s, original_s, e)
    end

    defp expand_each_spec(meta, [{expr, meta_e, args} = h | t], map, s, original_s, e)
         when is_atom(expr) do
      case validate_spec(expr, args) do
        {key, arg} ->
          {value, se, ee} = expand_spec_arg(arg, s, original_s, e)
          # elixir validates spec arg here
          # elixir raises bittype_mismatch in some cases
          expand_each_spec(meta, t, Map.put(map, key, value), se, original_s, ee)

        :none ->
          ha =
            if args == nil do
              {expr, meta_e, []}
            else
              h
            end

          # TODO not call it here
          case Macro.expand(ha, Map.put(e, :line, ElixirUtils.get_line(meta))) do
            ^ha ->
              # elixir raises here undefined_bittype
              # we omit the spec
              expand_each_spec(meta, t, map, s, original_s, e)

            new_types ->
              expand_each_spec(meta, unpack_specs(new_types, []) ++ t, map, s, original_s, e)
          end
      end
    end

    defp expand_each_spec(meta, [_expr | tail], map, s, original_s, e) do
      # elixir raises undefined_bittype
      # we skip it
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

    defp concat_or_prepend_bitstring(_meta, {:<<>>, _, []}, _e_right, acc),
      do: acc

    defp concat_or_prepend_bitstring(
           meta,
           {:<<>>, parts_meta, parts} = e_left,
           e_right,
           acc
         ) do
      # elixir raises unsized_binary in some cases

      case e_right do
        {:binary, _, nil} ->
          {alignment, alignment} = Keyword.fetch!(parts_meta, :alignment)

          if is_integer(alignment) do
            # elixir raises unaligned_binary if alignment != 0
            Enum.reverse(parts, acc)
          else
            [{:"::", meta, [e_left, e_right]} | acc]
          end

        {:bitstring, _, nil} ->
          Enum.reverse(parts, acc)
      end
    end

    defp concat_or_prepend_bitstring(meta, e_left, e_right, acc) do
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

    defp size_and_unit(type, size, unit)
         when type in [:bitstring, :binary] and (size != :default or unit != :default) do
      # elixir raises here bittype_literal_bitstring or bittype_literal_string
      # we don't care
      size_and_unit(type, :default, :default)
    end

    defp size_and_unit(_expr_type, size, unit) do
      add_arg(:unit, unit, add_arg(:size, size, []))
    end

    defp build_spec(_size, _unit, type, endianness, _sign, spec)
         when type in [:utf8, :utf16, :utf32] do
      # elixir raises bittype_signed if signed
      # elixir raises bittype_utf if size specified
      # we don't care

      add_spec(type, add_spec(endianness, spec))
    end

    defp build_spec(_size, _unit, type, _endianness, _sign, spec)
         when type in [:binary, :bitstring] do
      # elixir raises bittype_signed if signed
      # elixir raises bittype_mismatch if bitstring unit != 1 or default
      # we don't care

      add_spec(type, spec)
    end

    defp build_spec(size, unit, type, endianness, sign, spec)
         when type in [:integer, :float] do
      number_size = number_size(size, unit)

      cond do
        type == :float and is_integer(number_size) ->
          if valid_float_size(number_size) do
            add_spec(type, add_spec(endianness, add_spec(sign, spec)))
          else
            # elixir raises here bittype_float_size
            # we fall back to 64
            build_spec(64, :default, type, endianness, sign, spec)
          end

        size == :default and unit != :default ->
          # elixir raises here bittype_unit
          # we fall back to default
          build_spec(size, :default, type, endianness, sign, spec)

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
  end

  defmodule Fn do
    alias ElixirSense.Core.Compiler, as: ElixirExpand
    alias ElixirSense.Core.Compiler.Env, as: ElixirEnv
    alias ElixirSense.Core.Compiler.Clauses, as: ElixirClauses
    alias ElixirSense.Core.Compiler.Dispatch, as: ElixirDispatch
    alias ElixirSense.Core.Compiler.Utils, as: ElixirUtils

    def expand(meta, clauses, s, e) when is_list(clauses) do
      transformer = fn
        {:->, _, [_left, _right]} = clause, sa ->
          # elixir raises defaults_in_args
          s_reset = ElixirEnv.reset_vars(sa)

          # no point in doing type inference here, we have no idea what the fn will be called with
          {e_clause, s_acc, e_acc} =
            ElixirClauses.clause(meta, :fn, &ElixirClauses.head/3, clause, s_reset, e)

          {e_clause, ElixirEnv.merge_vars(s_acc, sa, e_acc)}
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
          ElixirDispatch.import_function(import_meta, atom, length(args), e)
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
                  ElixirDispatch.require_function(require_meta, e_left, right, length(args), ee)

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

  defmodule Quote do
    alias ElixirSense.Core.Compiler.Dispatch, as: ElixirDispatch

    defstruct line: false,
              file: nil,
              context: nil,
              op: :none,
              aliases_hygiene: nil,
              imports_hygiene: nil,
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
        generated: generated
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

    def escape(expr, op, unquote) do
      do_quote(
        expr,
        %__MODULE__{
          line: true,
          file: nil,
          op: op,
          unquote: unquote
        }
      )
    end

    def quote({:unquote_splicing, _, [_]} = expr, %__MODULE__{unquote: true} = q) do
      # elixir raises here unquote_splicing only works inside arguments and block contexts
      # try to recover from error by wrapping it in block
      __MODULE__.quote({:__block__, [], [expr]}, q)
    end

    def quote(expr, q) do
      do_quote(expr, q)
    end

    # quote/unquote

    defp do_quote({:quote, meta, [arg]}, q) when is_list(meta) do
      t_arg = do_quote(arg, %__MODULE__{q | unquote: false})

      new_meta =
        case q do
          %__MODULE__{op: :add_context, context: context} ->
            keystore(:context, meta, context)

          _ ->
            meta
        end

      {:{}, [], [:quote, meta(new_meta, q), [t_arg]]}
    end

    defp do_quote({:quote, meta, [opts, arg]}, q) when is_list(meta) do
      t_opts = do_quote(opts, q)
      t_arg = do_quote(arg, %__MODULE__{q | unquote: false})

      new_meta =
        case q do
          %__MODULE__{op: :add_context, context: context} ->
            keystore(:context, meta, context)

          _ ->
            meta
        end

      {:{}, [], [:quote, meta(new_meta, q), [t_opts, t_arg]]}
    end

    defp do_quote({:unquote, meta, [expr]}, %__MODULE__{unquote: true}) when is_list(meta),
      do: expr

    # Aliases

    defp do_quote({:__aliases__, meta, [h | t] = list}, %__MODULE__{aliases_hygiene: e = %{}} = q)
         when is_atom(h) and h != Elixir and is_list(meta) do
      annotation =
        case NormalizedMacroEnv.expand_alias(e, meta, list, trace: false) do
          {:alias, atom} -> atom
          :error -> false
        end

      alias_meta = keystore(:alias, Keyword.delete(meta, :counter), annotation)
      do_quote_tuple(:__aliases__, alias_meta, [h | t], q)
    end

    # Vars

    defp do_quote({name, meta, nil}, %__MODULE__{op: :add_context} = q)
         when is_atom(name) and is_list(meta) do
      import_meta =
        case q.imports_hygiene do
          nil -> meta
          e -> import_meta(meta, name, 0, q, e)
        end

      {:{}, [], [name, meta(import_meta, q), q.context]}
    end

    # cursor

    defp do_quote(
           {:__cursor__, meta, args},
           %__MODULE__{unquote: _}
         )
         when is_list(args) do
      # emit cursor as is regardless of unquote
      {:__cursor__, meta, args}
    end

    # Unquote

    defp do_quote(
           {{{:., meta, [left, :unquote]}, _, [expr]}, _, args},
           %__MODULE__{unquote: true} = q
         )
         when is_list(meta) do
      do_quote_call(left, meta, expr, args, q)
    end

    defp do_quote({{:., meta, [left, :unquote]}, _, [expr]}, %__MODULE__{unquote: true} = q)
         when is_list(meta) do
      do_quote_call(left, meta, expr, nil, q)
    end

    # Imports

    defp do_quote(
           {:&, meta, [{:/, _, [{f, _, c}, a]}] = args},
           %__MODULE__{imports_hygiene: e = %{}} = q
         )
         when is_atom(f) and is_integer(a) and is_atom(c) and is_list(meta) do
      new_meta =
        case ElixirDispatch.find_import(meta, f, a, e) do
          false ->
            meta

          receiver ->
            keystore(:context, keystore(:imports, meta, [{a, receiver}]), q.context)
        end

      do_quote_tuple(:&, new_meta, args, q)
    end

    defp do_quote({name, meta, args_or_context}, %__MODULE__{imports_hygiene: e = %{}} = q)
         when is_atom(name) and is_list(meta) and
                (is_list(args_or_context) or is_atom(args_or_context)) do
      arity =
        case args_or_context do
          args when is_list(args) -> length(args)
          context when is_atom(context) -> 0
        end

      import_meta = import_meta(meta, name, arity, q, e)
      annotated = annotate({name, import_meta, args_or_context}, q.context)
      do_quote_tuple(annotated, q)
    end

    # Two-element tuples

    defp do_quote({left, right}, %__MODULE__{unquote: true} = q)
         when is_tuple(left) and elem(left, 0) == :unquote_splicing and
                is_tuple(right) and elem(right, 0) == :unquote_splicing do
      do_quote({:{}, [], [left, right]}, q)
    end

    defp do_quote({left, right}, q) do
      t_left = do_quote(left, q)
      t_right = do_quote(right, q)
      {t_left, t_right}
    end

    # Everything else

    defp do_quote(other, q = %{op: op}) when op != :add_context do
      do_escape(other, q)
    end

    defp do_quote({_, _, _} = tuple, q) do
      annotated = annotate(tuple, q.context)
      do_quote_tuple(annotated, q)
    end

    defp do_quote([], _), do: []

    defp do_quote([h | t], %__MODULE__{unquote: false} = q) do
      head_quoted = do_quote(h, q)
      do_quote_simple_list(t, head_quoted, q)
    end

    defp do_quote([h | t], q) do
      do_quote_tail(:lists.reverse(t, [h]), q)
    end

    defp do_quote(other, _), do: other

    defp import_meta(meta, name, arity, q, e) do
      case Keyword.get(meta, :imports, false) == false &&
             ElixirDispatch.find_imports(meta, name, e) do
        [_ | _] = imports ->
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

    defp do_quote_call(left, meta, expr, args, q) do
      all = [left, {:unquote, meta, [expr]}, args, q.context]
      tall = Enum.map(all, fn x -> do_quote(x, q) end)
      {{:., meta, [:elixir_quote, :dot]}, meta, [meta(meta, q) | tall]}
    end

    defp do_quote_tuple({left, meta, right}, q) do
      do_quote_tuple(left, meta, right, q)
    end

    defp do_quote_tuple(left, meta, right, q) do
      t_left = do_quote(left, q)
      t_right = do_quote(right, q)
      {:{}, [], [t_left, meta(meta, q), t_right]}
    end

    defp do_quote_simple_list([], prev, _), do: [prev]

    defp do_quote_simple_list([h | t], prev, q) do
      [prev | do_quote_simple_list(t, do_quote(h, q), q)]
    end

    defp do_quote_simple_list(other, prev, q) do
      [{:|, [], [prev, do_quote(other, q)]}]
    end

    defp do_quote_tail(
           [{:|, meta, [{:unquote_splicing, _, [left]}, right]} | t],
           %__MODULE__{unquote: true} = q
         ) do
      tt = do_quote_splice(t, q, [], [])
      tr = do_quote(right, q)
      do_runtime_list(meta, :tail_list, [left, tr, tt])
    end

    defp do_quote_tail(list, q) do
      do_quote_splice(list, q, [], [])
    end

    defp do_quote_splice(
           [{:unquote_splicing, meta, [expr]} | t],
           %__MODULE__{unquote: true} = q,
           buffer,
           acc
         ) do
      runtime = do_runtime_list(meta, :list, [expr, do_list_concat(buffer, acc)])
      do_quote_splice(t, q, [], runtime)
    end

    defp do_quote_splice([h | t], q, buffer, acc) do
      th = do_quote(h, q)
      do_quote_splice(t, q, [th | buffer], acc)
    end

    defp do_quote_splice([], _q, buffer, acc) do
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

    defp do_escape({left, meta, right}, q = %{op: :prune_metadata}) when is_list(meta) do
      tm = for {k, v} <- meta, k == :no_parens or k == :line, do: {k, v}
      tl = do_quote(left, q)
      tr = do_quote(right, q)
      {:{}, [], [tl, tm, tr]}
    end

    defp do_escape(tuple, q) when is_tuple(tuple) do
      tt = do_quote(Tuple.to_list(tuple), q)
      {:{}, [], tt}
    end

    defp do_escape(bitstring, _) when is_bitstring(bitstring) do
      case Bitwise.band(bit_size(bitstring), 7) do
        0 ->
          bitstring

        size ->
          <<bits::size(size), bytes::binary>> = bitstring

          {:<<>>, [],
           [{:"::", [], [bits, {size, [], [size]}]}, {:"::", [], [bytes, {:binary, [], nil}]}]}
      end
    end

    defp do_escape(map, q) when is_map(map) do
      tt = do_quote(Enum.sort(Map.to_list(map)), q)
      {:%{}, [], tt}
    end

    defp do_escape([], _), do: []

    defp do_escape([h | t], %__MODULE__{unquote: false} = q) do
      do_quote_simple_list(t, do_quote(h, q), q)
    end

    defp do_escape([h | t], q) do
      # The improper case is inefficient, but improper lists are rare.
      try do
        l = Enum.reverse(t, [h])
        do_quote_tail(l, q)
      catch
        _ ->
          {l, r} = reverse_improper(t, [h])
          tl = do_quote_splice(l, q, [], [])
          tr = do_quote(r, q)
          update_last(tl, fn x -> {:|, [], [x, tr]} end)
      end
    end

    defp do_escape(other, _) when is_number(other) or is_pid(other) or is_atom(other),
      do: other

    defp do_escape(fun, _) when is_function(fun) do
      case {Function.info(fun, :env), Function.info(fun, :type)} do
        {{:env, []}, {:type, :external}} ->
          fun_to_quoted(fun)

        _ ->
          # elixir raises here ArgumentError
          nil
      end
    end

    defp do_escape(_other, _) do
      # elixir raises here ArgumentError
      nil
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
  end

  defmodule Dispatch do
    alias ElixirSense.Core.Compiler.Rewrite, as: ElixirRewrite
    import :ordsets, only: [is_element: 2]

    def find_import(meta, name, arity, e) do
      tuple = {name, arity}

      case find_import_by_name_arity(meta, tuple, [], e) do
        {:function, receiver} ->
          # TODO trace call?
          # ElixirEnv.trace({:imported_function, meta, receiver, name, arity}, e)
          receiver

        {:macro, receiver} ->
          # TODO trace call?
          # ElixirEnv.trace({:imported_macro, meta, receiver, name, arity}, e)
          receiver

        {:ambiguous, [head | _]} ->
          # elixir raises here, we choose first one
          # TODO trace call?
          head

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

        {:ambiguous, ambiguous} ->
          raise "ambiguous #{inspect(ambiguous)}"

        false ->
          if Macro.special_form?(name, arity) do
            false
          else
            function = e.function

            # TODO the condition has this at the end
            # and ElixirDef.local_for(meta, name, arity, [:defmacro, :defmacrop], e)
            if function != nil and function != tuple do
              false
            else
              # ElixirEnv.trace({:local_function, meta, name, arity}, e)
              # ElixirLocals.record_local(tuple, e.module, function, meta, false)
              # TODO we may want to record
              {:local, name, arity}
            end
          end
      end
    end

    def require_function(meta, receiver, name, arity, e) do
      required = receiver in e.requires

      if is_macro(name, arity, receiver, required) do
        false
      else
        # ElixirEnv.trace({:remote_function, meta, receiver, name, arity}, e)
        remote_function(meta, receiver, name, arity, e)
      end
    end

    defp remote_function(_meta, receiver, name, arity, _e) do
      case ElixirRewrite.inline(receiver, name, arity) do
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
          # elixir raises here ambiguous_call
          find_imports_by_name(name, imports, acc, mod, meta, e)
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
              {:ambiguous, fun_match ++ mac_match}
          end
      end
    end

    defp find_import_by_name_arity(tuple, list) do
      for {receiver, set} <- list, is_element(tuple, set), do: receiver
    end

    defp is_import(meta, arity) do
      with {:ok, imports = [_ | _]} <- Keyword.fetch(meta, :imports),
           {:ok, _} <- Keyword.fetch(meta, :context),
           {_arity, receiver} <- :lists.keyfind(arity, 1, imports) do
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
      map_args =
        case right do
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
      state =
        state
        |> new_func_vars_scope

      {ast, state, env} = do_expand_spec(ast, state, env)

      state =
        state
        |> remove_func_vars_scope

      {ast, state, env}
    end

    defp do_expand_spec({:when, meta, [spec, guard]}, state, env) when is_list(guard) do
      {spec, guard, state, env} = do_expand_spec(spec, guard, meta, state, env)
      {{:when, meta, [spec, guard]}, state, env}
    end

    defp do_expand_spec(spec, state, env) do
      {spec, _guard, state, env} = do_expand_spec(spec, [], [], state, env)
      {spec, state, env}
    end

    defp do_expand_spec(
           {:"::", meta, [{name, name_meta, args}, return]},
           guard,
           guard_meta,
           state,
           env
         )
         when is_atom(name) and name != :"::" do
      args =
        if is_atom(args) do
          []
        else
          args
        end
        |> sanitize_args()

      guard = if Keyword.keyword?(guard), do: guard, else: []

      state =
        Enum.reduce(guard, state, fn {name, _val}, state ->
          # guard is a keyword list so we don't have exact meta on keys
          add_var_write(state, {name, guard_meta, nil})
        end)

      {args_reverse, state, env} =
        Enum.reduce(args, {[], state, env}, fn arg, {acc, state, env} ->
          {arg, state, env} = expand_typespec(arg, state, env)
          {[arg | acc], state, env}
        end)

      args = Enum.reverse(args_reverse)

      {return, state, env} = expand_typespec(return, state, env)

      {guard_reverse, state, env} =
        Enum.reduce(guard, {[], state, env}, fn
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
      state =
        state
        |> new_func_vars_scope

      {ast, state, env} = do_expand_type(ast, state, env)

      state =
        state
        |> remove_func_vars_scope

      {ast, state, env}
    end

    defp do_expand_type({:"::", meta, [{name, name_meta, args}, definition]}, state, env) do
      args =
        if is_atom(args) do
          []
        else
          args
        end

      state =
        Enum.reduce(args, state, fn
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
      :|,
      :<<>>,
      :%{},
      :%,
      :..,
      :->,
      :"::",
      :+,
      :-,
      :.,
      :{},
      :__block__,
      :...
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
        Macro.traverse(
          ast,
          {state, env},
          fn
            {:__aliases__, _meta, list} = node, {state, env} when is_list(list) ->
              {node, state, env} = ElixirExpand.expand(node, state, env)
              {node, {state, env}}

            {:__MODULE__, _meta, ctx} = node, {state, env} when is_atom(ctx) ->
              {node, state, env} = ElixirExpand.expand(node, state, env)
              {node, {state, env}}

            {:"::", meta, [{var_name, var_meta, context}, expr]}, {state, env}
            when is_atom(var_name) and is_atom(context) ->
              # mark as annotation
              {{:"::", meta, [{var_name, [{:annotation, true} | var_meta], context}, expr]},
               {state, env}}

            {name, meta, args}, {state, env}
            when is_atom(name) and is_atom(args) and name not in @special_forms and
                   hd(meta) != {:annotation, true} ->
              [vars_from_scope | _other_vars] = state.vars_info

              ast =
                case Elixir.Map.get(vars_from_scope, {name, nil}) do
                  nil ->
                    # add parens to no parens local call
                    {name, meta, []}

                  _ ->
                    {name, meta, args}
                end

              {ast, {state, env}}

            other, acc ->
              {other, acc}
          end,
          fn
            {{:., dot_meta, [remote, name]}, meta, args}, {state, env} when is_atom(remote) ->
              line = Keyword.get(meta, :line, 0)
              column = Keyword.get(meta, :column, nil)

              args =
                if is_atom(args) do
                  []
                else
                  args
                end

              state = add_call_to_line(state, {remote, name, length(args)}, {line, column})

              {{{:., dot_meta, [remote, name]}, meta, args}, {state, env}}

            {name, meta, args}, {state, env}
            when is_atom(name) and is_list(args) and name not in @special_forms ->
              line = Keyword.get(meta, :line, 0)
              column = Keyword.get(meta, :column, nil)

              state = add_call_to_line(state, {nil, name, length(args)}, {line, column})

              {{name, meta, args}, {state, env}}

            {name, meta, context} = var, {state, env}
            when is_atom(name) and is_atom(context) and hd(meta) != {:annotation, true} ->
              state = add_var_read(state, var)
              {var, {state, env}}

            other, acc ->
              {other, acc}
          end
        )

      {ast, state, env}
    end

    # TODO Remove char_list type by v2.0
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

  defmodule Rewrite do
    def inline(module, fun, arity) do
      :elixir_rewrite.inline(module, fun, arity)
    end

    def rewrite(context, receiver, dot_meta, right, meta, e_args, s) do
      do_rewrite(context, receiver, dot_meta, right, meta, e_args, s)
    end

    defp do_rewrite(_, :erlang, _, :+, _, [arg], _s) when is_number(arg), do: {:ok, arg}

    defp do_rewrite(_, :erlang, _, :-, _, [arg], _s) when is_number(arg), do: {:ok, -arg}

    defp do_rewrite(:match, receiver, dot_meta, right, meta, e_args, _s) do
      :elixir_rewrite.match_rewrite(receiver, dot_meta, right, meta, e_args)
    end

    defp do_rewrite(:guard, receiver, dot_meta, right, meta, e_args, s) do
      :elixir_rewrite.guard_rewrite(receiver, dot_meta, right, meta, e_args, guard_context(s))
    end

    defp do_rewrite(_, receiver, dot_meta, right, meta, e_args, _s) do
      {:ok, :elixir_rewrite.rewrite(receiver, dot_meta, right, meta, e_args)}
    end

    # TODO probably we can remove it/hardcode, used only for generating error message
    defp guard_context(%{prematch: {_, _, {:bitsize, _}}}), do: "bitstring size specifier"
    defp guard_context(_), do: "guard"
  end
end
