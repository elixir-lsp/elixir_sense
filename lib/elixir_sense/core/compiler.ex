defmodule ElixirSense.Core.Compiler do
  @moduledoc false
  alias ElixirSense.Core.Compiler.State
  require Logger
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.TypeInfo
  alias ElixirSense.Core.TypeInference
  alias ElixirSense.Core.TypeInference.Guard
  alias ElixirSense.Core.Normalized.Macro.Env, as: NormalizedMacroEnv
  alias ElixirSense.Core.State.ModFunInfo
  alias ElixirSense.Core.ElixirTypes

  @trace_key :elixir_sense_trace
  @trace_paused_key :elixir_sense_trace_paused

  # Whether to emit the `{:version, N}` meta stamps on `case`/`cond`/`receive`/
  # `try`/`with`/`for`/`_` and bump the version counter accordingly. Introduced
  # in Elixir v1.20 (commit 603602e67 — reverse arrows for case). We gate by
  # host version so ElixirSense's expansion stays in lock-step with the host
  # Elixir's `:elixir_expand` output across 1.16–1.20+.
  @stamp_version Version.match?(System.version(), ">= 1.20.0-dev")

  # Stamps {:version, counter} on meta and bumps the counter — but only when
  # the host Elixir is 1.20+. On older hosts this is the identity so tests
  # comparing against `:elixir_expand` stay aligned.
  defp stamp_version(meta, s) do
    if @stamp_version do
      counter = s.unused
      {[{:version, counter} | meta], %{s | unused: counter + 1}}
    else
      {meta, s}
    end
  end

  def pause_trace do
    Process.put(@trace_paused_key, true)
  end

  def resume_trace do
    Process.delete(@trace_paused_key)
  end

  def trace(event, env) do
    if Process.get(@trace_paused_key) do
      :ok
    else
      trace = get_trace()
      Process.put(@trace_key, [{event, env} | trace])
      :ok
    end
  end

  defp get_trace do
    Process.get(@trace_key, [])
  end

  defp clear_trace do
    Process.delete(@trace_key)
  end

  def collect_traces(state) do
    trace = get_trace()

    state =
      Enum.reduce(trace, state, fn {event, env}, acc ->
        case event do
          {:alias_reference, meta, alias} ->
            # emitted by Macro.expand/2 and Macro.expand_once/2
            acc
            |> State.add_call_to_line({alias, nil, nil}, meta, :alias_reference)

          {:alias, meta, module, _as, _opts} ->
            # emitted by Macro.Env.define_alias/3 and Macro.Env.define_require/3
            acc
            |> State.add_call_to_line({module, nil, nil}, meta, :alias)

          {kind, meta, module, _opts} when kind in [:import, :require] ->
            # emitted by Macro.Env.define_import/3 and Macro.Env.define_require/3
            acc
            |> State.add_call_to_line({module, nil, nil}, meta, kind)

          {kind, meta, receiver, name, arity}
          when kind in [:remote_function, :remote_macro, :imported_function, :imported_macro] ->
            # emitted by MacroEnv.expand_require/6, MacroEnv.expand_import/5
            acc
            |> State.add_call_to_line({receiver, name, arity}, meta, kind)

          {:imported_quoted, meta, module, name, arities} ->
            for arity <- arities, reduce: acc do
              acc ->
                acc
                |> State.add_call_to_line({module, name, arity}, meta, :imported_quoted)
            end

          {kind, meta, name, arity} when kind in [:local_function, :local_macro] ->
            # emitted by MacroEnv.expand_require/6, MacroEnv.expand_import/5
            acc
            |> State.add_call_to_line({env.module, name, arity}, meta, kind)

          {:struct_expansion, meta, name, _assocs} ->
            acc
            |> State.add_call_to_line({name, nil, nil}, meta, :struct_expansion)

          {:alias_expansion, meta, as, alias} ->
            acc
            |> State.add_call_to_line({as, nil, nil}, meta, :alias_expansion_as)
            |> State.add_call_to_line({alias, nil, nil}, meta, :alias_expansion)

          _ ->
            Logger.warning("Unhandled trace event: #{inspect(event)}")
            acc
        end
      end)

    clear_trace()
    state
  end

  def env do
    env = :elixir_env.new()
    %{env | tracers: [__MODULE__]}
  end

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
      _kind, _payload ->
        # Logger.warning(
        #   "Unable to expand ast node #{inspect(ast)}: #{Exception.format(kind, payload, __STACKTRACE__)}"
        # )

        {ast, state, env}
    end
  end

  # =/2

  defp do_expand({:=, meta, [_, _]} = expr, s, %{context: :match} = e) do
    {e_expr, se, ee} = __MODULE__.Clauses.parallel_match(meta, expr, s, e)

    vars_with_inferred_types = TypeInference.find_typed_vars(e_expr, nil, :match)

    {vars_with_inferred_types, var_descrs} =
      merge_elixir_types_pattern_vars(vars_with_inferred_types, e_expr, se)

    se = State.merge_inferred_types(se, vars_with_inferred_types)
    se = State.merge_inferred_elixir_types(se, var_descrs)

    {e_expr, se, ee}
  end

  defp do_expand({:=, meta, [left, right]}, s, e) do
    # elixir validates we are not in guard context
    {e_right, sr, er} = expand(right, s, e)
    {e_left, sl, el} = __MODULE__.Clauses.match(&expand/3, left, sr, s, er)

    e_expr = {:=, meta, [e_left, e_right]}

    vars_with_inferred_types = TypeInference.find_typed_vars(e_expr, nil, el.context)

    {vars_with_inferred_types, var_descrs} =
      merge_elixir_types_pattern_vars(vars_with_inferred_types, e_expr, sl)

    sl = State.merge_inferred_types(sl, vars_with_inferred_types)
    sl = State.merge_inferred_elixir_types(sl, var_descrs)

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

  defp do_expand({:__aliases__, _, _} = alias, state, env) do
    expand_aliases(alias, state, env, true)
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

    {arg, state, env} = expand_without_aliases_report(arg, state, env)
    {opts, state, env} = expand_opts([:as, :warn], no_alias_opts(opts), state, env)

    if is_atom(arg) do
      case NormalizedMacroEnv.define_alias(env, meta, arg, [trace: true] ++ opts) do
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

    {arg, state, env} = expand_without_aliases_report(arg, state, env)

    {opts, state, env} =
      expand_opts([:as, :warn], no_alias_opts(opts), state, env)

    # elixir handles special meta key :defined in the require call.
    # It is only set by defmodule and we handle it there

    if is_atom(arg) do
      # elixir calls here :elixir_aliases.ensure_loaded(meta, e_ref, et)
      # and optionally waits until required module is compiled
      case NormalizedMacroEnv.define_require(env, meta, arg, [trace: true] ++ opts) do
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

    {arg, state, env} = expand_without_aliases_report(arg, state, env)
    {opts, state, env} = expand_opts([:only, :except, :warn], opts, state, env)

    if is_atom(arg) do
      opts =
        opts
        |> Keyword.merge(
          trace: true,
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

    {escape_map(escape_env_entries(meta, state, %{env | tracers: []})), state, env}
  end

  defp do_expand({{:., dot_meta, [{:__ENV__, meta, atom}, field]}, call_meta, []}, s, e)
       when is_atom(atom) and is_atom(field) do
    # elixir checks if context is not match
    s = State.add_current_env_to_line(s, call_meta, e)

    env = escape_env_entries(meta, s, %{e | tracers: []})

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

    binding_meta = Keyword.delete(meta, :column)

    e_binding =
      for {k, v} <- binding do
        {:{}, [], [:=, [], [{:{}, [], [k, binding_meta, e_context]}, v]]}
      end

    e_binding_quoted =
      case e_binding do
        [] -> e_quoted
        _ -> {:{}, [], [:__block__, [], e_binding ++ [e_quoted]]}
      end

    e_block =
      case e_prelude do
        [] -> e_binding_quoted
        _ -> {:__block__, [], e_prelude ++ [e_binding_quoted]}
      end

    # Wrap the quote result in a remote call to `elixir_quote.validate_quote/1`
    # outside of match/guard context. Mirrors upstream commit 3a038d176
    # (Wrap quote in a function that will implement recursive types) +
    # the partial revert 7a9ea30d9 that skips the wrap inside match/guard.
    # 1.20+ only — keeps test parity with host expansion on older hosts.
    if @stamp_version and e.context == nil do
      {{{:., meta, [:elixir_quote, :validate_quote]}, meta, [e_block]}, es, eq}
    else
      {e_block, es, eq}
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
        expand({:&, meta, [{:/, arity_meta, [{name, super_meta, context}, arity]}]}, s, e)

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
    # Stamp a {:version, N} on the block (1.20+ only — see stamp_version/2).
    # Mirrors upstream commit 603602e67 (reverse arrows for case).
    {meta, sc} = stamp_version(meta, sc)
    {{:cond, meta, [e_clauses]}, sc, ec}
  end

  defp do_expand({:case, meta, [expr, options]}, s, e) do
    expand_case(meta, expr, options, s, e)
  end

  defp do_expand({:receive, meta, [opts]}, s, e) do
    {e_clauses, sc, ec} = __MODULE__.Clauses.receive(opts, s, e)
    {meta, sc} = stamp_version(meta, sc)
    {{:receive, meta, [e_clauses]}, sc, ec}
  end

  defp do_expand({:try, meta, [opts]}, s, e) do
    {e_clauses, sc, ec} = __MODULE__.Clauses.try(opts, s, e)
    {meta, sc} = stamp_version(meta, sc)
    {{:try, meta, [e_clauses]}, sc, ec}
  end

  defp do_expand({:for, _, [_ | _]} = expr, s, e), do: expand_for(expr, s, e, true)

  defp do_expand({:with, meta, [_ | _] = args}, s, e) do
    __MODULE__.Clauses.with(meta, args, s, e)
  end

  # Cursor

  defp do_expand({:__cursor__, meta, args}, s, e) when is_list(args) do
    s =
      if s.cursor_env do
        s
      else
        s
        |> State.add_cursor_env(meta, e)
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
          |> State.add_call_to_line({e.module, name, arity}, meta, :local_function)
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

      {arg, ss, _e} ->
        # elixir raises here invalid_arg_for_pin (and marks tainted_function;
        # upstream commit 5bad452d0). We mirror the taint for state parity but
        # don't emit the warning — ElixirSense's diagnostic surfaces differ.
        # we may have cursor in arg
        # restore prematch and vars
        {{:^, meta, [arg]}, %{ss | prematch: s.prematch, vars: s.vars, tainted_function: true}, e}
    end
  end

  defp do_expand({:^, _meta, [arg]}, s, e) do
    # elixir raises here pin_outside_of_match (and marks tainted_function;
    # upstream commit 5bad452d0).
    # try to recover from error by dropping the pin and expanding arg
    {arg, s, e} = expand(arg, s, e)
    {arg, %{s | tainted_function: true}, e}
  end

  defp do_expand({:_, meta, kind} = var, s, %{context: context} = e) when is_atom(kind) do
    # Stamp a {:version, N} on `_` and bump the counter (1.20+ only —
    # upstream commit 603602e67 — reverse arrows). Pre-1.20 the underscore
    # passes through with no meta change.
    # elixir raises unbound_underscore if context is not match (commit
    # 5bad452d0 also marks tainted_function in the non-match branch).
    s = if context == :match, do: s, else: %{s | tainted_function: true}

    if @stamp_version do
      {meta, s} = stamp_version(meta, s)
      {{:_, meta, kind}, s, e}
    else
      {var, s, e}
    end
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
          case e do
            %{context: :guard} ->
              :raise

            %{} when s.prematch == :pin ->
              :pin

            _ ->
              Code.get_compiler_option(:on_undefined_variable)
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
            # elixir raises here undefined_var and marks tainted_function
            # (upstream commit 5bad452d0).
            {{name, meta, kind}, %{s | tainted_function: true}, e}

          # elixir plans to remove this clause on v2.0
          _ when error == :warn ->
            # convert to local call and add if_undefined meta
            expand({name, [{:if_undefined, :warn} | meta], []}, s, e)

          _ when error == :pin ->
            # elixir raises here undefined_var_pin (marks tainted_function)
            {{name, meta, kind}, %{s | tainted_function: true}, e}

          _ ->
            # elixir raises here undefined_var and attaches span meta
            # (marks tainted_function)
            {{name, meta, kind}, %{s | tainted_function: true}, e}
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
           trace: true,
           check_deprecations: false,
           allow_locals:
             allow_locals &&
               fn ->
                 case state.mods_funs_to_positions[{env.module, fun, arity}] do
                   nil ->
                     false

                   %ModFunInfo{} = info ->
                     category = ModFunInfo.get_category(info)
                     definition_line = info.positions |> List.first() |> elem(0)
                     usage_line = meta |> Keyword.get(:line)

                     if category == :macro and usage_line >= definition_line do
                       proper_arity = arity + 1
                       proper_name = :"MACRO-#{fun}"

                       if Code.ensure_loaded?(env.module) and
                            macro_exported?(env.module, fun, arity) do
                         Function.capture(env.module, proper_name, proper_arity)
                       else
                         Function.capture(
                           ElixirSense.Core.Compiler.FakeLocal,
                           :ok_fun,
                           proper_arity
                         )
                       end
                     else
                       false
                     end
                 end
               end
         ) do
      {:macro, module, callback} ->
        # NOTE there is a subtle difference - callback will call expander with state derived from env via
        # :elixir_env.env_to_ex(env) possibly losing some details. Jose Valim is convinced this is not a problem

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
    {module, state_l, env} = expand(module, State.prepare_write(state, env), env)
    arity = length(args)

    if is_atom(module) do
      case __MODULE__.Rewrite.inline(module, fun, arity) do
        {ar, an} ->
          state_l =
            state_l
            |> State.add_call_to_line({module, fun, arity}, meta, :remote_function)
            |> State.add_current_env_to_line(meta, env)

          expand_remote(ar, dot_meta, an, meta, args, state, state_l, env)

        false ->
          case NormalizedMacroEnv.expand_require(env, meta, module, fun, arity,
                 trace: true,
                 check_deprecations: false
               ) do
            {:macro, module, callback} ->
              # NOTE there is a subtle difference - callback will call expander with state derived from env via
              # :elixir_env.env_to_ex(env) possibly losing some details. Jose Valim is convinced this is not a problem
              # elixir expands the macro with original state, we pass state after left expand and need to revert some props
              state_l = %{
                state_l
                | vars: state.vars,
                  prematch: state.prematch,
                  unused: state.unused,
                  stacktrace: state.stacktrace,
                  caller: state.caller,
                  runtime_modules: state.runtime_modules
              }

              expand_macro(meta, module, fun, args, callback, state_l, env)

            :error ->
              state_l =
                state_l
                |> State.add_call_to_line({module, fun, arity}, meta, :remote_function)
                |> State.add_current_env_to_line(meta, env)

              expand_remote(module, dot_meta, fun, meta, args, state, state_l, env)
          end
      end
    else
      state_l =
        state_l
        |> State.add_call_to_line({module, fun, arity}, meta, :remote_function)
        |> State.add_current_env_to_line(meta, env)

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
      |> State.add_call_to_line({nil, e_expr, length(e_args)}, dot_meta, :anonymous_function)
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
         :use,
         [target_module | _opts] = args,
         callback,
         state,
         env
       ) do
    {expanded_module, state, env} = expand(target_module, state, env)

    state =
      if is_atom(expanded_module) do
        State.add_use(expanded_module, state, env)
      else
        state
      end

    expand_macro_callback(meta, Kernel, :use, args, callback, state, env)
  end

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
    state_orig_prematch = state.prematch
    {opts, state, env} = expand(opts, state, env)
    # elixir does validation here
    target = Keyword.get(opts, :to, :__unknown__)

    # TODO Remove List.wrap when multiple funs are no longer supported by elixir
    state =
      funs
      |> List.wrap()
      |> Enum.reduce(state, fn fun, state ->
        state_orig = state

        {fun, state} =
          case fun do
            {:__cursor__, _, list} when is_list(list) ->
              fa =
                case list do
                  [{f, _, a} | _] ->
                    {f, if(is_list(a), do: length(a), else: 0)}

                  _ ->
                    {:__unknown__, 0}
                end

              {expanded_fun, state, _} = expand(fun, state, %{env | function: fa})
              {expanded_fun, state}

            # sometimes cursor turns the AST around and transforms function head node into a 2-tuple
            {{:__cursor__, cursor_meta, [{fun, _, context}]}, meta, args}
            when (is_atom(fun) and is_atom(context) and is_list(args)) or is_atom(args) ->
              fa = {fun, if(is_list(args), do: length(args), else: 0)}

              {expanded_fun, state, _} =
                expand({:__cursor__, cursor_meta, [{fun, meta, args}]}, state, %{
                  env
                  | function: fa
                })

              {expanded_fun, state}

            _ ->
              {fun, state}
          end

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
          if has_unquotes do
            # remove scope
            State.remove_vars_scope(state, state_orig)
          else
            # restore module vars
            State.remove_func_vars_scope(state, state_orig)
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

    {[], %{state | prematch: state_orig_prematch}, env}
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
      if Keyword.keyword?(arg) do
        State.register_optional_callbacks(state, arg)
      else
        state
      end

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
      |> State.add_spec(env, name, type_args, spec, kind, range, spec_ast: expr)
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
        [arg] -> TypeInference.type_of(arg, env.context, state, env)
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

    {state, {doc, doc_meta}} = State.consume_doc_context(state)
    options = [generated: true]

    state =
      state
      |> State.add_func_to_index(
        env,
        name,
        [{:\\, [], [{:args, [], nil}, []]}],
        range,
        type,
        options |> Keyword.put(:doc, doc) |> Keyword.put(:meta, doc_meta)
      )
      |> State.add_func_to_index(
        env,
        name,
        [{:record, [], nil}, {:args, [], nil}],
        range,
        type,
        options
      )
      |> State.add_record(env, call, name, fields, doc, doc_meta)
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

    # TODO this can raise on some shapes, e.g. expanding a `defprotocol` whose
    # body fails to expand surfaces:
    #   [warning] Unable to expand ast node {:defprotocol, ...,
    #   [{:__aliases__, ..., [:Proto]}, [do: {:def, ..., [{:reverse, ...}]}]]}:
    #   ** (MatchError) no match of right hand side value: []
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
         [alias, {:__cursor__, _, cursor_args} = cursor],
         callback,
         state,
         env
       )
       when is_list(cursor_args) do
    # tolerant parsers may produce a cursor in place of the do block
    # expand as if the cursor was the module body
    expand_macro(meta, Kernel, :defmodule, [alias, [do: cursor]], callback, state, env)
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

    {expanded, _state, _env} = expand_without_aliases_report(alias, state, env)

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

    # Module body fully expanded: every clause of every local function is now
    # accumulated. Run ElixirTypes local inference once per function, then prune
    # the stored clause ASTs.
    state = infer_module_local_signatures(state, %{env | module: full})

    # here we handle module callbacks. Only before_compile macro callbacks are expanded as they
    # affect module body. Func before_compile callbacks are not executed. on_definition after_compile and after_verify
    # are not executed as we do not preform a real compilation
    {state, _e_env} =
      ~w(before_compile after_compile after_verify on_definition on_load)a
      |> Enum.reduce({state, e_env}, fn attribute, {state, e_env} ->
        for args <- Map.get(state.attribute_store, {full, attribute}, []) do
          case {attribute, args} do
            {:on_load, function} -> function
            {_, {module, fun}} -> [module, fun]
            {_, module} -> [module, :"__#{attribute}__"]
          end
        end
        |> Enum.reduce({state, e_env}, fn target, {state, env} ->
          # module vars are not accessible in module callbacks
          env = %{env | versioned_vars: %{}, line: meta[:line]}
          state_orig = state
          state = State.new_func_vars_scope(state)

          # elixir dispatches callbacks by raw dispatch and eval_forms
          # instead we expand a block with require and possibly expand macros
          # we do not attempt to exec function callbacks
          args =
            case attribute do
              :before_compile -> [env]
              :after_compile -> [env, <<>>]
              :after_verify -> [full]
              :on_definition -> [env, nil, nil, [], [], []]
              :on_load -> []
            end

          ast =
            case target do
              function when is_atom(function) ->
                {:__block__, [],
                 [
                   {function, [line: meta[:line]], args}
                 ]}

              [module, _function] ->
                {:__block__, [],
                 [
                   {:require, [], [module]},
                   {{:., [line: meta[:line]], target}, [line: meta[:line]], args}
                 ]}
            end

          {_result, state, env} = expand(ast, state, env)
          {State.remove_func_vars_scope(state, state_orig), env}
        end)
      end)

    # elixir forces runtime dependency on behaviour via Module.force_behaviour_dependencies
    # which emits a trace for a fake behaviour_info call
    state =
      Enum.reduce(Map.get(state.behaviours, full, []), state, fn behaviour, state ->
        state
        |> State.add_call_to_line(
          {behaviour, :behaviour_info, 1},
          [line: meta[:line]],
          :remote_function
        )
      end)

    # restore vars from outer scope
    # restore version counter
    state =
      state
      |> State.apply_optional_callbacks(%{env | module: full})
      |> State.remove_vars_scope(state_orig, true)
      |> State.remove_attributes_scope()
      |> State.remove_module(%{env | module: full})

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
    {call, state} =
      case call do
        {:__cursor__, _, list} when is_list(list) ->
          fa =
            case list do
              [{f, _, a} | _] ->
                {f, if(is_list(a), do: length(a), else: 0)}

              _ ->
                {:__unknown__, 0}
            end

          {expanded_call, state, _} = expand(call, state, %{env | function: fa})
          {expanded_call, state}

        # sometimes cursor turns the AST around and transforms function head node into a 2-tuple
        {{:__cursor__, cursor_meta, [{fun, _, context}]}, meta, args}
        when (is_atom(fun) and is_atom(context) and is_list(args)) or is_atom(args) ->
          fa = {fun, if(is_list(args), do: length(args), else: 0)}

          {expanded_call, state, _} =
            expand({:__cursor__, cursor_meta, [{fun, meta, args}]}, state, %{
              env
              | function: fa
            })

          {expanded_call, state}

        _ ->
          {call, state}
      end

    state_orig = state

    unquoted_call = __MODULE__.Quote.has_unquotes(call)
    unquoted_expr = __MODULE__.Quote.has_unquotes(expr)
    has_unquotes = unquoted_call or unquoted_expr

    # if there are unquote fragments in either call or body elixir escapes both and evaluates
    # if unquoted_expr or unquoted_call, do: __MODULE__.Quote.escape({call, expr}, :escape, true)
    # instead we try to expand the call and body ignoring the unquotes
    # (Note: op was renamed `:none` → `:escape` upstream — commit 2ee1d0eb7.)

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
      if has_unquotes do
        # make module variables accessible if there are unquote fragments in def body
        %{
          state
          | caller: def_kind in [:defmacro, :defmacrop, :defguard, :defguardp]
        }
        |> State.new_vars_scope()
      else
        # module vars are not accessible in def body
        %{
          state
          | caller: def_kind in [:defmacro, :defmacrop, :defguard, :defguardp]
        }
        |> State.new_func_vars_scope()
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

    # Function-parameter patterns have no scrutinee, so most bind no type
    # (`def f({:ok, x})` can't know `x` without a value). A binary-segment
    # spec, however, fixes the variable type intrinsically
    # (`def f(<<n::integer, rest::binary>>)` → `n :: integer`, `rest :: binary`).
    # Passing `nil` as the match context keeps every non-intrinsic pattern at
    # `nil`, so only the segment-typed vars are captured — nothing spurious.
    param_typed_vars = TypeInference.find_typed_vars(e_args_no_defaults, nil, :match)
    state = State.merge_inferred_types(state, param_typed_vars)

    # Inside a `defimpl`, the first argument of a *protocol callback* is the type
    # being implemented for (`defimpl String.Chars, for: URI` → in
    # `def to_string(t)`, `t :: %URI{}`). This matches Elixir 1.19/1.20 and is
    # version-independent here. Only functions whose name/arity belong to the
    # protocol are callbacks — plain helper `def`s in the impl are left alone.
    state =
      with :def <- def_kind,
           for_type when not is_nil(for_type) <-
             protocol_impl_arg_type(state, env_for_expand, name, arity),
           [first_arg | _] <- e_args_no_defaults do
        impl_vars = TypeInference.find_typed_vars(first_arg, for_type, :match)
        State.merge_inferred_types(state, impl_vars)
      else
        _ -> state
      end

    # elixir calls validate_cycles here

    args =
      Enum.zip(args, e_args_no_defaults)
      |> Enum.map(fn
        {{:"\\\\", meta, [_, expanded_default]}, expanded_arg} ->
          {:"\\\\", meta, [expanded_arg, expanded_default]}

        {_, expanded_arg} ->
          expanded_arg
      end)

    guard_prematch =
      if Version.match?(System.version(), ">= 1.18.0-dev") do
        :none
      else
        Code.get_compiler_option(:on_undefined_variable)
      end

    {e_guard, state, env_for_expand} =
      __MODULE__.Clauses.guard(
        guards,
        %{state | prematch: guard_prematch},
        %{env_for_expand | context: :guard}
      )

    type_info = Guard.type_information_from_guards(e_guard)

    state = State.merge_inferred_types(state, type_info)

    env_for_expand = %{env_for_expand | context: nil}

    prematch =
      if Version.match?(System.version(), ">= 1.18.0-dev") do
        state.prematch
      else
        Code.get_compiler_option(:on_undefined_variable)
      end

    state =
      %{state | prematch: prematch}
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
            # do block with receive/catch/else/after — wrap in try.
            # NOTE origin/definition kind may be not correct here. The meta
            # annotation is consumed only by error messages in
            # `:elixir_clauses`. Upstream renamed the key from `:origin` to
            # `:definition` in 1.20 (commit 40e930607 — Use definition instead
            # of origin in meta). We emit both so older and newer hosts find
            # what they expect.
            {:try, [{:origin, def_kind}, {:definition, def_kind} | meta], [expr]}
          else
            # elixir raises here
            expr
          end
      end

    {e_body, state, _env_for_expand} =
      expand(expr, state, env_for_expand)

    # Capture clause AST for ElixirTypes inference
    state =
      if ElixirTypes.enabled?() and def_kind in [:def, :defp] and
           not has_unquotes and name != :__unknown__ do
        clause_ast = %{
          meta: meta,
          args: e_args_no_defaults,
          guards: e_guard,
          body: e_body
        }

        # Only accumulate the clause AST here. Inference is deferred until the
        # whole module body has been expanded (see infer_module_local_signatures/2
        # called from defmodule expansion) so that we run inference exactly once
        # per function instead of re-running it after every clause (was O(n^2) in
        # the number of clauses — old backlog #39).
        State.add_clause_ast(state, env, {name, arity}, clause_ast)
      else
        state
      end

    # restore vars from outer scope
    state =
      %{state | caller: false}

    state =
      if has_unquotes do
        # remove scope
        State.remove_vars_scope(state, state_orig)
      else
        # restore module vars
        State.remove_func_vars_scope(state, state_orig)
      end

    # result of def expansion is fa tuple
    {{name, arity}, %{state | prematch: state_orig.prematch}, env}
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
    {args, state, env} = expand_macro_arg_cursor(args, state, env)

    try do
      callback.(meta, args)
    catch
      # If expanding the macro fails, we just give up.
      _kind, _payload ->
        # Logger.warning(Exception.format(kind, payload, __STACKTRACE__))
        uses = state.uses
        # look for cursor in args
        {_ast, state, _env} = expand(args, state, env)
        state = %{state | uses: uses}

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
    {args, state, env} = expand_macro_arg_cursor(args, state, env)

    ast = callback.(meta, args)

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

  defp expand_macro_arg_cursor(args, state, env) when is_list(args) do
    {args, state} =
      Enum.reduce(args, {[], state}, fn arg, {acc, state} ->
        {arg, state, _env} = expand_macro_arg_cursor(arg, state, env)
        {[arg | acc], state}
      end)

    {Enum.reverse(args), state, env}
  end

  defp expand_macro_arg_cursor({:__cursor__, _, list} = ast, state, env) when is_list(list) do
    expand(ast, state, env)
  end

  defp expand_macro_arg_cursor(arg, state, env), do: {arg, state, env}

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

  # The shape of a `defimpl`'s first argument: the type(s) being implemented for.
  # Returns `nil` unless we are expanding a *protocol callback* (a function whose
  # name/arity belongs to the protocol) inside an implementation module. Multiple
  # `for:` targets resolve to a union — their generated modules share source
  # lines, so we can't tell which dispatch a position belongs to.
  defp protocol_impl_arg_type(%{protocol: {protocol, for_list}} = state, env, name, arity)
       when is_list(for_list) and is_atom(protocol) do
    in_impl? = Enum.any?(for_list, fn for -> env.module == Module.concat(protocol, for) end)

    if in_impl? and protocol_callback?(state, protocol, name, arity) do
      for_list_union(for_list)
    end
  end

  defp protocol_impl_arg_type(_state, _env, _name, _arity), do: nil

  # A function is a protocol callback if its name/arity belongs to the protocol's
  # callback set. Reflection covers built-in and already-compiled protocols; the
  # collected `@callback` metadata covers a protocol defined in the same file
  # (`defprotocol` registers each function as a `:callback` spec — this is more
  # precise than the function index, which also holds generated `impl_for/1`
  # etc.).
  defp protocol_callback?(state, protocol, name, arity) do
    reflected_protocol_function?(protocol, name, arity) or
      metadata_callback?(state, protocol, name, arity)
  end

  defp reflected_protocol_function?(protocol, name, arity) do
    Code.ensure_loaded?(protocol) and
      function_exported?(protocol, :__protocol__, 1) and
      {name, arity} in protocol.__protocol__(:functions)
  rescue
    _ -> false
  end

  defp metadata_callback?(state, protocol, name, arity) do
    case state.specs[{protocol, name, arity}] do
      %{kind: kind} -> kind in [:callback, :macrocallback]
      _ -> false
    end
  end

  defp for_list_union(for_list) do
    case for_list |> Enum.map(&impl_for_shape/1) |> Enum.reject(&is_nil/1) |> Enum.uniq() do
      [] -> nil
      [single] -> single
      many -> {:union, many}
    end
  end

  # The built-in protocol targets map to their base types; any other module is a
  # struct (`for: URI` ⇒ `%URI{}`). `Any` and unresolved aliases carry no info.
  defp impl_for_shape(Atom), do: :atom
  defp impl_for_shape(Integer), do: {:integer, nil}
  defp impl_for_shape(Float), do: {:float, nil}
  defp impl_for_shape(List), do: {:list, nil}
  defp impl_for_shape(Map), do: {:map, [], nil}
  defp impl_for_shape(Tuple), do: :tuple
  defp impl_for_shape(BitString), do: :bitstring
  defp impl_for_shape(Function), do: :fun
  defp impl_for_shape(PID), do: :pid
  defp impl_for_shape(Port), do: :port
  defp impl_for_shape(Reference), do: :reference
  defp impl_for_shape(Any), do: nil
  defp impl_for_shape(:"Elixir.__Unknown__"), do: nil
  defp impl_for_shape(module) when is_atom(module), do: {:struct, [], {:atom, module}, nil}
  defp impl_for_shape(_), do: nil

  # defmodule helpers
  # defmodule automatically defines aliases, we need to mirror this feature here.

  # defmodule Elixir.Alias
  defp alias_defmodule({:__aliases__, _, [:"Elixir", _ | _]}, module, env), do: {module, env}

  # defmodule Alias in root
  defp alias_defmodule({:__aliases__, _, _}, module, %{module: nil} = env),
    do: {module, env}

  # defmodule Alias nested
  defp alias_defmodule({:__aliases__, meta, [h | t]}, _module, env) when is_atom(h) do
    module = Module.concat([env.module, h])
    alias = String.to_atom("Elixir." <> Atom.to_string(h))
    {:ok, env} = NormalizedMacroEnv.define_alias(env, meta, module, as: alias, trace: true)

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
              |> State.add_current_env_to_line(meta, e)

            {rewritten, s, ea}

          {:error, _error} ->
            # elixir raises here elixir_rewrite
            s =
              State.close_write(sa, s)
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

  defp expand_local(meta, :when, [_, _] = args, state, %{context: nil} = env) do
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
      |> State.add_call_to_line({env.module, fun, length(args)}, meta, :local_function)
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
    {base_ref, state, env} = expand_without_aliases_report(base, state, env)

    if is_atom(base_ref) do
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
    else
      # elixir raises invalid_alias here
      {{kind, meta, []}, state, env}
    end
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
            kind = ModFunInfo.get_def_kind(info)

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

        {{:&, meta, [{:/, [], [{{:., dot_meta, [remote, fun]}, attached_meta, []}, arity]}]}, se,
         ee}

      {{:local, fun, arity}, local_meta, _, se, ee} ->
        # elixir raises undefined_local_capture if ee.function is nil

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

    sf = State.remove_vars_scope(se, s)
    # Stamp a {:version, N} on `for` (1.20+ only — commit 603602e67).
    {meta, sf} = stamp_version(meta, sf)
    {{:for, meta, e_cases ++ [[{:do, e_expr} | normalized_opts]]}, sf, e}
  end

  defp expand_for_do_block([{:->, _, _} | _] = clauses, s, e, false) do
    # elixir raises here for_without_reduce_bad_block
    # try to recover from error by emitting fake reduce
    expand_for_do_block(clauses, s, e, {:reduce, []})
  end

  defp expand_for_do_block(expr, s, e, false), do: expand(expr, s, e)

  defp expand_for_do_block([{:->, _, _} | _] = clauses, s, e, {:reduce, reduce_acc}) do
    # The accumulator pattern matches the `reduce:` seed on the first iteration;
    # that initial value is the one type we can be certain of, so type the
    # accumulator var against it (`reduce: %{}` → `acc :: map()`).
    acc_type = TypeInference.type_of(reduce_acc, e.context, s, e)

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

        # Type the accumulator pattern against the seed value's type, *inside*
        # head expansion (so the type is captured before the clause scope is
        # recorded). Use the single accumulator pattern, not the one-element arg
        # list (`find_typed_vars` would read a list as a list pattern).
        head_fun = fn c, cs, ce ->
          {e_head, cs, ce} = __MODULE__.Clauses.head(c, cs, ce)

          cs =
            case e_head do
              # Skip guarded heads: a guard (`acc when is_integer(acc)`) is the
              # intentional, more precise constraint, and intersecting the seed
              # type with it only adds noise.
              [{:when, _, _} | _] ->
                cs

              [acc_pattern | _] ->
                acc_vars = TypeInference.find_typed_vars(acc_pattern, acc_type, :match)
                State.merge_inferred_types(cs, acc_vars)

              _ ->
                cs
            end

          {e_head, cs, ce}
        end

        {e_clause, s_acc, _e_acc} =
          __MODULE__.Clauses.clause(head_fun, clause, s_reset, e)

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

    match_context_r = TypeInference.type_of(e_right, e.context, sr, e)

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

        # Binary-generator element vars get their type from the segment specs
        # (`for <<b <- bin>>` → `b :: integer`, `for <<r::utf8 <- bin>>` → the
        # codepoint type). The scrutinee is irrelevant, so pass a nil context.
        vars_with_inferred_types = TypeInference.find_typed_vars(e_left, nil, :match)
        sl = State.merge_inferred_types(sl, vars_with_inferred_types)

        {{:<<>>, meta, [{:<-, op_meta, [e_left, e_right]}]}, sl, el}

      _ ->
        expand(x, s, e)
    end
  end

  defp expand_for_generator(x, s, e) do
    # A non-generator clause is a filter; if it is a guard-like test
    # (`for x <- xs, is_integer(x)`), narrow the tested vars for the rest of the
    # comprehension (generators/filters thread state into the body scope).
    {e_x, s, e} = expand(x, s, e)

    refinements =
      e_x
      |> Guard.type_information_from_guards()
      |> Enum.reject(fn {_key, type} -> is_nil(type) end)

    {e_x, State.merge_inferred_types(s, refinements), e}
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
    # Stamp a {:version, N} on `case` (1.20+ only — commit 603602e67).
    {meta, so} = stamp_version(meta, so)
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

  # Elixir 1.20+ shape for `if`/`unless`-generated case clauses, after the
  # `Kernel.in/2` rewrite. Upstream commit 19c628ae2 — Mark individual guards
  # of `||`, `&&`, `if`, and `unless` as generated. Same simplification, just
  # matched against the post-rewrite `orelse`/`=:=` form.
  def rewrite_case_clauses(
        do: [
          {:->, false_meta,
           [
             [
               {:when, _,
                [
                  {var_name, _, Kernel},
                  {{:., _, [:erlang, :orelse]}, _,
                   [
                     {{:., _, [:erlang, :"=:="]}, _, [{var_name, _, Kernel}, false]},
                     {{:., _, [:erlang, :"=:="]}, _, [{var_name, _, Kernel}, nil]}
                   ]}
                ]}
             ],
             false_expr
           ]},
          {:->, true_meta,
           [
             [{:_, _, _}],
             true_expr
           ]}
        ]
      )
      when is_atom(var_name) do
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

  @internals [{:behaviour_info, 1}, {:module_info, 1}, {:module_info, 0}]

  defp expand_without_aliases_report({:__aliases__, _, _} = alias, state, env) do
    expand_aliases(alias, state, env, false)
  end

  defp expand_without_aliases_report(other, state, env) do
    expand(other, state, env)
  end

  defp expand_aliases({:__aliases__, meta, [head | tail] = list}, state, env, report) do
    case NormalizedMacroEnv.expand_alias(env, meta, list, trace: true) do
      {:alias, alias} ->
        state =
          if report do
            state
            |> State.add_call_to_line({alias, nil, nil}, meta, :alias_reference)
          else
            state
          end

        {alias, state, env}

      :error ->
        {head, state, env} = expand(head, state, env)

        if is_atom(head) do
          alias = Module.concat([head | tail])

          state =
            if report do
              state
              |> State.add_call_to_line({alias, nil, nil}, meta, :alias_reference)
            else
              state
            end

          {alias, state, env}
        else
          # elixir raises here invalid_alias
          {{:__aliases__, meta, [head | tail]}, state, env}
        end
    end
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

  # Helper to merge ElixirTypes pattern variable refinements
  defp merge_elixir_types_pattern_vars(
         existing_vars,
         {:=, meta, [pattern_ast, _]} = match_ast,
         state
       ) do
    cond do
      existing_vars == [] ->
        {existing_vars, %{}}

      not ElixirTypes.enabled?() ->
        {existing_vars, %{}}

      true ->
        env = env_for_meta(state, meta)
        module = env && env.module
        function = env && env.function
        file = Keyword.get(meta, :file) || module_file(module)

        target_keys = Enum.map(existing_vars, &elem(&1, 0))

        # Seed every in-scope variable into the native context. We type each `=`
        # match in isolation, but the value expression may reference earlier body
        # vars (`mod = polymod(values)`); without them, native `refine_body_var`
        # raises when it can't find their version. Seed with the native
        # descriptor we already inferred and stored for each var (falling back to
        # the structural shape, then `:dynamic`) so types propagate across
        # statements instead of collapsing to `dynamic()`.
        variables = elixir_types_scope_variables(state)

        case ElixirTypes.of_match(
               pattern_ast,
               nil,
               match_ast,
               module,
               function,
               file,
               :dynamic,
               target_keys: target_keys,
               variables: variables
             ) do
          {:ok, refined, var_descrs} when map_size(refined) > 0 ->
            {merge_pattern_types(existing_vars, refined), var_descrs}

          _ ->
            {existing_vars, %{}}
        end
    end
  end

  defp merge_elixir_types_pattern_vars(existing_vars, _expr_ast, _state), do: {existing_vars, %{}}

  # All in-scope variables keyed `{name, version}`, mapped to the native
  # descriptor we already stored for each (`elixir_types_descr`), falling back to
  # the structural shape and finally `:dynamic`. Used to seed the native
  # `of_match` context so value-expression vars resolve with their real types
  # (not just `dynamic()`), and so `refine_body_var` never hits an unseeded
  # version. `coerce_var_type` accepts a `Descr`, a shape, or `:dynamic`.
  defp elixir_types_scope_variables(%{vars_info: [scope | _]}) when is_map(scope) do
    for {{name, version}, %ElixirSense.Core.State.VarInfo{} = var} <- scope,
        is_atom(name) and is_integer(version),
        into: %{} do
      {{name, version}, var.elixir_types_descr || var.type || :dynamic}
    end
  end

  defp elixir_types_scope_variables(_state), do: %{}

  defp merge_pattern_types(existing_vars, refined_map) do
    existing_map = Map.new(existing_vars)

    merged_map =
      Enum.reduce(refined_map, existing_map, fn {key, new_type}, acc ->
        case Map.fetch(acc, key) do
          {:ok, existing_type} ->
            Map.put(acc, key, TypeInference.intersect(existing_type, new_type))

          :error ->
            Map.put(acc, key, new_type)
        end
      end)

    ordered_keys = Enum.map(existing_vars, &elem(&1, 0))

    ordered = Enum.map(ordered_keys, fn key -> {key, Map.get(merged_map, key)} end)

    additional =
      merged_map
      |> Enum.reject(fn {key, _} -> key in ordered_keys end)

    ordered ++ additional
  end

  defp env_for_meta(state, meta) do
    line = Keyword.get(meta, :line, 0)

    if is_integer(line) and line > 0 do
      Map.get(state.lines_to_env, line) || closest_env(state)
    else
      closest_env(state)
    end
  end

  defp closest_env(%State{closest_env: {{_, _}, _dist, env}}) when not is_nil(env), do: env
  defp closest_env(_), do: nil

  defp module_file(nil), do: nil

  defp module_file(module) when is_atom(module) do
    case :code.which(module) do
      path when is_list(path) -> List.to_string(path)
      _ -> nil
    end
  end

  # Run ElixirTypes local inference once per function of `module`, after the
  # whole module body has been expanded and all clauses accumulated.
  #
  # Previously inference ran after every individual clause via
  # `maybe_infer_local_signature/3`, re-processing the whole accumulated clause
  # list each time — quadratic in the number of clauses (old backlog #39). Here
  # we walk the module's functions once, infer the signature from the complete
  # clause list, persist it (with `:inferred` provenance), and prune the stored
  # clause ASTs so we don't retain every function body for the lifetime of the
  # metadata.
  defp infer_module_local_signatures(state, %{module: module, file: file})
       when is_atom(module) do
    if ElixirTypes.enabled?() do
      state.mods_funs_to_positions
      |> Enum.filter(fn
        {{^module, fun, arity}, %{elixir_types_clauses: clauses}}
        when is_atom(fun) and is_integer(arity) and is_list(clauses) and clauses != [] ->
          true

        _ ->
          false
      end)
      |> Enum.reduce(state, fn {{^module, fun, arity} = key, info}, state ->
        # `elixir_types_clauses` is stored newest-first (add_clause_ast prepends).
        # We intentionally pass it as-is to preserve the exact clause ordering the
        # old per-clause inference observed on its final invocation, which feeds
        # build_domain / the clause-types union.
        clauses = info.elixir_types_clauses

        case ElixirTypes.infer_local_signature(module, {fun, arity}, clauses, file) do
          {:infer, _domain, _clause_types} = sig ->
            State.put_elixir_types_sig(state, key, sig, :ok, :inferred)

          :error ->
            State.put_elixir_types_sig(state, key, nil, :skipped, nil)
        end
      end)
    else
      state
    end
  end

  defp infer_module_local_signatures(state, _env), do: state
end
