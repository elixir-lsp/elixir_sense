defmodule ElixirSense.Core.MetadataBuilder do
  @moduledoc """
  This module is responsible for building/retrieving environment information from an AST.
  """

  import ElixirSense.Core.State
  import ElixirSense.Log
  import ElixirSense.Core.TypeInference

  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State
  alias ElixirSense.Core.State.VarInfo
  # alias ElixirSense.Core.TypeInfo
  alias ElixirSense.Core.Guard
  alias ElixirSense.Core.Compiler

  @scope_keywords [:for, :fn, :with]
  @block_keywords [:do, :else, :rescue, :catch, :after]
  @defs [:def, :defp, :defmacro, :defmacrop, :defdelegate, :defguard, :defguardp]

  defguardp is_call(call, params)
            when is_atom(call) and is_list(params) and
                   call not in [:., :__aliases__, :"::", :{}, :|>, :%, :%{}]

  @doc """
  Traverses the AST building/retrieving the environment information.
  It returns a `ElixirSense.Core.State` struct containing the information.
  """
  @spec build(Macro.t()) :: State.t()
  def build(ast) do
    if Version.match?(System.version(), ">= 1.17.0-dev") do
      {_ast, state, _env} = Compiler.expand(ast, %State{}, Compiler.env())
      state
      |> remove_attributes_scope
      |> remove_lexical_scope
      |> remove_vars_scope
      |> remove_module
      |> remove_protocol_implementation
    else
      # dbg(ast)
      {_ast, [state]} =
        Macro.traverse(ast, [%State{}], &safe_call_pre/2, &safe_call_post/2)

      try do
        state
        |> remove_attributes_scope
        |> remove_lexical_scope
        |> remove_vars_scope
        |> remove_module
        |> remove_protocol_implementation
      rescue
        exception ->
          warn(
            Exception.format(
              :error,
              "#{inspect(exception.__struct__)} during metadata build scope closing:\n" <>
                "#{Exception.message(exception)}\n" <>
                "ast node: #{inspect(ast, limit: :infinity)}",
              __STACKTRACE__
            )
          )

          vars_info_per_scope_id =
            try do
              update_vars_info_per_scope_id(state)
            rescue
              _ ->
                state.vars_info_per_scope_id
            end

          %{
            state
            | attributes: [],
              scope_attributes: [],
              aliases: [],
              imports: [],
              requires: [],
              scope_ids: [],
              vars: [],
              scope_vars: [],
              vars_info_per_scope_id: vars_info_per_scope_id,
              module: [],
              scopes: [],
              protocols: []
          }
      end
    end
  end

  defp safe_call_pre(ast, [state = %State{} | _] = states) do
    try do
      # if operation == :pre do
      #   dbg(ast)
      # end
      {ast_after_pre, state_after_pre} = pre(ast, state)
      {ast_after_pre, [state_after_pre | states]}
    rescue
      exception ->
        # reraise(exception, __STACKTRACE__)
        warn(
          Exception.format(
            :error,
            "#{inspect(exception.__struct__)} during metadata build pre:\n" <>
              "#{Exception.message(exception)}\n" <>
              "ast node: #{inspect(ast, limit: :infinity)}",
            __STACKTRACE__
          )
        )

        {nil, [:error | states]}
    end
  end

  defp safe_call_post(ast, [:error | states]) do
    {ast, states}
  end

  defp safe_call_post(ast_after_pre, [state_after_pre = %State{} | states]) do
    try do
      # if operation == :pre do
      #   dbg(ast_after_pre)
      # end
      {ast_after_post, state_after_post} = post(ast_after_pre, state_after_pre)
      {ast_after_post, [state_after_post | tl(states)]}
    rescue
      exception ->
        warn(
          Exception.format(
            :error,
            "#{inspect(exception.__struct__)} during metadata build post:\n" <>
              "#{Exception.message(exception)}\n" <>
              "ast node: #{inspect(ast_after_pre, limit: :infinity)}",
            __STACKTRACE__
          )
        )

        {nil, states}
    end
  end

  defp pre_func({type, meta, ast_args}, state, meta, name, params, options \\ [])
       when is_atom(name) do
    vars =
      state
      |> find_vars(params)

    _vars =
      if options[:guards],
        do: infer_type_from_guards(options[:guards], vars, state),
        else: vars

    {position, end_position} = extract_range(meta)

    options = Keyword.put(options, :generated, state.generated)

    ast = {type, Keyword.put(meta, :func, true), ast_args}

    env = nil

    state
    |> new_named_func(name, length(params || []))
    |> add_func_to_index(env, name, params || [], position, end_position, type, options)
    |> new_lexical_scope
    |> new_func_vars_scope
    # |> add_vars(vars, true)
    # |> add_current_env_to_line(Keyword.fetch!(meta, :line))
    |> result(ast)
  end

  defp extract_range(meta) do
    position = {
      Keyword.fetch!(meta, :line),
      Keyword.fetch!(meta, :column)
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

  defp post_func(ast, state) do
    # dbg(ast)
    state
    |> remove_lexical_scope
    |> remove_func_vars_scope
    |> remove_last_scope_from_scopes
    |> result(ast)
  end

  defp pre_scope_keyword(ast, state, _line) do
    state =
      case ast do
        {:for, _, _} ->
          state |> push_binding_context(:for)

        _ ->
          state
      end

    state
    # |> add_current_env_to_line(line)
    |> new_vars_scope
    |> result(ast)
  end

  defp post_scope_keyword(ast, state) do
    state =
      case ast do
        {:for, _, _} ->
          state |> pop_binding_context

        _ ->
          state
      end

    state
    |> maybe_move_vars_to_outer_scope
    |> remove_vars_scope
    |> result(ast)
  end

  defp pre_block_keyword(ast, state) do
    state =
      case ast do
        {:rescue, _} ->
          state |> push_binding_context(:rescue)

        _ ->
          state
      end

    state
    |> new_lexical_scope
    |> new_vars_scope
    |> result(ast)
  end

  defp post_block_keyword(ast, state) do
    state =
      case ast do
        {:rescue, _} ->
          state |> pop_binding_context

        _ ->
          state
      end

    state
    |> remove_lexical_scope
    |> maybe_move_vars_to_outer_scope
    |> remove_vars_scope
    |> result(ast)
  end

  defp pre_clause({_clause, _meta, _} = ast, state, lhs) do


    _vars =
      state
      |> find_vars(lhs, Enum.at(state.binding_context, 0))

    state
    |> new_lexical_scope
    |> new_vars_scope
    # |> add_vars(vars, true)
    # |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp post_clause(ast, state) do
    state
    |> remove_lexical_scope
    |> maybe_move_vars_to_outer_scope
    |> remove_vars_scope
    |> result(ast)
  end

  defp post_string_literal(ast, _state, _line, str) do
    str
    |> Source.split_lines()
    |> Enum.with_index()
    # |> Enum.reduce(state, fn {_s, i}, acc -> add_current_env_to_line(acc, line + i) end)
    |> result(ast)
  end

  # ex_unit describe
  defp pre(
         {:describe, meta, [name, _body]} = ast,
         state = %{scopes: [atom | _]}
       )
       when is_binary(name) and is_atom(atom) and atom != nil do
    line = Keyword.fetch!(meta, :line)
    column = Keyword.fetch!(meta, :column)

    state =
      state
      |> add_call_to_line({nil, :describe, 2}, {line, column})

    %{state | context: Map.put(state.context, :ex_unit_describe, name)}
    # |> add_current_env_to_line(line)
    |> result(ast)
  end

  # ex_unit not implemented test
  defp pre(
         {:test, meta, [name]},
         state = %{scopes: [atom | _]}
       )
       when is_binary(name) and is_atom(atom) and atom != nil do
    def_name = ex_unit_test_name(state, name)
    line = Keyword.fetch!(meta, :line)
    column = Keyword.fetch!(meta, :column)

    ast_without_params = {:def, meta, [{def_name, add_no_call([]), []}, [], []]}

    state =
      state
      |> add_call_to_line({nil, :test, 0}, {line, column})

    pre_func(ast_without_params, state, meta, def_name, [{:_, [line: line, column: column], nil}])
  end

  # ex_unit test without context
  defp pre(
         {:test, meta, [name, body]},
         state = %{scopes: [atom | _]}
       )
       when is_binary(name) and is_atom(atom) and atom != nil do
    def_name = ex_unit_test_name(state, name)
    line = Keyword.fetch!(meta, :line)
    column = Keyword.fetch!(meta, :column)

    ast_without_params = {:def, meta, [{def_name, add_no_call([]), []}, [], body]}

    state =
      state
      |> add_call_to_line({nil, :test, 2}, {line, column})

    pre_func(ast_without_params, state, meta, def_name, [{:_, [line: line, column: column], nil}])
  end

  # ex_unit test with context
  defp pre(
         {:test, meta, [name, param, body]},
         state = %{scopes: [atom | _]}
       )
       when is_binary(name) and is_atom(atom) and atom != nil do
    def_name = ex_unit_test_name(state, name)
    line = Keyword.fetch!(meta, :line)
    column = Keyword.fetch!(meta, :column)

    ast_without_params = {:def, meta, [{def_name, add_no_call([]), []}, [], body]}

    state =
      state
      |> add_call_to_line({nil, :test, 3}, {line, column})

    pre_func(ast_without_params, state, meta, def_name, [param])
  end

  # ex_unit setup with context
  defp pre(
         {setup, meta, [param, body]},
         state = %{scopes: [atom | _]}
       )
       when setup in [:setup, :setup_all] and is_atom(atom) and atom != nil do
    line = Keyword.fetch!(meta, :line)
    column = Keyword.fetch!(meta, :column)

    # NOTE this name is not 100% correct - ex_unit uses counters instead of line but it's too complicated
    def_name = :"__ex_unit_#{setup}_#{line}"
    ast_without_params = {:def, meta, [{def_name, add_no_call([]), []}, [], body]}

    state =
      state
      |> add_call_to_line({nil, setup, 2}, {line, column})

    pre_func(ast_without_params, state, meta, def_name, [param])
  end

  # ex_unit setup without context
  defp pre(
         {setup, meta, [body]},
         state = %{scopes: [atom | _]}
       )
       when setup in [:setup, :setup_all] and is_atom(atom) and atom != nil do
    line = Keyword.fetch!(meta, :line)
    column = Keyword.fetch!(meta, :column)

    # NOTE this name is not 100% correct - ex_unit uses counters instead of line but it's too complicated
    def_name = :"__ex_unit_#{setup}_#{line}"
    ast_without_params = {:def, meta, [{def_name, add_no_call([]), []}, [], body]}

    state =
      state
      |> add_call_to_line({nil, setup, 2}, {line, column})

    pre_func(ast_without_params, state, meta, def_name, [{:_, [line: line, column: column], nil}])
  end

  # function head with guards
  defp pre(
         {def_name, meta, [{:when, _, [{name, meta2, params}, guards]}, body]},
         state
       )
       when def_name in @defs and is_atom(name) do
    ast_without_params = {def_name, meta, [{name, add_no_call(meta2), []}, guards, body]}
    pre_func(ast_without_params, state, meta, name, params, guards: guards)
  end

  defp pre(
         {def_name, meta, [{name, meta2, params}, body]},
         state
       )
       when def_name in @defs and is_atom(name) do
    ast_without_params = {def_name, meta, [{name, add_no_call(meta2), []}, body]}
    pre_func(ast_without_params, state, meta, name, params)
  end

  # defguard and defguardp
  defp pre(
         {def_name, meta,
          [
            {:when, _meta, [{name, meta2, params}, body]}
          ]},
         state
       )
       when def_name in @defs and is_atom(name) do
    ast_without_params = {def_name, meta, [{name, add_no_call(meta2), []}, body]}
    pre_func(ast_without_params, state, meta, name, params)
  end

  # function head
  defp pre({def_name, meta, [{name, meta2, params}]}, state)
       when def_name in @defs and is_atom(name) do
    ast_without_params = {def_name, meta, [{name, add_no_call(meta2), []}, nil]}
    pre_func(ast_without_params, state, meta, name, params)
  end

  # incomplete spec
  # @callback my(integer)
  defp pre(
         {:@, _meta_attr, [{kind, _meta_kind, [{name, _meta_name, type_args}]} = _spec]},
         _state
       )
       when kind in [:spec, :callback, :macrocallback] and is_atom(name) and
              (is_nil(type_args) or is_list(type_args)) do
    # pre_spec(
    #   {:@, meta_attr, [{kind, add_no_call(meta_kind), [{name, meta_name, type_args}]}]},
    #   state,
    #   meta_attr,
    #   name,
    #   expand_aliases_in_ast(state, List.wrap(type_args)),
    #   expand_aliases_in_ast(state, spec),
    #   kind
    # )
  end

  defp pre({atom, meta, [_ | _]} = ast, state)
       when atom in @scope_keywords do
    line = Keyword.fetch!(meta, :line)
    pre_scope_keyword(ast, state, line)
  end

  defp pre({atom, _block} = ast, state) when atom in @block_keywords do
    pre_block_keyword(ast, state)
  end

  defp pre({:->, meta, [[{:when, _, [_var, guards]} = lhs], rhs]}, state) do
    pre_clause({:->, meta, [guards, rhs]}, state, lhs)
  end

  defp pre({:->, meta, [[lhs], rhs]}, state) do
    pre_clause({:->, meta, [:_, rhs]}, state, lhs)
  end

  defp pre({:->, meta, [lhs, rhs]}, state) do
    pre_clause({:->, meta, [:_, rhs]}, state, lhs)
  end

  defp pre({atom, meta, [lhs, rhs]}, state)
       when atom in [:=, :<-] do
    result(state, {atom, meta, [lhs, rhs]})
  end

  defp pre({:when, meta, [lhs, rhs]}, state) do
    _vars =
      state
      |> find_vars(lhs)

    state
    # |> add_vars(vars, true)
    |> result({:when, meta, [:_, rhs]})
  end

  defp pre(
         {:case, meta,
          [
            condition_ast,
            [
              do: _clauses
            ]
          ]} = ast,
         state
       ) do
    line = Keyword.fetch!(meta, :line)
    column = Keyword.fetch!(meta, :column)

    state
    |> push_binding_context(get_binding_type(state, condition_ast))
    |> add_call_to_line({nil, :case, 2}, {line, column})
    # |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp pre(
         {{:., meta1, [{:__aliases__, _, [_ | _]} = module_expression, call]}, _meta, params} =
           ast,
         state
       )
       when is_call(call, params) do
    line = Keyword.fetch!(meta1, :line)
    column = Keyword.fetch!(meta1, :column)

    try do
      # TODO pass env
      {module, state, _env} = expand(module_expression, state)

      shift = if state.generated, do: 0, else: 1

      state
      |> add_call_to_line({module, call, length(params)}, {line, column + shift})
      # |> add_current_env_to_line(line)
      |> result(ast)
    rescue
      _ ->
        # Module.concat can fail for invalid aliases
        result(state, nil)
    end
  end

  defp pre(
         {{:., meta1, [{:__MODULE__, _, nil}, call]}, _meta, params} = ast,
         state
       )
       when is_call(call, params) do
    line = Keyword.fetch!(meta1, :line)
    column = Keyword.fetch!(meta1, :column)
    module = get_current_module(state)

    shift = if state.generated, do: 0, else: 1

    state
    |> add_call_to_line({module, call, length(params)}, {line, column + shift})
    # |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp pre(
         {{:., meta1, [{:@, _, [{attribute, _, nil}]}, call]}, _meta, params} = ast,
         state
       )
       when is_call(call, params) and is_atom(attribute) do
    line = Keyword.fetch!(meta1, :line)
    column = Keyword.fetch!(meta1, :column)

    shift = if state.generated, do: 0, else: 1

    state
    |> add_call_to_line({{:attribute, attribute}, call, length(params)}, {line, column + shift})
    # |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp pre(
         {{:., meta1, [{:@, _, [{attribute, _, nil}]}]}, _meta, params} = ast,
         state
       )
       when is_atom(attribute) do
    line = Keyword.fetch!(meta1, :line)
    column = Keyword.fetch!(meta1, :column)

    shift = if state.generated, do: 0, else: 1

    state
    |> add_call_to_line({nil, {:attribute, attribute}, length(params)}, {line, column + shift})
    # |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp pre(
         {{:., meta1, [{variable, _var_meta, nil}]}, _meta, params} = ast,
         state
       )
       when is_atom(variable) do
    line = Keyword.fetch!(meta1, :line)
    column = Keyword.fetch!(meta1, :column)

    shift = if state.generated, do: 0, else: 1

    state
    |> add_call_to_line({nil, {:variable, variable}, length(params)}, {line, column + shift})
    # |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp pre(
         {{:., meta1, [{variable, _var_meta, nil}, call]}, _meta, params} = ast,
         state
       )
       when is_call(call, params) and is_atom(variable) do
    line = Keyword.fetch!(meta1, :line)
    column = Keyword.fetch!(meta1, :column)

    shift = if state.generated, do: 0, else: 1

    state
    |> add_call_to_line({{:variable, variable}, call, length(params)}, {line, column + shift})
    # |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp pre(
         {{:., meta1, [module, call]}, _meta, params} = ast,
         state
       )
       when is_atom(module) and is_call(call, params) do
    line = Keyword.fetch!(meta1, :line)
    column = Keyword.fetch!(meta1, :column)

    shift = if state.generated, do: 0, else: 1

    state
    |> add_call_to_line({module, call, length(params)}, {line, column + shift})
    # |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp pre({call, meta, params} = ast, state)
       when is_call(call, params) do
    case Keyword.get(meta, :line) do
      nil ->
        {ast, state}

      _ ->
        line = Keyword.fetch!(meta, :line)

        # credo:disable-for-next-line
        if not Keyword.get(meta, :no_call, false) do
          column = Keyword.fetch!(meta, :column)

          state =
            if String.starts_with?(to_string(call), "__atom_elixir_marker_") do
              state
            else
              add_call_to_line(state, {nil, call, length(params)}, {line, column})
            end

          state
          # |> add_current_env_to_line(line)
          |> result(ast)
        else
          state
          # |> add_current_env_to_line(line)
          |> result(ast)
        end
    end
  end

  # Any other tuple with a line
  defp pre({_, meta, _} = ast, state) do
    case Keyword.get(meta, :line) do
      nil ->
        {ast, state}

      _line ->
        state
        # |> add_current_env_to_line(line)
        |> result(ast)
    end
  end

  defp pre(ast, state) do
    {ast, state}
  end

  # ex_unit describe
  defp post(
         {:describe, _meta, [name, _body]} = ast,
         state
       )
       when is_binary(name) do
    %{state | context: Map.delete(state.context, :ex_unit_describe)}
    |> result(ast)
  end

  defp post({def_name, meta, [{name, _, _params} | _]} = ast, state)
       when def_name in @defs and is_atom(name) do
    if Keyword.get(meta, :func, false) do
      post_func(ast, state)
    else
      {ast, state}
    end
  end

  defp post({def_name, _, _} = ast, state) when def_name in @defs do
    {ast, state}
  end

  defp post(
         {:@, _meta_attr,
          [{kind, _, [{:"::", _meta, _params = [{name, _, type_args}, _type_def]} = _spec]}]} =
           ast,
         state
       )
       when kind in [:type, :typep, :opaque] and is_atom(name) and
              (is_nil(type_args) or is_list(type_args)) do
    state =
      state
      |> remove_last_scope_from_scopes

    {ast, state}
  end

  defp post(
         {:@, _meta_attr,
          [
            {kind, _,
             [
               {:when, _, [{:"::", _meta, _params = [{name, _, type_args}, _type_def]}, _]} =
                 _spec
             ]}
          ]} = ast,
         state
       )
       when kind in [:spec, :callback, :macrocallback] and is_atom(name) and
              (is_nil(type_args) or is_list(type_args)) do
    state =
      state
      |> remove_last_scope_from_scopes

    {ast, state}
  end

  defp post(
         {:@, _meta_attr,
          [{kind, _, [{:"::", _meta, _params = [{name, _, type_args}, _type_def]} = _spec]}]} =
           ast,
         state
       )
       when kind in [:spec, :callback, :macrocallback] and is_atom(name) and
              (is_nil(type_args) or is_list(type_args)) do
    state =
      state
      |> remove_last_scope_from_scopes

    {ast, state}
  end

  defp post(
         {:case, _meta,
          [
            _condition_ast,
            [
              do: _clauses
            ]
          ]} = ast,
         state
       ) do
    state
    |> pop_binding_context
    |> result(ast)
  end

  defp post({atom, _, [_ | _]} = ast, state) when atom in @scope_keywords do
    post_scope_keyword(ast, state)
  end

  defp post({atom, _block} = ast, state) when atom in @block_keywords do
    post_block_keyword(ast, state)
  end

  defp post({:->, _meta, [_lhs, _rhs]} = ast, state) do
    post_clause(ast, state)
  end

  defp post({:__generated__, _meta, inner}, state) do
    {inner, %{state | generated: false}}
  end

  defp post({atom, meta, [lhs, rhs]} = ast, state)
       when atom in [:=, :<-] do
    _line = Keyword.fetch!(meta, :line)
    match_context_r = get_binding_type(state, rhs)

    match_context_r =
      if atom == :<- and match?([:for | _], state.binding_context) do
        {:for_expression, match_context_r}
      else
        match_context_r
      end

    vars_l = find_vars(state, lhs, match_context_r)

    _vars =
      case rhs do
        {:=, _, [nested_lhs, _nested_rhs]} ->
          match_context_l = get_binding_type(state, lhs)
          nested_vars = find_vars(state, nested_lhs, match_context_l)

          vars_l ++ nested_vars

        _ ->
          vars_l
      end

    state
    # |> remove_calls(remove_positions)
    # |> add_current_env_to_line(line)
    |> result(ast)
  end

  # String literal
  defp post({_, [no_call: true, line: line, column: _column], [str]} = ast, state)
       when is_binary(str) do
    post_string_literal(ast, state, line, str)
  end

  # String literal in sigils
  defp post({:<<>>, [indentation: _, line: line, column: _column], [str]} = ast, state)
       when is_binary(str) do
    post_string_literal(ast, state, line, str)
  end

  defp post(ast, state) do
    {ast, state}
  end

  defp result(state, ast) do
    {ast, state}
  end

  defp find_vars(state, ast, match_context \\ nil)

  defp find_vars(_state, {var, _meta, nil}, _)
       when var in [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__] do
    # TODO local calls?
    []
  end

  defp find_vars(_state, {var, meta, nil}, :rescue) when is_atom(var) do
    line = Keyword.fetch!(meta, :line)
    column = Keyword.fetch!(meta, :column)
    match_context = {:struct, [], {:atom, Exception}, nil}
    [%VarInfo{name: var, positions: [{line, column}], type: match_context, is_definition: true}]
  end

  defp find_vars(state, ast, match_context) do
    {_ast, {vars, _match_context}} =
      Macro.prewalk(ast, {[], match_context}, &match_var(state, &1, &2))

    vars
  end

  defp match_var(
         state,
         {:in, _meta,
          [
            left,
            right
          ]},
         {vars, _match_context}
       ) do
    exception_type =
      case right do
        [elem] ->
          get_binding_type(state, elem)

        list when is_list(list) ->
          types = for elem <- list, do: get_binding_type(state, elem)
          if Enum.all?(types, &match?({:atom, _}, &1)), do: {:atom, Exception}

        elem ->
          get_binding_type(state, elem)
      end

    match_context =
      case exception_type do
        {:atom, atom} -> {:struct, [], {:atom, atom}, nil}
        _ -> nil
      end

    match_var(state, left, {vars, match_context})
  end

  defp match_var(
         state,
         {:=, _meta,
          [
            left,
            right
          ]},
         {vars, _match_context}
       ) do
    {_ast, {vars, _match_context}} =
      match_var(state, left, {vars, get_binding_type(state, right)})

    {_ast, {vars, _match_context}} =
      match_var(state, right, {vars, get_binding_type(state, left)})

    {[], {vars, nil}}
  end

  defp match_var(
         _state,
         {:^, _meta, [{var, meta, nil}]},
         {vars, match_context} = ast
       )
       when is_atom(var) do
    line = Keyword.fetch!(meta, :line)
    column = Keyword.fetch!(meta, :column)
    var_info = %VarInfo{name: var, positions: [{line, column}], type: match_context}
    {ast, {[var_info | vars], nil}}
  end

  defp match_var(
         _state,
         {var, meta, nil} = ast,
         {vars, match_context}
       )
       when is_atom(var) and
              var not in [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__] do
    # TODO local calls?
    # TODO {:__MODULE__, meta, nil} is not expanded here
    line = Keyword.fetch!(meta, :line)
    column = Keyword.fetch!(meta, :column)

    var_info = %VarInfo{
      name: var,
      positions: [{line, column}],
      type: match_context,
      is_definition: true
    }

    {ast, {[var_info | vars], nil}}
  end

  # drop right side of guard expression as guards cannot define vars
  defp match_var(state, {:when, _, [left, _right]}, {vars, _match_context}) do
    match_var(state, left, {vars, nil})
  end

  defp match_var(state, {:%, _, [type_ast, {:%{}, _, ast}]}, {vars, match_context})
       when not is_nil(match_context) do
    {_ast, {type_vars, _match_context}} = match_var(state, type_ast, {[], nil})

    destructured_vars =
      ast
      |> Enum.flat_map(fn {key, value_ast} ->
        key_type = get_binding_type(state, key)

        {_ast, {new_vars, _match_context}} =
          match_var(state, value_ast, {[], {:map_key, match_context, key_type}})

        new_vars
      end)

    {ast, {vars ++ destructured_vars ++ type_vars, nil}}
  end

  defp match_var(state, {:%{}, _, ast}, {vars, match_context}) when not is_nil(match_context) do
    destructured_vars =
      ast
      |> Enum.flat_map(fn {key, value_ast} ->
        key_type = get_binding_type(state, key)

        {_ast, {new_vars, _match_context}} =
          match_var(state, value_ast, {[], {:map_key, match_context, key_type}})

        new_vars
      end)

    {ast, {vars ++ destructured_vars, nil}}
  end

  # regular tuples use {:{}, [], [field_1, field_2]} ast
  # two element use `{field_1, field_2}` ast (probably as an optimization)
  # detect and convert to regular
  defp match_var(state, ast, {vars, match_context})
       when is_tuple(ast) and tuple_size(ast) == 2 do
    match_var(state, {:{}, [], ast |> Tuple.to_list()}, {vars, match_context})
  end

  defp match_var(state, {:{}, _, ast}, {vars, match_context}) when not is_nil(match_context) do
    indexed = ast |> Enum.with_index()
    total = length(ast)

    destructured_vars =
      indexed
      |> Enum.flat_map(fn {nth_elem_ast, n} ->
        bond =
          {:tuple, total,
           indexed |> Enum.map(&if(n != elem(&1, 1), do: get_binding_type(state, elem(&1, 0))))}

        match_context =
          if match_context != bond do
            {:intersection, [match_context, bond]}
          else
            match_context
          end

        {_ast, {new_vars, _match_context}} =
          match_var(state, nth_elem_ast, {[], {:tuple_nth, match_context, n}})

        new_vars
      end)

    {ast, {vars ++ destructured_vars, nil}}
  end

  # two element tuples on the left of `->` are encoded as list `[field1, field2]`
  # detect and convert to regular
  defp match_var(state, {:->, meta, [[left], right]}, {vars, match_context}) do
    match_var(state, {:->, meta, [left, right]}, {vars, match_context})
  end

  defp match_var(state, list, {vars, match_context})
       when not is_nil(match_context) and is_list(list) do
    match_var_list = fn head, tail ->
      {_ast, {new_vars_head, _match_context}} =
        match_var(state, head, {[], {:list_head, match_context}})

      {_ast, {new_vars_tail, _match_context}} =
        match_var(state, tail, {[], {:list_tail, match_context}})

      {list, {vars ++ new_vars_head ++ new_vars_tail, nil}}
    end

    case list do
      [] ->
        {list, {vars, nil}}

      [{:|, _, [head, tail]}] ->
        match_var_list.(head, tail)

      [head | tail] ->
        match_var_list.(head, tail)
    end
  end

  defp match_var(_state, ast, {vars, match_context}) do
    {ast, {vars, match_context}}
  end

  def infer_type_from_guards(guard_ast, vars, state) do
    type_info = Guard.type_information_from_guards(guard_ast, state)

    Enum.reduce(type_info, vars, fn {var, type}, acc ->
      index = Enum.find_index(acc, &(&1.name == var))

      if index,
        do: List.update_at(acc, index, &Map.put(&1, :type, type)),
        else: acc
    end)
  end

  defp add_no_call(meta) do
    [{:no_call, true} | meta]
  end

  defp ex_unit_test_name(state, name) do
    case state.context[:ex_unit_describe] do
      nil -> "test #{name}"
      describe -> "test #{describe} #{name}"
    end
    |> String.to_atom()
  end
end
