defmodule ElixirSense.Core.MetadataBuilder do
  @moduledoc """
  This module is responsible for building/retrieving environment information from an AST.
  """

  import ElixirSense.Core.State
  import ElixirSense.Log

  alias ElixirSense.Core.BuiltinFunctions
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State
  alias ElixirSense.Core.State.VarInfo
  alias ElixirSense.Core.TypeInfo
  alias ElixirSense.Core.Guard
  alias ElixirSense.Core.Compiler

  @scope_keywords [:for, :fn, :with]
  @block_keywords [:do, :else, :rescue, :catch, :after]
  @defs [:def, :defp, :defmacro, :defmacrop, :defdelegate, :defguard, :defguardp]
  @protocol_types [{:t, [], :type, "@type t :: term"}]
  @protocol_functions [
    {:__protocol__, [:atom], :def},
    {:impl_for, [:data], :def},
    {:impl_for!, [:data], :def},
    {:behaviour_info, [:atom], :def}
  ]

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

  defp pre_module(ast, state, meta, alias, types \\ [], functions \\ [], options \\ []) do
    {position, end_position} = extract_range(meta)

    {full, module, state} =
      case Keyword.get(options, :for) do
        nil ->
          {module, state, env} = expand(alias, state)
          {full, state, _env} = alias_defmodule(alias, module, state, env)

          {full, module, state}

        implementations ->
          {implementation_alias(alias, implementations), {alias, implementations}, state}
      end

    state =
      state
      |> maybe_add_protocol_implementation(module)
      |> add_module(full)
      |> add_current_module_to_index(position, end_position, generated: state.generated)
      |> new_lexical_scope
      |> new_attributes_scope
      |> new_vars_scope

    env = get_current_env(state)
    {state, env} = maybe_add_protocol_behaviour(module, state, env)

    state =
      types
      |> Enum.reduce(state, fn {type_name, type_args, spec, kind}, acc ->
        acc
        |> add_type(env, type_name, type_args, kind, spec, position, end_position,
          generated: true
        )
      end)

    state = add_module_functions(state, env, functions, position, end_position)

    state
    |> result(ast)
  end

  defp post_module(ast, state) do
    env = get_current_env(state)

    state
    |> apply_optional_callbacks(env)
    |> remove_attributes_scope
    |> remove_lexical_scope
    |> maybe_move_vars_to_outer_scope
    |> remove_vars_scope
    |> remove_module
    |> remove_protocol_implementation
    |> result(ast)
  end

  def pre_protocol(ast, state, meta, module) do
    # protocol defines a type `@type t :: term`
    # and functions __protocol__/1, impl_for/1, impl_for!/1

    pre_module(ast, state, meta, module, @protocol_types, @protocol_functions)
  end

  def post_protocol(ast, state) do
    # turn specs into callbacks or create dummy callbacks
    builtins = BuiltinFunctions.all() |> Keyword.keys()

    current_module = get_current_module(state)

    keys =
      state.mods_funs_to_positions
      |> Map.keys()
      |> Enum.filter(fn
        {^current_module, name, _arity} when not is_nil(name) ->
          name not in builtins

        _ ->
          false
      end)

    new_specs =
      for key = {_mod, name, _arity} <- keys,
          into: %{},
          do:
            (
              new_spec =
                case state.specs[key] do
                  nil ->
                    %State.ModFunInfo{positions: positions, params: params} =
                      state.mods_funs_to_positions[key]

                    args =
                      for param_variant <- params do
                        param_variant
                        |> Enum.map(&Macro.to_string/1)
                      end

                    specs =
                      for arg <- args do
                        joined = Enum.join(arg, ", ")
                        "@callback #{name}(#{joined}) :: term"
                      end

                    %State.SpecInfo{
                      name: name,
                      args: args,
                      specs: specs,
                      kind: :callback,
                      positions: positions,
                      end_positions: Enum.map(positions, fn _ -> nil end),
                      generated: Enum.map(positions, fn _ -> true end)
                    }

                  spec = %State.SpecInfo{specs: specs} ->
                    %State.SpecInfo{
                      spec
                      | # TODO :spec will get replaced here, refactor into array
                        kind: :callback,
                        specs:
                          specs
                          |> Enum.map(fn s ->
                            String.replace_prefix(s, "@spec", "@callback")
                          end)
                          |> Kernel.++(specs)
                    }
                end

              {key, new_spec}
            )

    specs = Map.merge(state.specs, new_specs)

    state = %{state | specs: specs}
    post_module(ast, state)
  end

  defp pre_func({type, meta, ast_args}, state, meta, name, params, options \\ [])
       when is_atom(name) do
    vars =
      state
      |> find_vars(params)
      |> merge_same_name_vars()

    vars =
      if options[:guards],
        do: infer_type_from_guards(options[:guards], vars, state),
        else: vars

    {position, end_position} = extract_range(meta)

    options = Keyword.put(options, :generated, state.generated)

    ast = {type, Keyword.put(meta, :func, true), ast_args}

    env = get_current_env(state)

    state
    |> new_named_func(name, length(params || []))
    |> add_func_to_index(env, name, params || [], position, end_position, type, options)
    |> new_lexical_scope
    |> new_func_vars_scope
    |> add_vars(vars, true)
    |> add_current_env_to_line(Keyword.fetch!(meta, :line))
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

  defp pre_scope_keyword(ast, state, line) do
    state =
      case ast do
        {:for, _, _} ->
          state |> push_binding_context(:for)

        _ ->
          state
      end

    state
    |> add_current_env_to_line(line)
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

  defp pre_clause({_clause, meta, _} = ast, state, lhs) do
    line = meta |> Keyword.fetch!(:line)

    vars =
      state
      |> find_vars(lhs, Enum.at(state.binding_context, 0))
      |> merge_same_name_vars()

    state
    |> new_lexical_scope
    |> new_vars_scope
    |> add_vars(vars, true)
    |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp post_clause(ast, state) do
    state
    |> remove_lexical_scope
    |> maybe_move_vars_to_outer_scope
    |> remove_vars_scope
    |> result(ast)
  end

  defp pre_module_attribute(ast, state, {line, _} = position, name, type, is_definition) do
    state
    |> add_attribute(name, type, is_definition, position)
    |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp pre_type(ast, state, meta, type_name, type_args, spec, kind) do
    spec = TypeInfo.typespec_to_string(kind, spec)

    {position = {line, _column}, end_position} = extract_range(meta)
    env = get_current_env(state)

    state
    |> add_type(env, type_name, type_args, spec, kind, position, end_position,
      generated: state.generated
    )
    |> add_typespec_namespace(type_name, length(type_args))
    |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp pre_spec(ast, state, meta, type_name, type_args, spec, kind) do
    spec = TypeInfo.typespec_to_string(kind, spec)

    {position = {line, _column}, end_position} = extract_range(meta)
    env = get_current_env(state)

    state =
      if kind in [:callback, :macrocallback] do
        state
        |> add_func_to_index(
          env,
          :behaviour_info,
          [{:atom, meta, nil}],
          position,
          end_position,
          :def,
          generated: true
        )
      else
        state
      end

    state
    |> add_spec(env, type_name, type_args, spec, kind, position, end_position,
      generated: state.generated
    )
    |> add_typespec_namespace(type_name, length(type_args))
    |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp post_string_literal(ast, state, line, str) do
    str
    |> Source.split_lines()
    |> Enum.with_index()
    |> Enum.reduce(state, fn {_s, i}, acc -> add_current_env_to_line(acc, line + i) end)
    |> result(ast)
  end

  defp pre(
         {:defmodule, meta, [module, _]} = ast,
         state
       ) do
    pre_module(ast, state, meta, module)
  end

  defp pre(
         {:defprotocol, meta, [protocol, _]} = ast,
         state
       ) do
    pre_protocol(ast, state, meta, protocol)
  end

  defp pre(
         {:defimpl, meta, [protocol, impl_args | _]} = ast,
         state
       ) do
    pre_protocol_implementation(ast, state, meta, protocol, impl_args)
  end

  defp pre(
         {:defdelegate, meta, [{name, meta2, params}, body]},
         state
       )
       when is_atom(name) and is_list(body) do
    ast_without_params = {:defdelegate, meta, [{name, add_no_call(meta2), []}, body]}
    target_module = body |> Keyword.get(:to)

    target_function =
      case body |> Keyword.get(:as) do
        nil -> {:ok, name}
        as when is_atom(as) -> {:ok, as}
        _ -> :error
      end

    options =
      with mod = target_module,
           {:ok, target_function} <- target_function do
        [target: {mod, target_function}]
      else
        _ -> []
      end

    pre_func(ast_without_params, state, meta, name, params, options)
  end

  # quote do
  # quote options do
  defp pre({:quote, _meta, list}, state) when is_list(list) do
    # replace with an empty AST node
    {[], state}
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
    |> add_current_env_to_line(line)
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

  defp pre(
         {:@, meta_attr, [{:moduledoc, meta, [doc_arg]}]},
         state
       ) do
    line = Keyword.fetch!(meta_attr, :line)
    column = Keyword.fetch!(meta_attr, :column)
    new_ast = {:@, meta_attr, [{:moduledoc, add_no_call(meta), [doc_arg]}]}
    env = get_current_env(state)

    state
    |> add_moduledoc_positions(
      [line: line, column: column],
      [{:moduledoc, meta, [doc_arg]}],
      line
    )
    |> register_doc(env, :moduledoc, doc_arg)
    |> result(new_ast)
  end

  defp pre(
         {:@, meta_attr, [{doc, meta, [doc_arg]}]},
         state
       )
       when doc in [:doc, :typedoc] do
    new_ast = {:@, meta_attr, [{doc, add_no_call(meta), [doc_arg]}]}
    env = get_current_env(state)

    state
    |> register_doc(env, doc, doc_arg)
    |> result(new_ast)
  end

  defp pre(
         {:@, meta_attr, [{:impl, meta, [impl_arg]}]},
         state
       ) do
    new_ast = {:@, meta_attr, [{:impl, add_no_call(meta), [impl_arg]}]}
    env = get_current_env(state)
    # impl adds sets :hidden by default
    state
    |> register_doc(env, :doc, :impl)
    |> result(new_ast)
  end

  defp pre(
         {:@, meta_attr, [{:optional_callbacks, meta, [args]}]},
         state
       ) do
    new_ast = {:@, meta_attr, [{:optional_callbacks, add_no_call(meta), [args]}]}

    state
    |> register_optional_callbacks(args)
    |> result(new_ast)
  end

  defp pre(
         {:@, meta_attr, [{:deprecated, meta, [deprecated_arg]}]},
         state
       ) do
    new_ast = {:@, meta_attr, [{:deprecated, add_no_call(meta), [deprecated_arg]}]}
    env = get_current_env(state)
    # treat @deprecated message as @doc deprecated: message
    state
    |> register_doc(env, :doc, deprecated: deprecated_arg)
    |> result(new_ast)
  end

  defp pre({:@, _meta, [{:behaviour, _, [_arg]}]} = ast, state) do
    {ast, state, _env} = expand(ast, state)
    {ast, state}
  end

  # protocol derive
  defp pre(
         {:@, meta, [{:derive, _, [derived_protos]}]} = ast,
         state
       ) do
    line = Keyword.fetch!(meta, :line)
    column = Keyword.fetch!(meta, :column)
    current_module = state |> get_current_module

    List.wrap(derived_protos)
    |> Enum.map(fn
      {proto, _opts} -> proto
      proto -> proto
    end)
    |> Enum.reduce(state, fn proto, acc ->
      case expand(proto, acc) do
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
    |> result(ast)
  end

  defp pre(
         {:@, meta_attr,
          [
            {kind, kind_meta,
             [{:"::", _meta, _params = [{name, _, type_args}, _type_def]} = spec] = kind_args}
          ]},
         state
       )
       when kind in [:type, :typep, :opaque] and is_atom(name) and
              (is_nil(type_args) or is_list(type_args)) do
    pre_type(
      {:@, meta_attr, [{kind, add_no_call(kind_meta), kind_args}]},
      state,
      meta_attr,
      name,
      List.wrap(type_args),
      expand_aliases_in_ast(state, spec),
      kind
    )
  end

  defp pre(
         {:@, meta_attr,
          [
            {kind, kind_meta,
             [{:when, _, [{:"::", _meta, _params = [{name, _, type_args}, _type_def]}, _]} = spec] =
               kind_args}
          ]},
         state
       )
       when kind in [:spec, :callback, :macrocallback] and is_atom(name) and
              (is_nil(type_args) or is_list(type_args)) do
    pre_spec(
      {:@, meta_attr,
       [
         {kind, add_no_call(kind_meta), kind_args}
       ]},
      state,
      meta_attr,
      name,
      expand_aliases_in_ast(state, List.wrap(type_args)),
      expand_aliases_in_ast(state, spec),
      kind
    )
  end

  defp pre(
         {:@, meta_attr,
          [
            {kind, meta_kind,
             [{:"::", _meta, _params = [{name, _, type_args}, _type_def]} = spec] = kind_args}
          ]},
         state
       )
       when kind in [:spec, :callback, :macrocallback] and is_atom(name) and
              (is_nil(type_args) or is_list(type_args)) do
    pre_spec(
      {:@, meta_attr, [{kind, add_no_call(meta_kind), kind_args}]},
      state,
      meta_attr,
      name,
      expand_aliases_in_ast(state, List.wrap(type_args)),
      expand_aliases_in_ast(state, spec),
      kind
    )
  end

  # incomplete spec
  # @callback my(integer)
  defp pre(
         {:@, meta_attr, [{kind, meta_kind, [{name, meta_name, type_args}]} = spec]},
         state
       )
       when kind in [:spec, :callback, :macrocallback] and is_atom(name) and
              (is_nil(type_args) or is_list(type_args)) do
    pre_spec(
      {:@, meta_attr, [{kind, add_no_call(meta_kind), [{name, meta_name, type_args}]}]},
      state,
      meta_attr,
      name,
      expand_aliases_in_ast(state, List.wrap(type_args)),
      expand_aliases_in_ast(state, spec),
      kind
    )
  end

  defp pre({:@, meta_attr, [{name, meta, params}]}, state) when is_atom(name) do
    name_string = Atom.to_string(name)

    if String.match?(name_string, ~r/^[_\p{Ll}\p{Lo}][\p{L}\p{N}_]*[?!]?$/u) and
         not String.starts_with?(name_string, "__atom_elixir_marker_") do
      line = Keyword.fetch!(meta_attr, :line)
      column = Keyword.fetch!(meta_attr, :column)

      binding =
        case List.wrap(params) do
          [] ->
            {nil, false}

          [param] ->
            {get_binding_type(state, param), true}

          _ ->
            :error
        end

      case binding do
        {type, is_definition} ->
          state =
            add_moduledoc_positions(
              state,
              [line: line, column: column],
              [{name, meta, params}],
              line
            )

          new_ast = {:@, meta_attr, [{name, add_no_call(meta), params}]}
          pre_module_attribute(new_ast, state, {line, column}, name, type, is_definition)

        _ ->
          {[], state}
      end
    else
      # most likely not an attribute
      {[], state}
    end
  end

  defp pre(
         {directive, _meta, _args} = ast,
         state
       )
       when directive in [:alias, :require, :import, :use] do
    {ast, state, _env} = expand(ast, state)
    {ast, state}
  end

  defp pre({:defoverridable, meta, [arg]} = ast, state) do
    {ast, state, _env} = expand(ast, state)
    {ast, state}
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

  defp pre({var_or_call, meta, nil} = ast, state)
       when is_atom(var_or_call) and var_or_call != :__MODULE__ do
    line = Keyword.fetch!(meta, :line)
    column = Keyword.fetch!(meta, :column)

    if Enum.any?(get_current_vars(state), &(&1.name == var_or_call)) do
      vars =
        state
        |> find_vars(ast)
        |> merge_same_name_vars()

      add_vars(state, vars, false)
    else
      # pre Elixir 1.4 local call syntax
      # TODO can we remove when we require elixir 1.15+?
      # it's only legal inside typespecs
      # credo:disable-for-next-line
      if not Keyword.get(meta, :no_call, false) and
           (Version.match?(System.version(), "< 1.15.0-dev") or
              match?([{:typespec, _, _} | _], state.scopes)) do
        add_call_to_line(state, {nil, var_or_call, 0}, {line, column})
      else
        state
      end
    end
    |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp pre({:when, meta, [lhs, rhs]}, state) do
    vars =
      state
      |> find_vars(lhs)
      |> merge_same_name_vars()

    state
    |> add_vars(vars, true)
    |> result({:when, meta, [:_, rhs]})
  end

  defp pre({type, meta, fields} = ast, state)
       when type in [:defstruct, :defexception] do
    {position, end_position} = extract_range(meta)

    fields =
      case fields do
        [fields] when is_list(fields) ->
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

        _ ->
          []
      end

    state
    |> add_struct_or_exception(type, fields, position, end_position)
    |> result(ast)
  end

  # transform `a |> b(c)` calls into `b(a, c)`
  defp pre({:|>, _, [params_1, {call, meta, params_rest}]}, state) do
    params = [params_1 | params_rest || []]
    pre({call, meta, params}, state)
  end

  # transform external and local func capture into fake call
  defp pre({:&, _, [{:/, _, [fun, arity]}]}, state) when is_integer(arity) do
    fake_params =
      if arity == 0 do
        []
      else
        for _ <- 1..arity, do: nil
      end

    call =
      case fun do
        {func, position, nil} ->
          {func, position, fake_params}

        {{:., _, [_ | _]} = ast_part, position, []} ->
          {ast_part, position, fake_params}

        _ ->
          nil
      end

    pre(call, state)
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
    |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp pre(
         {{:., meta1, [{:__aliases__, _, [:Record]} = module_expression, call]}, _meta,
          params = [name, _]} = ast,
         state
       )
       when is_call(call, params) and call in [:defrecord, :defrecordp] and
              is_atom(name) do
    {position = {line, column}, end_position} = extract_range(meta1)

    # TODO pass env
    {module, state, _env} = expand(module_expression, state)

    type =
      case call do
        :defrecord -> :defmacro
        :defrecordp -> :defmacrop
      end

    options = [generated: true]

    shift = if state.generated, do: 0, else: 1
    env = get_current_env(state)

    state
    |> new_named_func(name, 1)
    |> add_func_to_index(
      env,
      name,
      [{:\\, [], [{:args, [], nil}, []]}],
      position,
      end_position,
      type,
      options
    )
    |> new_named_func(name, 2)
    |> add_func_to_index(
      env,
      name,
      [{:record, [], nil}, {:args, [], nil}],
      position,
      end_position,
      type,
      options
    )
    |> add_call_to_line({module, call, length(params)}, {line, column + shift})
    |> add_current_env_to_line(line)
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
      |> add_current_env_to_line(line)
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
    |> add_current_env_to_line(line)
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
    |> add_current_env_to_line(line)
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
    |> add_current_env_to_line(line)
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
    |> add_current_env_to_line(line)
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
    |> add_current_env_to_line(line)
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
    |> add_current_env_to_line(line)
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
          |> add_current_env_to_line(line)
          |> result(ast)
        else
          state
          |> add_current_env_to_line(line)
          |> result(ast)
        end
    end
  end

  # Any other tuple with a line
  defp pre({_, meta, _} = ast, state) do
    case Keyword.get(meta, :line) do
      nil ->
        {ast, state}

      line ->
        state
        |> add_current_env_to_line(line)
        |> result(ast)
    end
  end

  defp pre(ast, state) do
    {ast, state}
  end

  defp post(
         {:defmodule, _meta, [_module, _]} = ast,
         state
       ) do
    post_module(ast, state)
  end

  defp post(
         {:defprotocol, _meta, [_protocol, _]} = ast,
         state
       ) do
    post_protocol(ast, state)
  end

  defp post(
         {:defimpl, _meta, [_protocol, _impl_args | _]} = ast,
         state
       ) do
    post_module(ast, state)
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
    line = Keyword.fetch!(meta, :line)
    match_context_r = get_binding_type(state, rhs)

    match_context_r =
      if atom == :<- and match?([:for | _], state.binding_context) do
        {:for_expression, match_context_r}
      else
        match_context_r
      end

    vars_l = find_vars(state, lhs, match_context_r)

    vars =
      case rhs do
        {:=, _, [nested_lhs, _nested_rhs]} ->
          match_context_l = get_binding_type(state, lhs)
          nested_vars = find_vars(state, nested_lhs, match_context_l)

          vars_l ++ nested_vars

        _ ->
          vars_l
      end
      |> merge_same_name_vars()

    # Variables and calls were added for the left side of the assignment in `pre` call, but without
    # the context of an assignment. Thus, they have to be removed here. On their place there will
    # be added new variables having types merged with types of corresponding deleted variables.
    remove_positions = Enum.flat_map(vars, fn %VarInfo{positions: positions} -> positions end)

    state
    |> remove_calls(remove_positions)
    |> merge_new_vars(vars, remove_positions)
    |> add_current_env_to_line(line)
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

  # struct or struct update
  def get_binding_type(
        state,
        {:%, _meta,
         [
           struct_ast,
           {:%{}, _, _} = ast
         ]}
      ) do
    {fields, updated_struct} =
      case get_binding_type(state, ast) do
        {:map, fields, updated_map} -> {fields, updated_map}
        {:struct, fields, _, updated_struct} -> {fields, updated_struct}
        _ -> {[], nil}
      end

    # expand struct type - only compile type atoms or attributes are supported
    type =
      case get_binding_type(state, struct_ast) do
        {:atom, atom} -> {:atom, atom}
        {:attribute, attribute} -> {:attribute, attribute}
        _ -> nil
      end

    {:struct, fields, type, updated_struct}
  end

  # pipe
  def get_binding_type(state, {:|>, _, [params_1, {call, meta, params_rest}]}) do
    params = [params_1 | params_rest || []]
    get_binding_type(state, {call, meta, params})
  end

  # remote call
  def get_binding_type(state, {{:., _, [target, fun]}, _, args})
      when is_atom(fun) and is_list(args) do
    target = get_binding_type(state, target)
    {:call, target, fun, Enum.map(args, &get_binding_type(state, &1))}
  end

  # current module
  def get_binding_type(state, {:__MODULE__, _, nil} = module) do
    {module, _state, _env} = expand(module, state)
    {:atom, module}
  end

  # elixir module
  def get_binding_type(state, {:__aliases__, _, list} = module) when is_list(list) do
    try do
      {module, _state, _env} = expand(module, state)
      {:atom, module}
    rescue
      _ -> nil
    end
  end

  # variable or local no parens call
  def get_binding_type(_state, {var, _, nil}) when is_atom(var) do
    {:variable, var}
  end

  # attribute
  def get_binding_type(_state, {:@, _, [{attribute, _, nil}]})
      when is_atom(attribute) do
    {:attribute, attribute}
  end

  # erlang module or atom
  def get_binding_type(_state, atom) when is_atom(atom) do
    {:atom, atom}
  end

  # map update
  def get_binding_type(
        state,
        {:%{}, _meta,
         [
           {:|, _meta1,
            [
              updated_map,
              fields
            ]}
         ]}
      )
      when is_list(fields) do
    {:map, get_fields_binding_type(state, fields), get_binding_type(state, updated_map)}
  end

  # map
  def get_binding_type(state, {:%{}, _meta, fields}) when is_list(fields) do
    {:map, get_fields_binding_type(state, fields), nil}
  end

  # match
  def get_binding_type(state, {:=, _, [_, ast]}) do
    get_binding_type(state, ast)
  end

  # stepped range struct
  def get_binding_type(_state, {:"..//", _, [_, _, _]}) do
    {:struct, [], {:atom, Range}}
  end

  # range struct
  def get_binding_type(_state, {:.., _, [_, _]}) do
    {:struct, [], {:atom, Range}}
  end

  @builtin_sigils %{
    sigil_D: Date,
    sigil_T: Time,
    sigil_U: DateTime,
    sigil_N: NaiveDateTime,
    sigil_R: Regex,
    sigil_r: Regex
  }

  # builtin sigil struct
  def get_binding_type(_state, {sigil, _, _}) when is_map_key(@builtin_sigils, sigil) do
    # TODO support custom sigils
    {:struct, [], {:atom, @builtin_sigils |> Map.fetch!(sigil)}}
  end

  # tuple
  # regular tuples use {:{}, [], [field_1, field_2]} ast
  # two element use {field_1, field_2} ast (probably as an optimization)
  # detect and convert to regular
  def get_binding_type(state, ast) when is_tuple(ast) and tuple_size(ast) == 2 do
    get_binding_type(state, {:{}, [], Tuple.to_list(ast)})
  end

  def get_binding_type(state, {:{}, _, list}) do
    {:tuple, length(list), list |> Enum.map(&get_binding_type(state, &1))}
  end

  def get_binding_type(state, list) when is_list(list) do
    type =
      case list do
        [] -> :empty
        [{:|, _, [head, _tail]}] -> get_binding_type(state, head)
        [head | _] -> get_binding_type(state, head)
      end

    {:list, type}
  end

  def get_binding_type(state, list) when is_list(list) do
    {:list, list |> Enum.map(&get_binding_type(state, &1))}
  end

  # pinned variable
  def get_binding_type(state, {:^, _, [pinned]}), do: get_binding_type(state, pinned)

  # local call
  def get_binding_type(state, {var, _, args}) when is_atom(var) and is_list(args) do
    {:local_call, var, Enum.map(args, &get_binding_type(state, &1))}
  end

  # integer
  def get_binding_type(_state, integer) when is_integer(integer) do
    {:integer, integer}
  end

  # other
  def get_binding_type(_state, _), do: nil

  defp get_fields_binding_type(state, fields) do
    for {field, value} <- fields,
        is_atom(field) do
      {field, get_binding_type(state, value)}
    end
  end

  defp add_no_call(meta) do
    [{:no_call, true} | meta]
  end

  defp pre_protocol_implementation(
         ast,
         state,
         meta,
         protocol,
         for_expression
       ) do
    # TODO pass env
    {protocol, state, _env} = expand(protocol, state)
    implementations = get_implementations_from_for_expression(state, for_expression)

    pre_module(ast, state, meta, protocol, [], [{:__impl__, [:atom], :def}], for: implementations)
  end

  defp get_implementations_from_for_expression(state, for: for_expression) do
    # TODO fold state?
    for_expression
    |> List.wrap()
    |> Enum.map(fn alias ->
      {module, _state, _env} = expand(alias, state)
      module
    end)
  end

  defp get_implementations_from_for_expression(state, _other) do
    [state |> get_current_module]
  end

  defp maybe_add_protocol_behaviour({protocol, _}, state, env) do
    {_, state, env} = add_behaviour(protocol, state, env)
    {state, env}
  end

  defp maybe_add_protocol_behaviour(_, state, env), do: {state, env}

  defp add_struct_or_exception(state, type, fields, {line, column} = position, end_position) do
    fields =
      fields ++
        if type == :defexception do
          [__exception__: true]
        else
          []
        end

    options = [generated: true]
    env = get_current_env(state)

    state =
      if type == :defexception do
        {_, state, env} = add_behaviour(Exception, state, env)

        if Keyword.has_key?(fields, :message) do
          state
          |> add_func_to_index(
            env,
            :exception,
            [{:msg, [line: line, column: column], nil}],
            position,
            end_position,
            :def,
            options
          )
          |> add_func_to_index(
            env,
            :message,
            [{:exception, [line: line, column: column], nil}],
            position,
            end_position,
            :def,
            options
          )
        else
          state
        end
        |> add_func_to_index(
          env,
          :exception,
          [{:args, [line: line, column: column], nil}],
          position,
          end_position,
          :def,
          options
        )
      else
        state
      end
      |> add_func_to_index(env, :__struct__, [], position, end_position, :def, options)
      |> add_func_to_index(
        env,
        :__struct__,
        [{:kv, [line: line, column: column], nil}],
        position,
        end_position,
        :def,
        options
      )

    state
    |> add_struct(env, type, fields)
  end

  defp expand_aliases_in_ast(state, ast) do
    # TODO shouldn't that handle more cases?
    Macro.prewalk(ast, fn
      {:__aliases__, meta, [Elixir]} ->
        {:__aliases__, meta, [Elixir]}

      {:__aliases__, meta, _list} = module ->
        {module, _state, _env} = expand(module, state)
        list = module |> Module.split() |> Enum.map(&String.to_atom/1)
        {:__aliases__, meta, list}

      {:__MODULE__, meta, nil} = module ->
        {module, _state, _env} = expand(module, state)
        list = module |> Module.split() |> Enum.map(&String.to_atom/1)
        {:__aliases__, meta, list}

      other ->
        other
    end)
  end

  defp ex_unit_test_name(state, name) do
    case state.context[:ex_unit_describe] do
      nil -> "test #{name}"
      describe -> "test #{describe} #{name}"
    end
    |> String.to_atom()
  end
end
