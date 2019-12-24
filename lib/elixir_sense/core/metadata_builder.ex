defmodule ElixirSense.Core.MetadataBuilder do
  @moduledoc """
  This module is responsible for building/retrieving environment information from an AST.
  """

  import ElixirSense.Core.State
  alias ElixirSense.Core.Ast
  alias ElixirSense.Core.State
  alias ElixirSense.Core.State.VarInfo
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Source

  @scope_keywords [:for, :try, :fn]
  @block_keywords [:do, :else, :rescue, :catch, :after]
  @defs [:def, :defp, :defmacro, :defmacrop, :defdelegate, :defguard, :defguardp]
  @protocol_types [{:t, [], :type}]

  defguardp is_call(call, params)
            when is_atom(call) and is_list(params) and
                   call not in [:., :__aliases__, :"::", :{}, :|>]

  @doc """
  Traverses the AST building/retrieving the environment information.
  It returns a `ElixirSense.Core.State` struct containing the information.
  """
  @spec build(Macro.t()) :: State.t()
  def build(ast) do
    {_ast, state} = Macro.traverse(ast, %State{}, &pre/2, &post/2)
    state
  end

  defp pre_module(ast, state, position = {line, column}, module, types \\ []) do
    module = normalize_module(module)

    state =
      state
      |> maybe_add_protocol_implementation(module)
      |> new_namespace(module)
      |> add_current_module_to_index(position)
      |> new_attributes_scope
      |> new_behaviours_scope
      |> new_alias_scope
      |> new_import_scope
      |> new_require_scope
      |> new_vars_scope

    types
    |> Enum.reduce(state, fn {type_name, type_args, kind}, acc ->
      acc
      |> add_type(type_name, type_args, kind, %{line: line, col: column})
    end)
    |> result(ast)
  end

  defp post_module(ast, state, module) do
    module = normalize_module(module)

    state
    |> remove_attributes_scope
    |> remove_behaviours_scope
    |> remove_alias_scope
    |> remove_import_scope
    |> remove_require_scope
    |> remove_vars_scope
    |> remove_module_from_namespace(module)
    |> remove_protocol_implementation
    |> result(ast)
  end

  defp pre_func(ast = {type, _, _}, state, %{line: line, col: col}, name, params) do
    state
    |> new_named_func(name, length(params || []))
    |> add_func_to_index(name, params || [], {line, col}, type)
    |> new_alias_scope
    |> new_import_scope
    |> new_require_scope
    |> new_func_vars_scope
    |> add_vars(find_vars(params), true)
    |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp post_func(ast, state) do
    state
    |> remove_alias_scope
    |> remove_import_scope
    |> remove_require_scope
    |> remove_func_vars_scope
    |> remove_last_scope_from_scopes
    |> result(ast)
  end

  defp pre_scope_keyword(ast, state, line) do
    state
    |> add_current_env_to_line(line)
    |> new_vars_scope
    |> result(ast)
  end

  defp post_scope_keyword(ast, state) do
    state
    |> remove_vars_scope
    |> result(ast)
  end

  defp pre_block_keyword(ast, state) do
    state
    |> new_alias_scope
    |> new_import_scope
    |> new_require_scope
    |> new_vars_scope
    |> result(ast)
  end

  defp post_block_keyword(ast, state) do
    state
    |> remove_alias_scope
    |> remove_import_scope
    |> remove_require_scope
    |> remove_vars_scope
    |> result(ast)
  end

  defp pre_clause(ast = {_, [line: line, column: _column], _}, state, lhs) do
    state
    |> new_alias_scope
    |> new_import_scope
    |> new_require_scope
    |> new_vars_scope
    |> add_vars(find_vars(lhs), true)
    |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp post_clause(ast, state) do
    state
    |> remove_alias_scope
    |> remove_import_scope
    |> remove_require_scope
    |> remove_vars_scope
    |> result(ast)
  end

  defp pre_alias(ast, state, line, aliases_tuples) do
    state
    |> add_current_env_to_line(line)
    |> add_aliases(List.wrap(aliases_tuples))
    |> result(ast)
  end

  defp pre_import(ast, state, line, modules) do
    modules = List.wrap(modules)

    state
    |> add_current_env_to_line(line)
    |> add_requires(modules)
    |> add_imports(modules)
    |> result(ast)
  end

  defp pre_require(ast, state, line, modules) do
    state
    |> add_current_env_to_line(line)
    |> add_requires(List.wrap(modules))
    |> result(ast)
  end

  defp pre_module_attribute(ast, state, line, name) do
    state
    |> add_current_env_to_line(line)
    |> add_attribute(name)
    |> result(ast)
  end

  defp pre_behaviour(ast, state, line, module) do
    state
    |> add_current_env_to_line(line)
    |> add_behaviour(module)
    |> result(ast)
  end

  defp pre_type(ast, state, %{line: line, col: col}, type_name, type_args, kind) do
    state
    |> add_current_env_to_line(line)
    |> add_type(type_name, type_args, kind, %{line: line, col: col})
    |> result(ast)
  end

  defp post_string_literal(ast, state, line, str) do
    str
    |> String.split(["\n", "\r\n"])
    |> Enum.with_index()
    |> Enum.reduce(state, fn {_s, i}, acc -> add_current_env_to_line(acc, line + i) end)
    |> result(ast)
  end

  defp pre(
         {:defmodule, _, [{:__aliases__, [line: line, column: column], module}, _]} = ast,
         state
       ) do
    pre_module(ast, state, {line, column}, module)
  end

  defp pre({:defmodule, [line: line, column: column], [module, _]} = ast, state)
       when is_atom(module) do
    pre_module(ast, state, {line, column}, module)
  end

  defp pre(
         {:defprotocol, _, [{:__aliases__, [line: line, column: column], module}, _]} = ast,
         state
       ) do
    # protocol defines a type `{:type, {:t, {:type, 1, :term, []}, []}}`
    pre_module(ast, state, {line, column}, module, @protocol_types)
  end

  defp pre({:defprotocol, [line: line, column: column], [module, _]} = ast, state)
       when is_atom(module) do
    # protocol defines a type `{:type, {:t, {:type, 1, :term, []}, []}}`
    pre_module(ast, state, {line, column}, module, @protocol_types)
  end

  defp pre(
         {:defimpl, _, [{:__aliases__, [line: line, column: column], protocol}, impl_args | _]} =
           ast,
         state
       ) do
    pre_protocol_implementation(ast, state, {line, column}, protocol, impl_args)
  end

  defp pre(
         {:defimpl, [line: line, column: column], [protocol, impl_args | _]} = ast,
         state
       )
       when is_atom(protocol) do
    pre_protocol_implementation(ast, state, {line, column}, protocol, impl_args)
  end

  defp pre({def_name, meta, [{:when, _, [head | _]}, body]}, state) when def_name in @defs do
    pre({def_name, meta, [head, body]}, state)
  end

  defp pre({def_name, meta, [{name, [line: line, column: column] = meta2, params}, body]}, state)
       when def_name in @defs and is_atom(name) do
    ast_without_params = {def_name, meta, [{name, add_no_call(meta2), []}, body]}
    pre_func(ast_without_params, state, %{line: line, col: column}, name, params)
  end

  # defguard and defguardp
  defp pre(
         {def_name, meta,
          [
            {:when, [line: _, column: _],
             [{name, [line: line, column: column] = meta2, params}, body]}
          ]},
         state
       )
       when def_name in [:defguard, :defguardp] do
    ast_without_params = {def_name, meta, [{name, add_no_call(meta2), []}, body]}
    pre_func(ast_without_params, state, %{line: line, col: column}, name, params)
  end

  # protocol function
  defp pre({def_name, meta, [{name, [line: line, column: column] = meta2, params}]}, state)
       when def_name == :def do
    ast_without_params = {def_name, meta, [{name, add_no_call(meta2), []}, nil]}
    pre_func(ast_without_params, state, %{line: line, col: column}, name, params)
  end

  defp pre({def_name, _meta, _} = ast, state) when def_name in @defs do
    {ast, state}
  end

  defp pre(
         {:@, [line: line, column: _column],
          [{:behaviour, _, [{:__aliases__, _, module_expression}]}]} = ast,
         state
       ) do
    case concat_module_expression(state, module_expression) do
      {:ok, module} ->
        pre_behaviour(ast, state, line, module)

      :error ->
        state
        |> result(ast)
    end
  end

  defp pre({:@, [line: line, column: _column], [{:behaviour, _, [erlang_module]}]} = ast, state) do
    pre_behaviour(ast, state, line, erlang_module)
  end

  # protocol derive
  defp pre(
         {:@, [line: _line, column: _column] = position, [{:derive, _, [derived_protos]}]} = ast,
         state
       ) do
    current_module_variants = state |> get_current_module_variants

    List.wrap(derived_protos)
    |> Enum.map(fn
      {proto, _opts} -> proto
      proto -> proto
    end)
    |> Enum.reduce(state, fn proto, acc ->
      case split_module_expression(state, proto) do
        {:ok, proto_module} ->
          # protocol implementation module for Any
          mod_any = Module.concat(proto_module ++ [Any])

          current_module_variants
          |> Enum.reduce(acc, fn variant, acc_1 ->
            # protocol implementation module built by @derive
            mod = Module.concat(proto_module ++ [variant])

            case acc_1.mods_funs[mod_any] do
              nil ->
                # implemantation for: Any not detected (is in other file etc.)
                acc_1
                |> add_module_to_index(mod, position)

              any_mods_funs ->
                # copy implemantation for: Any
                copied_mods_funs_to_positions =
                  for {{module, fun, arity}, val} <- acc_1.mods_funs_to_positions,
                      module == mod_any,
                      into: %{},
                      do: {{mod, fun, arity}, val}

                %{
                  acc_1
                  | mods_funs: acc_1.mods_funs |> Map.put(mod, any_mods_funs),
                    mods_funs_to_positions:
                      acc_1.mods_funs_to_positions |> Map.merge(copied_mods_funs_to_positions)
                }
            end
          end)

        :error ->
          acc
      end
    end)
    |> result(ast)
  end

  defp pre(
         {:@, [line: line, column: column] = _meta_attr,
          [{kind, _, [{:"::", _meta, _params = [{name, _, type_args}, _type_def]}]}]} = ast,
         state
       )
       when kind in [:type, :typep, :opaque] and is_atom(name) and
              (is_nil(type_args) or is_list(type_args)) do
    pre_type(ast, state, %{line: line, col: column}, name, List.wrap(type_args), kind)
  end

  defp pre({:@, [line: line, column: _column] = meta_attr, [{name, meta, params}]}, state) do
    new_ast = {:@, meta_attr, [{name, add_no_call(meta), params}]}
    pre_module_attribute(new_ast, state, line, name)
  end

  # import with v1.2 notation
  defp pre(
         {:import, [line: line, column: _column],
          [{{:., _, [prefix_expression, :{}]}, _, imports}]} = ast,
         state
       ) do
    imports_modules = modules_from_12_syntax(state, imports, prefix_expression)

    pre_import(ast, state, line, imports_modules)
  end

  # import without options
  defp pre({:import, meta, [module_info]}, state) do
    pre({:import, meta, [module_info, []]}, state)
  end

  # import with options
  defp pre(
         {:import, [line: line, column: _column], [{_, _, module_expression = [_ | _]}, _opts]} =
           ast,
         state
       ) do
    case concat_module_expression(state, module_expression) do
      {:ok, module} ->
        pre_import(ast, state, line, module)

      :error ->
        state
        |> result(ast)
    end
  end

  # atom module
  defp pre({:import, [line: line, column: _column], [atom | _] = ast}, state)
       when is_atom(atom) do
    pre_import(ast, state, line, atom)
  end

  # require with v1.2 notation
  defp pre(
         {:require, [line: line, column: _column],
          [{{:., _, [prefix_expression, :{}]}, _, requires}]} = ast,
         state
       ) do
    requires_modules = modules_from_12_syntax(state, requires, prefix_expression)

    pre_require(ast, state, line, requires_modules)
  end

  # require without options
  defp pre({:require, meta, [module_info]}, state) do
    pre({:require, meta, [module_info, []]}, state)
  end

  # require with `as` option
  defp pre(
         {:require, [line: line, column: _column],
          [{_, _, module_expression = [_ | _]}, [as: alias_expression]]} = ast,
         state
       ) do
    case concat_module_expression(state, module_expression) do
      {:ok, module} ->
        alias_tuple = alias_tuple(module, alias_expression)

        {_, new_state} = pre_alias(ast, state, line, alias_tuple)
        pre_require(ast, new_state, line, module)

      :error ->
        state
        |> result(ast)
    end
  end

  # require erlang module with `as` option
  defp pre({:require, [line: line, column: _column], [mod, [as: alias_expression]]} = ast, state)
       when is_atom(mod) do
    alias_tuple = alias_tuple(mod, alias_expression)
    {_, new_state} = pre_alias(ast, state, line, alias_tuple)
    pre_require(ast, new_state, line, mod)
  end

  # require with options
  defp pre(
         {:require, [line: line, column: _column], [{_, _, module_expression = [_ | _]}, _opts]} =
           ast,
         state
       ) do
    case concat_module_expression(state, module_expression) do
      {:ok, module} ->
        pre_require(ast, state, line, module)

      :error ->
        state
        |> result(ast)
    end
  end

  defp pre({:require, [line: line, column: _column], [mod, _opts]} = ast, state)
       when is_atom(mod) do
    pre_require(ast, state, line, mod)
  end

  # alias with v1.2 notation
  defp pre(
         {:alias, [line: line, column: _column],
          [{{:., _, [prefix_expression, :{}]}, _, aliases}]} = ast,
         state
       ) do
    case split_module_expression(state, prefix_expression) do
      {:ok, prefix_atoms} ->
        aliases_tuples =
          aliases
          |> Enum.map(fn
            {:__aliases__, _, mods} -> {Module.concat(mods), Module.concat(prefix_atoms ++ mods)}
            mod when is_atom(mod) -> {mod, Module.concat(prefix_atoms ++ [mod])}
          end)

        pre_alias(ast, state, line, aliases_tuples)

      :error ->
        state
        |> result(ast)
    end
  end

  # alias without options
  defp pre(
         {:alias, [line: line, column: _column], [{:__aliases__, _, module_expression = [_ | _]}]} =
           ast,
         state
       ) do
    case concat_module_expression(state, module_expression) do
      {:ok, module} ->
        alias_tuple = {Module.concat([List.last(module_expression)]), module}
        pre_alias(ast, state, line, alias_tuple)

      :error ->
        state
        |> result(ast)
    end
  end

  # alias with `as` option
  defp pre(
         {:alias, [line: line, column: _column],
          [{_, _, module_expression = [_ | _]}, [as: alias_expression]]} = ast,
         state
       ) do
    case concat_module_expression(state, module_expression) do
      {:ok, module} ->
        alias_tuple = alias_tuple(module, alias_expression)
        pre_alias(ast, state, line, alias_tuple)

      :error ->
        state
        |> result(ast)
    end
  end

  # alias atom module
  defp pre({:alias, [line: line, column: _column], [mod]} = ast, state) when is_atom(mod) do
    alias_tuple =
      if Introspection.elixir_module?(mod) do
        {Module.concat([List.last(Module.split(mod))]), mod}
      else
        {mod, mod}
      end

    pre_alias(ast, state, line, alias_tuple)
  end

  # alias atom module with `as` option
  defp pre({:alias, [line: line, column: _column], [mod, [as: alias_expression]]} = ast, state)
       when is_atom(mod) do
    alias_tuple = alias_tuple(mod, alias_expression)
    pre_alias(ast, state, line, alias_tuple)
  end

  defp pre({atom, [line: line, column: _column], [_ | _]} = ast, state)
       when atom in @scope_keywords do
    pre_scope_keyword(ast, state, line)
  end

  defp pre({atom, _block} = ast, state) when atom in @block_keywords do
    pre_block_keyword(ast, state)
  end

  defp pre({:->, meta, [lhs, rhs]}, state) do
    pre_clause({:->, meta, [:_, rhs]}, state, lhs)
  end

  defp pre({:=, meta, [lhs, rhs]}, state) do
    state
    |> add_vars(find_vars(lhs), true)
    |> result({:=, meta, [:_, rhs]})
  end

  defp pre({var_or_call, [line: line, column: column], context} = ast, state)
       when is_atom(var_or_call) and var_or_call != :__MODULE__ and context in [nil, Elixir] do
    if Enum.any?(get_current_vars(state), &(&1.name == var_or_call)) do
      state
      |> add_vars(find_vars(ast), false)
    else
      # pre Elixir 1.4 local call syntax
      # TODO remove on Elixir 2.0
      state
      |> add_call_to_line({nil, var_or_call, 0}, line, column)
      |> add_current_env_to_line(line)
    end
    |> result(ast)
  end

  defp pre({:<-, meta, [lhs, rhs]}, state) do
    state
    |> add_vars(find_vars(lhs), true)
    |> result({:<-, meta, [:_, rhs]})
  end

  # Kernel: defmacro use(module, opts \\ [])
  defp pre({:use, [line: _, column: _], [{param, _, nil} | _]} = ast, state)
       when is_atom(param) do
    state
    |> result(ast)
  end

  defp pre({:use, [line: line, column: column], _} = ast, state) do
    # take first variant as we optimistically assume that the result of expanding `use` will be the same for all variants
    current_module = get_current_module(state)

    current_module_length =
      case current_module do
        Elixir -> 0
        other -> length(Module.split(other))
      end

    current_module_variants = get_current_module_variants(state)

    %{
      requires: requires,
      imports: imports,
      behaviours: behaviours,
      aliases: aliases,
      attributes: attributes,
      mods_funs: mods_funs
    } = Ast.extract_use_info(ast, current_module, state)

    state =
      state
      |> add_aliases(aliases)
      |> add_requires(requires)
      |> add_imports(imports)
      |> add_behaviours(behaviours)
      |> add_attributes(attributes)

    state =
      Enum.reduce(mods_funs, state, fn
        {name, args, type}, acc ->
          acc
          |> add_func_to_index(name, args, {line, column}, type)

        module, acc ->
          submodule_parts = Module.split(module) |> Enum.drop(current_module_length)

          Enum.reduce(current_module_variants, acc, fn variant, acc_1 ->
            module =
              (Module.split(variant) ++ submodule_parts)
              |> Module.concat()

            acc_1
            |> add_module_to_index(module, {line, column})
          end)
      end)

    state
    |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp pre({type, _, fields} = ast, state) when type in [:defstruct, :defexception] do
    fields =
      case fields do
        [fields] when is_list(fields) ->
          if Enum.all?(fields, fn
               field when is_atom(field) -> true
               {field, _} when is_atom(field) -> true
               _ -> false
             end) do
            fields
            |> Enum.map(fn
              field when is_atom(field) -> {field, nil}
              {field, value} when is_atom(field) -> {field, value}
            end)
          else
            []
          end

        _ ->
          []
      end

    state =
      if type == :defexception do
        state
        |> add_behaviour(Exception)
      else
        state
      end

    state
    |> add_struct(type, fields)
    |> result(ast)
  end

  # transform `a |> b(c)` calls into `b(a, c)`
  defp pre({:|>, _, [params_1, {call, [line: line, column: column], params_rest}]}, state) do
    params = [params_1 | params_rest || []]
    pre({call, [line: line, column: column], params}, state)
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

  defp pre({call, [line: line, column: column], params} = ast, state)
       when is_call(call, params) do
    state =
      if !String.starts_with?(to_string(call), "__atom_elixir_marker_") do
        add_call_to_line(state, {nil, call, length(params)}, line, column)
      else
        state
      end

    state
    |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp pre(
         {{:., _, [{:__aliases__, _, module_expression = [_ | _]}, call]},
          [line: line, column: col], params} = ast,
         state
       )
       when is_call(call, params) do
    case concat_module_expression(state, module_expression) do
      {:ok, module} ->
        state
        |> add_call_to_line({module, call, length(params)}, line, col + 1)
        |> add_current_env_to_line(line)

      :error ->
        state
    end
    |> result(ast)
  end

  defp pre(
         {{:., _, [{:__MODULE__, _, nil}, call]}, [line: line, column: col], params} = ast,
         state
       )
       when is_call(call, params) do
    module = get_current_module(state)

    state
    |> add_call_to_line({module, call, length(params)}, line, col + 1)
    |> add_current_env_to_line(line)
    |> result(ast)
  end

  defp pre(
         {{:., _, [module, call]}, [line: line, column: col], params} = ast,
         state
       )
       when is_atom(module) and is_call(call, params) do
    state
    |> add_call_to_line({module, call, length(params)}, line, col + 1)
    |> add_current_env_to_line(line)
    |> result(ast)
  end

  # Any other tuple with a line
  defp pre({_, [line: line, column: _column], _} = ast, state) do
    state
    |> add_current_env_to_line(line)
    |> result(ast)
  end

  # No line defined
  defp pre(ast, state) do
    {ast, state}
  end

  defp post({:defmodule, _, [{:__aliases__, _, module}, _]} = ast, state) do
    post_module(ast, state, module)
  end

  defp post({:defmodule, _, [module, _]} = ast, state) when is_atom(module) do
    post_module(ast, state, module)
  end

  defp post({:defprotocol, _, [{:__aliases__, _, module}, _]} = ast, state) do
    post_module(ast, state, module)
  end

  defp post({:defprotocol, _, [module, _]} = ast, state) when is_atom(module) do
    post_module(ast, state, module)
  end

  defp post({:defimpl, _, [{:__aliases__, _, protocol}, [for: implementations], _]} = ast, state) do
    post_protocol_implementation(ast, state, protocol, implementations)
  end

  defp post({:defimpl, _, [protocol, [for: implementations], _]} = ast, state)
       when is_atom(protocol) do
    post_protocol_implementation(ast, state, protocol, implementations)
  end

  defp post({def_name, [line: _line, column: _column], [{name, _, _params}, _]} = ast, state)
       when def_name in @defs and is_atom(name) do
    post_func(ast, state)
  end

  defp post({def_name, _, _} = ast, state) when def_name in @defs do
    {ast, state}
  end

  defp post({atom, _, [_ | _]} = ast, state) when atom in @scope_keywords do
    post_scope_keyword(ast, state)
  end

  defp post({atom, _block} = ast, state) when atom in @block_keywords do
    post_block_keyword(ast, state)
  end

  defp post({:->, [line: _line, column: _column], [_lhs, _rhs]} = ast, state) do
    post_clause(ast, state)
  end

  # String literal
  defp post({_, [no_call: true, line: line, column: _column], [str]} = ast, state)
       when is_binary(str) do
    post_string_literal(ast, state, line, str)
  end

  # String literal in sigils
  defp post({:<<>>, [line: line, column: _column], [str]} = ast, state) when is_binary(str) do
    post_string_literal(ast, state, line, str)
  end

  defp post(ast, state) do
    {ast, state}
  end

  defp result(state, ast) do
    {ast, state}
  end

  defp find_vars(ast) do
    {_ast, vars} = Macro.prewalk(ast, [], &match_var/2)
    vars
  end

  defp match_var({var, [line: line, column: column], context} = ast, vars)
       when is_atom(var) and context in [nil, Elixir] do
    var_info = %VarInfo{name: var, positions: [{line, column}]}
    {ast, [var_info | vars]}
  end

  defp match_var(ast, vars) do
    {ast, vars}
  end

  defp add_no_call(meta) do
    [{:no_call, true} | meta]
  end

  defp pre_protocol_implementation(
         ast,
         state,
         position,
         protocol,
         for_expression
       ) do
    implementations = get_implementations_from_for_expression(state, for_expression)

    pre_module(ast, state, position, {protocol, implementations})
  end

  defp post_protocol_implementation(ast, state, protocol, for_expression) do
    implementations = get_implementations_from_for_expression(state, for_expression)

    post_module(ast, state, {protocol, implementations})
  end

  defp get_implementations_from_for_expression(state, for: for_expression) do
    for_expression
    |> List.wrap()
    |> Enum.map(fn
      {:__aliases__, _, implementation} -> implementation
      module when is_atom(module) -> module
      {:__MODULE__, _, nil} -> state |> get_current_module
      _ -> nil
    end)
  end

  defp get_implementations_from_for_expression(state, _other) do
    [state |> get_current_module]
  end

  defp alias_tuple(module, alias_module) when is_atom(alias_module) do
    {alias_module, module}
  end

  defp alias_tuple(module, {:__aliases__, _, alias_atoms = [al | _]})
       when is_atom(module) and is_atom(al) do
    {Module.concat(alias_atoms), module}
  end

  defp modules_from_12_syntax(state, expressions, prefix_expression) do
    case split_module_expression(state, prefix_expression) do
      {:ok, prefix_atoms} ->
        for expression <- expressions do
          case split_module_expression(state, expression) do
            {:ok, suffix_atoms} ->
              Module.concat(prefix_atoms ++ suffix_atoms)

            :error ->
              :error
          end
        end
        |> Enum.reject(&(&1 == :error))

      :error ->
        []
    end
  end

  defp split_module_expression(state, {:__aliases__, _, mods}) do
    {:ok,
     case mods do
       [{:__MODULE__, _, nil} | rest] -> [state |> get_current_module | rest]
       other -> other
     end}
  end

  defp split_module_expression(_state, mod) when is_atom(mod), do: {:ok, [mod]}

  defp split_module_expression(state, {:__MODULE__, _, nil}),
    do: {:ok, [state |> get_current_module]}

  defp split_module_expression(_, _), do: :error

  defp concat_module_expression(state, module_parts) do
    Source.concat_module_parts(module_parts, state |> get_current_module, [])
  end

  defp normalize_module([{:__MODULE__, _, nil} | rest]), do: rest

  defp normalize_module({[{:__MODULE__, _, nil} | rest], implementations}),
    do: {rest, implementations}

  defp normalize_module(other), do: other
end
