defmodule ElixirSense.Core.MetadataBuilder do
  @moduledoc """
  This module is responsible for building/retrieving environment information from an AST.
  """

  import ElixirSense.Core.State

  alias ElixirSense.Core.State
  alias ElixirSense.Core.Compiler

  @doc """
  Traverses the AST building/retrieving the environment information.
  It returns a `ElixirSense.Core.State` struct containing the information.
  """
  @spec build(Macro.t()) :: State.t()
  def build(ast) do
    {_ast, state, _env} = Compiler.expand(ast, %State{}, Compiler.env())

    state
    |> remove_attributes_scope
    |> remove_vars_scope
    |> remove_module
  end

  # defp post_string_literal(ast, _state, _line, str) do
  #   str
  #   |> Source.split_lines()
  #   |> Enum.with_index()
  #   # |> Enum.reduce(state, fn {_s, i}, acc -> add_current_env_to_line(acc, line + i) end)
  #   # |> result(ast)
  # end

  # # ex_unit describe
  # defp pre(
  #        {:describe, meta, [name, _body]} = ast,
  #        state = %{scopes: [atom | _]}
  #      )
  #      when is_binary(name) and is_atom(atom) and atom != nil do
  #   line = Keyword.fetch!(meta, :line)
  #   column = Keyword.fetch!(meta, :column)

  #   state =
  #     state
  #     |> add_call_to_line({nil, :describe, 2}, {line, column})

  #   %{state | context: Map.put(state.context, :ex_unit_describe, name)}
  #   # |> add_current_env_to_line(line)
  #   # |> result(ast)
  # end

  # # ex_unit not implemented test
  # defp pre(
  #        {:test, meta, [name]},
  #        state = %{scopes: [atom | _]}
  #      )
  #      when is_binary(name) and is_atom(atom) and atom != nil do
  #   def_name = ex_unit_test_name(state, name)
  #   line = Keyword.fetch!(meta, :line)
  #   column = Keyword.fetch!(meta, :column)

  #   _ast_without_params = {:def, meta, [{def_name, add_no_call([]), []}, [], []]}

  #   _state =
  #     state
  #     |> add_call_to_line({nil, :test, 0}, {line, column})

  #   # pre_func(ast_without_params, state, meta, def_name, [{:_, [line: line, column: column], nil}])
  # end

  # # ex_unit test without context
  # defp pre(
  #        {:test, meta, [name, body]},
  #        state = %{scopes: [atom | _]}
  #      )
  #      when is_binary(name) and is_atom(atom) and atom != nil do
  #   def_name = ex_unit_test_name(state, name)
  #   line = Keyword.fetch!(meta, :line)
  #   column = Keyword.fetch!(meta, :column)

  #   _ast_without_params = {:def, meta, [{def_name, add_no_call([]), []}, [], body]}

  #   _state =
  #     state
  #     |> add_call_to_line({nil, :test, 2}, {line, column})

  #   # pre_func(ast_without_params, state, meta, def_name, [{:_, [line: line, column: column], nil}])
  # end

  # # ex_unit test with context
  # defp pre(
  #        {:test, meta, [name, _param, body]},
  #        state = %{scopes: [atom | _]}
  #      )
  #      when is_binary(name) and is_atom(atom) and atom != nil do
  #   def_name = ex_unit_test_name(state, name)
  #   line = Keyword.fetch!(meta, :line)
  #   column = Keyword.fetch!(meta, :column)

  #   _ast_without_params = {:def, meta, [{def_name, add_no_call([]), []}, [], body]}

  #   _state =
  #     state
  #     |> add_call_to_line({nil, :test, 3}, {line, column})

  #   # pre_func(ast_without_params, state, meta, def_name, [param])
  # end

  # # ex_unit setup with context
  # defp pre(
  #        {setup, meta, [_param, body]},
  #        state = %{scopes: [atom | _]}
  #      )
  #      when setup in [:setup, :setup_all] and is_atom(atom) and atom != nil do
  #   line = Keyword.fetch!(meta, :line)
  #   column = Keyword.fetch!(meta, :column)

  #   # NOTE this name is not 100% correct - ex_unit uses counters instead of line but it's too complicated
  #   def_name = :"__ex_unit_#{setup}_#{line}"
  #   _ast_without_params = {:def, meta, [{def_name, add_no_call([]), []}, [], body]}

  #   _state =
  #     state
  #     |> add_call_to_line({nil, setup, 2}, {line, column})

  #   # pre_func(ast_without_params, state, meta, def_name, [param])
  # end

  # # ex_unit setup without context
  # defp pre(
  #        {setup, meta, [body]},
  #        state = %{scopes: [atom | _]}
  #      )
  #      when setup in [:setup, :setup_all] and is_atom(atom) and atom != nil do
  #   line = Keyword.fetch!(meta, :line)
  #   column = Keyword.fetch!(meta, :column)

  #   # NOTE this name is not 100% correct - ex_unit uses counters instead of line but it's too complicated
  #   def_name = :"__ex_unit_#{setup}_#{line}"
  #   _ast_without_params = {:def, meta, [{def_name, add_no_call([]), []}, [], body]}

  #   state =
  #     state
  #     |> add_call_to_line({nil, setup, 2}, {line, column})

  #   # pre_func(ast_without_params, state, meta, def_name, [{:_, [line: line, column: column], nil}])
  # end

  # # incomplete spec
  # # @callback my(integer)
  # defp pre(
  #        {:@, _meta_attr, [{kind, _meta_kind, [{name, _meta_name, type_args}]} = _spec]},
  #        _state
  #      )
  #      when kind in [:spec, :callback, :macrocallback] and is_atom(name) and
  #             (is_nil(type_args) or is_list(type_args)) do
  #   # pre_spec(
  #   #   {:@, meta_attr, [{kind, add_no_call(meta_kind), [{name, meta_name, type_args}]}]},
  #   #   state,
  #   #   meta_attr,
  #   #   name,
  #   #   expand_aliases_in_ast(state, List.wrap(type_args)),
  #   #   expand_aliases_in_ast(state, spec),
  #   #   kind
  #   # )
  # end

  # defp pre({:when, _meta, [lhs, _rhs]}, state) do
  #   _vars = find_typed_vars(lhs, nil)

  #   state
  #   # |> add_vars(vars, true)
  #   # |> result({:when, meta, [:_, rhs]})
  # end

  # defp pre(
  #        {:case, meta,
  #         [
  #           condition_ast,
  #           [
  #             do: _clauses
  #           ]
  #         ]} = _ast,
  #        state
  #      ) do
  #   line = Keyword.fetch!(meta, :line)
  #   column = Keyword.fetch!(meta, :column)

  #   state
  #   |> push_binding_context(type_of(condition_ast))
  #   |> add_call_to_line({nil, :case, 2}, {line, column})
  #   # |> add_current_env_to_line(line)
  #   # |> result(ast)
  # end

  # # Any other tuple with a line
  # defp pre({_, meta, _} = ast, state) do
  #   case Keyword.get(meta, :line) do
  #     nil ->
  #       {ast, state}

  #     _line ->
  #       state
  #       # |> add_current_env_to_line(line)
  #       # |> result(ast)
  #   end
  # end

  # defp pre(ast, state) do
  #   {ast, state}
  # end

  # # ex_unit describe
  # defp post(
  #        {:describe, _meta, [name, _body]} = _ast,
  #        state
  #      )
  #      when is_binary(name) do
  #   %{state | context: Map.delete(state.context, :ex_unit_describe)}
  #   # |> result(ast)
  # end

  # defp post({atom, meta, [lhs, rhs]} = _ast, state)
  #      when atom in [:=, :<-] do
  #   _line = Keyword.fetch!(meta, :line)
  #   match_context_r = type_of(rhs)

  #   match_context_r =
  #     if atom == :<- and match?([:for | _], state.binding_context) do
  #       {:for_expression, match_context_r}
  #     else
  #       match_context_r
  #     end

  #   vars_l = find_typed_vars(lhs, match_context_r)

  #   _vars =
  #     case rhs do
  #       {:=, _, [nested_lhs, _nested_rhs]} ->
  #         match_context_l = type_of(lhs)
  #         nested_vars = find_typed_vars(nested_lhs, match_context_l)

  #         vars_l ++ nested_vars

  #       _ ->
  #         vars_l
  #     end

  #   state
  #   # |> remove_calls(remove_positions)
  #   # |> add_current_env_to_line(line)
  #   # |> result(ast)
  # end

  # # String literal
  # defp post({_, [no_call: true, line: line, column: _column], [str]} = ast, state)
  #      when is_binary(str) do
  #   post_string_literal(ast, state, line, str)
  # end

  # # String literal in sigils
  # defp post({:<<>>, [indentation: _, line: line, column: _column], [str]} = ast, state)
  #      when is_binary(str) do
  #   post_string_literal(ast, state, line, str)
  # end

  # defp post(ast, state) do
  #   {ast, state}
  # end

  # # defp find_typed_vars(state, ast, match_context \\ nil)

  # # defp find_typed_vars(_state, {var, _meta, nil}, _)
  # #      when var in [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__] do
  # #   # TODO local calls?
  # #   []
  # # end

  # # defp find_typed_vars(_state, {var, meta, nil}, :rescue) when is_atom(var) do
  # #   line = Keyword.fetch!(meta, :line)
  # #   column = Keyword.fetch!(meta, :column)
  # #   match_context = {:struct, [], {:atom, Exception}, nil}
  # #   [%VarInfo{name: var, positions: [{line, column}], type: match_context, is_definition: true}]
  # # end

  # def infer_type_from_guards(guard_ast, vars, _state) do
  #   type_info = Guard.type_information_from_guards(guard_ast)

  #   Enum.reduce(type_info, vars, fn {var, type}, acc ->
  #     index = Enum.find_index(acc, &(&1.name == var))

  #     if index,
  #       do: List.update_at(acc, index, &Map.put(&1, :type, type)),
  #       else: acc
  #   end)
  # end

  # defp add_no_call(meta) do
  #   [{:no_call, true} | meta]
  # end

  # defp ex_unit_test_name(state, name) do
  #   case state.context[:ex_unit_describe] do
  #     nil -> "test #{name}"
  #     describe -> "test #{describe} #{name}"
  #   end
  #   |> String.to_atom()
  # end
end
