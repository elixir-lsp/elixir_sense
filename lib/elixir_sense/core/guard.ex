defmodule ElixirSense.Core.Guard do
  @moduledoc """
  This module is responsible for infer type information from guard expressions
  """

  import ElixirSense.Core.State

  alias ElixirSense.Core.TypeInference

  # A guard expression can be in either these form:
  #        :and                          :or
  #      /     \            or          /   \              or      guard_expr
  # guard_expr  guard_expr        guard_expr  guard_expr
  #
  # type information from :and subtrees are mergeable
  # type information from :or subtrees are discarded
  def type_information_from_guards(list, state) when is_list(list) do
    for expr <- list, reduce: %{} do
      acc ->
        right = type_information_from_guards(expr, state)
        Map.merge(acc, right, fn _k, v1, v2 ->
          case {v1, v2} do
            {{:union, types_1}, {:union, types_2}} -> {:union, types_1 ++ types_2}
            {{:union, types}, _} -> {:union, types ++ [v2]}
            {_, {:union, types}} -> {:union, [v1 | types]}
            _ -> {:union, [v1, v2]}
          end
        end)
    end
  end
  def type_information_from_guards({{:., _, [:erlang, :andalso]}, _, [guard_l, guard_r]}, state) do
    left = type_information_from_guards(guard_l, state)
    right = type_information_from_guards(guard_r, state)

    Map.merge(left, right, fn _k, v1, v2 -> {:intersection, [v1, v2]} end)
  end
  # TODO remove?
  def type_information_from_guards({:and, _, [guard_l, guard_r]}, state) do
    left = type_information_from_guards(guard_l, state)
    right = type_information_from_guards(guard_r, state)

    Keyword.merge(left, right, fn _k, v1, v2 -> {:intersection, [v1, v2]} end)
  end

  def type_information_from_guards({{:., _, [:erlang, :orelse]}, _, [guard_l, guard_r]}, state) do
    left = type_information_from_guards(guard_l, state)
    right = type_information_from_guards(guard_r, state)

    Map.merge(left, right, fn _k, v1, v2 ->
      case {v1, v2} do
        {{:union, types_1}, {:union, types_2}} -> {:union, types_1 ++ types_2}
        {{:union, types}, _} -> {:union, types ++ [v2]}
        {_, {:union, types}} -> {:union, [v1 | types]}
        _ -> {:union, [v1, v2]}
      end
    end)
  end

  # TODO remove
  def type_information_from_guards({:or, _, [guard_l, guard_r]}, state) do
    left = type_information_from_guards(guard_l, state)
    right = type_information_from_guards(guard_r, state)

    Keyword.merge(left, right, fn _k, v1, v2 ->
      case {v1, v2} do
        {{:union, types_1}, {:union, types_2}} -> {:union, types_1 ++ types_2}
        {{:union, types}, _} -> {:union, types ++ [v2]}
        {_, {:union, types}} -> {:union, [v1 | types]}
        _ -> {:union, [v1, v2]}
      end
    end)
  end

  def type_information_from_guards(guard_ast, state) do
    {_, acc} =
      Macro.prewalk(guard_ast, %{}, fn
        # Standalone variable: func my_func(x) when x
        {var, meta, context} = node, acc when is_atom(var) and is_atom(context) ->
          case Keyword.fetch(meta, :version) do
            {:ok, version} ->
              {node, Map.put(acc, {var, version}, :boolean)}
            _ ->
              {node, acc}
          end

        {{:., _dot_meta, [:erlang, fun]}, _call_meta, params} = node, acc when is_atom(fun) ->
          with {type, binding} <- guard_predicate_type(fun, params, state),
            {var, meta, context} when is_atom(var) and is_atom(context) <- binding,
            {:ok, version} <- Keyword.fetch(meta, :version) do
              # If we found the predicate type, we can prematurely exit traversing the subtree
              {[], Map.put(acc, {var, version}, type)}
          else
            _ -> {node, acc}
          end

        # TODO can we drop this clause?
        {guard_predicate, _, params} = node, acc ->
          with {type, binding} <- guard_predicate_type(guard_predicate, params, state),
            {var, meta, context} when is_atom(var) and is_atom(context) <- binding,
            {:ok, version} <- Keyword.fetch(meta, :version) do
              # If we found the predicate type, we can prematurely exit traversing the subtree
              {[], Map.put(acc, {var, version}, type)}

          else
            _ -> {node, acc}
          end

        node, acc ->
          {node, acc}
      end)

    acc
  end

  defp guard_predicate_type(p, params, _)
       when p in [:is_number, :is_float, :is_integer, :round, :trunc, :div, :rem, :abs],
       do: {:number, hd(params)}

  defp guard_predicate_type(p, params, _) when p in [:is_binary, :binary_part],
    do: {:binary, hd(params)}

  defp guard_predicate_type(p, params, _) when p in [:is_bitstring, :bit_size, :byte_size],
    do: {:bitstring, hd(params)}

  defp guard_predicate_type(p, params, _) when p in [:is_list, :length], do: {:list, hd(params)}

  defp guard_predicate_type(p, params, _) when p in [:hd, :tl],
    do: {{:list, :boolean}, hd(params)}

  # when hd(x) == 1
  # when tl(x) <= 2
  defp guard_predicate_type(p, [{guard, _, guard_params}, rhs], _)
       when p in [:==, :===, :>=, :>, :<=, :<] and guard in [:hd, :tl] do
    rhs_type =
      cond do
        is_number(rhs) -> :number
        is_binary(rhs) -> :binary
        is_bitstring(rhs) -> :bitstring
        is_atom(rhs) -> :atom
        is_boolean(rhs) -> :boolean
        true -> nil
      end

    rhs_type = if rhs_type, do: {:list, rhs_type}, else: :list

    {rhs_type, hd(guard_params)}
  end

  defp guard_predicate_type(p, params, _) when p in [:is_tuple, :elem],
    do: {:tuple, hd(params)}

  # when tuple_size(x) == 1
  # when tuple_size(x) == 2
  defp guard_predicate_type(p, [{:tuple_size, _, guard_params}, size], _)
       when p in [:==, :===] do
    type =
      if is_integer(size) do
        {:tuple, size, if(size > 0, do: Enum.map(1..size, fn _ -> nil end), else: [])}
      else
        :tuple
      end

    {type, hd(guard_params)}
  end

  defp guard_predicate_type(:is_map, params, _), do: {{:map, [], nil}, hd(params)}
  defp guard_predicate_type(:map_size, params, _), do: {{:map, [], nil}, hd(params)}

  defp guard_predicate_type(:is_map_key, [var, key], state) do
    type =
      case TypeInference.get_binding_type(state, key) do
        {:atom, key} -> {:map, [{key, nil}], nil}
        nil when is_binary(key) -> {:map, [{key, nil}], nil}
        _ -> {:map, [], nil}
      end

    {type, var}
  end

  defp guard_predicate_type(:is_atom, params, _), do: {:atom, hd(params)}
  defp guard_predicate_type(:is_boolean, params, _), do: {:boolean, hd(params)}

  defp guard_predicate_type(:is_struct, [var, {:__aliases__, _, _list} = module], state) do
    {module, _state, _env} = expand(module, state)
    type = {:struct, [], {:atom, module}, nil}
    {type, var}
  end

  defp guard_predicate_type(:is_struct, params, _), do: {{:struct, [], nil, nil}, hd(params)}
  defp guard_predicate_type(_, _, _), do: nil
end
