defmodule ElixirSense.Core.Guard do
  @moduledoc """
  This module is responsible for infer type information from guard expressions
  """

  # A guard expression can be in either these form:
  #        :and                          :or
  #      /     \            or          /   \              or  :not guard_expr or   guard_expr or list(guard_expr)
  # guard_expr  guard_expr        guard_expr  guard_expr
  #
  def type_information_from_guards(list) when is_list(list) do
    for expr <- list, reduce: %{} do
      acc ->
        right = type_information_from_guards(expr)
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

  def type_information_from_guards({{:., _, [:erlang, :not]}, _, [guard_l]}) do
    left = type_information_from_guards(guard_l)
    for {k, _v} <- left, into: %{}, do: {k, nil}
  end

  def type_information_from_guards({{:., _, [:erlang, :andalso]}, _, [guard_l, guard_r]}) do
    left = type_information_from_guards(guard_l)
    right = type_information_from_guards(guard_r)

    Map.merge(left, right, fn _k, v1, v2 -> {:intersection, [v1, v2]} end)
  end

  def type_information_from_guards({{:., _, [:erlang, :orelse]}, _, [guard_l, guard_r]}) do
    left = type_information_from_guards(guard_l)
    right = type_information_from_guards(guard_r)

    Map.merge(left, right, fn _k, v1, v2 ->
      case {v1, v2} do
        {{:union, types_1}, {:union, types_2}} -> {:union, types_1 ++ types_2}
        {{:union, types}, _} -> {:union, types ++ [v2]}
        {_, {:union, types}} -> {:union, [v1 | types]}
        _ -> {:union, [v1, v2]}
      end
    end)
  end

  def type_information_from_guards(guard_ast) do
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

        {{:., _dot_meta, [:erlang, fun]}, _call_meta, params}, acc when is_atom(fun) and is_list(params) ->
          with {type, binding} <- guard_predicate_type(fun, params),
            {var, meta, context} when is_atom(var) and is_atom(context) <- binding,
            {:ok, version} <- Keyword.fetch(meta, :version) do
              # If we found the predicate type, we can prematurely exit traversing the subtree
              {[], Map.put(acc, {var, version}, type)}
          else
            _ ->
              # traverse params
              {params, acc}
          end

        {{:., _dot_meta, [_remote, _fun]}, _call_meta, params}, acc ->
          # not expanded remote or fun - traverse params
          {params, acc}

        node, acc ->
          {node, acc}
      end)

    acc
  end

  # TODO div and rem only work on first arg
  defp guard_predicate_type(p, [first | _])
       when p in [:is_number, :is_float, :is_integer, :round, :trunc, :div, :rem, :abs, :ceil, :floor],
       do: {:number, first}

  defp guard_predicate_type(p, [first | _]) when p in [:is_binary, :binary_part],
    do: {:binary, first}

  defp guard_predicate_type(p, [first | _]) when p in [:is_bitstring, :bit_size, :byte_size],
    do: {:bitstring, first}

  defp guard_predicate_type(p, [first | _]) when p in [:is_list, :length], do: {:list, first}

  defp guard_predicate_type(p, [first | _]) when p in [:hd, :tl],
    do: {{:list, :boolean}, first}

  # when hd(x) == 1
  # when tl(x) == [2]
  defp guard_predicate_type(p, [{{:., _, [:erlang, guard]}, _, [first | _]}, rhs])
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

    rhs_type = if guard == :hd and rhs_type, do: {:list, rhs_type}, else: :list

    {rhs_type, first}
  end
  defp guard_predicate_type(p, [lhs, {{:., _, [:erlang, guard]}, _, _guard_params} = call])
  when p in [:==, :===, :>=, :>, :<=, :<] and guard in [:hd, :tl] do
    guard_predicate_type(p, [call, lhs])
  end

  defp guard_predicate_type(p, [first | _]) when p in [:is_tuple],
    do: {:tuple, first}

  defp guard_predicate_type(p, [_, second | _]) when p in [:element],
    do: {:tuple, second}

  # when tuple_size(x) == 1
  # when tuple_size(x) == 2
  defp guard_predicate_type(p, [{{:., _, [:erlang, :tuple_size]}, _, [first | _]}, size])
       when p in [:==, :===, :>=, :>, :<=, :<] do
    type =
      if is_integer(size) and p in [:==, :===] do
        {:tuple, size, if(size > 0, do: Enum.map(1..size, fn _ -> nil end), else: [])}
      else
        :tuple
      end

    {type, first}
  end

  defp guard_predicate_type(p, [size, {{:., _, [:erlang, :tuple_size]}, _, _guard_params} = call])
       when p in [:==, :===, :>=, :>, :<=, :<] do
    guard_predicate_type(p, [call, size])
  end

  defp guard_predicate_type(p, [{{:., _, [:erlang, :map_get]}, _, [key, second | _]}, value])
       when p in [:==, :===] do
    type = cond do
      key == :__struct__ and is_atom(value) ->
        {:struct, [], {:atom, value}, nil}
      key == :__struct__ ->
        {:struct, [], nil, nil}
      is_atom(key) or is_binary(key) ->
        # TODO other types of keys?
        rhs_type =
          cond do
            is_number(value) -> {:number, value}
            is_binary(value) -> :binary
            is_bitstring(value) -> :bitstring
            is_atom(value) -> {:atom, value}
            is_boolean(value) -> :boolean
            true -> nil
          end
        {:map, [{key, rhs_type}], nil}
      end

    {type, second}
  end

  defp guard_predicate_type(p, [value, {{:., _, [:erlang, :map_get]}, _, _guard_params} = call])
       when p in [:==, :===] do
    guard_predicate_type(p, [call, value])
  end

  defp guard_predicate_type(:is_map, [first | _]), do: {{:map, [], nil}, first}
  defp guard_predicate_type(:is_non_struct_map, [first | _]), do: {{:map, [], nil}, first}
  defp guard_predicate_type(:map_size, [first | _]), do: {{:map, [], nil}, first}

  # TODO macro
  defp guard_predicate_type(:is_map_key, [key, var | _]) do
    # TODO other types of keys?
    type =
    case key do
      :__struct__ -> {:struct, [], nil, nil}
      key when is_atom(key) -> {:map, [{key, nil}], nil}
      key when is_binary(key) -> {:map, [{key, nil}], nil}
      _ -> {:map, [], nil}
    end

    {type, var}
  end

  defp guard_predicate_type(:map_get, [key, var | _]) do
    # TODO other types of keys?
    type =
      case key do
        :__struct__ -> {:struct, [], nil, nil}
        key when is_atom(key) -> {:map, [{key, nil}], nil}
        key when is_binary(key) -> {:map, [{key, nil}], nil}
        _ -> {:map, [], nil}
      end

    {type, var}
  end

  defp guard_predicate_type(:is_atom, [first | _]), do: {:atom, first}
  defp guard_predicate_type(:is_boolean, [first | _]), do: {:boolean, first}

  defp guard_predicate_type(_, _), do: nil
end

# :in :is_function/1-2 :is_nil :is_pid :is_port :is_reference :node/0-1
# :self
