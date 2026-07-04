defmodule ElixirSense.Core.TypeInference.Guard do
  alias ElixirSense.Core.TypeInference

  @moduledoc """
  This module is responsible for infer type information from guard expressions
  """

  # A guard expression can be in either these form:
  #        :and                          :or
  #      /     \            or          /   \              or  :not guard_expr or   guard_expr or list(guard_expr)
  # guard_expr  guard_expr        guard_expr  guard_expr
  #
  def type_information_from_guards({:when, meta, [left, right]}) do
    # treat nested guard as or expression
    # this is not valid only in case of raising expressions in guard
    # but it doesn't matter in our case we are not evaluating
    type_information_from_guards({{:., meta, [:erlang, :orelse]}, meta, [left, right]})
  end

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

  # Negative `is_map_key`: the key is known to be ABSENT, recorded as
  # `:not_set` (so e.g. `x.key` would be a violation and completion won't
  # suggest the key). Erlang arg order is `is_map_key(key, map)`.
  #
  # Narrowing to a map is sound: in a guard, `is_map_key(k, non_map)` raises,
  # which makes `not is_map_key(k, non_map)` fail the guard — so a clause with
  # this as its (sole) guard matches only maps lacking the key (verified: a
  # non-map argument takes the no-match clause). Inside an `orelse`, the
  # handler below keeps a per-var fact only when both disjuncts constrain it
  # and then unions them, so the map fact never leaks unconditionally.
  def type_information_from_guards(
        {{:., _, [:erlang, :not]}, _, [{{:., _, [:erlang, :is_map_key]}, _, [key, var]}]}
      ) do
    case extract_var_type(var, not_set_map_type(key)) do
      nil -> %{}
      {var_key, type} -> %{var_key => type}
    end
  end

  def type_information_from_guards({{:., _, [:erlang, :not]}, _, [guard_l]}) do
    left = type_information_from_guards(guard_l)
    for {k, _v} <- left, into: %{}, do: {k, nil}
  end

  def type_information_from_guards({{:., _, [:erlang, :andalso]}, _, [guard_l, guard_r]}) do
    left = type_information_from_guards(guard_l)
    right = type_information_from_guards(guard_r)

    Map.merge(left, right, fn _k, v1, v2 ->
      v1 |> TypeInference.intersect(v2) |> sort_intersection()
    end)
  end

  def type_information_from_guards({{:., _, [:erlang, :orelse]}, _, [guard_l, guard_r]}) do
    left = type_information_from_guards(guard_l)
    right = type_information_from_guards(guard_r)

    merged_keys = (Map.keys(left) ++ Map.keys(right)) |> Enum.uniq()

    Enum.reduce(merged_keys, %{}, fn key, acc ->
      v1 = Map.get(left, key)
      v2 = Map.get(right, key)

      # we can union types only if both sides constrain the same variable
      # otherwise, it's not possible to infer type information from guard expression
      # e.g. is_integer(x) or is_atom(x) can be unionized
      # is_integer(x) or is_atom(y) cannot

      new_value =
        case {v1, v2} do
          {nil, nil} -> nil
          {nil, _} -> nil
          {_, nil} -> nil
          {{:union, types_1}, {:union, types_2}} -> {:union, types_1 ++ types_2}
          {{:union, types}, other} -> {:union, types ++ [other]}
          {other, {:union, types}} -> {:union, [other | types]}
          {other1, other2} -> {:union, [other1, other2]}
        end

      Map.put(acc, key, new_value)
    end)
  end

  # {{:., _, [target, key]}, _, []}
  def type_information_from_guards({{:., _, [target, key]}, _, []}) when is_atom(key) do
    case extract_var_type(target, {:map, [{key, {:atom, true}}], []}) do
      nil -> %{}
      {var, type} -> %{var => type}
    end
  end

  # Standalone variable: func my_func(x) when x
  def type_information_from_guards({var, meta, context}) when is_atom(var) and is_atom(context) do
    case Keyword.fetch(meta, :version) do
      {:ok, version} ->
        %{{var, version} => {:atom, true}}

      _ ->
        %{}
    end
  end

  def type_information_from_guards(guard_ast) do
    {_, acc} =
      Macro.prewalk(guard_ast, %{}, fn
        {{:., _dot_meta, [:erlang, fun0]}, _call_meta, params}, acc
        when is_atom(fun0) and is_list(params) ->
          fun = normalize_erlang_guard_fun(fun0)

          with {type, binding} <- guard_predicate_type(fun, params),
               {{var, version}, type} <- extract_var_type(binding, type) do
            # If we found the predicate type, we can prematurely exit traversing the subtree
            acc = Map.put(acc, {var, version}, type)

            # Apply secondary constraints (e.g., div/rem constrain both args to integer)
            acc = apply_secondary_constraints(fun, params, acc)

            {nil, acc}
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

  # Does `length(x) p size` imply x is non-empty?
  defp nonempty_length?(:>, n) when is_integer(n) and n >= 0, do: true
  defp nonempty_length?(:>=, n) when is_integer(n) and n >= 1, do: true
  defp nonempty_length?(p, n) when p in [:==, :===] and is_integer(n) and n >= 1, do: true
  defp nonempty_length?(p, 0) when p in [:!=, :!==], do: true
  defp nonempty_length?(_p, _n), do: false

  # Invert a comparison operator (for when operands are flipped).
  defp invert_comparison(:<), do: :>
  defp invert_comparison(:>), do: :<
  defp invert_comparison(:<=), do: :>=
  defp invert_comparison(:>=), do: :<=
  defp invert_comparison(p) when p in [:==, :===, :!=, :!==], do: p

  # A map known to NOT have `key` (from `not is_map_key/2`).
  # Atom keys are stored raw; non-atom keys use the `{:domain, key_type}` form
  # that matches type_inference.ex `get_fields_type` and binding.ex `{:map_key, ...}`.
  defp not_set_map_type(key) when is_atom(key),
    do: {:map, [{key, :not_set}], nil}

  defp not_set_map_type(key) when is_binary(key),
    do: {:map, [{{:domain, type_of(key)}, :not_set}], nil}

  defp not_set_map_type(key) when is_integer(key),
    do: {:map, [{{:domain, type_of(key)}, :not_set}], nil}

  defp not_set_map_type(_key), do: {:map, [], nil}

  defp extract_var_type({var, meta, context}, type) when is_atom(var) and is_atom(context) do
    case Keyword.fetch(meta, :version) do
      {:ok, version} ->
        {{var, version}, type}

      _ ->
        nil
    end
  end

  defp extract_var_type({{:., _, [target, key]}, _, []}, type) when is_atom(key) do
    extract_var_type(target, {:map, [{key, type}], []})
  end

  defp extract_var_type(_, _), do: nil

  # Guards are expanded to Erlang BIFs before reaching us, so comparison
  # operators arrive under their Erlang spellings (`=:=`, `=/=`, `=<`) rather
  # than the Elixir ones the clauses below match. Normalize the equality and
  # ordering operators so strict equality (`===`) and `<=` refine like `==`/`<`
  # do. This also makes `x in [...]` work, since in guards it expands to an
  # `orelse` chain of `=:=` comparisons.
  defp normalize_erlang_guard_fun(:"=:="), do: :===
  defp normalize_erlang_guard_fun(:"=/="), do: :!==
  defp normalize_erlang_guard_fun(:"=<"), do: :<=
  defp normalize_erlang_guard_fun(other), do: other

  # div and rem require integer args
  defp guard_predicate_type(p, [first | _]) when p in [:is_integer, :div, :rem],
    do: {:integer, first}

  defp guard_predicate_type(:is_float, [first | _]), do: {:float, first}

  # div/rem second arg constrained by apply_secondary_constraints/3
  defp guard_predicate_type(p, [first | _])
       when p in [
              :is_number,
              :round,
              :trunc,
              :abs,
              :ceil,
              :floor
            ],
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
    rhs_type = type_of(rhs)

    rhs_type = if guard == :hd and rhs_type != nil, do: {:list, rhs_type}, else: :list

    {rhs_type, first}
  end

  defp guard_predicate_type(p, [lhs, {{:., _, [:erlang, guard]}, _, _guard_params} = call])
       when p in [:==, :===, :>=, :>, :<=, :<] and guard in [:hd, :tl] do
    guard_predicate_type(p, [call, lhs])
  end

  # when length(x) > 0 / >= 1 / == n (n >= 1) / != 0 -> non-empty list
  # Also handles operators that do NOT imply non-emptiness (< / <= / != 0-like),
  # but in all cases length/1 already constrains the variable to a proper list.
  defp guard_predicate_type(p, [{{:., _, [:erlang, :length]}, _, [first | _]}, size])
       when p in [:>, :>=, :==, :===, :<, :<=, :!=, :!==] do
    type = if nonempty_length?(p, size), do: {:nonempty_list, nil}, else: {:list, nil}
    {type, first}
  end

  # When the size literal is on the LEFT (e.g. `5 > length(x)`), flip the
  # operand order AND invert the comparison operator so that the delegated call
  # sees `length(x) < 5` rather than `length(x) > 5`.
  defp guard_predicate_type(p, [size, {{:., _, [:erlang, :length]}, _, _guard_params} = call])
       when p in [:>, :>=, :<=, :<, :==, :===, :!=, :!==] do
    guard_predicate_type(invert_comparison(p), [call, size])
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
    type =
      cond do
        key == :__struct__ and is_atom(value) ->
          {:struct, [], {:atom, value}, nil}

        key == :__struct__ ->
          {:struct, [], nil, nil}

        is_atom(key) ->
          rhs_type = type_of(value)
          {:map, [{key, rhs_type}], nil}

        is_binary(key) or is_integer(key) ->
          rhs_type = type_of(value)
          {:map, [{{:domain, type_of(key)}, rhs_type}], nil}

        true ->
          {:map, [], nil}
      end

    {type, second}
  end

  defp guard_predicate_type(p, [value, {{:., _, [:erlang, :map_get]}, _, _guard_params} = call])
       when p in [:==, :===] do
    guard_predicate_type(p, [call, value])
  end

  defp guard_predicate_type(p, [{variable_l, _, context_l}, {variable_r, _, context_r}])
       when p in [:==, :===] and is_atom(variable_l) and is_atom(context_l) and
              is_atom(variable_r) and is_atom(context_r),
       do: nil

  defp guard_predicate_type(p, [{variable, _, context} = lhs, value])
       when p in [:==, :===] and is_atom(variable) and is_atom(context) do
    {type_of(value), lhs}
  end

  defp guard_predicate_type(p, [{{:., _, _}, _, []} = lhs, value]) when p in [:==, :===] do
    {type_of(value), lhs}
  end

  defp guard_predicate_type(p, [value, {variable, _, context} = rhs])
       when p in [:==, :===] and is_atom(variable) and is_atom(context) do
    guard_predicate_type(p, [rhs, value])
  end

  defp guard_predicate_type(p, [value, {{:., _, _}, _, []} = rhs]) when p in [:==, :===] do
    guard_predicate_type(p, [rhs, value])
  end

  defp guard_predicate_type(:is_map, [first | _]), do: {{:map, [], nil}, first}
  defp guard_predicate_type(:is_non_struct_map, [first | _]), do: {{:map, [], nil}, first}
  defp guard_predicate_type(:map_size, [first | _]), do: {{:map, [], nil}, first}

  defp guard_predicate_type(:is_map_key, [key, var | _]) do
    type =
      case key do
        :__struct__ ->
          {:struct, [], nil, nil}

        key when is_atom(key) ->
          {:map, [{key, nil}], nil}

        key when is_binary(key) or is_integer(key) ->
          {:map, [{{:domain, type_of(key)}, nil}], nil}

        _ ->
          {:map, [], nil}
      end

    {type, var}
  end

  defp guard_predicate_type(:map_get, [key, var | _]) do
    type =
      case key do
        :__struct__ ->
          {:struct, [], nil, nil}

        key when is_atom(key) ->
          {:map, [{key, nil}], nil}

        key when is_binary(key) or is_integer(key) ->
          {:map, [{{:domain, type_of(key)}, nil}], nil}

        _ ->
          {:map, [], nil}
      end

    {type, var}
  end

  defp guard_predicate_type(:is_atom, [first | _]), do: {:atom, first}
  defp guard_predicate_type(:is_boolean, [first | _]), do: {:boolean, first}
  defp guard_predicate_type(:is_pid, [first | _]), do: {:pid, first}
  defp guard_predicate_type(:is_port, [first | _]), do: {:port, first}
  defp guard_predicate_type(:is_reference, [first | _]), do: {:reference, first}
  # is_function(x, arity) with a literal arity narrows to {:fun, arity}
  defp guard_predicate_type(:is_function, [first, arity]) when is_integer(arity),
    do: {{:fun, arity}, first}

  defp guard_predicate_type(:is_function, [first | _]), do: {:fun, first}

  defp guard_predicate_type(_, _), do: nil

  # Apply constraints on additional arguments (beyond the first) for multi-arg guards
  defp apply_secondary_constraints(fun, [_, second | _], acc) when fun in [:div, :rem] do
    case extract_var_type(second, :integer) do
      {{var, version}, type} -> Map.put(acc, {var, version}, type)
      _ -> acc
    end
  end

  defp apply_secondary_constraints(_fun, _params, acc), do: acc

  # Reorder intersection members so that the most display-informative shape comes
  # first. TypePresentation.segment/1 picks the first struct-or-map? member when
  # rendering an intersection; by placing `{:struct, …, {:atom, Mod}, …}` before
  # plain map constraints we ensure `%RuntimeError{}` wins over `%{__exception__:
  # term()}` when both are in the same intersection (e.g. is_exception(x, Mod)).
  # An intersection is semantically commutative, so reordering is always sound.
  defp sort_intersection({:intersection, members}) do
    {concrete_structs, rest} =
      Enum.split_with(members, fn
        {:struct, _, {:atom, _}, _} -> true
        _ -> false
      end)

    {:intersection, concrete_structs ++ rest}
  end

  defp sort_intersection(other), do: other

  defp type_of(expression) do
    TypeInference.type_of(expression, :guard)
  end

  # TODO :in :node/0-1 :self
end
