defmodule ElixirSense.Core.TypeInference.GuardTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.TypeInference.Guard

  defp wrap(guard) do
    {_ast, vars} =
      Macro.prewalk(guard, [], fn
        {atom, _meta, var_context} = node, acc when is_atom(atom) and is_atom(var_context) ->
          {node, [node | acc]}

        node, acc ->
          {node, acc}
      end)

    vars =
      case Enum.uniq(vars) do
        [var] -> var
        list -> list
      end

    {:fn, [],
     [
       {:->, [],
        [
          [
            {:when, [],
             [
               vars,
               guard
             ]}
          ],
          :ok
        ]}
     ]}
  end

  defp unwrap(
         {:fn, [],
          [
            {:->, [],
             [
               [
                 {:when, _, [_, guard]}
               ],
               _
             ]}
          ]}
       ) do
    guard
  end

  defp expand(ast) do
    ast = wrap(ast)
    env = :elixir_env.new()
    {ast, _, _} = :elixir_expand.expand(ast, :elixir_env.env_to_ex(env), env)
    unwrap(ast)
  end

  describe "type_information_from_guards/1" do
    test "infers type from naked var" do
      guard_expr = quote(do: x) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:atom, true}}
    end

    # 1. Simple guards
    test "infers type from simple guard: is_number/1" do
      guard_expr = quote(do: is_number(x)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => :number}
    end

    test "infers type from simple guard: is_binary/1" do
      guard_expr = quote(do: is_binary(x)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => :binary}
    end

    test "infers type from simple guard: is_atom/1" do
      guard_expr = quote(do: is_atom(x)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => :atom}
    end

    test "infers type from simple guard: is_nil/1" do
      guard_expr = quote(do: is_nil(x)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:atom, nil}}
    end

    test "infers type from simple guard: == integer" do
      guard_expr = quote(do: x == 5) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:integer, 5}}
    end

    test "infers type from simple guard: == atom" do
      guard_expr = quote(do: x == :foo) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:atom, :foo}}
    end

    test "infers type from simple guard: == alias" do
      guard_expr = quote(do: x == Some.Mod) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:atom, Some.Mod}}
    end

    test "infers type from simple guard: == list empty" do
      guard_expr = quote(do: x == []) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:list, :empty}}
    end

    test "infers type from simple guard: == list" do
      guard_expr = quote(do: x == [1]) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:list, {:integer, 1}}}
    end

    test "infers type from simple guard: == map" do
      guard_expr = quote(do: x == %{a: :b}) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:map, [a: {:atom, :b}], nil}}
    end

    test "infers type from simple guard: == tuple empty" do
      guard_expr = quote(do: x == {}) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:tuple, 0, []}}
    end

    # 2. Guards with and
    test "infers type from guard with and: is_number/1 and is_atom/1" do
      guard_expr = quote(do: is_number(x) and is_atom(x)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:intersection, [:number, :atom]}}
    end

    # 3. Guards with or
    test "infers type from guard with or: is_number/1 or is_binary/1" do
      guard_expr = quote(do: is_number(x) or is_binary(x)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:union, [:number, :binary]}}
    end

    # 4. Guards with tuples
    test "infers type from guard with tuple: is_tuple/1" do
      guard_expr = quote(do: is_tuple(x)) |> expand
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => :tuple}
    end

    test "infers type from guard with tuple_size/1" do
      guard_expr = quote(do: tuple_size(x) == 2) |> expand
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:tuple, 2, [nil, nil]}}
    end

    # 5. Guards with lists
    test "infers type from guard with list: is_list/1" do
      guard_expr = quote(do: is_list(x)) |> expand
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => :list}
    end

    test "infers type from guard with list: hd/1 and tl/1" do
      guard_expr = quote(do: hd(x) == 1 and tl(x) == [2]) |> expand
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:intersection, [{:list, {:integer, 1}}, :list]}}
    end

    # 6. Guards with structs
    test "infers type from guard with struct: is_map/1 and map_get/2" do
      guard_expr = quote(do: is_struct(x, MyStruct)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)

      assert result == %{
               {:x, 0} => {
                 :intersection,
                 [
                   {:struct, [], {:atom, MyStruct}, nil},
                   {:map, [], nil},
                   {:struct, [], nil, nil}
                 ]
               }
             }
    end

    test "infers type from guard with struct: is_map_key/2" do
      guard_expr = quote(do: is_map_key(x, :key)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:map, [{:key, nil}], nil}}
    end
  end

  describe "type_information_from_guards not" do
    test "handles not guard" do
      guard_expr = quote(do: not is_number(x)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => nil}
    end

    # for simplicity we do not traverse not guards in the guard tree
    # this should return :number type
    test "handles nested not guards" do
      guard = quote(do: not not is_number(x)) |> expand()
      result = Guard.type_information_from_guards(guard)
      assert result == %{{:x, 0} => nil}
    end

    test "handles multiple variables in not guard" do
      guard = quote(do: not (is_integer(x) and is_atom(y))) |> expand()
      result = Guard.type_information_from_guards(guard)
      assert result == %{{:x, 1} => nil, {:y, 0} => nil}
    end
  end

  describe "type_information_from_guards and" do
    test "handles and with two guards" do
      guard = quote(do: is_number(x) and is_atom(x)) |> expand()

      result = Guard.type_information_from_guards(guard)

      assert result == %{
               {:x, 0} => {:intersection, [:number, :atom]}
             }
    end

    test "handles nested and guards" do
      guard = quote(do: is_number(x) and is_atom(x) and is_nil(x)) |> expand()

      result = Guard.type_information_from_guards(guard)

      assert result == %{{:x, 0} => {:intersection, [{:atom, nil}, :number, :atom]}}
    end

    test "handles and with different variables" do
      guard = quote(do: is_integer(x) and is_binary(y)) |> expand()

      result = Guard.type_information_from_guards(guard)

      assert result == %{{:x, 1} => :number, {:y, 0} => :binary}
    end
  end

  describe "type_information_from_guards or" do
    test "handles or with simple types" do
      guard = quote(do: is_integer(x) or is_binary(x)) |> expand()

      result = Guard.type_information_from_guards(guard)
      assert result == %{{:x, 0} => {:union, [:number, :binary]}}
    end

    test "handles nested or" do
      guard = quote(do: is_number(x) or is_atom(x) or is_nil(x)) |> expand()

      result = Guard.type_information_from_guards(guard)
      assert result == %{{:x, 0} => {:union, [:number, :atom, {:atom, nil}]}}
    end

    test "handles or with different variables" do
      guard = quote(do: is_integer(x) or is_binary(y)) |> expand()

      result = Guard.type_information_from_guards(guard)
      assert result == %{{:x, 1} => nil, {:y, 0} => nil}
    end

    test "handles or with existing unions" do
      guard = quote(do: is_number(x) or is_atom(x) or (is_nil(x) or x)) |> expand()

      result = Guard.type_information_from_guards(guard)
      assert result == %{{:x, 0} => {:union, [:number, :atom, {:atom, nil}, {:atom, true}]}}
    end

    test "handles nested when" do
      guard = quote(do: is_integer(x) when is_binary(x)) |> expand()

      result = Guard.type_information_from_guards(guard)
      assert result == %{{:x, 0} => {:union, [:number, :binary]}}
    end
  end

  describe "guard on map field" do
    test "naked" do
      guard = quote(do: x.foo) |> expand()

      result = Guard.type_information_from_guards(guard)
      assert result == %{{:x, 0} => {:map, [{:foo, {:atom, true}}], []}}
    end

    test "naked nested" do
      guard = quote(do: x.foo.bar) |> expand()

      result = Guard.type_information_from_guards(guard)
      assert result == %{{:x, 0} => {:map, [{:foo, {:map, [{:bar, {:atom, true}}], []}}], []}}
    end

    test "simple" do
      guard = quote(do: is_atom(x.foo)) |> expand()

      result = Guard.type_information_from_guards(guard)
      assert result == %{{:x, 0} => {:map, [{:foo, :atom}], []}}
    end

    test "nested" do
      guard = quote(do: is_atom(x.foo.bar.baz)) |> expand()

      result = Guard.type_information_from_guards(guard)

      assert result == %{
               {:x, 0} => {:map, [{:foo, {:map, [{:bar, {:map, [{:baz, :atom}], []}}], []}}], []}
             }
    end

    test "with operator" do
      guard = quote(do: x.foo == 1) |> expand()

      result = Guard.type_information_from_guards(guard)
      assert result == %{{:x, 0} => {:map, [{:foo, {:integer, 1}}], []}}
    end
  end
end
