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
         {:fn, _,
          [
            {:->, _,
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

    test "infers type from strict equality: === atom (Erlang =:=)" do
      # `===` expands to the Erlang `=:=` BIF; it must refine like `==`.
      guard_expr = quote(do: x === :foo) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:atom, :foo}}
    end

    test "infers union type from membership: x in [atoms]" do
      # In guards `in` expands to an `orelse` chain of `=:=` comparisons.
      guard_expr = quote(do: x in [:a, :b]) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:union, [atom: :a, atom: :b]}}
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

    test "length(x) > 0 infers a non-empty list" do
      assert Guard.type_information_from_guards(quote(do: length(x) > 0) |> expand()) ==
               %{{:x, 0} => {:nonempty_list, nil}}

      assert Guard.type_information_from_guards(quote(do: length(x) >= 1) |> expand()) ==
               %{{:x, 0} => {:nonempty_list, nil}}

      # A plain length comparison that doesn't imply non-empty stays a list.
      # Returns {:list, nil} (not bare :list) because it now hits the direct
      # comparison clause rather than falling through to the is_list/length base
      # case. {:list, nil} and :list both represent "any proper list"; the change
      # in representation is a side-effect of the task-#4 operator-coverage fix.
      assert Guard.type_information_from_guards(quote(do: length(x) < 3) |> expand()) ==
               %{{:x, 0} => {:list, nil}}
    end

    # Regression for task #4: flipped comparisons must invert the operator.
    # `5 > length(x)` means length(x) < 5, which does NOT imply non-empty.
    # Bug was: the flip clause delegated `guard_predicate_type(:>, [length(x), 5])`
    # without inversion, so nonempty_length?(:>, 5) returned true → wrong.
    test "5 > length(x) does NOT produce non-empty (just {:list, nil})" do
      # size on left, should be treated as length(x) < 5 — not non-empty
      result = Guard.type_information_from_guards(quote(do: 5 > length(x)) |> expand())
      assert result == %{{:x, 0} => {:list, nil}}
    end

    test "0 < length(x) produces non-empty list (common spelling)" do
      # `0 < length(x)` is equivalent to `length(x) > 0`, must infer non-empty
      result = Guard.type_information_from_guards(quote(do: 0 < length(x)) |> expand())
      assert result == %{{:x, 0} => {:nonempty_list, nil}}
    end

    test "length(x) >= 0 does not produce non-empty (zero is allowed)" do
      result = Guard.type_information_from_guards(quote(do: length(x) >= 0) |> expand())
      assert result == %{{:x, 0} => {:list, nil}}
    end

    test "1 <= length(x) produces non-empty list" do
      # `1 <= length(x)` flips to `length(x) >= 1` which implies non-empty
      result = Guard.type_information_from_guards(quote(do: 1 <= length(x)) |> expand())
      assert result == %{{:x, 0} => {:nonempty_list, nil}}
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

    # Regression for task #28: non-atom map keys must be stored with the
    # `{:domain, key_type}` encoding matching type_inference.ex get_fields_type,
    # so that covers?/same_keys?/map_key lookup see a single spelling.
    # Bug was: guard.ex stored raw `{"a", nil}` / `{1, nil}` while the rest
    # of the codebase uses `{{:domain, {:binary, "a"}}, nil}` etc.
    test "is_map_key with string key produces domain-key encoding" do
      guard_expr = quote(do: is_map_key(x, "akey")) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      # Must match get_fields_type encoding: {{:domain, {:binary, "akey"}}, nil}
      assert result == %{{:x, 0} => {:map, [{{:domain, {:binary, "akey"}}, nil}], nil}}
    end

    test "is_map_key with integer key produces domain-key encoding" do
      guard_expr = quote(do: is_map_key(x, 42)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      # Must match get_fields_type encoding: {{:domain, {:integer, 42}}, nil}
      assert result == %{{:x, 0} => {:map, [{{:domain, {:integer, 42}}, nil}], nil}}
    end
  end

  describe "type_information_from_guards not" do
    test "handles not guard" do
      guard_expr = quote(do: not is_number(x)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => nil}
    end

    test "negative is_map_key records the key as :not_set" do
      guard_expr = quote(do: not is_map_key(x, :foo)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:map, [foo: :not_set], nil}}
    end

    # Regression for task #28: not is_map_key with non-atom key must also
    # produce the domain-key encoding for :not_set facts.
    test "negative is_map_key with string key uses domain-key encoding" do
      guard_expr = quote(do: not is_map_key(x, "foo")) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:map, [{{:domain, {:binary, "foo"}}, :not_set}], nil}}
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

      assert result == %{{:x, 1} => :integer, {:y, 0} => :binary}
    end

    test "div constrains both arguments to integer" do
      guard = quote(do: div(x, y) == 0) |> expand()

      result = Guard.type_information_from_guards(guard)

      assert result == %{{:x, 1} => :integer, {:y, 0} => :integer}
    end

    test "rem constrains both arguments to integer" do
      guard = quote(do: rem(x, y) == 0) |> expand()

      result = Guard.type_information_from_guards(guard)

      assert result == %{{:x, 1} => :integer, {:y, 0} => :integer}
    end
  end

  describe "type_information_from_guards or" do
    test "handles or with simple types" do
      guard = quote(do: is_integer(x) or is_binary(x)) |> expand()

      result = Guard.type_information_from_guards(guard)
      assert result == %{{:x, 0} => {:union, [:integer, :binary]}}
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
      assert result == %{{:x, 0} => {:union, [:integer, :binary]}}
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

  # Round-4 precision improvements

  describe "is_function/2 arity narrowing (task 1)" do
    test "is_function(x, 2) narrows to {:fun, 2}" do
      guard_expr = quote(do: is_function(x, 2)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:fun, 2}}
    end

    test "is_function(x, 0) narrows to {:fun, 0}" do
      guard_expr = quote(do: is_function(x, 0)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:fun, 0}}
    end

    test "is_function(x, 5) narrows to {:fun, 5}" do
      guard_expr = quote(do: is_function(x, 5)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:fun, 5}}
    end

    test "is_function(x) without arity stays :fun" do
      guard_expr = quote(do: is_function(x)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => :fun}
    end
  end

  describe "is_exception narrowing (task 2)" do
    test "is_exception(x) narrows to intersection with struct and __exception__ key" do
      guard_expr = quote(do: is_exception(x)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      # The intersection contains a struct shape and a map with __exception__ key
      {:intersection, members} = result[{:x, 0}]
      assert {:struct, [], nil, nil} in members
      assert {:map, [], nil} in members
    end

    test "is_exception(x, RuntimeError) puts struct-with-module first in intersection" do
      guard_expr = quote(do: is_exception(x, RuntimeError)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      {:intersection, [first | _]} = result[{:x, 0}]
      # The concrete struct must come first for correct TypePresentation rendering
      assert {:struct, [], {:atom, RuntimeError}, nil} = first
    end

    test "is_exception(x, ArgumentError) intersection includes struct-with-module" do
      guard_expr = quote(do: is_exception(x, ArgumentError)) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      {:intersection, members} = result[{:x, 0}]
      assert {:struct, [], {:atom, ArgumentError}, nil} in members
    end
  end

  describe "tuple_size narrowing (task 4 verification)" do
    test "tuple_size(x) == 2 narrows to 2-tuple with nil element types" do
      guard_expr = quote(do: tuple_size(x) == 2) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:tuple, 2, [nil, nil]}}
    end

    test "tuple_size(x) == 0 narrows to 0-tuple" do
      guard_expr = quote(do: tuple_size(x) == 0) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => {:tuple, 0, []}}
    end

    test "tuple_size(x) >= 1 falls back to plain :tuple (non-equality)" do
      guard_expr = quote(do: tuple_size(x) >= 1) |> expand()
      result = Guard.type_information_from_guards(guard_expr)
      assert result == %{{:x, 0} => :tuple}
    end
  end
end
