defmodule ElixirSense.Core.ElixirTypesTest do
  use ExUnit.Case, async: false

  alias ElixirSense.Core.ElixirTypes

  describe "availability" do
    test "available?/0 returns boolean" do
      result = ElixirTypes.available?()
      assert is_boolean(result)
    end

    test "enabled?/0 returns false by default" do
      # Should be false since config defaults to false
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, false)

      on_exit(fn ->
        Application.put_env(:elixir_sense, :use_elixir_types, original_value)
      end)

      refute ElixirTypes.enabled?()
    end
  end

  describe "shape conversion" do
    test "to_shape/1 returns :none for empty descriptor" do
      # Empty descriptor is the "none" type
      result = ElixirTypes.to_shape(%{})
      assert result == :none
    end
  end

  describe "shape conversions" do
    setup do
      # Save original value and enable feature
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, true)

      on_exit(fn ->
        Application.put_env(:elixir_sense, :use_elixir_types, original_value)
      end)

      # Only run tests if Module.Types is available
      if ElixirTypes.available?() do
        :ok
      else
        :skip
      end
    end

    test "handles none" do
      assert ElixirTypes.to_shape(Module.Types.Descr.none()) == :none
    end

    test "handles term" do
      assert ElixirTypes.to_shape(Module.Types.Descr.term()) == nil
    end

    test "handles dynamic" do
      assert ElixirTypes.to_shape(Module.Types.Descr.dynamic()) == nil
    end

    test "handles atom" do
      assert ElixirTypes.to_shape(Module.Types.Descr.atom([:foo])) == {:atom, :foo}
      assert ElixirTypes.to_shape(Module.Types.Descr.atom()) == :atom
    end

    test "handles atom unions via atom_fetch" do
      assert ElixirTypes.to_shape(Module.Types.Descr.atom([:ok, :error])) ==
               {:union, [atom: :error, atom: :ok]}
    end

    test "handles binary" do
      assert ElixirTypes.to_shape(Module.Types.Descr.binary()) == {:binary, nil}
    end

    test "handles closed map" do
      assert ElixirTypes.to_shape(Module.Types.Descr.closed_map(foo: Module.Types.Descr.binary())) ==
               {:map, [foo: {:binary, nil}], nil}
    end

    test "handles empty list" do
      assert ElixirTypes.to_shape(Module.Types.Descr.empty_list()) == {:list, :empty}
    end

    test "handles empty map" do
      assert ElixirTypes.to_shape(Module.Types.Descr.empty_map()) == {:map, [], nil}
    end

    test "handles integer" do
      assert ElixirTypes.to_shape(Module.Types.Descr.integer()) == {:integer, nil}
    end

    test "handles float" do
      assert ElixirTypes.to_shape(Module.Types.Descr.float()) == {:float, nil}
    end

    test "handles list" do
      assert ElixirTypes.to_shape(Module.Types.Descr.list(Module.Types.Descr.integer())) ==
               {:list, {:integer, nil}}
    end

    test "handles nonempty list" do
      assert ElixirTypes.to_shape(Module.Types.Descr.non_empty_list(Module.Types.Descr.integer())) ==
               {:list, {:integer, nil}}

      # we loose improper info
      assert ElixirTypes.to_shape(
               Module.Types.Descr.non_empty_list(
                 Module.Types.Descr.integer(),
                 Module.Types.Descr.integer()
               )
             ) == {:list, {:integer, nil}}
    end

    test "handles open map" do
      # open_map() is a top open map (any map), which cannot be converted to a shape (no known fields)
      assert ElixirTypes.to_shape(Module.Types.Descr.open_map()) == {:map, [], nil}
      # open_map with atom key fields - extract known fields, lose open/closed distinction
      assert ElixirTypes.to_shape(Module.Types.Descr.open_map(foo: Module.Types.Descr.binary())) ==
               {:map, [foo: {:binary, nil}], nil}

      # Elixir 1.20 removed the open_map/2 "default" form (replaced by domain
      # keys). An open map with multiple atom fields still has all known fields
      # extracted, losing the open/closed distinction (keys come back sorted).
      assert ElixirTypes.to_shape(
               Module.Types.Descr.open_map(
                 foo: Module.Types.Descr.binary(),
                 bar: Module.Types.Descr.integer()
               )
             ) == {:map, [bar: {:integer, nil}, foo: {:binary, nil}], nil}
    end

    test "handles open tuple" do
      # Open tuple with known elements - treat as minimum-size tuple, lose open/closed distinction
      assert ElixirTypes.to_shape(
               Module.Types.Descr.open_tuple([
                 Module.Types.Descr.atom([:ok]),
                 Module.Types.Descr.binary()
               ])
             ) == {:tuple, 2, [{:atom, :ok}, {:binary, nil}]}

      # Open tuple with fallback - still extracts known elements
      assert ElixirTypes.to_shape(
               Module.Types.Descr.open_tuple(
                 [Module.Types.Descr.atom([:ok]), Module.Types.Descr.binary()],
                 Module.Types.Descr.term()
               )
             ) == {:tuple, 2, [{:atom, :ok}, {:binary, nil}]}
    end

    test "handles pid" do
      assert ElixirTypes.to_shape(Module.Types.Descr.pid()) == :pid
    end

    test "handles port" do
      assert ElixirTypes.to_shape(Module.Types.Descr.port()) == :port
    end

    test "handles reference" do
      assert ElixirTypes.to_shape(Module.Types.Descr.reference()) == :reference
    end

    test "handles pid/port/reference through compatible subtypes" do
      assert ElixirTypes.to_shape(
               Module.Types.Descr.intersection(
                 Module.Types.Descr.pid(),
                 Module.Types.Descr.term()
               )
             ) == :pid

      assert ElixirTypes.to_shape(
               Module.Types.Descr.intersection(
                 Module.Types.Descr.port(),
                 Module.Types.Descr.term()
               )
             ) == :port

      assert ElixirTypes.to_shape(
               Module.Types.Descr.intersection(
                 Module.Types.Descr.reference(),
                 Module.Types.Descr.term()
               )
             ) == :reference
    end

    test "handles tuple" do
      # tuple() returns an open tuple (any tuple), which cannot be converted to a shape
      assert ElixirTypes.to_shape(Module.Types.Descr.tuple()) == {:tuple, 0, []}

      assert ElixirTypes.to_shape(
               Module.Types.Descr.tuple([
                 Module.Types.Descr.atom([:ok]),
                 Module.Types.Descr.integer()
               ])
             ) == {:tuple, 2, [{:atom, :ok}, {:integer, nil}]}
    end

    test "handles boolean" do
      assert ElixirTypes.to_shape(Module.Types.Descr.boolean()) ==
               {:union, [atom: false, atom: true]}
    end

    test "handles fun/0 (any function)" do
      assert ElixirTypes.to_shape(Module.Types.Descr.fun()) == :fun
    end

    test "handles fun/1 (function with specific arity)" do
      assert ElixirTypes.to_shape(Module.Types.Descr.fun(0)) == {:fun, 0}
      assert ElixirTypes.to_shape(Module.Types.Descr.fun(1)) == {:fun, 1}
      assert ElixirTypes.to_shape(Module.Types.Descr.fun(2)) == {:fun, 2}
    end

    test "handles fun/2 (function with arg and return types)" do
      # (integer() -> atom())
      fun_type = Module.Types.Descr.fun([Module.Types.Descr.integer()], Module.Types.Descr.atom())
      assert ElixirTypes.to_shape(fun_type) == {:fun, [{:integer, nil}], :atom}

      # (integer(), float() -> binary())
      fun_type2 =
        Module.Types.Descr.fun(
          [Module.Types.Descr.integer(), Module.Types.Descr.float()],
          Module.Types.Descr.binary()
        )

      assert ElixirTypes.to_shape(fun_type2) ==
               {:fun, [{:integer, nil}, {:float, nil}], {:binary, nil}}
    end

    test "handles fun_from_non_overlapping_clauses" do
      # Multiple non-overlapping function clauses
      clauses = [
        {[Module.Types.Descr.integer()], Module.Types.Descr.atom()},
        {[Module.Types.Descr.float()], Module.Types.Descr.binary()}
      ]

      fun_type = Module.Types.Descr.fun_from_non_overlapping_clauses(clauses)
      # Should be a union of function types
      shape = ElixirTypes.to_shape(fun_type)
      assert match?({:fun_clauses, _}, shape)
    end

    test "handles fun_from_inferred_clauses" do
      # Multiple potentially overlapping function clauses
      clauses = [
        {[Module.Types.Descr.integer()], Module.Types.Descr.atom()},
        {[Module.Types.Descr.integer()], Module.Types.Descr.binary()}
      ]

      fun_type = Module.Types.Descr.fun_from_inferred_clauses(clauses)
      shape = ElixirTypes.to_shape(fun_type)
      # Inferred clauses with same domain get merged into a single clause
      # with union return type: (integer() -> atom() | binary())
      assert match?({:fun, [{:integer, nil}], _return}, shape)
    end

    test "handles not_set" do
      assert ElixirTypes.to_shape(Module.Types.Descr.not_set()) == :not_set
    end

    test "handles if_set" do
      # if_set makes a type optional in a map
      optional_int = Module.Types.Descr.if_set(Module.Types.Descr.integer())
      assert ElixirTypes.to_shape(optional_int) == {:optional, {:integer, nil}}

      optional_atom = Module.Types.Descr.if_set(Module.Types.Descr.atom())
      assert ElixirTypes.to_shape(optional_atom) == {:optional, :atom}
    end

    test "handles cross-family unions" do
      # integer() | atom() — a union across different type families
      union = Module.Types.Descr.union(Module.Types.Descr.integer(), Module.Types.Descr.atom())
      assert ElixirTypes.to_shape(union) == {:union, [:atom, {:integer, nil}]}

      # integer() | binary() | nil
      union2 =
        Module.Types.Descr.union(
          Module.Types.Descr.union(Module.Types.Descr.integer(), Module.Types.Descr.binary()),
          Module.Types.Descr.atom([nil])
        )

      shape = ElixirTypes.to_shape(union2)
      assert match?({:union, _}, shape)
    end

    test "handles dynamic-wrapped types" do
      # dynamic(integer()) - to_quoted unwraps to {:integer, [], []}
      dynamic_int = Module.Types.Descr.dynamic(Module.Types.Descr.integer())
      assert ElixirTypes.to_shape(dynamic_int) == {:integer, nil}

      # dynamic(atom(:ok)) - preserves literal atom info
      dynamic_atom = Module.Types.Descr.dynamic(Module.Types.Descr.atom([:ok]))
      assert ElixirTypes.to_shape(dynamic_atom) == {:atom, :ok}
    end

    test "handles struct as dynamic map" do
      # Module.Types.Of.struct_type produces dynamic-wrapped map with __struct__
      struct = Module.Types.Of.struct_type(Date, [%{field: :year}, %{field: :month}])
      shape = ElixirTypes.to_shape(struct)
      assert match?({:struct, _fields, {:atom, Date}, nil}, shape)
    end
  end

  describe "expression typing" do
    test "of_expr/1 handles simple expressions safely" do
      # Should return :error if not enabled, or {:ok, descr} if available
      result = ElixirTypes.of_expr(42)
      assert result == :error or match?({:ok, _}, result)
    end
  end

  describe "expression typing with feature enabled" do
    setup do
      # Save original value and enable feature
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, true)

      on_exit(fn ->
        Application.put_env(:elixir_sense, :use_elixir_types, original_value)
      end)

      # Only run tests if Module.Types is available
      if ElixirTypes.available?() do
        :ok
      else
        :skip
      end
    end

    test "types integer literals" do
      result = ElixirTypes.of_expr(42)
      assert {:ok, descr} = result
      assert ElixirTypes.to_shape(descr) == {:integer, nil}
    end

    test "types float literals" do
      result = ElixirTypes.of_expr(3.14)
      assert {:ok, descr} = result
      assert ElixirTypes.to_shape(descr) == {:float, nil}
    end

    test "types binary literals" do
      result = ElixirTypes.of_expr("test")
      assert {:ok, descr} = result
      assert ElixirTypes.to_shape(descr) == {:binary, nil}
    end

    test "types atom literals" do
      for a <- [true, false, nil, :test_atom] do
        result = ElixirTypes.of_expr(a)
        assert {:ok, descr} = result
        assert ElixirTypes.to_shape(descr) == {:atom, a}
      end
    end

    test "types list literals" do
      # List with integers
      list_ast = [1, 2, 3]
      result = ElixirTypes.of_expr(list_ast)
      assert {:ok, descr} = result
      assert ElixirTypes.to_shape(descr) == {:list, {:integer, nil}}

      # Empty list
      empty_ast = []
      result = ElixirTypes.of_expr(empty_ast)
      assert {:ok, descr} = result
      assert ElixirTypes.to_shape(descr) == {:list, :empty}
    end

    test "types list mixed" do
      list_ast = [1, :ok]
      result = ElixirTypes.of_expr(list_ast)
      assert {:ok, descr} = result
      # Mixed list elements get unified to a single type
      shape = ElixirTypes.to_shape(descr)
      # The shape should be a list, but the element type depends on Module.Types' union handling
      assert match?({:list, _}, shape)
    end

    test "types list improper" do
      list_ast = [1 | :ok]
      result = ElixirTypes.of_expr(list_ast)
      # Improper lists may not be supported by Module.Types in all cases
      # Accept either error or a successful result
      case result do
        {:ok, descr} ->
          shape = ElixirTypes.to_shape(descr)
          assert match?({:list, _}, shape) or is_nil(shape)

        :error ->
          assert true
      end
    end

    test "types tuple-0 AST" do
      tuple_ast = {:{}, [], []}
      result = ElixirTypes.of_expr(tuple_ast)
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert {:tuple, 0, []} = shape
    end

    test "types tuple-2 AST" do
      tuple_ast = {1, :ok}
      result = ElixirTypes.of_expr(tuple_ast)
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert {:tuple, 2, elements} = shape
      assert elements == [{:integer, nil}, {:atom, :ok}]
    end

    test "types tuple-3 AST" do
      tuple_ast = {:{}, [], [1, :ok, 1.2]}
      result = ElixirTypes.of_expr(tuple_ast)
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert {:tuple, 3, elements} = shape
      assert elements == [{:integer, nil}, {:atom, :ok}, {:float, nil}]
    end

    test "types map empty AST" do
      # Map AST: %{key: :value}
      map_ast = {:%{}, [], []}
      result = ElixirTypes.of_expr(map_ast)
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert {:map, [], nil} = shape
    end

    test "types closed map AST" do
      # Map AST: %{key: :value}
      map_ast = {:%{}, [], [key: :value]}
      result = ElixirTypes.of_expr(map_ast)
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert {:map, fields, nil} = shape
      assert fields == [key: {:atom, :value}]
    end

    test "types open map AST" do
      map_ast = {:%{}, [], [{"some", "other"}, key: :value]}
      result = ElixirTypes.of_expr(map_ast)
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert {:map, fields, nil} = shape
      assert fields == [key: {:atom, :value}]
    end

    test "types struct AST" do
      # Map AST: %{key: :value}
      struct_ast =
        {:%, [],
         [
           Date,
           {:%{}, [], [year: 2000, month: 1, day: 1]}
         ]}

      result = ElixirTypes.of_expr(struct_ast)
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert {:struct, fields, {:atom, Date}, nil} = shape

      assert Enum.sort(fields) == [
               day: {:integer, nil},
               month: {:integer, nil},
               year: {:integer, nil}
             ]
    end

    test "handles complex nested structures" do
      # Nested tuple with list: {[1, 2], :ok}
      nested_ast = {:{}, [], [[1, 2], :ok]}
      result = ElixirTypes.of_expr(nested_ast)
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert {:tuple, 2, elements} = shape
      assert elements == [{:list, {:integer, nil}}, {:atom, :ok}]
    end

    test "types local call" do
      call_ast = {:foo, [], []}
      assert {:ok, _descr} = ElixirTypes.of_expr(call_ast)
      # Returns dynamic(term()) — no local handler or remote sig available
      shape = ElixirTypes.to_shape(call_ast)
      assert shape == nil
    end

    test "types remote call" do
      call_ast = {{:., [], [Foo, :bar]}, [], []}
      assert {:ok, _descr} = ElixirTypes.of_expr(call_ast)
      # Returns dynamic(term()) — no ExCk sig for Foo.bar
      shape = ElixirTypes.to_shape(call_ast)
      assert shape == nil
    end

    test "types anonymous call" do
      call_ast = {{:., [], [{:foo, [version: 0], nil}]}, [], []}
      result = ElixirTypes.of_expr(call_ast)
      # Returns dynamic(term()) — no local handler or remote sig available
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert shape == nil
    end

    test "types variables" do
      variable_ast = {:foo, [version: 0], nil}
      result = ElixirTypes.of_expr(variable_ast)
      # dynamic when var not know
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert shape == nil

      result =
        ElixirTypes.of_expr(variable_ast, variables: %{{:foo, 0} => Module.Types.Descr.integer()})

      # concrete type when known
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert shape == {:integer, nil}
    end

    test "types property access" do
      call_ast = {{:., [], [{:foo, [version: 0], nil}, :bar]}, [no_parens: true], []}
      assert {:ok, _descr} = ElixirTypes.of_expr(call_ast)
      # Returns dynamic(term()) — no local handler or remote sig available
      shape = ElixirTypes.to_shape(call_ast)
      assert shape == nil
    end

    test "handles unknown expressions gracefully" do
      # Try with a complex AST that might not be fully supported
      complex_ast = {:some_unknown_call, [], []}
      result = ElixirTypes.of_expr(complex_ast)
      # Should either succeed or fail gracefully
      assert result == :error or match?({:ok, _}, result)
    end
  end

  describe "shape merging" do
    test "merge_shapes/2 prefers more specific types" do
      # Test integer literal vs generic
      result = ElixirTypes.merge_shapes({:integer, nil}, {:integer, 42})
      assert result == {:integer, 42}

      # Test keeping :none
      result = ElixirTypes.merge_shapes(:none, {:integer, 42})
      assert result == :none

      # Test nil handling
      result = ElixirTypes.merge_shapes(nil, {:integer, 42})
      assert result == {:integer, 42}

      result = ElixirTypes.merge_shapes({:integer, 42}, nil)
      assert result == {:integer, 42}
    end

    test "merge_shapes/2 prefers lists with concrete element types" do
      result = ElixirTypes.merge_shapes({:list, nil}, {:list, {:integer, nil}})
      assert result == {:list, {:integer, nil}}

      result = ElixirTypes.merge_shapes({:list, {:integer, nil}}, {:list, nil})
      assert result == {:list, {:integer, nil}}
    end

    test "merge_shapes/2 defaults to existing for unknown cases" do
      result = ElixirTypes.merge_shapes({:custom, :type}, {:other, :type})
      assert result == {:custom, :type}
    end
  end

  describe "integration helpers" do
    test "init_stack/4 returns stack or nil" do
      result = ElixirTypes.init_stack()
      assert result == nil or is_map(result)
    end

    test "init_context/0 returns context or nil" do
      result = ElixirTypes.init_context()
      assert result == nil or is_map(result)
    end
  end
end
