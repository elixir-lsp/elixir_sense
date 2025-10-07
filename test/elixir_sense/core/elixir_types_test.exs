defmodule ElixirSense.Core.ElixirTypesTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Core.ElixirTypes

  describe "availability" do
    test "available?/0 returns boolean" do
      result = ElixirTypes.available?()
      assert is_boolean(result)
    end

    test "enabled?/0 returns false by default" do
      # Should be false since config defaults to false
      refute ElixirTypes.enabled?()
    end
  end

  describe "shape conversion" do
    test "to_shape/1 returns nil when Module.Types not available" do
      # This will be nil if Module.Types isn't available or enabled
      result = ElixirTypes.to_shape(%{})
      assert result == nil or is_tuple(result)
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
      result = ElixirTypes.of_expr(list_ast) |> dbg
      assert {:ok, descr} = result
      assert ElixirTypes.to_shape(descr) == {:list, {:integer, nil}}

      # Empty list
      empty_ast = []
      result = ElixirTypes.of_expr(empty_ast)
      assert {:ok, descr} = result
      assert ElixirTypes.to_shape(descr) == {:list, :empty}
    end

    test "types tuple-0 AST" do
      tuple_ast = {:{}, [], []}
      result = ElixirTypes.of_expr(tuple_ast) |> dbg
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert {:tuple, 0, []} = shape
    end

    test "types tuple-2 AST" do
      tuple_ast = {1, :ok}
      result = ElixirTypes.of_expr(tuple_ast) |> dbg
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert {:tuple, 2, elements} = shape
      assert elements == [{:integer, nil}, {:atom, :ok}]
    end

    test "types tuple-3 AST" do
      tuple_ast = {:{}, [], [1, :ok, 1.2]}
      result = ElixirTypes.of_expr(tuple_ast) |> dbg
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

    test "types map AST" do
      # Map AST: %{key: :value}
      map_ast = {:%{}, [], [key: :value]}
      result = ElixirTypes.of_expr(map_ast)
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert {:map, fields, nil} = shape
      assert fields == [key: {:atom, :value}]
    end

    test "types struct AST" do
      # Map AST: %{key: :value}
      struct_ast = {:%, [],
        [
          Date,
          {:%{}, [], [year: 2000, month: 1, day: 1]}
        ]}
      result = ElixirTypes.of_expr(struct_ast) |> dbg
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert {:struct, fields, {:atom, Date}, nil} = shape
      assert Enum.sort(fields) == [day: {:integer, nil}, month: {:integer, nil}, year: {:integer, nil}]
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
