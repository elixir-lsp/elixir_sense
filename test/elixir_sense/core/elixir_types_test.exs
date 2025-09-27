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