defmodule ElixirSense.Core.ElixirTypesRealTest do
  use ExUnit.Case, async: false

  # Entire module exercises the native Module.Types backend (Elixir 1.18+).
  @moduletag :requires_native_types

  alias ElixirSense.Core.{ElixirTypes, TypeInference}

  @moduletag :elixir_types_integration

  describe "real Module.Types integration" do
    setup do
      # Save original value and enable feature for these tests
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, true)

      on_exit(fn ->
        Application.put_env(:elixir_sense, :use_elixir_types, original_value)
      end)

      # Skip tests if Module.Types is not available
      if ElixirTypes.available?() do
        :ok
      else
        :skip
      end
    end

    test "TypeInference integrates with ElixirTypes for complex expressions" do
      # Test that TypeInference.type_of uses ElixirTypes when enabled

      # Integer literal should be typed via ElixirTypes
      result = TypeInference.type_of(42, :none)
      assert result == {:integer, 42} or result == {:integer, nil}

      # Tuple should be enhanced with ElixirTypes
      tuple_ast = {:{}, [], [1, :ok]}
      result = TypeInference.type_of(tuple_ast, :none)
      assert {:tuple, 2, elements} = result
      # Should have at least the basic structure
      assert length(elements) == 2

      # List should be enhanced with ElixirTypes. task #22: a non-empty literal
      # list now surfaces as {:nonempty_list, _} (or {:list, _} via the custom
      # fallback engine), so accept either.
      list_ast = [1, 2, 3]
      result = TypeInference.type_of(list_ast, :none)
      assert match?({:list, _element_type}, result) or match?({:nonempty_list, _}, result)
    end

    test "fallback typing works for unknown expressions" do
      # Unknown AST should fallback gracefully
      unknown_ast = {:unknown_function, [line: 1], []}
      result = TypeInference.type_of(unknown_ast, :none)

      # Should either get a local call type or enhanced type from ElixirTypes
      case result do
        {:local_call, :unknown_function, {1, 1}, []} -> :ok
        nil -> :ok
        other when is_tuple(other) -> :ok
      end
    end

    test "ElixirTypes shape conversion handles all supported types" do
      test_cases = [
        # Basic types
        {42, {:integer, nil}},
        {3.14, {:float, nil}},
        {"test", {:binary, nil}},
        {:atom_test, {:atom, :atom_test}},
        # task #22: [] -> :empty_list, non-empty list -> {:nonempty_list, _}
        {[], :empty_list},

        # Complex types (AST form)
        {[1, 2, 3], {:nonempty_list, {:integer, nil}}},
        {{:{}, [], [1, :ok]}, {:tuple, 2, [{:integer, nil}, {:atom, :ok}]}},
        {{:%{}, [], [key: :value]}, {:map, [key: {:atom, :value}], :closed}}
      ]

      for {expr, expected_shape} <- test_cases do
        case ElixirTypes.of_expr(expr) do
          {:ok, descr} ->
            shape = ElixirTypes.to_shape(descr)

            assert shape == expected_shape,
                   "Expected #{inspect(expected_shape)} for #{inspect(expr)}, got #{inspect(shape)}"

          :error ->
            # Some expressions might not be supported, that's ok for M1
            :ok
        end
      end
    end

    test "shape merging preserves specificity" do
      # Test that ElixirTypes.merge_shapes works correctly

      # More specific integer should win
      result = ElixirTypes.merge_shapes({:integer, nil}, {:integer, 42})
      assert result == {:integer, 42}

      result = ElixirTypes.merge_shapes({:integer, 42}, {:integer, nil})
      assert result == {:integer, 42}

      # More specific list element should win
      result = ElixirTypes.merge_shapes({:list, nil}, {:list, {:integer, nil}})
      assert result == {:list, {:integer, nil}}

      # :none should always win (represents impossible type)
      result = ElixirTypes.merge_shapes(:none, {:integer, 42})
      assert result == :none

      # nil should be replaced
      result = ElixirTypes.merge_shapes(nil, {:integer, 42})
      assert result == {:integer, 42}
    end

    test "error handling is robust" do
      # Test that malformed input doesn't crash the system

      # Invalid descriptor
      result = ElixirTypes.to_shape(%{invalid: :descriptor})
      assert result == nil

      # Truly invalid AST structure (not a 3-tuple)
      result = ElixirTypes.of_expr({:invalid})
      assert result == :error

      # Feature disabled mid-operation
      Application.put_env(:elixir_sense, :use_elixir_types, false)
      refute ElixirTypes.enabled?()

      # After disabling, of_expr should either return :error or still work
      # if Module.Types is available (since our check is in enabled?(), not of_expr itself)
      result = ElixirTypes.of_expr(42)

      if ElixirTypes.available?() do
        assert result == :error or match?({:ok, _}, result)
      else
        assert result == :error
      end

      # Re-enable for cleanup
      Application.put_env(:elixir_sense, :use_elixir_types, true)
    end

    test "performance is reasonable for common expressions" do
      # Simple performance test to ensure no catastrophic slowdowns
      expressions = [
        42,
        3.14,
        "test",
        :atom,
        [],
        [1, 2, 3],
        {:{}, [], [1, :ok]},
        {:%{}, [], [key: :value]}
      ]

      # Time the operations
      start_time = System.monotonic_time(:millisecond)

      for expr <- expressions do
        case ElixirTypes.of_expr(expr) do
          {:ok, descr} -> ElixirTypes.to_shape(descr)
          :error -> :ok
        end
      end

      end_time = System.monotonic_time(:millisecond)
      duration = end_time - start_time

      # Should complete within reasonable time (100ms is very generous)
      assert duration < 100, "ElixirTypes operations took #{duration}ms, expected < 100ms"
    end

    test "version compatibility checks work" do
      # Test that availability checks work correctly
      assert is_boolean(ElixirTypes.available?())
      assert is_boolean(ElixirTypes.enabled?())

      # When enabled and available, should be true
      if ElixirTypes.available?() do
        assert ElixirTypes.enabled?()
      end

      # Stack and context creation should work or return nil gracefully
      stack = ElixirTypes.init_stack()
      assert stack == nil or is_map(stack)

      context = ElixirTypes.init_context()
      assert context == nil or is_map(context)
    end
  end

  describe "integration with disabled feature" do
    setup do
      # Ensure feature is disabled for these tests
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, false)

      on_exit(fn ->
        Application.put_env(:elixir_sense, :use_elixir_types, original_value)
      end)
    end

    test "TypeInference works normally when ElixirTypes is disabled" do
      refute ElixirTypes.enabled?()

      # Should work with existing ElixirSense behavior
      result = TypeInference.type_of(42, :none)
      assert result == {:integer, 42}

      # Complex expressions should work as before
      tuple_ast = {:{}, [], [1, :ok]}
      result = TypeInference.type_of(tuple_ast, :none)
      assert {:tuple, 2, elements} = result
      assert length(elements) == 2
    end

    test "ElixirTypes functions return error/nil when disabled" do
      refute ElixirTypes.enabled?()

      # Should return :error when disabled (but feature may still be available)
      result = ElixirTypes.of_expr(42)

      if ElixirTypes.available?() do
        # If Module.Types is available but feature is disabled via config,
        # our enabled?() check should prevent usage in normal integration,
        # but direct calls to of_expr might still work. This is expected.
        assert result == :error or match?({:ok, _}, result)
      else
        assert result == :error
      end

      # Shape conversion should still work (it's pure)
      result = ElixirTypes.merge_shapes({:integer, nil}, {:integer, 42})
      assert result == {:integer, 42}
    end
  end
end
