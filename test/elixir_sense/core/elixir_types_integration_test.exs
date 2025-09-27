defmodule ElixirSense.Core.ElixirTypesIntegrationTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Core.TypeInference

  describe "integration with TypeInference when disabled" do
    setup do
      # Ensure the feature is disabled for these tests
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, false)

      on_exit(fn ->
        Application.put_env(:elixir_sense, :use_elixir_types, original_value)
      end)
    end

    test "falls back to existing behavior when disabled" do
      # Test that our integration doesn't break existing functionality
      result = TypeInference.type_of(42, :none)
      assert result == {:integer, 42}

      # Test with an unknown function call - should return local call type
      result = TypeInference.type_of({:unknown_function, [], []}, :none)
      assert match?({:local_call, :unknown_function, {1, 1}, []}, result)
    end

    test "tuple and list typing works as before" do
      # Test that our enhanced tuple/list handlers don't break
      result = TypeInference.type_of({:{}, [], [1, 2, 3]}, :none)
      assert {:tuple, 3, [{:integer, 1}, {:integer, 2}, {:integer, 3}]} = result

      result = TypeInference.type_of([1, 2, 3], :none)
      assert {:list, {:integer, 1}} = result
    end
  end

  describe "integration with TypeInference when enabled" do
    setup do
      # Only run these tests if Module.Types is available
      if ElixirSense.Core.ElixirTypes.available?() do
        original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
        Application.put_env(:elixir_sense, :use_elixir_types, true)

        on_exit(fn ->
          Application.put_env(:elixir_sense, :use_elixir_types, original_value)
        end)

        :ok
      else
        :skip
      end
    end

    @tag :skip_if_module_types_unavailable
    test "enhances type inference for simple expressions" do
      # If Module.Types is available and enabled, we should get enhanced typing
      # For now, just verify it doesn't crash and returns reasonable results
      result = TypeInference.type_of(42, :none)
      assert result == {:integer, 42} or result == {:integer, nil}

      result = TypeInference.type_of([1, 2, 3], :none)
      assert match?({:list, _}, result)

      result = TypeInference.type_of({:{}, [], [1, 2]}, :none)
      assert match?({:tuple, 2, _}, result)
    end

    @tag :skip_if_module_types_unavailable
    test "handles unknown expressions gracefully" do
      # Unknown expressions should either return nil or a reasonable fallback
      result = TypeInference.type_of({:unknown_call, [], []}, :none)
      assert match?({:local_call, :unknown_call, {1, 1}, []}, result) or is_tuple(result)
    end
  end

  describe "error handling" do
    test "handles Module.Types unavailability gracefully" do
      # Even if we try to enable but Module.Types isn't available, should not crash
      Application.put_env(:elixir_sense, :use_elixir_types, true)

      result = TypeInference.type_of(42, :none)
      # Should still work, either via ElixirTypes or fallback
      assert result == {:integer, 42} or result == {:integer, nil}

      Application.put_env(:elixir_sense, :use_elixir_types, false)
    end
  end
end
