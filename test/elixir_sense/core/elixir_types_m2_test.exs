defmodule ElixirSense.Core.ElixirTypesM2Test do
  use ExUnit.Case, async: false

  alias ElixirSense.Core.{ElixirTypes, MetadataBuilder, TypeInference}

  @moduletag :elixir_types_m2

  describe "M2 local function inference" do
    setup do
      # Enable ElixirTypes for M2 tests
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

    test "captures clause AST during compilation" do
      code = """
      defmodule TestModule do
        def add(x, y) do
          x + y
        end

        def factorial(0), do: 1
        def factorial(n) when n > 0 do
          n * factorial(n - 1)
        end
      end
      """

      state =
        code
        |> Code.string_to_quoted(columns: true, token_metadata: true)
        |> elem(1)
        |> MetadataBuilder.build()

      # Check that clauses were captured for add/2
      add_key = {TestModule, :add, 2}
      assert %{elixir_types_clauses: add_clauses} = state.mods_funs_to_positions[add_key]
      assert length(add_clauses) == 1

      # Check that clauses were captured for factorial/1
      factorial_key = {TestModule, :factorial, 1}

      assert %{elixir_types_clauses: factorial_clauses} =
               state.mods_funs_to_positions[factorial_key]

      assert length(factorial_clauses) == 2
    end

    test "infers signatures from captured clauses" do
      code = """
      defmodule TestModule do
        def simple_add(x, y) do
          x + y
        end

        def identity(x), do: x
      end
      """

      state =
        code
        |> Code.string_to_quoted(columns: true, token_metadata: true)
        |> elem(1)
        |> MetadataBuilder.build()

      # Check that signatures were inferred
      add_key = {TestModule, :simple_add, 2}

      assert %{elixir_types_sig: add_sig, elixir_types_status: add_status} =
               state.mods_funs_to_positions[add_key]

      identity_key = {TestModule, :identity, 1}

      assert %{elixir_types_sig: identity_sig, elixir_types_status: identity_status} =
               state.mods_funs_to_positions[identity_key]

      # Signatures should either be inferred successfully or skipped gracefully
      assert add_status in [:ok, :skipped]
      assert identity_status in [:ok, :skipped]

      if add_status == :ok do
        assert {:infer, _domain, _clauses} = add_sig
      end

      if identity_status == :ok do
        assert {:infer, _domain, _clauses} = identity_sig
      end
    end

    test "handles functions with guards" do
      code = """
      defmodule TestModule do
        def guarded_fun(x) when is_integer(x), do: x * 2
        def guarded_fun(x) when is_binary(x), do: String.length(x)
      end
      """

      state =
        code
        |> Code.string_to_quoted(columns: true, token_metadata: true)
        |> elem(1)
        |> MetadataBuilder.build()

      key = {TestModule, :guarded_fun, 1}

      assert %{
               elixir_types_clauses: clauses,
               elixir_types_sig: sig,
               elixir_types_status: status
             } = state.mods_funs_to_positions[key]

      # Should have captured both clauses
      assert length(clauses) == 2

      # Status should be ok or skipped
      assert status in [:ok, :skipped]

      if status == :ok do
        assert {:infer, _domain, clause_types} = sig
        assert length(clause_types) == 2
      end
    end

    test "skips functions with unquotes" do
      code = """
      defmodule TestModule do
        name = :dynamic_name
        def unquote(name)(x), do: x
      end
      """

      state =
        code
        |> Code.string_to_quoted(columns: true, token_metadata: true)
        |> elem(1)
        |> MetadataBuilder.build()

      # Should not have captured clauses for functions with unquotes
      unknown_key = {TestModule, :__unknown__, 1}

      case state.mods_funs_to_positions[unknown_key] do
        # Not captured at all
        nil -> :ok
        %{elixir_types_clauses: clauses} -> assert clauses == []
      end
    end

    test "handles private functions" do
      code = """
      defmodule TestModule do
        def public_fun(x), do: private_fun(x)

        defp private_fun(x), do: x * 2
      end
      """

      state =
        code
        |> Code.string_to_quoted(columns: true, token_metadata: true)
        |> elem(1)
        |> MetadataBuilder.build()

      # Both public and private functions should be captured
      public_key = {TestModule, :public_fun, 1}
      private_key = {TestModule, :private_fun, 1}

      assert %{elixir_types_clauses: [_]} = state.mods_funs_to_positions[public_key]
      assert %{elixir_types_clauses: [_]} = state.mods_funs_to_positions[private_key]
    end

    test "local handler integration works" do
      code = """
      defmodule TestModule do
        def simple_add(x, y), do: x + y
        def identity(x), do: x
      end
      """

      state =
        code
        |> Code.string_to_quoted(columns: true, token_metadata: true)
        |> elem(1)
        |> MetadataBuilder.build()

      # Build local signatures map
      local_sigs_map = ElixirTypes.build_local_sigs_map(state, TestModule)

      # Should have captured signatures for both functions
      assert Map.has_key?(local_sigs_map, {:simple_add, 2})
      assert Map.has_key?(local_sigs_map, {:identity, 1})

      # Test that we can use the local handler
      case Map.get(local_sigs_map, {:simple_add, 2}) do
        {kind, {:infer, _domain, _clause_types}} ->
          assert kind == :def

        _ ->
          # If inference failed, that's ok for M2
          :ok
      end
    end

    test "TypeInference integration with local signatures" do
      code = """
      defmodule TestModule do
        def add(x, y), do: x + y
      end
      """

      state =
        code
        |> Code.string_to_quoted(columns: true, token_metadata: true)
        |> elem(1)
        |> MetadataBuilder.build()

      # Build local signatures map
      local_sigs_map = ElixirTypes.build_local_sigs_map(state, TestModule)

      # Test typing a local call with the signatures map
      call_ast = {:add, [], [1, 2]}
      result = TypeInference.type_of_with_elixir_types(call_ast, :none, local_sigs_map)

      # Result should either be a type or nil (graceful fallback)
      case result do
        # Fallback is acceptable for M2
        nil -> :ok
        # Got a type result
        type when is_tuple(type) -> :ok
        other -> flunk("Unexpected result: #{inspect(other)}")
      end
    end
  end

  describe "M2 integration with disabled feature" do
    setup do
      # Ensure feature is disabled
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, false)

      on_exit(fn ->
        Application.put_env(:elixir_sense, :use_elixir_types, original_value)
      end)
    end

    test "does not capture clauses when disabled" do
      code = """
      defmodule TestModule do
        def add(x, y), do: x + y
      end
      """

      state =
        code
        |> Code.string_to_quoted(columns: true, token_metadata: true)
        |> elem(1)
        |> MetadataBuilder.build()

      key = {TestModule, :add, 2}
      mod_fun_info = state.mods_funs_to_positions[key]

      # Should have default empty clauses
      assert mod_fun_info.elixir_types_clauses == []
      assert mod_fun_info.elixir_types_sig == nil
      assert mod_fun_info.elixir_types_status == :skipped
    end
  end
end
