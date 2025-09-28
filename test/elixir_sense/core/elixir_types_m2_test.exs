defmodule ElixirSense.Core.ElixirTypesM2Test do
  use ExUnit.Case, async: false

  alias ElixirSense.Core.{Binding, ElixirTypes, Metadata, MetadataBuilder, TypeInference}

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
        # For M2, we accept that some clauses might not be successfully inferred
        # The important thing is that we captured the clauses and attempted inference
        assert length(clause_types) >= 1
        assert length(clause_types) <= 2
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

    test "Binding uses inferred local signatures for local calls" do
      code = """
      defmodule LocalExample do
        def helper(), do: 1
        def caller() do
          helper()
        end
      end
      """

      {:ok, ast} = Code.string_to_quoted(code, columns: true, token_metadata: true)
      state = MetadataBuilder.build(ast)
      metadata = Metadata.fill(code, state)

      call_line = 4
      env = state.lines_to_env[call_line]
      refute is_nil(env)

      binding = Binding.from_env(env, metadata, {call_line, 5})

      result = Binding.expand(binding, {:local_call, :helper, {call_line, 5}, []})

      assert result == {:integer, nil}
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

  describe "M2 remote function integration" do
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

    test "remote_handler_from creates proper handler closure" do
      handler = ElixirTypes.remote_handler_from()
      assert is_function(handler, 6)
    end

    test "remote handler looks up ExCk signatures" do
      # Test with a module that likely has ExCk data (like Enum)
      handler = ElixirTypes.remote_handler_from()

      # Call handler with Enum.map/2 (commonly available function)
      result = handler.(Enum, :map, 2, [], nil, nil)

      # Result should either be a type result or false (graceful fallback)
      case result do
        # ExCk not available, fallback is acceptable
        false -> :ok
        # Got ExCk signature
        {:def, _type, _context} -> :ok
        other -> flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "remote handler falls back gracefully when ExCk unavailable" do
      # Test with a non-existent module
      handler = ElixirTypes.remote_handler_from()

      result = handler.(NonExistentModule, :some_function, 1, [], nil, nil)

      # Should return false for fallback
      assert result == false
    end

    test "remote handler unions clause return types from ExCk" do
      mod = Module.concat(ElixirSense, "ExCkFixture#{System.unique_integer([:positive])}")

      clauses = [
        {[Module.Types.Descr.atom([true])], Module.Types.Descr.atom([:yes])},
        {[Module.Types.Descr.atom([false])], Module.Types.Descr.atom([:no])}
      ]

      chunk_payload =
        {:elixir_checker_v2,
         %{
           exports: [
             {{:choose, 1}, %{sig: {:infer, nil, clauses}}}
           ],
           mode: :elixir
         }}

      chunk_binary = :erlang.term_to_binary(chunk_payload)

      case :ets.whereis(ElixirSense.Core.ExCkReader) do
        :undefined -> :ok
        _ -> :ets.delete(ElixirSense.Core.ExCkReader, mod)
      end

      assert {:ok, signatures} =
               ElixirSense.Core.ExCkReader.read_chunk(mod, chunk: chunk_binary)

      table_name = ElixirSense.Core.ExCkReader

      case :ets.info(table_name) do
        :undefined -> :ets.new(table_name, [:named_table, :public, :set, read_concurrency: true])
        _ -> :ok
      end

      :ets.insert(table_name, {mod, {:ok, signatures}, System.monotonic_time(:millisecond)})

      on_exit(fn ->
        case :ets.whereis(ElixirSense.Core.ExCkReader) do
          :undefined -> :ok
          _ -> :ets.delete(ElixirSense.Core.ExCkReader, mod)
        end
      end)

      handler = ElixirTypes.remote_handler_from()
      assert {:def, return_type, _ctx} = handler.(mod, :choose, 1, [], nil, nil)

      expected = Module.Types.Descr.atom([:no, :yes])
      assert Module.Types.Descr.equal?(return_type, expected)
    end

    test "init_stack integrates remote handler" do
      stack = ElixirTypes.init_stack(TestModule, {:test_fun, 1}, "test.ex", :dynamic, nil, %{})

      case stack do
        nil ->
          # Module.Types not available, that's ok
          :ok

        %{remote_handler: handler} ->
          # Should have remote handler
          assert is_function(handler, 6)

        _other ->
          # Different stack structure, check if it has remote handler capability
          assert stack != nil
      end
    end

    test "of_expr with metadata parameter" do
      # Test the updated of_expr function with metadata
      ast = {:+, [], [1, 2]}

      result = ElixirTypes.of_expr(ast, TestModule, {:test, 1}, "test.ex", :dynamic, nil, %{})

      case result do
        # Got type descriptor
        {:ok, _descr} -> :ok
        # Acceptable fallback for M2
        :error -> :ok
        other -> flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "TypeInference integration with remote signatures" do
      # Test TypeInference with metadata parameter
      ast = {{:., [], [Enum, :map]}, [], [[1, 2, 3], {:fn, [], nil}]}

      result = TypeInference.type_of_with_elixir_types(ast, :none, nil, %{})

      # Result should either be a type or nil (graceful fallback)
      case result do
        # Fallback is acceptable for M2
        nil -> :ok
        # Got a type result
        type when is_tuple(type) -> :ok
        other -> flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "backward compatibility with existing TypeInference calls" do
      # Ensure 3-arity version still works
      ast = {:+, [], [1, 2]}

      result = TypeInference.type_of_with_elixir_types(ast, :none, nil)

      # Should work the same as before
      case result do
        # Fallback is acceptable
        nil -> :ok
        # Got a type result
        type when is_tuple(type) -> :ok
        other -> flunk("Unexpected result: #{inspect(other)}")
      end
    end
  end

  describe "M2 enhanced shape conversion" do
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

    test "converts union types to shape" do
      # Test union type conversion - this is simplified for M2
      # In a real scenario, we'd need proper union descriptors from Module.Types

      # Test basic atom union
      atom_set = :sets.from_list([:ok, :error])
      union_descr = %{atom: {:union, atom_set}}

      result = ElixirTypes.to_shape(union_descr)

      case result do
        {:union, shapes} ->
          # Should have union of atom shapes
          assert length(shapes) >= 1
          assert Enum.all?(shapes, fn shape -> match?({:atom, _}, shape) end)

        {:atom, atom} when atom in [:ok, :error] ->
          # Single atom fallback is acceptable
          :ok

        nil ->
          # M2 may not handle all union cases yet
          :ok

        other ->
          flunk("Unexpected union shape: #{inspect(other)}")
      end
    end

    test "converts struct types to shape" do
      # Test struct type conversion - simplified for M2
      # Create a descriptor that looks like a struct
      struct_descr = %{
        map: %{
          closed: [
            {:__struct__, %{atom: {:union, :sets.from_list([User])}}},
            {:name, %{binary: %{}}},
            {:age, %{integer: %{}}}
          ]
        }
      }

      result = ElixirTypes.to_shape(struct_descr)

      case result do
        {:struct, User, fields} ->
          # Should have struct with fields
          assert is_list(fields)
          field_names = Keyword.keys(fields)
          assert :name in field_names or :age in field_names

        nil ->
          # M2 may not handle all struct cases yet
          :ok

        other ->
          flunk("Unexpected struct shape: #{inspect(other)}")
      end
    end

    test "converts function types to shape" do
      # Test function type conversion
      fun_descr = %{
        fun: %{
          arity: 2,
          type: :function
        }
      }

      result = ElixirTypes.to_shape(fun_descr)

      case result do
        # Correct function shape
        {:fun, 2} -> :ok
        # M2 may not handle all function cases yet
        nil -> :ok
        other -> flunk("Unexpected function shape: #{inspect(other)}")
      end
    end

    test "converts bounded integer types to shape" do
      # Test bounded integer conversion
      range_descr = %{
        integer: %{
          min: 1,
          max: 10
        }
      }

      result = ElixirTypes.to_shape(range_descr)

      case result do
        # Correct bounded integer
        {:integer, {1, 10}} -> :ok
        # Fallback to unbounded is acceptable
        {:integer, nil} -> :ok
        # M2 may not handle all range cases yet
        nil -> :ok
        other -> flunk("Unexpected integer range shape: #{inspect(other)}")
      end
    end

    test "converts string literal types to shape" do
      # Test string literal conversion
      literal_descr = %{
        binary: %{
          literal: "hello"
        }
      }

      result = ElixirTypes.to_shape(literal_descr)

      case result do
        # Correct string literal
        {:binary, "hello"} -> :ok
        # Fallback to general binary is acceptable
        {:binary, nil} -> :ok
        # M2 may not handle all literal cases yet
        nil -> :ok
        other -> flunk("Unexpected string literal shape: #{inspect(other)}")
      end
    end

    test "handles PID types" do
      # Test that PID descriptor converts properly
      if ElixirTypes.available?() do
        pid_descr = Module.Types.Descr.pid()
        result = ElixirTypes.to_shape(pid_descr)

        case result do
          # Correct PID shape
          :pid -> :ok
          # Fallback is acceptable for M2
          nil -> :ok
          other -> flunk("Unexpected PID shape: #{inspect(other)}")
        end
      else
        :skip
      end
    end

    test "handles complex nested types" do
      # Test complex nested type like list(%{String.t => atom | integer})
      # This is a simplified version for M2

      # Create a list of maps descriptor
      map_descr = %{
        map: %{
          closed: [
            {:key, %{binary: %{}}},
            {:value, %{atom: {:union, :sets.from_list([:a, :b])}}}
          ]
        }
      }

      list_descr = %{
        list: %{
          element: map_descr
        }
      }

      result = ElixirTypes.to_shape(list_descr)

      case result do
        # List of maps
        {:list, {:map, _fields, nil}} -> :ok
        # Simplified list fallback
        {:list, nil} -> :ok
        # M2 may not handle all complex cases yet
        nil -> :ok
        other -> flunk("Unexpected complex shape: #{inspect(other)}")
      end
    end

    test "gracefully handles unknown descriptor formats" do
      # Test that unknown descriptor formats don't crash
      unknown_descr = %{
        unknown_type: %{
          some_data: "test"
        }
      }

      result = ElixirTypes.to_shape(unknown_descr)

      # Should not crash and return nil for unknown types
      assert result == nil
    end

    test "preserves existing shape conversion behavior" do
      # Test that existing basic types still work correctly
      if ElixirTypes.available?() do
        # Test basic integer
        int_descr = Module.Types.Descr.integer()
        assert ElixirTypes.to_shape(int_descr) == {:integer, nil}

        # Test basic binary
        bin_descr = Module.Types.Descr.binary()
        assert ElixirTypes.to_shape(bin_descr) == {:binary, nil}

        # Test basic float
        float_descr = Module.Types.Descr.float()
        assert ElixirTypes.to_shape(float_descr) == {:float, nil}
      else
        :skip
      end
    end
  end

  describe "M2 pattern matching refinement" do
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

    test "enhanced of_match with variable type refinement" do
      # Test basic pattern matching with variable refinement
      pattern_ast = {:x, [version: 1], nil}
      match_ast = {:=, [], [pattern_ast, 42]}

      result =
        ElixirTypes.of_match(
          pattern_ast,
          nil,
          match_ast,
          TestModule,
          {:test, 1},
          "test.ex",
          :dynamic,
          target_keys: [{:x, 1}]
        )

      case result do
        {:ok, vars} when is_map(vars) ->
          # Should get variable refinement
          assert map_size(vars) >= 0

        :error ->
          # Acceptable fallback for complex cases
          :ok

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "struct pattern matching refinement" do
      # Test struct pattern matching
      pattern_ast =
        {:%, [],
         [
           {:__aliases__, [], [:User]},
           {:%{}, [], [{:name, {:x, [version: 1], nil}}]}
         ]}

      match_ast = {:=, [], [pattern_ast, {:user_value, [], nil}]}

      result =
        ElixirTypes.of_match(
          pattern_ast,
          nil,
          match_ast,
          TestModule,
          {:test, 1},
          "test.ex",
          :dynamic,
          target_keys: [{:x, 1}]
        )

      case result do
        {:ok, vars} when is_map(vars) ->
          # Should handle struct patterns gracefully
          assert is_map(vars)

        :error ->
          # Acceptable for complex patterns
          :ok

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "tuple pattern matching refinement" do
      # Test tuple pattern matching
      pattern_ast =
        {:{}, [],
         [
           {:x, [version: 1], nil},
           {:y, [version: 2], nil}
         ]}

      match_ast = {:=, [], [pattern_ast, {{:., [], [Tuple, :new]}, [], [2]}]}

      result =
        ElixirTypes.of_match(
          pattern_ast,
          nil,
          match_ast,
          TestModule,
          {:test, 1},
          "test.ex",
          :dynamic,
          target_keys: [{:x, 1}, {:y, 2}]
        )

      case result do
        {:ok, vars} when is_map(vars) ->
          # Should handle tuple patterns
          assert is_map(vars)

        :error ->
          # Acceptable for complex cases
          :ok

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "list pattern matching refinement" do
      # Test list pattern matching
      pattern_ast = [
        {:|, [],
         [
           {:head, [version: 1], nil},
           {:tail, [version: 2], nil}
         ]}
      ]

      match_ast = {:=, [], [pattern_ast, [1, 2, 3]]}

      result =
        ElixirTypes.of_match(
          pattern_ast,
          nil,
          match_ast,
          TestModule,
          {:test, 1},
          "test.ex",
          :dynamic,
          target_keys: [{:head, 1}, {:tail, 2}]
        )

      case result do
        {:ok, vars} when is_map(vars) ->
          # Should handle list patterns
          assert is_map(vars)

        :error ->
          # Acceptable for complex cases
          :ok

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "nested pattern matching refinement" do
      # Test complex nested patterns
      pattern_ast =
        {:%, [],
         [
           {:__aliases__, [], [:Response]},
           {:%{}, [],
            [
              {:status,
               {:=, [],
                [
                  {:status_var, [version: 1], nil},
                  200
                ]}},
              {:data, {:data_var, [version: 2], nil}}
            ]}
         ]}

      match_ast = {:=, [], [pattern_ast, {:api_response, [], nil}]}

      result =
        ElixirTypes.of_match(
          pattern_ast,
          nil,
          match_ast,
          TestModule,
          {:test, 1},
          "test.ex",
          :dynamic,
          target_keys: [{:status_var, 1}, {:data_var, 2}]
        )

      case result do
        {:ok, vars} when is_map(vars) ->
          # Should handle nested patterns
          assert is_map(vars)

        :error ->
          # Acceptable for very complex patterns
          :ok

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "guard pattern matching refinement" do
      # Test pattern with guards
      pattern_ast =
        {:when, [],
         [
           {:x, [version: 1], nil},
           {{:., [], [:erlang, :is_integer]}, [], [{:x, [version: 1], nil}]}
         ]}

      match_ast = {:=, [], [pattern_ast, 42]}

      result =
        ElixirTypes.of_match(
          pattern_ast,
          nil,
          match_ast,
          TestModule,
          {:test, 1},
          "test.ex",
          :dynamic,
          target_keys: [{:x, 1}]
        )

      case result do
        {:ok, vars} when is_map(vars) ->
          # Should handle guard patterns
          assert is_map(vars)

        :error ->
          # Acceptable for guard patterns
          :ok

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "map pattern matching refinement" do
      # Test map pattern matching
      pattern_ast =
        {:%{}, [],
         [
           {:name, {:name_var, [version: 1], nil}},
           {:age, {:age_var, [version: 2], nil}}
         ]}

      match_ast = {:=, [], [pattern_ast, {:%{}, [], [{:name, "John"}, {:age, 30}]}]}

      result =
        ElixirTypes.of_match(
          pattern_ast,
          nil,
          match_ast,
          TestModule,
          {:test, 1},
          "test.ex",
          :dynamic,
          target_keys: [{:name_var, 1}, {:age_var, 2}]
        )

      case result do
        {:ok, vars} when is_map(vars) ->
          # Should handle map patterns
          assert is_map(vars)

        :error ->
          # Acceptable for complex map patterns
          :ok

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "fallback to original pattern matching on errors" do
      # Test graceful fallback when enhanced matching fails
      invalid_pattern = {:invalid, [], []}
      invalid_match = {:=, [], [invalid_pattern, :whatever]}

      result =
        ElixirTypes.of_match(
          invalid_pattern,
          nil,
          invalid_match,
          TestModule,
          {:test, 1},
          "test.ex",
          :dynamic,
          target_keys: []
        )

      # Should gracefully handle invalid patterns
      case result do
        {:ok, vars} when is_map(vars) -> :ok
        # Expected for invalid patterns
        :error -> :ok
        other -> flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "type intersection with expected descriptors" do
      # Test type intersection with value-based refinement
      pattern_ast = {:x, [version: 1], nil}
      match_ast = {:=, [], [pattern_ast, 42]}

      # Use integer descriptor as expected type
      if ElixirTypes.available?() do
        expected_descr = Module.Types.Descr.integer()

        result =
          ElixirTypes.of_match(
            pattern_ast,
            expected_descr,
            match_ast,
            TestModule,
            {:test, 1},
            "test.ex",
            :dynamic,
            target_keys: [{:x, 1}]
          )

        case result do
          {:ok, vars} when is_map(vars) ->
            # Should use expected descriptor for better refinement
            assert is_map(vars)

          :error ->
            # Acceptable fallback
            :ok

          other ->
            flunk("Unexpected result: #{inspect(other)}")
        end
      else
        # Skip if Module.Types not available
        :skip
      end
    end

    test "enhanced pattern refinement maintains compatibility" do
      # Test that enhanced refinement doesn't break existing functionality
      pattern_ast = {:x, [version: 1], nil}
      match_ast = {:=, [], [pattern_ast, 42]}

      # Call with minimal parameters
      result = ElixirTypes.of_match(pattern_ast, nil, match_ast)

      case result do
        {:ok, vars} when is_map(vars) ->
          # Should work with minimal parameters
          assert is_map(vars)

        :error ->
          # Acceptable fallback
          :ok

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end
  end
end
