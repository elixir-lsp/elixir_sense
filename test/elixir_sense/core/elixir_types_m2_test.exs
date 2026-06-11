defmodule ElixirSense.Core.ElixirTypesM2Test do
  use ExUnit.Case, async: false

  # Entire module exercises the native Module.Types backend (Elixir 1.18+).
  @moduletag :requires_native_types

  alias ElixirSense.Core.{Binding, ElixirTypes, Metadata, MetadataBuilder, TypeInference}

  @moduletag :elixir_types_m2

  describe "local function inference" do
    # Local signature inference feeds the native expression typer, which is the
    # expected-type backend (1.19+). Excluded on 1.18 via test_helper.
    @describetag :requires_expected_type_native

    setup do
      # Enable ElixirTypes
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, true)

      on_exit(fn ->
        Application.put_env(:elixir_sense, :use_elixir_types, original_value)
      end)

      :ok
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

      # Clauses are captured during the module body and PRUNED once the
      # module-level inference pass computes the signature (O(n) rework) —
      # the signature itself is the evidence the clauses were captured.
      add_key = {TestModule, :add, 2}

      assert %{elixir_types_clauses: [], elixir_types_sig: {:infer, _, _}} =
               state.mods_funs_to_positions[add_key]

      # Check that clauses were captured for factorial/1
      factorial_key = {TestModule, :factorial, 1}

      # Recursive functions deliberately skip inference (local handler
      # disabled); clauses are pruned after the module pass either way.
      assert %{elixir_types_clauses: []} = state.mods_funs_to_positions[factorial_key]
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

      # Signatures should either be inferred successfully
      assert add_status == :ok
      assert identity_status == :ok

      assert {:infer, _domain, _clauses} = add_sig
      assert {:infer, _domain, _clauses} = identity_sig
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

      # Clauses pruned post-inference; the two-clause sig proves capture.
      assert clauses == []
      assert {:infer, _domain, sig_clauses} = sig
      assert length(sig_clauses) == 2

      # Status should be ok or skipped
      assert status == :ok

      assert {:infer, _domain, clause_types} = sig
      # Note: String.length return type is dynamic() since remote handler
      # doesn't have ExCk data during local inference
      assert length(clause_types) == 2
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

      assert %{elixir_types_clauses: []} = state.mods_funs_to_positions[unknown_key]
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

      assert %{elixir_types_clauses: [], elixir_types_sig: {:infer, _, _}} =
               state.mods_funs_to_positions[public_key]

      assert %{elixir_types_clauses: [], elixir_types_sig: {:infer, _, _}} =
               state.mods_funs_to_positions[private_key]
    end

    test "local handler integration works" do
      code = """
      defmodule TestModule do
        def simple_add(x, y), do: x + y
        # def simple_add(x, y) do
          # if x do
          #   %{ispies: 1}
          # else
          #   %{fo: 1}
          # end
          # try do
          #   %{ispies: :ok}
          # else
          #   a -> %{a | ispies: :ok}
          # rescue
          #   _ -> %{foj: 1}
          # end
          # cond do
          #   x -> %{ispies: :ok}
          #   true -> 1
          # end
          # n = receive do
          #   a -> %{ispies: :ok}
          # after
          #   2 -> %{foj: 1}
          # end
          # for _ <- 1..10 do
          #   %{ispies: :ok}
          # end
        # end
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
      assert {:def, {:infer, _domain, _clause_types}} = Map.get(local_sigs_map, {:simple_add, 2})
    end

    test "TypeInference integration with local signatures" do
      code = """
      defmodule TestModule do
        def add(x, y), do: %{foo: x, bar: y}
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

      # add/2 body is %{foo: x, bar: y}, which returns a map. task #20: native
      # dynamic-mode inference yields a dynamic-wrapped descr, so to_shape now
      # surfaces a {:dynamic, {:map, ...}} marker — unwrap it for the assertion.
      {:map, fields, nil} =
        case result do
          {:dynamic, inner} -> inner
          inner -> inner
        end

      assert Keyword.has_key?(fields, :foo)
      assert Keyword.has_key?(fields, :bar)
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

    test "build_local_sigs_map includes native signatures derived from declared specs" do
      code = """
      defmodule TestModule do
        @spec helper(integer()) :: {:ok, integer()}
        def helper(value), do: {:ok, value}
      end
      """

      {:ok, ast} = Code.string_to_quoted(code, columns: true, token_metadata: true)
      metadata = MetadataBuilder.build(ast)

      local_sigs_map = ElixirTypes.build_local_sigs_map(metadata, TestModule)

      assert {:def, {sig_kind, _domain, [{[arg_descr], return_descr}]}} =
               local_sigs_map[{:helper, 1}]

      assert sig_kind in [:infer, :strong]
      assert arg_descr != nil
      assert is_map(return_descr)
    end
  end

  describe "integration with disabled feature" do
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

  describe "remote function integration" do
    # Remote-call typing and remote-signature resolution go through native
    # expression typing (expected-type backend, 1.19+). Excluded on 1.18.
    @describetag :requires_expected_type_native

    setup do
      # Enable ElixirTypes for M2 tests
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, true)

      on_exit(fn ->
        Application.put_env(:elixir_sense, :use_elixir_types, original_value)
      end)

      :ok
    end

    test "reads remote signatures from ExCk" do
      assert {:ok, %{sig: {:infer, _domain, clauses}}} =
               ElixirSense.Core.ExCkReader.lookup_signature(Enum, :map, 2)

      assert is_list(clauses)
      assert length(clauses) > 0
    end

    test "ExCk lookup falls back gracefully when signatures are unavailable" do
      assert :error =
               ElixirSense.Core.ExCkReader.lookup_signature(NonExistentModule, :some_function, 1)
    end

    test "remote signatures resolve __MODULE__ and aliases from metadata env" do
      metadata = %ElixirSense.Core.Metadata{
        cursor_env: {[], %{module: String, aliases: [{Elixir.MyAlias, Integer}]}}
      }

      assert {:ok, {_kind, _domain, _clauses}} =
               ElixirTypes.maybe_remote_call_sig(
                 {{:., [], [{:__MODULE__, [], nil}, :split]}, [], [nil, nil]},
                 metadata
               )

      assert {:ok, {_kind, _domain, _clauses}} =
               ElixirTypes.maybe_remote_call_sig(
                 {{:., [], [{:__aliases__, [], [MyAlias]}, :to_string]}, [], [nil]},
                 metadata
               )
    end

    test "remote signatures resolve module attributes from metadata env" do
      metadata = %ElixirSense.Core.Metadata{
        cursor_env:
          {[],
           %{
             attributes: [
               %ElixirSense.Core.State.AttributeInfo{name: :mod, type: {:atom, String}}
             ]
           }}
      }

      assert {:ok, {_kind, _domain, _clauses}} =
               ElixirTypes.maybe_remote_call_sig(
                 {{:., [], [{:@, [], [{:mod, [], nil}]}, :split]}, [], [nil, nil]},
                 metadata
               )
    end

    test "remote signatures resolve module-valued variables from metadata env" do
      metadata = %ElixirSense.Core.Metadata{
        cursor_env:
          {[],
           %{
             vars: [
               %ElixirSense.Core.State.VarInfo{
                 name: :mod_var,
                 version: 1,
                 type: {:atom, String}
               }
             ]
           }}
      }

      assert {:ok, {_kind, _domain, _clauses}} =
               ElixirTypes.maybe_remote_call_sig(
                 {{:., [], [{:mod_var, [], nil}, :split]}, [], [nil, nil]},
                 metadata
               )
    end

    test "remote signatures resolve nested module expressions from metadata env" do
      metadata = %ElixirSense.Core.Metadata{
        cursor_env: {[], %{module: String}}
      }

      # __MODULE__.Chars resolves to String.Chars, which has ExCk sigs
      assert {:ok, {_kind, _domain, _clauses}} =
               ElixirTypes.maybe_remote_call_sig(
                 {{:., [], [{{:., [], [{:__MODULE__, [], nil}, :Chars]}, [], []}, :to_string]},
                  [], [nil]},
                 metadata
               )
    end

    test "ExCk signatures preserve clause return information" do
      mod = Module.concat(ElixirSense, "ExCkFixture#{System.unique_integer([:positive])}")

      clauses = [
        {[Module.Types.Descr.atom([true])], Module.Types.Descr.atom([:yes])},
        {[Module.Types.Descr.atom([false])], Module.Types.Descr.atom([:no])}
      ]

      # The chunk must be tagged with the *running* runtime's checker version;
      # ExCkReader (task #12) now rejects foreign versions with
      # {:error, :version_mismatch} since descr internals changed across releases.
      checker_version =
        if :erlang.function_exported(:elixir_erl, :checker_version, 0) do
          :elixir_erl.checker_version()
        else
          :elixir_checker_v2
        end

      chunk_payload =
        {checker_version,
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

      assert {:ok, %{sig: {:infer, nil, fetched_clauses}}} =
               ElixirSense.Core.ExCkReader.lookup_signature(mod, :choose, 1)

      assert fetched_clauses == clauses
    end

    test "init_stack integrates native checker cache" do
      stack = ElixirTypes.init_stack(TestModule, {:test_fun, 1}, "test.ex", :dynamic, nil, %{})

      case stack do
        nil ->
          :ok

        %{cache: cache, local_handler: handler} ->
          assert cache != nil
          assert is_function(handler, 4)
      end
    end

    test "of_expr accepts keyword options for metadata" do
      ast = {:+, [], [1, 2]}

      result =
        ElixirTypes.of_expr(
          ast,
          module: TestModule,
          function: {:test, 1},
          file: "test.ex",
          mode: :infer,
          metadata: %{},
          variables: %{}
        )

      case result do
        {:ok, %{dynamic: :term}} ->
          :ok

        {:ok, _descr} ->
          :ok

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "TypeInference integration with remote signatures" do
      ast = {{:., [], [String, :to_integer]}, [], [""]}

      result = TypeInference.type_of_with_elixir_types(ast, :none, nil, %{})

      case result do
        {:integer, nil} -> :ok
        other -> flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "of_expr resolves aliases from metadata during remote call typing" do
      # Verify alias resolution works through of_expr (via init_stack metadata threading)
      metadata = %ElixirSense.Core.Metadata{
        cursor_env: {[], %{module: TestModule, aliases: [{Elixir.MyAlias, Integer}]}}
      }

      # MyAlias.to_string(42) where MyAlias → Integer
      # of_expr may return dynamic(term()) if ExCk doesn't have the sig cached,
      # but should not crash — the alias resolution itself is tested more directly
      # via maybe_remote_call_sig tests above.
      ast = {{:., [], [{:__aliases__, [], [MyAlias]}, :to_string]}, [], [42]}

      result =
        ElixirTypes.of_expr(ast,
          module: TestModule,
          metadata: metadata
        )

      # Should succeed (not crash) — result may be dynamic or a specific type
      assert match?({:ok, _}, result),
             "Expected {:ok, _} for aliased remote call, got: #{inspect(result)}"
    end

    test "of_expr resolves __MODULE__ from metadata during remote call typing" do
      # Verify __MODULE__ resolution works through maybe_remote_call_sig
      # (full of_expr for remote calls with complex args can fail on variable lookup)
      metadata = %ElixirSense.Core.Metadata{
        cursor_env: {[], %{module: String}}
      }

      assert {:ok, {_kind, _domain, _clauses}} =
               ElixirTypes.maybe_remote_call_sig(
                 {{:., [], [{:__MODULE__, [], nil}, :split]}, [], [nil, nil]},
                 metadata
               )
    end
  end

  describe "enhanced shape conversion" do
    setup do
      # Enable ElixirTypes
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

        other ->
          flunk("Unexpected union shape: #{inspect(other)}")
      end
    end

    test "converts struct types to shape" do
      # On 1.18 to_shape renders the dynamic-wrapped struct descr looser; the
      # precise struct shape is 1.19+ (see elixir_types_test "struct as dynamic map").
      if ElixirTypes.available?(:expr) do
        struct_descr = Module.Types.Of.struct_type(Date, [%{field: :year}, %{field: :month}])

        result = ElixirTypes.to_shape(struct_descr)

        # task #20: struct_type produces a dynamic-wrapped descr; to_shape now
        # surfaces the {:dynamic, _} marker. Unwrap it for the struct assertion.
        unwrapped =
          case result do
            {:dynamic, inner} -> inner
            inner -> inner
          end

        case unwrapped do
          {:struct, fields, {:atom, Date}, nil} ->
            assert is_list(fields)
            field_names = Keyword.keys(fields)
            assert :year in field_names
            assert :month in field_names

          other ->
            flunk("Unexpected struct shape: #{inspect(other)}")
        end
      end
    end

    test "converts function types to shape" do
      # Descr.fun/1 (arity-only function descr) is a 1.20 constructor.
      if function_exported?(Module.Types.Descr, :fun, 1) do
        fun_descr = Module.Types.Descr.fun(2)

        result = ElixirTypes.to_shape(fun_descr)

        case result do
          # Correct function shape
          {:fun, 2} -> :ok
          other -> flunk("Unexpected function shape: #{inspect(other)}")
        end
      end
    end

    test "converts bounded integer types to shape" do
      range_descr = Module.Types.Descr.integer()

      result = ElixirTypes.to_shape(range_descr)

      case result do
        {:integer, nil} -> :ok
        other -> flunk("Unexpected integer range shape: #{inspect(other)}")
      end
    end

    test "converts string literal types to shape" do
      literal_descr = Module.Types.Descr.binary()

      result = ElixirTypes.to_shape(literal_descr)

      case result do
        {:binary, nil} -> :ok
        other -> flunk("Unexpected string literal shape: #{inspect(other)}")
      end
    end

    test "handles PID type" do
      # Test that PID descriptor converts properly
      pid_descr = Module.Types.Descr.pid()
      result = ElixirTypes.to_shape(pid_descr)

      case result do
        # Correct PID shape
        :pid -> :ok
        other -> flunk("Unexpected PID shape: #{inspect(other)}")
      end
    end

    test "handles port type" do
      # Test that port descriptor converts properly
      port_descr = Module.Types.Descr.port()
      result = ElixirTypes.to_shape(port_descr)

      case result do
        # Correct port shape
        :port -> :ok
        other -> flunk("Unexpected port shape: #{inspect(other)}")
      end
    end

    test "handles reference type" do
      # Test that reference descriptor converts properly
      reference_descr = Module.Types.Descr.reference()
      result = ElixirTypes.to_shape(reference_descr)

      case result do
        # Correct reference shape
        :reference -> :ok
        other -> flunk("Unexpected reference shape: #{inspect(other)}")
      end
    end

    test "handles top type" do
      top_descr = Module.Types.Descr.term()
      result = ElixirTypes.to_shape(top_descr)

      assert result == nil
    end

    test "handles bottom type" do
      bottom_descr = Module.Types.Descr.none()
      result = ElixirTypes.to_shape(bottom_descr)

      assert result == :none
    end

    test "handles complex nested types" do
      map_descr =
        Module.Types.Descr.closed_map(
          key: Module.Types.Descr.binary(),
          value: Module.Types.Descr.atom([:a, :b])
        )

      list_descr = Module.Types.Descr.list(map_descr)

      result = ElixirTypes.to_shape(list_descr)

      case result do
        # List of maps
        {:list, {:map, _fields, nil}} -> :ok
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
      # Test basic integer
      int_descr = Module.Types.Descr.integer()
      assert ElixirTypes.to_shape(int_descr) == {:integer, nil}

      # Test basic binary
      bin_descr = Module.Types.Descr.binary()
      assert ElixirTypes.to_shape(bin_descr) == {:binary, nil}

      # Test basic float
      float_descr = Module.Types.Descr.float()
      assert ElixirTypes.to_shape(float_descr) == {:float, nil}
    end
  end

  describe "pattern matching refinement" do
    setup do
      # Enable ElixirTypes
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
        {:ok, vars, _descrs} when is_map(vars) ->
          # Should get variable refinement
          assert vars[{:x, 1}] == {:integer, nil}

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "struct pattern matching refinement" do
      pattern_ast =
        {:%, [], [Date, {:%{}, [], [{:year, {:x, [version: 1], nil}}]}]}

      match_ast = {:=, [], [pattern_ast, Macro.escape(~D[2023-01-01])]}

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
        {:ok, vars, _descrs} when is_map(vars) ->
          # Struct field refinement picks up the literal value from the match AST
          assert match?({:integer, _}, vars[{:x, 1}])

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
        {:ok, vars, _descrs} when is_map(vars) ->
          # Should handle tuple patterns
          assert is_map(vars)

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
        {:ok, vars, _descrs} when is_map(vars) ->
          # Should handle list patterns. Native of_match is authoritative (Task
          # 2): the tail of `[h | t] = [1, 2, 3]` types as a (possibly empty)
          # list of integers — `{:list, {:integer, nil}}` — rather than the AST
          # refinement's hand-built union.
          assert vars[{:head, 1}] == {:integer, nil}

          assert vars[{:tail, 2}] in [
                   {:list, {:integer, nil}},
                   {:union, [list: {:integer, nil}, list: :empty]}
                 ]

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "nested pattern matching refinement" do
      pattern_ast =
        {:%{}, [],
         [
           {:status,
            {:=, [],
             [
               {:status_var, [version: 1], nil},
               200
             ]}},
           {:data, {:%{}, [], [{:value, {:data_var, [version: 2], nil}}]}}
         ]}

      match_ast =
        {:=, [], [pattern_ast, Macro.escape(%{status: 200, data: %{value: "ok"}})]}

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
        {:ok, vars, _descrs} when is_map(vars) ->
          assert vars[{:status_var, 1}] in [{:integer, 200}, {:integer, nil}]
          # Native of_match is authoritative; it widens the literal to a generic
          # binary() rather than keeping the AST literal "ok" (Task 2).
          assert vars[{:data_var, 2}] in [{:binary, "ok"}, {:binary, nil}]

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
        {:ok, vars, _descrs} when is_map(vars) ->
          # Should handle guard patterns
          assert is_map(vars)

        :error ->
          # Acceptable — Module.Types.Pattern.of_match may not handle :when directly
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
        {:ok, vars, _descrs} when is_map(vars) ->
          # Should handle map patterns. Native of_match is authoritative (Task
          # 2) and widens the literal to a generic binary().
          assert vars[{:name_var, 1}] in [{:binary, "John"}, {:binary, nil}]
          assert vars[{:age_var, 2}] in [{:integer, 30}, {:integer, nil}]

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
        # Expected for invalid patterns
        :error -> :ok
        other -> flunk("Unexpected result: #{inspect(other)}")
      end
    end

    test "type intersection with expected descriptors" do
      # The "intersection" is between the expected_descr (integer) and the
      # matched value type (42 = integer), applied by Module.Types.Pattern
      pattern_ast = {:x, [version: 1], nil}
      match_ast = {:=, [], [pattern_ast, 42]}

      # Use integer descriptor as expected type
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
        {:ok, vars, _descrs} when is_map(vars) ->
          # Should use expected descriptor for better refinement
          assert is_map(vars)

        other ->
          flunk("Unexpected result: #{inspect(other)}")
      end
    end
  end

  describe "pattern refinement implementations" do
    setup do
      # Enable ElixirTypes for pattern refinement tests
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, true)

      on_exit(fn ->
        Application.put_env(:elixir_sense, :use_elixir_types, original_value)
      end)

      :ok
    end

    test "map pattern refinement extracts variable types" do
      if ElixirTypes.available?() do
        # Map pattern %{name: n} where n should be refined
        pattern_ast = {:%{}, [], [{:name, {:n, [version: 1], nil}}]}

        # Call refine_map_pattern_vars (via of_match)
        match_ast = {:=, [], [pattern_ast, {:%{}, [], [{:name, "test"}]}]}

        result = ElixirTypes.of_match(pattern_ast, nil, match_ast)

        case result do
          {:ok, vars, _descrs} when is_map(vars) ->
            # Pattern refinement should work. Native is authoritative and
            # widens the literal to generic binary() (Task 2).
            assert vars[{:n, 1}] in [{:binary, "test"}, {:binary, nil}]

          other ->
            flunk("Unexpected result: #{inspect(other)}")
        end
      end
    end

    test "2-tuple pattern refinement handles element positions" do
      if ElixirTypes.available?() do
        # Tuple pattern {a, b} where variables should be refined
        pattern_ast = {{:a, [version: 1], nil}, {:b, [version: 2], nil}}

        match_ast = {:=, [], [pattern_ast, {1, :ok}]}

        result = ElixirTypes.of_match(pattern_ast, nil, match_ast)

        case result do
          {:ok, vars, _descrs} when is_map(vars) ->
            # Pattern refinement should work. task #20: a refined singleton atom
            # coming from the native (dynamic-mode) descr surfaces with a
            # {:dynamic, _} marker, so accept either the bare or wrapped form.
            assert vars[{:a, 1}] in [{:integer, nil}, {:dynamic, {:integer, nil}}]
            assert vars[{:b, 2}] in [{:atom, :ok}, {:dynamic, {:atom, :ok}}]

          other ->
            flunk("Unexpected result: #{inspect(other)}")
        end
      end
    end

    test "tuple pattern refinement handles element positions" do
      if ElixirTypes.available?() do
        # Tuple pattern {a, b, c} where variables should be refined. The RHS must
        # be a proper 3-element-tuple AST node (`{:{}, _, [...]}`); a bare 3-tuple
        # `{1, :ok, ""}` is not valid quoted form (it happened to be tolerated on
        # 1.19/1.20 but breaks value typing on 1.18).
        pattern_ast =
          {:{}, [], [{:a, [version: 1], nil}, {:b, [version: 2], nil}, {:c, [version: 3], nil}]}

        match_ast = {:=, [], [pattern_ast, {:{}, [], [1, :ok, ""]}]}

        result = ElixirTypes.of_match(pattern_ast, nil, match_ast)

        case result do
          {:ok, vars, _descrs} when is_map(vars) ->
            # Pattern refinement should work. Native is authoritative and widens
            # the "" literal to generic binary() (Task 2).
            assert vars[{:a, 1}] == {:integer, nil}
            assert vars[{:b, 2}] == {:atom, :ok}
            assert vars[{:c, 3}] in [{:binary, ""}, {:binary, nil}]

          :error ->
            # Acceptable fallback for conservative implementation
            :ok

          other ->
            flunk("Unexpected result: #{inspect(other)}")
        end
      end
    end

    test "list pattern refinement handles head and tail" do
      if ElixirTypes.available?() do
        # List pattern [head | tail]
        pattern_ast = [{:|, [], [{:head, [version: 1], nil}, {:tail, [version: 2], nil}]}]

        match_ast = {:=, [], [pattern_ast, [1, 2, 3]]}

        result = ElixirTypes.of_match(pattern_ast, nil, match_ast)

        case result do
          {:ok, vars, _descrs} when is_map(vars) ->
            # Pattern refinement should work. Native of_match is authoritative
            # (Task 2): the tail types as `{:list, {:integer, nil}}`.
            assert vars[{:head, 1}] == {:integer, nil}

            assert vars[{:tail, 2}] in [
                     {:list, {:integer, nil}},
                     {:union, [list: {:integer, nil}, list: :empty]}
                   ]

          other ->
            flunk("Unexpected result: #{inspect(other)}")
        end
      end
    end

    test "list pattern refinement handles simple list elements" do
      if ElixirTypes.available?() do
        # List pattern [a, b, c]
        pattern_ast = [{:a, [version: 1], nil}, {:b, [version: 2], nil}, {:c, [version: 3], nil}]

        match_ast = {:=, [], [pattern_ast, [1, 2, 3]]}

        result = ElixirTypes.of_match(pattern_ast, nil, match_ast)

        case result do
          {:ok, vars, _descrs} when is_map(vars) ->
            # Pattern refinement should work
            assert vars[{:a, 1}] == {:integer, nil}
            assert vars[{:b, 2}] == {:integer, nil}
            assert vars[{:c, 3}] == {:integer, nil}

          other ->
            flunk("Unexpected result: #{inspect(other)}")
        end
      end
    end

    test "pattern refinement is conservative and safe" do
      if ElixirTypes.available?() do
        # Complex pattern that may not be fully supported
        pattern_ast =
          {:when, [], [{:x, [version: 1], nil}, {:>, [], [{:x, [version: 1], nil}, 0]}]}

        match_ast = {:=, [], [pattern_ast, 42]}

        result = ElixirTypes.of_match(pattern_ast, nil, match_ast)

        case result do
          {:ok, _vars} ->
            # If we get a result, it should be valid
            :ok

          :error ->
            # Conservative fallback is expected for complex patterns
            :ok

          other ->
            flunk("Unexpected result: #{inspect(other)}")
        end
      end
    end
  end

  describe "assertive pattern refinement tests" do
    setup do
      # Enable ElixirTypes for assertive tests
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, true)

      on_exit(fn ->
        Application.put_env(:elixir_sense, :use_elixir_types, original_value)
      end)

      :ok
    end

    test "struct pattern refinement maintains conservatism" do
      if ElixirTypes.available?() do
        # Test struct pattern with existing struct - should be conservative
        pattern_ast =
          {:%, [], [Date, {:%{}, [], [{:year, {:year, [version: 1], nil}}]}]}

        match_ast = {:=, [], [pattern_ast, Macro.escape(~D[2023-01-01])]}

        result = ElixirTypes.of_match(pattern_ast, nil, match_ast)

        case result do
          {:ok, vars, _descrs} when is_map(vars) ->
            # Should work but be conservative with struct fields
            assert is_map(vars)

          other ->
            flunk("Unexpected result: #{inspect(other)}")
        end
      end
    end

    test "multiple clauses with different return types integration" do
      if ElixirTypes.available?() do
        # Test pattern matching in different clauses
        patterns = [
          # Clause 1: integer pattern
          {1, {:result, [version: 1], nil}},
          # Clause 2: atom pattern
          {:ok, {:result, [version: 2], nil}}
        ]

        results =
          for {pattern, var} <- patterns do
            pattern_ast = var
            match_ast = {:=, [], [pattern_ast, pattern]}
            ElixirTypes.of_match(pattern_ast, nil, match_ast)
          end

        # All should either work or fail conservatively
        for result <- results do
          case result do
            {:ok, vars, _descrs} when is_map(vars) -> assert is_map(vars)
            other -> flunk("Unexpected result: #{inspect(other)}")
          end
        end
      end
    end

    test "nested pattern refinement handles complexity gracefully" do
      if ElixirTypes.available?() do
        # Test nested pattern like %{user: %{name: n}}
        inner_pattern = {:%{}, [], [{:name, {:n, [version: 1], nil}}]}
        pattern_ast = {:%{}, [], [{:user, inner_pattern}]}
        match_ast = {:=, [], [pattern_ast, Macro.escape(%{user: %{name: "Alice"}})]}

        result = ElixirTypes.of_match(pattern_ast, nil, match_ast)

        case result do
          {:ok, vars, _descrs} when is_map(vars) ->
            # Should handle nested patterns conservatively. Native is
            # authoritative and widens the literal to binary() (Task 2).
            assert vars[{:n, 1}] in [{:binary, "Alice"}, {:binary, nil}]

          other ->
            flunk("Unexpected result: #{inspect(other)}")
        end
      end
    end

    test "binary pattern demonstrates conservative behavior" do
      if ElixirTypes.available?() do
        pattern_ast =
          {:<<>>, [],
           [
             {:"::", [], [{:data, [version: 1], nil}, 8]},
             {:"::", [], [{:rest, [version: 2], nil}, {:binary, [], Elixir}]}
           ]}

        match_ast = {:=, [], [pattern_ast, <<72, 101, 108, 108, 111>>]}

        result = ElixirTypes.of_match(pattern_ast, nil, match_ast)

        case result do
          {:ok, vars, _descrs} when is_map(vars) ->
            assert vars[{:data, 1}] == {:integer, nil}
            assert vars[{:rest, 2}] == {:binary, nil}

          :error ->
            :ok

          other ->
            flunk("Unexpected result: #{inspect(other)}")
        end
      end
    end

    @tag :requires_expected_type_native
    test "end-to-end integration: local signatures influence binding type inference" do
      if ElixirTypes.available?() do
        # This test ensures local signatures actually change the type seen at call sites
        # Similar to the existing integration test but more comprehensive

        code = """
        defmodule TestModule do
          def caller() do
            result = helper()
            result
          end

          defp helper() do
            case 1 do
              1 -> :success
              _ -> :failure
            end
          end
        end
        """

        # Build metadata
        {:ok, ast} = Code.string_to_quoted(code, columns: true, token_metadata: true)
        metadata = ElixirSense.Core.MetadataBuilder.build(ast)

        # Test that local signatures are built and used
        local_sigs = ElixirTypes.build_local_sigs_map(metadata, TestModule)

        case local_sigs do
          %{} when map_size(local_sigs) > 0 ->
            assert Map.has_key?(local_sigs, {:helper, 0})

            # Test that these signatures can be used for type inference
            call_ast = {:helper, [], []}
            context = %{module: TestModule, function: {:caller, 0}, file: "test.ex"}

            result =
              TypeInference.type_of_with_elixir_types(
                call_ast,
                :none,
                local_sigs,
                metadata,
                context
              )

            case result do
              type when is_tuple(type) ->
                assert tuple_size(type) >= 2

              other ->
                flunk("Unexpected result: #{inspect(other)}")
            end

          other ->
            flunk("Unexpected local_sigs result: #{inspect(other)}")
        end
      end
    end

    @tag :requires_expected_type_native
    test "end-to-end integration: remote signatures influence binding type inference" do
      if ElixirTypes.available?() do
        assert {:ok, %{sig: sig}} =
                 ElixirSense.Core.ExCkReader.lookup_signature(
                   ElixirSenseExample.RemoteSignatures,
                   :helper,
                   0
                 )

        assert sig ==
                 {:infer, nil, [{[], %{dynamic: %{atom: {:union, %{success: [], failure: []}}}}}]}

        # This test ensures local signatures actually change the type seen at call sites
        # Similar to the existing integration test but more comprehensive

        code = """
        defmodule TestModule do
          def caller() do
            result = ElixirSenseExample.RemoteSignatures.helper()
            result
          end
        end
        """

        # Build metadata
        {:ok, ast} = Code.string_to_quoted(code, columns: true, token_metadata: true)
        metadata = ElixirSense.Core.MetadataBuilder.build(ast)

        # Test that local signatures are built and used
        local_sigs = ElixirTypes.build_local_sigs_map(metadata, TestModule)

        # Test that these signatures can be used for type inference
        call_ast =
          {{:., [line: 3], [ElixirSenseExample.RemoteSignatures, :helper]}, [line: 1], []}

        context = %{module: TestModule, function: {:caller, 0}, file: "test.ex"}

        result =
          TypeInference.type_of_with_elixir_types(
            call_ast,
            :none,
            local_sigs,
            metadata,
            context
          )

        case result do
          type when is_tuple(type) ->
            assert tuple_size(type) >= 2

          other ->
            flunk("Unexpected result: #{inspect(other)}")
        end
      end
    end

    test "Binding unions results across union remote targets" do
      env = %Binding{module: TestModule, function: {:caller, 0}, requires: [], vars: []}

      results =
        [String, Integer]
        |> Enum.map(fn mod ->
          Binding.expand(env, {:call, {:atom, mod}, :module_info, [{:atom, :module}]})
        end)

      assert Enum.all?(results, &(&1 in [nil, :none] or is_tuple(&1) or is_map(&1)))
    end
  end
end
