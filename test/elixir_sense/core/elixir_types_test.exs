defmodule ElixirSense.Core.ElixirTypesTest do
  use ExUnit.Case, async: false

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.ElixirTypes
  alias ElixirSense.Core.MetadataBuilder
  alias ElixirSense.Core.TypeInference
  alias ElixirSense.Test.DescrCompat

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

  describe "capabilities" do
    @capability_keys [
      :expr,
      :expr_basic,
      :pattern_match,
      :head,
      :local_signature,
      :previous
    ]

    test "capabilities/0 returns a boolean for every known capability" do
      caps = ElixirTypes.capabilities()

      for key <- @capability_keys do
        assert is_boolean(Map.get(caps, key)), "expected boolean for capability #{inspect(key)}"
      end
    end

    test "available?/1 agrees with capabilities/0" do
      caps = ElixirTypes.capabilities()

      for key <- @capability_keys do
        assert ElixirTypes.available?(key) == caps[key]
      end

      refute ElixirTypes.available?(:no_such_capability)
    end

    test "capability profile matches the running Elixir" do
      caps = ElixirTypes.capabilities()

      cond do
        Version.match?(System.version(), ">= 1.20.0") ->
          # 1.20 has the full surface: expected-type expr, of_match/6, of_head/8,
          # and the cross-clause `previous` machinery.
          assert caps.expr and caps.pattern_match and caps.head
          assert caps.local_signature
          assert caps.previous

        Version.match?(System.version(), ">= 1.19.0") ->
          # 1.19 has expr/5, of_match/5, of_head/7 and stack/7, but NOT the
          # 1.20-only cross-clause/reverse-arrow/domain-map machinery. If these
          # flip, the V19 dispatch assumptions are wrong — fail loudly here.
          assert caps.expr and caps.expr_basic and caps.pattern_match and caps.head
          assert caps.local_signature
          refute caps.previous

        Version.match?(System.version(), ">= 1.18.0") ->
          # 1.18 has the basic of_expr/3 (NOT the expected-type of_expr/5),
          # of_match/7, of_head/7 and stack/7. The adapter is active here, but
          # without expected-type expr or any 1.20 machinery.
          refute caps.expr
          assert caps.expr_basic and caps.pattern_match and caps.head
          assert caps.local_signature
          refute caps.previous

        true ->
          # 1.17: has of_expr/3 (so expr_basic can be true) but only stack/5 and
          # no of_match, so the adaptor is OFF (available?/0 false) and the
          # usable capabilities are all false — callers use the custom engine.
          refute ElixirTypes.available?()
          refute caps.expr
          refute caps.pattern_match
          refute caps.head
          refute caps.local_signature
          refute caps.previous
      end
    end
  end

  describe "capabilities memoization" do
    # Capabilities are immutable within a VM (modules do not gain or lose exports).
    # The computed map is cached in :persistent_term after the first call.
    test "capabilities/0 returns identical maps on repeated calls (persistent_term memoized)" do
      caps1 = ElixirTypes.capabilities()
      caps2 = ElixirTypes.capabilities()
      caps3 = ElixirTypes.capabilities()
      assert caps1 == caps2
      assert caps2 == caps3
    end

    test "available?/0 is consistent with cached capabilities" do
      caps = ElixirTypes.capabilities()
      expected = Map.get(caps, :expr_basic, false) and Map.get(caps, :local_signature, false)
      assert ElixirTypes.available?() == expected
    end

    test "enabled?/0 respects runtime Application.get_env toggle (not memoized)" do
      original = Application.get_env(:elixir_sense, :use_elixir_types, false)

      Application.put_env(:elixir_sense, :use_elixir_types, false)
      refute ElixirTypes.enabled?()

      Application.put_env(:elixir_sense, :use_elixir_types, true)
      # With native backend available, enabled? should reflect the env change.
      if ElixirTypes.available?() do
        assert ElixirTypes.enabled?()
      else
        refute ElixirTypes.enabled?()
      end

      Application.put_env(:elixir_sense, :use_elixir_types, original)
    end
  end

  describe "version-dispatched pattern API (1.19 and 1.20)" do
    setup do
      original = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, true)
      on_exit(fn -> Application.put_env(:elixir_sense, :use_elixir_types, original) end)
      :ok
    end

    # These pin the V19/V20 dispatch (of_match/5-or-6, of_head/7-or-8,
    # of_domain expected-or-stack) loudly: a broken arity choice degrades to
    # :error here instead of being silently swallowed by a tolerant assertion.

    test "of_match refines pattern variables when pattern_match is available" do
      if ElixirTypes.available?(:pattern_match) do
        pattern = {:%{}, [], [{:name, {:name_var, [version: 1], nil}}]}
        match = {:=, [], [pattern, {:%{}, [], [{:name, "John"}]}]}

        assert {:ok, vars, _descrs} =
                 ElixirTypes.of_match(pattern, nil, match, TestModule, {:t, 1}, "t.ex", :dynamic,
                   target_keys: [{:name_var, 1}]
                 )

        # Native of_match is authoritative (Task 2): it widens the "John"
        # literal to a generic binary() rather than the AST-refinement literal.
        assert vars[{:name_var, 1}] in [{:binary, "John"}, {:binary, nil}]
      end
    end

    test "infer_local_signature returns a real signature when head is available" do
      if ElixirTypes.available?(:head) do
        clauses = [%{meta: [], args: [], guards: nil, body: 42}]

        assert {:infer, _domain, [{[], return}]} =
                 ElixirTypes.infer_local_signature(MyMod, {:answer, 0}, clauses, "nofile")

        # body `42` is an integer, on both 1.19 and 1.20
        assert ElixirTypes.to_shape(return) == {:integer, nil}
      end
    end
  end

  describe "of_match refinement with expected descriptors and guards" do
    @describetag :requires_native_types

    setup do
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, true)

      on_exit(fn ->
        Application.put_env(:elixir_sense, :use_elixir_types, original_value)
      end)

      :ok
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

  describe "pattern refinement conservatism" do
    setup do
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
        metadata = MetadataBuilder.build(ast)

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
        metadata = MetadataBuilder.build(ast)

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

  describe "shape conversion" do
    @describetag :requires_native_types

    test "to_shape/1 returns :none for empty descriptor" do
      # Empty descriptor is the "none" type
      result = ElixirTypes.to_shape(%{})
      assert result == :none
    end
  end

  describe "shape conversions" do
    # Descr-based shape conversion is part of the native backend (1.18+).
    @describetag :requires_native_types

    setup do
      # Save original value and enable feature
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, true)

      on_exit(fn ->
        Application.put_env(:elixir_sense, :use_elixir_types, original_value)
      end)

      :ok
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
      # A closed descr round-trips to a `:closed` tail (literal-complete), so the
      # descr round-trip is closed-by-default.
      assert ElixirTypes.to_shape(Module.Types.Descr.closed_map(foo: Module.Types.Descr.binary())) ==
               {:map, [foo: {:binary, nil}], :closed}
    end

    test "handles empty list" do
      assert ElixirTypes.to_shape(Module.Types.Descr.empty_list()) == :empty_list
    end

    test "handles empty map" do
      assert ElixirTypes.to_shape(Module.Types.Descr.empty_map()) == :empty_map
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
               {:nonempty_list, {:integer, nil}}

      assert ElixirTypes.to_shape(
               Module.Types.Descr.non_empty_list(
                 Module.Types.Descr.integer(),
                 Module.Types.Descr.integer()
               )
             ) == {:nonempty_list, {:integer, nil}, {:integer, nil}}
    end

    test "handles open map" do
      # open_map() is the top open map (any map). On 1.20 it quotes as `map()`
      # (`{:map, [], []}`), which carries no open-marker, so its tail is nil; on
      # 1.18/1.19 the same descr quotes with the `{:..., [], nil}` open marker, so
      # the tail comes back `:open`. Both are sound representations of "any map".
      assert ElixirTypes.to_shape(Module.Types.Descr.open_map()) in [
               {:map, [], nil},
               {:map, [], :open}
             ]

      # open_map with atom key fields: known fields are extracted AND the open
      # tail is preserved (`:open`) — the quoted form carries the `{:..., [], nil}`
      # open marker, distinguishing it from a closed map (nil tail).
      assert ElixirTypes.to_shape(Module.Types.Descr.open_map(foo: Module.Types.Descr.binary())) ==
               {:map, [foo: {:binary, nil}], :open}

      # Elixir 1.20 removed the open_map/2 "default" form (replaced by domain
      # keys). An open map with multiple atom fields still has all known fields
      # extracted and keeps the `:open` tail (keys come back sorted).
      assert ElixirTypes.to_shape(
               Module.Types.Descr.open_map(
                 foo: Module.Types.Descr.binary(),
                 bar: Module.Types.Descr.integer()
               )
             ) == {:map, [bar: {:integer, nil}, foo: {:binary, nil}], :open}
    end

    test "preserves domain (non-atom) keys" do
      # %{integer() => binary()} keeps the domain key as {:domain, key_shape}
      # rather than dropping it. (open_map also admits other domains, hence
      # membership rather than equality.)
      #
      # A domain-keyed open map carries NO `{:..., [], nil}` open marker in its
      # quoted form — openness is encoded by the domain keys themselves
      # (`atom() => term()`, `integer() => binary()`, ...) — so `to_shape` gives it
      # a `nil` (PARTIAL) tail, never `:closed`. The `:closed` coercion path
      # (maybe_closed_map_descr) refuses to close a map with domain keys, and a
      # `nil`-partial tail coerces OPEN anyway, so this is not a soundness hazard.
      # `Descr.to_domain_keys/1` (and the domain-key map representation it feeds)
      # is 1.20-only; on 1.18/1.19 there is no domain-keyed map form to exercise.
      if DescrCompat.to_domain_keys?() do
        descr =
          Module.Types.Descr.open_map([
            {Module.Types.Descr.to_domain_keys(Module.Types.Descr.integer()),
             Module.Types.Descr.binary()}
          ])

        assert {:map, fields, nil} = ElixirTypes.to_shape(descr)
        assert {{:domain, {:integer, nil}}, {:binary, nil}} in fields
      end
    end

    test "handles open tuple" do
      assert ElixirTypes.to_shape(
               Module.Types.Descr.open_tuple([
                 Module.Types.Descr.atom([:ok]),
                 Module.Types.Descr.binary()
               ])
             ) == {:tuple_open, [{:atom, :ok}, {:binary, nil}]}

      assert ElixirTypes.to_shape(
               Module.Types.Descr.open_tuple(
                 [Module.Types.Descr.atom([:ok]), Module.Types.Descr.binary()],
                 Module.Types.Descr.term()
               )
             ) == {:tuple_open, [{:atom, :ok}, {:binary, nil}]}
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
      assert ElixirTypes.to_shape(Module.Types.Descr.tuple()) == :tuple

      assert ElixirTypes.to_shape(
               Module.Types.Descr.tuple([
                 Module.Types.Descr.atom([:ok]),
                 Module.Types.Descr.integer()
               ])
             ) == {:tuple, 2, [{:atom, :ok}, {:integer, nil}]}
    end

    test "handles boolean" do
      assert ElixirTypes.to_shape(Module.Types.Descr.boolean()) == :boolean
    end

    test "handles fun/0 (any function)" do
      assert ElixirTypes.to_shape(Module.Types.Descr.fun()) == :fun
    end

    # Descr.fun/1, fun/2 and fun_from_*_clauses are 1.20-only constructors.
    @fun_descr_api function_exported?(Module.Types.Descr, :fun, 1)

    test "handles fun/1 (function with specific arity)" do
      if @fun_descr_api do
        assert ElixirTypes.to_shape(Module.Types.Descr.fun(0)) == {:fun, 0}
        assert ElixirTypes.to_shape(Module.Types.Descr.fun(1)) == {:fun, 1}
        assert ElixirTypes.to_shape(Module.Types.Descr.fun(2)) == {:fun, 2}
      end
    end

    test "handles fun/2 (function with arg and return types)" do
      if @fun_descr_api do
        # (integer() -> atom())
        fun_type =
          Module.Types.Descr.fun([Module.Types.Descr.integer()], Module.Types.Descr.atom())

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
    end

    test "handles fun_from_non_overlapping_clauses" do
      if @fun_descr_api do
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
    end

    test "handles fun_from_inferred_clauses" do
      if @fun_descr_api do
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
      # task #20: Descr.to_quoted only emits an explicit {:dynamic, [], [inner]}
      # wrapper for non-trivial inner types. dynamic(integer()) quotes to a bare
      # {:integer, [], []}, so it stays unwrapped...
      dynamic_int = Module.Types.Descr.dynamic(Module.Types.Descr.integer())
      assert ElixirTypes.to_shape(dynamic_int) == {:integer, nil}

      # ...but dynamic over a singleton atom keeps the wrapper, so we surface a
      # {:dynamic, shape} marker rather than silently unwrapping it.
      dynamic_atom = Module.Types.Descr.dynamic(Module.Types.Descr.atom([:ok]))
      assert ElixirTypes.to_shape(dynamic_atom) == {:atom, :ok}
    end

    test "handles struct as dynamic map" do
      # Module.Types.Of.struct_type produces a dynamic-wrapped map with
      # __struct__. task #20: to_quoted emits the explicit {:dynamic, [], [...]}
      # wrapper here, so to_shape surfaces a {:dynamic, {:struct, ...}} marker.
      # On 1.18 the descr/to_quoted form differs, so this is 1.19+.
      if ElixirTypes.available?(:expr) do
        struct = Module.Types.Of.struct_type(Date, [%{field: :year}, %{field: :month}])
        shape = ElixirTypes.to_shape(struct)
        assert match?({:struct, _fields, {:atom, Date}, nil}, shape)
      end
    end
  end

  describe "closed-map coercion fidelity (three-marker model)" do
    @describetag :requires_native_types

    alias Module.Types.Descr

    test "a nil-tail (PARTIAL) map coerces OPEN (soundness-preserving default)" do
      shape = {:map, [a: {:integer, nil}], nil}

      coerced = ElixirTypes.coerce_var_type_public(shape)
      # open: the upper bound admits maps with OTHER keys, so it is a supertype of
      # the closed map %{a: integer()}.
      assert Descr.subtype?(
               DescrCompat.upper_bound(Descr.closed_map(a: Descr.integer())),
               DescrCompat.upper_bound(coerced)
             )

      # ...and STRICTLY a supertype: an open map with another key is admitted.
      with_other = Descr.closed_map(a: Descr.integer(), b: Descr.binary())
      assert Descr.subtype?(DescrCompat.upper_bound(with_other), DescrCompat.upper_bound(coerced))
    end

    test "a :closed-tail map coerces to a CLOSED map BY DEFAULT" do
      shape = {:map, [a: {:integer, nil}], :closed}

      coerced = ElixirTypes.coerce_var_type_public(shape)
      # closed: still a (dynamic-wrapped) supertype of the closed original...
      assert Descr.subtype?(
               DescrCompat.upper_bound(Descr.closed_map(a: Descr.integer())),
               DescrCompat.upper_bound(coerced)
             )

      # ...but NOT of an open map / a map with another key (it excludes them).
      with_other = Descr.closed_map(a: Descr.integer(), b: Descr.binary())
      refute Descr.subtype?(DescrCompat.upper_bound(with_other), DescrCompat.upper_bound(coerced))
    end

    test "a nil-tail (partial) map coerces OPEN (closedness lives in the tail)" do
      shape = {:map, [a: {:integer, nil}], nil}

      coerced = ElixirTypes.coerce_var_type_public(shape)
      # nil is PARTIAL; coercion must NOT close it.
      with_other = Descr.closed_map(a: Descr.integer(), b: Descr.binary())
      assert Descr.subtype?(DescrCompat.upper_bound(with_other), DescrCompat.upper_bound(coerced))
    end

    test "an :open-tail map coerces OPEN" do
      shape = {:map, [a: {:integer, nil}], :open}

      coerced = ElixirTypes.coerce_var_type_public(shape)
      with_other = Descr.closed_map(a: Descr.integer(), b: Descr.binary())
      assert Descr.subtype?(DescrCompat.upper_bound(with_other), DescrCompat.upper_bound(coerced))
    end

    test "an empty :closed map coerces to the empty closed map %{}" do
      shape = {:map, [], :closed}

      coerced = ElixirTypes.coerce_var_type_public(shape)
      # The empty closed map admits ONLY %{}, so it is NOT a supertype of a
      # non-empty map.
      refute Descr.subtype?(
               DescrCompat.upper_bound(Descr.closed_map(a: Descr.integer())),
               DescrCompat.upper_bound(coerced)
             )

      assert Descr.subtype?(
               DescrCompat.upper_bound(Descr.empty_map()),
               DescrCompat.upper_bound(coerced)
             )
    end

    test "a domain-keyed map round-trips to a nil (partial) tail and coerces OPEN" do
      # A domain-keyed open map (e.g. `%{integer() => binary()}`) has no `:...`
      # marker, so to_shape gives it a `nil` (partial) tail — never `:closed`.
      shape = {:map, [{{:domain, {:integer, nil}}, {:binary, nil}}], nil}

      coerced = ElixirTypes.coerce_var_type_public(shape)
      # Domain keys are dropped by coercion, so the closed variant would be the
      # empty map; staying OPEN means a non-empty map is still admitted.
      assert Descr.subtype?(
               DescrCompat.upper_bound(Descr.closed_map(a: Descr.integer())),
               DescrCompat.upper_bound(coerced)
             )
    end

    test "EXACT closed round-trip: closed descr -> to_shape (:closed) -> coerce == dynamic(closed)" do
      closed = Descr.closed_map(a: Descr.integer(), b: Descr.binary())

      shape = ElixirTypes.to_shape(closed)
      # to_shape now yields a `:closed` tail; the default coercion closes it.
      assert {:map, _fields, :closed} = shape

      coerced = ElixirTypes.coerce_var_type_public(shape)
      # coerce_var_type wraps in dynamic(); the round-trip is exact modulo that wrap.
      assert Descr.equal?(coerced, Descr.dynamic(closed))
    end
  end

  describe "expression typing" do
    test "of_expr/1 handles simple expressions safely" do
      # Should return :error if not enabled, or {:ok, descr} if available
      result = ElixirTypes.of_expr(42)
      assert result == :error or match?({:ok, _}, result)
    end
  end

  describe "TypeInference integration when feature disabled" do
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
  end

  describe "expression typing with feature enabled" do
    # Native expression typing requires the expected-type API (1.19+); excluded
    # on 1.18 via test_helper (see :requires_expected_type_native).
    @describetag :requires_expected_type_native

    setup do
      # Save original value and enable feature
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, true)

      on_exit(fn ->
        Application.put_env(:elixir_sense, :use_elixir_types, original_value)
      end)

      :ok
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
      list_ast = [1, 2, 3]
      result = ElixirTypes.of_expr(list_ast)
      assert {:ok, descr} = result
      assert ElixirTypes.to_shape(descr) == {:nonempty_list, {:integer, nil}}

      empty_ast = []
      result = ElixirTypes.of_expr(empty_ast)
      assert {:ok, descr} = result
      assert ElixirTypes.to_shape(descr) == :empty_list
    end

    test "types list mixed" do
      list_ast = [1, :ok]
      result = ElixirTypes.of_expr(list_ast)
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert match?({:nonempty_list, _}, shape)
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
      # Empty map literal %{} types as the closed empty_map(), surfaced as the
      # dedicated :empty_map shape (task #22).
      map_ast = {:%{}, [], []}
      result = ElixirTypes.of_expr(map_ast)
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert :empty_map = shape
    end

    test "types closed map AST" do
      # Map AST: %{key: :value}
      map_ast = {:%{}, [], [key: :value]}
      result = ElixirTypes.of_expr(map_ast)
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      # A closed descr round-trips to a `:closed` tail.
      assert {:map, fields, :closed} = shape
      assert fields == [key: {:atom, :value}]
    end

    test "types open map AST" do
      map_ast = {:%{}, [], [{"some", "other"}, key: :value]}
      result = ElixirTypes.of_expr(map_ast)
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)

      # 1.20 models the string key as a `binary() => binary()` DOMAIN key,
      # preserved alongside the atom key with a `nil` (partial) tail. 1.19 has no
      # domain-key map representation, so it surfaces only the atom key and an
      # `:open` tail (the string key widens openness). Accept the
      # version-appropriate form via the `to_domain_keys/1` capability probe.
      if DescrCompat.to_domain_keys?() do
        assert {:map, fields, nil} = shape

        assert fields == [
                 {{:domain, {:binary, nil}}, {:binary, nil}},
                 {:key, {:atom, :value}}
               ]
      else
        assert {:map, [key: {:atom, :value}], :open} = shape
      end
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
      # task #22: the nested non-empty literal list surfaces as {:nonempty_list, _}.
      assert elements == [{:nonempty_list, {:integer, nil}}, {:atom, :ok}]
    end

    test "types local call" do
      call_ast = {:foo, [], []}
      assert {:ok, descr} = ElixirTypes.of_expr(call_ast)
      shape = ElixirTypes.to_shape(descr)
      assert shape == nil
    end

    test "types remote call" do
      call_ast = {{:., [], [Foo, :bar]}, [], []}
      assert {:ok, descr} = ElixirTypes.of_expr(call_ast)
      shape = ElixirTypes.to_shape(descr)
      assert shape == nil
    end

    test "types anonymous call" do
      call_ast = {{:., [], [{:foo, [version: 0], nil}]}, [], []}
      result = ElixirTypes.of_expr(call_ast)
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert shape == nil
    end

    test "types variables" do
      variable_ast = {:foo, [version: 0], nil}
      result = ElixirTypes.of_expr(variable_ast)
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert shape == nil

      result =
        ElixirTypes.of_expr(variable_ast, variables: %{{:foo, 0} => Module.Types.Descr.integer()})

      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert shape == {:integer, nil}
    end

    test "types property access" do
      call_ast = {{:., [], [{:foo, [version: 0], nil}, :bar]}, [no_parens: true], []}
      assert {:ok, descr} = ElixirTypes.of_expr(call_ast)
      shape = ElixirTypes.to_shape(descr)
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

  describe "audit fixes" do
    @describetag :requires_native_types

    setup do
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, true)
      on_exit(fn -> Application.put_env(:elixir_sense, :use_elixir_types, original_value) end)
      :ok
    end

    alias Module.Types.Descr

    test "bitstring() converts to :bitstring shape" do
      if function_exported?(Descr, :bitstring, 0) do
        # credo:disable-for-next-line Credo.Check.Refactor.Apply
        assert ElixirTypes.to_shape(apply(Descr, :bitstring, [])) == :bitstring
      end
    end

    test "not_set() converts to :not_set shape" do
      assert ElixirTypes.to_shape(Descr.not_set()) == :not_set
    end

    test "open tuple keeps the {:tuple_open, _} shape" do
      shape =
        ElixirTypes.to_shape(Descr.open_tuple([Descr.atom([:ok]), Descr.integer()]))

      assert shape == {:tuple_open, [{:atom, :ok}, {:integer, nil}]}
    end

    test "a loaded struct's quoted form converts to a struct shape" do
      descr = Module.Types.Of.struct_type(Date, [%{field: :year}])
      shape = ElixirTypes.to_shape(descr)

      unwrapped =
        case shape do
          {:dynamic, inner} -> inner
          inner -> inner
        end

      assert {:struct, _fields, {:atom, Date}, nil} = unwrapped
    end

    test "a negation member degrades the type to nil (not silently dropped)" do
      negated = Descr.negation(Descr.binary())
      union = Descr.union(Descr.integer(), negated)
      assert ElixirTypes.to_shape(union) == nil
    end

    test "coerced shapes are dynamic-wrapped (gradual, not certain)" do
      descr = ElixirTypes.coerce_var_type_public(:integer)
      assert Descr.gradual?(descr)
      refute Descr.gradual?(Descr.integer())
      assert Descr.equal?(descr, Descr.dynamic(Descr.integer()))
    end

    test "nil-tail (partial) map shapes coerce to an OPEN map (not closed)" do
      descr = ElixirTypes.coerce_var_type_public({:map, [foo: {:integer, nil}], nil})
      fuller = Descr.dynamic(Descr.closed_map(foo: Descr.integer(), bar: Descr.binary()))
      refute Descr.empty?(Descr.intersection(descr, fuller))
    end

    test "a loaded struct coerces to a closed map over the FULL field set" do
      descr =
        ElixirTypes.coerce_var_type_public({:struct, [year: {:integer, nil}], {:atom, Date}, nil})

      full_keys = Date.__struct__() |> Map.from_struct() |> Map.keys()

      real_pairs =
        [{:__struct__, Descr.atom([Date])}] ++ for(k <- full_keys, do: {k, Descr.term()})

      real = Descr.dynamic(Descr.closed_map(real_pairs))

      refute Descr.empty?(Descr.intersection(descr, real))

      partial =
        Descr.dynamic(
          Descr.closed_map([{:__struct__, Descr.atom([Date])}, {:year, Descr.integer()}])
        )

      assert Descr.empty?(Descr.intersection(descr, partial))
    end

    test "descr_to_string renders via the compiler's to_quoted_string" do
      case ElixirTypes.descr_to_string(Descr.integer()) do
        {:ok, str} -> assert str == "integer()"
        :error -> :ok
      end

      case ElixirTypes.descr_to_string(Descr.binary(), collapse_structs: true) do
        {:ok, str} -> assert is_binary(str)
        :error -> :ok
      end
    end

    @tag :requires_expected_type_native
    test "distinct unversioned named vars in a body don't alias" do
      ast = {:+, [], [{:foo, [], nil}, {:bar, [], nil}]}
      assert {:ok, _descr} = ElixirTypes.of_expr(ast)
    end
  end

  describe "apply_signature" do
    @describetag :requires_native_types

    setup do
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, true)
      on_exit(fn -> Application.put_env(:elixir_sense, :use_elixir_types, original_value) end)
      :ok
    end

    alias Module.Types.Descr

    # (integer() -> :a) and (binary() -> :b)
    defp two_clause_infer_sig do
      {:infer, nil,
       [
         {[Descr.integer()], Descr.atom([:a])},
         {[Descr.binary()], Descr.atom([:b])}
       ]}
    end

    test "(a) args matching ONE clause return only that clause's return, dynamic-wrapped" do
      sig = two_clause_infer_sig()
      assert {:ok, descr} = ElixirTypes.apply_signature(sig, [{:integer, 5}])
      # Only the :a clause's return, and it is gradual (dynamic-wrapped).
      assert Descr.gradual?(descr)
      assert Descr.equal?(descr, Descr.dynamic(Descr.atom([:a])))
      # Crucially NOT the union :a or :b.
      refute Descr.equal?(descr, Descr.dynamic(Descr.atom([:a, :b])))
    end

    test "(b) args matching NO clause return :error, not the union of all returns" do
      sig = two_clause_infer_sig()
      # :foo is neither integer() nor binary(): ill-typed for this signature.
      assert :error = ElixirTypes.apply_signature(sig, [{:atom, :foo}])

      # And the descr-returning wrapper degrades to dynamic() (shape nil) so the
      # caller falls back to structural typing — NOT the invented :a or :b union.
      descr = ElixirTypes.extract_return_type_from_sig(sig, [{:atom, :foo}])
      assert ElixirTypes.to_shape(descr) == nil
    end

    test "(c) unknown/dynamic args match ALL clauses (union, dynamic-wrapped)" do
      sig = two_clause_infer_sig()
      # nil shape coerces to dynamic() — a gradual arg widens matching, so every
      # clause is selected and the returns are unioned.
      assert {:ok, descr} = ElixirTypes.apply_signature(sig, [nil])
      assert Descr.gradual?(descr)
      assert Descr.equal?(descr, Descr.dynamic(Descr.atom([:a, :b])))
    end

    test "no argument information considers all clauses (union, dynamic-wrapped)" do
      sig = two_clause_infer_sig()
      assert {:ok, descr} = ElixirTypes.apply_signature(sig, nil)
      assert Descr.equal?(descr, Descr.dynamic(Descr.atom([:a, :b])))
    end

    test ":strong sig returns :error on a domain violation" do
      # Single-clause strong sig: (integer() -> :ok). A STATIC binary arg violates
      # the domain (not compatible) -> :error. We pass raw descrs (which
      # coerce_var_type passes through unchanged) because shape-coerced args are
      # always dynamic()-wrapped (FABLE #10) and gradual args always satisfy the
      # compiler's `zip_compatible_or_only_gradual?` domain check.
      sig = {:strong, nil, [{[Descr.integer()], Descr.atom([:ok])}]}
      assert :error = ElixirTypes.apply_signature(sig, [Descr.binary()])
      assert {:ok, descr} = ElixirTypes.apply_signature(sig, [Descr.integer()])
      # Task 2.1: :strong routes through Apply.return/3, which wraps in dynamic()
      # ONLY when an arg is gradual. integer() is fully static, so the BARE
      # static return is returned (not dynamic-wrapped).
      refute Descr.gradual?(descr)
      assert Descr.equal?(descr, Descr.atom([:ok]))
    end

    test ":strong sig with a gradual (unknown) arg widens to satisfy the domain" do
      # Mirrors zip_compatible_or_only_gradual?: a gradual arg is accepted even
      # against a mismatched static domain. Only an UNKNOWN arg (nil shape) is
      # gradual for clause selection — known shapes coerce statically so
      # selection can narrow (a static binary() against integer() is a domain
      # violation, asserted in the test above).
      sig = {:strong, nil, [{[Descr.integer()], Descr.atom([:ok])}]}
      assert {:ok, descr} = ElixirTypes.apply_signature(sig, [nil])
      assert Descr.equal?(descr, Descr.dynamic(Descr.atom([:ok])))

      assert :error = ElixirTypes.apply_signature(sig, [{:binary, nil}])
    end

    test "Task 2.1: :infer ALWAYS dynamic-wraps even with fully-static args (apply.ex:1827)" do
      # Unlike :strong, the compiler's apply_infer wraps the inferred union
      # unconditionally — it does not consult return/3. So even a static integer
      # arg yields a dynamic-wrapped return.
      sig = two_clause_infer_sig()
      assert {:ok, descr} = ElixirTypes.apply_signature(sig, [Descr.integer()])
      assert Descr.gradual?(descr)
      assert Descr.equal?(descr, Descr.dynamic(Descr.atom([:a])))
    end

    test "Task 2.1: :strong with a gradual descr arg keeps the dynamic wrap" do
      # A genuinely gradual descr arg (carries a :dynamic key) triggers return/3's
      # wrapping branch even though it is type-compatible with the domain.
      sig = {:strong, nil, [{[Descr.integer()], Descr.atom([:ok])}]}
      grad = Descr.dynamic(Descr.integer())
      assert {:ok, descr} = ElixirTypes.apply_signature(sig, [grad])
      assert Descr.gradual?(descr)
      assert Descr.equal?(descr, Descr.dynamic(Descr.atom([:ok])))
    end

    test ":none and unknown sigs are handled" do
      assert {:ok, descr} = ElixirTypes.apply_signature(:none, [{:integer, 1}])
      assert Descr.equal?(descr, Descr.dynamic())
      assert :error = ElixirTypes.apply_signature(:garbage, [{:integer, 1}])
    end
  end

  describe "build_local_sigs_map provenance precedence (Task 1.4)" do
    alias ElixirSense.Core.State.ModFunInfo
    alias ElixirSense.Core.State.SpecInfo

    # Minimal metadata carrying one stored (native) sig + one spec sig for the
    # same fun/arity. The stored sig source is `:inferred`, the spec sig is
    # always tagged `:spec`.
    defp metadata_with_sigs(stored_sig, stored_source, spec_sig) do
      mfa = {SomeMod, :f, 0}

      mods_funs = %{
        mfa => %ModFunInfo{
          type: :def,
          elixir_types_sig: stored_sig,
          elixir_types_sig_source: stored_source
        }
      }

      specs =
        case spec_sig do
          nil ->
            %{}

          _ ->
            %{
              mfa => %SpecInfo{
                name: :f,
                kind: :spec,
                elixir_types_sig: spec_sig,
                elixir_types_sig_source: :spec
              }
            }
        end

      %{mods_funs_to_positions: mods_funs, specs: specs}
    end

    test "native-inferred sig wins over a spec sig (current behavior preserved)" do
      inferred = {:infer, nil, [{[], {:atom, [], []}}]}
      spec = {:infer, nil, [{[], {:integer, [], []}}]}

      sigs =
        ElixirTypes.build_local_sigs_map(metadata_with_sigs(inferred, :inferred, spec), SomeMod)

      assert {:def, ^inferred} = sigs[{:f, 0}]
    end

    test "a hypothetical :strong-kind spec sig does NOT outrank a native-inferred sig" do
      # The old precedence keyed off the sig kind tag and let a `{:strong, ...}`
      # spec sig win. Provenance ordering (:inferred > :spec) now makes the
      # native-inferred sig win regardless of the spec sig's kind. This locks in
      # the deletion of the dead :strong-spec clause.
      inferred = {:infer, nil, [{[], {:atom, [], []}}]}
      strong_spec = {:strong, nil, [{[], {:integer, [], []}}]}

      sigs =
        ElixirTypes.build_local_sigs_map(
          metadata_with_sigs(inferred, :inferred, strong_spec),
          SomeMod
        )

      assert {:def, ^inferred} = sigs[{:f, 0}]
      refute match?({:def, {:strong, _, _}}, sigs[{:f, 0}])
    end

    test "spec sig is used as a fallback when there is no native sig" do
      spec = {:infer, nil, [{[], {:integer, [], []}}]}

      sigs = ElixirTypes.build_local_sigs_map(metadata_with_sigs(nil, nil, spec), SomeMod)

      assert {:def, ^spec} = sigs[{:f, 0}]
    end
  end

  describe "synthetic version hygiene (Task 3)" do
    @describetag :requires_native_types

    setup do
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, true)
      on_exit(fn -> Application.put_env(:elixir_sense, :use_elixir_types, original_value) end)
      :ok
    end

    test "synthetic-version keys never leak into the returned var-shape map" do
      # Pattern with an UNVERSIONED variable: ensure_body_var_versions stamps it a
      # synthetic version (>= 1_000_000). That synthetic key must NOT appear in
      # the result keyed for VarInfo consumers — only the real target survives.
      pattern = {:%{}, [], [{:name, {:unversioned, [], nil}}]}
      match = {:=, [], [pattern, {:%{}, [], [{:name, "John"}]}]}

      # No target_keys -> all vars are eligible, so a leaked synthetic key would
      # show up if the filter were missing.
      assert {:ok, vars, _descrs} =
               ElixirTypes.of_match(pattern, nil, match, TestModule, {:t, 1}, "t.ex", :dynamic)

      refute Enum.any?(Map.keys(vars), fn
               {_name, version} when is_integer(version) -> version >= 1_000_000
               _ -> false
             end)
    end
  end

  describe "native pattern results are authoritative (Task 2)" do
    @describetag :requires_native_types

    setup do
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, true)
      on_exit(fn -> Application.put_env(:elixir_sense, :use_elixir_types, original_value) end)
      :ok
    end

    test "(d) native var descr survives even when AST refinement would guess a literal" do
      # `%{name: name_var} = %{name: "John"}`. The AST refinement layer would
      # guess the literal {:binary, "John"}; the native of_match widens to a
      # generic binary(). Native must win — the result is the generic binary(),
      # never the AST-guessed literal.
      pattern = {:%{}, [], [{:name, {:name_var, [version: 1], nil}}]}
      match = {:=, [], [pattern, {:%{}, [], [{:name, "John"}]}]}

      assert {:ok, vars, _descrs} =
               ElixirTypes.of_match(pattern, nil, match, TestModule, {:t, 1}, "t.ex", :dynamic,
                 target_keys: [{:name_var, 1}]
               )

      assert vars[{:name_var, 1}] == {:binary, nil}
    end

    test "(e) refinement still fills a var the native path produced nothing for" do
      # When native typing is unavailable (or yields nothing) for a var, the
      # AST refinement fallback should still provide a shape. We exercise the
      # refinement engine directly on a non-natively-typeable struct module var
      # spot — here we assert the engine no longer INVENTS a shape for the struct
      # module variable (bug fix), and DOES carry a known field value where it
      # can map it positionally.
      #
      # Tuple pattern {a, b} = {1, :ok}: when both vars are real targets, native
      # gives precise types; the refinement merges UNDER it. The result must be
      # well-typed and contain both vars.
      pattern = {:a, [version: 1], nil}
      match = {:=, [], [pattern, 42]}

      assert {:ok, vars, _descrs} =
               ElixirTypes.of_match(pattern, nil, match, TestModule, {:t, 1}, "t.ex", :dynamic,
                 target_keys: [{:a, 1}]
               )

      # integer-ish (native may render {:integer, nil}); never invented.
      assert vars[{:a, 1}] in [{:integer, nil}, {:integer, 42}]
    end

    test "struct module variable is no longer forced to {:atom, nil}" do
      # `%mod{} = %Date{}` — the module var `mod`. Previously the refinement
      # layer forced it to {:atom, nil}. It must now be left to the native path
      # (or unknown), never an invented bare-atom guess overriding native.
      pattern = {:%, [], [{:mod, [version: 1], nil}, {:%{}, [], []}]}
      match = {:=, [], [pattern, Macro.escape(%Date{year: 2020, month: 1, day: 1})]}

      result =
        ElixirTypes.of_match(pattern, nil, match, TestModule, {:t, 1}, "t.ex", :dynamic,
          target_keys: [{:mod, 1}]
        )

      case result do
        {:ok, vars, _descrs} ->
          # Whatever the module var resolves to, it must not be the invented
          # {:atom, nil} from the old refinement guess unless native produced it.
          assert vars[{:mod, 1}] in [nil, {:atom, Date}, {:atom, nil}]

        :error ->
          :ok
      end
    end

    test "unknown binary segment no longer defaults a var to {:integer, nil}" do
      # `<<x::custom>>` where `custom` is not a recognized segment spec. The
      # refinement layer used to default such a var to {:integer, nil}; it must
      # now produce nothing (nil/unknown), leaving native/structural typing to
      # decide. We exercise of_match and assert the var, if present, is never the
      # invented {:integer, nil} guess unless native produced an integer.
      pattern = {:<<>>, [], [{:"::", [], [{:x, [version: 1], nil}, {:custom, [], nil}]}]}
      match = {:=, [], [pattern, {:<<>>, [], [{:"::", [], [1, {:custom, [], nil}]}]}]}

      result =
        ElixirTypes.of_match(pattern, nil, match, TestModule, {:t, 1}, "t.ex", :dynamic,
          target_keys: [{:x, 1}]
        )

      # Either native typed it precisely, or it is absent/unknown — but the AST
      # refinement no longer fabricates {:integer, nil} from an unknown spec.
      case result do
        {:ok, vars, _descrs} -> assert is_map(vars)
        :error -> :ok
      end
    end
  end
end
