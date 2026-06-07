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

  describe "capabilities" do
    @capability_keys [
      :expr,
      :pattern_match,
      :head,
      :local_signature,
      :previous,
      :reverse_arrow,
      :domain_map_ops
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
          # cross-clause previous + reverse arrows, and domain-aware map ops.
          assert caps.expr and caps.pattern_match and caps.head
          assert caps.local_signature
          assert caps.previous and caps.reverse_arrow and caps.domain_map_ops

        Version.match?(System.version(), ">= 1.19.0") ->
          # 1.19 has expr/5, of_match/5, of_head/7 and stack/7, but NOT the
          # 1.20-only cross-clause/reverse-arrow/domain-map machinery. If these
          # flip, the V19 dispatch assumptions are wrong — fail loudly here.
          assert caps.expr and caps.expr_basic and caps.pattern_match and caps.head
          assert caps.local_signature
          refute caps.previous
          refute caps.reverse_arrow
          refute caps.domain_map_ops

        Version.match?(System.version(), ">= 1.18.0") ->
          # 1.18 has the basic of_expr/3 (NOT the expected-type of_expr/5),
          # of_match/7, of_head/7 and stack/7. The adapter is active here, but
          # without expected-type expr or any 1.20 machinery.
          refute caps.expr
          assert caps.expr_basic and caps.pattern_match and caps.head
          assert caps.local_signature
          refute caps.previous
          refute caps.reverse_arrow
          refute caps.domain_map_ops

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
          refute caps.reverse_arrow
          refute caps.domain_map_ops
      end
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

        assert vars[{:name_var, 1}] == {:binary, "John"}
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

    test "preserves domain (non-atom) keys" do
      # %{integer() => binary()} keeps the domain key as {:domain, key_shape}
      # rather than dropping it. (open_map also admits other domains, hence
      # membership rather than equality.)
      descr =
        Module.Types.Descr.open_map([
          {Module.Types.Descr.to_domain_keys(Module.Types.Descr.integer()),
           Module.Types.Descr.binary()}
        ])

      assert {:map, fields, nil} = ElixirTypes.to_shape(descr)
      assert {{:domain, {:integer, nil}}, {:binary, nil}} in fields
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
      # dynamic(integer()) - to_quoted unwraps to {:integer, [], []}
      dynamic_int = Module.Types.Descr.dynamic(Module.Types.Descr.integer())
      assert ElixirTypes.to_shape(dynamic_int) == {:integer, nil}

      # dynamic(atom(:ok)) - preserves literal atom info
      dynamic_atom = Module.Types.Descr.dynamic(Module.Types.Descr.atom([:ok]))
      assert ElixirTypes.to_shape(dynamic_atom) == {:atom, :ok}
    end

    test "handles struct as dynamic map" do
      # Module.Types.Of.struct_type produces a dynamic-wrapped map with
      # __struct__. On 1.18 to_shape renders this looser (the descr/to_quoted
      # form differs), so this precise assertion is 1.19+.
      if ElixirTypes.available?(:expr) do
        struct = Module.Types.Of.struct_type(Date, [%{field: :year}, %{field: :month}])
        shape = ElixirTypes.to_shape(struct)
        assert match?({:struct, _fields, {:atom, Date}, nil}, shape)
      end
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
      # the string key is generalized to a binary() domain key, now preserved
      # alongside the atom key (previously the domain key was dropped)
      assert fields == [{{:domain, {:binary, nil}}, {:binary, nil}}, {:key, {:atom, :value}}]
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
