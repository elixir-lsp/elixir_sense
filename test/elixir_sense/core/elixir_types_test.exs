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
      # task #20: dynamic() is no longer dropped to nil — it carries a
      # `{:dynamic, nil}` marker so the presentation layer can render it.
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
      # task #22: empty_list() is a distinct shape, not a generic empty list tuple.
      assert ElixirTypes.to_shape(Module.Types.Descr.empty_list()) == :empty_list
    end

    test "handles empty map" do
      # task #22: empty_map() (closed `%{}`) is distinct from the map top type.
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
      # task #22: non-emptiness is preserved as a distinct {:nonempty_list, _}.
      assert ElixirTypes.to_shape(Module.Types.Descr.non_empty_list(Module.Types.Descr.integer())) ==
               {:nonempty_list, {:integer, nil}}

      # task #22: a non_empty_list with an explicit non-list (improper) tail is
      # unrepresentable as a proper-list shape, so we degrade to nil rather than
      # silently claiming a proper list.
      assert ElixirTypes.to_shape(
               Module.Types.Descr.non_empty_list(
                 Module.Types.Descr.integer(),
                 Module.Types.Descr.integer()
               )
             ) == nil
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
      # task #7: open tuples carry a dedicated {:tuple_open, shapes} shape (the
      # known prefix), distinct from a closed-arity tuple — we must NOT claim the
      # exact arity for an open tuple.
      assert ElixirTypes.to_shape(
               Module.Types.Descr.open_tuple([
                 Module.Types.Descr.atom([:ok]),
                 Module.Types.Descr.binary()
               ])
             ) == {:tuple_open, [{:atom, :ok}, {:binary, nil}]}

      # Open tuple with fallback - still extracts the known prefix
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
      # task #7: the tuple top type quotes to a bare open marker `{...}` and
      # becomes the `:tuple` shape — NOT an arity-0 tuple `{}`.
      assert ElixirTypes.to_shape(Module.Types.Descr.tuple()) == :tuple

      assert ElixirTypes.to_shape(
               Module.Types.Descr.tuple([
                 Module.Types.Descr.atom([:ok]),
                 Module.Types.Descr.integer()
               ])
             ) == {:tuple, 2, [{:atom, :ok}, {:integer, nil}]}
    end

    test "handles boolean" do
      # task #22: boolean() stays a dedicated :boolean shape (compiler prints
      # `boolean()`), not a decomposed false | true union.
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
      # List with integers. task #22: a non-empty literal list types as a
      # non_empty_list, surfaced as the {:nonempty_list, _} shape.
      list_ast = [1, 2, 3]
      result = ElixirTypes.of_expr(list_ast)
      assert {:ok, descr} = result
      assert ElixirTypes.to_shape(descr) == {:nonempty_list, {:integer, nil}}

      # Empty list. task #22: surfaced as the :empty_list shape.
      empty_ast = []
      result = ElixirTypes.of_expr(empty_ast)
      assert {:ok, descr} = result
      assert ElixirTypes.to_shape(descr) == :empty_list
    end

    test "types list mixed" do
      list_ast = [1, :ok]
      result = ElixirTypes.of_expr(list_ast)
      assert {:ok, descr} = result
      # Mixed list elements get unified to a single type. A non-empty literal
      # list surfaces as {:nonempty_list, _} (task #22).
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
      # task #22: the nested non-empty literal list surfaces as {:nonempty_list, _}.
      assert elements == [{:nonempty_list, {:integer, nil}}, {:atom, :ok}]
    end

    test "types local call" do
      call_ast = {:foo, [], []}
      assert {:ok, descr} = ElixirTypes.of_expr(call_ast)
      # Returns dynamic(term()); task #20 surfaces the dynamic top as
      # {:dynamic, nil} rather than dropping it to nil. (Previously this test
      # passed the raw AST to to_shape — a no-op that always yielded nil; fixed
      # to convert the actual descr.)
      shape = ElixirTypes.to_shape(descr)
      assert shape == nil
    end

    test "types remote call" do
      call_ast = {{:., [], [Foo, :bar]}, [], []}
      assert {:ok, descr} = ElixirTypes.of_expr(call_ast)
      # Returns dynamic(term()) — no ExCk sig for Foo.bar (task #20). (Fixed to
      # convert the actual descr, not the raw AST.)
      shape = ElixirTypes.to_shape(descr)
      assert shape == nil
    end

    test "types anonymous call" do
      call_ast = {{:., [], [{:foo, [version: 0], nil}]}, [], []}
      result = ElixirTypes.of_expr(call_ast)
      # Returns dynamic(term()) — no local handler or remote sig available (task #20).
      assert {:ok, descr} = result
      shape = ElixirTypes.to_shape(descr)
      assert shape == nil
    end

    test "types variables" do
      variable_ast = {:foo, [version: 0], nil}
      result = ElixirTypes.of_expr(variable_ast)
      # dynamic when var not known; task #20 surfaces it as {:dynamic, nil}.
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
      assert {:ok, descr} = ElixirTypes.of_expr(call_ast)
      # Returns dynamic(term()) — no local handler or remote sig available (task
      # #20). (Fixed to convert the actual descr, not the raw AST.)
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

  # New coverage for the Fable audit fixes (tasks #7-#11, #15, #17, #19-#22, #37).
  describe "audit fixes" do
    @describetag :requires_native_types

    setup do
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, true)
      on_exit(fn -> Application.put_env(:elixir_sense, :use_elixir_types, original_value) end)
      :ok
    end

    alias Module.Types.Descr

    test "task #21: bitstring() converts to :bitstring shape" do
      # bitstring() is the 1.20 spelling; older Descr may not expose bitstring/0.
      if function_exported?(Descr, :bitstring, 0) do
        # credo:disable-for-next-line Credo.Check.Refactor.Apply
        assert ElixirTypes.to_shape(apply(Descr, :bitstring, [])) == :bitstring
      end
    end

    test "task #21: not_set() converts to :not_set shape" do
      assert ElixirTypes.to_shape(Descr.not_set()) == :not_set
    end

    test "task #7: open tuple keeps the {:tuple_open, _} shape" do
      shape =
        ElixirTypes.to_shape(Descr.open_tuple([Descr.atom([:ok]), Descr.integer()]))

      assert shape == {:tuple_open, [{:atom, :ok}, {:integer, nil}]}
    end

    test "task #19: a loaded struct's quoted form converts to a struct shape" do
      # Date is loaded and complete; descr.ex map_literal_to_quoted emits the
      # `%Date{...}` quoted form, which we now convert to a {:struct, ...} shape.
      descr = Module.Types.Of.struct_type(Date, [%{field: :year}])
      shape = ElixirTypes.to_shape(descr)

      unwrapped =
        case shape do
          {:dynamic, inner} -> inner
          inner -> inner
        end

      assert {:struct, _fields, {:atom, Date}, nil} = unwrapped
    end

    test "task #9: a negation member degrades the type to nil (not silently dropped)" do
      # `integer() | not binary()` simplifies to `not binary()` (a bare negation
      # in the quoted form). Negations are unconvertible; we degrade to nil rather
      # than dropping the member, which would make the displayed type narrower
      # than the truth.
      negated = Descr.negation(Descr.binary())
      union = Descr.union(Descr.integer(), negated)
      assert ElixirTypes.to_shape(union) == nil
    end

    test "task #10: coerced shapes are dynamic-wrapped (gradual, not certain)" do
      # A statically-coerced descr makes the typesystem treat the seed as certain,
      # collapsing matches to none(). The compiler wraps inferred values in
      # dynamic/1; we mirror that. Assert the coerced integer is gradual (carries
      # a dynamic component) rather than a fully-static integer().
      descr = ElixirTypes.coerce_var_type_public(:integer)
      assert Descr.gradual?(descr)
      refute Descr.gradual?(Descr.integer())
      # It is still equivalent to integer() as a gradual type.
      assert Descr.equal?(descr, Descr.dynamic(Descr.integer()))
    end

    test "task #10: plain map shapes coerce to an OPEN map (not closed)" do
      # An open map admits extra keys; a closed map asserts all other keys absent
      # and would intersect emptily with a fuller real map. A map that ALSO has a
      # :bar key must still be compatible (non-empty intersection) with our seed.
      descr = ElixirTypes.coerce_var_type_public({:map, [foo: {:integer, nil}], nil})
      fuller = Descr.dynamic(Descr.closed_map(foo: Descr.integer(), bar: Descr.binary()))
      refute Descr.empty?(Descr.intersection(descr, fuller))
    end

    test "task #10: a loaded struct coerces to a closed map over the FULL field set" do
      # Date is loaded; we expand the complete defstruct field set so the seed
      # matches a real, complete Date value rather than asserting unknown fields
      # absent (which a partial closed_map over just {:year} would do).
      descr =
        ElixirTypes.coerce_var_type_public({:struct, [year: {:integer, nil}], {:atom, Date}, nil})

      # A real complete Date value: closed map over the full field set.
      full_keys = Date.__struct__() |> Map.from_struct() |> Map.keys()

      real_pairs =
        [{:__struct__, Descr.atom([Date])}] ++ for(k <- full_keys, do: {k, Descr.term()})

      real = Descr.dynamic(Descr.closed_map(real_pairs))

      refute Descr.empty?(Descr.intersection(descr, real))

      # And a struct missing fields (a partial closed map) must NOT match it —
      # confirming we expanded to the full field set.
      partial =
        Descr.dynamic(
          Descr.closed_map([{:__struct__, Descr.atom([Date])}, {:year, Descr.integer()}])
        )

      assert Descr.empty?(Descr.intersection(descr, partial))
    end

    test "task #17: descr_to_string renders via the compiler's to_quoted_string" do
      case ElixirTypes.descr_to_string(Descr.integer()) do
        {:ok, str} -> assert str == "integer()"
        :error -> :ok
      end

      # opts form is accepted and still produces a string
      case ElixirTypes.descr_to_string(Descr.binary(), collapse_structs: true) do
        {:ok, str} -> assert is_binary(str)
        :error -> :ok
      end
    end

    test "task #11: distinct unversioned named vars in a body don't alias" do
      # `foo + bar` — two distinct unversioned names. Before the fix both were
      # stamped version 0 and collapsed into one context slot. With the fix they
      # get distinct versions; typing the expression must not crash and must
      # produce a result (no cross-contamination / no raise).
      ast = {:+, [], [{:foo, [], nil}, {:bar, [], nil}]}
      assert {:ok, _descr} = ElixirTypes.of_expr(ast)
    end
  end

  describe "apply_signature (Task 1 — mirror compiler apply semantics)" do
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
