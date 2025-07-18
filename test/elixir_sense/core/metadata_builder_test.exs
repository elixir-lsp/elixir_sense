defmodule ElixirSense.Core.MetadataBuilderTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Core.MetadataBuilder
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State

  alias ElixirSense.Core.State.{
    VarInfo,
    CallInfo,
    StructInfo,
    ModFunInfo,
    AttributeInfo,
    RecordInfo
  }

  describe "versioned_vars" do
    test "in block" do
      state =
        """
        abc = 5
        record_env()
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[2].versioned_vars, {:abc, nil})

      assert [
               %VarInfo{name: :abc, positions: [{1, 1}]}
             ] = state |> get_line_vars(2)
    end

    test "call does not create a scope" do
      state =
        """
        inspect(abc = 5)
        record_env()
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[2].versioned_vars, {:abc, nil})

      assert [
               %VarInfo{name: :abc, positions: [{1, 9}]}
             ] = state |> get_line_vars(2)
    end

    test "nested binding" do
      state =
        """
        abc = cde = 5
        record_env()
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[2].versioned_vars, {:abc, nil})
      assert Map.has_key?(state.lines_to_env[2].versioned_vars, {:cde, nil})

      assert [
               %VarInfo{name: :abc, positions: [{1, 1}]},
               %VarInfo{name: :cde, positions: [{1, 7}]}
             ] = state |> get_line_vars(2)
    end

    test "nested binding repeated" do
      state =
        """
        abc = cde = abc = 5
        record_env()
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[2].versioned_vars, {:abc, nil})
      assert Map.has_key?(state.lines_to_env[2].versioned_vars, {:cde, nil})

      assert [
               %VarInfo{name: :abc, positions: [{1, 1}]},
               #  %VarInfo{name: :abc, positions: [{1, 13}]},
               %VarInfo{name: :cde, positions: [{1, 7}]}
             ] = state |> get_line_vars(2)
    end

    test "in nested blocks" do
      state =
        """
        (); ((abc = 1); ()); ( ); (inspect(abc))
        record_env()
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[2].versioned_vars, {:abc, nil})

      assert [
               %VarInfo{name: :abc, positions: [{1, 7}, {1, 36}]}
             ] = state |> get_line_vars(2)
    end

    test "repeated in match" do
      state =
        """
        [abc, abc] = foo()
        record_env()
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[2].versioned_vars, {:abc, nil})

      assert [
               %VarInfo{name: :abc, positions: [{1, 2}, {1, 7}]}
             ] = state |> get_line_vars(2)
    end

    test "underscored" do
      state =
        """
        _ = foo()
        _abc = bar()
        record_env()
        """
        |> string_to_state

      refute Map.has_key?(state.lines_to_env[3].versioned_vars, {:_, nil})
      assert Map.has_key?(state.lines_to_env[3].versioned_vars, {:_abc, nil})

      assert [
               %VarInfo{name: :_abc, positions: [{2, 1}]}
             ] = state |> get_line_vars(3)
    end

    test "pin" do
      state =
        """
        abc = 5
        ^abc = foo()
        record_env()
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[3].versioned_vars, {:abc, nil})

      assert [
               %VarInfo{name: :abc, positions: [{1, 1}, {2, 2}]}
             ] = state |> get_line_vars(3)
    end

    test "pin undefined" do
      state =
        """
        ^abc = foo()
        record_env()
        """
        |> string_to_state

      refute Map.has_key?(state.lines_to_env[2].versioned_vars, {:abc, nil})

      assert [] = state |> get_line_vars(3)
    end

    test "rebinding" do
      state =
        """
        abc = 5
        abc = foo()
        record_env()
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[3].versioned_vars, {:abc, nil})

      assert [
               %VarInfo{name: :abc, version: 1, positions: [{2, 1}]}
             ] = state |> get_line_vars(3)

      assert [
               %VarInfo{name: :abc, version: 0, positions: [{1, 1}]}
             ] = state |> get_line_vars(2)

      assert state.vars_info_per_scope_id[0] == %{
               {:abc, 0} => %VarInfo{
                 name: :abc,
                 positions: [{1, 1}],
                 scope_id: 0,
                 version: 0,
                 type: {:integer, 5}
               },
               {:abc, 1} => %VarInfo{
                 name: :abc,
                 positions: [{2, 1}],
                 scope_id: 0,
                 version: 1,
                 type: {:local_call, :foo, {2, 7}, []}
               }
             }
    end

    test "rebinding in defs" do
      state =
        """
        defmodule MyModule do
          def go(asd = 3, asd, x) do
            :ok
          end

          def go(asd = 3, [2, asd], y) do
            :ok
          end
        end
        """
        |> string_to_state

      assert %{
               {:x, 1} => %VarInfo{positions: [{2, 24}]},
               {:asd, 0} => %VarInfo{positions: [{2, 10}, {2, 19}]}
             } = state.vars_info_per_scope_id[2]

      assert %{
               {:y, 1} => %VarInfo{positions: [{6, 29}]},
               {:asd, 0} => %VarInfo{positions: [{6, 10}, {6, 23}]}
             } = state.vars_info_per_scope_id[3]
    end

    test "binding in function call" do
      state =
        """
        foo(abc = 5)
        Remote.bar(cde = 6)
        x.(xyz = 7)
        record_env()
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[4].versioned_vars) == [
               {:abc, nil},
               {:cde, nil},
               {:xyz, nil}
             ]

      assert [
               %VarInfo{name: :abc, positions: [{1, 5}]},
               %VarInfo{name: :cde, positions: [{2, 12}]},
               %VarInfo{name: :xyz, positions: [{3, 4}]}
             ] = state |> get_line_vars(4)
    end

    test "usages" do
      state =
        """
        abc = 5
        {abc}
        {abc, abc}
        [abc | [abc, abc]]
        %{x: abc}
        %{abc => abc}
        <<abc>>
        local(abc)
        Some.remote(abc)
        x.(abc)
        -abc
        record_env()
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[12].versioned_vars, {:abc, nil})

      assert [
               %VarInfo{name: :abc, positions: positions}
             ] = state |> get_line_vars(12)

      assert positions == [
               {1, 1},
               {2, 2},
               {3, 2},
               {3, 7},
               {4, 2},
               {4, 9},
               {4, 14},
               {5, 6},
               {6, 3},
               {6, 10},
               {7, 3},
               {8, 7},
               {9, 13},
               {10, 4},
               {11, 2}
             ]
    end

    test "in bitstring modifier" do
      state =
        """
        y = 1
        <<1::size(y)>>
        <<1::size(y)>> = <<>>
        record_env()
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[4].versioned_vars) == [
               {:y, nil}
             ]

      assert [
               %VarInfo{name: :y, positions: [{1, 1}, {2, 11}, {3, 11}]}
             ] = state |> get_line_vars(4)
    end

    test "undefined usage" do
      state =
        """
        foo(abc)
        record_env()
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[2].versioned_vars) == []

      assert [] = state |> get_line_vars(2)
    end

    test "undefined pin" do
      state =
        """
        ^abc = 1
        record_env()
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[2].versioned_vars) == []

      assert [] = state |> get_line_vars(2)
    end

    test "in if" do
      state =
        """
        cde = "1"
        if true do
          abc = 5
          record_env()
        else
          xyz = 123
          record_env()
        end
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[4].versioned_vars, {:abc, nil})
      assert Map.has_key?(state.lines_to_env[4].versioned_vars, {:cde, nil})

      assert [
               %VarInfo{name: :abc, positions: [{3, 3}]},
               %VarInfo{name: :cde, positions: [{1, 1}]}
             ] = state |> get_line_vars(4)

      assert Map.has_key?(state.lines_to_env[7].versioned_vars, {:xyz, nil})
      assert Map.has_key?(state.lines_to_env[7].versioned_vars, {:cde, nil})
      refute Map.has_key?(state.lines_to_env[7].versioned_vars, {:abc, nil})

      assert [
               %VarInfo{name: :cde, positions: [{1, 1}]},
               %VarInfo{name: :xyz, positions: [{6, 3}]}
             ] = state |> get_line_vars(7)
    end

    test "does not leak outside if" do
      state =
        """
        cde = "1"
        if true do
          abc = 5
        end
        record_env()
        """
        |> string_to_state

      refute Map.has_key?(state.lines_to_env[5].versioned_vars, {:abc, nil})
      assert Map.has_key?(state.lines_to_env[5].versioned_vars, {:cde, nil})

      assert [
               %VarInfo{name: :cde, positions: [{1, 1}]}
             ] = state |> get_line_vars(5)
    end

    test "usage in if" do
      state =
        """
        cde = "1"
        if true do
          _ = cde
          record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[4].versioned_vars, {:cde, nil})

      assert [
               %VarInfo{name: :cde, positions: [{1, 1}, {3, 7}]}
             ] = state |> get_line_vars(4)

      assert Map.has_key?(state.lines_to_env[6].versioned_vars, {:cde, nil})

      assert [
               %VarInfo{name: :cde, positions: [{1, 1}, {3, 7}]}
             ] = state |> get_line_vars(6)
    end

    test "rebinding in if" do
      state =
        """
        cde = "1"
        if true do
          cde = 5
          record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[4].versioned_vars, {:cde, nil})

      assert [
               # %VarInfo{name: :cde, positions: [{1, 1}], scope_id: scope_id_1},
               %VarInfo{name: :cde, positions: [{3, 3}]}
             ] = state |> get_line_vars(4)

      assert Map.has_key?(state.lines_to_env[6].versioned_vars, {:cde, nil})

      assert [
               %VarInfo{name: :cde, positions: [{1, 1}]}
             ] = state |> get_line_vars(6)
    end

    test "defined on if" do
      state =
        """
        if cde = "1" do
          abc = 5
          record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[3].versioned_vars, {:abc, nil})
      assert Map.has_key?(state.lines_to_env[3].versioned_vars, {:cde, nil})

      assert [
               %VarInfo{name: :abc, positions: [{2, 3}]},
               %VarInfo{name: :cde, positions: [{1, 4}]}
             ] = state |> get_line_vars(3)

      refute Map.has_key?(state.lines_to_env[5].versioned_vars, {:abc, nil})
      assert Map.has_key?(state.lines_to_env[5].versioned_vars, {:cde, nil})

      assert [
               %VarInfo{name: :cde, positions: [{1, 4}]}
             ] = state |> get_line_vars(5)
    end

    test "case pattern" do
      state =
        """
        case foo() do
          abc ->
            record_env()
          _ ->
            record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[3].versioned_vars, {:abc, nil})

      assert [
               %VarInfo{name: :abc, positions: [{2, 3}]}
             ] = state |> get_line_vars(3)

      refute Map.has_key?(state.lines_to_env[5].versioned_vars, {:abc, nil})

      assert [] = state |> get_line_vars(5)

      refute Map.has_key?(state.lines_to_env[7].versioned_vars, {:abc, nil})

      assert [] = state |> get_line_vars(5)
    end

    test "cond pattern" do
      state =
        """
        cond do
          abc = foo() ->
            record_env()
          true ->
            record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[3].versioned_vars, {:abc, nil})

      assert [
               %VarInfo{name: :abc, positions: [{2, 3}]}
             ] = state |> get_line_vars(3)

      refute Map.has_key?(state.lines_to_env[5].versioned_vars, {:abc, nil})

      assert [] = state |> get_line_vars(5)

      refute Map.has_key?(state.lines_to_env[7].versioned_vars, {:abc, nil})
      assert [] = state |> get_line_vars(7)
    end

    test "receive pattern" do
      state =
        """
        receive do
          abc ->
            record_env()
        after
          123 ->
            x = foo()
            record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[3].versioned_vars, {:abc, nil})

      assert [
               %VarInfo{name: :abc, positions: [{2, 3}]}
             ] = state |> get_line_vars(3)

      assert Map.has_key?(state.lines_to_env[7].versioned_vars, {:x, nil})
      refute Map.has_key?(state.lines_to_env[7].versioned_vars, {:abc, nil})

      assert [
               %VarInfo{name: :x, positions: [{6, 5}]}
             ] = state |> get_line_vars(7)

      refute Map.has_key?(state.lines_to_env[9].versioned_vars, {:x, nil})
      refute Map.has_key?(state.lines_to_env[9].versioned_vars, {:abc, nil})

      assert [] = state |> get_line_vars(9)
    end

    test "with" do
      state =
        """
        with abc <- foo(),
          cde = bar(),
          xyz() do
          z = abc + cde
          record_env()
        else
          other ->
            c = 123
            record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[1].versioned_vars) == []
      assert [] = state |> get_line_vars(1)

      assert Map.keys(state.lines_to_env[2].versioned_vars) == [{:abc, nil}]

      assert [
               %VarInfo{name: :abc, positions: [{1, 6}]}
             ] = state |> get_line_vars(2)

      assert Map.keys(state.lines_to_env[3].versioned_vars) == [{:abc, nil}, {:cde, nil}]

      assert [
               %VarInfo{name: :abc, positions: [{1, 6}]},
               %VarInfo{name: :cde, positions: [{2, 3}]}
             ] = state |> get_line_vars(3)

      assert Map.keys(state.lines_to_env[5].versioned_vars) == [
               {:abc, nil},
               {:cde, nil},
               {:z, nil}
             ]

      assert [
               %VarInfo{name: :abc, positions: [{1, 6}, {4, 7}]},
               %VarInfo{name: :cde, positions: [{2, 3}, {4, 13}]},
               %VarInfo{name: :z, positions: [{4, 3}]}
             ] = state |> get_line_vars(5)

      assert Map.keys(state.lines_to_env[9].versioned_vars) == [{:c, nil}, {:other, nil}]

      assert [
               %VarInfo{name: :c, positions: [{8, 5}]},
               %VarInfo{name: :other, positions: [{7, 3}]}
             ] = state |> get_line_vars(9)

      assert Map.keys(state.lines_to_env[11].versioned_vars) == []

      assert [] = state |> get_line_vars(11)
    end

    test "for" do
      state =
        """
        for abc <- foo(),
          cde <- bar() do
          z = 6
          record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[1].versioned_vars) == []
      assert [] = state |> get_line_vars(1)

      assert Map.keys(state.lines_to_env[2].versioned_vars) == [{:abc, nil}]

      assert [
               %VarInfo{name: :abc, positions: [{1, 5}]}
             ] = state |> get_line_vars(2)

      assert Map.keys(state.lines_to_env[4].versioned_vars) == [
               {:abc, nil},
               {:cde, nil},
               {:z, nil}
             ]

      assert [
               %VarInfo{name: :abc, positions: [{1, 5}]},
               %VarInfo{name: :cde, positions: [{2, 3}]},
               %VarInfo{name: :z, positions: [{3, 3}]}
             ] = state |> get_line_vars(4)

      assert Map.keys(state.lines_to_env[6].versioned_vars) == []
      assert [] = state |> get_line_vars(6)
    end

    test "for bitstring" do
      state =
        """
        for <<r::8, g::8, b::8 <- pixels>> do
          record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[2].versioned_vars) == [{:b, nil}, {:g, nil}, {:r, nil}]

      assert [
               %VarInfo{name: :b, positions: [{1, 19}]},
               %VarInfo{name: :g, positions: [{1, 13}]},
               %VarInfo{name: :r, positions: [{1, 7}]}
             ] = state |> get_line_vars(2)
    end

    test "for assignment" do
      state =
        """
        for {language, parent} <- languages, grandparent = languages[parent] do
          record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[2].versioned_vars) == [
               {:grandparent, nil},
               {:language, nil},
               {:parent, nil}
             ]

      assert [
               %VarInfo{name: :grandparent, positions: [{1, 38}]},
               %VarInfo{name: :language, positions: [{1, 6}]},
               %VarInfo{name: :parent, positions: [{1, 16}, {1, 62}]}
             ] = state |> get_line_vars(2)
    end

    test "for opts" do
      state =
        """
        for line <- IO.stream(), into: IO.stream() do
          record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[2].versioned_vars) == [{:line, nil}]

      assert [
               %VarInfo{name: :line, positions: [{1, 5}]}
             ] = state |> get_line_vars(2)
    end

    test "for reduce" do
      state =
        """
        for <<x <- "AbCabCABc">>, x in ?a..?z, reduce: %{} do
          acc -> record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[2].versioned_vars) == [{:acc, nil}, {:x, nil}]

      assert [
               %VarInfo{name: :acc, positions: [{2, 3}]},
               %VarInfo{name: :x, positions: [{1, 7}, {1, 27}]}
             ] = state |> get_line_vars(2)
    end

    test "fn" do
      state =
        """
        a = fn x ->
          record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[2].versioned_vars) == [{:x, nil}]

      assert [
               %VarInfo{name: :x, positions: [{1, 8}]}
             ] = state |> get_line_vars(2)

      assert Map.keys(state.lines_to_env[4].versioned_vars) == [{:a, nil}]

      assert [
               %VarInfo{name: :a, positions: [{1, 1}]}
             ] = state |> get_line_vars(4)
    end

    test "fn argument usage" do
      state =
        """
        fn x ->
          foo(x)
          record_env()
        end
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[3].versioned_vars) == [{:x, nil}]

      assert [
               %VarInfo{name: :x, positions: [{1, 4}, {2, 7}]}
             ] = state |> get_line_vars(3)
    end

    test "fn multiple clauses" do
      state =
        """
        a = fn
          x, 1 ->
            record_env()
          y, z when is_integer(z) ->
            record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[3].versioned_vars) == [{:x, nil}]

      assert [
               %VarInfo{name: :x, positions: [{2, 3}]}
             ] = state |> get_line_vars(3)

      assert Map.keys(state.lines_to_env[5].versioned_vars) == [{:y, nil}, {:z, nil}]

      assert [
               %VarInfo{name: :y, positions: [{4, 3}]},
               %VarInfo{name: :z, positions: [{4, 6}, {4, 24}]}
             ] = state |> get_line_vars(5)

      assert Map.keys(state.lines_to_env[7].versioned_vars) == [{:a, nil}]

      assert [
               %VarInfo{name: :a, positions: [{1, 1}]}
             ] = state |> get_line_vars(7)
    end

    test "fn closure" do
      state =
        """
        abc = 5
        fn ->
          record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[3].versioned_vars) == [{:abc, nil}]

      assert [
               %VarInfo{name: :abc, positions: [{1, 1}]}
             ] = state |> get_line_vars(3)

      assert Map.keys(state.lines_to_env[5].versioned_vars) == [{:abc, nil}]

      assert [
               %VarInfo{name: :abc, positions: [{1, 1}]}
             ] = state |> get_line_vars(5)
    end

    test "fn usage in closure" do
      state =
        """
        abc = 5
        fn ->
          foo(abc)
          record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[4].versioned_vars) == [{:abc, nil}]

      assert [
               %VarInfo{name: :abc, positions: [{1, 1}, {3, 7}]}
             ] = state |> get_line_vars(4)

      assert Map.keys(state.lines_to_env[6].versioned_vars) == [{:abc, nil}]

      assert [
               %VarInfo{name: :abc, positions: [{1, 1}, {3, 7}]}
             ] = state |> get_line_vars(6)
    end

    test "fn closure rebinding" do
      state =
        """
        abc = 5
        fn ->
          abc = 6
          record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[4].versioned_vars) == [{:abc, nil}]

      assert [
               # %VarInfo{name: :abc, positions: [{1, 1}], scope_id: scope_id_1},
               %VarInfo{name: :abc, positions: [{3, 3}]}
             ] = state |> get_line_vars(4)

      assert Map.keys(state.lines_to_env[6].versioned_vars) == [{:abc, nil}]

      assert [
               %VarInfo{name: :abc, positions: [{1, 1}]}
             ] = state |> get_line_vars(6)
    end

    test "try" do
      state =
        """
        try do
          a = 2
          do_something_that_may_fail()
        rescue
          e in ArgumentError ->
            IO.puts("Invalid argument given")
        catch
          value ->
            IO.puts("Caught \#{inspect(value)}")
        else
          other ->
            IO.puts("Success! The result was \#{inspect(other)}")
        after
          b = 2
          IO.puts("This is printed regardless if it failed or succeeded")
        end
        record_env()
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[3].versioned_vars) == [{:a, nil}]

      assert [
               %VarInfo{name: :a, positions: [{2, 3}]}
             ] = state |> get_line_vars(3)

      assert Map.keys(state.lines_to_env[6].versioned_vars) == [{:e, nil}]

      assert [
               %VarInfo{name: :e, positions: [{5, 3}]}
             ] = state |> get_line_vars(6)

      assert Map.keys(state.lines_to_env[9].versioned_vars) == [{:value, nil}]

      assert [
               %VarInfo{name: :value, positions: [{8, 3}, {9, 31}]}
             ] = state |> get_line_vars(9)

      assert Map.keys(state.lines_to_env[12].versioned_vars) == [{:other, nil}]

      assert [
               %VarInfo{name: :other, positions: [{11, 3}, {12, 48}]}
             ] = state |> get_line_vars(12)

      assert Map.keys(state.lines_to_env[15].versioned_vars) == [{:b, nil}]

      assert [
               %VarInfo{name: :b, positions: [{14, 3}]}
             ] = state |> get_line_vars(15)

      assert Map.keys(state.lines_to_env[17].versioned_vars) == []
      assert [] = state |> get_line_vars(17)
    end

    test "in quote" do
      state =
        """
        quote do
          abc = 5
        end
        record_env()
        """
        |> string_to_state

      refute Map.has_key?(state.lines_to_env[4].versioned_vars, {:abc, nil})

      assert [] = state |> get_line_vars(4)
    end

    test "in quote unquote" do
      state =
        """
        abc = 5
        quote do
          unquote(abc)
        end
        record_env()
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[5].versioned_vars, {:abc, nil})

      assert [%VarInfo{name: :abc, positions: [{1, 1}, {3, 11}]}] = state |> get_line_vars(5)
    end

    test "in quote unquote_splicing" do
      state =
        """
        abc = foo()
        quote do
          unquote_splicing(abc)
          {unquote_splicing(abc), unquote_splicing(abc)}
          [1 | unquote_splicing(abc)]
          [unquote_splicing(abc) | [1]]
        end
        record_env()
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[8].versioned_vars, {:abc, nil})

      assert [
               %VarInfo{
                 name: :abc,
                 positions: [{1, 1}, {3, 20}, {4, 21}, {4, 44}, {5, 25}, {6, 21}]
               }
             ] = state |> get_line_vars(8)
    end

    test "in unquote fragment" do
      state =
        """
        defmodule MyModuleWithFuns do
          kv = [foo: 1, bar: 2] |> IO.inspect
          Enum.each(kv, fn {k, v} ->
            @spec unquote(k)() :: unquote(v)
            @type unquote(k)() :: unquote(v)
            defdelegate unquote(k)(), to: Foo
            def unquote(k)() do
              unquote(v)
              record_env()
            end
          end)

          keys = [{:foo, [], nil}, {:bar, [], nil}]
          @spec foo_splicing(unquote_splicing(keys)) :: :ok
          @type foo_splicing(unquote_splicing(keys)) :: :ok
          defdelegate foo_splicing(unquote_splicing(keys)), to: Foo
          def foo_splicing(unquote_splicing(keys)) do
            record_env()
          end
        end
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[9].versioned_vars) == [{:k, nil}, {:kv, nil}, {:v, nil}]

      assert [
               %VarInfo{name: :k, positions: [{3, 21}, {4, 19}, {5, 19}, {6, 25}, {7, 17}]},
               %VarInfo{name: :kv, positions: [{2, 3}, {3, 13}]},
               %VarInfo{name: :v, positions: [{3, 24}, {4, 35}, {5, 35}, {8, 15}]}
             ] = state |> get_line_vars(9)

      assert Map.keys(state.lines_to_env[18].versioned_vars) == [keys: nil, kv: nil]

      assert [
               %VarInfo{
                 name: :keys,
                 positions: [{13, 3}, {14, 39}, {15, 39}, {16, 45}, {17, 37}]
               },
               %VarInfo{name: :kv, positions: [{2, 3}, {3, 13}]}
             ] = state |> get_line_vars(18)
    end

    if Version.match?(System.version(), ">= 1.18.0") do
      test "in unquote fragment defguard" do
        state =
          """
          defmodule MyModuleWithFuns do
            kv = [foo: 1, bar: 2] |> IO.inspect
            Enum.each(kv, fn {k, v} ->
              defguard unquote(k)(a) when is_integer(unquote(v)) and record_env()
            end)

            keys = [{:foo, [], nil}, {:bar, [], nil}]
            defguard foo_splicing(unquote_splicing(keys)) when record_env()
          end
          """
          |> string_to_state

        assert Map.keys(state.lines_to_env[4].versioned_vars) == [
                 {:a, nil},
                 {:k, nil},
                 {:kv, nil},
                 {:v, nil}
               ]

        assert [
                 %VarInfo{name: :a, positions: [{4, 25}]},
                 %VarInfo{name: :k, positions: [{3, 21}, {4, 22}]},
                 %VarInfo{name: :kv, positions: [{2, 3}, {3, 13}]},
                 %VarInfo{name: :v, positions: [{3, 24}, {4, 52}]}
               ] = state |> get_line_vars(4)

        assert Map.keys(state.lines_to_env[8].versioned_vars) == [keys: nil, kv: nil]

        assert [
                 %VarInfo{
                   name: :keys,
                   positions: [{7, 3}, {8, 42}]
                 },
                 %VarInfo{name: :kv, positions: [{2, 3}, {3, 13}]}
               ] = state |> get_line_vars(8)
      end
    end

    test "in capture" do
      state =
        """
        abc = 5
        & [
          &1,
          abc,
          cde = 1,
          record_env()  
        ]
        record_env()
        """
        |> string_to_state

      assert [{:"&1", _}, {:abc, nil}] = Map.keys(state.lines_to_env[6].versioned_vars)

      assert [
               %VarInfo{name: :"&1", positions: [{3, 3}]},
               %VarInfo{name: :abc, positions: [{1, 1}, {4, 3}]}
             ] = state |> get_line_vars(6)

      assert Map.keys(state.lines_to_env[8].versioned_vars) == [{:abc, nil}]

      assert [
               %VarInfo{name: :abc, positions: [{1, 1}, {4, 3}]}
             ] = state |> get_line_vars(8)
    end

    test "module body" do
      state =
        """
        x = 1
        defmodule My do
          y = 2
          record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[4].versioned_vars) == [{:x, nil}, {:y, nil}]

      assert [
               %VarInfo{name: :x, positions: [{1, 1}]},
               %VarInfo{name: :y, positions: [{3, 3}]}
             ] = state |> get_line_vars(4)

      assert Map.keys(state.lines_to_env[6].versioned_vars) == [{:x, nil}]

      assert [
               %VarInfo{name: :x, positions: [{1, 1}]}
             ] = state |> get_line_vars(6)
    end

    test "module body usage" do
      state =
        """
        x = 1
        defmodule My do
          foo(x)
          record_env()
        end
        record_env()
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[4].versioned_vars) == [{:x, nil}]

      assert [
               %VarInfo{name: :x, positions: [{1, 1}, {3, 7}]}
             ] = state |> get_line_vars(4)

      assert Map.keys(state.lines_to_env[6].versioned_vars) == [{:x, nil}]

      assert [
               %VarInfo{name: :x, positions: [{1, 1}, {3, 7}]}
             ] = state |> get_line_vars(6)
    end

    test "def body" do
      state =
        """
        defmodule My do
          x = 1
          def foo() do
            abc = bar()
            record_env()
          end
          record_env()
        end
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[5].versioned_vars, {:abc, nil})
      refute Map.has_key?(state.lines_to_env[5].versioned_vars, {:x, nil})

      assert [
               %VarInfo{name: :abc, positions: [{4, 5}]}
             ] = state |> get_line_vars(5)

      refute Map.has_key?(state.lines_to_env[7].versioned_vars, {:abc, nil})
      assert Map.has_key?(state.lines_to_env[7].versioned_vars, {:x, nil})

      assert [
               %VarInfo{name: :x, positions: [{2, 3}]}
             ] = state |> get_line_vars(7)
    end

    test "def argument list" do
      state =
        """
        defmodule My do
          def foo(abc) do
            record_env()
          end
          record_env()
        end
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[3].versioned_vars, {:abc, nil})

      assert [
               %VarInfo{name: :abc, positions: [{2, 11}]}
             ] = state |> get_line_vars(3)

      refute Map.has_key?(state.lines_to_env[5].versioned_vars, {:abc, nil})
      assert [] = state |> get_line_vars(5)
    end

    test "def argument usage" do
      state =
        """
        defmodule My do
          def foo(abc) do
            foo(abc)
            record_env()
          end
        end
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[4].versioned_vars, {:abc, nil})

      assert [
               %VarInfo{name: :abc, positions: [{2, 11}, {3, 9}]}
             ] = state |> get_line_vars(4)
    end

    test "def guard" do
      state =
        """
        defmodule My do
          def foo(abc)
            when record_env() do
            record_env()
          end
          record_env()
        end
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[3].versioned_vars, {:abc, nil})

      assert [
               %VarInfo{name: :abc, positions: [{2, 11}]}
             ] = state |> get_line_vars(3)
    end

    test "def guard usage" do
      state =
        """
        defmodule My do
          def foo(abc)
            when abc == 1 and record_env() do
            record_env()
          end
          record_env()
        end
        """
        |> string_to_state

      assert Map.has_key?(state.lines_to_env[3].versioned_vars, {:abc, nil})

      assert [
               %VarInfo{name: :abc, positions: [{2, 11}, {3, 10}]}
             ] = state |> get_line_vars(3)
    end

    test "variables hygiene" do
      state =
        """
        defmodule MyModule do
          import ElixirSenseExample.Math
          def func do
            squared(5)
            IO.puts ""
          end
        end
        """
        |> string_to_state

      assert [] == state |> get_line_vars(5)
    end

    test "variables are added to environment" do
      state =
        """
        defmodule MyModule do
          def func do
            var = :my_var
            IO.puts ""
          end
        end
        """
        |> string_to_state

      assert [%VarInfo{scope_id: scope_id}] = state |> get_line_vars(4)
      assert [%VarInfo{name: :var}] = state.vars_info_per_scope_id[scope_id] |> Map.values()
    end

    test "vars defined inside a function without params" do
      state =
        """
        defmodule MyModule do
          var_out1 = 1
          def func do
            var_in1 = 1
            var_in2 = 1
            IO.puts ""
          end
          var_out2 = 1
          IO.puts ""
        end
        """
        |> string_to_state

      assert [
               %VarInfo{name: :var_in1, positions: [{4, 5}], scope_id: scope_id},
               %VarInfo{name: :var_in2, positions: [{5, 5}], scope_id: scope_id}
             ] = state |> get_line_vars(6)
    end
  end

  describe "vars in ex_unit" do
    test "variables are added to environment in ex_unit test" do
      state =
        """
        defmodule MyModuleTests do
          use ExUnit.Case, async: true
          IO.puts("")
          test "it does what I want", %{some: some} do
            IO.puts("")
          end

          describe "this" do
            test "too does what I want" do
              IO.puts("")
            end
          end

          test "is not implemented"
        end
        """
        |> string_to_state

      assert [%VarInfo{name: :some}] = state |> get_line_vars(5)

      assert Map.has_key?(
               state.mods_funs_to_positions,
               {MyModuleTests, :"test it does what I want", 1}
             )

      assert Map.has_key?(
               state.mods_funs_to_positions,
               {MyModuleTests, :"test this too does what I want", 1}
             )

      assert Map.has_key?(
               state.mods_funs_to_positions,
               {MyModuleTests, :"test is not implemented", 1}
             )
    end

    test "variables are added to environment in ex_unit setup" do
      state =
        """
        defmodule MyModuleTests do
          use ExUnit.Case, async: true

          setup_all %{some: some} do
            IO.puts("")
          end

          setup %{some: other} do
            IO.puts("")
          end

          setup do
            IO.puts("")
          end

          setup :clean_up_tmp_directory

          setup [:clean_up_tmp_directory, :another_setup]

          setup {MyModule, :my_setup_function}
        end
        """
        |> string_to_state

      assert [%VarInfo{name: :some}] = state |> get_line_vars(5)

      assert [%VarInfo{name: :other}] = state |> get_line_vars(9)

      # we do not generate defs - ExUnit.Callbacks.__setup__ is too complicated and generates def names with counters, e.g.
      # :"__ex_unit_setup_#{counter}_#{length(setup)}"
    end
  end

  describe "typespec vars" do
    test "registers type parameters" do
      state =
        """
        defmodule A do
          @type some(p) :: {p, list(p), integer}
        end
        """
        |> string_to_state

      assert [
               %VarInfo{name: :p, positions: [{2, 14}, {2, 21}, {2, 29}]}
             ] = state.vars_info_per_scope_id[2] |> Map.values()
    end

    test "registers spec parameters" do
      state =
        """
        defmodule A do
          @callback some(p) :: {p, list(p), integer, q} when p: integer, q: {p}
        end
        """
        |> string_to_state

      # no position in guard, elixir parses guards as keyword list so p is an atom with no metadata
      # we use when meta instead so the position is not exact...
      assert [
               %VarInfo{name: :p, positions: [{2, 49}, {2, 18}, {2, 25}, {2, 33}, {2, 70}]},
               %VarInfo{name: :q, positions: [{2, 49}, {2, 46}]}
             ] = state.vars_info_per_scope_id[2] |> Map.values()
    end

    test "does not register annotated spec params as type variables" do
      state =
        """
        defmodule A do
          @callback some(p :: integer) :: integer
        end
        """
        |> string_to_state

      assert %{} == state.vars_info_per_scope_id[2]
    end

    test "does not register annotated type elements as variables" do
      state =
        """
        defmodule A do
          @type color :: {red :: integer, green :: integer, blue :: integer}
        end
        """
        |> string_to_state

      assert %{} == state.vars_info_per_scope_id[2]
    end
  end

  @tag requires_source: true
  test "build metadata from kernel.ex" do
    assert get_subject_definition_line(Kernel, :defmodule, 2) =~
             "defmacro defmodule(alias, do_block)"
  end

  @tag requires_source: true
  test "build metadata from kernel/special_forms.ex" do
    assert get_subject_definition_line(Kernel.SpecialForms, :alias, 2) =~
             "defmacro alias(module, opts)"
  end

  test "build_metadata from a module" do
    assert get_subject_definition_line(
             ElixirSenseExample.ModuleWithFunctions,
             :function_arity_zero,
             0
           ) =~ "def function_arity_zero"
  end

  test "closes all scopes" do
    state =
      """
      """
      |> string_to_state

    assert state.attributes == []
    assert state.scope_attributes == []
    assert state.vars_info == []
    assert state.scope_ids == []
    assert state.doc_context == []
    assert state.typedoc_context == []
    assert state.optional_callbacks_context == []
  end

  describe "moduledoc positions" do
    test "moduledoc heredoc version" do
      state =
        """
        defmodule Outer do
          @moduledoc \"\"\"
          This is the here doc version
          \"\"\"
          defmodule Inner do
            @moduledoc \"\"\"
            This is the Inner modules moduledoc
            \"\"\"

            foo()
          end
        end
        """
        |> string_to_state

      assert %{Outer => {5, 3}, Outer.Inner => {9, 5}} = state.moduledoc_positions
    end

    test "moduledoc boolean version" do
      state =
        """
        defmodule Outer do
          @moduledoc false

          foo()
        end
        """
        |> string_to_state

      assert %{Outer => {3, 3}} = state.moduledoc_positions
    end
  end

  test "module attributes" do
    state =
      """
      defmodule MyModule do
        @myattribute String
        IO.puts @myattribute
        defmodule InnerModule do
          @inner_attr %{abc: nil}
          @inner_attr_1 __MODULE__
          IO.puts @inner_attr
        end
        IO.puts ""
        @otherattribute Application.get_env(:elixir_sense, :some_attribute, InnerModule)
      end
      """
      |> string_to_state

    assert [
             %ElixirSense.Core.State.AttributeInfo{
               name: :myattribute,
               positions: [{2, 3}, {3, 11}]
             },
             %AttributeInfo{
               name: :otherattribute,
               positions: [{10, 3}]
             }
           ] = get_line_attributes(state, 10)

    assert [
             %AttributeInfo{
               name: :myattribute,
               positions: [{2, 3}, {3, 11}]
             }
           ] = get_line_attributes(state, 3)

    assert [
             %AttributeInfo{
               name: :inner_attr,
               positions: [{5, 5}, {7, 13}]
             },
             %AttributeInfo{
               name: :inner_attr_1,
               positions: [{6, 5}]
             }
           ] = get_line_attributes(state, 7)

    assert [
             %AttributeInfo{
               name: :myattribute,
               positions: [{2, 3}, {3, 11}]
             }
           ] = get_line_attributes(state, 9)
  end

  describe "binding" do
    test "module attributes binding" do
      state =
        """
        defmodule MyModule do
          @myattribute String
          IO.puts @myattribute
          defmodule InnerModule do
            @inner_attr %{abc: nil}
            @inner_attr_1 __MODULE__
            IO.puts @inner_attr
          end
          IO.puts ""
          @otherattribute Application.get_env(:elixir_sense, :some_attribute, InnerModule)
        end
        """
        |> string_to_state

      assert get_line_attributes(state, 10) == [
               %ElixirSense.Core.State.AttributeInfo{
                 name: :myattribute,
                 positions: [{2, 3}, {3, 11}],
                 type: {:atom, String}
               },
               %AttributeInfo{
                 name: :otherattribute,
                 positions: [{10, 3}],
                 type:
                   {:call, {:atom, Application}, :get_env,
                    [atom: :elixir_sense, atom: :some_attribute, atom: MyModule.InnerModule]}
               }
             ]

      assert get_line_attributes(state, 3) == [
               %AttributeInfo{
                 name: :myattribute,
                 positions: [{2, 3}, {3, 11}],
                 type: {:atom, String}
               }
             ]

      assert get_line_attributes(state, 7) == [
               %AttributeInfo{
                 name: :inner_attr,
                 positions: [{5, 5}, {7, 13}],
                 type: {:map, [abc: {:atom, nil}], nil}
               },
               %AttributeInfo{
                 name: :inner_attr_1,
                 positions: [{6, 5}],
                 type: {:atom, MyModule.InnerModule}
               }
             ]

      assert get_line_attributes(state, 9) == [
               %AttributeInfo{
                 name: :myattribute,
                 positions: [{2, 3}, {3, 11}],
                 type: {:atom, String}
               }
             ]
    end

    test "module attributes rebinding" do
      state =
        """
        defmodule MyModule do
          @myattribute String
          IO.puts ""
          @myattribute List
          @myattribute
          IO.puts ""
          def a do
            @myattribute
          end
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_attributes(state, 3) == [
               %AttributeInfo{
                 name: :myattribute,
                 positions: [{2, 3}],
                 type: {:atom, String}
               }
             ]

      assert get_line_attributes(state, 6) == [
               %AttributeInfo{
                 name: :myattribute,
                 positions: [{2, 3}, {4, 3}, {5, 3}],
                 type: {:atom, List}
               }
             ]

      assert get_line_attributes(state, 10) == [
               %AttributeInfo{
                 name: :myattribute,
                 positions: [{2, 3}, {4, 3}, {5, 3}, {8, 5}],
                 type: {:atom, List}
               }
             ]
    end

    test "module attributes value binding" do
      state =
        """
        defmodule MyModule do
          @myattribute %{abc: String}
          @some_attr @myattribute
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_attributes(state, 4) == [
               %AttributeInfo{
                 name: :myattribute,
                 positions: [{2, 3}, {3, 14}],
                 type: {:map, [abc: {:atom, String}], nil}
               },
               %AttributeInfo{
                 name: :some_attr,
                 positions: [{3, 3}],
                 type: {:attribute, :myattribute}
               }
             ]
    end

    test "variable binding simple case" do
      state =
        """
        var = :my_var
        IO.puts("")
        """
        |> string_to_state

      assert [%VarInfo{type: {:atom, :my_var}}] = state |> get_line_vars(2)
    end

    test "variable binding simple case match context" do
      state =
        """
        case x do
          var = :my_var ->
            IO.puts("")
        end
        """
        |> string_to_state

      if Version.match?(System.version(), "< 1.15.0") do
        assert [%VarInfo{type: {:intersection, [{:atom, :my_var}, {:local_call, :x, _, []}]}}] =
                 state |> get_line_vars(3)
      else
        assert [%VarInfo{type: {:atom, :my_var}}] = state |> get_line_vars(3)
      end
    end

    test "variable binding simple case match context reverse order" do
      state =
        """
        case x do
          :my_var = var ->
            IO.puts("")
        end
        """
        |> string_to_state

      if Version.match?(System.version(), "< 1.15.0") do
        assert [%VarInfo{type: {:intersection, [{:atom, :my_var}, {:local_call, :x, _, []}]}}] =
                 state |> get_line_vars(3)
      else
        assert [%VarInfo{type: {:atom, :my_var}}] = state |> get_line_vars(3)
      end
    end

    test "variable binding simple case match context guard" do
      state =
        """
        receive do
          [v = :ok, var] when is_map(var) ->
            IO.puts("")
        end
        """
        |> string_to_state

      assert [%VarInfo{type: {:atom, :ok}}, %VarInfo{type: {:map, [], nil}}] =
               state |> get_line_vars(3)
    end

    test "module attributes value binding to and from variables" do
      state =
        """
        defmodule MyModule do
          @myattribute %{abc: String}
          var = @myattribute
          @other var
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_attributes(state, 5) == [
               %AttributeInfo{
                 name: :myattribute,
                 positions: [{2, 3}, {3, 9}],
                 type: {:map, [abc: {:atom, String}], nil}
               },
               %AttributeInfo{
                 name: :other,
                 positions: [{4, 3}],
                 type: {:variable, :var, 0}
               }
             ]

      assert [
               %VarInfo{name: :var, type: {:attribute, :myattribute}}
             ] = state |> get_line_vars(5)
    end

    test "variable rebinding" do
      state =
        """
        abc = 1
        some(abc)
        abc = %Abc{cde: 1}
        IO.puts ""
        """
        |> string_to_state

      assert [
               %State.VarInfo{
                 name: :abc,
                 type: {:struct, [cde: {:integer, 1}], {:atom, Abc}, nil},
                 positions: [{3, 1}]
               }
             ] = state |> get_line_vars(4)
    end

    test "tuple destructuring" do
      state =
        """
        defmodule MyModule do
          @myattribute {:ok, %{abc: nil}}
          {:ok, var} = @myattribute
          other = elem(@myattribute, 0)
          IO.puts
          q = {:a, :b, :c}
          {_, _, q1} = q
          IO.puts
        end
        """
        |> string_to_state

      assert get_line_attributes(state, 4) == [
               %AttributeInfo{
                 name: :myattribute,
                 positions: [{2, 3}, {3, 16}],
                 type: {:tuple, 2, [{:atom, :ok}, {:map, [abc: {:atom, nil}], nil}]}
               }
             ]

      assert [
               %VarInfo{
                 name: :other,
                 type: {
                   :call,
                   {:atom, :erlang},
                   :element,
                   [{:integer, 1}, {:attribute, :myattribute}]
                 }
               },
               %VarInfo{
                 name: :var,
                 type: {:tuple_nth, {:attribute, :myattribute}, 1}
               }
             ] = state |> get_line_vars(5)

      assert [
               %VarInfo{
                 name: :q,
                 type: {:tuple, 3, [{:atom, :a}, {:atom, :b}, {:atom, :c}]}
               },
               %VarInfo{
                 name: :q1,
                 type: {:tuple_nth, {:variable, :q, 2}, 2}
               }
             ] =
               state
               |> get_line_vars(8)
               |> Enum.filter(&(&1.name |> Atom.to_string() |> String.starts_with?("q")))
    end

    test "list destructuring" do
      state =
        """
        defmodule MyModule do
          @a []
          @myattribute [:ok, :error, :other]
          @other1 [:some, :error | @myattribute]
          @other2 [:some | @myattribute]
          [var, _var1, _var2] = @myattribute
          [other | rest] = @myattribute
          [a] = @other
          [b] = []
          IO.puts
        end
        """
        |> string_to_state

      assert get_line_attributes(state, 5) == [
               %AttributeInfo{
                 name: :a,
                 positions: [{2, 3}],
                 type: {:list, :empty}
               },
               %AttributeInfo{
                 name: :myattribute,
                 positions: [{3, 3}, {4, 28}, {5, 20}],
                 type: {:list, {:atom, :ok}}
               },
               %AttributeInfo{
                 name: :other1,
                 positions: [{4, 3}],
                 type: {:list, {:atom, :some}}
               },
               %AttributeInfo{name: :other2, positions: [{5, 3}], type: {:list, {:atom, :some}}}
             ]

      assert [
               %VarInfo{
                 name: :_var1,
                 type: {:list_head, {:list_tail, {:attribute, :myattribute}}}
               },
               %VarInfo{
                 name: :_var2,
                 type: {:list_head, {:list_tail, {:list_tail, {:attribute, :myattribute}}}}
               },
               %VarInfo{
                 name: :a,
                 type: {:list_head, {:attribute, :other}}
               },
               %VarInfo{
                 name: :b,
                 type: {:list_head, {:list, :empty}}
               },
               %VarInfo{name: :other, type: {:list_head, {:attribute, :myattribute}}},
               %VarInfo{name: :rest, type: {:list_tail, {:attribute, :myattribute}}},
               %VarInfo{name: :var, type: {:list_head, {:attribute, :myattribute}}}
             ] = state |> get_line_vars(10)
    end

    test "list destructuring for" do
      state =
        """
        defmodule MyModule do
          @myattribute [:ok, :error, :other]
          for a <- @myattribute do
            b = a
            IO.puts
          end

          for a <- @myattribute, a1 = @myattribute, a2 <- a1 do
            b = a
            IO.puts
          end
        end
        """
        |> string_to_state

      assert [
               %VarInfo{name: :a, type: {:for_expression, {:attribute, :myattribute}}},
               %VarInfo{name: :b, type: {:variable, :a, 0}}
             ] = state |> get_line_vars(5)

      assert [
               %VarInfo{name: :a, type: {:for_expression, {:attribute, :myattribute}}},
               %VarInfo{name: :a1, type: {:attribute, :myattribute}},
               %VarInfo{name: :a2, type: {:for_expression, {:variable, :a1, 3}}},
               %VarInfo{name: :b, type: {:variable, :a, 2}}
             ] = state |> get_line_vars(10)
    end

    test "map destructuring" do
      state =
        """
        defmodule MyModule do
          @a %{}
          @myattribute %{ok: :a, error: b, other: :c}
          @other %{"a" => :a, "b" => b}
          %{error: var1} = @myattribute
          %{"a" => var2} = @other
          IO.puts
        end
        """
        |> string_to_state

      assert [
               %VarInfo{
                 name: :var1,
                 type: {:map_key, {:attribute, :myattribute}, {:atom, :error}}
               },
               # NOTE non atom keys currently not supported
               %VarInfo{
                 name: :var2,
                 type: {:map_key, {:attribute, :other}, nil}
               }
             ] = state |> get_line_vars(7)
    end

    test "map destructuring for" do
      state =
        """
        defmodule MyModule do
          @myattribute %{ok: :a, error: b, other: :c}
          for {k, v} <- @myattribute do
            IO.puts
          end
        end
        """
        |> string_to_state

      assert [
               %VarInfo{
                 name: :k,
                 type: {:tuple_nth, {:for_expression, {:attribute, :myattribute}}, 0}
               },
               %VarInfo{
                 name: :v,
                 type: {:tuple_nth, {:for_expression, {:attribute, :myattribute}}, 1}
               }
             ] = state |> get_line_vars(4)
    end

    test "struct destructuring" do
      state =
        """
        defmodule MyModule do
          @a %My{}
          @myattribute %My{ok: :a, error: b, other: :c}
          %{error: var1} = @myattribute
          %My{error: other} = @myattribute
          IO.puts
        end
        """
        |> string_to_state

      assert [
               %VarInfo{
                 name: :other,
                 type: {:map_key, {:attribute, :myattribute}, {:atom, :error}}
               },
               %VarInfo{
                 name: :var1,
                 type: {:map_key, {:attribute, :myattribute}, {:atom, :error}}
               }
             ] = state |> get_line_vars(6)
    end

    test "binding in with expression" do
      state =
        """
        defmodule MyModule do
          @myattribute [:ok, :error, :other]
          with a <- @myattribute do
            b = a
            IO.puts
          end
        end
        """
        |> string_to_state

      assert [
               %VarInfo{name: :a, type: {:attribute, :myattribute}},
               %VarInfo{name: :b, type: {:variable, :a, 0}}
             ] = state |> get_line_vars(5)
    end

    test "binding in with expression more complex" do
      state =
        """
        defmodule MyModule do
          @myattribute [:ok, :error, :other]
          with a <- @myattribute,
            b = Date.utc_now(),
            [c | _] <- a do
            IO.puts
          end
        end
        """
        |> string_to_state

      assert [
               %VarInfo{name: :a, type: {:attribute, :myattribute}},
               %VarInfo{name: :b, type: {:call, {:atom, Date}, :utc_now, []}},
               %VarInfo{name: :c, type: {:list_head, {:variable, :a, 0}}}
             ] = state |> get_line_vars(6)
    end

    test "binding in with expression with guard" do
      state =
        """
        defmodule MyModule do
          @myattribute [:ok, :error, :other]
          with [a | _] when is_atom(a) <- @myattribute do
            IO.puts
          end
        end
        """
        |> string_to_state

      assert [
               %VarInfo{
                 name: :a,
                 type: {:intersection, [:atom, {:list_head, {:attribute, :myattribute}}]}
               }
             ] = state |> get_line_vars(4)
    end

    test "binding in with expression else" do
      state =
        """
        defmodule MyModule do
          @myattribute [:ok, :error, :other]
          with a <- @myattribute do
            b = a
            IO.puts
          else
            a = :ok ->
              IO.puts
          end
        end
        """
        |> string_to_state

      assert [
               %VarInfo{name: :a, type: {:atom, :ok}}
             ] = state |> get_line_vars(8)
    end

    test "vars binding" do
      state =
        """
        defmodule MyModule do
          def func do
            var = String
            IO.puts ""
            var = Map
            IO.puts ""
            if abc do
              IO.puts ""
              var = List
              IO.puts ""
              var = Enum
              IO.puts ""
            end
            IO.puts ""
            var = Atom
            IO.puts ""
            other = var
            IO.puts ""
          end
        end
        """
        |> string_to_state

      assert [%VarInfo{type: {:atom, String}}] = state |> get_line_vars(4)

      assert [%VarInfo{type: {:atom, Map}}] =
               state |> get_line_vars(6)

      assert [%VarInfo{type: {:atom, Map}}] =
               state |> get_line_vars(8)

      assert [
               %VarInfo{type: {:atom, List}}
             ] = state |> get_line_vars(10)

      assert [
               %VarInfo{type: {:atom, Enum}}
             ] = state |> get_line_vars(12)

      assert [%VarInfo{type: {:atom, Map}}] =
               state |> get_line_vars(14)

      assert [
               %VarInfo{type: {:atom, Atom}}
             ] = state |> get_line_vars(16)

      assert [
               %VarInfo{name: :other, type: {:variable, :var, 5}},
               %VarInfo{type: {:atom, Atom}}
             ] = state |> get_line_vars(18)
    end

    test "call binding" do
      state =
        """
        defmodule MyModule do
          def remote_calls do
            var1 = DateTime.now
            var2 = :erlang.now()
            var3 = __MODULE__.now(:abc)
            var4 = "Etc/UTC" |> DateTime.now
            IO.puts ""
          end

          def local_calls do
            var1 = now
            var2 = now()
            var3 = now(:abc)
            var4 = :abc |> now
            var5 = :abc |> now(5)
            IO.puts ""
          end

          @attr %{qwe: String}
          def map_field(var1, abc) do
            var1 = var1.abc
            var2 = @attr.qwe(0)
            var3 = abc.cde.efg
            IO.puts ""
          end
        end
        """
        |> string_to_state

      assert [
               %VarInfo{name: :var1, type: {:call, {:atom, DateTime}, :now, []}},
               %VarInfo{name: :var2, type: {:call, {:atom, :erlang}, :now, []}},
               %VarInfo{name: :var3, type: {:call, {:atom, MyModule}, :now, [{:atom, :abc}]}},
               %VarInfo{name: :var4, type: {:call, {:atom, DateTime}, :now, [nil]}}
             ] = state |> get_line_vars(7)

      assert [
               %VarInfo{name: :var1, type: maybe_local_call},
               %VarInfo{name: :var2, type: {:local_call, :now, {12, 12}, []}},
               %VarInfo{name: :var3, type: {:local_call, :now, {13, 12}, [{:atom, :abc}]}},
               %VarInfo{name: :var4, type: {:local_call, :now, {14, 20}, [{:atom, :abc}]}},
               %VarInfo{
                 name: :var5,
                 type: {:local_call, :now, {15, 20}, [{:atom, :abc}, {:integer, 5}]}
               }
             ] = state |> get_line_vars(16)

      if Version.match?(System.version(), "< 1.15.0") do
        assert maybe_local_call == {:local_call, :now, {11, 12}, []}
      else
        assert maybe_local_call == nil
      end

      assert [
               %VarInfo{name: :abc, type: nil},
               %VarInfo{name: :var1, type: {:call, {:variable, :var1, 0}, :abc, []}},
               %VarInfo{name: :var2, type: {:call, {:attribute, :attr}, :qwe, [{:integer, 0}]}},
               %VarInfo{
                 name: :var3,
                 type: {:call, {:call, {:variable, :abc, 1}, :cde, []}, :efg, []}
               }
             ] = state |> get_line_vars(24)
    end

    test "map binding" do
      state =
        """
        defmodule MyModule do
          def func do
            var = %{asd: 5}
            IO.puts ""
            var = %{asd: 5, nested: %{wer: "asd"}}
            IO.puts ""
            var = %{"asd" => "dsds"}
            IO.puts ""
            var = %{asd: 5, zxc: String}
            IO.puts ""
            qwe = %{var | asd: 2, zxc: 5}
            IO.puts ""
            qwe = %{var | asd: 2}
            IO.puts ""

          end
        end
        """
        |> string_to_state

      assert [%VarInfo{type: {:map, [asd: {:integer, 5}], nil}}] = state |> get_line_vars(4)

      assert [
               %VarInfo{
                 type: {:map, [asd: {:integer, 5}, nested: {:map, [wer: nil], nil}], nil}
               }
             ] = state |> get_line_vars(6)

      assert [
               %VarInfo{type: {:map, [], nil}}
             ] = state |> get_line_vars(8)

      assert [
               %VarInfo{type: {:map, [asd: {:integer, 5}, zxc: {:atom, String}], nil}}
             ] = state |> get_line_vars(10)

      assert [
               %VarInfo{
                 type: {:map, [asd: {:integer, 2}, zxc: {:integer, 5}], {:variable, :var, 3}}
               }
             ] =
               state |> get_line_vars(12) |> Enum.filter(&(&1.name == :qwe))

      assert [
               %VarInfo{type: {:map, [{:asd, {:integer, 2}}], {:variable, :var, 3}}}
             ] = state |> get_line_vars(14) |> Enum.filter(&(&1.name == :qwe))
    end

    test "struct binding" do
      state =
        """
        defmodule MyModule do
          def func(%MyStruct{} = var1, var2 = %:other_struct{}, var3 = %__MODULE__{},
            var4 = %__MODULE__.Sub{}, var7 = %_{}) do
            IO.puts ""
          end

          def some(a) do
            asd = %Some{sub: Atom}
            IO.puts ""
            asd = %Other{a | sub: Atom}
            IO.puts ""
            asd = %{asd | other: 123}
            IO.puts ""
            z = x = asd
            IO.puts ""
          end
        end
        """
        |> string_to_state

      assert [
               %VarInfo{name: :var1, type: {:struct, [], {:atom, MyStruct}, nil}},
               %VarInfo{name: :var2, type: {:struct, [], {:atom, :other_struct}, nil}},
               %VarInfo{name: :var3, type: {:struct, [], {:atom, MyModule}, nil}},
               %VarInfo{name: :var4, type: {:struct, [], {:atom, MyModule.Sub}, nil}},
               %VarInfo{name: :var7, type: {:struct, [], nil, nil}}
             ] = state |> get_line_vars(4)

      assert %VarInfo{name: :asd, type: {:struct, [{:sub, {:atom, Atom}}], {:atom, Some}, nil}} =
               state |> get_line_vars(9) |> Enum.find(&(&1.name == :asd))

      assert [
               %VarInfo{
                 name: :asd,
                 type: {:struct, [{:sub, {:atom, Atom}}], {:atom, Other}, {:variable, :a, 0}}
               }
             ] = state |> get_line_vars(11) |> Enum.filter(&(&1.name == :asd))

      assert [
               %VarInfo{
                 name: :asd,
                 type: {:map, [{:other, {:integer, 123}}], {:variable, :asd, 2}}
               }
             ] = state |> get_line_vars(13) |> Enum.filter(&(&1.name == :asd))

      assert [
               %VarInfo{name: :x, type: {:variable, :asd, 3}},
               %VarInfo{name: :z, type: {:variable, :asd, 3}}
             ] = state |> get_line_vars(15) |> Enum.filter(&(&1.name in [:x, :z]))
    end

    test "struct binding understands builtin sigils and ranges" do
      state =
        """
        defmodule MyModule do
          def some() do
            var1 = ~D[2000-01-01]
            var2 = ~T[13:00:07]
            var3 = ~U[2015-01-13 13:00:07Z]
            var4 = ~N[2000-01-01 23:00:07]
            var5 = ~r/foo/iu
            var6 = ~R(f\#{1,3}o)
            var7 = 12..34
            var8 = 12..34//1
            IO.puts ""
          end
        end
        """
        |> string_to_state

      assert [
               %VarInfo{name: :var1, type: {:struct, _, {:atom, Date}, nil}},
               %VarInfo{name: :var2, type: {:struct, _, {:atom, Time}, nil}},
               %VarInfo{name: :var3, type: {:struct, _, {:atom, DateTime}, nil}},
               %VarInfo{name: :var4, type: {:struct, _, {:atom, NaiveDateTime}, nil}},
               %VarInfo{name: :var5, type: regex_type_1},
               %VarInfo{name: :var6, type: regex_type_2},
               %VarInfo{name: :var7, type: {:struct, _, {:atom, Range}, nil}},
               %VarInfo{name: :var8, type: {:struct, _, {:atom, Range}, nil}}
             ] = state |> get_line_vars(11)

      # if Version.match?(System.version(), ">= 1.18.4") do
      assert {:struct, _, {:atom, Regex}, nil} = regex_type_1
      assert {:struct, _, {:atom, Regex}, nil} = regex_type_2
      # else
      #   assert {:call, {:atom, Regex}, :compile!, [nil, nil]} = regex_type_1
      #   assert {:call, {:atom, Regex}, :compile!, [nil, nil]} = regex_type_2
      # end
    end

    test "struct binding understands stepped ranges" do
      state =
        """
        defmodule MyModule do
          def some() do
            var1 = 12..34//2
            IO.puts ""
          end
        end
        """
        |> string_to_state

      assert [
               %VarInfo{
                 name: :var1,
                 type:
                   {:struct,
                    [{:first, {:integer, 12}}, {:last, {:integer, 34}}, {:step, {:integer, 2}}],
                    {:atom, Range}, nil}
               }
             ] = state |> get_line_vars(4)
    end

    test "two way refinement in match context" do
      state =
        """
        defmodule MyModule do
          def some(%MyState{formatted: formatted} = state) do
            IO.puts ""

            case :ok do
              %{foo: 1} = state = %{bar: 1} = x ->
                IO.puts ""
            end
          end
        end
        """
        |> string_to_state

      assert [
               %VarInfo{
                 name: :formatted,
                 type: nil
               },
               %VarInfo{
                 name: :state,
                 type: {:struct, [formatted: nil], {:atom, MyState}, nil}
               }
             ] = state |> get_line_vars(3)

      assert [
               %VarInfo{
                 name: :formatted
               },
               %VarInfo{
                 name: :state,
                 type: {
                   :intersection,
                   [
                     {:map, [foo: {:integer, 1}], nil},
                     {:map, [bar: {:integer, 1}], nil},
                     {:atom, :ok}
                   ]
                 }
               },
               %VarInfo{
                 name: :x,
                 type: {
                   :intersection,
                   [
                     {:map, [foo: {:integer, 1}], nil},
                     {:map, [bar: {:integer, 1}], nil},
                     {:atom, :ok}
                   ]
                 }
               }
             ] = state |> get_line_vars(7)
    end

    test "two way refinement in match context nested" do
      state =
        """
        defmodule MyModule do
          def some(%{foo: 1} = state = %{bar: 1} = x) do
            IO.puts ""
          end
        end
        """
        |> string_to_state

      assert [
               %VarInfo{
                 name: :state,
                 type: {
                   :intersection,
                   [{:map, [foo: {:integer, 1}], nil}, {:map, [bar: {:integer, 1}], nil}]
                 }
               },
               %VarInfo{
                 name: :x,
                 type: {
                   :intersection,
                   [{:map, [foo: {:integer, 1}], nil}, {:map, [bar: {:integer, 1}], nil}]
                 }
               }
             ] = state |> get_line_vars(3)
    end

    test "two way refinement in match context nested case" do
      state =
        """
        defmodule MyModule do
          def some(state) do
            case :ok do
              %{foo: 1} = state = %{bar: 1} = x ->
                IO.puts ""
            end
          end
        end
        """
        |> string_to_state

      assert [
               %VarInfo{
                 name: :state,
                 type:
                   {:intersection,
                    [
                      {:map, [foo: {:integer, 1}], nil},
                      {:map, [bar: {:integer, 1}], nil},
                      {:atom, :ok}
                    ]}
               },
               %VarInfo{
                 name: :x,
                 type:
                   {:intersection,
                    [
                      {:map, [foo: {:integer, 1}], nil},
                      {:map, [bar: {:integer, 1}], nil},
                      {:atom, :ok}
                    ]}
               }
             ] = state |> get_line_vars(5)
    end

    test "two way refinement in nested `=` binding" do
      state =
        """
        defmodule MyModule do
          def some(socket) do
            %MyState{formatted: formatted} = state = socket.assigns.state
            IO.puts ""
          end
        end
        """
        |> string_to_state

      assert [
               %VarInfo{
                 name: :formatted,
                 type: {
                   :map_key,
                   {:call, {:call, {:variable, :socket, 0}, :assigns, []}, :state, []},
                   {:atom, :formatted}
                 }
               },
               %ElixirSense.Core.State.VarInfo{
                 name: :socket,
                 type: nil
               },
               %VarInfo{
                 name: :state,
                 type:
                   {:intersection,
                    [
                      {:call, {:call, {:variable, :socket, 0}, :assigns, []}, :state, []},
                      {:struct, [formatted: nil], {:atom, MyState}, nil}
                    ]}
               }
             ] = state |> get_line_vars(4)
    end

    test "case binding" do
      state =
        """
        defmodule MyModule do
          def some() do
            case Some.call() do
              {:ok, x} ->
                IO.puts ""
            end
          end
        end
        """
        |> string_to_state

      assert [
               %VarInfo{
                 name: :x,
                 type: {:tuple_nth, {:call, {:atom, Some}, :call, []}, 1}
               }
             ] = state |> get_line_vars(5)
    end

    test "case binding with match" do
      state =
        """
        defmodule MyModule do
          def some() do
            case Some.call() do
              {:ok, x} = res ->
                IO.puts ""
            end
          end
        end
        """
        |> string_to_state

      assert [
               %VarInfo{
                 name: :res,
                 type:
                   {:intersection,
                    [
                      {:tuple, 2, [{:atom, :ok}, nil]},
                      {:call, {:atom, Some}, :call, []}
                    ]}
               },
               %VarInfo{
                 name: :x,
                 type: {:tuple_nth, {:call, {:atom, Some}, :call, []}, 1}
               }
             ] = state |> get_line_vars(5)
    end

    test "rescue binding" do
      state =
        """
        defmodule MyModule do
          def some() do
            try do
              Some.call()
            rescue
              e0 in ArgumentError ->
                IO.puts ""
              e1 in [ArgumentError] ->
                IO.puts ""
              e2 in [RuntimeError, Enum.EmptyError] ->
                IO.puts ""
              e3 in _ ->
                IO.puts ""
              e4 ->
                IO.puts ""
            else
              a ->
                IO.puts ""
            end
          end
        end
        """
        |> string_to_state

      assert [
               %VarInfo{
                 name: :e0,
                 type: {:struct, [], {:atom, ArgumentError}, nil}
               }
             ] = state |> get_line_vars(7)

      assert [
               %VarInfo{
                 name: :e1,
                 type: {:struct, [], {:atom, ArgumentError}, nil}
               }
             ] = state |> get_line_vars(9)

      assert [
               %VarInfo{
                 name: :e2,
                 type: {
                   :union,
                   [
                     {:struct, [], {:atom, RuntimeError}, nil},
                     {:struct, [], {:atom, Enum.EmptyError}, nil}
                   ]
                 }
               }
             ] = state |> get_line_vars(11)

      assert [
               %VarInfo{
                 name: :e3,
                 type: {:struct, [], {:atom, Exception}, nil}
               }
             ] = state |> get_line_vars(13)

      assert [
               %VarInfo{
                 name: :e4,
                 type: {:struct, [], {:atom, Exception}, nil}
               }
             ] = state |> get_line_vars(15)

      assert [
               %VarInfo{
                 name: :a,
                 type: nil
               }
             ] = state |> get_line_vars(18)
    end

    test "def rescue binding" do
      state =
        """
        defmodule MyModule do
          def some() do
            Some.call()
          rescue
            e0 in ArgumentError ->
              IO.puts ""
          end
        end
        """
        |> string_to_state

      assert [
               %VarInfo{
                 name: :e0,
                 type: {:struct, [], {:atom, ArgumentError}, nil}
               }
             ] = state |> get_line_vars(6)
    end

    test "vars binding by pattern matching with pin operators" do
      state =
        """
        defmodule MyModule do
          def func(a) do
            b = 1
            case a do
              %{b: 2} = a1 ->
                IO.puts ""
              %{b: ^b} = a2 ->
                IO.puts ""
            end
          end
        end
        """
        |> string_to_state

      vars = state |> get_line_vars(6)

      assert %VarInfo{
               name: :a1,
               positions: [{5, 17}],
               type: {:intersection, [{:map, [b: {:integer, 2}], nil}, {:variable, :a, 0}]}
             } = Enum.find(vars, &(&1.name == :a1))

      vars = state |> get_line_vars(8)

      assert %VarInfo{
               name: :a2,
               positions: [{7, 18}],
               type: {
                 :intersection,
                 [{:map, [b: {:variable, :b, 1}], nil}, {:variable, :a, 0}]
               }
             } = Enum.find(vars, &(&1.name == :a2))
    end
  end

  describe "var" do
    test "vars defined inside a function `after`/`rescue`/`catch`" do
      state =
        """
        defmodule MyModule do
          var_out1 = 1
          def func(var_arg) do
            var_in1 = 1
            var_in2 = 1
            IO.puts ""
          after
            var_after = 1
            IO.puts ""
          end
        end
        """
        |> string_to_state

      assert match?(
               [
                 %VarInfo{name: :var_arg, positions: [{3, 12}], scope_id: scope_id_1},
                 %VarInfo{name: :var_in1, positions: [{4, 5}], scope_id: scope_id_2},
                 %VarInfo{name: :var_in2, positions: [{5, 5}], scope_id: scope_id_2}
               ]
               when scope_id_2 > scope_id_1,
               state |> get_line_vars(6)
             )

      assert match?(
               [
                 %VarInfo{name: :var_after, positions: [{8, 5}], scope_id: scope_id_2},
                 %VarInfo{name: :var_arg, positions: [{3, 12}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               state |> get_line_vars(9)
             )
    end

    test "vars defined inside a function with params" do
      state =
        """
        defmodule MyModule do
          var_out1 = 1
          def func(%{key1: par1, key2: [par2|[par3, _]]}, par4, _par5) do
            var_in1 = 1
            var_in2 = 1
            IO.puts ""
          end
          defp func1(arg), do: arg + 1
          var_out2 = 1
        end
        """
        |> string_to_state

      assert [
               %VarInfo{name: :_par5, positions: [{3, 57}], scope_id: scope_id_1},
               %VarInfo{name: :par1, positions: [{3, 20}], scope_id: scope_id_1},
               %VarInfo{name: :par2, positions: [{3, 33}], scope_id: scope_id_1},
               %VarInfo{name: :par3, positions: [{3, 39}], scope_id: scope_id_1},
               %VarInfo{name: :par4, positions: [{3, 51}], scope_id: scope_id_1},
               %VarInfo{name: :var_in1, positions: [{4, 5}], scope_id: scope_id_1},
               %VarInfo{name: :var_in2, positions: [{5, 5}], scope_id: scope_id_1}
             ] = state |> get_line_vars(6)

      assert [
               %VarInfo{name: :arg, positions: [{8, 14}, {8, 24}]}
             ] = state |> get_line_vars(8)
    end

    test "rebinding vars" do
      state =
        """
        defmodule MyModule do
          var1 = 1
          def func(%{var: var1, key: [_|[_, var1]]}) do
            var1 = 1
            var1 = 2
            IO.puts ""
          end
          var1 = 1
        end
        """
        |> string_to_state

      vars = state |> get_line_vars(6)

      assert [
               # %VarInfo{name: :var1, positions: [{3, 19}, {3, 37}], scope_id: scope_id_1},
               # %VarInfo{name: :var1, positions: [{4, 5}], scope_id: scope_id_2},
               %VarInfo{name: :var1, positions: [{5, 5}]}
             ] = vars
    end

    test "vars defined inside a module body" do
      state =
        """
        defmodule MyModule do
          var_out1 = 1
          def func do
            var_in = 1
            IO.puts ""
          end
          var_out2 = 1
          IO.puts ""
        end
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[5].versioned_vars) == [
               {:var_in, nil}
             ]

      assert [
               %VarInfo{name: :var_in, positions: [{4, 5}]}
             ] = state |> get_line_vars(5)

      assert Map.keys(state.lines_to_env[8].versioned_vars) == [
               {:var_out1, nil},
               {:var_out2, nil}
             ]

      assert [
               %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id},
               %VarInfo{name: :var_out2, positions: [{7, 3}], scope_id: scope_id}
             ] = state |> get_line_vars(8)
    end

    test "vars defined in a `for` comprehension" do
      state =
        """
        defmodule MyModule do
          var_out1 = 1
          IO.puts ""
          for var_on <- [1,2], var_on != 2, var_on1 = var_on + 1 do
            var_in = 1
            IO.puts ""
          end
          var_out2 = 1
          IO.puts ""
        end
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[3].versioned_vars) == [{:var_out1, nil}]

      assert [
               %VarInfo{name: :var_out1, positions: [{2, 3}]}
             ] = get_line_vars(state, 3)

      assert Map.keys(state.lines_to_env[6].versioned_vars) == [
               {:var_in, nil},
               {:var_on, nil},
               {:var_on1, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{
                   name: :var_in,
                   positions: [{5, 5}],
                   scope_id: scope_id_2
                 },
                 %VarInfo{
                   name: :var_on,
                   positions: [{4, 7}, {4, 24}, {4, 47}],
                   scope_id: scope_id_2
                 },
                 %VarInfo{
                   name: :var_on1,
                   positions: [{4, 37}],
                   scope_id: scope_id_2
                 },
                 %VarInfo{
                   name: :var_out1,
                   positions: [{2, 3}],
                   scope_id: scope_id_1
                 }
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 6)
             )

      assert Map.keys(state.lines_to_env[9].versioned_vars) == [
               {:var_out1, nil},
               {:var_out2, nil}
             ]

      assert [
               %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id},
               %VarInfo{name: :var_out2, positions: [{8, 3}], scope_id: scope_id}
             ] = get_line_vars(state, 9)
    end

    test "vars defined in a `with` expression" do
      state =
        """
        defmodule MyModule do
          var_out1 = 1
          IO.puts ""
          with var_on <- [1,2], var_on != 2, var_on1 = var_on + 1 do
            var_in = 1
            IO.puts ""
          end
          var_out2 = 1
          IO.puts ""
        end
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[3].versioned_vars) == [{:var_out1, nil}]

      assert [
               %VarInfo{name: :var_out1, positions: [{2, 3}]}
             ] = get_line_vars(state, 3)

      assert Map.keys(state.lines_to_env[6].versioned_vars) == [
               {:var_in, nil},
               {:var_on, nil},
               {:var_on1, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{
                   name: :var_in,
                   positions: [{5, 5}],
                   scope_id: scope_id_2
                 },
                 %VarInfo{
                   name: :var_on,
                   positions: [{4, 8}, {4, 25}, {4, 48}],
                   scope_id: scope_id_2
                 },
                 %VarInfo{
                   name: :var_on1,
                   positions: [{4, 38}],
                   scope_id: scope_id_2
                 },
                 %VarInfo{
                   name: :var_out1,
                   positions: [{2, 3}],
                   scope_id: scope_id_1
                 }
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 6)
             )

      assert Map.keys(state.lines_to_env[9].versioned_vars) == [
               {:var_out1, nil},
               {:var_out2, nil}
             ]

      assert [
               %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id},
               %VarInfo{name: :var_out2, positions: [{8, 3}], scope_id: scope_id}
             ] = get_line_vars(state, 9)
    end

    test "vars defined in a `if/else` expression" do
      state =
        """
        defmodule MyModule do
          var_out1 = 1
          if var_on = true do
            var_in_if = 1
            IO.puts ""
          else
            var_in_else = 1
            IO.puts x
          end
          var_out2 = 1
          IO.puts ""
        end
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[5].versioned_vars) == [
               {:var_in_if, nil},
               {:var_on, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :var_in_if, positions: [{4, 5}], scope_id: scope_id_2},
                 %VarInfo{name: :var_on, positions: [{3, 6}], scope_id: scope_id_1},
                 %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 5)
             )

      assert Map.keys(state.lines_to_env[8].versioned_vars)
             |> Enum.reject(&(&1 |> elem(1) != nil)) == [
               {:var_in_else, nil},
               {:var_on, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :var_in_else, positions: [{7, 5}], scope_id: scope_id_2},
                 %VarInfo{name: :var_on, positions: [{3, 6}], scope_id: scope_id_1},
                 %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 8)
             )

      assert Map.keys(state.lines_to_env[11].versioned_vars) == [
               {:var_on, nil},
               {:var_out1, nil},
               {:var_out2, nil}
             ]

      assert [
               %VarInfo{name: :var_on, positions: [{3, 6}], scope_id: scope_id},
               %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id},
               %VarInfo{name: :var_out2, positions: [{10, 3}], scope_id: scope_id}
             ] = get_line_vars(state, 11)
    end

    test "vars defined inside a `fn`" do
      state =
        """
        defmodule MyModule do
          var_out1 = 1
          fn var_on ->
            var_in = 1
            IO.puts ""
          end
          var_out2 = 1
          IO.puts ""
        end
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[5].versioned_vars) == [
               {:var_in, nil},
               {:var_on, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{
                   name: :var_in,
                   positions: [{4, 5}],
                   scope_id: scope_id_2
                 },
                 %VarInfo{
                   name: :var_on,
                   positions: [{3, 6}],
                   scope_id: scope_id_2
                 },
                 %VarInfo{
                   name: :var_out1,
                   positions: [{2, 3}],
                   scope_id: scope_id_1
                 }
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 5)
             )

      assert Map.keys(state.lines_to_env[8].versioned_vars) == [
               {:var_out1, nil},
               {:var_out2, nil}
             ]

      assert [
               %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id},
               %VarInfo{name: :var_out2, positions: [{7, 3}], scope_id: scope_id}
             ] = get_line_vars(state, 8)
    end

    test "vars defined inside a `case`" do
      state =
        """
        defmodule MyModule do
          var_out1 = 1
          case var_on0 = var_out1 do
            {var_on1} ->
              var_in1 = 1
              IO.puts ""
            {var_on2} ->
              var_in2 = 2
              IO.puts ""
            var_on3 -> IO.puts ""
          end
          var_out2 = 1
          IO.puts ""
        end
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[6].versioned_vars) == [
               {:var_in1, nil},
               {:var_on0, nil},
               {:var_on1, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{
                   name: :var_in1,
                   positions: [{5, 7}],
                   scope_id: scope_id_2
                 },
                 %VarInfo{
                   name: :var_on0,
                   positions: [{3, 8}],
                   scope_id: scope_id_1
                 },
                 %VarInfo{
                   name: :var_on1,
                   positions: [{4, 6}],
                   scope_id: scope_id_2
                 },
                 %VarInfo{
                   name: :var_out1,
                   positions: [{2, 3}, {3, 18}],
                   scope_id: scope_id_1
                 }
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 6)
             )

      assert Map.keys(state.lines_to_env[9].versioned_vars) == [
               {:var_in2, nil},
               {:var_on0, nil},
               {:var_on2, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :var_in2, positions: [{8, 7}], scope_id: scope_id_2},
                 %VarInfo{name: :var_on0, positions: [{3, 8}], scope_id: scope_id_1},
                 %VarInfo{name: :var_on2, positions: [{7, 6}], scope_id: scope_id_2},
                 %VarInfo{name: :var_out1, positions: [{2, 3}, {3, 18}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 9)
             )

      assert Map.keys(state.lines_to_env[10].versioned_vars) == [
               {:var_on0, nil},
               {:var_on3, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :var_on0, positions: [{3, 8}], scope_id: scope_id_1},
                 %VarInfo{name: :var_on3, positions: [{10, 5}], scope_id: scope_id_2},
                 %VarInfo{name: :var_out1, positions: [{2, 3}, {3, 18}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 10)
             )

      assert Map.keys(state.lines_to_env[13].versioned_vars) == [
               {:var_on0, nil},
               {:var_out1, nil},
               {:var_out2, nil}
             ]

      assert [
               %VarInfo{name: :var_on0, positions: [{3, 8}], scope_id: scope_id},
               %VarInfo{name: :var_out1, positions: [{2, 3}, {3, 18}], scope_id: scope_id},
               %VarInfo{name: :var_out2, positions: [{12, 3}], scope_id: scope_id}
             ] = get_line_vars(state, 13)
    end

    test "vars defined inside a `cond`" do
      state =
        """
        defmodule MyModule do
          var_out1 = 1
          cond do
            1 == 1 ->
              var_in = 1
              IO.puts ""
            var_in1 = Enum.find([], 1) ->
              IO.puts ""
            var_in2 = Enum.find([], 2) -> IO.puts ""
          end
          var_out2 = 1
          IO.puts ""
        end
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[6].versioned_vars) == [
               {:var_in, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :var_in, positions: [{5, 7}], scope_id: scope_id_2},
                 %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 6)
             )

      assert Map.keys(state.lines_to_env[8].versioned_vars) == [
               {:var_in1, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :var_in1, positions: [{7, 5}], scope_id: scope_id_2},
                 %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 8)
             )

      assert Map.keys(state.lines_to_env[9].versioned_vars) == [
               {:var_in2, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :var_in2, positions: [{9, 5}], scope_id: scope_id_2},
                 %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 9)
             )

      assert Map.keys(state.lines_to_env[12].versioned_vars) == [
               {:var_out1, nil},
               {:var_out2, nil}
             ]

      assert [
               %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id},
               %VarInfo{name: :var_out2, positions: [{11, 3}], scope_id: scope_id}
             ] = get_line_vars(state, 12)
    end

    test "vars defined inside a `try`" do
      state =
        """
        defmodule MyModule do
          var_out1 = 1
          try do
            var_in_try = 1
            IO.puts ""
          rescue
            e1 in ArgumentError -> IO.puts ""
            e2 in KeyError ->
              var_in_rescue = 0
              IO.puts ""
          catch
            :exit, reason1 -> IO.puts ""
            reason2 ->
              var_in_catch = 0
              IO.puts ""
          else
            {:atom, var_on_else} -> IO.puts ""
            :atom ->
              var_in_else = 0
              IO.puts ""
          after
            var_in_after = 0
            IO.puts ""
          end
          var_out2 = 1
          IO.puts ""
        end
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[5].versioned_vars) == [
               {:var_in_try, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :var_in_try, positions: [{4, 5}], scope_id: scope_id_2},
                 %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 5)
             )

      assert Map.keys(state.lines_to_env[7].versioned_vars) == [{:e1, nil}, {:var_out1, nil}]

      assert match?(
               [
                 %VarInfo{name: :e1, positions: [{7, 5}], scope_id: scope_id_2},
                 %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 7)
             )

      assert Map.keys(state.lines_to_env[10].versioned_vars) == [
               {:e2, nil},
               {:var_in_rescue, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :e2, positions: [{8, 5}], scope_id: scope_id_2},
                 %VarInfo{name: :var_in_rescue, positions: [{9, 7}], scope_id: scope_id_2},
                 %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 10)
             )

      assert Map.keys(state.lines_to_env[12].versioned_vars) == [
               {:reason1, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :reason1, positions: [{12, 12}], scope_id: scope_id_2},
                 %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 12)
             )

      assert Map.keys(state.lines_to_env[15].versioned_vars) == [
               {:reason2, nil},
               {:var_in_catch, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :reason2, positions: [{13, 5}], scope_id: scope_id_2},
                 %VarInfo{name: :var_in_catch, positions: [{14, 7}], scope_id: scope_id_2},
                 %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 15)
             )

      assert Map.keys(state.lines_to_env[17].versioned_vars) == [
               {:var_on_else, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :var_on_else, positions: [{17, 13}], scope_id: scope_id_2},
                 %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 17)
             )

      assert Map.keys(state.lines_to_env[20].versioned_vars) == [
               {:var_in_else, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :var_in_else, positions: [{19, 7}], scope_id: scope_id_2},
                 %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 20)
             )

      assert Map.keys(state.lines_to_env[23].versioned_vars) == [
               {:var_in_after, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :var_in_after, positions: [{22, 5}], scope_id: scope_id_2},
                 %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 23)
             )

      assert Map.keys(state.lines_to_env[26].versioned_vars) == [
               {:var_out1, nil},
               {:var_out2, nil}
             ]

      assert [
               %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id},
               %VarInfo{name: :var_out2, positions: [{25, 3}], scope_id: scope_id}
             ] = get_line_vars(state, 26)
    end

    test "vars defined inside a `receive`" do
      state =
        """
        defmodule MyModule do
          var_out1 = 1
          receive do
            {:atom, msg1} -> IO.puts ""
            :msg ->
              var_in = 0
              IO.puts ""
          after
            300 ->
              var_in_after = 0
              IO.puts ""
          end
          var_out2 = 1
          IO.puts ""
        end
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[4].versioned_vars) == [{:msg1, nil}, {:var_out1, nil}]

      assert match?(
               [
                 %VarInfo{name: :msg1, positions: [{4, 13}], scope_id: scope_id_2},
                 %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 4)
             )

      assert Map.keys(state.lines_to_env[7].versioned_vars) == [
               {:var_in, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :var_in, positions: [{6, 7}], scope_id: scope_id_2},
                 %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 7)
             )

      assert Map.keys(state.lines_to_env[11].versioned_vars) == [
               {:var_in_after, nil},
               {:var_out1, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :var_in_after, positions: [{10, 7}], scope_id: scope_id_2},
                 %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 11)
             )

      assert Map.keys(state.lines_to_env[14].versioned_vars) == [
               {:var_out1, nil},
               {:var_out2, nil}
             ]

      assert [
               %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: scope_id},
               %VarInfo{name: :var_out2, positions: [{13, 3}], scope_id: scope_id}
             ] = get_line_vars(state, 14)
    end

    test "functions of arity 0 should not be in the vars list" do
      state =
        """
        defmodule MyModule do
          myself = self
          mynode = node()
          IO.puts ""
        end
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[4].versioned_vars) == [{:mynode, nil}, {:myself, nil}]

      assert [
               %VarInfo{name: :mynode, positions: [{3, 3}], scope_id: scope_id},
               %VarInfo{name: :myself, positions: [{2, 3}], scope_id: scope_id}
             ] = get_line_vars(state, 4)
    end

    test "inherited vars" do
      state =
        """
        top_level_var = 1
        IO.puts ""
        defmodule OuterModule do
          outer_module_var = 1
          IO.puts ""
          defmodule InnerModule do
            inner_module_var = 1
            IO.puts ""
            def func do
              func_var = 1
              IO.puts ""
            end
            IO.puts ""
          end
          IO.puts ""
        end
        IO.puts ""
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[2].versioned_vars) == [{:top_level_var, nil}]

      assert [
               %VarInfo{name: :top_level_var, positions: [{1, 1}], scope_id: 0}
             ] = get_line_vars(state, 2)

      assert Map.keys(state.lines_to_env[5].versioned_vars) == [
               {:outer_module_var, nil},
               {:top_level_var, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :outer_module_var, positions: [{4, 3}], scope_id: scope_id_2},
                 %VarInfo{name: :top_level_var, positions: [{1, 1}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 5)
             )

      assert Map.keys(state.lines_to_env[8].versioned_vars) == [
               {:inner_module_var, nil},
               {:outer_module_var, nil},
               {:top_level_var, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :inner_module_var, positions: [{7, 5}], scope_id: scope_id_3},
                 %VarInfo{name: :outer_module_var, positions: [{4, 3}], scope_id: scope_id_2},
                 %VarInfo{name: :top_level_var, positions: [{1, 1}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1 and scope_id_3 > scope_id_2,
               get_line_vars(state, 8)
             )

      assert Map.keys(state.lines_to_env[11].versioned_vars) == [{:func_var, nil}]

      assert [
               %VarInfo{name: :func_var, positions: [{10, 7}]}
             ] = get_line_vars(state, 11)

      assert Map.keys(state.lines_to_env[13].versioned_vars) == [
               {:inner_module_var, nil},
               {:outer_module_var, nil},
               {:top_level_var, nil}
             ]

      assert match?(
               [
                 %VarInfo{name: :inner_module_var, positions: [{7, 5}], scope_id: scope_id_3},
                 %VarInfo{name: :outer_module_var, positions: [{4, 3}], scope_id: scope_id_2},
                 %VarInfo{name: :top_level_var, positions: [{1, 1}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1 and scope_id_3 > scope_id_2,
               get_line_vars(state, 13)
             )

      assert Map.keys(state.lines_to_env[15].versioned_vars) == [
               outer_module_var: nil,
               top_level_var: nil
             ]

      assert match?(
               [
                 %VarInfo{name: :outer_module_var, positions: [{4, 3}], scope_id: scope_id_2},
                 %VarInfo{name: :top_level_var, positions: [{1, 1}], scope_id: scope_id_1}
               ]
               when scope_id_2 > scope_id_1,
               get_line_vars(state, 15)
             )

      assert Map.keys(state.lines_to_env[17].versioned_vars) == [{:top_level_var, nil}]

      assert [
               %VarInfo{name: :top_level_var, positions: [{1, 1}]}
             ] = get_line_vars(state, 17)
    end

    test "vars as a struct type" do
      state =
        """
        defmodule MyModule do
          def func(%my_var{}, %_my_other{}, %_{}, x) do
            %abc{} = x
            IO.puts ""
          end
        end
        """
        |> string_to_state

      assert Map.keys(state.lines_to_env[4].versioned_vars) == [
               {:_my_other, nil},
               {:abc, nil},
               {:my_var, nil},
               {:x, nil}
             ]

      assert [
               %VarInfo{
                 name: :_my_other,
                 positions: [{2, 24}],
                 scope_id: scope_id_1
               },
               %VarInfo{
                 name: :abc,
                 positions: [{3, 6}],
                 scope_id: scope_id_1
               },
               %VarInfo{
                 name: :my_var,
                 positions: [{2, 13}],
                 scope_id: scope_id_1
               },
               %VarInfo{
                 name: :x,
                 positions: [{2, 43}, {3, 14}],
                 scope_id: scope_id_1
               }
             ] = state |> get_line_vars(4)
    end
  end

  describe "infer vars type information from guards" do
    defp var_with_guards(guard) do
      """
      defmodule MyModule do
        def func(x) when #{guard} do
          IO.puts ""
        end
      end
      """
      |> string_to_state()
      |> get_line_vars(3)
      |> hd()
    end

    test "guards in case clauses" do
      buffer = """
      defmodule MyModule do
        def func(x) do
          IO.puts ""
          case x do
            {a, b} when is_nil(a) and is_integer(b) ->
              IO.puts ""
            _ when is_integer(x) ->
              IO.puts ""
          end
          IO.puts ""
        end
      end
      """

      state = string_to_state(buffer)

      assert [%VarInfo{name: :x, type: nil}] = get_line_vars(state, 3)

      assert [
               %VarInfo{
                 name: :a,
                 type: {:intersection, [{:atom, nil}, {:tuple_nth, {:variable, :x, 0}, 0}]}
               },
               %VarInfo{
                 name: :b,
                 type: {:intersection, [:number, {:tuple_nth, {:variable, :x, 0}, 1}]}
               },
               %VarInfo{name: :x, type: nil}
             ] = get_line_vars(state, 6)

      assert [%VarInfo{name: :x, type: :number}] = get_line_vars(state, 8)

      assert [%VarInfo{name: :x, type: nil}] =
               get_line_vars(state, 10)
    end

    test "guards in case clauses more complicated" do
      buffer = """
      defmodule MyModule do
        def func(x) do
          IO.puts ""
          case {x, :foo} do
            {a, ^x} when is_nil(a) ->
              IO.puts ""
            some_macro(c) ->
              IO.puts ""
            _ when is_integer(x) ->
              IO.puts ""
          end
          IO.puts ""
        end
      end
      """

      state = string_to_state(buffer)

      assert [%VarInfo{name: :x, type: nil}] = get_line_vars(state, 3)

      assert [
               %VarInfo{
                 name: :a,
                 type: {
                   :intersection,
                   [
                     {:atom, nil},
                     {
                       :tuple_nth,
                       {:tuple, 2, [{:variable, :x, 0}, {:atom, :foo}]},
                       0
                     }
                   ]
                 }
               },
               %VarInfo{name: :x, type: nil}
             ] = get_line_vars(state, 6)

      assert [%VarInfo{name: :c, type: nil}, %VarInfo{name: :x, type: nil}] =
               get_line_vars(state, 8)

      assert [%VarInfo{name: :x, type: :number}] = get_line_vars(state, 10)

      assert [%VarInfo{name: :x, type: nil}] = get_line_vars(state, 12)
    end

    test "guards in with clauses" do
      buffer = """
      defmodule MyModule do
        def func(x) do
          IO.puts ""
          with {a, b} when is_nil(a) and is_integer(b) <- x do
            IO.puts ""
          else
            {:error, e} when is_atom(e) ->
              IO.puts ""
            _ when is_integer(x) ->
              IO.puts ""
          end
          IO.puts ""
        end
      end
      """

      state = string_to_state(buffer)

      assert [%VarInfo{name: :x, type: nil}] = get_line_vars(state, 3)

      assert [
               %VarInfo{
                 name: :a,
                 type: {:intersection, [{:atom, nil}, {:tuple_nth, {:variable, :x, 0}, 0}]}
               },
               %VarInfo{
                 name: :b,
                 type: {:intersection, [:number, {:tuple_nth, {:variable, :x, 0}, 1}]}
               },
               %VarInfo{name: :x, type: nil}
             ] = get_line_vars(state, 5)

      assert [%VarInfo{name: :e, type: :atom}, %VarInfo{name: :x, type: nil}] =
               get_line_vars(state, 8)

      assert [%VarInfo{name: :x, type: :number}] = get_line_vars(state, 10)

      assert [%VarInfo{name: :x, type: nil}] = get_line_vars(state, 12)
    end

    test "guards in receive clauses" do
      buffer = """
      defmodule MyModule do
        def func(x) do
          IO.puts ""
          receive do
            {a, b} when is_nil(a) and is_integer(b) ->
              IO.puts ""
            _ when is_integer(x) ->
              IO.puts ""
          end
          IO.puts ""
        end
      end
      """

      state = string_to_state(buffer)

      assert [%VarInfo{name: :x, type: nil}] = get_line_vars(state, 3)

      assert [
               %VarInfo{name: :a, type: {:atom, nil}},
               %VarInfo{name: :b, type: :number},
               %VarInfo{name: :x, type: nil}
             ] = get_line_vars(state, 6)

      assert [%VarInfo{name: :x, type: :number}] = get_line_vars(state, 8)

      assert [%VarInfo{name: :x, type: nil}] = get_line_vars(state, 10)
    end

    test "guards in for generator clauses" do
      buffer = """
      defmodule MyModule do
        def func(x) do
          IO.puts ""
          for {a, b} when is_nil(a) and is_integer(b) <- x, y when is_integer(x) <- a do
            IO.puts ""
          end
          IO.puts ""
        end
      end
      """

      state = string_to_state(buffer)

      assert [%VarInfo{name: :x, type: nil}] = get_line_vars(state, 3)

      assert [
               %VarInfo{
                 name: :a,
                 type:
                   {:intersection,
                    [{:atom, nil}, {:tuple_nth, {:for_expression, {:variable, :x, 0}}, 0}]}
               },
               %VarInfo{
                 name: :b,
                 type:
                   {:intersection,
                    [:number, {:tuple_nth, {:for_expression, {:variable, :x, 0}}, 1}]}
               },
               %VarInfo{name: :x, type: :number},
               %VarInfo{name: :y, type: {:for_expression, {:variable, :a, 1}}}
             ] = get_line_vars(state, 5)

      assert [%VarInfo{name: :x, type: nil}] = get_line_vars(state, 7)
    end

    test "guards in for aggregate clauses" do
      buffer = """
      defmodule MyModule do
        def func(x) do
          IO.puts ""
          for a <- x, reduce: %{} do
            b when is_integer(b) ->
              IO.puts ""
            c when is_atom(c) ->
              IO.puts ""
            _ when is_integer(x) ->
              IO.puts ""
          end
          IO.puts ""
        end
      end
      """

      state = string_to_state(buffer)

      assert [%VarInfo{name: :x, type: nil}] = get_line_vars(state, 3)

      assert [
               %VarInfo{name: :a, type: {:for_expression, {:variable, :x, 0}}},
               %VarInfo{name: :b, type: :number},
               %VarInfo{name: :x, type: nil}
             ] = get_line_vars(state, 6)

      assert [
               %VarInfo{name: :a, type: {:for_expression, {:variable, :x, 0}}},
               %VarInfo{name: :c, type: :atom},
               %VarInfo{name: :x, type: nil}
             ] = get_line_vars(state, 8)

      assert [
               %VarInfo{name: :a, type: {:for_expression, {:variable, :x, 0}}},
               %VarInfo{name: :x, type: :number}
             ] = get_line_vars(state, 10)

      assert [%VarInfo{name: :x, type: nil}] = get_line_vars(state, 12)
    end

    test "guards in try clauses" do
      buffer = """
      defmodule MyModule do
        def func(x) do
          IO.puts ""
          try do
            foo()
          catch
            a, b when is_nil(a) and is_integer(b) ->
              IO.puts ""
          else
            c when is_nil(c) when is_binary(c) ->
              IO.puts ""
            _ when is_integer(x) ->
              IO.puts ""
          end
          IO.puts ""
        end
      end
      """

      state = string_to_state(buffer)

      assert [%VarInfo{name: :x, type: nil}] = get_line_vars(state, 3)

      assert [
               %VarInfo{name: :a, type: {:atom, nil}},
               %VarInfo{name: :b, type: :number},
               %VarInfo{name: :x, type: nil}
             ] = get_line_vars(state, 8)

      assert [
               %VarInfo{name: :c, type: {:union, [{:atom, nil}, :binary]}},
               %VarInfo{name: :x, type: nil}
             ] = get_line_vars(state, 11)

      assert [%VarInfo{name: :x, type: :number}] = get_line_vars(state, 13)

      assert [%VarInfo{name: :x, type: nil}] = get_line_vars(state, 15)
    end

    test "guards in fn clauses" do
      buffer = """
      defmodule MyModule do
        def func(x) do
          IO.puts ""
          fn
            a, b when is_nil(a) and is_integer(b) ->
              IO.puts ""
            c, _ when is_nil(c) when is_binary(c) ->
              IO.puts ""
            _, _ when is_integer(x) ->
              IO.puts ""
          end
          IO.puts ""
        end
      end
      """

      state = string_to_state(buffer)

      assert [%VarInfo{name: :x, type: nil}] = get_line_vars(state, 3)

      assert [
               %VarInfo{name: :a, type: {:atom, nil}},
               %VarInfo{name: :b, type: :number},
               %VarInfo{name: :x, type: nil}
             ] = get_line_vars(state, 6)

      assert [
               %VarInfo{name: :c, type: {:union, [{:atom, nil}, :binary]}},
               %VarInfo{name: :x, type: nil}
             ] = get_line_vars(state, 8)

      assert [%VarInfo{name: :x, type: :number}] = get_line_vars(state, 10)
      assert [%VarInfo{name: :x, type: nil}] = get_line_vars(state, 12)
    end

    test "number guards" do
      assert %VarInfo{name: :x, type: :number} = var_with_guards("is_number(x)")
      assert %VarInfo{name: :x, type: :number} = var_with_guards("is_float(x)")
      assert %VarInfo{name: :x, type: :number} = var_with_guards("is_integer(x)")
      assert %VarInfo{name: :x, type: :number} = var_with_guards("round(x)")
      assert %VarInfo{name: :x, type: :number} = var_with_guards("trunc(x)")
      assert %VarInfo{name: :x, type: :number} = var_with_guards("div(x, 1)")
      assert %VarInfo{name: :x, type: :number} = var_with_guards("rem(x, 1)")
      assert %VarInfo{name: :x, type: :number} = var_with_guards("abs(x)")
      assert %VarInfo{name: :x, type: :number} = var_with_guards("ceil(x)")
      assert %VarInfo{name: :x, type: :number} = var_with_guards("floor(x)")
    end

    test "binary guards" do
      assert %VarInfo{name: :x, type: :binary} = var_with_guards("is_binary(x)")

      assert %VarInfo{name: :x, type: :binary} =
               var_with_guards(~s/binary_part(x, 0, 1) == "a"/)
    end

    test "bitstring guards" do
      assert %VarInfo{name: :x, type: :bitstring} = var_with_guards("is_bitstring(x)")
      assert %VarInfo{name: :x, type: :bitstring} = var_with_guards("bit_size(x) == 1")
      assert %VarInfo{name: :x, type: :bitstring} = var_with_guards("byte_size(x) == 1")
    end

    test "multiple guards" do
      assert %VarInfo{name: :x, type: {:union, [:bitstring, :number]}} =
               var_with_guards("is_bitstring(x) when is_integer(x)")
    end

    test "list guards" do
      assert %VarInfo{name: :x, type: :list} = var_with_guards("is_list(x)")
      assert %VarInfo{name: :x, type: {:list, {:integer, 1}}} = var_with_guards("hd(x) == 1")
      assert %VarInfo{name: :x, type: {:list, {:integer, 1}}} = var_with_guards("1 == hd(x)")
      assert %VarInfo{name: :x, type: :list} = var_with_guards("tl(x) == [1]")
      assert %VarInfo{name: :x, type: :list} = var_with_guards("length(x) == 1")
      assert %VarInfo{name: :x, type: :list} = var_with_guards("1 == length(x)")
      assert %VarInfo{name: :x, type: {:list, :boolean}} = var_with_guards("hd(x)")
    end

    test "tuple guards" do
      assert %VarInfo{name: :x, type: :tuple} = var_with_guards("is_tuple(x)")

      assert %VarInfo{name: :x, type: {:tuple, 1, [nil]}} =
               var_with_guards("tuple_size(x) == 1")

      assert %VarInfo{name: :x, type: {:tuple, 1, [nil]}} =
               var_with_guards("1 == tuple_size(x)")

      assert %VarInfo{name: :x, type: :tuple} = var_with_guards("elem(x, 0) == 1")
    end

    test "atom guards" do
      assert %VarInfo{name: :x, type: :atom} = var_with_guards("is_atom(x)")
    end

    test "boolean guards" do
      assert %VarInfo{name: :x, type: :boolean} = var_with_guards("is_boolean(x)")
    end

    test "map guards" do
      assert %VarInfo{name: :x, type: {:map, [], nil}} = var_with_guards("is_map(x)")

      if Version.match?(System.version(), ">= 1.17.0") do
        assert %VarInfo{name: :x, type: {:map, [], nil}} =
                 var_with_guards("is_non_struct_map(x)")
      end

      assert %VarInfo{name: :x, type: {:map, [], nil}} = var_with_guards("map_size(x) == 1")
      assert %VarInfo{name: :x, type: {:map, [], nil}} = var_with_guards("1 == map_size(x)")

      assert %VarInfo{name: :x, type: {:map, [a: nil], nil}} =
               var_with_guards("is_map_key(x, :a)")

      assert %VarInfo{name: :x, type: {:map, [{"a", nil}], nil}} =
               var_with_guards(~s/is_map_key(x, "a")/)
    end

    test "struct guards" do
      assert %VarInfo{
               name: :x,
               type: {
                 :intersection,
                 [
                   {:struct, [], nil, nil},
                   {:map, [], nil}
                 ]
               }
             } = var_with_guards("is_struct(x)")

      assert %VarInfo{
               name: :x,
               type: {
                 :intersection,
                 [
                   {:struct, [], {:atom, URI}, nil},
                   {:map, [], nil},
                   {:struct, [], nil, nil}
                 ]
               }
             } =
               var_with_guards("is_struct(x, URI)")

      assert %VarInfo{
               name: :x,
               type: {
                 :intersection,
                 [
                   {:struct, [], {:atom, URI}, nil},
                   {:map, [], nil},
                   {:struct, [], nil, nil}
                 ]
               }
             } =
               """
               defmodule MyModule do
                 alias URI, as: MyURI

                 def func(x) when is_struct(x, MyURI) do
                   IO.puts ""
                 end
               end
               """
               |> string_to_state()
               |> get_line_vars(5)
               |> hd()
    end

    test "exception guards" do
      assert %VarInfo{
               name: :x,
               type: {
                 :intersection,
                 [
                   {:map, [{:__exception__, {:atom, true}}], nil},
                   {:map, [{:__exception__, nil}], nil},
                   {:struct, [], nil, nil},
                   {:map, [], nil}
                 ]
               }
             } = var_with_guards("is_exception(x)")

      assert %VarInfo{
               name: :x,
               type: {
                 :intersection,
                 [
                   {:map, [{:__exception__, {:atom, true}}], nil},
                   {:map, [{:__exception__, nil}], nil},
                   {:struct, [], {:atom, ArgumentError}, nil},
                   {:map, [], nil},
                   {:struct, [], nil, nil}
                 ]
               }
             } =
               var_with_guards("is_exception(x, ArgumentError)")

      assert %VarInfo{
               name: :x,
               type: {
                 :intersection,
                 [
                   {:struct, [], {:atom, ArgumentError}, nil},
                   {:map, [], nil},
                   {:struct, [], nil, nil}
                 ]
               }
             } =
               """
               defmodule MyModule do
                 alias ArgumentError, as: MyURI

                 def func(x) when is_struct(x, MyURI) do
                   IO.puts ""
                 end
               end
               """
               |> string_to_state()
               |> get_line_vars(5)
               |> hd()
    end

    test "and combination predicate guards can be merged" do
      assert %VarInfo{name: :x, type: :number} =
               var_with_guards("is_number(x) and x >= 1")

      assert %VarInfo{
               name: :x,
               type: {:intersection, [{:map, [a: nil], nil}, {:map, [b: nil], nil}]}
             } = var_with_guards("is_map_key(x, :a) and is_map_key(x, :b)")
    end

    test "or combination predicate guards can be merge into union type" do
      assert %VarInfo{name: :x, type: {:union, [:number, :atom]}} =
               var_with_guards("is_number(x) or is_atom(x)")

      assert %VarInfo{name: :x, type: {:union, [:number, :atom, :binary]}} =
               var_with_guards("is_number(x) or is_atom(x) or is_binary(x)")
    end

    test "negated guards cannot be used for inference" do
      assert %VarInfo{name: :x, type: nil} =
               var_with_guards("not is_map(x)")

      assert %VarInfo{name: :x, type: nil} =
               var_with_guards("not is_map(x) or is_atom(x)")

      assert %VarInfo{name: :x, type: :atom} =
               var_with_guards("not is_map(x) and is_atom(x)")
    end
  end

  describe "alias" do
    test "aliases" do
      state =
        """
        defmodule OuterModule do
          alias List, as: MyList
          def a1, do: IO.puts "OuterModule " <> inspect(__ENV__.aliases)
          defmodule InnerModule do
            alias Enum, as: MyEnum
            def a1, do: IO.puts "InnerModule " <> inspect(__ENV__.aliases)
            def func do
              alias String, as: MyString
              IO.puts ""
              if true do
                alias Macro, as: MyMacro
                IO.puts ""
              end
              IO.puts ""
            end
            IO.puts ""
          end
          def a2, do: IO.puts "OuterModule " <> inspect(__ENV__.aliases)
          alias Code, as: MyCode
          def a3, do: IO.puts "OuterModule " <> inspect(__ENV__.aliases)
          defmodule AnotherInnerModule do
            def a1, do: IO.puts "AnotherInnerModule " <> inspect(__ENV__.aliases)
          end
          def a4, do: IO.puts "OuterModule " <> inspect(__ENV__.aliases)
          defmodule SomeInnerModule.Nested do
            def a1, do: IO.puts "SomeInnerModule.Nested " <> inspect(__ENV__.aliases)
          end
          def a5, do: IO.puts "OuterModule " <> inspect(__ENV__.aliases)
        end
        IO.puts ""
        """
        |> string_to_state

      assert get_line_aliases(state, 3) == [{MyList, List}]

      assert get_line_aliases(state, 6) == [
               {MyList, List},
               {InnerModule, OuterModule.InnerModule},
               {MyEnum, Enum}
             ]

      assert get_line_aliases(state, 9) == [
               {MyList, List},
               {InnerModule, OuterModule.InnerModule},
               {MyEnum, Enum},
               {MyString, String}
             ]

      assert get_line_aliases(state, 12) == [
               {MyList, List},
               {InnerModule, OuterModule.InnerModule},
               {MyEnum, Enum},
               {MyString, String},
               {MyMacro, Macro}
             ]

      assert get_line_aliases(state, 14) == [
               {MyList, List},
               {InnerModule, OuterModule.InnerModule},
               {MyEnum, Enum},
               {MyString, String}
             ]

      assert get_line_aliases(state, 16) == [
               {MyList, List},
               {InnerModule, OuterModule.InnerModule},
               {MyEnum, Enum}
             ]

      # submodule defines an implicit alias in parent module
      assert get_line_aliases(state, 18) == [
               {MyList, List},
               {InnerModule, OuterModule.InnerModule}
             ]

      assert get_line_aliases(state, 20) == [
               {MyList, List},
               {InnerModule, OuterModule.InnerModule},
               {MyCode, Code}
             ]

      assert get_line_aliases(state, 22) == [
               {MyList, List},
               {InnerModule, OuterModule.InnerModule},
               {MyCode, Code},
               {AnotherInnerModule, OuterModule.AnotherInnerModule}
             ]

      assert get_line_aliases(state, 24) == [
               {MyList, List},
               {InnerModule, OuterModule.InnerModule},
               {MyCode, Code},
               {AnotherInnerModule, OuterModule.AnotherInnerModule}
             ]

      # submodule aliases are inherited to sibling submodules
      assert get_line_aliases(state, 26) == [
               {MyList, List},
               {InnerModule, OuterModule.InnerModule},
               {MyCode, Code},
               {AnotherInnerModule, OuterModule.AnotherInnerModule},
               {SomeInnerModule, OuterModule.SomeInnerModule}
             ]

      # submodule Sub0.Sub1.Sub2 is equivalent to alias Sub0
      assert get_line_aliases(state, 28) == [
               {MyList, List},
               {InnerModule, OuterModule.InnerModule},
               {MyCode, Code},
               {AnotherInnerModule, OuterModule.AnotherInnerModule},
               {SomeInnerModule, OuterModule.SomeInnerModule}
             ]

      assert get_line_aliases(state, 30) == []
    end

    test "nested module alias" do
      state =
        """
        defmodule Parent.Nested do
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 2) == []
    end

    test "submodule alias" do
      state =
        """
        defmodule Parent do
          alias OtherParent.Child
          IO.puts ""
          defmodule Some do
            IO.puts ""
          end
          IO.puts ""
          defmodule Other.One do
            IO.puts ""
          end
          IO.puts ""
          defprotocol MyProt do
            def a(t)
          end
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 3) == [{Child, OtherParent.Child}]
      assert get_line_aliases(state, 5) == [{Child, OtherParent.Child}, {Some, Parent.Some}]
      assert get_line_aliases(state, 7) == [{Child, OtherParent.Child}, {Some, Parent.Some}]

      assert get_line_aliases(state, 9) == [
               {Child, OtherParent.Child},
               {Some, Parent.Some},
               {Other, Parent.Other}
             ]

      assert get_line_aliases(state, 11) == [
               {Child, OtherParent.Child},
               {Some, Parent.Some},
               {Other, Parent.Other}
             ]

      assert get_line_aliases(state, 15) == [
               {Child, OtherParent.Child},
               {Some, Parent.Some},
               {Other, Parent.Other},
               {MyProt, Parent.MyProt}
             ]
    end

    test "submodule alias nested parent" do
      state =
        """
        defmodule Parent.Nested do
          alias OtherParent.Child
          IO.puts ""
          defmodule Some do
            def a1, do: IO.puts "Some " <> inspect(__ENV__.aliases)
          end
          def a1, do: IO.puts "Some " <> inspect(__ENV__.aliases)
          defmodule Other.One do
            def a1, do: IO.puts "Other.One " <> inspect(__ENV__.aliases)
          end
          def a2, do: IO.puts "Some " <> inspect(__ENV__.aliases)
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 3) == [{Child, OtherParent.Child}]

      assert get_line_aliases(state, 5) == [
               {Child, OtherParent.Child},
               {Some, Parent.Nested.Some}
             ]

      assert get_line_aliases(state, 7) == [
               {Child, OtherParent.Child},
               {Some, Parent.Nested.Some}
             ]

      assert get_line_aliases(state, 9) == [
               {Child, OtherParent.Child},
               {Some, Parent.Nested.Some},
               {Other, Parent.Nested.Other}
             ]

      assert get_line_aliases(state, 11) == [
               {Child, OtherParent.Child},
               {Some, Parent.Nested.Some},
               {Other, Parent.Nested.Other}
             ]
    end

    # see https://github.com/elixir-lang/elixir/pull/12451#issuecomment-1459604747
    test "submodule overwriting alias" do
      state =
        """
        defmodule Parent do
          alias OtherParent.Child
          def a1, do: IO.puts "Parent " <> inspect(__ENV__.aliases)
          defmodule Child do
            def a2, do: IO.puts "Parent.Child " <> inspect(__ENV__.aliases)
          end
          def a3, do: IO.puts "Parent " <> inspect(__ENV__.aliases)
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 3) == [{Child, OtherParent.Child}]
      assert get_line_aliases(state, 5) == [{Child, Parent.Child}]
      assert get_line_aliases(state, 7) == [{Child, Parent.Child}]
    end

    # see https://github.com/elixir-lang/elixir/pull/12451#issuecomment-1459604747
    test "nested submodule overwriting alias" do
      state =
        """
        defmodule Parent do
          alias OtherParent.Child
          IO.puts ""
          defmodule Child.One do
            IO.puts ""
          end
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 3) == [{Child, OtherParent.Child}]
      assert get_line_aliases(state, 5) == [{Child, Parent.Child}]
      assert get_line_aliases(state, 7) == [{Child, Parent.Child}]
    end

    # see https://github.com/elixir-lang/elixir/pull/12451#issuecomment-1461393633
    test "external submodule overwriting alias" do
      state =
        """
        defmodule Parent do
          alias OtherParent.Child
          def a1, do: IO.puts "Parent " <> inspect(__ENV__.aliases)
          defmodule Elixir.Child do
            def a2, do: IO.puts "Elixir.Child " <> inspect(__ENV__.aliases)
          end
          def a3, do: IO.puts "Parent " <> inspect(__ENV__.aliases)
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 3) == [{Child, OtherParent.Child}]

      if Version.match?(System.version(), "< 1.16.0") do
        assert get_line_aliases(state, 5) == []
        assert get_line_aliases(state, 7) == []
      else
        # on elixir >= 1.16 no unaliasing is happening
        # https://github.com/elixir-lang/elixir/issues/12456
        assert get_line_aliases(state, 5) == [{Child, OtherParent.Child}]
        assert get_line_aliases(state, 7) == [{Child, OtherParent.Child}]
      end
    end

    # see https://github.com/elixir-lang/elixir/pull/12451#issuecomment-1461393633
    test "nested external submodule overwriting alias" do
      state =
        """
        defmodule Parent do
          alias OtherParent.Child
          def a1, do: IO.puts "Parent " <> inspect(__ENV__.aliases)
          defmodule Elixir.Child.One do
            def a2, do: IO.puts "Elixir.Child.One " <> inspect(__ENV__.aliases)
          end
          def a3, do: IO.puts "Parent " <> inspect(__ENV__.aliases)
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 3) == [{Child, OtherParent.Child}]
      assert get_line_aliases(state, 5) == [{Child, OtherParent.Child}]
      assert get_line_aliases(state, 7) == [{Child, OtherParent.Child}]
    end

    test "aliases with `fn`" do
      state =
        """
        defmodule MyModule do
          alias Enum, as: MyEnum
          IO.puts ""
          fn var_on ->
            alias List, as: MyList
            IO.puts ""
          end
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 3) == [{MyEnum, Enum}]
      assert get_line_aliases(state, 6) == [{MyEnum, Enum}, {MyList, List}]
      assert get_line_aliases(state, 8) == [{MyEnum, Enum}]
    end

    test "aliases defined with multi alias notation" do
      state =
        """
        defmodule MyModule do
          alias Foo.{User, Email, Elixir.Test}
          alias Elixir.User.{Data.Other, Address}
          alias Elixir.{String.Stream}
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 5) == [
               {User, Foo.User},
               {Email, Foo.Email},
               {Test, Foo.Elixir.Test},
               {Other, User.Data.Other},
               {Address, User.Address},
               {Stream, String.Stream}
             ]
    end

    test "aliases defined with multi alias notation __MODULE__" do
      state =
        """
        defmodule MyModule do
          alias __MODULE__.{User, Email}
          alias __MODULE__.Sub.{A, B.C}
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 4) == [
               {User, MyModule.User},
               {Email, MyModule.Email},
               {A, MyModule.Sub.A},
               {C, MyModule.Sub.B.C}
             ]
    end

    test "aliases with __MODULE__" do
      state =
        """
        defmodule MyModule do
          alias __MODULE__.Sub
          alias __MODULE__
          alias __MODULE__.A.B, as: C
          alias __MODULE__, as: Some
          alias Some.Private
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 7) == [
               {Sub, MyModule.Sub},
               {C, MyModule.A.B},
               {Some, MyModule},
               {Private, MyModule.Private}
             ]
    end

    test "aliases with __MODULE__ when module has parts" do
      state =
        """
        defmodule MyModule.Sub do
          alias __MODULE__
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 3) == [
               {Sub, MyModule.Sub}
             ]
    end

    test "aliases of aliases" do
      ElixirSense.Core.Source.split_module_and_func("Elixir.Keyword", CurrentMod, [
        {Keyword, My.Mod}
      ])

      state =
        """
        defmodule MyModule do
          alias Foo.Bar, as: Fb
          alias Fb.Sub, as: S
          alias Elixir.Fb.Oth
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 5) == [{Fb, Foo.Bar}, {S, Foo.Bar.Sub}, {Oth, Fb.Oth}]
    end

    test "aliases erlang module" do
      state =
        """
        defmodule MyModule do
          alias :ets, as: Ets
          alias :erlang_module
          IO.puts ""
        end
        """
        |> string_to_state

      # alias :erlang_module is a compile error since 1.14
      assert get_line_aliases(state, 4) == [{Ets, :ets}]
    end

    test "aliases atom module" do
      state =
        """
        defmodule MyModule do
          alias :"Elixir.A.B"
          alias :"Elixir.A.C", as: S
          alias :"Elixir.A.D", as: :"Elixir.X"
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 5) == [{B, A.B}, {S, A.C}, {X, A.D}]
    end

    test "aliases defined with multi alias notation (multiline)" do
      state =
        """
        defmodule A do
          alias A.{
            B
          }
          IO.puts("")
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 5) == [{B, A.B}]
    end

    test "aliases defined with multi alias notation nested" do
      state =
        """
        defmodule A do
          alias Components.{Dialog, Dialog.Footer, Button, :"Elixir.Other"}
          alias Some.{}
          def a1, do: IO.puts "Parent " <> inspect(__ENV__.aliases)
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 4) == [
               {Dialog, Components.Dialog},
               {Footer, Components.Dialog.Footer},
               {Button, Components.Button},
               {Other, Components.Other}
             ]
    end

    test "aliases defined with multi alias notation with atom module" do
      state =
        """
        defmodule A do
          alias :"Elixir.Components".{Dialog, Dialog.Footer, Button, :"Elixir.Other"}
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 3) == [
               {Dialog, Components.Dialog},
               {Footer, Components.Dialog.Footer},
               {Button, Components.Button},
               {Other, Components.Other}
             ]
    end

    test "aliases without options" do
      state =
        """
        defmodule MyModule do
          alias Foo.User
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 3) == [{User, Foo.User}]
    end

    test "aliases single level without options" do
      state =
        """
        defmodule MyModule do
          alias Foo
          alias :erlang_module
          IO.puts ""
          alias Enum, as: Foo
          alias Elixir.Foo
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 4) == []
      assert get_line_aliases(state, 7) == []
    end

    test "aliases duplicated" do
      state =
        """
        defmodule MyModule do
          alias Foo.User
          alias Foo.User
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 4) == [{User, Foo.User}]
    end

    test "unalias" do
      state =
        """
        defmodule MyModule do
          alias Foo.User
          IO.puts ""
          alias Elixir.User
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_aliases(state, 3) == [{User, Foo.User}]
      assert get_line_aliases(state, 5) == []
    end

    defmodule Macro.AliasTest.Definer do
      defmacro __using__(_options) do
        quote do
          @before_compile unquote(__MODULE__)
        end
      end

      defmacro __before_compile__(_env) do
        quote do
          defmodule First do
            defstruct foo: :bar
          end

          defmodule Second do
            defstruct baz: %First{}
          end
        end
      end
    end

    defmodule Macro.AliasTest.Aliaser do
      defmacro __using__(_options) do
        quote do
          alias Some.First
        end
      end
    end

    test "macro alias does not leak outside macro" do
      state =
        """
        defmodule MyModule do
          use ElixirSense.Core.MetadataBuilderTest.Macro.AliasTest.Definer
          use ElixirSense.Core.MetadataBuilderTest.Macro.AliasTest.Aliaser
          IO.puts ""
        end
        """
        |> string_to_state

      assert [{First, {_, Some.First}}] = state.lines_to_env[4].macro_aliases

      assert %{
               MyModule.First => %StructInfo{
                 fields: [foo: :bar, __struct__: MyModule.First]
               },
               MyModule.Second => %StructInfo{
                 fields: [
                   baz:
                     {:%, [{:line, 1}], [MyModule.First, {:%{}, [{:line, 1}], [{:foo, :bar}]}]},
                   __struct__: MyModule.Second
                 ]
               }
             } = state.structs
    end
  end

  describe "import" do
    test "import with options" do
      state =
        """
        defmodule MyModule do
          import Enum, only: [{:at, 2}]
          import Elixir.{List}, only: []
          import :lists, only: []
          IO.puts ""
        end
        """
        |> string_to_state

      {functions, macros} = get_line_imports(state, 5)
      assert Keyword.keys(functions) == [Enum, Kernel]
      assert Keyword.keys(macros) == [Kernel]
    end

    test "imports defined with multi import notation" do
      state =
        """
        defmodule MyModule do
          import String.{Chars}
          import Code.{}
          IO.puts ""
        end
        """
        |> string_to_state

      {functions, _} = get_line_imports(state, 4)
      assert Keyword.keys(functions) == [String.Chars, Kernel]
    end

    test "imports defined with multi import notation with atom module" do
      state =
        """
        defmodule MyModule do
          import :"Elixir.Foo.Bar".{User, Email, :"Elixir.Other"}
          import Bar.{}
          IO.puts ""
        end
        """
        |> string_to_state

      {functions, _macros} = get_line_imports(state, 4)
      assert Keyword.keys(functions) == [Kernel]
    end

    test "imports" do
      state =
        """
        defmodule OuterModule do
          import List
          IO.puts ""
          defmodule InnerModule do
            import Enum.List
            IO.puts ""
            def func do
              import String
              IO.puts ""
              if true do
                import Macro
                IO.puts ""
              end
              IO.puts ""
            end
            IO.puts ""
          end
          import Code
          IO.puts ""
        end
        IO.puts ""
        """
        |> string_to_state

      {functions, _} = get_line_imports(state, 3)
      assert Keyword.keys(functions) == [List, Kernel]

      # note that `import` causes `require` module's macros available
      assert get_line_requires(state, 3) ==
               [Application, Kernel, Kernel.Typespec, List] |> maybe_reject_typespec

      {functions, _} = get_line_imports(state, 6)
      assert Keyword.keys(functions) == [List, Kernel]
      {functions, _} = get_line_imports(state, 9)
      assert Keyword.keys(functions) == [String, List, Kernel]

      {functions, _} = get_line_imports(state, 12)
      assert Keyword.keys(functions) == [Macro, String, List, Kernel]

      {functions, _} = get_line_imports(state, 14)
      assert Keyword.keys(functions) == [String, List, Kernel]

      {functions, _} = get_line_imports(state, 16)
      assert Keyword.keys(functions) == [List, Kernel]
      {functions, _} = get_line_imports(state, 19)
      assert Keyword.keys(functions) == [Code, List, Kernel]
      {functions, _} = get_line_imports(state, 21)
      assert Keyword.keys(functions) == [Kernel]
    end

    test "imports duplicated" do
      state =
        """
        defmodule OuterModule do
          import List
          import List
          IO.puts ""
        end
        """
        |> string_to_state

      {functions, _macros} = get_line_imports(state, 4)
      assert Keyword.keys(functions) == [List, Kernel]
    end

    test "imports aliased module" do
      state =
        """
        defmodule OuterModule do
          alias Enum, as: S
          import S
          IO.puts ""
        end
        """
        |> string_to_state

      {functions, _} = get_line_imports(state, 4)
      assert Keyword.has_key?(functions, Enum)
    end

    test "imports current buffer module" do
      state =
        """
        defmodule ImportedModule do
          def some_fun(a), do: a
          def _some_fun_underscored(a), do: a
          defp some_fun_priv(a), do: a
          defguard my_guard(x) when x > 0
          defguardp my_guard_priv(x) when x > 0
          defdelegate to_list(map), to: Map
          defmacro some(a, b) do
            quote do: unquote(a) + unquote(b)
          end
          defmacrop some_priv(a, b) do
            quote do: unquote(a) + unquote(b)
          end
          defmacro _some_underscored(a, b) do
            quote do: unquote(a) + unquote(b)
          end
        end

        defmodule OuterModule do
          import ImportedModule
          IO.puts ""
        end
        """
        |> string_to_state

      {functions, macros} = get_line_imports(state, 21)
      assert Keyword.has_key?(functions, ImportedModule)
      assert functions[ImportedModule] == [{:some_fun, 1}, {:to_list, 1}]

      assert Keyword.has_key?(macros, ImportedModule)
      assert macros[ImportedModule] == [{:my_guard, 1}, {:some, 2}]
    end

    test "imports inside protocol" do
      state =
        """
        defprotocol OuterModule do
          IO.puts ""
        end
        """
        |> string_to_state

      {_functions, macros} = get_line_imports(state, 2)
      assert Keyword.keys(macros) == [Protocol, Kernel]
      kernel_macros = Keyword.fetch!(macros, Kernel)
      assert {:def, 1} not in kernel_macros
      assert {:defdelegate, 2} not in kernel_macros
      assert {:def, 1} in Keyword.fetch!(macros, Protocol)
    end
  end

  describe "require" do
    test "requires" do
      state =
        """
        defmodule MyModule do
          require Mod
          IO.puts ""
          defmodule Inner do
            require OtherMod
            IO.puts ""
          end
          IO.puts ""
        end
        IO.puts ""
        """
        |> string_to_state

      assert get_line_requires(state, 3) ==
               [Application, Kernel, Kernel.Typespec, Mod] |> maybe_reject_typespec

      assert get_line_requires(state, 6) ==
               [Application, Kernel, Kernel.Typespec, Mod, OtherMod] |> maybe_reject_typespec

      assert get_line_requires(state, 8) ==
               [Application, Kernel, Kernel.Typespec, Mod] |> maybe_reject_typespec

      assert get_line_requires(state, 10) ==
               [Application, Kernel, Kernel.Typespec] |> maybe_reject_typespec
    end

    test "requires single level" do
      state =
        """
        defmodule MyModule do
          require Mod
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_requires(state, 3) ==
               [Application, Kernel, Kernel.Typespec, Mod] |> maybe_reject_typespec
    end

    test "defmodule emits require with :defined meta" do
      state =
        """
        IO.puts ""
        defmodule Foo.Bar do
          IO.puts ""
          defmodule Some.Mod do
            IO.puts ""
          end
          IO.puts ""
        end
        IO.puts ""
        """
        |> string_to_state

      assert state.lines_to_env[1].context_modules == []
      assert state.lines_to_env[3].context_modules == [Foo.Bar]
      assert state.lines_to_env[5].context_modules == [Foo.Bar.Some.Mod, Foo.Bar]
      assert state.lines_to_env[7].context_modules == [Foo.Bar.Some.Mod, Foo.Bar]
      assert state.lines_to_env[9].context_modules == [Foo.Bar]
      assert state.runtime_modules == []
    end

    test "defmodule emits require with :defined meta - runtime module" do
      state =
        """
        IO.puts ""
        defmodule Foo.Bar do
          IO.puts ""
          def a do
            defmodule Some.Mod do
              IO.puts ""
              def b, do: :ok
            end
            IO.puts ""
            Some.Mod.b()
            IO.puts ""
          end
          IO.puts ""
        end
        IO.puts ""
        """
        |> string_to_state

      assert state.lines_to_env[1].context_modules == []
      assert state.lines_to_env[3].context_modules == [Foo.Bar]
      assert state.lines_to_env[6].context_modules == [Foo.Bar.Some.Mod, Foo.Bar]
      assert state.lines_to_env[9].context_modules == [Foo.Bar.Some.Mod, Foo.Bar]
      assert state.lines_to_env[11].context_modules == [Foo.Bar.Some.Mod, Foo.Bar]
      assert state.lines_to_env[13].context_modules == [Foo.Bar]
      assert state.lines_to_env[15].context_modules == [Foo.Bar]
      assert state.runtime_modules == [Foo.Bar.Some.Mod]

      assert state.lines_to_env[9].aliases == [{Some, Foo.Bar.Some}]
    end

    test "requires local module" do
      state =
        """
        defmodule Mod do
          defmacro some, do: :ok
        end

        defmodule MyModule do
          require Mod
          Mod.some()
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_requires(state, 8) ==
               [Application, Kernel, Kernel.Typespec, Mod] |> maybe_reject_typespec
    end

    test "requires with __MODULE__" do
      state =
        """
        defmodule MyModule do
          require __MODULE__.Sub
          require __MODULE__.A, as: B
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_requires(state, 4) ==
               [
                 Application,
                 Kernel,
                 Kernel.Typespec,
                 MyModule.A,
                 MyModule.Sub
               ]
               |> maybe_reject_typespec

      assert get_line_aliases(state, 4) == [{B, MyModule.A}]
    end

    test "requires with multi require notation" do
      state =
        """
        defmodule MyModule do
          require Mod.{Mo1, Mod2, :"Elixir.Mod3"}
          require Foo.{}
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_requires(state, 4) ==
               [
                 Application,
                 Kernel,
                 Kernel.Typespec,
                 Mod.Mo1,
                 Mod.Mod2,
                 Mod.Mod3
               ]
               |> maybe_reject_typespec
    end

    test "requires with multi require notation with atom module" do
      state =
        """
        defmodule MyModule do
          require :"Elixir.Mod".{Mo1, Mod2, :"Elixir.Mod3"}
          require Foo.{}
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_requires(state, 4) ==
               [
                 Application,
                 Kernel,
                 Kernel.Typespec,
                 Mod.Mo1,
                 Mod.Mod2,
                 Mod.Mod3
               ]
               |> maybe_reject_typespec
    end

    test "requires duplicated" do
      state =
        """
        defmodule MyModule do
          require Mod.Mo1
          require Mod.Mo1
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_requires(state, 4) ==
               [Application, Kernel, Kernel.Typespec, Mod.Mo1] |> maybe_reject_typespec
    end

    test "requires with :as option" do
      state =
        """
        defmodule MyModule do
          require Integer, as: I
          require :ets, as: E
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_requires(state, 4) ==
               [Application, Integer, Kernel, Kernel.Typespec, :ets] |> maybe_reject_typespec

      assert get_line_aliases(state, 4) == [{I, Integer}, {E, :ets}]
    end

    test "requires atom module" do
      state =
        """
        defmodule MyModule do
          require :my_mod
          require :"Elixir.MyMod.Some"
          require :"Elixir.MyMod.Other", as: A
          require :"Elixir.MyMod.Other1", as: :"Elixir.A1"
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_requires(state, 6) ==
               [
                 Application,
                 Kernel,
                 Kernel.Typespec,
                 MyMod.Other,
                 MyMod.Other1,
                 MyMod.Some,
                 :my_mod
               ]
               |> maybe_reject_typespec

      assert get_line_aliases(state, 6) == [{A, MyMod.Other}, {A1, MyMod.Other1}]
    end

    test "requires aliased module" do
      state =
        """
        defmodule MyModule do
          alias Some.Other.Module, as: S
          require S
          require S.Sub
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_requires(state, 5) ==
               [
                 Application,
                 Kernel,
                 Kernel.Typespec,
                 Some.Other.Module,
                 Some.Other.Module.Sub
               ]
               |> maybe_reject_typespec
    end
  end

  describe "current module" do
    test "current module" do
      state =
        """
        IO.puts ""
        defmodule OuterModule do
          IO.puts ""
          defmodule InnerModule do
            def func do
              if true do
                IO.puts ""
              end
            end
          end
          IO.puts ""
        end

        defmodule Some.Nested do
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_module(state, 1) == nil
      assert get_line_protocol(state, 1) == nil
      assert get_line_module(state, 3) == OuterModule
      assert get_line_protocol(state, 3) == nil
      assert get_line_module(state, 7) == OuterModule.InnerModule
      assert get_line_protocol(state, 7) == nil
      assert get_line_module(state, 11) == OuterModule
      assert get_line_protocol(state, 11) == nil

      assert get_line_module(state, 15) == Some.Nested
      assert get_line_protocol(state, 15) == nil
    end

    test "current module with __MODULE__" do
      state =
        """
        IO.puts ""
        defmodule OuterModule do
          IO.puts ""
          defmodule __MODULE__.InnerModule do
            def func do
              if true do
                IO.puts ""
              end
            end
          end
          IO.puts ""
        end

        defmodule Some.Nested do
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_module(state, 1) == nil
      assert get_line_protocol(state, 1) == nil
      assert get_line_module(state, 3) == OuterModule
      assert get_line_protocol(state, 3) == nil
      assert get_line_module(state, 7) == OuterModule.InnerModule
      assert get_line_protocol(state, 7) == nil
      assert get_line_module(state, 11) == OuterModule
      assert get_line_protocol(state, 11) == nil

      assert get_line_module(state, 15) == Some.Nested
      assert get_line_protocol(state, 15) == nil
    end

    test "current module with `Elixir` prefix" do
      state =
        """
        IO.puts ""
        defmodule Elixir.OuterModule do
          IO.puts ""
          defmodule InnerModule do
            IO.puts ""
          end

          defmodule Elixir.ExternalModule do
            IO.puts ""
          end

          defprotocol Elixir.Reversible do
            def reverse(term)
            IO.puts ""
          end
          IO.puts ""
        end

        defmodule Elixir.Some.Nested do
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_module(state, 1) == nil
      assert get_line_module(state, 3) == OuterModule
      assert get_line_module(state, 5) == OuterModule.InnerModule
      assert get_line_module(state, 9) == ExternalModule
      assert get_line_module(state, 14) == Reversible
      assert get_line_module(state, 16) == OuterModule
      # external submodule does not create an alias
      assert get_line_aliases(state, 16) == [{InnerModule, OuterModule.InnerModule}]

      assert get_line_module(state, 20) == Some.Nested
    end

    test "current module with `Elixir` submodule" do
      state =
        """
        IO.puts ""
        defmodule OuterModule do
          IO.puts ""

          defmodule Elixir do
            IO.puts ""
          end

          IO.puts ""
        end

        defmodule Some.Nested do
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_module(state, 1) == nil
      assert get_line_module(state, 3) == OuterModule
      assert get_line_module(state, 6) == OuterModule.Elixir
      assert get_line_module(state, 9) == OuterModule

      assert get_line_module(state, 13) == Some.Nested
    end

    test "current module atom" do
      state =
        """
        IO.puts ""
        defmodule :outer_module do
          IO.puts ""
          defmodule :inner_module do
            def func do
              if true do
                IO.puts ""
              end
            end
          end
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_module(state, 1) == nil
      assert get_line_protocol(state, 1) == nil
      assert get_line_module(state, 3) == :outer_module
      assert get_line_protocol(state, 3) == nil
      assert get_line_module(state, 7) == :inner_module
      assert get_line_protocol(state, 7) == nil
      assert get_line_module(state, 11) == :outer_module
      assert get_line_protocol(state, 11) == nil
    end

    test "current module as atom" do
      state =
        """
        IO.puts ""
        defmodule :"Elixir.OuterModule" do
          IO.puts ""
          defmodule :"Elixir.InnerModule" do
            def func do
              if true do
                IO.puts ""
              end
            end
          end
          IO.puts ""

          defmodule :"Elixir.OuterModule.InnerModule1" do
            def func do
              if true do
                IO.puts ""
              end
            end
          end
        end
        """
        |> string_to_state

      assert get_line_module(state, 1) == nil
      assert get_line_protocol(state, 1) == nil
      assert get_line_module(state, 3) == OuterModule
      assert get_line_protocol(state, 3) == nil
      assert get_line_module(state, 7) == InnerModule
      assert get_line_protocol(state, 7) == nil
      assert get_line_module(state, 11) == OuterModule
      assert get_line_protocol(state, 11) == nil
      assert get_line_module(state, 16) == OuterModule.InnerModule1
      assert get_line_protocol(state, 16) == nil
    end

    test "current module and protocol" do
      state =
        """
        defprotocol My.Reversible do
          def reverse(term)
          IO.puts ""
        end
        """
        |> string_to_state

      # protocol and implementations create modules
      assert get_line_module(state, 3) == My.Reversible
      assert get_line_protocol(state, 3) == nil
    end

    test "current module and protocol implementation - simple case" do
      state =
        """
        defimpl Inspect, for: Atom do
          IO.puts("")
        end
        """
        |> string_to_state

      assert get_line_module(state, 2) == Inspect.Atom
      assert get_line_protocol(state, 2) == {Inspect, [Atom]}
    end

    test "current module and protocol implementation" do
      state =
        """
        defprotocol My.Reversible do
          def reverse(term)
          IO.puts ""
        end

        defimpl My.Reversible, for: String do
          def reverse(term), do: String.reverse(term)
          IO.puts ""
        end

        defimpl My.Reversible, for: [Map, My.List] do
          def reverse(term), do: Enum.reverse(term)
          IO.puts ""

          defmodule OuterModule do
            IO.puts ""
          end

          defprotocol Other do
            def other(term)
            IO.puts ""
          end

          defimpl Other, for: [Map, My.Map] do
            def other(term), do: nil
            IO.inspect(__ENV__.module)
          end
        end
        """
        |> string_to_state

      # protocol and implementations create modules
      assert get_line_module(state, 3) == My.Reversible
      assert get_line_protocol(state, 3) == nil
      assert get_line_module(state, 8) == My.Reversible.String
      assert get_line_protocol(state, 8) == {My.Reversible, [String]}
      assert get_line_module(state, 13) == My.Reversible.My.List
      assert get_line_protocol(state, 13) == {My.Reversible, [Map, My.List]}

      # implementation has behaviour
      assert get_line_behaviours(state, 8) == [My.Reversible]

      # multiple implementations create multiple modules
      assert get_line_module(state, 16) == My.Reversible.My.List.OuterModule

      assert get_line_protocol(state, 16) == nil

      # protocol and implementations inside protocol implementation creates a cross product
      assert get_line_module(state, 21) == My.Reversible.My.List.Other
      assert get_line_protocol(state, 21) == nil

      assert get_line_module(state, 26) == My.Reversible.My.List.Other.My.Map

      assert get_line_protocol(state, 26) == {My.Reversible.My.List.Other, [Map, My.Map]}
    end
  end

  describe "protocol implementation" do
    test "protocol implementation for atom modules" do
      state =
        """
        defprotocol :my_reversible do
          def reverse(term)
          IO.puts ""
        end

        defimpl :my_reversible, for: [String, :my_str, :"Elixir.MyStr"] do
          def reverse(term), do: String.reverse(term)
          IO.puts ""
        end

        defprotocol :"Elixir.My.Reversible" do
          def reverse(term)
          IO.puts ""
        end

        defimpl :"Elixir.My.Reversible", for: [String, :my_str, :"Elixir.MyStr"] do
          def reverse(term), do: String.reverse(term)
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_module(state, 3) == :my_reversible
      assert get_line_protocol(state, 3) == nil

      assert get_line_module(state, 8) == :"Elixir.my_reversible.MyStr"

      assert get_line_protocol(state, 8) == {:my_reversible, [String, :my_str, MyStr]}

      assert get_line_module(state, 13) == My.Reversible
      assert get_line_protocol(state, 13) == nil

      assert get_line_module(state, 18) == My.Reversible.MyStr

      assert get_line_protocol(state, 18) == {My.Reversible, [String, :my_str, MyStr]}
    end

    test "protocol implementation module naming rules" do
      state =
        """
        defprotocol NiceProto do
          def reverse(term)
        end

        defmodule NiceProtoImplementations do
          defimpl NiceProto, for: String do
            def reverse(term), do: String.reverse(term)
            def a3, do: IO.puts "OuterModule " <> inspect(__ENV__.aliases)
          end
          def a3, do: IO.puts "OuterModule " <> inspect(__ENV__.aliases)

          defmodule Some do
            defstruct [a: nil]
          end

          defimpl NiceProto, for: Some do
            def reverse(term), do: String.reverse(term)
            IO.inspect(__ENV__.module)
          end

          alias Enumerable.Date.Range, as: R
          alias NiceProto, as: N

          defimpl N, for: R do
            def reverse(term), do: String.reverse(term)
            IO.puts ""
          end
        end
        """
        |> string_to_state

      # protocol implementation module name does not inherit enclosing module, only protocol
      assert get_line_module(state, 8) == NiceProto.String
      assert get_line_protocol(state, 8) == {NiceProto, [String]}
      assert get_line_aliases(state, 8) == []
      assert get_line_aliases(state, 10) == []

      # properly gets implementation name inherited from enclosing module
      assert get_line_module(state, 18) == NiceProto.NiceProtoImplementations.Some
      assert get_line_protocol(state, 18) == {NiceProto, [NiceProtoImplementations.Some]}

      # aliases are expanded on protocol and implementation
      assert get_line_module(state, 24) == NiceProto.Enumerable.Date.Range
      assert get_line_protocol(state, 24) == {NiceProto, [Enumerable.Date.Range]}
    end

    test "protocol implementation using __MODULE__" do
      state =
        """
        defprotocol NiceProto do
          def reverse(term)
        end

        defmodule MyStruct do
          defstruct [a: nil]

          defimpl NiceProto, for: __MODULE__ do
            def reverse(term), do: String.reverse(term)
          end
        end
        """
        |> string_to_state

      # protocol implementation module name does not inherit enclosing module, only protocol
      assert get_line_module(state, 8) == NiceProto.MyStruct
      assert get_line_protocol(state, 8) == {NiceProto, [MyStruct]}
    end

    test "protocol implementation using __MODULE__ 2" do
      state =
        """
        defmodule Nice do
          defprotocol Proto do
            def reverse(term)
          end

          defimpl __MODULE__.Proto, for: String do
            def reverse(term), do: String.reverse(term)
          end
        end
        """
        |> string_to_state

      assert get_line_module(state, 7) == Nice.Proto.String
      assert get_line_protocol(state, 7) == {Nice.Proto, [String]}
    end

    test "protocol implementation for structs does not require for" do
      state =
        """
        defprotocol Proto do
          def reverse(term)
        end

        defmodule MyStruct do
          defstruct [:field]

          defimpl Proto do
            def reverse(term), do: String.reverse(term)
          end
        end
        """
        |> string_to_state

      assert get_line_module(state, 9) == Proto.MyStruct
      assert get_line_protocol(state, 9) == {Proto, [MyStruct]}
    end

    test "protocol implementation by deriving" do
      state =
        """
        defprotocol Proto do
          def reverse(term)
        end

        defimpl Proto, for: Any do
          def reverse(term), do: term
        end

        defmodule MyStruct do
          @derive Proto
          defstruct [:field]
          IO.puts ""
        end
        IO.puts ""

        defmodule MyOtherStruct do
          @derive [{Proto, opt: 1}, Enumerable]
          defstruct [:field]
        end
        """
        |> string_to_state

      assert %{
               {Enumerable.MyOtherStruct, nil, nil} => %ModFunInfo{
                 params: [nil],
                 type: :defmodule
               },
               {Proto.Any, nil, nil} => %ModFunInfo{
                 params: [nil],
                 type: :defmodule
               },
               {Proto.MyOtherStruct, nil, nil} => %ModFunInfo{
                 params: [nil],
                 type: :defmodule
               },
               {Proto.MyOtherStruct, :reverse, 1} => %ModFunInfo{
                 params: [[{:term, _, nil}]],
                 type: :def
               },
               {Proto.MyStruct, nil, nil} => %ModFunInfo{
                 params: [nil],
                 type: :defmodule
               },
               {Proto.MyStruct, :reverse, 1} => %ModFunInfo{
                 params: [[{:term, _, nil}]],
                 type: :def
               }
             } = state.mods_funs_to_positions
    end
  end

  test "protocol registers callbacks from specs or generate dummy callbacks" do
    state =
      """
      defprotocol Proto do
        @spec with_spec(t, integer) :: String.t
        @spec with_spec(t, boolean) :: number
        def with_spec(t, integer)

        def without_spec(t, integer)
      end
      """
      |> string_to_state

    assert %{
             {Proto, :with_spec, 2} => %ElixirSense.Core.State.SpecInfo{
               args: [["t()", "boolean()"], ["t()", "integer()"]],
               kind: :callback,
               name: :with_spec,
               positions: [{3, 3}, {2, 3}],
               end_positions: [{3, 40}, {2, 42}],
               generated: [false, false],
               specs: [
                 "@callback with_spec(t(), boolean()) :: number()",
                 "@callback with_spec(t(), integer()) :: String.t()",
                 "@spec with_spec(t(), boolean()) :: number()",
                 "@spec with_spec(t(), integer()) :: String.t()"
               ]
             },
             {Proto, :without_spec, 2} => %ElixirSense.Core.State.SpecInfo{
               args: [["t()", "term()"]],
               kind: :callback,
               name: :without_spec,
               positions: [{6, 3}],
               end_positions: [nil],
               generated: [true],
               specs: ["@callback without_spec(t(), term()) :: term()"]
             },
             # there is raw unquote in spec...
             {Proto, :__protocol__, 1} => %ElixirSense.Core.State.SpecInfo{
               kind: :spec
             },
             {Proto, :impl_for, 1} => %ElixirSense.Core.State.SpecInfo{
               kind: :spec,
               specs: ["@spec impl_for(term()) :: atom() | nil"]
             },
             {Proto, :impl_for!, 1} => %ElixirSense.Core.State.SpecInfo{
               kind: :spec,
               specs: ["@spec impl_for!(term()) :: atom()"]
             }
           } = state.specs
  end

  test "registers positions" do
    state =
      """
      IO.puts ""
      defmodule OuterModule do
        IO.puts ""
        defmodule InnerModule do
          def func do
            if true do
              IO.puts ""
            end
          end
        end
        IO.puts ""
      end

      defmodule Some.Nested do
        IO.puts ""
      end
      """
      |> string_to_state

    assert %{
             {OuterModule, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{2, 1}],
               type: :defmodule
             },
             {OuterModule.InnerModule, :func, 0} => %ModFunInfo{
               params: [[]],
               positions: [{5, 5}],
               type: :def
             }
           } = state.mods_funs_to_positions
  end

  test "registers def positions in protocol" do
    state =
      """
      defprotocol Reversible do
        def reverse(term)
        IO.puts ""
      end
      """
      |> string_to_state

    assert %{
             {Reversible, :reverse, 1} => %ModFunInfo{
               params: [[{:term, _, nil}]],
               positions: [{2, 3}],
               type: :def
             }
           } = state.mods_funs_to_positions
  end

  test "registers def positions in protocol implementation" do
    state =
      """
      defprotocol Reversible do
        def reverse(term)
        IO.puts ""
      end

      defimpl Reversible, for: String do
        def reverse(term), do: String.reverse(term)
        IO.puts ""
      end

      defmodule Impls do
        alias Reversible, as: R
        alias My.List, as: Ml
        defimpl R, for: [Map, Ml] do
          def reverse(term), do: Enum.reverse(term)
          IO.puts ""
        end
      end
      """
      |> string_to_state

    assert %{
             {Impls, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{11, 1}],
               type: :defmodule
             },
             {Reversible.String, :__impl__, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 6, column: 1], nil}]],
               positions: [{6, 1}],
               type: :def
             }
           } = state.mods_funs_to_positions
  end

  test "functions head" do
    state =
      """
      defmodule OuterModule do
        def abc(a \\\\ nil)
        def abc(1), do: :ok
        def abc(nil), do: :error
        IO.puts ""
      end
      """
      |> string_to_state

    assert %{
             {OuterModule, :abc, 1} => %ModFunInfo{
               params: [
                 [nil],
                 [1],
                 [
                   {:\\, _, [{:a, _, nil}, nil]}
                 ]
               ]
             }
           } = state.mods_funs_to_positions
  end

  test "functions with default args" do
    state =
      """
      defmodule OuterModule do
        def abc(a, b \\\\ nil, c, d \\\\ [1]), do: a
        IO.puts ""
      end
      """
      |> string_to_state

    assert %{
             {OuterModule, :abc, 4} => %ModFunInfo{
               params: [
                 [
                   {:a, _, nil},
                   {:\\, _, [{:b, _, nil}, nil]},
                   {:c, _, nil},
                   {:\\, _, [{:d, _, nil}, [1]]}
                 ]
               ]
             }
           } = state.mods_funs_to_positions
  end

  describe "behaviour" do
    test "behaviours" do
      state =
        """
        IO.puts ""
        defmodule OuterModule do
          use Application
          @behaviour SomeModule.SomeBehaviour
          IO.puts ""
          defmodule InnerModuleWithUse1 do
            use GenServer
            IO.puts ""
          end
          defmodule InnerModuleWithBh do
            @behaviour SomeOtherBehaviour
            IO.puts ""
          end
          defmodule InnerModuleWithoutBh do
            IO.puts ""
          end
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_behaviours(state, 1) == []
      assert get_line_behaviours(state, 5) == [Application, SomeModule.SomeBehaviour]
      assert get_line_behaviours(state, 8) == [GenServer]
      assert get_line_behaviours(state, 12) == [SomeOtherBehaviour]
      assert get_line_behaviours(state, 15) == []
      assert get_line_behaviours(state, 17) == [Application, SomeModule.SomeBehaviour]
    end

    test "behaviour from erlang module" do
      state =
        """
        defmodule OuterModule do
          @behaviour :gen_server
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_behaviours(state, 3) == [:gen_server]
    end

    test "behaviour from __MODULE__" do
      state =
        """
        defmodule OuterModule do
          @behaviour __MODULE__.Sub
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_behaviours(state, 3) == [OuterModule.Sub]
    end

    test "behaviour from atom module" do
      state =
        """
        defmodule OuterModule do
          @behaviour :"Elixir.My.Behavior"
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_behaviours(state, 3) == [My.Behavior]
    end

    test "behaviour duplicated" do
      state =
        """
        defmodule OuterModule do
          @behaviour :gen_server
          @behaviour :gen_server
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_behaviours(state, 4) == [:gen_server]
    end

    test "behaviour from aliased module" do
      state =
        """
        defmodule OuterModule do
          alias Some.Module, as: S
          @behaviour S
          IO.puts ""
        end
        """
        |> string_to_state

      assert get_line_behaviours(state, 4) == [Some.Module]
    end

    test "defprotocol implements Protocol" do
      state =
        """
        defprotocol Some do
          def foo(t)
          IO.puts ""
        end
        """
        |> string_to_state

      if Version.match?(System.version(), ">= 1.18.0") do
        assert get_line_behaviours(state, 3) == [Protocol]
      else
        assert get_line_behaviours(state, 3) == []
      end
    end
  end

  test "current scope" do
    state =
      """
      defmodule MyModule do
        def func do
          IO.puts ""
        end
        IO.puts ""
        def func_with_when(par) when is_list(par) do
          IO.puts ""
        end
        IO.puts ""
        defmacro macro1(ast) do
          IO.puts ""
        end
        IO.puts ""
        defmacro import(module, opts)
        IO.puts ""
        defdelegate func_delegated(par), to: OtherModule
        IO.puts ""
        defguard is_even(value) when is_integer(value) and rem(value, 2) == 0
        IO.puts ""
        defmodule Nester.Module1 do
          IO.puts ""
        end
      end

      defmodule AnotherNester.Module2 do
        IO.puts ""
      end

      defprotocol Reversible do
        def reverse(term)
        IO.puts ""
      end

      defimpl Reversible, for: [Map, My.List] do
        IO.puts ""
        def reverse(term) do
          IO.puts ""
        end
      end
      """
      |> string_to_state

    assert nil == get_line_typespec(state, 3)
    assert {:func, 0} == get_line_function(state, 3)
    assert MyModule == get_line_module(state, 3)

    assert nil == get_line_typespec(state, 5)
    assert nil == get_line_function(state, 5)
    assert MyModule == get_line_module(state, 5)

    assert nil == get_line_typespec(state, 7)
    assert {:func_with_when, 1} == get_line_function(state, 7)
    assert MyModule == get_line_module(state, 7)

    assert nil == get_line_typespec(state, 9)
    assert nil == get_line_function(state, 9)
    assert MyModule == get_line_module(state, 9)

    assert nil == get_line_typespec(state, 11)
    assert {:macro1, 1} == get_line_function(state, 11)
    assert MyModule == get_line_module(state, 11)

    assert nil == get_line_typespec(state, 13)
    assert nil == get_line_function(state, 13)
    assert MyModule == get_line_module(state, 13)

    assert nil == get_line_typespec(state, 15)
    assert nil == get_line_function(state, 15)
    assert MyModule == get_line_module(state, 15)

    assert nil == get_line_typespec(state, 16)
    assert {:func_delegated, 1} == get_line_function(state, 16)
    assert MyModule == get_line_module(state, 16)

    assert nil == get_line_typespec(state, 18)
    assert {:is_even, 1} == get_line_function(state, 18)
    assert MyModule == get_line_module(state, 18)

    assert nil == get_line_typespec(state, 21)
    assert nil == get_line_function(state, 21)
    assert MyModule.Nester.Module1 == get_line_module(state, 21)

    assert nil == get_line_typespec(state, 26)
    assert nil == get_line_function(state, 26)
    assert AnotherNester.Module2 == get_line_module(state, 26)

    assert nil == get_line_typespec(state, 31)
    assert nil == get_line_function(state, 31)
    assert Reversible == get_line_module(state, 31)

    assert nil == get_line_typespec(state, 35)
    assert nil == get_line_function(state, 35)
    assert Reversible.My.List == get_line_module(state, 35)

    assert nil == get_line_typespec(state, 37)
    assert {:reverse, 1} == get_line_function(state, 37)
    assert Reversible.My.List == get_line_module(state, 37)
  end

  test "finds positions for guards" do
    state =
      """
      defmodule MyModule do
        defguard is_even(value) when is_integer(value) and rem(value, 2) == 0
        defguardp is_odd(value) when is_integer(value) and rem(value, 2) == 1
        defguardp useless when 1 == 1
        IO.puts ""
      end
      """
      |> string_to_state

    assert %{
             {MyModule, :is_even, 1} => %{
               params: [[{:value, _, nil}]],
               positions: [{2, 3}]
             },
             {MyModule, :is_odd, 1} => %{
               params: [[{:value, _, nil}]],
               positions: [{3, 3}]
             },
             {MyModule, :useless, 0} => %{
               params: [[]],
               positions: [{4, 3}]
             }
           } = state.mods_funs_to_positions
  end

  test "registers mods and func" do
    state =
      """
      defmodule MyModuleWithoutFuns do
      end
      defmodule MyModuleWithFuns do
        def func do
          IO.puts ""
        end
        defp funcp do
          IO.puts ""
        end
        defmacro macro1(ast) do
          IO.puts ""
        end
        defmacrop macro1p(ast) do
          IO.puts ""
        end
        defguard is_even(value) when is_integer(value) and rem(value, 2) == 0
        defguardp is_evenp(value) when is_integer(value) and rem(value, 2) == 0

        defmodule Nested do
        end
      end
      """
      |> string_to_state

    assert %{
             {MyModuleWithFuns, :func, 0} => %ModFunInfo{
               params: [[]],
               type: :def
             },
             {MyModuleWithFuns, :funcp, 0} => %ModFunInfo{
               params: [[]],
               type: :defp
             },
             {MyModuleWithFuns, :is_even, 1} => %ModFunInfo{
               params: [[{:value, _, nil}]],
               type: :defguard
             },
             {MyModuleWithFuns, :is_evenp, 1} => %ModFunInfo{
               params: [[{:value, _, nil}]],
               type: :defguardp
             },
             {MyModuleWithFuns, :macro1, 1} => %ModFunInfo{
               params: [[{:ast, _, nil}]],
               type: :defmacro
             },
             {MyModuleWithFuns, :macro1p, 1} => %ModFunInfo{
               params: [[{:ast, _, nil}]],
               type: :defmacrop
             },
             {MyModuleWithFuns, nil, nil} => %ModFunInfo{
               params: [nil],
               type: :defmodule
             },
             {MyModuleWithFuns.Nested, nil, nil} => %ModFunInfo{
               params: [nil],
               type: :defmodule
             },
             {MyModuleWithoutFuns, nil, nil} => %ModFunInfo{
               params: [nil],
               type: :defmodule
             },
             {MyModuleWithFuns, :__info__, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, _, nil}]],
               type: :def
             },
             {MyModuleWithFuns, :module_info, 0} => %ElixirSense.Core.State.ModFunInfo{
               params: [[]],
               type: :def
             },
             {MyModuleWithFuns, :module_info, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, _, nil}]],
               type: :def
             },
             {MyModuleWithFuns.Nested, :__info__, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, _, nil}]],
               type: :def
             },
             {MyModuleWithFuns.Nested, :module_info, 0} => %ElixirSense.Core.State.ModFunInfo{
               params: [[]],
               type: :def
             },
             {MyModuleWithFuns.Nested, :module_info, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, _, nil}]],
               type: :def
             },
             {MyModuleWithoutFuns, :__info__, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, _, nil}]],
               type: :def
             },
             {MyModuleWithoutFuns, :module_info, 0} => %ElixirSense.Core.State.ModFunInfo{
               params: [[]],
               type: :def
             },
             {MyModuleWithoutFuns, :module_info, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, _, nil}]],
               type: :def
             }
           } = state.mods_funs_to_positions
  end

  test "registers delegated func" do
    state =
      """
      defmodule MyModuleWithFuns do
        alias Enum, as: E
        defdelegate func_delegated(par), to: OtherModule
        defdelegate func_delegated_erlang(par), to: :erlang_module
        defdelegate func_delegated_as(par), to: __MODULE__.Sub, as: :my_func
        defdelegate func_delegated_alias(par), to: E
        defdelegate func_delegated_defaults(par \\\\ 123), to: E
      end
      """
      |> string_to_state

    assert %{
             {MyModuleWithFuns, :func_delegated, 1} => %ModFunInfo{
               params: [[{:par, _, nil}]],
               positions: [{3, 3}],
               target: {OtherModule, :func_delegated},
               type: :defdelegate
             },
             {MyModuleWithFuns, :func_delegated_alias, 1} => %ModFunInfo{
               params: [[{:par, _, nil}]],
               positions: [{6, 3}],
               target: {Enum, :func_delegated_alias},
               type: :defdelegate
             },
             {MyModuleWithFuns, :func_delegated_as, 1} => %ModFunInfo{
               params: [[{:par, _, nil}]],
               positions: [{5, 3}],
               target: {MyModuleWithFuns.Sub, :my_func},
               type: :defdelegate
             },
             {MyModuleWithFuns, :func_delegated_erlang, 1} => %ModFunInfo{
               params: [[{:par, _, nil}]],
               positions: [{4, 3}],
               target: {:erlang_module, :func_delegated_erlang},
               type: :defdelegate
             },
             {MyModuleWithFuns, :func_delegated_defaults, 1} => %ModFunInfo{
               params: [[{:\\, _, [{:par, _, nil}, 123]}]],
               positions: [{7, 3}],
               target: {Enum, :func_delegated_defaults},
               type: :defdelegate
             }
           } = state.mods_funs_to_positions
  end

  test "gracefully handles delegated with unquote fragment" do
    state =
      """
      defmodule MyModuleWithFuns do
        dynamic = :dynamic_flatten
        defdelegate unquote(dynamic)(list), to: List, as: :flatten
      end
      """
      |> string_to_state

    assert %{
             {MyModuleWithFuns, :__unknown__, 1} => %ModFunInfo{
               target: {List, :flatten},
               type: :defdelegate
             }
           } = state.mods_funs_to_positions
  end

  test "registers defs with unquote fragments in body" do
    state =
      """
      defmodule MyModuleWithFuns do
        kv = [foo: 1]
        Enum.each(kv, fn {k, v} ->
          def foo(), do: unquote(v)
        end)
      end
      """
      |> string_to_state

    assert %{
             {MyModuleWithFuns, :foo, 0} => %ModFunInfo{
               params: [[]]
             }
           } = state.mods_funs_to_positions
  end

  test "registers unknown for defs with unquote fragments in call" do
    state =
      """
      defmodule MyModuleWithFuns do
        kv = [foo: 1, bar: 2]
        Enum.each(kv, fn {k, v} ->
          def unquote(k)(), do: 123
        end)
      end
      """
      |> string_to_state

    assert Map.keys(state.mods_funs_to_positions) == [
             {MyModuleWithFuns, :__info__, 1},
             {MyModuleWithFuns, :__unknown__, 0},
             {MyModuleWithFuns, :module_info, 0},
             {MyModuleWithFuns, :module_info, 1},
             {MyModuleWithFuns, nil, nil}
           ]
  end

  if Version.match?(System.version(), ">= 1.18.0") do
    test "registers defguard with unquote fragments in when" do
      state =
        """
        defmodule MyModuleWithFuns do
          kv = [foo: 1]
          Enum.each(kv, fn {k, v} ->
            defguard foo(a) when is_integer(unquote(v))
          end)
        end
        """
        |> string_to_state

      assert %{
               {MyModuleWithFuns, :foo, 1} => %ModFunInfo{
                 params: [[{:a, _, _}]]
               }
             } = state.mods_funs_to_positions
    end

    test "registers unknown for defguard with unquote fragments in call" do
      state =
        """
        defmodule MyModuleWithFuns do
          kv = [foo: 1, bar: 2]
          Enum.each(kv, fn {k, v} ->
            defguard unquote(k)(a) when is_integer(a)
          end)
        end
        """
        |> string_to_state

      assert Map.keys(state.mods_funs_to_positions) == [
               {MyModuleWithFuns, :__info__, 1},
               {MyModuleWithFuns, :__unknown__, 1},
               {MyModuleWithFuns, :module_info, 0},
               {MyModuleWithFuns, :module_info, 1},
               {MyModuleWithFuns, nil, nil}
             ]
    end
  end

  test "registers unknown for defdelegate with unquote fragments in call" do
    state =
      """
      defmodule MyModuleWithFuns do
        kv = [foo: 1, bar: 2]
        Enum.each(kv, fn {k, v} ->
          defdelegate unquote(k)(), to: Foo
        end)
      end
      """
      |> string_to_state

    assert Map.keys(state.mods_funs_to_positions) == [
             {MyModuleWithFuns, :__info__, 1},
             {MyModuleWithFuns, :__unknown__, 0},
             {MyModuleWithFuns, :module_info, 0},
             {MyModuleWithFuns, :module_info, 1},
             {MyModuleWithFuns, nil, nil}
           ]
  end

  test "registers builtin functions for protocols" do
    state =
      """
      defprotocol Reversible do
        def reverse(term)
        IO.puts ""
      end
      """
      |> string_to_state

    assert Map.has_key?(state.mods_funs_to_positions, {Reversible, :__protocol__, 1})
    assert Map.has_key?(state.mods_funs_to_positions, {Reversible, :impl_for, 1})
    assert Map.has_key?(state.mods_funs_to_positions, {Reversible, :impl_for!, 1})
    assert Map.has_key?(state.mods_funs_to_positions, {Reversible, :impl_for!, 1})
    assert Map.has_key?(state.mods_funs_to_positions, {Reversible, :__info__, 1})
    assert Map.has_key?(state.mods_funs_to_positions, {Reversible, :module_info, 0})
    assert Map.has_key?(state.mods_funs_to_positions, {Reversible, :module_info, 1})
    assert Map.has_key?(state.mods_funs_to_positions, {Reversible, :behaviour_info, 1})
  end

  test "registers builtin functions for protocol implementations" do
    state =
      """
      defmodule MyModuleWithoutFuns do
      end
      defmodule MyModuleWithFuns do
        def func do
          IO.puts ""
        end
        defp funcp do
          IO.puts ""
        end
        defmacro macro1(ast) do
          IO.puts ""
        end
        defmacrop macro1p(ast) do
          IO.puts ""
        end
        defguard is_even(value) when is_integer(value) and rem(value, 2) == 0
        defguardp is_evenp(value) when is_integer(value) and rem(value, 2) == 0
        defdelegate func_delegated(par), to: OtherModule
        defmodule Nested do
        end
      end

      defprotocol Reversible do
        def reverse(term)
        IO.puts ""
      end

      defimpl Reversible, for: String do
        def reverse(term), do: String.reverse(term)
        IO.puts ""
      end

      defmodule Impls do
        alias Reversible, as: R
        alias My.List, as: Ml
        defimpl R, for: [Ml, Map] do
          def reverse(term), do: Enum.reverse(term)
          IO.puts ""
        end
      end
      """
      |> string_to_state

    assert Map.has_key?(state.mods_funs_to_positions, {Impls, :__info__, 1})
    assert Map.has_key?(state.mods_funs_to_positions, {Reversible.My.List, :__impl__, 1})
  end

  describe "macro expansion" do
    defmodule WithMacros do
      IO.inspect(__ENV__.module)

      defmacro go do
        quote do
          def my_fun, do: :ok
        end
      end
    end

    test "expands remote macro" do
      state =
        """
        defmodule SomeMod do
          require ElixirSense.Core.MetadataBuilderTest.WithMacros, as: WithMacros
          WithMacros.go()
        end
        """
        |> string_to_state

      assert %{{SomeMod, :my_fun, 0} => _} = state.mods_funs_to_positions
    end

    test "expands remote imported macro" do
      state =
        """
        defmodule SomeMod do
          import ElixirSense.Core.MetadataBuilderTest.WithMacros
          go()
        end
        """
        |> string_to_state

      assert %{{SomeMod, :my_fun, 0} => _} = state.mods_funs_to_positions
    end

    defmodule SomeCompiledMod do
      defmacro go do
        quote do
          self()
          Node.list()
        end
      end

      defmacrop go_priv do
        quote do
          self()
        end
      end
    end

    test "expands public local macro from compiled module" do
      # NOTE we optimistically assume the previously compiled module version has
      # the same macro implementation as the currently expanded
      state =
        """
        defmodule ElixirSense.Core.MetadataBuilderTest.SomeCompiledMod do
          defmacro go do
            quote do
              self(); Node.list()
            end
          end

          defmacrop go_priv do
            quote do
              self()
            end
          end

          def foo do
            go()
            go_priv()
          end
        end
        """
        |> string_to_state

      # NOTE remote call is abstract so no event, import call is imprecise
      assert [%CallInfo{func: :self, kind: :imported_quoted}] = state.calls[4]

      # import in quoted expression emits remote function call on expansion
      assert [
               %CallInfo{func: :go, kind: :local_macro},
               %CallInfo{mod: Kernel, func: :self, kind: :remote_function},
               %CallInfo{func: :list, kind: :remote_function},
               %CallInfo{kind: :alias_reference}
             ] = state.calls[15]

      # NOTE we rely on module.__info__(:macros) for local macros and assume the loaded one is the same
      # as the one currently expanded
      # this means only public local macros are expandable
      # making it work for all locals would require hooking into :elixir_def and compiling the code
      assert [%CallInfo{func: :go_priv}] = state.calls[16]
    end
  end

  test "first_alias_positions" do
    state =
      """
      defmodule OuterMod do
        alias Foo.Bar
        alias Foo1.Bar1
        defmodule InnerMod do
          alias Baz.Quz
        end
      end
      """
      |> string_to_state

    assert %{OuterMod => {2, 3}, OuterMod.InnerMod => {5, 5}} = state.first_alias_positions
  end

  describe "use" do
    test "use" do
      state =
        """
        defmodule InheritMod do
          use ElixirSenseExample.ExampleBehaviour

          IO.puts("")
        end
        """
        |> string_to_state

      assert get_line_behaviours(state, 4) == [ElixirSenseExample.ExampleBehaviour]

      # note that `use` causes `require` to be able to execute `__using__/1` macro
      assert get_line_requires(state, 4) ==
               [
                 Application,
                 ElixirSenseExample.ExampleBehaviour,
                 Kernel,
                 Kernel.Typespec,
                 List,
                 MyImports.NestedImports,
                 MyImports.OneImports,
                 MyImports.Two.ThreeImports,
                 MyMacros,
                 MyMacros.Nested,
                 MyMacros.One,
                 MyMacros.Two.Three,
                 :ets,
                 :lists
               ]
               |> maybe_reject_typespec

      {functions, _} = get_line_imports(state, 4)
      assert Keyword.keys(functions) == [List, Kernel]

      assert Enum.sort(get_line_aliases(state, 4)) ==
               Enum.sort([
                 {Utils, MyModule.Some.Nested},
                 {Ets, :ets},
                 {One, MyModule.One},
                 {Three, MyModule.Two.Three},
                 {Four, MyModule.Four},
                 {OutsideOfMyModule, Three.OutsideOfMyModule},
                 {NestedMacros, MyMacros.Nested},
                 {ErlangMacros, :ets},
                 {Nested, InheritMod.Nested},
                 {Deeply, InheritMod.Deeply},
                 {ProtocolEmbedded, InheritMod.ProtocolEmbedded}
               ])

      assert [
               %AttributeInfo{name: :before_compile, positions: [{2, _}]},
               %AttributeInfo{name: :my_attribute, positions: [{2, _}]}
             ] = get_line_attributes(state, 4)

      assert %{
               {InheritMod, :handle_call, 3} => %ModFunInfo{
                 params: [
                   [
                     {:msg, _, _},
                     {:_from, _, _},
                     {:state, _, _}
                   ]
                 ],
                 type: :def
               },
               {InheritMod, nil, nil} => %ModFunInfo{
                 type: :defmodule
               },
               {InheritMod, :private_func, 0} => %ModFunInfo{
                 params: [[]],
                 type: :defp
               },
               {InheritMod, :private_func_arg, 1} => %ModFunInfo{
                 params: [
                   [{:a, _, _}],
                   [{:\\, _, [{:a, _, _}, nil]}]
                 ],
                 type: :defp
               },
               {InheritMod, :private_guard, 0} => %ModFunInfo{
                 params: [[]],
                 type: :defguardp
               },
               {InheritMod, :private_guard_arg, 1} => %ModFunInfo{
                 params: [
                   [
                     {:a, _, _}
                   ]
                 ],
                 type: :defguardp
               },
               {InheritMod, :private_macro, 0} => %ModFunInfo{
                 params: [[]],
                 type: :defmacrop
               },
               {InheritMod, :private_macro_arg, 1} => %ModFunInfo{
                 params: [
                   [
                     {:a, _, _}
                   ]
                 ],
                 type: :defmacrop
               },
               {InheritMod, :public_func, 0} => %ModFunInfo{
                 params: [[]],
                 type: :def,
                 overridable: {true, ElixirSenseExample.ExampleBehaviour}
               },
               {InheritMod, :public_func_arg, 2} => %ModFunInfo{
                 params: [
                   [
                     {:b, _, _},
                     {:\\, _,
                      [
                        {:a, _, _},
                        "def"
                      ]}
                   ]
                 ],
                 type: :def
               },
               {InheritMod, :public_guard, 0} => %ModFunInfo{
                 params: [[]],
                 type: :defguard
               },
               {InheritMod, :public_guard_arg, 1} => %ModFunInfo{
                 params: [
                   [
                     {:a, _, _}
                   ]
                 ],
                 type: :defguard
               },
               {InheritMod, :public_macro, 0} => %ModFunInfo{
                 params: [[]],
                 type: :defmacro
               },
               {InheritMod, :public_macro_arg, 1} => %ModFunInfo{
                 params: [
                   [
                     {:a, _, _}
                   ]
                 ],
                 type: :defmacro
               },
               {InheritMod.Deeply.Nested, nil, nil} => %ModFunInfo{
                 type: :defmodule
               },
               {InheritMod.Nested, nil, nil} => %ModFunInfo{
                 type: :defmodule
               },
               {InheritMod.ProtocolEmbedded, nil, nil} => %ModFunInfo{
                 type: :defmodule
               },
               {InheritMod, :behaviour_info, 1} => %ModFunInfo{
                 params: [[{:atom, _, nil}]],
                 type: :def
               },
               {InheritMod.ProtocolEmbedded, :module_info, 1} => %ModFunInfo{}
             } = state.mods_funs_to_positions

      assert %{
               {InheritMod, :my_opaque_type, 0} => %State.TypeInfo{
                 args: [[]],
                 kind: :opaque,
                 name: :my_opaque_type,
                 #  positions: [{2, 3}],
                 specs: ["@opaque my_opaque_type() :: any()"]
               },
               {InheritMod, :my_priv_type, 0} => %State.TypeInfo{
                 args: [[]],
                 kind: :typep,
                 name: :my_priv_type,
                 #  positions: [{2, 3}],
                 specs: ["@typep my_priv_type() :: any()"]
               },
               {InheritMod, :my_pub_type, 0} => %State.TypeInfo{
                 args: [[]],
                 kind: :type,
                 name: :my_pub_type,
                 #  positions: [{2, 3}],
                 specs: ["@type my_pub_type() :: any()"]
               },
               {InheritMod, :my_pub_type_arg, 2} => %State.TypeInfo{
                 args: [["a", "b"]],
                 kind: :type,
                 name: :my_pub_type_arg,
                 #  positions: [{2, 3}],
                 specs: ["@type my_pub_type_arg(a, b) :: {b, a}"]
               }
             } = state.types

      assert %{
               {InheritMod, :private_func, 0} => %State.SpecInfo{
                 args: [[]],
                 kind: :spec,
                 name: :private_func,
                 #  positions: [{2, 3}],
                 specs: ["@spec private_func() :: String.t()"]
               },
               {InheritMod, :some_callback, 1} => %State.SpecInfo{
                 args: [["abc"]],
                 kind: :callback,
                 name: :some_callback,
                 #  positions: [{2, 3}],
                 specs: ["@callback some_callback(abc) :: :ok when abc: integer()"]
               }
             } = state.specs
    end

    test "use defining struct" do
      state =
        """
        defmodule InheritMod do
          use ElixirSenseExample.ExampleBehaviourWithStruct

          IO.puts("")
        end
        """
        |> string_to_state

      assert %{
               InheritMod => %State.StructInfo{
                 fields: [{:a, nil}, {:b, 1}, __struct__: InheritMod],
                 type: :defstruct
               }
             } = state.structs

      assert %{
               {InheritMod, :__struct__, 0} => %State.ModFunInfo{},
               {InheritMod, :__struct__, 1} => %State.ModFunInfo{}
             } = state.mods_funs_to_positions
    end

    test "use defining exception" do
      state =
        """
        defmodule MyError do
          use ElixirSenseExample.ExampleBehaviourWithException

          IO.puts("")
        end
        """
        |> string_to_state

      assert %{
               MyError => %State.StructInfo{
                 fields: [{:a, nil}, {:b, 1}, __exception__: true, __struct__: MyError],
                 type: :defexception
               }
             } = state.structs

      assert %{
               {MyError, :__struct__, 0} => %State.ModFunInfo{},
               {MyError, :__struct__, 1} => %State.ModFunInfo{},
               {MyError, :exception, 1} => %State.ModFunInfo{}
             } = state.mods_funs_to_positions
    end

    test "use multi notation" do
      state =
        """
        defmodule InheritMod do
          use ElixirSenseExample.{ExampleBehaviour}
          # use Foo.{}

          IO.puts("")
        end
        """
        |> string_to_state

      assert get_line_behaviours(state, 5) == [ElixirSenseExample.ExampleBehaviour]
    end

    test "use multi notation with atom module" do
      state =
        """
        defmodule InheritMod do
          use :"Elixir.ElixirSenseExample".{:"Elixir.ExampleBehaviour"}

          IO.puts("")
        end
        """
        |> string_to_state

      assert get_line_behaviours(state, 4) == [ElixirSenseExample.ExampleBehaviour]
    end

    test "use with __MODULE__" do
      state =
        """
        defmodule ElixirSenseExample do
          use __MODULE__.ExampleBehaviour

          IO.puts("")
        end
        """
        |> string_to_state

      assert get_line_behaviours(state, 4) == [ElixirSenseExample.ExampleBehaviour]
    end

    test "use aliased" do
      state =
        """
        defmodule InheritMod do
          alias ElixirSenseExample.ExampleBehaviour, as: S
          use S

          IO.puts("")
        end
        """
        |> string_to_state

      assert get_line_behaviours(state, 5) == [ElixirSenseExample.ExampleBehaviour]
    end

    test "use atom module" do
      state =
        """
        defmodule InheritMod do
          use :"Elixir.ElixirSenseExample.ExampleBehaviour"

          IO.puts("")
        end
        """
        |> string_to_state

      assert get_line_behaviours(state, 4) == [ElixirSenseExample.ExampleBehaviour]
    end
  end

  describe "defstruct" do
    test "find struct" do
      state =
        """
        defmodule MyStruct do
          defstruct [:some_field, a_field: 1]
          IO.puts ""
          %Date{month: 1, day: 1, year: 1}
        end
        """
        |> string_to_state

      assert state.structs == %{
               MyStruct => %StructInfo{
                 type: :defstruct,
                 fields: [some_field: nil, a_field: 1, __struct__: MyStruct]
               }
             }

      # defstruct adds struct/0 and struct/1 functions
      assert %{
               {MyStruct, :__struct__, 0} => %ModFunInfo{
                 params: [[]],
                 positions: [{2, 3}],
                 type: :def
               },
               {MyStruct, :__struct__, 1} => %ModFunInfo{
                 params: [[{:kv, [line: 2, column: 3], nil}]],
                 positions: [{2, 3}],
                 type: :def
               },
               {MyStruct, nil, nil} => %ModFunInfo{
                 params: [nil],
                 positions: [{1, 1}],
                 type: :defmodule
               }
             } = state.mods_funs_to_positions
    end

    test "gracefully handles struct with expression fields" do
      state =
        """
        defmodule MyStruct do
          @fields_1 [a: nil]
          defstruct [a_field: nil] ++ @fields_1
        end
        """
        |> string_to_state

      assert state.structs == %{
               MyStruct => %StructInfo{type: :defstruct, fields: [__struct__: MyStruct]}
             }
    end

    test "find exception" do
      state =
        """
        defmodule MyError do
          defexception [message: nil]

          IO.puts("")
        end
        """
        |> string_to_state

      assert state.structs == %{
               MyError => %StructInfo{
                 type: :defexception,
                 fields: [message: nil, __exception__: true, __struct__: MyError]
               }
             }

      # defexception adds Exception behaviour
      assert get_line_behaviours(state, 4) == [Exception]
      # and message/1 and exception/1 callbacks
      assert %{
               {MyError, :__struct__, 0} => %ModFunInfo{
                 params: [[]],
                 positions: [{2, 3}],
                 type: :def
               },
               {MyError, :__struct__, 1} => %ModFunInfo{
                 params: [[{:kv, [line: 2, column: 3], nil}]],
                 positions: [{2, 3}],
                 type: :def
               },
               {MyError, :exception, 1} => %ModFunInfo{
                 params: [
                   [{:args, [line: 2, column: 3], nil}],
                   [{:msg, [line: 2, column: 3], nil}]
                 ],
                 positions: [{2, 3}, {2, 3}],
                 type: :def
               },
               {MyError, :message, 1} => %ModFunInfo{
                 params: [[{:exception, [line: 2, column: 3], nil}]],
                 positions: [{2, 3}],
                 type: :def
               }
             } = state.mods_funs_to_positions
    end

    test "find exception without message key" do
      state =
        """
        defmodule MyError do
          defexception []

          IO.puts("")
        end
        """
        |> string_to_state

      assert state.structs == %{
               MyError => %StructInfo{
                 type: :defexception,
                 fields: [__exception__: true, __struct__: MyError]
               }
             }

      # defexception adds Exception behaviour
      assert get_line_behaviours(state, 4) == [Exception]
      # and message/1 and exception/1 callbacks
      assert %{
               {MyError, :__struct__, 0} => %ModFunInfo{
                 params: [[]],
                 positions: [{2, 3}],
                 type: :def
               },
               {MyError, :__struct__, 1} => %ModFunInfo{
                 params: [[{:kv, [line: 2, column: 3], nil}]],
                 positions: [{2, 3}],
                 type: :def
               },
               {MyError, :exception, 1} => %ModFunInfo{
                 params: [[{:args, [line: 2, column: 3], nil}]],
                 positions: [{2, 3}],
                 type: :def
               }
             } = state.mods_funs_to_positions
    end

    test "expands local struct" do
      state =
        """
        defmodule MyStruct do
          defstruct [:some_field, a_field: 1]
          var = %MyStruct{some_field: 3}
          var = %MyStruct{}
          IO.puts ""
        end
        """
        |> string_to_state

      assert state.structs == %{
               MyStruct => %StructInfo{
                 type: :defstruct,
                 fields: [some_field: nil, a_field: 1, __struct__: MyStruct]
               }
             }
    end

    test "expands local not existing struct" do
      state =
        """
        defmodule MyStruct do
          var = %MyStruct{some_field: 3}
          IO.puts ""
        end
        """
        |> string_to_state

      assert state.structs == %{}
    end

    test "expands remote not existing struct" do
      state =
        """
        defmodule MyStruct do
          var = %FooStruct{some_field: 3}
          IO.puts ""
        end
        """
        |> string_to_state

      assert state.structs == %{}
    end

    test "expands local struct defined in other module" do
      state =
        """
        defmodule MyStruct do
          defstruct [:some_field, a_field: 1]
        end

        defmodule Foo do
          var = %MyStruct{some_field: 3}
          IO.puts ""
        end
        """
        |> string_to_state

      assert state.structs == %{
               MyStruct => %StructInfo{
                 type: :defstruct,
                 fields: [some_field: nil, a_field: 1, __struct__: MyStruct]
               }
             }
    end

    test "captures documentation and since metadata" do
      state =
        """
        defmodule MyStruct do
          @moduledoc "Module documentation"
          @doc "Struct documentation"
          @doc since: "1.2.3"
          defstruct [:some_field, a_field: 1]
        end
        """
        |> string_to_state

      assert state.structs == %{
               MyStruct => %StructInfo{
                 type: :defstruct,
                 fields: [some_field: nil, a_field: 1, __struct__: MyStruct],
                 doc: "Struct documentation",
                 meta: %{since: "1.2.3"}
               }
             }

      assert %{meta: %{since: "1.2.3"}, doc: "Struct documentation"} =
               state.mods_funs_to_positions[{MyStruct, :__struct__, 0}]
    end
  end

  describe "calls" do
    test "registers calls on default parameters" do
      state =
        """
        defmodule NyModule do
          def func1(a, b \\\\ some(), c \\\\ Some.other()), do: :ok
        end
        """
        |> string_to_state

      assert [
               _,
               %CallInfo{
                 arity: 0,
                 func: :other,
                 mod: Some,
                 position: {2, 39},
                 kind: :remote_function
               },
               %CallInfo{
                 arity: nil,
                 position: {2, 34},
                 mod: Some,
                 func: nil,
                 kind: :alias_reference
               },
               %CallInfo{
                 arity: 0,
                 position: {2, 21},
                 func: :some,
                 mod: NyModule,
                 kind: :local_function
               }
             ] = state.calls[2]
    end

    test "registers calls with __MODULE__" do
      state =
        """
        defmodule NyModule do
          def func1, do: :ok
          def func2(a), do: :ok
          def func do
            __MODULE__.func1
            __MODULE__.func1()
            __MODULE__.func2(2)
            __MODULE__.Sub.func2(2)
          end
        end
        """
        |> string_to_state

      assert %{
               5 => [%CallInfo{arity: 0, func: :func1, position: {5, 16}, mod: NyModule}],
               6 => [%CallInfo{arity: 0, func: :func1, position: {6, 16}, mod: NyModule}],
               7 => [%CallInfo{arity: 1, func: :func2, position: {7, 16}, mod: NyModule}],
               8 => [
                 %CallInfo{arity: 1, func: :func2, position: {8, 20}, mod: NyModule.Sub},
                 %CallInfo{arity: nil, position: {8, 15}, func: nil, mod: NyModule.Sub}
               ]
             } = state.calls
    end

    test "registers calls with erlang module" do
      state =
        """
        defmodule NyModule do
          def func do
            :erl_mod.func1
            :erl_mod.func1()
            :erl_mod.func2(2)
          end
        end
        """
        |> string_to_state

      assert %{
               3 => [%CallInfo{arity: 0, func: :func1, position: {3, 14}, mod: :erl_mod}],
               4 => [%CallInfo{arity: 0, func: :func1, position: {4, 14}, mod: :erl_mod}],
               5 => [%CallInfo{arity: 1, func: :func2, position: {5, 14}, mod: :erl_mod}]
             } = state.calls
    end

    test "registers calls with atom module" do
      state =
        """
        defmodule NyModule do
          def func do
            :"Elixir.MyMod".func1
            :"Elixir.MyMod".func1()
            :"Elixir.MyMod".func2(2)
          end
        end
        """
        |> string_to_state

      assert %{
               3 => [%CallInfo{arity: 0, func: :func1, position: {3, 21}, mod: MyMod}],
               4 => [%CallInfo{arity: 0, func: :func1, position: {4, 21}, mod: MyMod}],
               5 => [%CallInfo{arity: 1, func: :func2, position: {5, 21}, mod: MyMod}]
             } = state.calls
    end

    test "registers calls no arg no parens" do
      state =
        """
        defmodule NyModule do
          def func do
            MyMod.func
          end
        end
        """
        |> string_to_state

      assert %{
               3 => [
                 %CallInfo{arity: 0, func: :func, position: {3, 11}, mod: MyMod},
                 %ElixirSense.Core.State.CallInfo{
                   arity: nil,
                   position: {3, 5},
                   mod: MyMod,
                   func: nil
                 }
               ]
             } = state.calls
    end

    test "registers calls no arg" do
      state =
        """
        defmodule NyModule do
          def func do
            MyMod.func()
          end
        end
        """
        |> string_to_state

      assert %{
               3 => [
                 %CallInfo{arity: 0, func: :func, position: {3, 11}, mod: MyMod},
                 %ElixirSense.Core.State.CallInfo{
                   arity: nil,
                   position: {3, 5},
                   mod: MyMod,
                   func: nil
                 }
               ]
             } = state.calls
    end

    test "registers calls local no arg no parens" do
      state =
        """
        defmodule NyModule do
          def func_1, do: :ok
          def func do
            func_1
          end
        end
        """
        |> string_to_state

      if Version.match?(System.version(), ">= 1.15.0") do
        assert state.calls
               |> Enum.flat_map(fn {_line, info} -> info end)
               |> Enum.filter(fn info -> info.kind == :local_function end) == []
      else
        assert %{
                 4 => [
                   %CallInfo{
                     arity: 0,
                     func: :func_1,
                     position: {4, 5},
                     mod: NyModule,
                     kind: :local_function
                   }
                 ]
               } = state.calls
      end
    end

    test "registers macro calls" do
      state =
        """
        defmodule NyModule do
          @foo "123"
          require Record
          Record.defrecord(:user, name: "meg", age: "25")
          def func do
            IO.inspect(binding())
            :ok
          end
        end
        """
        |> string_to_state

      assert %{
               1 => [
                 %CallInfo{
                   arity: 2,
                   position: {1, 1},
                   func: :defmodule,
                   mod: Kernel,
                   kind: :imported_macro
                 }
               ],
               2 => [
                 %CallInfo{
                   arity: 1,
                   position: {2, 3},
                   func: :@,
                   mod: Kernel,
                   kind: :imported_macro
                 }
               ],
               3 => [
                 %CallInfo{
                   arity: nil,
                   func: nil,
                   mod: Record,
                   position: {3, 3},
                   kind: :require
                 }
               ],
               4 => [
                 %CallInfo{
                   arity: 2,
                   position: {4, 10},
                   func: :defrecord,
                   mod: Record,
                   kind: :remote_macro
                 },
                 %CallInfo{
                   arity: nil,
                   position: {4, 3},
                   mod: Record,
                   func: nil,
                   kind: :alias_reference
                 }
               ],
               5 => [
                 %CallInfo{
                   arity: 2,
                   position: {5, 3},
                   func: :def,
                   mod: Kernel,
                   kind: :imported_macro
                 }
               ],
               6 => [
                 %CallInfo{
                   arity: 0,
                   position: {6, 16},
                   func: :binding,
                   mod: Kernel,
                   kind: :imported_macro
                 },
                 %CallInfo{
                   arity: 1,
                   position: {6, 8},
                   func: :inspect,
                   mod: IO,
                   kind: :remote_function
                 },
                 %CallInfo{
                   arity: nil,
                   position: {6, 5},
                   mod: IO,
                   func: nil,
                   kind: :alias_reference
                 }
               ]
             } == state.calls
    end

    defmodule ModuleCallbacks do
      defmacro __before_compile_macro__(_env) do
        quote do
          def hello, do: "world"
        end
      end

      def __before_compile__(env) do
        IO.inspect(env)
      end

      def __after_compile__(env, _bytecode) do
        IO.inspect(env)
      end

      def __after_verify__(module) do
        IO.inspect(module)
        :ok
      end

      def __on_definition__(env, kind, name, args, guards, body) do
        IO.inspect(env)
        IO.inspect(kind)
        IO.inspect(name)
        IO.inspect(args)
        IO.inspect(guards)
        IO.inspect(body)
      end
    end

    test "registers module callback calls" do
      state =
        """
        defmodule WithCallbacks do
          @before_compile {ElixirSense.Core.MetadataBuilderTest.ModuleCallbacks, :__before_compile_macro__}
          @before_compile ElixirSense.Core.MetadataBuilderTest.ModuleCallbacks
          @after_compile ElixirSense.Core.MetadataBuilderTest.ModuleCallbacks
          @after_verify ElixirSense.Core.MetadataBuilderTest.ModuleCallbacks
          @on_definition ElixirSense.Core.MetadataBuilderTest.ModuleCallbacks
          @on_load :load_check

          def load_check do
            :ok
          end
        end
        """
        |> string_to_state

      assert state.calls[1]
             |> Enum.any?(fn info ->
               match?(
                 %ElixirSense.Core.State.CallInfo{
                   arity: 1,
                   position: {1, nil},
                   mod: ElixirSense.Core.MetadataBuilderTest.ModuleCallbacks,
                   func: :__before_compile_macro__,
                   kind: :remote_macro
                 },
                 info
               )
             end)

      assert state.calls[1]
             |> Enum.any?(fn info ->
               match?(
                 %ElixirSense.Core.State.CallInfo{
                   arity: 1,
                   position: {1, nil},
                   mod: ElixirSense.Core.MetadataBuilderTest.ModuleCallbacks,
                   func: :__before_compile__,
                   kind: :remote_function
                 },
                 info
               )
             end)

      assert state.calls[1]
             |> Enum.any?(fn info ->
               match?(
                 %ElixirSense.Core.State.CallInfo{
                   arity: 2,
                   position: {1, nil},
                   mod: ElixirSense.Core.MetadataBuilderTest.ModuleCallbacks,
                   func: :__after_compile__,
                   kind: :remote_function
                 },
                 info
               )
             end)

      assert state.calls[1]
             |> Enum.any?(fn info ->
               match?(
                 %ElixirSense.Core.State.CallInfo{
                   arity: 1,
                   position: {1, nil},
                   mod: ElixirSense.Core.MetadataBuilderTest.ModuleCallbacks,
                   func: :__after_verify__,
                   kind: :remote_function
                 },
                 info
               )
             end)

      assert state.calls[1]
             |> Enum.any?(fn info ->
               match?(
                 %ElixirSense.Core.State.CallInfo{
                   arity: 6,
                   position: {1, nil},
                   mod: ElixirSense.Core.MetadataBuilderTest.ModuleCallbacks,
                   func: :__on_definition__,
                   kind: :remote_function
                 },
                 info
               )
             end)

      assert state.calls[1]
             |> Enum.any?(fn info ->
               match?(
                 %ElixirSense.Core.State.CallInfo{
                   arity: 0,
                   position: {1, nil},
                   mod: WithCallbacks,
                   func: :load_check,
                   kind: :local_function
                 },
                 info
               )
             end)
    end

    test "registers behaviour_info call on @behaviour" do
      state =
        """
        defmodule NyModule do
          @behaviour MyBehaviour
        end
        """
        |> string_to_state

      assert [
               %CallInfo{func: :defmodule},
               %CallInfo{
                 arity: 1,
                 position: {1, nil},
                 mod: MyBehaviour,
                 func: :behaviour_info,
                 kind: :remote_function
               }
             ] = state.calls[1]
    end

    defmodule StructExpansion do
      defstruct [:foo]
    end

    test "registers struct expansion" do
      state =
        """
        defmodule MyModule do
          defstruct [:foo]
        end

        defmodule Foo do
          @spec bar(%MyModule{}) :: %ElixirSense.Core.MetadataBuilderTest.StructExpansion{}
          def bar(x) do
            a = %ElixirSense.Core.MetadataBuilderTest.StructExpansion{foo: "bar"}
            b = %MyModule{x | foo: "baz"}
            a
          end
        end
        """
        |> string_to_state

      assert %{
               6 => [
                 %CallInfo{
                   mod: Kernel,
                   func: :@
                 },
                 %CallInfo{
                   arity: nil,
                   position: {6, 14},
                   mod: MyModule,
                   func: nil,
                   kind: :alias_reference
                 },
                 %CallInfo{
                   arity: nil,
                   position: {6, 13},
                   mod: MyModule,
                   func: nil,
                   kind: :struct_expansion
                 },
                 %CallInfo{
                   arity: nil,
                   position: {6, 30},
                   mod: ElixirSense.Core.MetadataBuilderTest.StructExpansion,
                   func: nil,
                   kind: :alias_reference
                 },
                 %CallInfo{
                   arity: nil,
                   position: {6, 29},
                   mod: ElixirSense.Core.MetadataBuilderTest.StructExpansion,
                   func: nil,
                   kind: :struct_expansion
                 }
               ],
               8 => [
                 %CallInfo{
                   arity: nil,
                   position: {8, 9},
                   mod: ElixirSense.Core.MetadataBuilderTest.StructExpansion,
                   func: nil,
                   kind: :struct_expansion
                 },
                 %CallInfo{
                   arity: nil,
                   position: {8, 10},
                   mod: ElixirSense.Core.MetadataBuilderTest.StructExpansion,
                   func: nil,
                   kind: :alias_reference
                 }
               ],
               9 => [
                 %CallInfo{
                   arity: nil,
                   position: {9, 9},
                   mod: MyModule,
                   func: nil,
                   kind: :struct_expansion
                 },
                 %CallInfo{
                   arity: nil,
                   position: {9, 10},
                   mod: MyModule,
                   func: nil,
                   kind: :alias_reference
                 }
               ]
             } = state.calls
    end

    test "registers typespec no parens calls" do
      state =
        """
        defmodule NyModule do
          @type a :: integer
        end
        """
        |> string_to_state

      assert %{
               2 => [
                 _,
                 %CallInfo{
                   arity: 0,
                   func: :integer,
                   position: {2, 14},
                   mod: nil,
                   kind: :builtin_typespec
                 }
               ]
             } = state.calls
    end

    test "registers typespec parens calls" do
      state =
        """
        defmodule NyModule do
          @type a() :: integer()
        end
        """
        |> string_to_state

      assert %{
               2 => [
                 _,
                 %CallInfo{
                   arity: 0,
                   func: :integer,
                   position: {2, 16},
                   mod: nil,
                   kind: :builtin_typespec
                 }
               ]
             } = state.calls
    end

    test "registers typespec no parens remote calls" do
      state =
        """
        defmodule NyModule do
          @type a :: Enum.t
        end
        """
        |> string_to_state

      assert %{
               2 => [
                 _,
                 %CallInfo{
                   arity: 0,
                   func: :t,
                   position: {2, 19},
                   mod: Enum,
                   kind: :remote_typespec
                 }
               ]
             } = state.calls
    end

    test "registers typespec parens remote calls" do
      state =
        """
        defmodule NyModule do
          @type a() :: Enum.t()
          @type a(x) :: {Enum.t(), x}
        end
        """
        |> string_to_state

      assert %{
               2 => [
                 %CallInfo{arity: 1, func: :@, position: {2, 3}, mod: Kernel},
                 %CallInfo{
                   arity: 0,
                   func: :t,
                   position: {2, 21},
                   mod: Enum,
                   kind: :remote_typespec
                 }
               ],
               3 => [
                 %CallInfo{arity: 1, func: :@, position: {3, 3}, mod: Kernel},
                 %CallInfo{
                   arity: 0,
                   func: :t,
                   position: {3, 23},
                   mod: Enum,
                   kind: :remote_typespec
                 }
               ]
             } = state.calls
    end

    test "registers typespec calls in specs with when guard" do
      state =
        """
        defmodule NyModule do
          @type foo :: integer()
          @callback a(b, c, d) :: {b, integer(), c} when b: foo(), c: var, d: pos_integer
        end
        """
        |> string_to_state

      # NOTE var is not a type but a special variable
      assert %{
               3 => [
                 %CallInfo{arity: 1, func: :@, position: {3, 3}, mod: Kernel},
                 %CallInfo{
                   arity: 0,
                   func: :pos_integer,
                   position: {3, 71},
                   mod: nil,
                   kind: :builtin_typespec
                 },
                 %CallInfo{
                   arity: 0,
                   func: :foo,
                   position: {3, 53},
                   mod: NyModule,
                   kind: :local_typespec
                 },
                 %CallInfo{
                   arity: 0,
                   func: :integer,
                   position: {3, 31},
                   mod: nil,
                   kind: :builtin_typespec
                 }
               ]
             } = state.calls
    end

    test "registers typespec calls in typespec with named args" do
      state =
        """
        defmodule NyModule do
          @callback days_since_epoch(year :: integer, month :: integer, day :: integer) :: integer
          @type color :: {red :: integer, green :: integer, blue :: integer}
        end
        """
        |> string_to_state

      assert %{
               2 => [
                 %CallInfo{arity: 1, func: :@, position: {2, 3}, mod: Kernel},
                 %CallInfo{
                   arity: 0,
                   func: :integer,
                   position: {2, 84},
                   mod: nil,
                   kind: :builtin_typespec
                 },
                 %CallInfo{
                   arity: 0,
                   func: :integer,
                   position: {2, 72},
                   mod: nil,
                   kind: :builtin_typespec
                 },
                 %CallInfo{
                   arity: 0,
                   func: :integer,
                   position: {2, 56},
                   mod: nil,
                   kind: :builtin_typespec
                 },
                 %CallInfo{
                   arity: 0,
                   func: :integer,
                   position: {2, 38},
                   mod: nil,
                   kind: :builtin_typespec
                 }
                 | _
               ],
               3 => [
                 %CallInfo{arity: 1, func: :@, position: {3, 3}, mod: Kernel},
                 %CallInfo{
                   arity: 0,
                   func: :integer,
                   position: {3, 61},
                   mod: nil,
                   kind: :builtin_typespec
                 },
                 %CallInfo{
                   arity: 0,
                   func: :integer,
                   position: {3, 44},
                   mod: nil,
                   kind: :builtin_typespec
                 },
                 %CallInfo{
                   arity: 0,
                   func: :integer,
                   position: {3, 26},
                   mod: nil,
                   kind: :builtin_typespec
                 }
                 | _
               ]
             } = state.calls
    end

    test "registers calls local no arg" do
      state =
        """
        defmodule NyModule do
          def func_1, do: :ok
          def func do
            func_1()
          end
        end
        """
        |> string_to_state

      assert %{
               4 => [
                 %CallInfo{
                   arity: 0,
                   func: :func_1,
                   position: {4, 5},
                   mod: NyModule,
                   kind: :local_function
                 }
               ]
             } = state.calls
    end

    test "registers calls local arg" do
      state =
        """
        defmodule NyModule do
          def func do
            func_1("a")
          end
        end
        """
        |> string_to_state

      assert %{
               3 => [
                 %CallInfo{
                   arity: 1,
                   func: :func_1,
                   position: {3, 5},
                   mod: NyModule,
                   kind: :local_function
                 }
               ]
             } = state.calls
    end

    test "registers calls arg" do
      state =
        """
        defmodule NyModule do
          def func do
            MyMod.func("test")
          end
        end
        """
        |> string_to_state

      assert %{
               3 => [
                 %CallInfo{
                   arity: 1,
                   func: :func,
                   position: {3, 11},
                   mod: MyMod,
                   kind: :remote_function
                 },
                 %CallInfo{
                   arity: nil,
                   position: {3, 5},
                   mod: MyMod,
                   func: nil,
                   kind: :alias_reference
                 }
               ]
             } = state.calls
    end

    test "registers calls on attribute and var with args" do
      state =
        """
        defmodule NyModule do
          @attr Some
          def func(var) do
            @attr.func("test")
            var.func("test")
          end
        end
        """
        |> string_to_state

      assert %{
               4 => [
                 %CallInfo{mod: Kernel, func: :@},
                 %CallInfo{
                   arity: 1,
                   func: :func,
                   position: {4, 11},
                   mod: {:attribute, :attr},
                   kind: :remote_function
                 }
               ],
               5 => [
                 %CallInfo{
                   arity: 1,
                   func: :func,
                   position: {5, 9},
                   mod: {:variable, :var, 0},
                   kind: :remote_function
                 }
               ]
             } = state.calls
    end

    test "registers calls on attribute and var without args" do
      state =
        """
        defmodule NyModule do
          @attr (fn -> :ok end)
          def func(var) do
            @attr.func
            var.func
          end
        end
        """
        |> string_to_state

      Enum.any?(
        state.calls[4],
        &match?(
          %CallInfo{arity: 0, func: :func, position: {4, 11}, mod: {:attribute, :attr}},
          &1
        )
      )

      Enum.any?(
        state.calls[4],
        &match?(
          %CallInfo{arity: 0, func: :func, position: {5, 9}, mod: {:variable, :var, 0}},
          &1
        )
      )
    end

    test "registers calls on attribute and var anonymous" do
      state =
        """
        defmodule NyModule do
          @attr (fn -> :ok end)
          def func(var) do
            @attr.()
            var.()
          end
        end
        """
        |> string_to_state

      assert %{
               4 => [
                 %CallInfo{mod: Kernel, func: :@},
                 %CallInfo{
                   arity: 0,
                   func: {:attribute, :attr},
                   position: {4, 11},
                   mod: nil,
                   kind: :anonymous_function
                 }
               ],
               5 => [
                 %CallInfo{
                   arity: 0,
                   func: {:variable, :var, 0},
                   position: {5, 9},
                   mod: nil,
                   kind: :anonymous_function
                 }
               ]
             } = state.calls
    end

    test "registers calls pipe with __MODULE__ operator no parens" do
      state =
        """
        defmodule NyModule do
          def func do
            "test" |> __MODULE__.func
          end
        end
        """
        |> string_to_state

      assert Enum.any?(
               state.calls[3],
               &match?(%CallInfo{arity: 1, position: {3, 26}, func: :func, mod: NyModule}, &1)
             )
    end

    test "registers calls pipe operator no parens" do
      state =
        """
        defmodule NyModule do
          def func do
            "test" |> MyMod.func
          end
        end
        """
        |> string_to_state

      assert Enum.any?(
               state.calls[3],
               &match?(%CallInfo{arity: 1, position: {3, 21}, func: :func, mod: MyMod}, &1)
             )
    end

    test "registers calls pipe operator" do
      state =
        """
        defmodule NyModule do
          def func do
            "test" |> MyMod.func()
          end
        end
        """
        |> string_to_state

      assert Enum.any?(
               state.calls[3],
               &match?(%CallInfo{arity: 1, position: {3, 21}, func: :func, mod: MyMod}, &1)
             )
    end

    test "registers calls pipe operator with arg" do
      state =
        """
        defmodule NyModule do
          def func do
            "test" |> MyMod.func("arg")
          end
        end
        """
        |> string_to_state

      assert Enum.any?(
               state.calls[3],
               &match?(%CallInfo{arity: 2, position: {3, 21}, func: :func, mod: MyMod}, &1)
             )
    end

    test "registers calls pipe operator erlang module" do
      state =
        """
        defmodule NyModule do
          def func do
            "test" |> :my_mod.func("arg")
          end
        end
        """
        |> string_to_state

      assert Enum.any?(
               state.calls[3],
               &match?(%CallInfo{arity: 2, position: {3, 23}, func: :func, mod: :my_mod}, &1)
             )
    end

    test "registers calls pipe operator atom module" do
      state =
        """
        defmodule NyModule do
          def func do
            "test" |> :"Elixir.MyMod".func("arg")
          end
        end
        """
        |> string_to_state

      assert Enum.any?(
               state.calls[3],
               &match?(%CallInfo{arity: 2, position: {3, 31}, func: :func, mod: MyMod}, &1)
             )
    end

    test "registers calls pipe operator local" do
      state =
        """
        defmodule NyModule do
          def func do
            "test" |> func("arg")
          end
        end
        """
        |> string_to_state

      assert Enum.any?(
               state.calls[3],
               &match?(%CallInfo{arity: 2, position: {3, 15}, func: :func, mod: NyModule}, &1)
             )
    end

    test "registers calls pipe operator nested external into local" do
      state =
        """
        defmodule NyModule do
          def func do
            "test" |> MyMod.func() |> other
          end
        end
        """
        |> string_to_state

      assert Enum.any?(
               state.calls[3],
               &match?(%CallInfo{arity: 1, position: {3, 21}, func: :func, mod: MyMod}, &1)
             )

      assert Enum.any?(
               state.calls[3],
               &match?(%CallInfo{arity: 1, position: {3, 31}, func: :other, mod: NyModule}, &1)
             )
    end

    test "registers calls pipe operator nested external into external" do
      state =
        """
        defmodule NyModule do
          def func do
            "test" |> MyMod.func() |> Other.other
          end
        end
        """
        |> string_to_state

      assert Enum.any?(
               state.calls[3],
               &match?(%CallInfo{arity: 1, position: {3, 21}, func: :func, mod: MyMod}, &1)
             )

      assert Enum.any?(
               state.calls[3],
               &match?(%CallInfo{arity: 1, position: {3, 37}, func: :other, mod: Other}, &1)
             )
    end

    test "registers calls pipe operator nested local into external" do
      state =
        """
        defmodule NyModule do
          def func do
            "test" |> func_1() |> Some.other
          end
        end
        """
        |> string_to_state

      assert Enum.any?(
               state.calls[3],
               &match?(%CallInfo{arity: 1, position: {3, 15}, func: :func_1, mod: NyModule}, &1)
             )

      assert Enum.any?(
               state.calls[3],
               &match?(%CallInfo{arity: 1, position: {3, 32}, func: :other, mod: Some}, &1)
             )
    end

    test "registers calls pipe operator nested local into local" do
      state =
        """
        defmodule NyModule do
          def func do
            "test" |> func_1() |> other
          end
        end
        """
        |> string_to_state

      assert Enum.any?(
               state.calls[3],
               &match?(%CallInfo{arity: 1, position: {3, 15}, func: :func_1, mod: NyModule}, &1)
             )

      assert Enum.any?(
               state.calls[3],
               &match?(%CallInfo{arity: 1, position: {3, 27}, func: :other, mod: NyModule}, &1)
             )
    end

    test "registers super call" do
      state =
        """
        defmodule My do
          use ElixirSenseExample.OverridableFunctions

          def test(a, b) do
            super(a, b)
          end
        end
        """
        |> string_to_state

      assert state.calls[5] == [
               %CallInfo{arity: 2, position: {5, 5}, func: :test, mod: My, kind: :local_function}
             ]
    end

    test "registers super capture expression" do
      state =
        """
        defmodule My do
          use ElixirSenseExample.OverridableFunctions

          def test(a, b) do
            a |> Enum.map(&super(&1, b))
          end
        end
        """
        |> string_to_state

      assert [
               _,
               %CallInfo{
                 arity: 2,
                 position: {5, 20},
                 func: :test,
                 mod: My,
                 kind: :local_function
               },
               _,
               _
             ] =
               state.calls[5]
    end

    test "registers super capture" do
      state =
        """
        defmodule My do
          use ElixirSenseExample.OverridableFunctions

          def test(a, b) do
            a |> Enum.map_reduce([], &super/2)
          end
        end
        """
        |> string_to_state

      assert [
               _,
               %CallInfo{
                 arity: 2,
                 position: {5, 31},
                 func: :test,
                 mod: My,
                 kind: :local_function
               },
               _,
               _
             ] =
               state.calls[5]
    end

    test "registers calls capture operator __MODULE__" do
      state =
        """
        defmodule NyModule do
          def func do
            &__MODULE__.func/1
            &__MODULE__.Sub.func/1
          end
        end
        """
        |> string_to_state

      assert %{
               3 => [%CallInfo{arity: 1, position: {3, 17}, func: :func, mod: NyModule}],
               4 => [
                 %CallInfo{arity: 1, position: {4, 21}, func: :func, mod: NyModule.Sub},
                 %ElixirSense.Core.State.CallInfo{
                   arity: nil,
                   position: {4, 16},
                   mod: NyModule.Sub,
                   func: nil
                 }
               ]
             } = state.calls
    end

    test "registers calls capture operator external" do
      state =
        """
        defmodule NyModule do
          def func do
            &MyMod.func/1
          end
        end
        """
        |> string_to_state

      assert %{
               3 => [
                 %CallInfo{arity: 1, position: {3, 12}, func: :func, mod: MyMod},
                 %CallInfo{arity: nil, position: {3, 6}, func: nil, mod: MyMod}
               ]
             } = state.calls
    end

    test "registers calls capture required macro" do
      state =
        """
        defmodule Foo do
          defmacro bar, do: :ok
        end

        defmodule NyModule do
          require Foo
          require ElixirSenseExample.Math
          def func do
            &Foo.bar/0
            &ElixirSenseExample.Math.squared/1
          end
        end
        """
        |> string_to_state

      assert %{
               9 => [
                 %CallInfo{
                   arity: 0,
                   position: {9, 10},
                   func: :bar,
                   mod: Foo,
                   kind: :remote_function
                 },
                 %CallInfo{
                   arity: nil,
                   position: {9, 6},
                   mod: Foo,
                   func: nil,
                   kind: :alias_reference
                 }
               ],
               10 => [
                 %CallInfo{
                   arity: 1,
                   position: {10, 30},
                   func: :squared,
                   mod: ElixirSenseExample.Math,
                   kind: :remote_macro
                 },
                 %ElixirSense.Core.State.CallInfo{
                   arity: 2,
                   func: :*,
                   kind: :remote_function,
                   mod: Kernel,
                   position: {10, nil}
                 },
                 %ElixirSense.Core.State.CallInfo{
                   arity: nil,
                   position: {10, 6},
                   mod: ElixirSenseExample.Math,
                   func: nil,
                   kind: :alias_reference
                 }
               ]
             } = state.calls
    end

    defmodule QuotedCalls do
      def aaa, do: :ok
      defmacro bbb, do: :ok

      defmacro foo do
        quote do
          aaa()
          &aaa/0
          bbb()
          &bbb/0
          inspect(1)
          &inspect/1
          Node.list()
          &Node.list/0
        end
      end
    end

    test "registers calls capture quoted expanded macro" do
      state =
        """
        defmodule MyModule do
          require ElixirSense.Core.MetadataBuilderTest.QuotedCalls, as: Q
          def aasa, do: Q.foo()
        end
        """
        |> string_to_state

      assert [
               %ElixirSense.Core.State.CallInfo{
                 func: :def,
                 kind: :imported_macro,
                 mod: Kernel,
                 arity: 2
               },
               %ElixirSense.Core.State.CallInfo{
                 arity: nil,
                 position: {3, 17},
                 mod: ElixirSense.Core.MetadataBuilderTest.QuotedCalls,
                 func: nil,
                 kind: :alias_expansion
               },
               %ElixirSense.Core.State.CallInfo{
                 arity: nil,
                 position: {3, 17},
                 mod: Q,
                 func: nil,
                 kind: :alias_expansion_as
               },
               %ElixirSense.Core.State.CallInfo{
                 func: :foo,
                 kind: :remote_macro,
                 mod: ElixirSense.Core.MetadataBuilderTest.QuotedCalls,
                 arity: 0
               },
               %ElixirSense.Core.State.CallInfo{
                 func: :aaa,
                 kind: :local_function,
                 mod: MyModule,
                 arity: 0
               },
               %ElixirSense.Core.State.CallInfo{
                 func: :bbb,
                 kind: :local_function,
                 mod: MyModule,
                 arity: 0
               },
               %ElixirSense.Core.State.CallInfo{
                 func: :inspect,
                 mod: Kernel,
                 arity: 1,
                 kind: :remote_function
               },
               %ElixirSense.Core.State.CallInfo{
                 arity: 1,
                 position: {3, nil},
                 mod: Kernel,
                 func: :inspect,
                 kind: :remote_function
               },
               %ElixirSense.Core.State.CallInfo{
                 arity: 0,
                 func: :list,
                 mod: Node,
                 kind: :remote_function
               },
               %ElixirSense.Core.State.CallInfo{
                 func: nil,
                 kind: :alias_reference,
                 mod: Node,
                 arity: nil
               },
               %ElixirSense.Core.State.CallInfo{
                 func: :list,
                 mod: Node,
                 arity: 0,
                 kind: :remote_function
               },
               %ElixirSense.Core.State.CallInfo{
                 arity: nil,
                 func: nil,
                 mod: Node,
                 kind: :alias_reference
               },
               %ElixirSense.Core.State.CallInfo{
                 func: :bbb,
                 kind: :local_function,
                 mod: MyModule,
                 arity: 0
               },
               %ElixirSense.Core.State.CallInfo{
                 func: :aaa,
                 kind: :local_function,
                 mod: MyModule,
                 arity: 0
               },
               %ElixirSense.Core.State.CallInfo{
                 func: nil,
                 kind: :alias_reference,
                 mod: ElixirSense.Core.MetadataBuilderTest.QuotedCalls
               }
             ] = state.calls[3]
    end

    test "registers calls capture quoted" do
      state =
        """
        defmodule MyModule do
          def aaa, do: :ok
          defmacro bbb, do: :ok
          defmacro foo do
            quote do
              aaa()
              &aaa/0
              bbb()
              &bbb/0
              inspect(1)
              &inspect/1
              Node.list()
              &Node.list/0
            end
          end
        end
        """
        |> string_to_state

      # see https://github.com/elixir-lang/elixir/issues/13878 for discussion
      # In elixir quoted only import capture is precise and has meaning
      # It emits function/macro_imported. Import local call emits imported_quoted
      # with all imported arities but this only indicates that import may be used.
      # All other nodes are meaningless. They are traced as calls on macro expansion.
      # Hence we trace call only on line with import capture

      assert %{
               7 => [
                 %ElixirSense.Core.State.CallInfo{
                   arity: 2,
                   position: {7, 11},
                   func: :/,
                   mod: Kernel,
                   kind: :imported_quoted
                 }
               ],
               9 => [
                 %ElixirSense.Core.State.CallInfo{
                   arity: 2,
                   position: {9, 11},
                   func: :/,
                   mod: Kernel,
                   kind: :imported_quoted
                 }
               ],
               10 => [
                 %ElixirSense.Core.State.CallInfo{
                   arity: 2,
                   position: {10, 7},
                   func: :inspect,
                   mod: Kernel,
                   kind: :imported_quoted
                 },
                 %ElixirSense.Core.State.CallInfo{
                   arity: 1,
                   position: {10, 7},
                   func: :inspect,
                   mod: Kernel,
                   kind: :imported_quoted
                 }
               ],
               11 => [
                 %CallInfo{
                   arity: 1,
                   position: {11, 7},
                   func: :inspect,
                   mod: Kernel,
                   kind: :imported_function
                 },
                 %ElixirSense.Core.State.CallInfo{
                   arity: 2,
                   position: {11, 15},
                   mod: Kernel,
                   func: :/,
                   kind: :imported_quoted
                 },
                 %ElixirSense.Core.State.CallInfo{
                   arity: 2,
                   position: {11, 8},
                   mod: Kernel,
                   func: :inspect,
                   kind: :imported_quoted
                 },
                 %ElixirSense.Core.State.CallInfo{
                   arity: 1,
                   position: {11, 8},
                   mod: Kernel,
                   func: :inspect,
                   kind: :imported_quoted
                 }
               ],
               13 => [
                 %ElixirSense.Core.State.CallInfo{
                   arity: 2,
                   position: {13, 17},
                   func: :/,
                   mod: Kernel,
                   kind: :imported_quoted
                 }
               ]
             } = state.calls

      for i <- [5, 6, 8, 12] do
        refute Map.has_key?(state.calls, i)
      end
    end

    test "registers calls capture expression external" do
      state =
        """
        defmodule NyModule do
          def func do
            &MyMod.func(1, &1)
          end
        end
        """
        |> string_to_state

      assert %{
               3 => [
                 %CallInfo{arity: 2, position: {3, 12}, func: :func, mod: MyMod},
                 %ElixirSense.Core.State.CallInfo{
                   arity: nil,
                   position: {3, 6},
                   mod: MyMod,
                   func: nil
                 }
               ]
             } = state.calls
    end

    test "registers calls capture operator external erlang module" do
      state =
        """
        defmodule NyModule do
          def func do
            &:erl_mod.func/1
          end
        end
        """
        |> string_to_state

      assert %{
               3 => [%CallInfo{arity: 1, func: :func, position: {3, 15}, mod: :erl_mod}]
             } = state.calls
    end

    test "registers calls capture operator external atom module" do
      state =
        """
        defmodule NyModule do
          def func do
            &:"Elixir.MyMod".func/1
          end
        end
        """
        |> string_to_state

      assert %{
               3 => [%CallInfo{arity: 1, func: :func, position: {3, 22}, mod: MyMod}]
             } = state.calls
    end

    test "registers calls capture import" do
      state =
        """
        defmodule NyModule do
          import Node
          def func do
            &list/0
            &binding/0
          end
        end
        """
        |> string_to_state

      assert %{
               4 => [
                 %CallInfo{
                   arity: 0,
                   func: :list,
                   position: {4, 6},
                   mod: Node,
                   kind: :imported_function
                 }
               ],
               5 => [
                 %CallInfo{
                   arity: 0,
                   func: :binding,
                   position: {5, 6},
                   mod: Kernel,
                   kind: :imported_macro
                 }
               ]
             } = state.calls
    end

    test "registers calls capture operator local" do
      state =
        """
        defmodule NyModule do
          def foo, do: ok
          defmacro bar, do: :ok
          def func do
            &func/1
            &func/0
            &foo/0
            &bar/0
          end
        end
        """
        |> string_to_state

      assert %{
               5 => [%CallInfo{arity: 1, func: :func, position: {5, 6}, mod: NyModule}],
               6 => [%CallInfo{arity: 0, func: :func, position: {6, 6}, mod: NyModule}],
               7 => [%CallInfo{arity: 0, func: :foo, position: {7, 6}, mod: NyModule}],
               8 => [%CallInfo{arity: 0, func: :bar, position: {8, 6}, mod: NyModule}]
             } = state.calls
    end

    test "registers calls capture expression local" do
      state =
        """
        defmodule NyModule do
          def func do
            &func(1, &1)
          end
        end
        """
        |> string_to_state

      assert %{
               3 => [%CallInfo{arity: 2, func: :func, position: {3, 6}, mod: NyModule}]
             } = state.calls
    end

    test "registers calls on ex_unit DSL" do
      state =
        """
        defmodule MyModuleTests do
          use ExUnit.Case

          describe "describe1" do
            test "test1" do
            end
          end

          test "test2", %{some: param} do
          end

          test "not implemented"
        end
        """
        |> string_to_state

      assert Enum.any?(
               state.calls[2],
               &match?(%CallInfo{arity: 2, position: {2, _}, func: :__register__}, &1)
             )

      assert Enum.any?(
               state.calls[4],
               &match?(%CallInfo{arity: 2, position: {4, 3}, func: :describe}, &1)
             )

      assert Enum.any?(
               state.calls[5],
               &match?(%CallInfo{arity: 2, position: {5, 5}, func: :test}, &1)
             )

      assert Enum.any?(
               state.calls[9],
               &match?(%CallInfo{arity: 3, position: {9, 3}, func: :test}, &1)
             )

      assert Enum.any?(
               state.calls[12],
               &match?(%CallInfo{arity: 1, position: {12, 3}, func: :test}, &1)
             )
    end
  end

  describe "typespec" do
    test "registers types" do
      state =
        """
        defmodule My do
          @type no_arg_no_parens :: integer
          @typep no_args() :: integer
          @opaque with_args(a, b) :: {a, b}
          @type overloaded :: {}
          @type overloaded(a) :: {a}
        end
        IO.puts("")
        """
        |> string_to_state

      assert %{
               {My, :no_arg_no_parens, 0} => %ElixirSense.Core.State.TypeInfo{
                 args: [[]],
                 kind: :type,
                 name: :no_arg_no_parens,
                 positions: [{2, 3}],
                 end_positions: [{2, 36}],
                 generated: [false],
                 specs: ["@type no_arg_no_parens() :: integer()"]
               },
               {My, :no_args, 0} => %ElixirSense.Core.State.TypeInfo{
                 args: [[]],
                 kind: :typep,
                 name: :no_args,
                 positions: [{3, 3}],
                 end_positions: [{3, 30}],
                 generated: [false],
                 specs: ["@typep no_args() :: integer()"]
               },
               {My, :overloaded, 0} => %ElixirSense.Core.State.TypeInfo{
                 args: [[]],
                 kind: :type,
                 name: :overloaded,
                 positions: [{5, 3}],
                 end_positions: [{5, 25}],
                 generated: [false],
                 specs: ["@type overloaded() :: {}"]
               },
               {My, :overloaded, 1} => %ElixirSense.Core.State.TypeInfo{
                 kind: :type,
                 name: :overloaded,
                 positions: [{6, 3}],
                 end_positions: [_],
                 generated: [false],
                 args: [["a"]],
                 specs: ["@type overloaded(a) :: {a}"]
               },
               {My, :with_args, 2} => %ElixirSense.Core.State.TypeInfo{
                 kind: :opaque,
                 name: :with_args,
                 positions: [{4, 3}],
                 end_positions: [{4, 36}],
                 generated: [false],
                 args: [["a", "b"]],
                 meta: %{opaque: true},
                 specs: ["@opaque with_args(a, b) :: {a, b}"]
               }
             } = state.types
    end

    test "registers types with unquote fragments in body" do
      state =
        """
        defmodule My do
          kv = [foo: 1]
          Enum.each(kv, fn {k, v} ->
            @type foo :: unquote(v)
          end)
        end
        """
        |> string_to_state

      assert %{
               {My, :foo, 0} => %ElixirSense.Core.State.TypeInfo{
                 args: [[]],
                 kind: :type,
                 name: :foo,
                 positions: [{4, 5}],
                 end_positions: _,
                 generated: [false],
                 specs: ["@type foo() :: :__unknown__"]
               }
             } = state.types
    end

    test "store types as unknown when unquote fragments in call" do
      state =
        """
        defmodule My do
          kv = [foo: 1]
          Enum.each(kv, fn {k, v} ->
            @type unquote(k)() :: 123
          end)
        end
        """
        |> string_to_state

      assert %{
               {My, :__unknown__, 0} => %ElixirSense.Core.State.TypeInfo{
                 name: :__unknown__,
                 args: [[]],
                 specs: ["@type __unknown__() :: 123"],
                 kind: :type,
                 positions: [{4, 5}],
                 end_positions: [_],
                 generated: [false],
                 doc: "",
                 meta: %{hidden: true}
               }
             } = state.types
    end

    test "registers incomplete types" do
      state =
        """
        defmodule My do
          @type foo
          @type bar()
          @type baz(a)
        end
        """
        |> string_to_state

      assert %{
               {My, :foo, 0} => %ElixirSense.Core.State.TypeInfo{
                 name: :foo,
                 args: [[]],
                 specs: ["@type foo() :: nil"],
                 kind: :type,
                 positions: [{2, 3}],
                 end_positions: [{2, 12}],
                 generated: [false],
                 doc: "",
                 meta: %{}
               },
               {My, :bar, 0} => %ElixirSense.Core.State.TypeInfo{
                 name: :bar,
                 args: [[]],
                 specs: ["@type bar() :: nil"],
                 kind: :type,
                 positions: [{3, 3}],
                 end_positions: [{3, 14}],
                 generated: [false],
                 doc: "",
                 meta: %{}
               },
               {My, :baz, 1} => %ElixirSense.Core.State.TypeInfo{
                 name: :baz,
                 args: [["a"]],
                 specs: ["@type baz(a) :: nil"],
                 kind: :type,
                 positions: [{4, 3}],
                 end_positions: _,
                 generated: [false],
                 doc: "",
                 meta: %{}
               }
             } = state.types
    end

    test "protocol exports type t" do
      state =
        """
        defprotocol Proto do
          def reverse(term)
        end
        """
        |> string_to_state

      assert %{
               {Proto, :t, 0} => %ElixirSense.Core.State.TypeInfo{
                 args: [[]],
                 kind: :type,
                 name: :t,
                 specs: ["@type t() :: term()"],
                 doc: doc
               }
             } = state.types

      if Version.match?(System.version(), ">= 1.15.0") do
        assert "All the types that implement this protocol" <> _ = doc
      end
    end

    test "specs and callbacks" do
      state =
        """
        defmodule Proto do
          @spec abc :: atom | integer
          @spec abc :: reference
          @callback my(a :: integer) :: atom
          @macrocallback other(x) :: Macro.t when x: integer
        end
        """
        |> string_to_state

      # if there are callbacks behaviour_info/1 is defined
      assert state.mods_funs_to_positions[{Proto, :behaviour_info, 1}] != nil

      assert %{
               {Proto, :abc, 0} => %ElixirSense.Core.State.SpecInfo{
                 args: [[], []],
                 kind: :spec,
                 name: :abc,
                 positions: [{3, 3}, {2, 3}],
                 end_positions: [{3, 25}, {2, 30}],
                 generated: [false, false],
                 specs: ["@spec abc() :: reference()", "@spec abc() :: atom() | integer()"]
               },
               {Proto, :my, 1} => %ElixirSense.Core.State.SpecInfo{
                 kind: :callback,
                 name: :my,
                 args: [["a :: integer()"]],
                 positions: [{4, 3}],
                 end_positions: [{4, 37}],
                 generated: [false],
                 specs: ["@callback my(a :: integer()) :: atom()"]
               },
               {Proto, :other, 1} => %ElixirSense.Core.State.SpecInfo{
                 kind: :macrocallback,
                 name: :other,
                 args: [["x"]],
                 positions: [{5, 3}],
                 end_positions: [_],
                 generated: [false],
                 specs: ["@macrocallback other(x) :: Macro.t() when x: integer()"]
               }
             } = state.specs
    end

    test "registers incomplete specs" do
      state =
        """
        defmodule My do
          @spec foo
          @spec bar()
          @spec baz(number)
        end
        """
        |> string_to_state

      assert %{
               {My, :foo, 0} => %ElixirSense.Core.State.SpecInfo{
                 name: :foo,
                 args: [[]],
                 specs: ["@spec foo() :: nil"],
                 kind: :spec,
                 positions: [{2, 3}],
                 end_positions: [{2, 12}],
                 generated: [false],
                 doc: "",
                 meta: %{}
               },
               {My, :bar, 0} => %ElixirSense.Core.State.SpecInfo{
                 name: :bar,
                 args: [[]],
                 specs: ["@spec bar() :: nil"],
                 kind: :spec,
                 positions: [{3, 3}],
                 end_positions: [{3, 14}],
                 generated: [false],
                 doc: "",
                 meta: %{}
               },
               {My, :baz, 1} => %ElixirSense.Core.State.SpecInfo{
                 name: :baz,
                 args: [["number()"]],
                 specs: ["@spec baz(number()) :: nil"],
                 kind: :spec,
                 positions: [{4, 3}],
                 end_positions: _,
                 generated: [false],
                 doc: "",
                 meta: %{}
               }
             } = state.specs
    end

    test "specs and types expand aliases" do
      state =
        """
        defmodule Model.User do
          defstruct name: nil
        end

        defmodule Model.UserOrder do
          defstruct order: nil
        end

        defmodule Proto do
          alias Model.User
          alias Model.Order
          alias Model.UserOrder
          @type local_type() :: User.t
          @spec abc({%User{}}) :: {%UserOrder{order: Order.t}, local_type()}
        end
        """
        |> string_to_state

      assert %{
               {Proto, :abc, 1} => %State.SpecInfo{
                 args: [["{%Model.User{name: term()}}"]],
                 specs: [
                   "@spec abc({%Model.User{name: term()}}) :: {%Model.UserOrder{order: Model.Order.t()}, local_type()}"
                 ]
               }
             } = state.specs

      assert %{
               {Proto, :local_type, 0} => %State.TypeInfo{
                 specs: ["@type local_type() :: Model.User.t()"]
               }
             } = state.types
    end

    defmodule TypespecMacros do
      defmacro some() do
        quote do
          Foo
        end
      end
    end

    test "specs and types expand macros in remote type" do
      state =
        """
        defmodule Proto do
          require ElixirSense.Core.MetadataBuilderTest.TypespecMacros, as: TypespecMacros
          @type local_type() :: TypespecMacros.some().foo(integer())
        end
        """
        |> string_to_state

      assert %{
               {Proto, :local_type, 0} => %State.TypeInfo{
                 specs: ["@type local_type() :: Foo.foo(integer())"]
               }
             } = state.types
    end

    test "specs and types expand attributes in remote type" do
      state =
        """
        defmodule Proto do
          @some Remote.Module
          @type local_type() :: @some.foo(integer())
          IO.puts ""
        end
        """
        |> string_to_state

      assert %{
               {Proto, :local_type, 0} => %State.TypeInfo{
                 specs: ["@type local_type() :: Remote.Module.foo(integer())"]
               }
             } = state.types

      assert [
               %AttributeInfo{
                 positions: [{2, 3}, {3, 25}]
               }
             ] = state.lines_to_env[4].attributes

      assert [%CallInfo{position: {3, 3}}, %CallInfo{position: {3, 25}}] =
               state.calls[3] |> Enum.filter(&(&1.func == :@))
    end
  end

  describe "defrecord" do
    test "defrecord defines record macros" do
      state =
        """
        defmodule MyRecords do
          require Record
          Record.defrecord(:user, name: "meg", age: "25")
          @type user :: record(:user, name: String.t(), age: integer)
          Record.defrecordp(:userp, name: "meg", age: "25")
          Record.defrecord(:my_rec, Record.extract(:file_info, from_lib: "kernel/include/file.hrl")
            |> Keyword.merge(fun_field: &__MODULE__.foo/2))
          def foo(bar, baz), do: IO.inspect({bar, baz})
        end
        """
        |> string_to_state

      assert %{
               {MyRecords, :my_rec} => %RecordInfo{
                 type: :defrecord,
                 fields: []
               },
               {MyRecords, :user} => %RecordInfo{
                 type: :defrecord,
                 fields: [name: "meg", age: "25"]
               },
               {MyRecords, :userp} => %RecordInfo{
                 type: :defrecordp,
                 fields: [name: "meg", age: "25"]
               }
             } = state.records

      assert %{
               {MyRecords, :user, 1} => %ModFunInfo{
                 params: [[{:\\, [], [{:args, [], nil}, []]}]],
                 positions: [{3, 10}],
                 type: :defmacro
               },
               {MyRecords, :user, 2} => %ModFunInfo{
                 params: [[{:record, [], nil}, {:args, [], nil}]],
                 positions: [{3, 10}],
                 type: :defmacro
               },
               {MyRecords, :userp, 1} => %ModFunInfo{type: :defmacrop},
               {MyRecords, :my_rec, 1} => %ModFunInfo{type: :defmacro}
             } = state.mods_funs_to_positions

      assert %{
               {MyRecords, :user, 0} => %State.TypeInfo{
                 name: :user,
                 specs: ["@type user() :: record(:user, name: String.t(), age: integer())"]
               }
             } = state.types
    end

    test "defrecord imported defines record macros" do
      state =
        """
        defmodule MyRecords do
          import Record
          defrecord(:user, name: "meg", age: "25")
          @type user :: record(:user, name: String.t(), age: integer)
        end
        """
        |> string_to_state

      assert %{
               {MyRecords, :user, 1} => %ModFunInfo{
                 params: [[{:\\, [], [{:args, [], nil}, []]}]],
                 positions: [{3, 3}],
                 type: :defmacro
               },
               {MyRecords, :user, 2} => %ModFunInfo{
                 params: [[{:record, [], nil}, {:args, [], nil}]],
                 positions: [{3, 3}],
                 type: :defmacro
               }
             } = state.mods_funs_to_positions

      assert %{
               {MyRecords, :user, 0} => %State.TypeInfo{
                 name: :user,
                 specs: ["@type user() :: record(:user, name: String.t(), age: integer())"]
               }
             } = state.types
    end

    test "defrecord consumes doc and meta" do
      state =
        """
        defmodule MyRecords do
          import Record
          @doc "User record"
          @doc since: "1.0.0"
          defrecord(:user, name: "meg", age: "25")
        end
        """
        |> string_to_state

      assert %ModFunInfo{
               type: :defmacro,
               doc: "User record",
               meta: %{since: "1.0.0"}
             } = state.mods_funs_to_positions[{MyRecords, :user, 1}]

      assert %RecordInfo{
               meta: %{since: "1.0.0"},
               type: :defrecord,
               doc: "User record",
               fields: [name: "meg", age: "25"]
             } = state.records[{MyRecords, :user}]
    end
  end

  test "gets ExUnit imports from `use ExUnit.Case`" do
    state =
      """
      defmodule MyModuleTest do
        use ExUnit.Case
        IO.puts ""
      end
      """
      |> string_to_state

    {functions, _} = get_line_imports(state, 3)
    assert ExUnit.Assertions in Keyword.keys(functions)
  end

  test "gets ExUnit imports from case template" do
    state =
      """
      defmodule My1Test do
        use ElixirSenseExample.CaseTemplateExample
        IO.puts ""
      end
      """
      |> string_to_state

    {functions, _} = get_line_imports(state, 3)
    assert ExUnit.Assertions in Keyword.keys(functions)
  end

  test "safely skip code inside `quote do`" do
    state =
      """
      defmodule My do
        quote do
          defmodule Some do
          end
        end
        quote unquote: false do
          defmodule Some do
          end
        end
        quote(do: (defmodule Some, do: :ok))
        quote([location: :keep], do: (defmodule Some, do: :ok))
      end
      """
      |> string_to_state

    refute Map.has_key?(state.mods_funs_to_positions, {My.Some, nil, nil})
  end

  describe "defoverridable" do
    test "extract info about overridable defs" do
      state =
        """
        defmodule My do
          use ElixirSenseExample.OverridableFunctions
        end
        """
        |> string_to_state

      assert %{
               {My, :required, 1} => %ModFunInfo{
                 params: [[{:var, _, _}]],
                 positions: [{2, _}],
                 target: nil,
                 type: :defmacro,
                 overridable: {true, ElixirSenseExample.OverridableFunctions}
               },
               {My, :test, 2} => %ModFunInfo{
                 params: [[{:x, _, _}, {:y, _, _}]],
                 positions: [{2, _}],
                 target: nil,
                 type: :def,
                 overridable: {true, ElixirSenseExample.OverridableFunctions}
               }
             } = state.mods_funs_to_positions
    end

    test "extract info about overridable behaviour callbacks" do
      state =
        """
        defmodule My do
          use ElixirSenseExample.OverridableImplementation
        end
        """
        |> string_to_state

      assert %{
               {My, :foo, 0} => %ModFunInfo{
                 params: [[]],
                 positions: [{2, _}],
                 target: nil,
                 type: :def,
                 overridable: {true, ElixirSenseExample.OverridableImplementation}
               },
               {My, :bar, 1} => %ModFunInfo{
                 params: [[{:var, _, _}]],
                 positions: [{2, _}],
                 target: nil,
                 type: :defmacro,
                 overridable: {true, ElixirSenseExample.OverridableImplementation}
               }
             } = state.mods_funs_to_positions
    end

    test "override defs" do
      state =
        """
        defmodule My do
          use ElixirSenseExample.OverridableFunctions

          def test(a, b) do
            a * b
          end

          defmacro required(baz), do: baz
        end
        """
        |> string_to_state

      assert %{
               {My, :required, 1} => %ModFunInfo{
                 params: [
                   [{:baz, _, nil}],
                   [{:var, _, _}]
                 ],
                 positions: [{8, 3}, {2, _}],
                 target: nil,
                 type: :defmacro,
                 overridable: {true, ElixirSenseExample.OverridableFunctions}
               },
               {My, :test, 2} => %ModFunInfo{
                 params: [
                   [{:a, _, nil}, {:b, _, nil}],
                   [{:x, _, _}, {:y, _, _}]
                 ],
                 positions: [{4, 3}, {2, _}],
                 target: nil,
                 type: :def,
                 overridable: {true, ElixirSenseExample.OverridableFunctions}
               }
             } = state.mods_funs_to_positions
    end

    test "override behaviour callbacks" do
      state =
        """
        defmodule My do
          use ElixirSenseExample.OverridableImplementation

          def foo do
            ""
          end

          defmacro bar(baz), do: baz
        end
        """
        |> string_to_state

      assert %{
               {My, :foo, 0} => %ModFunInfo{
                 params: [[], []],
                 positions: [{4, 3}, {2, _}],
                 target: nil,
                 type: :def,
                 overridable: {true, ElixirSenseExample.OverridableImplementation}
               },
               {My, :bar, 1} => %ModFunInfo{
                 params: [
                   [{:baz, _, nil}],
                   [{:var, _, _}]
                 ],
                 positions: [{8, 3}, {2, _}],
                 target: nil,
                 type: :defmacro,
                 overridable: {true, ElixirSenseExample.OverridableImplementation}
               }
             } = state.mods_funs_to_positions
    end

    test "override defs changes type" do
      state =
        """
        defmodule My do
          use ElixirSenseExample.OverridableFunctions

          defp test(a, b) do
            a * b
          end

          defmacrop required(baz), do: baz
        end
        """
        |> string_to_state

      assert %{
               {My, :required, 1} => %ModFunInfo{
                 params: [
                   [{:baz, _, nil}],
                   [{:var, _, _}]
                 ],
                 positions: [{8, 3}, {2, _}],
                 target: nil,
                 type: :defmacrop,
                 overridable: {true, ElixirSenseExample.OverridableFunctions}
               },
               {My, :test, 2} => %ModFunInfo{
                 params: [
                   [{:a, _, nil}, {:b, _, nil}],
                   [{:x, _, _}, {:y, _, _}]
                 ],
                 positions: [{4, 3}, {2, _}],
                 target: nil,
                 type: :defp,
                 overridable: {true, ElixirSenseExample.OverridableFunctions}
               }
             } = state.mods_funs_to_positions
    end
  end

  test "scopes" do
    state =
      """
      IO.puts ""
      defmodule My do
        @attr "asd"

        @type a :: integer

        @attr1 "cc"

        defmodule B do
          IO.puts ""
        end

        IO.puts ""

        @spec test(integer, integer) :: integer
        defp test(a, b) do
          a * b
        end

        @attr2 "gd"
      end
      IO.puts ""
      """
      |> string_to_state

    assert nil == get_line_typespec(state, 1)
    assert nil == get_line_function(state, 1)
    assert nil == get_line_module(state, 1)

    assert nil == get_line_typespec(state, 2)
    assert nil == get_line_function(state, 2)
    assert My == get_line_module(state, 2)

    assert {:a, 0} == get_line_typespec(state, 5)
    assert nil == get_line_function(state, 5)
    assert My == get_line_module(state, 5)

    assert nil == get_line_typespec(state, 7)
    assert nil == get_line_function(state, 7)
    assert My == get_line_module(state, 7)

    assert nil == get_line_typespec(state, 9)
    assert nil == get_line_function(state, 9)
    assert My.B == get_line_module(state, 9)

    assert nil == get_line_typespec(state, 13)
    assert nil == get_line_function(state, 13)
    assert My == get_line_module(state, 13)

    assert {:test, 2} == get_line_typespec(state, 15)
    assert nil == get_line_function(state, 15)
    assert My == get_line_module(state, 15)

    assert nil == get_line_typespec(state, 16)
    assert {:test, 2} == get_line_function(state, 16)
    assert My == get_line_module(state, 16)

    assert nil == get_line_typespec(state, 17)
    assert {:test, 2} == get_line_function(state, 17)
    assert My == get_line_module(state, 17)

    assert nil == get_line_typespec(state, 20)
    assert nil == get_line_function(state, 20)
    assert My == get_line_module(state, 20)

    assert nil == get_line_typespec(state, 22)
    assert nil == get_line_function(state, 22)
    assert nil == get_line_module(state, 22)
  end

  test "invalid def" do
    _state =
      """
      def MetadataProtocol, for: BitString do
      end
      """
      |> string_to_state
  end

  describe "doc" do
    test "moduledoc is applied to current module" do
      state =
        """
        defmodule Some do
          @moduledoc "Some module"
          @moduledoc since: "1.2.3"

          defmodule NoDoc do
          end

          defmodule Sub do
            @moduledoc "Some.Sub module"
            @moduledoc deprecated: "2.3.4"
          end
        end

        defmodule Other do
          @moduledoc "Other module"
        end

        defmodule NoDoc do
        end
        """
        |> string_to_state

      assert %{doc: "Some module", meta: %{since: "1.2.3"}} =
               state.mods_funs_to_positions[{Some, nil, nil}]

      assert %{doc: "", meta: %{}} = state.mods_funs_to_positions[{Some.NoDoc, nil, nil}]

      assert %{doc: "Some.Sub module", meta: %{deprecated: "2.3.4"}} =
               state.mods_funs_to_positions[{Some.Sub, nil, nil}]

      assert %{doc: "Other module", meta: %{}} = state.mods_funs_to_positions[{Other, nil, nil}]
      assert %{doc: "", meta: %{}} = state.mods_funs_to_positions[{NoDoc, nil, nil}]
    end

    test "moduledoc handles charlist" do
      state =
        """
        defmodule Some do
          @moduledoc 'Some module'
        end
        """
        |> string_to_state

      assert %{doc: "Some module"} = state.mods_funs_to_positions[{Some, nil, nil}]
    end

    test "moduledoc handles interpolated charlist" do
      state =
        """
        defmodule Some do
          @moduledoc 'Some #{inspect(1)} module'
        end
        """
        |> string_to_state

      assert %{doc: "Some 1 module"} = state.mods_funs_to_positions[{Some, nil, nil}]
    end

    test "moduledoc handles interpolated string" do
      state =
        """
        defmodule Some do
          @moduledoc \"Some #{inspect(1)} module\"
        end
        """
        |> string_to_state

      assert %{doc: "Some 1 module"} = state.mods_funs_to_positions[{Some, nil, nil}]
    end

    test "moduledoc handles heredoc" do
      state =
        """
        defmodule Some do
          @moduledoc \"\"\"
          Some module
          \"\"\"
        end
        """
        |> string_to_state

      assert %{doc: "Some module\n"} = state.mods_funs_to_positions[{Some, nil, nil}]
    end

    test "moduledoc handles charlist heredoc" do
      state =
        """
        defmodule Some do
          @moduledoc '''
          Some module
          '''
        end
        """
        |> string_to_state

      assert %{doc: "Some module\n"} = state.mods_funs_to_positions[{Some, nil, nil}]
    end

    test "moduledoc handles sigil" do
      state =
        """
        defmodule Some do
          @moduledoc ~S\"\"\"
          Some module
          \"\"\"
        end
        """
        |> string_to_state

      assert %{doc: "Some module\n"} = state.mods_funs_to_positions[{Some, nil, nil}]
    end

    test "moduledoc false is applied to current module" do
      state =
        """
        defmodule Some do
          @moduledoc false
        end
        """
        |> string_to_state

      assert %{doc: "", meta: %{hidden: true}} = state.mods_funs_to_positions[{Some, nil, nil}]
    end

    test "doc is applied to next function" do
      state =
        """
        defmodule Some do
          @doc "Some fun"
          @doc since: "1.2.3"
          def fun() do
            :ok
          end

          def fun_nodoc() do
            :ok
          end

          @doc "Some macro"
          @doc deprecated: "2.3.4"
          defmacro macro() do
            :ok
          end
        end
        """
        |> string_to_state

      assert %{doc: "Some fun", meta: %{since: "1.2.3"}} =
               state.mods_funs_to_positions[{Some, :fun, 0}]

      assert %{doc: "", meta: %{}} = state.mods_funs_to_positions[{Some, :fun_nodoc, 0}]

      assert %{doc: "Some macro", meta: %{deprecated: "2.3.4"}} =
               state.mods_funs_to_positions[{Some, :macro, 0}]
    end

    test "doc is applied to next delegate" do
      state =
        """
        defmodule Some do
          @doc "Some fun"
          @doc since: "1.2.3"
          defdelegate count(a), to: Enum
        end
        """
        |> string_to_state

      assert %{doc: "Some fun", meta: %{since: "1.2.3"}} =
               state.mods_funs_to_positions[{Some, :count, 1}]
    end

    test "doc is applied to next guard" do
      state =
        """
        defmodule Some do
          @doc "Some fun"
          @doc since: "1.2.3"
          defguard foo(a) when is_integer(a) 
        end
        """
        |> string_to_state

      assert %{doc: "Some fun", meta: %{since: "1.2.3"}} =
               state.mods_funs_to_positions[{Some, :foo, 1}]
    end

    test "doc false is applied to next function" do
      state =
        """
        defmodule Some do
          @doc false
          def fun(), do: :ok
        end
        """
        |> string_to_state

      assert %{doc: "", meta: %{hidden: true}} = state.mods_funs_to_positions[{Some, :fun, 0}]
    end

    test "doc on private is discarded" do
      state =
        """
        defmodule Some do
          @doc "Some"
          defp fun(), do: :ok
        end
        """
        |> string_to_state

      assert %{doc: ""} = state.mods_funs_to_positions[{Some, :fun, 0}]
    end

    test "impl true sets hidden meta if no doc" do
      state =
        """
        defmodule Some do
          @impl true
          def fun(), do: :ok

          @doc "Some"
          @impl true
          def fun_with_doc(), do: :ok
        end
        """
        |> string_to_state

      assert %{doc: "", meta: %{hidden: true}} = state.mods_funs_to_positions[{Some, :fun, 0}]
      assert %{doc: "Some", meta: meta} = state.mods_funs_to_positions[{Some, :fun_with_doc, 0}]
      refute match?(%{hidden: true}, meta)
    end

    test "underscored def sets hidden meta if no doc" do
      state =
        """
        defmodule Some do
          def _fun(), do: :ok

          @doc "Some"
          def _fun_with_doc(), do: :ok
        end
        """
        |> string_to_state

      assert %{doc: "", meta: %{hidden: true}} = state.mods_funs_to_positions[{Some, :_fun, 0}]

      assert %{doc: "Some", meta: meta} =
               state.mods_funs_to_positions[{Some, :_fun_with_doc, 0}]

      refute match?(%{hidden: true}, meta)
    end

    test "deprecated attribute sets deprecated meta" do
      state =
        """
        defmodule Some do
          @deprecated "to be removed"
          def fun(), do: :ok
        end
        """
        |> string_to_state

      assert %{doc: "", meta: %{deprecated: "to be removed"}} =
               state.mods_funs_to_positions[{Some, :fun, 0}]
    end

    test "doc is applied to next callback" do
      state =
        """
        defmodule Some do
          @doc "Some fun"
          @doc since: "1.2.3"
          @callback fun() :: any()

          @callback fun_nodoc() :: any()

          @doc "Some macro"
          @doc deprecated: "2.3.4"
          @macrocallback macro() :: Macro.t()
        end
        """
        |> string_to_state

      assert %{doc: "Some fun", meta: %{since: "1.2.3"}} = state.specs[{Some, :fun, 0}]
      assert %{doc: "", meta: %{}} = state.specs[{Some, :fun_nodoc, 0}]
      assert %{doc: "Some macro", meta: %{deprecated: "2.3.4"}} = state.specs[{Some, :macro, 0}]
    end

    test "underscored callback sets hidden meta if no doc" do
      state =
        """
        defmodule Some do
          @callback _fun() :: any()

          @doc "Some"
          @callback _fun_with_doc() :: any()
        end
        """
        |> string_to_state

      assert %{doc: "", meta: %{hidden: true}} = state.specs[{Some, :_fun, 0}]
      assert %{doc: "Some", meta: meta} = state.specs[{Some, :_fun_with_doc, 0}]
      refute match?(%{hidden: true}, meta)
    end

    test "typedoc is applied to next type" do
      state =
        """
        defmodule Some do
          @typedoc "Some type"
          @typedoc since: "1.2.3"
          @type my_type() :: any()

          @type type_nodoc() :: any()

          @typedoc "Some opaque"
          @typedoc deprecated: "2.3.4"
          @opaque my_opaque() :: integer()
        end
        """
        |> string_to_state

      assert %{doc: "Some type", meta: %{since: "1.2.3"}} = state.types[{Some, :my_type, 0}]
      assert %{doc: "", meta: %{}} = state.types[{Some, :type_nodoc, 0}]

      assert %{doc: "Some opaque", meta: %{deprecated: "2.3.4"}} =
               state.types[{Some, :my_opaque, 0}]
    end

    test "typedoc false is applied to next type" do
      state =
        """
        defmodule Some do
          @typedoc false
          @type my_type() :: any()
        end
        """
        |> string_to_state

      assert %{doc: "", meta: %{hidden: true}} = state.types[{Some, :my_type, 0}]
    end

    test "typedoc is discarded on private" do
      state =
        """
        defmodule Some do
          @typedoc "Some"
          @typep my_type() :: any()
        end
        """
        |> string_to_state

      assert %{doc: ""} = state.types[{Some, :my_type, 0}]
    end

    test "underscored type sets hidden meta when there is no typedoc" do
      state =
        """
        defmodule Some do
          @type _my_type() :: any()

          @typedoc "Some"
          @type _my_type_with_doc() :: any()
        end
        """
        |> string_to_state

      assert %{doc: "", meta: %{hidden: true}} = state.types[{Some, :_my_type, 0}]
      assert %{doc: "Some", meta: meta} = state.types[{Some, :_my_type_with_doc, 0}]
      refute match?(%{hidden: true}, meta)
    end
  end

  describe "meta" do
    test "behaviours" do
      state =
        """
        defmodule Some do
          @behaviour Other
        end
        """
        |> string_to_state

      assert %{meta: %{behaviours: [Other]}} =
               state.mods_funs_to_positions[{Some, nil, nil}]
    end

    test "guard" do
      state =
        """
        defmodule Some do
          defguard fun(a) when a == 1
        end
        """
        |> string_to_state

      assert %{meta: %{guard: true}} =
               state.mods_funs_to_positions[{Some, :fun, 1}]
    end

    test "delegate" do
      state =
        """
        defmodule Some do
          defdelegate count(a), to: Enum
        end
        """
        |> string_to_state

      assert %{meta: %{delegate_to: {Enum, :count, 1}}} =
               state.mods_funs_to_positions[{Some, :count, 1}]
    end

    test "opaque" do
      state =
        """
        defmodule Some do
          @opaque my_type() :: any()
        end
        """
        |> string_to_state

      assert %{meta: %{opaque: true}} = state.types[{Some, :my_type, 0}]
    end

    test "optional" do
      state =
        """
        defmodule Some do
          @callback some(any) :: any
          @optional_callbacks some: 1
        end
        """
        |> string_to_state

      assert %{meta: %{optional: true}} = state.specs[{Some, :some, 1}]
    end

    test "overridable" do
      state =
        """
        defmodule Some do
          use ElixirSenseExample.OverridableFunctions
        end
        """
        |> string_to_state

      assert %{meta: %{overridable: true}} = state.mods_funs_to_positions[{Some, :test, 2}]
    end
  end

  test "no endless loop on use variable" do
    state =
      """
      defmodule Some do
        use
        @spec my(number()) :: number()
        def my(abc) do
          abc + 1
        end
      end
      """
      |> string_to_state

    assert state
  end

  describe "module callbacks" do
    defmodule Callbacks do
      defmacro __before_compile__(_arg) do
        quote do
          def constant, do: 1
          defoverridable constant: 0
        end
      end
    end

    test "before_compile" do
      state =
        """
        defmodule User do
          @before_compile ElixirSense.Core.MetadataBuilderTest.Callbacks
        end
        """
        |> string_to_state

      assert %ModFunInfo{meta: %{overridable: true}} =
               state.mods_funs_to_positions[{User, :constant, 0}]
    end
  end

  defp string_to_state(string) do
    string
    |> Code.string_to_quoted(columns: true, token_metadata: true)
    |> (fn {:ok, ast} -> ast end).()
    |> MetadataBuilder.build()
  end

  defp get_line_vars(state, line) do
    case state.lines_to_env[line] do
      nil ->
        []

      env ->
        env.vars
    end
    |> Enum.sort()
  end

  defp get_line_aliases(state, line) do
    case state.lines_to_env[line] do
      nil -> []
      env -> env.aliases
    end
  end

  defp get_line_imports(state, line) do
    case state.lines_to_env[line] do
      nil -> {[], []}
      env -> {env.functions, env.macros}
    end
  end

  defp get_line_requires(state, line) do
    case state.lines_to_env[line] do
      nil -> []
      env -> env.requires
    end
  end

  defp get_line_attributes(state, line) do
    case state.lines_to_env[line] do
      nil -> []
      env -> env.attributes
    end
    |> Enum.sort()
  end

  defp get_line_behaviours(state, line) do
    case state.lines_to_env[line] do
      nil -> []
      env -> env.behaviours
    end
    |> Enum.sort()
  end

  defp get_line_module(state, line) do
    if env = state.lines_to_env[line] do
      env.module
    end
  end

  defp get_line_typespec(state, line) do
    if env = state.lines_to_env[line] do
      env.typespec
    end
  end

  defp get_line_function(state, line) do
    if env = state.lines_to_env[line] do
      env.function
    end
  end

  defp get_line_protocol(state, line) do
    if env = state.lines_to_env[line] do
      env.protocol
    end
  end

  defp get_subject_definition_line(module, func, arity) do
    file = module.module_info(:compile)[:source]

    {:ok, ast} =
      File.read!(file)
      |> Code.string_to_quoted(columns: true, token_metadata: true)

    acc = MetadataBuilder.build(ast)

    %{positions: positions} = Map.get(acc.mods_funs_to_positions, {module, func, arity})
    {line_number, _col} = List.last(positions)

    File.read!(file) |> Source.split_lines() |> Enum.at(line_number - 1)
  end

  @tag requires_source: true
  test "all elixir modules" do
    elixir_sense_src_path =
      MetadataBuilder.module_info(:compile)[:source]
      |> Path.join("../../../..")
      |> Path.expand()
      |> ls_r

    elixir_src_path =
      Enum.module_info(:compile)[:source]
      |> Path.join("../../..")
      |> Path.expand()
      |> ls_r

    for path <- elixir_sense_src_path ++ elixir_src_path do
      case File.read!(path)
           |> Code.string_to_quoted(columns: true, token_metadata: true) do
        {:ok, ast} -> MetadataBuilder.build(ast)
        _ -> :ok
      end
    end
  end

  def ls_r(path \\ ".") do
    cond do
      File.regular?(path) ->
        [path]

      File.dir?(path) ->
        File.ls!(path)
        |> Enum.map(&Path.join(path, &1))
        |> Enum.map(&ls_r/1)
        |> Enum.concat()

      true ->
        []
    end
    # .eex?
    |> Enum.filter(&(String.ends_with?(&1, ".ex") or String.ends_with?(&1, ".exs")))
  end

  defp maybe_reject_typespec(requires) do
    if Version.match?(System.version(), ">= 1.17.0") do
      requires -- [Kernel.Typespec]
    else
      requires
    end
  end
end
