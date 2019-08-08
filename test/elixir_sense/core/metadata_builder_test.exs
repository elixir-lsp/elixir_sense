defmodule ElixirSense.Core.MetadataBuilderTest do

  use ExUnit.Case

  alias ElixirSense.Core.MetadataBuilder
  alias ElixirSense.Core.State
  alias ElixirSense.Core.State.VarInfo

  @tag requires_source: true
  test "build metadata from kernel.ex" do
    assert get_subject_definition_line(Kernel, :defmodule, nil) =~ "defmacro defmodule(alias, do: block) do"
  end

  @tag requires_source: true
  test "build metadata from kernel/special_forms.ex" do
    assert get_subject_definition_line(Kernel.SpecialForms, :alias, nil) =~ "defmacro alias(module, opts)"
  end

  test "build_metadata from a module" do
    assert get_subject_definition_line(ElixirSenseExample.ModuleWithFunctions, :function_arity_zero, nil) =~ "def function_arity_zero"
  end

  test "module attributes" do
    state = """
      defmodule MyModule do
        @myattribute 1
        IO.puts @myattribute
        defmodule InnerModule do
          @inner_attr module_var
          IO.puts @inner_attr
        end
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_attributes(state, 3) == [:myattribute]
    assert get_line_attributes(state, 6) == [:inner_attr]
    assert get_line_attributes(state, 8) == [:myattribute]
  end

  test "module attributes duplicated" do
    state = """
      defmodule MyModule do
        @myattribute 1
        @myattribute 2
        IO.puts @myattribute
      end
      """
      |> string_to_state

    assert get_line_attributes(state, 4) == [:myattribute]
  end

  test "vars defined inside a function without params" do
    state = """
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

    vars = state |> get_line_vars(6)
    assert vars == [
      %VarInfo{name: :var_in1, positions: [{4, 5}], scope_id: 3},
      %VarInfo{name: :var_in2, positions: [{5, 5}], scope_id: 3},
    ]
  end

  test "vars defined inside a function `after`/`rescue`/`catch`" do
    state = """
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

    vars = state |> get_line_vars(6)
    assert vars == [
      %VarInfo{name: :var_arg, positions: [{3, 12}], scope_id: 2},
      %VarInfo{name: :var_in1, positions: [{4, 5}], scope_id: 3},
      %VarInfo{name: :var_in2, positions: [{5, 5}], scope_id: 3},
    ]

    vars = state |> get_line_vars(9)
    assert vars == [
      %VarInfo{name: :var_after, positions: [{8, 5}], scope_id: 4},
      %VarInfo{name: :var_arg, positions: [{3, 12}], scope_id: 2},
    ]
  end

  test "vars defined inside a function with params" do

    state = """
      defmodule MyModule do
        var_out1 = 1
        def func(%{key1: par1, key2: [par2|[par3, _]]}, par4) do
          var_in1 = 1
          var_in2 = 1
          IO.puts ""
        end
        defp func1(arg), do: arg + 1
        var_out2 = 1
      end
      """
      |> string_to_state

    vars = state |> get_line_vars(6)
    assert vars == [
      %VarInfo{name: :par1, positions: [{3, 20}], scope_id: 2},
      %VarInfo{name: :par2, positions: [{3, 33}], scope_id: 2},
      %VarInfo{name: :par3, positions: [{3, 39}], scope_id: 2},
      %VarInfo{name: :par4, positions: [{3, 51}], scope_id: 2},
      %VarInfo{name: :var_in1, positions: [{4, 5}], scope_id: 3},
      %VarInfo{name: :var_in2, positions: [{5, 5}], scope_id: 3},
    ]

    vars = state |> get_line_vars(8)
    assert vars == [
      %VarInfo{name: :arg, positions: [{8, 14}], scope_id: 2},
    ]
  end

  test "rebinding vars" do

    state = """
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
    assert vars == [%VarInfo{name: :var1, positions: [{3, 19}, {3, 37}, {4, 5}, {5, 5}], scope_id: 3}]
  end

  test "vars defined inside a module" do

    state =
      """
      defmodule MyModule do
        var_out1 = 1
        def func do
          var_in = 1
        end
        var_out2 = 1
        IO.puts ""
      end
      """
      |> string_to_state

    vars = state |> get_line_vars(7)
    assert vars == [
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
      %VarInfo{name: :var_out2, positions: [{6, 3}], scope_id: 2},
    ]
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

    assert get_line_vars(state, 3) == [
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
    ]
    assert get_line_vars(state, 6) == [
      %VarInfo{name: :var_in, positions: [{5, 5}], scope_id: 4},
      %VarInfo{name: :var_on, positions: [{4, 7}, {4, 24}, {4, 47}], scope_id: 3},
      %VarInfo{name: :var_on1, positions: [{4, 37}], scope_id: 3},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
    ]
    assert get_line_vars(state, 9) == [
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
      %VarInfo{name: :var_out2, positions: [{8, 3}], scope_id: 2},
    ]
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
          IO.puts ""
        end
        var_out2 = 1
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_vars(state, 5) == [
      %VarInfo{name: :var_in_if, positions: [{4, 5}], scope_id: 3},
      %VarInfo{name: :var_on, positions: [{3, 6}], scope_id: 2},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
    ]
    assert get_line_vars(state, 8) == [
      %VarInfo{name: :var_in_else, positions: [{7, 5}], scope_id: 4},
      %VarInfo{name: :var_on, positions: [{3, 6}], scope_id: 2},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
    ]

    assert get_line_vars(state, 11) == [
      %VarInfo{name: :var_on, positions: [{3, 6}], scope_id: 2},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
      %VarInfo{name: :var_out2, positions: [{10, 3}], scope_id: 2}
    ]
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

    assert get_line_vars(state, 5) == [
      %VarInfo{name: :var_in, positions: [{4, 5}], scope_id: 4},
      %VarInfo{name: :var_on, positions: [{3, 6}], scope_id: 4},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
    ]
    assert get_line_vars(state, 8) == [
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
      %VarInfo{name: :var_out2, positions: [{7, 3}], scope_id: 2},
    ]
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

    assert get_line_vars(state, 6) == [
      %VarInfo{name: :var_in1, positions: [{5, 7}], scope_id: 4},
      %VarInfo{name: :var_on0, positions: [{3, 8}], scope_id: 2},
      %VarInfo{name: :var_on1, positions: [{4, 6}], scope_id: 4},
      %VarInfo{name: :var_out1, positions: [{2, 3}, {3, 18}], scope_id: 2}
    ]
    assert get_line_vars(state, 9) == [
      %VarInfo{name: :var_in2, positions: [{8, 7}], scope_id: 5},
      %VarInfo{name: :var_on0, positions: [{3, 8}], scope_id: 2},
      %VarInfo{name: :var_on2, positions: [{7, 6}], scope_id: 5},
      %VarInfo{name: :var_out1, positions: [{2, 3}, {3, 18}], scope_id: 2},
    ]
    assert get_line_vars(state, 10) == [
      %VarInfo{name: :var_on0, positions: [{3, 8}], scope_id: 2},
      %VarInfo{name: :var_on3, positions: [{10, 5}], scope_id: 6},
      %VarInfo{name: :var_out1, positions: [{2, 3}, {3, 18}], scope_id: 2},
    ]

    assert get_line_vars(state, 13) == [
      %VarInfo{name: :var_on0, positions: [{3, 8}], scope_id: 2},
      %VarInfo{name: :var_out1, positions: [{2, 3}, {3, 18}], scope_id: 2},
      %VarInfo{name: :var_out2, positions: [{12, 3}], scope_id: 2},
    ]
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

    assert get_line_vars(state, 6) == [
      %VarInfo{name: :var_in, positions: [{5, 7}], scope_id: 4},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
    ]

    assert get_line_vars(state, 8) == [
      %VarInfo{name: :var_in1, positions: [{7, 5}], scope_id: 5},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
    ]

    assert get_line_vars(state, 9) == [
      %VarInfo{name: :var_in2, positions: [{9, 5}], scope_id: 6},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
    ]

    assert get_line_vars(state, 12) == [
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
      %VarInfo{name: :var_out2, positions: [{11, 3}], scope_id: 2},
    ]
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

    assert get_line_vars(state, 5) == [
      %VarInfo{name: :var_in_try, positions: [{4, 5}], scope_id: 4},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
    ]

    assert get_line_vars(state, 7) == [
      %VarInfo{name: :e1, positions: [{7, 5}], scope_id: 6},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
    ]

    assert get_line_vars(state, 10) == [
      %VarInfo{name: :e2, positions: [{8, 5}], scope_id: 7},
      %VarInfo{name: :var_in_rescue, positions: [{9, 7}], scope_id: 7},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
    ]

    assert get_line_vars(state, 12) == [
      %VarInfo{name: :reason1, positions: [{12, 12}], scope_id: 9},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
    ]

    assert get_line_vars(state, 15) == [
      %VarInfo{name: :reason2, positions: [{13, 5}], scope_id: 10},
      %VarInfo{name: :var_in_catch, positions: [{14, 7}], scope_id: 10},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
    ]

    assert get_line_vars(state, 17) == [
      %VarInfo{name: :var_on_else, positions: [{17, 13}], scope_id: 12},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
    ]

    assert get_line_vars(state, 20) == [
      %VarInfo{name: :var_in_else, positions: [{19, 7}], scope_id: 13},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
    ]

    assert get_line_vars(state, 23) == [
      %VarInfo{name: :var_in_after, positions: [{22, 5}], scope_id: 14},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
    ]

    assert get_line_vars(state, 26) == [
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
      %VarInfo{name: :var_out2, positions: [{25, 3}], scope_id: 2}
    ]

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

    assert get_line_vars(state, 4) == [
      %VarInfo{name: :msg1, positions: [{4, 13}], scope_id: 4},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
    ]

    assert get_line_vars(state, 7) == [
      %VarInfo{name: :var_in, positions: [{6, 7}], scope_id: 5},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
    ]

    assert get_line_vars(state, 11) == [
      %VarInfo{name: :var_in_after, positions: [{10, 7}], scope_id: 7},
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
    ]

    assert get_line_vars(state, 14) == [
      %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
      %VarInfo{name: :var_out2, positions: [{13, 3}], scope_id: 2},
    ]
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

    assert get_line_vars(state, 3) == [
      %VarInfo{name: :mynode, positions: [{3, 3}], scope_id: 2},
      %VarInfo{name: :myself, positions: [{2, 3}], scope_id: 2},
    ]
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

    assert get_line_vars(state, 2)  == [
      %VarInfo{name: :top_level_var, positions: [{1, 1}], scope_id: 0},
    ]
    assert get_line_vars(state, 5)  == [
      %VarInfo{name: :outer_module_var, positions: [{4, 3}], scope_id: 2},
      %VarInfo{name: :top_level_var, positions: [{1, 1}], scope_id: 0},
    ]
    assert get_line_vars(state, 8)  == [
      %VarInfo{name: :inner_module_var, positions: [{7, 5}], scope_id: 4},
      %VarInfo{name: :outer_module_var, positions: [{4, 3}], scope_id: 2},
      %VarInfo{name: :top_level_var, positions: [{1, 1}], scope_id: 0},
    ]
    assert get_line_vars(state, 11) == [
      %VarInfo{name: :func_var, positions: [{10, 7}], scope_id: 5},
    ]
    assert get_line_vars(state, 13) == [
      %VarInfo{name: :inner_module_var, positions: [{7, 5}], scope_id: 4},
      %VarInfo{name: :outer_module_var, positions: [{4, 3}], scope_id: 2},
      %VarInfo{name: :top_level_var, positions: [{1, 1}], scope_id: 0},
    ]
    assert get_line_vars(state, 15) == [
      %VarInfo{name: :outer_module_var, positions: [{4, 3}], scope_id: 2},
      %VarInfo{name: :top_level_var, positions: [{1, 1}], scope_id: 0},
    ]
    assert get_line_vars(state, 17) == [
      %VarInfo{name: :top_level_var, positions: [{1, 1}], scope_id: 0},
    ]
  end

  test "aliases" do

    state =
      """
      defmodule OuterModule do
        alias List, as: MyList
        IO.puts ""
        defmodule InnerModule do
          alias Enum, as: MyEnum
          IO.puts ""
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
        alias Code, as: MyCode
        IO.puts ""
        defmodule AnotherInnerModule do
          IO.puts ""
        end
        IO.puts ""
      end
      IO.puts ""
      """
      |> string_to_state

    assert get_line_aliases(state, 3)  == [{MyList, List}]
    assert get_line_aliases(state, 6)  == [{MyList, List}, {MyEnum, Enum}]
    assert get_line_aliases(state, 9)  == [{MyList, List}, {MyEnum, Enum}, {MyString, String}]
    assert get_line_aliases(state, 12) == [{MyList, List}, {MyEnum, Enum}, {MyString, String}, {MyMacro, Macro}]
    assert get_line_aliases(state, 14) == [{MyList, List}, {MyEnum, Enum}, {MyString, String}]
    assert get_line_aliases(state, 16) == [{MyList, List}, {MyEnum, Enum}]
    assert get_line_aliases(state, 19) == [{MyList, List}, {MyCode, Code}]
    assert get_line_aliases(state, 21) == [{MyList, List}, {MyCode, Code}]
    assert get_line_aliases(state, 23) == [{MyList, List}, {MyCode, Code}]
    assert get_line_aliases(state, 25) == []
  end

  test "aliases nested" do

    state =
      """
      defmodule OuterModule.Nested do
        alias List, as: MyList
        IO.puts ""
        defmodule InnerModule do
          alias Enum, as: MyEnum
          IO.puts ""
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
        alias Code, as: MyCode
        IO.puts ""
      end
      defmodule OuterModule.Nested1 do
        IO.puts ""
        defmodule InnerModule.Nested do
          IO.puts ""
        end
        IO.puts ""
      end
      IO.puts ""
      """
      |> string_to_state

    assert get_line_aliases(state, 3)  == [{MyList, List}]
    assert get_line_aliases(state, 6)  == [{MyList, List}, {MyEnum, Enum}]
    assert get_line_aliases(state, 9)  == [{MyList, List}, {MyEnum, Enum}, {MyString, String}]
    assert get_line_aliases(state, 12) == [{MyList, List}, {MyEnum, Enum}, {MyString, String}, {MyMacro, Macro}]
    assert get_line_aliases(state, 14) == [{MyList, List}, {MyEnum, Enum}, {MyString, String}]
    assert get_line_aliases(state, 16) == [{MyList, List}, {MyEnum, Enum}]
    assert get_line_aliases(state, 19) == [{MyList, List}, {MyCode, Code}]
    assert get_line_aliases(state, 22) == []
    assert get_line_aliases(state, 24) == []
    assert get_line_aliases(state, 26) == []
    assert get_line_aliases(state, 28) == []
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

  test "aliases defined with v1.2 notation" do

    state =
      """
      defmodule MyModule do
        alias Foo.{User, Email}
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_aliases(state, 3) == [{User, Foo.User}, {Email, Foo.Email}]
  end

  test "aliases of aliases" do

    state =
      """
      defmodule MyModule do
        alias Foo.Bar, as: Fb
        alias Fb.Sub, as: S
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_aliases(state, 4) == [{Fb, Foo.Bar}, {S, Foo.Bar.Sub}]
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

    assert get_line_aliases(state, 4) == [{Ets, :ets}]
  end

  test "aliases defined with v1.2 notation (multiline)" do

    state =
      """
      defmodule A do
        alias A.{
          B
        }
      end
      """
      |> string_to_state

    assert get_line_aliases(state, 3) == [{B, A.B}]
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
      end
      """
      |> string_to_state

    assert get_line_aliases(state, 4) == []
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

  test "imports defined with v1.2 notation" do

    state =
      """
      defmodule MyModule do
        import Foo.Bar.{User, Email}
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_imports(state, 3) == [Foo.Bar.Email, Foo.Bar.User]
  end

  test "imports" do

    state =
      """
      defmodule OuterModule do
        import List
        IO.puts ""
        defmodule InnerModule do
          import Enum
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

    assert get_line_imports(state, 3)   == [List]
    assert get_line_imports(state, 6)   == [List, Enum]
    assert get_line_imports(state, 9)   == [List, Enum, String]
    assert get_line_imports(state, 12)  == [List, Enum, String, Macro]
    assert get_line_imports(state, 14)  == [List, Enum, String]
    assert get_line_imports(state, 16)  == [List, Enum]
    assert get_line_imports(state, 19)  == [Code, List]
    assert get_line_imports(state, 21)  == []
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

    assert get_line_imports(state, 4)   == [List]
  end

  test "imports aliased module" do

    state =
      """
      defmodule OuterModule do
        alias Some.Other.Module, as: S
        import S
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_imports(state, 4) == [Some.Other.Module]
  end

  test "imports nested" do

    state =
      """
      defmodule OuterModule do
        import List
        import SomeModule.Inner
        import :erlang_module
        IO.puts ""
      end
      """
      |> string_to_state

    refute get_line_imports(state, 5) == [SomeModule.Inner, List, :erlang_module]
    assert get_line_aliases(state, 5) == [{Inner, SomeModule.Inner}]
  end

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

    assert get_line_requires(state, 3)  == [Mod]
    assert get_line_requires(state, 6)  == [Mod, OtherMod]
    assert get_line_requires(state, 8)  == [Mod]
    assert get_line_requires(state, 10)  == []
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

    assert get_line_requires(state, 3)  == [Mod]
  end

  test "requires with 1.2 notation" do

    state =
      """
      defmodule MyModule do
        require Mod.{Mo1, Mod2}
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_requires(state, 3)  == [Mod.Mod2, Mod.Mo1]
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

    assert get_line_requires(state, 4)  == [Mod.Mo1]
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

    assert get_line_requires(state, 4)  == [:ets, Integer]
    assert get_line_aliases(state, 4)  == [{I, Integer}, {E, :ets}]
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

    assert get_line_requires(state, 5)  == [Some.Other.Module.Sub, Some.Other.Module]
  end

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

    assert get_line_module(state, 1)  == Elixir
    assert get_line_protocol(state, 1)  == nil
    assert get_line_module(state, 3)  == OuterModule
    assert get_line_protocol(state, 3)  == nil
    assert get_line_module(state, 7)  == OuterModule.InnerModule
    assert get_line_protocol(state, 7)  == nil
    assert get_line_module(state, 11) == OuterModule
    assert get_line_protocol(state, 11)  == nil

    assert get_line_module(state, 15) == Some.Nested
    assert get_line_protocol(state, 15)  == nil
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
          IO.puts ""
        end
      end
      """
      |> string_to_state

    # protocol and implementations create modules
    assert get_line_module(state, 3) == My.Reversible
    assert get_line_protocol(state, 3)  == nil
    assert get_line_module(state, 8) == My.Reversible.String
    assert get_line_protocol(state, 8) == {My.Reversible, [String]}
    assert get_line_module(state, 13) == [My.Reversible.Map, My.Reversible.My.List]
    assert get_line_protocol(state, 13) == {My.Reversible, [Map, My.List]}

    # multiple implementations create multiple modules
    assert get_line_module(state, 16) == [My.Reversible.Map.OuterModule, My.Reversible.My.List.OuterModule]
    assert get_line_protocol(state, 16)  == nil

    # protocol and implementations inside protocol implementation creates a cross product
    assert get_line_module(state, 21) == [My.Reversible.Map.Other, My.Reversible.My.List.Other]
    assert get_line_protocol(state, 21)  == nil
    assert get_line_module(state, 26) == [
      My.Reversible.Map.Other.Map,
      My.Reversible.Map.Other.My.Map,
      My.Reversible.My.List.Other.Map,
      My.Reversible.My.List.Other.My.Map
    ]
    assert get_line_protocol(state, 26)  == [{My.Reversible.Map.Other, [Map, My.Map]}, {My.Reversible.My.List.Other, [Map, My.Map]}]
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
          IO.puts ""
        end

        defmodule Some do
          defstruct [a: nil]
        end

        defimpl NiceProto, for: Some do
          def reverse(term), do: String.reverse(term)
          IO.puts ""
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
    assert get_line_protocol(state, 8)  == {NiceProto, [String]}

    # properly gets implementation name inherited from enclosing module
    assert get_line_module(state, 16) == NiceProto.NiceProtoImplementations.Some
    assert get_line_protocol(state, 16)  == {NiceProto, [NiceProtoImplementations.Some]}

    # aliases are expanded on protocol and implementation
    assert get_line_module(state, 24) == NiceProto.Enumerable.Date.Range
    assert get_line_protocol(state, 24) == {NiceProto, [Enumerable.Date.Range]}
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
      {OuterModule, nil, nil} => %{params: [nil], positions: [{2, 11}]},
      {OuterModule.InnerModule, :func, 0} => %{
        params: [[]],
        positions: [{5, 9}]
      },
      {OuterModule.InnerModule, :func, nil} => %{
        params: [[]],
        positions: [{5, 9}]
      },
      {OuterModule.InnerModule, nil, nil} => %{
        params: [nil],
        positions: [{4, 13}]
      },
      {Impls, nil, nil} => %{params: [nil], positions: [{28, 11}]},
      {Reversible, :reverse, 1} => %{params: [[{:term, [line: 19, column: 15], nil}]], positions: [{19, 7}]},
      {Reversible, :reverse, nil} => %{params: [[{:term, [line: 19, column: 15], nil}]], positions: [{19, 7}]},
      {Reversible, nil, nil} => %{params: [nil], positions: [{18, 13}]},
      {Reversible.Map, nil, nil} => %{
        params: [nil],
        positions: [{31, 11}]
      },
      {Reversible.Map, :reverse, 1} => %{
        params: [[{:term, [line: 32, column: 17], nil}]],
        positions: [{32, 9}]
      },
      {Reversible.Map, :reverse, nil} => %{
        params: [[{:term, [line: 32, column: 17], nil}]],
        positions: [{32, 9}]
      },
      {Reversible.My.List, nil, nil} => %{
        params: [nil],
        positions: [{31, 11}]
      },
      {Reversible.My.List, :reverse, 1} => %{
        params: [[{:term, [line: 32, column: 17], nil}]],
        positions: [{32, 9}]
      },
      {Reversible.My.List, :reverse, nil} => %{
        params: [[{:term, [line: 32, column: 17], nil}]],
        positions: [{32, 9}]
      },
      {Reversible.String, nil, nil} => %{
        params: [nil],
        positions: [{23, 9}]
      },
      {Reversible.String, :reverse, 1} => %{
        params: [[{:term, [line: 24, column: 15], nil}]],
        positions: [{24, 7}]
      },
      {Reversible.String, :reverse, nil} => %{
        params: [[{:term, [line: 24, column: 15], nil}]],
        positions: [{24, 7}]
      },
      {Some.Nested, nil, nil} => %{params: [nil], positions: [{14, 11}]}
    } == state.mods_funs_to_positions
  end

  test "behaviours" do

    state =
      """
      IO.puts ""
      defmodule OuterModule do
        use Application
        @behaviour SomeModule.SomeBehaviour
        IO.puts ""
        defmodule InnerModuleWithUse do
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

    assert get_line_behaviours(state, 1)  == []
    assert get_line_behaviours(state, 5)  == [Application, SomeModule.SomeBehaviour]
    assert get_line_behaviours(state, 8)  == [GenServer]
    assert get_line_behaviours(state, 12)  == [SomeOtherBehaviour]
    assert get_line_behaviours(state, 15)  == []
    assert get_line_behaviours(state, 17)  == [Application, SomeModule.SomeBehaviour]
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

    assert get_line_behaviours(state, 3)  == [:gen_server]
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

    assert get_line_behaviours(state, 4)  == [:gen_server]
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

    assert get_line_behaviours(state, 4)  == [Some.Module]
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

    assert State.get_scope_name(state, 3) == {:func, 0}
    assert State.get_scope_name(state, 5) == :MyModule
    assert State.get_scope_name(state, 7) == {:func_with_when, 1}
    assert State.get_scope_name(state, 9) == :MyModule
    assert State.get_scope_name(state, 11) == {:macro1, 1}
    assert State.get_scope_name(state, 13) == :MyModule
    assert State.get_scope_name(state, 15) == :MyModule
    assert State.get_scope_name(state, 16) == {:func_delegated, 1}
    assert State.get_scope_name(state, 18) == {:is_even, 1}
    assert State.get_scope_name(state, 21) == :Module1
    assert State.get_scope_name(state, 26) == :Module2
    assert State.get_scope_name(state, 31) == :Reversible
    assert State.get_scope_name(state, 35) == :"Map(__or__)My(__dot__)List"
    assert State.get_scope_name(state, 37) == {:reverse, 1}
  end

  test "finds positions for guards" do
    state =
      """
      defmodule MyModule do
        defguard is_even(value) when is_integer(value) and rem(value, 2) == 0
        defguardp is_odd(value) when is_integer(value) and rem(value, 2) == 1
        IO.puts ""
      end
      """
      |> string_to_state

      assert %{
        mods_funs_to_positions: %{
          {MyModule, :is_even, 1} => %{
            params: [[{:value, [line: 2, column: 20], nil}]],
            positions: [{2, 12}]
          },
          {MyModule, :is_even, nil} => %{
            params: [[{:value, [line: 2, column: 20], nil}]],
            positions: [{2, 12}]
          },
          {MyModule, :is_odd, 1} => %{
            params: [[{:value, [line: 3, column: 20], nil}]],
            positions: [{3, 13}]
          },
          {MyModule, :is_odd, nil} => %{
            params: [[{:value, [line: 3, column: 20], nil}]],
            positions: [{3, 13}]
          }
        }
      } = state
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
        defdelegate func_delegated(par), to: OtherModule
        defmodule Nested do
        end
      end
      """
      |> string_to_state

      assert %{
        MyModuleWithFuns => %{
          {:func, 0} => %ElixirSense.Core.State.ModFunInfo{type: :def},
          {:func_delegated, 1} => %ElixirSense.Core.State.ModFunInfo{type: :defdelegate},
          {:funcp, 0} => %ElixirSense.Core.State.ModFunInfo{type: :defp},
          {:is_even, 1} => %ElixirSense.Core.State.ModFunInfo{type: :defguard},
          {:is_evenp, 1} => %ElixirSense.Core.State.ModFunInfo{type: :defguardp},
          {:macro1, 1} => %ElixirSense.Core.State.ModFunInfo{type: :defmacro},
          {:macro1p, 1} => %ElixirSense.Core.State.ModFunInfo{type: :defmacrop}
        },
        MyModuleWithoutFuns => %{},
        MyModuleWithFuns.Nested => %{},
      } == state.mods_funs
  end

  test "registers mods and func for protocols" do
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
        defimpl R, for: [Map, Ml] do
          def reverse(term), do: Enum.reverse(term)
          IO.puts ""
        end
      end
      """
      |> string_to_state

      assert %{
        MyModuleWithFuns => %{
          {:func, 0} => %ElixirSense.Core.State.ModFunInfo{type: :def},
          {:func_delegated, 1} => %ElixirSense.Core.State.ModFunInfo{type: :defdelegate},
          {:funcp, 0} => %ElixirSense.Core.State.ModFunInfo{type: :defp},
          {:is_even, 1} => %ElixirSense.Core.State.ModFunInfo{type: :defguard},
          {:is_evenp, 1} => %ElixirSense.Core.State.ModFunInfo{type: :defguardp},
          {:macro1, 1} => %ElixirSense.Core.State.ModFunInfo{type: :defmacro},
          {:macro1p, 1} => %ElixirSense.Core.State.ModFunInfo{type: :defmacrop}
        },
        MyModuleWithoutFuns => %{},
        MyModuleWithFuns.Nested => %{},
        Impls => %{},
        Reversible => %{
          {:reverse, 1} => %ElixirSense.Core.State.ModFunInfo{type: :def}
        },
        Reversible.Map => %{
          {:reverse, 1} => %ElixirSense.Core.State.ModFunInfo{type: :def}
        },
        Reversible.My.List => %{
          {:reverse, 1} => %ElixirSense.Core.State.ModFunInfo{type: :def}
        },
        Reversible.String => %{
          {:reverse, 1} => %ElixirSense.Core.State.ModFunInfo{type: :def}
        }
      } == state.mods_funs
  end

  defp string_to_state(string) do
    string
    |> Code.string_to_quoted(columns: true)
    |> (fn {:ok, ast} -> ast end).()
    |> MetadataBuilder.build
  end

  defp get_line_vars(state, line) do
    case state.lines_to_env[line] do
      nil -> []
      env -> env.vars
    end |> Enum.sort
  end

  defp get_line_aliases(state, line) do
    case state.lines_to_env[line] do
      nil -> []
      env -> env.aliases
    end
  end

  defp get_line_imports(state, line) do
    case state.lines_to_env[line] do
      nil -> []
      env -> env.imports
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
    end |> Enum.sort
  end

  defp get_line_behaviours(state, line) do
    case state.lines_to_env[line] do
      nil -> []
      env -> env.behaviours
    end |> Enum.sort
  end

  defp get_line_module(state, line) do
    if env = state.lines_to_env[line] do
      case env.module_variants do
        [single] -> single
        other -> other
      end
    end
  end

  defp get_line_protocol(state, line) do
    if env = state.lines_to_env[line] do
      case env.protocols do
        [] -> nil
        [single] -> single
        other -> other
      end
    end
  end

  defp get_subject_definition_line(module, func, arity) do
    file = module.module_info(:compile)[:source]
    acc =
      File.read!(file)
      |> Code.string_to_quoted(columns: true)
      |> MetadataBuilder.build

    %{positions: positions} = Map.get(acc.mods_funs_to_positions, {module, func, arity})
    {line_number, _col} = List.last(positions)

    File.read!(file) |> String.split("\n") |> Enum.at(line_number-1)
  end

end
