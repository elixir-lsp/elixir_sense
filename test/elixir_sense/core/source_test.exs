defmodule ElixirSense.Core.SourceTest do
  use ExUnit.Case

  import ElixirSense.Core.Source

  describe "whichd_func/" do

    test "functions without namespace" do
      assert which_func("var = func(") == {nil, :func, 0}
      assert which_func("var = func(param1, ") == {nil, :func, 1}
    end

    test "functions with namespace" do
      assert which_func("var = Mod.func(param1, par") == {Mod, :func, 1}
      assert which_func("var = Mod.SubMod.func(param1, param2, par") == {Mod.SubMod, :func, 2}
    end

    test "nested functions calls" do
      assert which_func("var = outer_func(Mod.SubMod.func(param1,") == {Mod.SubMod, :func, 1}
      assert which_func("var = outer_func(Mod.SubMod.func(param1, [inner_func(") == {nil, :inner_func, 0}
      assert which_func("var = outer_func(func(param1, inner_func, ") == {nil, :func, 2}
      assert which_func("var = outer_func(func(param1, inner_func(), ") == {nil, :func, 2}
      assert which_func("var = func(param1, func2(fun(p3), 4, 5), func3(p1, p2), ") == {nil, :func, 3}
    end

    test "function call with multiple lines" do
      assert which_func("""
        var = Mod.func(param1,
          param2,

        """) == {Mod, :func, 2}
    end

    test "after double quotes" do
      assert which_func("var = func(param1, \"not_a_func(, ") == {nil, :func, 1}
      assert which_func("var = func(\"a_string_(param1\", ") == {nil, :func, 1}
    end

    test "with operators" do
      assert which_func("var = Mod.func1(param) + func2(param1, ") == {nil, :func2, 1}
    end

    test "erlang functions" do
      assert which_func("var = :global.whereis_name( ") == {:global, :whereis_name, 0}
    end

    test "with fn" do
      assert which_func("fn(a, ") == :none
    end

    test "with another fn before" do
      assert which_func("var = Enum.sort_by(list, fn(i) -> i*i end, fn(a, ") == {Enum, :sort_by, 2}
    end

    test "inside fn body" do
      assert which_func("var = Enum.map([1,2], fn(i) -> i*") == {Enum, :map, 1}
    end

    test "inside a list" do
      assert which_func("var = Enum.map([1,2,3") == {Enum, :map, 0}
    end

    test "inside a list after comma" do
      assert which_func("var = Enum.map([1,") == {Enum, :map, 0}
    end

    test "inside an list without items" do
      assert which_func("var = Enum.map([") == {Enum, :map, 0}
    end

    test "inside a list with a list before" do
      assert which_func("var = Enum.map([1,2], [1, ") == {Enum, :map, 1}
    end

    test "inside a tuple" do
      assert which_func("var = Enum.map({1,2,3") == {Enum, :map, 0}
    end

    test "inside a tuple with another tuple before" do
      assert which_func("var = Enum.map({1,2}, {1, ") == {Enum, :map, 1}
    end

    test "inside a tuple inside a list" do
      assert which_func("var = Enum.map({1,2}, [{1, ") == {Enum, :map, 1}
    end

    test "inside a tuple after comma" do
      assert which_func("var = Enum.map([{1,") == {Enum, :map, 0}
    end

    test "inside a list inside a tuple inside a list" do
      assert which_func("var = Enum.map([{1,[a, ") == {Enum, :map, 0}
    end

    test "fails when code has parsing errors before the cursor" do
      assert which_func("} = Enum.map(list, ") == :none
    end

  end

end
