defmodule ElixirSense.Core.SourceTest do
  use ExUnit.Case

  import ElixirSense.Core.Source

  describe "which_func/1" do

    test "functions without namespace" do
      assert which_func("var = func(") == %{
        candidate: {nil, :func},
        npar: 0,
        pipe_before: false,
        pos: {{1, 7}, {1, 11}}
      }
      assert which_func("var = func(param1, ") == %{
        candidate: {nil, :func},
        npar: 1,
        pipe_before: false,
        pos: {{1, 7}, {1, 11}}
      }
    end

    test "functions with namespace" do
      assert which_func("var = Mod.func(param1, par") == %{
        candidate: {Mod, :func},
        npar: 1,
        pipe_before: false,
        pos: {{1, 7}, {1, 15}}
      }
      assert which_func("var = Mod.SubMod.func(param1, param2, par") == %{
        candidate: {Mod.SubMod, :func},
        npar: 2,
        pipe_before: false,
        pos: {{1, 7}, {1, 22}}
      }
    end

    test "nested functions calls" do
      assert which_func("var = outer_func(Mod.SubMod.func(param1,") == %{
        candidate: {Mod.SubMod, :func},
        npar: 1,
        pipe_before: false,
        pos: {{1, 18}, {1, 33}}
      }
      assert which_func("var = outer_func(Mod.SubMod.func(param1, [inner_func(") == %{
        candidate: {nil, :inner_func},
        npar: 0,
        pipe_before: false,
        pos: {{1, 43}, {1, 53}}
      }
      assert which_func("var = outer_func(func(param1, inner_func, ") == %{
        candidate: {nil, :func},
        npar: 2,
        pipe_before: false,
        pos: {{1, 18}, {1, 22}}
      }
      assert which_func("var = outer_func(func(param1, inner_func(), ") == %{
        candidate: {nil, :func},
        npar: 2,
        pipe_before: false,
        pos: {{1, 18}, {1, 22}}
      }
      assert which_func("var = func(param1, func2(fun(p3), 4, 5), func3(p1, p2), ") == %{
        candidate: {nil, :func},
        npar: 3,
        pipe_before: false,
        pos: {{1, 7}, {1, 11}}
      }
    end

    test "function call with multiple lines" do
      assert which_func("""
        var = Mod.func(param1,
          param2,

        """) == %{candidate: {Mod, :func}, npar: 2, pipe_before: false, pos: {{1, 7}, {1, 15}}}
    end

    test "after double quotes" do
      assert which_func("var = func(param1, \"not_a_func(, ") == %{
        candidate: {nil, :func},
        npar: 1,
        pipe_before: false,
        pos: {{1, 7}, {1, 11}}
      }
      assert which_func("var = func(\"a_string_(param1\", ") == %{
        candidate: {nil, :func},
        npar: 1,
        pipe_before: false,
        pos: {{1, 7}, {1, 11}}
      }
    end

    test "with operators" do
      assert which_func("var = Mod.func1(param) + func2(param1, ") == %{
        candidate: {nil, :func2},
        npar: 1,
        pipe_before: false,
        pos: {{1, 26}, {1, 31}}
      }
    end

    test "erlang functions" do
      assert which_func("var = :global.whereis_name( ") == %{
        candidate: {:global, :whereis_name},
        npar: 0,
        pipe_before: false,
        pos: {{1, 7}, {1, 27}}
      }
    end

    test "with fn" do
      assert which_func("fn(a, ") == %{candidate: :none, npar: 0, pipe_before: false, pos: nil}
    end

    test "with another fn before" do
      assert which_func("var = Enum.sort_by(list, fn(i) -> i*i end, fn(a, ") == %{
        candidate: {Enum, :sort_by},
        npar: 2,
        pipe_before: false,
        pos: {{1, 7}, {1, 19}}
      }
    end

    test "inside fn body" do
      assert which_func("var = Enum.map([1,2], fn(i) -> i*") == %{
        candidate: {Enum, :map},
        npar: 1,
        pipe_before: false,
        pos: {{1, 7}, {1, 15}}
      }
    end

    test "inside a list" do
      assert which_func("var = Enum.map([1,2,3") == %{
        candidate: {Enum, :map},
        npar: 0,
        pipe_before: false,
        pos: {{1, 7}, {1, 15}}
      }
    end

    test "inside a list after comma" do
      assert which_func("var = Enum.map([1,") == %{
        candidate: {Enum, :map},
        npar: 0,
        pipe_before: false,
        pos: {{1, 7}, {1, 15}}
      }
    end

    test "inside an list without items" do
      assert which_func("var = Enum.map([") == %{
        candidate: {Enum, :map},
        npar: 0,
        pipe_before: false,
        pos: {{1, 7}, {1, 15}}
      }
    end

    test "inside a list with a list before" do
      assert which_func("var = Enum.map([1,2], [1, ") == %{
        candidate: {Enum, :map},
        npar: 1,
        pipe_before: false,
        pos: {{1, 7}, {1, 15}}
      }
    end

    test "inside a tuple" do
      assert which_func("var = Enum.map({1,2,3") == %{
        candidate: {Enum, :map},
        npar: 0,
        pipe_before: false,
        pos: {{1, 7}, {1, 15}}
      }
    end

    test "inside a tuple with another tuple before" do
      assert which_func("var = Enum.map({1,2}, {1, ") == %{
        candidate: {Enum, :map},
        npar: 1,
        pipe_before: false,
        pos: {{1, 7}, {1, 15}}
      }
    end

    test "inside a tuple inside a list" do
      assert which_func("var = Enum.map({1,2}, [{1, ") == %{
        candidate: {Enum, :map},
        npar: 1,
        pipe_before: false,
        pos: {{1, 7}, {1, 15}}
      }
    end

    test "inside a tuple after comma" do
      assert which_func("var = Enum.map([{1,") == %{
        candidate: {Enum, :map},
        npar: 0,
        pipe_before: false,
        pos: {{1, 7}, {1, 15}}
      }
    end

    test "inside a list inside a tuple inside a list" do
      assert which_func("var = Enum.map([{1,[a, ") == %{
        candidate: {Enum, :map},
        npar: 0,
        pipe_before: false,
        pos: {{1, 7}, {1, 15}}
      }
    end

    test "fails when code has parsing errors before the cursor" do
      assert which_func("} = Enum.map(list, ") == %{candidate: :none, npar: 0, pipe_before: false, pos: nil}
    end

  end

  describe "text_before/3" do

    test "functions without namespace" do
      code = """
      defmodule MyMod do
        def my_func(par1, )
      end
      """
      text = """
      defmodule MyMod do
        def my_func(par1,
      """ |> String.trim()

      assert text_before(code, 2, 20) == text
    end

  end
end
