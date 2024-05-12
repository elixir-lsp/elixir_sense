defmodule ElixirSense.Core.MetadataBuilder.ErrorRecoveryTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Core.MetadataBuilder
  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode

  defp get_cursor_env(code) do
    {:ok, ast} = NormalizedCode.Fragment.container_cursor_to_quoted(code, [columns: true, token_metadata: true])
    state = MetadataBuilder.build(ast)
    state.cursor_env
  end

  describe "incomplete case" do
    test "cursor in argument" do
      code = """
      x = 5
      case \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in clause left side" do
      code = """
      case a do
        [x, \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in clause guard" do
      code = """
      case a do
        x when \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in clause guard call" do
      code = """
      case a do
        x when is_atom(\
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in clause right side" do
      code = """
      case a do
        x -> \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in clause right side after expressions" do
      code = """
      case a do
        x ->
          foo(1)
          \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end
  end

  describe "incomplete cond" do
    test "cursor in clause left side" do
      code = """
      x = foo()
      cond do
        \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in clause left side with assignment" do
      code = """
      cond do
        (x = foo(); \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in clause right side" do
      code = """
      cond do
        x = foo() -> \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in clause right side after expressions" do
      code = """
      cond do
        x = foo() ->
          foo(1)
          \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end
  end

  describe "incomplete receive" do
    test "cursor in clause left side" do
      code = """
      x = foo()
      receive do
        \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in clause left side pin" do
      code = """
      x = foo()
      receive do
        {^\
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in clause left side multiple matches" do
      code = """
      receive do
        {:msg, x, \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in clause left side guard" do
      code = """
      receive do
        {:msg, x} when \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in clause left side guard call" do
      code = """
      receive do
        {:msg, x} when is_atom(\
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in clause right side" do
      code = """
      receive do
        {:msg, x} -> \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in after clause left side" do
      code = """
      x = foo()
      receive do
        a -> :ok
      after
        \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in after clause right side" do
      code = """
      x = foo()
      receive do
        a -> :ok
      after
        0 -> \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end
  end

  describe "incomplete try" do
    test "cursor in do block" do
      code = """
      x = foo()
      try do
        \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in left side of rescue clause" do
      code = """
      x = foo()
      try do
        bar(x)
      rescue
        \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in left side of rescue clause match expression - invalid var" do
      code = """
      x = foo()
      try do
        bar(x)
      rescue
        bar() in [\
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in left side of rescue clause match expression" do
      code = """
      x = foo()
      try do
        bar(x)
      rescue
        e in [\
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in right side of rescue clause" do
      code = """
      try do
        bar()
      rescue
        x in [Error] -> \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in left side of catch clause" do
      code = """
      x = foo()
      try do
        bar()
      catch
        \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in left side of catch clause guard" do
      code = """
      try do
        bar()
      catch
        x when \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in left side of catch clause after type" do
      code = """
      try do
        bar()
      catch
        x, \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in left side of catch clause 2 arg guard" do
      code = """
      try do
        bar()
      catch
        x, _ when \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in right side of catch clause" do
      code = """
      try do
        bar()
      catch
        x -> \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in right side of catch clause 2 arg" do
      code = """
      try do
        bar()
      catch
        x, _ -> \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in left side of else clause" do
      code = """
      x = foo()
      try do
        bar()
      else
        \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in left side of else clause guard" do
      code = """
      try do
        bar()
      else
        x when \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in right side of else clause" do
      code = """
      try do
        bar()
      else
        x -> \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in after block" do
      code = """
      x = foo()
      try do
        bar()
      after
        \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end
  end
end
