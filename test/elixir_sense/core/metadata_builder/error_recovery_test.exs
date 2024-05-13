defmodule ElixirSense.Core.MetadataBuilder.ErrorRecoveryTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Core.MetadataBuilder
  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode

  defp get_cursor_env(code) do
    {:ok, ast} = NormalizedCode.Fragment.container_cursor_to_quoted(code, [columns: true, token_metadata: true])
    # dbg(ast)
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

  describe "incomplete with" do
    test "cursor in match expressions" do
      code = """
      x = foo()
      with \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in match expressions guard" do
      code = """
      with x when \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in match expressions - right side" do
      code = """
      x = foo()
      with 1 <- \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in match expressions - right side next expression" do
      code = """
      with x <- foo(), \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in do block" do
      code = """
      with x <- foo() do
        \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in else clause left side" do
      code = """
      x = foo()
      with 1 <- foo() do
        :ok
      else
        \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in else clause left side guard" do
      code = """
      with 1 <- foo() do
        :ok
      else
        x when \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in else clause right side" do
      code = """
      with 1 <- foo() do
        :ok
      else
        x -> \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end
  end

  describe "incomplete for" do
    test "cursor in generator match expressions" do
      code = """
      x = foo()
      for \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in generator match expression guard" do
      code = """
      for x when \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in generator match expression right side" do
      code = """
      x = foo()
      for a <- \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in generator match expressions bitstring" do
      code = """
      x = foo()
      for <<\
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in generator match expression guard bitstring" do
      code = """
      for <<x when \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in generator match expression right side bitstring" do
      code = """
      x = foo()
      for <<a <- \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in generator next match expression" do
      code = """
      for x <- [], \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in into option" do
      code = """
      x = foo()
      for x <- [], into: \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in uniq option" do
      code = """
      x = foo()
      for x <- [], uniq: \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in reduce option" do
      code = """
      x = foo()
      for x <- [], reduce: \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in do block" do
      code = """
      x = foo()
      for x <- [], do: \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in do block reduce left side of clause" do
      code = """
      x = foo()
      for x <- [], reduce: %{} do
        \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in do block reduce left side of clause guard" do
      code = """
      for x <- [], reduce: %{} do
        y when \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
      assert Enum.any?(env.vars, & &1.name == :y)
    end

    test "cursor in do block reduce left side of clause too many args" do
      code = """
      for x <- [], reduce: %{} do
        y, \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
      assert Enum.any?(env.vars, & &1.name == :y)
    end

    test "cursor in do block reduce right side of clause" do
      code = """
      for x <- [], reduce: %{} do
        y -> \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
      assert Enum.any?(env.vars, & &1.name == :y)
    end

    test "cursor in do block reduce right side of clause too many args" do
      code = """
      for x <- [], reduce: %{} do
        y, z -> \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
      assert Enum.any?(env.vars, & &1.name == :y)
    end

    test "cursor in do block reduce right side of clause too little args" do
      code = """
      for x <- [], reduce: %{} do
        -> \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "cursor in do block right side of clause without reduce" do
      code = """
      for x <- [] do
        y -> \
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
      assert Enum.any?(env.vars, & &1.name == :y)
    end
  end
end
