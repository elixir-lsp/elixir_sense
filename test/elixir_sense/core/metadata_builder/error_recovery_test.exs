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

  describe "invalid fn" do
    # unfortunately container_cursor_to_quoted cannot handle fn
    test "different clause arities" do
      code = """
      fn
        _ -> :ok
        x, _ -> __cursor__()
      end
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "default args in clause" do
      code = """
      fn
        x \\\\ nil -> __cursor__()
      end
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "incomplete clause left side" do
      code = """
      x = foo()
      fn
        __cursor__()
      end
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "incomplete clause left side guard" do
      code = """
      fn
        x when __cursor__()
      end
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end

    test "incomplete clause right side" do
      code = """
      fn
        x -> __cursor__()
      end
      """
      assert {meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, & &1.name == :x)
    end
  end

  describe "capture" do
    test "empty" do
      code = """
      &\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "local" do
      code = """
      &foo\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "local slash no arity" do
      code = """
      &foo/\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "local slash arity" do
      code = """
      &foo/1\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "local slash invalid arity" do
      code = """
      &foo/1000; \
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "local dot" do
      code = """
      &foo.\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "local dot call" do
      code = """
      &foo.(\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "local dot call closed" do
      code = """
      &foo.()\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "local dot right" do
      code = """
      &foo.bar\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "remote" do
      code = """
      &Foo\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "remote dot" do
      code = """
      &Foo.\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "remote dot right" do
      code = """
      &Foo.bar\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "remote dot right no arity" do
      code = """
      &Foo.bar/\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "remote dot right arity" do
      code = """
      &Foo.bar/1\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "remote dot call" do
      code = """
      &Foo.bar(\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "remote dot call closed" do
      code = """
      &Foo.bar()\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "tuple" do
      code = """
      &{\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "tuple closed" do
      code = """
      &{}\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "list" do
      code = """
      &[\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "list closed" do
      code = """
      &[]\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "bitstring" do
      code = """
      &<<\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "bitstring closed" do
      code = """
      &<<>>\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "map no braces" do
      code = """
      &%\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "map" do
      code = """
      &%{\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "map closed" do
      code = """
      &%{}\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "struct no braces" do
      code = """
      &%Foo\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "struct" do
      code = """
      &%Foo{\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "struct closed" do
      code = """
      &%Foo{}\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "block" do
      code = """
      & (\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "block multiple expressions" do
      code = """
      & (:ok; \
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "arg var incomplete" do
      code = """
      & &\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "arg var" do
      code = """
      & &2\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "arg var in list" do
      code = """
      &[&1, \
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "arg var in list without predecessor" do
      code = """
      &[&2, \
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "no arg" do
      code = """
      &{}; \
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "invalid arg nuber" do
      code = """
      & &0; \
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "outside of capture" do
      code = """
      &1; \
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "invalid arg local" do
      code = """
      &foo; \
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "invalid arg" do
      code = """
      &"foo"; \
      """
      assert {meta, env} = get_cursor_env(code)
    end
  end

  describe "pin" do
    test "outside of match" do
      code = """
      ^\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "cursor in match" do
      code = """
      ^__cursor__() = x\
      """
      assert {meta, env} = get_cursor_env(code)
    end
  end

  describe "map" do
    test "invalid key in match" do
      code = """
      %{foo => x} = x\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "update in match" do
      code = """
      %{a | x: __cursor__()} = x\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "cursor in place of key value pair" do
      code = """
      %{a: "123", \
      """
      assert {meta, env} = get_cursor_env(code)
    end
  end

  describe "struct" do
    test "no map" do
      code = """
      %\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "invalid map name" do
      code = """
      %foo{\
      """
      assert {meta, env} = get_cursor_env(code)
    end

    test "invalid key" do
      code = """
      %Foo{"asd" => [\
      """
      assert {meta, env} = get_cursor_env(code)
    end
  end
end
