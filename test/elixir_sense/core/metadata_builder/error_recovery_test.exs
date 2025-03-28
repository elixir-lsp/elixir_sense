defmodule ElixirSense.Core.MetadataBuilder.ErrorRecoveryTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Core.MetadataBuilder
  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode

  defp get_cursor_env(code, use_string_to_quoted \\ false, trailing_fragment \\ "") do
    {:ok, ast} =
      if use_string_to_quoted do
        Code.string_to_quoted(code,
          columns: true,
          token_metadata: true
        )
      else
        options = ElixirSense.Core.Metadata.container_cursor_to_quoted_options(trailing_fragment)
        NormalizedCode.Fragment.container_cursor_to_quoted(code, options)
      end

    # dbg(ast)
    state = MetadataBuilder.build(ast)
    state.cursor_env
  end

  describe "incomplete case" do
    test "no arg 1" do
      code = """
      case []
      \
      """

      assert {_meta, _env} = get_cursor_env(code)
    end

    test "no arg 2" do
      code = """
      case [], []
      \
      """

      assert {_meta, _env} = get_cursor_env(code)
    end

    test "cursor in argument 1" do
      code = """
      case [], \
      """

      assert {_meta, _env} = get_cursor_env(code)
    end

    test "cursor in argument 2" do
      code = """
      x = 5
      case \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in clause left side" do
      code = """
      case a do
        [x, \
      """

      assert {_meta, _env} = get_cursor_env(code)
      # this test fails
      # assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in clause guard" do
      code = """
      case a do
        x when \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    test "cursor in clause guard call" do
      code = """
      case a do
        x when is_atom(\
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in clause right side" do
      code = """
      case a do
        x -> \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    test "cursor in clause right side after expressions" do
      code = """
      case a do
        x ->
          foo(1)
          \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    if Version.match?(System.version(), ">= 1.15.0") do
      test "invalid number of args with when" do
        code = """
        case nil do 0, z when not is_nil(z) -> \
        """

        assert get_cursor_env(code)
      end
    end

    if Version.match?(System.version(), ">= 1.15.0") do
      test "invalid number of args" do
        code = """
        case nil do 0, z -> \
        """

        assert get_cursor_env(code)
      end
    end
  end

  describe "incomplete cond" do
    test "no arg" do
      code = """
      cond []
      \
      """

      assert {_meta, _env} = get_cursor_env(code)
    end

    test "cursor in arg" do
      code = """
      x = foo()
      cond \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in clause left side" do
      code = """
      x = foo()
      cond do
        \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in clause left side with assignment" do
      code = """
      cond do
        (x = foo(); \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    test "cursor in clause right side" do
      code = """
      cond do
        x = foo() -> \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    test "cursor in clause right side after expressions" do
      code = """
      cond do
        x = foo() ->
          foo(1)
          \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    if Version.match?(System.version(), ">= 1.15.0") do
      test "invalid number of args" do
        code = """
        cond do 0, z -> \
        """

        assert get_cursor_env(code)
      end
    end
  end

  describe "incomplete receive" do
    test "no arg" do
      code = """
      receive []
      \
      """

      assert {_meta, _env} = get_cursor_env(code)
    end

    test "cursor in arg" do
      code = """
      x = foo()
      receive \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in clause left side" do
      code = """
      x = foo()
      receive do
        \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in clause left side pin" do
      code = """
      x = foo()
      receive do
        {^\
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in clause left side multiple matches" do
      code = """
      receive do
        {:msg, x, \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in clause left side guard" do
      code = """
      receive do
        {:msg, x} when \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    test "cursor in clause left side guard call" do
      code = """
      receive do
        {:msg, x} when is_atom(\
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in clause right side" do
      code = """
      receive do
        {:msg, x} -> \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    test "cursor in after clause left side" do
      code = """
      x = foo()
      receive do
        a -> :ok
      after
        \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in after clause right side" do
      code = """
      x = foo()
      receive do
        a -> :ok
      after
        0 -> \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    if Version.match?(System.version(), ">= 1.15.0") do
      test "invalid number of args in after" do
        code = """
        receive do
          a -> :ok
        after
          0, z -> \
        """

        assert get_cursor_env(code)
      end
    end

    test "invalid number of clauses in after" do
      code = """
      receive do
        a -> :ok
      after
        0 -> :ok
        1 -> \
      """

      assert get_cursor_env(code)
    end
  end

  describe "incomplete try" do
    test "no arg" do
      code = """
      try []
      \
      """

      assert {_meta, _env} = get_cursor_env(code)
    end

    test "cursor in arg" do
      code = """
      x = foo()
      try \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in do block" do
      code = """
      x = foo()
      try do
        \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in left side of rescue clause" do
      code = """
      x = foo()
      try do
        bar(x)
      rescue
        \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in left side of rescue clause match expression - invalid var" do
      code = """
      x = foo()
      try do
        bar(x)
      rescue
        bar() in [\
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in left side of rescue clause match expression" do
      code = """
      x = foo()
      try do
        bar(x)
      rescue
        e in [\
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in right side of rescue clause" do
      code = """
      try do
        bar()
      rescue
        x in [Error] -> \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    test "cursor in left side of catch clause" do
      code = """
      x = foo()
      try do
        bar()
      catch
        \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in left side of catch clause guard" do
      code = """
      try do
        bar()
      catch
        x when \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    if Version.match?(System.version(), ">= 1.17.0") do
      if Version.match?(System.version(), ">= 1.18.0") do
        test "cursor in left side of catch clause after type" do
          code = """
          try do
            bar()
          catch
            x, \
          """

          assert {_meta, env} = get_cursor_env(code, false, " -> :ok\nend")
          assert Enum.any?(env.vars, &(&1.name == :x))
        end
      else
        test "cursor in left side of catch clause after type" do
          code = """
          try do
            bar()
          catch
            x, \
          """

          assert {_meta, env} = get_cursor_env(code)
          assert Enum.any?(env.vars, &(&1.name == :x))
        end
      end
    end

    if Version.match?(System.version(), ">= 1.17.0") do
      if Version.match?(System.version(), ">= 1.18.0") do
        test "cursor in left side of catch clause 2 arg guard" do
          code = """
          try do
            bar()
          catch
            x, _ when \
          """

          assert {_meta, env} = get_cursor_env(code, false, " -> :ok\nend")
          assert Enum.any?(env.vars, &(&1.name == :x))
        end
      else
        test "cursor in left side of catch clause 2 arg guard" do
          code = """
          try do
            bar()
          catch
            x, _ when \
          """

          assert {_meta, env} = get_cursor_env(code)
          assert Enum.any?(env.vars, &(&1.name == :x))
        end
      end
    end

    test "cursor in right side of catch clause" do
      code = """
      try do
        bar()
      catch
        x -> \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    if Version.match?(System.version(), ">= 1.15.0") do
      test "cursor in right side of catch clause 2 arg" do
        code = """
        try do
          bar()
        catch
          x, _ -> \
        """

        assert {_meta, env} = get_cursor_env(code)
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    test "cursor in left side of else clause" do
      code = """
      x = foo()
      try do
        bar()
      else
        \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in left side of else clause guard" do
      code = """
      try do
        bar()
      else
        x when \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    test "cursor in right side of else clause" do
      code = """
      try do
        bar()
      else
        x -> \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    test "cursor in after block" do
      code = """
      x = foo()
      try do
        bar()
      after
        \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end
  end

  describe "incomplete with" do
    test "cursor in arg" do
      code = """
      x = foo()
      with [], \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in match expressions" do
      code = """
      x = foo()
      with \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in match expressions guard" do
      code = """
      with x when \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    test "cursor in match expressions - right side" do
      code = """
      x = foo()
      with 1 <- \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    test "cursor in match expressions - right side next expression" do
      code = """
      with x <- foo(), \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in do block" do
      code = """
      with x <- foo() do
        \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in else clause left side" do
      code = """
      x = foo()
      with 1 <- foo() do
        :ok
      else
        \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in else clause left side guard" do
      code = """
      with 1 <- foo() do
        :ok
      else
        x when \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    test "cursor in else clause right side" do
      code = """
      with 1 <- foo() do
        :ok
      else
        x -> \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end
  end

  describe "incomplete for" do
    test "cursor in arg" do
      code = """
      x = foo()
      for [], \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in generator match expressions" do
      code = """
      x = foo()
      for \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in generator match expression guard" do
      code = """
      for x when \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    test "cursor in generator match expression right side" do
      code = """
      x = foo()
      for a <- \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    test "cursor in generator match expressions bitstring" do
      code = """
      x = foo()
      for <<\
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in generator match expression guard bitstring" do
      code = """
      for <<x when \
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    test "cursor in generator match expression right side bitstring" do
      code = """
      x = foo()
      for <<a <- \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in generator next match expression" do
      code = """
      for x <- [], \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in into option" do
      code = """
      x = foo()
      for x <- [], into: \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in uniq option" do
      code = """
      x = foo()
      for x <- [], uniq: \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in reduce option" do
      code = """
      x = foo()
      for x <- [], reduce: \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in do block" do
      code = """
      x = foo()
      for x <- [], do: \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in do block reduce left side of clause" do
      code = """
      x = foo()
      for x <- [], reduce: %{} do
        \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in do block reduce left side of clause guard" do
      code = """
      for x <- [], reduce: %{} do
        y when \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :y))
      end
    end

    if Version.match?(System.version(), ">= 1.17.0") do
      test "cursor in do block reduce left side of clause too many args" do
        code = """
        for x <- [], reduce: %{} do
          y, \
        """

        if Version.match?(System.version(), ">= 1.18.0") do
          assert {_meta, env} = get_cursor_env(code, false, " -> :ok\nend")
          assert Enum.any?(env.vars, &(&1.name == :x))
        else
          assert {_meta, env} = get_cursor_env(code)
          assert Enum.any?(env.vars, &(&1.name == :x))
        end

        # this test fails
        # assert Enum.any?(env.vars, &(&1.name == :y))
      end
    end

    test "cursor in do block reduce right side of clause" do
      code = """
      for x <- [], reduce: %{} do
        y -> \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :y))
      end
    end

    if Version.match?(System.version(), ">= 1.15.0") do
      test "cursor in do block reduce right side of clause too many args" do
        code = """
        for x <- [], reduce: %{} do
          y, z -> \
        """

        assert {_meta, env} = get_cursor_env(code)
        assert Enum.any?(env.vars, &(&1.name == :x))
        assert Enum.any?(env.vars, &(&1.name == :y))
      end
    end

    test "cursor in do block reduce right side of clause too little args" do
      code = """
      for x <- [], reduce: %{} do
        -> \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))
    end

    test "cursor in do block right side of clause without reduce" do
      code = """
      for x <- [] do
        y -> \
      """

      assert {_meta, env} = get_cursor_env(code)
      assert Enum.any?(env.vars, &(&1.name == :x))

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :y))
      end
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

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    test "default args in clause" do
      code = """
      fn
        x \\\\ nil -> __cursor__()
      end
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end

    test "incomplete clause left side" do
      code = """
      x = foo()
      fn \
      """

      if Version.match?(System.version(), ">= 1.18.0") do
        assert {_meta, env} = get_cursor_env(code, false, " -> :ok end")
        assert Enum.any?(env.vars, &(&1.name == :x))
      else
        assert {_meta, env} = get_cursor_env(code)

        if Version.match?(System.version(), ">= 1.15.0") do
          assert Enum.any?(env.vars, &(&1.name == :x))
        end
      end
    end

    if Version.match?(System.version(), ">= 1.17.0") do
      if Version.match?(System.version(), ">= 1.18.0") do
        test "incomplete clause left side guard" do
          code = """
          fn
            x when \
          """

          assert {_meta, env} = get_cursor_env(code, false, " -> :ok\nend")
          assert Enum.any?(env.vars, &(&1.name == :x))
        end
      else
        test "incomplete clause left side guard" do
          code = """
          fn
            x when \
          """

          assert {_meta, env} = get_cursor_env(code)
          assert Enum.any?(env.vars, &(&1.name == :x))
        end
      end
    end

    test "incomplete clause right side" do
      code = """
      fn
        x -> __cursor__()
      end
      """

      assert {_meta, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert Enum.any?(env.vars, &(&1.name == :x))
      end
    end
  end

  describe "capture" do
    test "empty" do
      code = """
      &\
      """

      assert get_cursor_env(code)
    end

    test "local" do
      code = """
      &foo\
      """

      assert get_cursor_env(code)
    end

    test "local slash no arity" do
      code = """
      &foo/\
      """

      assert get_cursor_env(code)
    end

    test "local slash arity" do
      code = """
      &foo/1\
      """

      assert get_cursor_env(code)
    end

    test "local slash invalid arity" do
      code = """
      &foo/1000; \
      """

      assert get_cursor_env(code)
    end

    test "local dot" do
      code = """
      &foo.\
      """

      assert get_cursor_env(code)
    end

    test "local dot call" do
      code = """
      &foo.(\
      """

      assert get_cursor_env(code)
    end

    test "local dot call closed" do
      code = """
      &foo.()\
      """

      assert get_cursor_env(code)
    end

    test "local dot right" do
      code = """
      &foo.bar\
      """

      assert get_cursor_env(code)
    end

    test "remote" do
      code = """
      &Foo\
      """

      assert get_cursor_env(code)
    end

    test "remote dot" do
      code = """
      &Foo.\
      """

      assert get_cursor_env(code)
    end

    test "remote dot right" do
      code = """
      &Foo.bar\
      """

      assert get_cursor_env(code)
    end

    test "remote dot right no arity" do
      code = """
      &Foo.bar/\
      """

      assert get_cursor_env(code)
    end

    test "remote dot right arity" do
      code = """
      &Foo.bar/1\
      """

      assert get_cursor_env(code)
    end

    test "remote dot call" do
      code = """
      &Foo.bar(\
      """

      assert get_cursor_env(code)
    end

    test "remote dot call closed" do
      code = """
      &Foo.bar()\
      """

      assert get_cursor_env(code)
    end

    test "tuple" do
      code = """
      &{\
      """

      assert get_cursor_env(code)
    end

    test "tuple closed" do
      code = """
      &{}\
      """

      assert get_cursor_env(code)
    end

    test "list" do
      code = """
      &[\
      """

      assert get_cursor_env(code)
    end

    test "list closed" do
      code = """
      &[]\
      """

      assert get_cursor_env(code)
    end

    test "bitstring" do
      code = """
      &<<\
      """

      assert get_cursor_env(code)
    end

    test "bitstring closed" do
      code = """
      &<<>>\
      """

      assert get_cursor_env(code)
    end

    test "map no braces" do
      code = """
      &%\
      """

      assert get_cursor_env(code)
    end

    test "map" do
      code = """
      &%{\
      """

      assert get_cursor_env(code)
    end

    test "map closed" do
      code = """
      &%{}\
      """

      assert get_cursor_env(code)
    end

    test "struct no braces" do
      code = """
      &%Foo\
      """

      assert get_cursor_env(code)
    end

    test "struct" do
      code = """
      &%Foo{\
      """

      assert get_cursor_env(code)
    end

    test "struct closed" do
      code = """
      &%Foo{}\
      """

      assert get_cursor_env(code)
    end

    test "block" do
      code = """
      & (\
      """

      assert get_cursor_env(code)
    end

    test "block multiple expressions" do
      code = """
      & (:ok; \
      """

      assert get_cursor_env(code)
    end

    test "arg var incomplete" do
      code = """
      & &\
      """

      assert get_cursor_env(code)
    end

    test "arg var" do
      code = """
      & &2\
      """

      assert get_cursor_env(code)
    end

    test "arg var in list" do
      code = """
      &[&1, \
      """

      assert get_cursor_env(code)
    end

    test "arg var in list without predecessor" do
      code = """
      &[&2, \
      """

      assert get_cursor_env(code)
    end

    test "no arg" do
      code = """
      &{}; \
      """

      assert get_cursor_env(code)
    end

    test "invalid arg number" do
      code = """
      & &0; \
      """

      assert get_cursor_env(code)
    end

    test "outside of capture" do
      code = """
      &1; \
      """

      assert get_cursor_env(code)
    end

    test "invalid arg local" do
      code = """
      &foo; \
      """

      assert get_cursor_env(code)
    end

    test "invalid arg" do
      code = """
      &"foo"; \
      """

      assert get_cursor_env(code)
    end

    test "undefined local capture" do
      code = """
      defmodule A do
        (&asdf/1) +\
      """

      assert get_cursor_env(code)
    end

    test "ambiguous local" do
      code = """
      defmodule Kernel.ErrorsTest.FunctionImportConflict do
        import :erlang, only: [exit: 1], warn: false
        def foo, do: (&exit/1) +\
      """

      assert get_cursor_env(code)
    end
  end

  describe "pin" do
    test "outside of match" do
      code = """
      ^\
      """

      assert get_cursor_env(code)
    end

    test "cursor in match" do
      code = """
      ^__cursor__() = x\
      """

      assert get_cursor_env(code)
    end
  end

  describe "map" do
    test "invalid key in match" do
      code = """
      %{foo => x} = x\
      """

      assert get_cursor_env(code)
    end

    test "update in match" do
      code = """
      %{a | x: __cursor__()} = x\
      """

      assert get_cursor_env(code)
    end

    test "cursor in place of key value pair" do
      code = """
      %{a: "123", \
      """

      assert get_cursor_env(code)
    end
  end

  describe "struct" do
    test "no map" do
      code = """
      %\
      """

      assert get_cursor_env(code)
    end

    test "invalid map name" do
      code = """
      %foo{\
      """

      assert get_cursor_env(code)
    end

    test "invalid key" do
      code = """
      %Foo{"asd" => [\
      """

      assert get_cursor_env(code)
    end
  end

  describe "bitstring" do
    test "no size specifier with unit" do
      code = """
      <<x::unit(8), \
      """

      assert {_meta, _env} = get_cursor_env(code)
    end

    test "invalid float size" do
      code = """
      <<12.3::32*4, \
      """

      assert get_cursor_env(code)
    end

    test "signed binary" do
      code = """
      <<x::binary-signed, \
      """

      assert get_cursor_env(code)
    end

    test "signed utf" do
      code = """
      <<x::utf8-signed, \
      """

      assert get_cursor_env(code)
    end

    test "utf with size" do
      code = """
      <<x::utf8-size(1), \
      """

      assert get_cursor_env(code)
    end

    test "conflicting type" do
      code = """
      <<"foo"::integer, \
      """

      assert get_cursor_env(code)
    end

    test "conflicting endianness" do
      code = """
      <<1::little-big, \
      """

      assert get_cursor_env(code)
    end

    test "conflicting unit" do
      code = """
      <<x::bitstring-unit(2), \
      """

      assert get_cursor_env(code)
    end

    test "binary literal with unit" do
      code = """
      <<"foo"::32, \
      """

      assert get_cursor_env(code)
    end

    test "bitstring literal with unit" do
      code = """
      <<(<<>>)::32, \
      """

      assert get_cursor_env(code)
    end

    test "unsized" do
      code = """
      <<x::binary, "foo" >> = \
      """

      assert get_cursor_env(code)
    end

    test "bad argument" do
      code = """
      <<"foo"::size(8)-unit(:oops), \
      """

      assert get_cursor_env(code)
    end

    test "undefined" do
      code = """
      <<1::unknown(), \
      """

      assert get_cursor_env(code)
    end

    test "unknown" do
      code = """
      <<1::refb_spec, \
      """

      assert get_cursor_env(code)
    end

    test "invalid literal" do
      code = """
      <<:ok, \
      """

      assert get_cursor_env(code)
    end

    test "nested match" do
      code = """
      <<bar = baz>> = \
      """

      assert get_cursor_env(code)
    end

    test "incomplete" do
      code = """
      <<\
      """

      assert get_cursor_env(code)
    end

    test "incomplete ::" do
      code = """
      <<1::\
      """

      assert get_cursor_env(code)
    end

    test "incomplete -" do
      code = """
      <<1::binary-\
      """

      assert get_cursor_env(code)
    end

    test "incomplete open parens" do
      code = """
      <<1::size(\
      """

      assert get_cursor_env(code)
    end
  end

  describe "quote/unquote/unquote_splicing" do
    test "invalid bind quoted" do
      code = """
      quote [bind_quoted: 123] do\
      """

      assert get_cursor_env(code)
    end

    test "incomplete 1" do
      code = """
      quote \
      """

      assert get_cursor_env(code)
    end

    test "incomplete 2" do
      code = """
      quote [\
      """

      assert get_cursor_env(code)
    end

    test "incomplete 3" do
      code = """
      quote [bind_quoted: \
      """

      assert get_cursor_env(code)
    end

    test "incomplete 4" do
      code = """
      quote [bind_quoted: [\
      """

      assert get_cursor_env(code)
    end

    test "incomplete 5" do
      code = """
      quote [bind_quoted: [asd: \
      """

      assert get_cursor_env(code)
    end

    test "incomplete 6" do
      code = """
      quote [bind_quoted: [asd: 1]], \
      """

      assert get_cursor_env(code)
    end

    test "incomplete 7" do
      code = """
      quote [bind_quoted: [asd: 1]], [\
      """

      assert get_cursor_env(code)
    end

    test "incomplete 8" do
      code = """
      quote :foo, [\
      """

      assert get_cursor_env(code)
    end

    test "in do block" do
      code = """
      quote do
        \
      """

      assert get_cursor_env(code)
    end

    test "in do block unquote" do
      code = """
      quote do
        unquote(\
      """

      assert get_cursor_env(code)
    end

    test "in do block unquote_splicing" do
      code = """
      quote do
        unquote_splicing(\
      """

      assert get_cursor_env(code)
    end

    test "in do block unquote with bind_quoted" do
      code = """
      quote bind_quoted: [a: 1] do
        unquote(\
      """

      assert get_cursor_env(code)
    end

    test "unquote without quote" do
      code = """
      unquote(\
      """

      assert get_cursor_env(code)
    end

    test "invalid compile option" do
      code = """
      quote [file: 1] do\
      """

      assert get_cursor_env(code)
    end

    test "invalid runtime option" do
      code = """
      quote [unquote: 1] do\
      """

      assert get_cursor_env(code)
    end

    test "unquote_splicing not in block" do
      code = """
      quote do: unquote_splicing(\
      """

      assert get_cursor_env(code)
    end
  end

  describe "calls" do
    test "invalid anonymous call" do
      code = """
      :foo.(a, \
      """

      assert get_cursor_env(code)
    end

    test "anonymous call in match" do
      code = """
      a.() = \
      """

      assert get_cursor_env(code)
    end

    test "anonymous call in guard" do
      code = """
      case x do
        y when a.() -> \
      """

      assert get_cursor_env(code)
    end

    test "parens map lookup guard" do
      code = """
      case x do
        y when map.field() -> \
      """

      assert get_cursor_env(code)
    end

    test "remote call in match" do
      code = """
      Foo.bar() = \
      """

      assert get_cursor_env(code)
    end

    test "invalid remote call" do
      code = """
      __ENV__.line.foo \
      """

      assert get_cursor_env(code)
    end

    test "clause in remote call" do
      code = """
      Foo.foo do
        a -> \
      """

      assert get_cursor_env(code)
    end

    test "invalid local call" do
      code = """
      1.foo \
      """

      assert get_cursor_env(code)
    end

    test "ambiguous local call" do
      code = """
      a = 1
      a -1 .. \
      """

      assert get_cursor_env(code)
    end

    test "clause in local call" do
      code = """
      foo do
        a -> \
      """

      assert get_cursor_env(code)
    end

    test "local call in match" do
      code = """
      bar() = \
      """

      assert get_cursor_env(code)
    end

    test "ambiguous call" do
      code = """
      defmodule Kernel.ErrorsTest.FunctionImportConflict do
        import :erlang, only: [exit: 1], warn: false
        def foo, do: exit(\
      """

      assert get_cursor_env(code)
    end

    # this is not crashing because we don't support local macros yet
    test "conflicting macro" do
      code = """
      defmodule Kernel.ErrorsTest.MacroLocalConflict do
        def hello, do: 1 || 2
        defmacro _ || _, do: :ok

        defmacro _ && _, do: :error
        def world, do: 1 && \
      """

      assert get_cursor_env(code)
    end

    test "invalid call cursor" do
      code = """
      __cursor__(a.b)()
      """

      assert get_cursor_env(code, true)
    end
  end

  describe "__ENV__, __MODULE__, __CALLER__, __STACKTRACE__, __DIR__" do
    test "__ENV__ in match" do
      code = """
      __ENV__ = \
      """

      assert get_cursor_env(code)
    end

    test "__CALLER__ not in macro" do
      code = """
      inspect(__CALLER__, \
      """

      assert get_cursor_env(code)
    end

    test "__STACKTRACE__ outside of catch/rescue" do
      code = """
      inspect(__STACKTRACE__, \
      """

      assert get_cursor_env(code)
    end

    test "__MODULE__ outside of module" do
      code = """
      inspect(__MODULE__, \
      """

      assert get_cursor_env(code)
    end

    test "__DIR__ when nofile" do
      code = """
      inspect(__DIR__, \
      """

      assert get_cursor_env(code)
    end
  end

  describe "alias/import/require" do
    test "invalid alias expansion" do
      code = """
      foo = :foo
      foo.Foo.a(\
      """

      assert get_cursor_env(code)
    end

    test "incomplete" do
      code = """
      alias \
      """

      assert get_cursor_env(code)

      code = """
      require \
      """

      assert get_cursor_env(code)

      code = """
      import \
      """

      assert get_cursor_env(code)
    end

    test "multi" do
      code = """
      alias ElixirSenseExample\
      """

      assert get_cursor_env(code)

      code = """
      alias ElixirSenseExample.\
      """

      assert get_cursor_env(code)

      code = """
      alias ElixirSenseExample.{\
      """

      assert get_cursor_env(code)

      code = """
      alias ElixirSenseExample.{S\
      """

      assert get_cursor_env(code)

      code = """
      alias ElixirSenseExample.{Some, \
      """

      assert get_cursor_env(code)

      code = """
      alias ElixirSenseExample.{Some, Mod\
      """

      assert get_cursor_env(code)
    end

    test "invalid" do
      code = """
      alias A.a\
      """

      assert get_cursor_env(code)

      code = """
      require A.a\
      """

      assert get_cursor_env(code)

      code = """
      import A.a\
      """

      assert get_cursor_env(code)
    end

    test "in options" do
      code = """
      alias A.B, \
      """

      assert get_cursor_env(code)

      code = """
      require A.B, \
      """

      assert get_cursor_env(code)

      code = """
      import A.B, \
      """

      assert get_cursor_env(code)
    end

    test "in option" do
      code = """
      alias A.B, warn: \
      """

      assert get_cursor_env(code)

      code = """
      require A.B, warn: \
      """

      assert get_cursor_env(code)

      code = """
      import A.B, warn: \
      """

      assert get_cursor_env(code)
    end
  end

  describe "super" do
    test "call outside module" do
      code = """
      super(\
      """

      assert get_cursor_env(code)
    end

    test "call outside function" do
      code = """
      defmodule A do
        super(\
      """

      assert get_cursor_env(code)
    end

    test "call in match" do
      code = """
      super() = \
      """

      assert get_cursor_env(code)
    end

    test "capture expression outside module" do
      code = """
      & super(&1, \
      """

      assert get_cursor_env(code)
    end

    test "capture outside module" do
      code = """
      &super\
      """

      assert get_cursor_env(code)

      code = """
      &super/\
      """

      assert get_cursor_env(code)

      code = """
      &super/1 \
      """

      assert get_cursor_env(code)

      code = """
      (&super/1) +\
      """

      assert get_cursor_env(code)
    end

    test "call wrong args" do
      code = """
      defmodule A do
        def a do
          super(\
      """

      assert get_cursor_env(code)
    end

    test "call no super" do
      code = """
      defmodule A do
        def a(1), do: :ok
        def a(x) do
          super(x) +\
      """

      assert get_cursor_env(code)
    end
  end

  describe "var" do
    test "_ in cond" do
      code = """
      cond do
        x -> x
        _ -> \
      """

      assert get_cursor_env(code)
    end

    test "_ outside of match" do
      code = """
      {1, _, [\
      """

      assert get_cursor_env(code)
    end

    test "parallel bitstring match" do
      code = """
      <<foo>> = <<baz>> = \
      """

      assert get_cursor_env(code)
    end

    test "match in guard" do
      code = """
      case 1 do
        x when x = \
      """

      assert get_cursor_env(code)
    end

    test "stacktrace in match" do
      code = """
      __STACKTRACE__ = \
      """

      assert get_cursor_env(code)
    end

    test "duplicate match" do
      code = """
      case 1 do
        (x = x) = :ok -> \
      """

      assert get_cursor_env(code)
    end
  end

  describe "typespec" do
    test "in type name" do
      code = """
      defmodule Abc do
        @type foo\
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.typespec == {:__unknown__, 0}
      end
    end

    test "in spec name" do
      code = """
      defmodule Abc do
        @spec foo\
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.typespec == {:__unknown__, 0}
      end
    end

    test "in type after ::" do
      code = """
      defmodule Abc do
        @type foo :: \
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.typespec == {:foo, 0}
      end
    end

    test "in spec after ::" do
      code = """
      defmodule Abc do
        @spec foo :: \
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.typespec == {:foo, 0}
      end
    end

    test "in type after :: type" do
      code = """
      defmodule Abc do
        @type foo :: bar\
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.typespec == {:foo, 0}
      end
    end

    test "in type after :: type with | empty" do
      code = """
      defmodule Abc do
        @type foo :: bar | \
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.typespec == {:foo, 0}
      end
    end

    test "in type after :: type with |" do
      code = """
      defmodule Abc do
        @type foo :: bar | baz\
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.typespec == {:foo, 0}
      end
    end

    test "in type after :: type with fun" do
      code = """
      defmodule Abc do
        @type foo :: (...\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type after :: type with fun ->" do
      code = """
      defmodule Abc do
        @type foo :: (... -> \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type after :: type with fun -> no arg" do
      code = """
      defmodule Abc do
        @type foo :: (-> \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type after :: type with fun (" do
      code = """
      defmodule Abc do
        @type foo :: (\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    if Version.match?(System.version(), ">= 1.17.0") do
      if Version.match?(System.version(), ">= 1.18.0") do
        test "in type after :: type with fun ( next arg" do
          code = """
          defmodule Abc do
            @type foo :: a(bar, \
          """

          assert {_, env} = get_cursor_env(code, false, ")\nend")
          assert env.typespec == {:foo, 0}
        end
      else
        test "in type after :: type with fun ( next arg" do
          code = """
          defmodule Abc do
            @type foo :: a(bar, \
          """

          assert {_, env} = get_cursor_env(code)
          assert env.typespec == {:foo, 0}
        end
      end
    end

    test "in type after :: type with map empty" do
      code = """
      defmodule Abc do
        @type foo :: %{\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type after :: type with map key" do
      code = """
      defmodule Abc do
        @type foo :: %{bar\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type after :: type with map after key" do
      code = """
      defmodule Abc do
        @type foo :: %{bar: \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type after :: type with map after =>" do
      code = """
      defmodule Abc do
        @type foo :: %{:bar => \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type after :: type with map optional" do
      code = """
      defmodule Abc do
        @type foo :: %{optional(\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type after :: type named empty" do
      code = """
      defmodule Abc do
        @type foo :: {bar :: \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type after :: type named" do
      code = """
      defmodule Abc do
        @type foo :: {bar :: baz\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in spec after :: type" do
      code = """
      defmodule Abc do
        @spec foo :: bar\
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.typespec == {:foo, 0}
      end
    end

    test "in type after :: remote type" do
      code = """
      defmodule Abc do
        @type foo :: Remote.bar\
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.typespec == {:foo, 0}
      end
    end

    test "in type after :: type args" do
      code = """
      defmodule Abc do
        @type foo :: Remote.bar(\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in spec after :: type args" do
      code = """
      defmodule Abc do
        @spec foo :: Remote.bar(\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type arg empty" do
      code = """
      defmodule Abc do
        @type foo(\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 1}
    end

    test "in spec arg empty" do
      code = """
      defmodule Abc do
        @spec foo(\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 1}
    end

    test "in type arg" do
      code = """
      defmodule Abc do
        @type foo(bar\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 1}
    end

    test "in spec arg" do
      code = """
      defmodule Abc do
        @spec foo(bar\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 1}
    end

    test "in spec arg named empty" do
      code = """
      defmodule Abc do
        @spec foo(bar :: \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 1}
    end

    test "in spec arg named" do
      code = """
      defmodule Abc do
        @spec foo(bar :: baz\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 1}
    end

    test "in type arg next" do
      code = """
      defmodule Abc do
        @type foo(asd, \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 2}
    end

    test "in spec when" do
      code = """
      defmodule Abc do
        @spec foo(a) :: integer when \
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.typespec == {:foo, 1}
      end
    end

    test "in spec when after :" do
      code = """
      defmodule Abc do
        @spec foo(a) :: integer when x: \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 1}
    end

    test "in spec when after : type" do
      code = """
      defmodule Abc do
        @spec foo(a) :: integer when x: bar\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 1}
    end

    test "in spec when after : type arg" do
      code = """
      defmodule Abc do
        @spec foo(a) :: integer when x: bar(\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 1}
    end

    test "in spec when after : next" do
      code = """
      defmodule Abc do
        @spec foo(a) :: integer when x: bar(), \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 1}
    end

    test "in type invalid expression" do
      code = """
      defmodule Abc do
        @type [{\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:__unknown__, 0}
    end

    test "in spec invalid expression" do
      code = """
      defmodule Abc do
        @spec [{\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:__unknown__, 0}
    end

    test "redefining built in" do
      code = """
      defmodule Abc do
        @type required(a) :: \
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.typespec == {:__required__, 1}
      end
    end

    test "in type list" do
      code = """
      defmodule Abc do
        @type foo :: [\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type list next" do
      code = """
      defmodule Abc do
        @type foo :: [:foo, \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type list keyword" do
      code = """
      defmodule Abc do
        @type foo :: [foo: \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type tuple" do
      code = """
      defmodule Abc do
        @type foo :: {\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type tuple next" do
      code = """
      defmodule Abc do
        @type foo :: {:foo, \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type union" do
      code = """
      defmodule Abc do
        @type foo :: :foo | \
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.typespec == {:foo, 0}
      end
    end

    test "in type bitstring" do
      code = """
      defmodule Abc do
        @type foo :: <<\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type bitstring after ::" do
      code = """
      defmodule Abc do
        @type foo :: <<_::\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    # test "in type bitstring next" do
    #   code = """
    #   defmodule Abc do
    #     @type foo :: <<_::, \
    #   """

    #   assert {_, env} = get_cursor_env(code)
    #   assert env.typespec == {:foo, 0}
    # end

    test "in type bitstring next after" do
      code = """
      defmodule Abc do
        @type foo :: <<_::size, _::_*\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type struct" do
      code = """
      defmodule Abc do
        @type foo :: %\
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.typespec == {:foo, 0}
      end
    end

    test "in type struct {}" do
      code = """
      defmodule Abc do
        @type foo :: %Date{\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type struct key" do
      code = """
      defmodule Abc do
        @type foo :: %Date{key: \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.typespec == {:foo, 0}
    end

    test "in type range" do
      code = """
      defmodule Abc do
        @type foo :: 1..\
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.typespec == {:foo, 0}
      end
    end

    test "type with underscored arg" do
      code = """
      defmodule Abc do
        @type foo(_) :: 1..\
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.typespec == {:foo, 1}
      end
    end
  end

  describe "def" do
    test "in def" do
      code = """
      defmodule Abc do
        def\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.module == Abc
    end

    test "in def name" do
      code = """
      defmodule Abc do
        def foo\
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.function == {:__unknown__, 0}
      end
    end

    test "in def args" do
      code = """
      defmodule Abc do
        def foo(\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.function == {:foo, 1}
    end

    test "in def arg" do
      code = """
      defmodule Abc do
        def foo(some\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.function == {:foo, 1}
    end

    test "in def arg next" do
      code = """
      defmodule Abc do
        def foo(some, \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.function == {:foo, 2}
    end

    test "in def after args" do
      code = """
      defmodule Abc do
        def foo(some) \
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.function == {:__unknown__, 0}
      end
    end

    test "in def after," do
      code = """
      defmodule Abc do
        def foo(some), \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.function == {:foo, 1}
    end

    test "in def after do:" do
      code = """
      defmodule Abc do
        def foo(some), do: \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.function == {:foo, 1}
    end

    test "in def after do" do
      code = """
      defmodule Abc do
        def foo(some) do
          \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.function == {:foo, 1}
    end

    test "in def guard" do
      code = """
      defmodule Abc do
        def foo(some) when \
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.function == {:foo, 1}
        assert env.context == :guard
      end
    end

    test "in def guard variable" do
      code = """
      defmodule Abc do
        def foo(some) when some\
      """

      assert {_, env} = get_cursor_env(code)

      if Version.match?(System.version(), ">= 1.15.0") do
        assert env.function == {:foo, 1}
        assert env.context == :guard
      end
    end

    test "in def after block" do
      code = """
      defmodule Abc do
        def foo(some) do
          :ok
        after
          \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.function == {:foo, 1}
    end
  end

  describe "defmodule" do
    test "in defmodule" do
      code = """
      defmodule\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.module == nil
    end

    test "in defmodule alias" do
      code = """
      defmodule A\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.module == nil
    end

    test "in defmodule after do:" do
      code = """
      defmodule Abc, do: \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.module == Abc
    end

    test "in defmodule after do" do
      code = """
      defmodule Abc do\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.module == Abc
    end

    test "in defmodule invalid alias" do
      code = """
      defmodule 123, do: \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.module == :"Elixir.__Unknown__"
    end
  end

  describe "attribute" do
    test "after @" do
      code = """
      defmodule Abc do
        @\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.module == Abc
    end

    test "in name" do
      code = """
      defmodule Abc do
        @foo\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.module == Abc
    end

    test "after name" do
      code = """
      defmodule Abc do
        @foo \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.module == Abc
    end

    test "outside module" do
      code = """
      @foo [\
      """

      assert {_, env} = get_cursor_env(code)
      assert env.module == nil
    end

    test "setting inside def" do
      code = """
      defmodule Abc do
        def go do
          @foo \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.module == Abc
    end

    test "invalid args" do
      code = """
      defmodule Abc do
        @
        def init(id) do
          {:ok,
            %Some.Mod{
              id: id,
              events: [],
              version: __cursor__()
            }}
        end
        \
      """

      assert {_, env} = get_cursor_env(code)
      assert env.module == Abc
    end
  end

  test "defimpl for" do
    code = """
    defimpl Enumerable, for: \
    """

    assert {_, env} = get_cursor_env(code)
    assert env.module == nil
  end
end
