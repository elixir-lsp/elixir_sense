if Version.match?(System.version(), ">= 1.17.0-dev") do
  defmodule ElixirSense.Core.CompilerTest do
    use ExUnit.Case, async: true
    alias ElixirSense.Core.Compiler
    alias ElixirSense.Core.State
    require Record

    defp to_quoted!(string_or_ast, ast \\ false)
    defp to_quoted!(ast, true), do: ast

    defp to_quoted!(string, false),
      do: Code.string_to_quoted!(string, columns: true, token_metadata: true)

    Record.defrecordp(:elixir_ex,
      caller: false,
      prematch: :raise,
      stacktrace: false,
      unused: {%{}, 0},
      runtime_modules: [],
      vars: {%{}, false}
    )

    defp elixir_ex_to_map(
           elixir_ex(
             caller: caller,
             prematch: prematch,
             stacktrace: stacktrace,
             unused: unused,
             runtime_modules: runtime_modules,
             vars: vars
           )
         ) do
      %{
        caller: caller,
        prematch: prematch,
        stacktrace: stacktrace,
        unused: unused,
        runtime_modules: runtime_modules,
        vars: vars
      }
    end

    defp state_to_map(%State{} = state) do
      Map.take(state, [:caller, :prematch, :stacktrace, :unused, :runtime_modules, :vars])
    end

    defp expand(ast) do
      Compiler.expand(ast, %State{}, Compiler.env())
    end

    defp elixir_expand(ast) do
      env = :elixir_env.new()
      :elixir_expand.expand(ast, :elixir_env.env_to_ex(env), env)
    end

    defmacrop assert_expansion(code, ast \\ false) do
      quote do
        ast = to_quoted!(unquote(code), unquote(ast))
        {elixir_expanded, elixir_state, elixir_env} = elixir_expand(ast)
        dbg(elixir_expanded)
        {expanded, state, env} = expand(ast)
        dbg(expanded)

        assert expanded == elixir_expanded
        assert env == elixir_env
        assert state_to_map(state) == elixir_ex_to_map(elixir_state)
      end
    end

    defmacrop assert_expansion_env(code, ast \\ false) do
      quote do
        ast = to_quoted!(unquote(code), unquote(ast))
        {elixir_expanded, elixir_state, elixir_env} = elixir_expand(ast)
        dbg(elixir_expanded)
        dbg(elixir_ex_to_map(elixir_state))
        {expanded, state, env} = expand(ast)
        dbg(expanded)
        dbg(state_to_map(state))

        assert env == elixir_env
        assert state_to_map(state) == elixir_ex_to_map(elixir_state)
      end
    end

    test "initial" do
      elixir_env = :elixir_env.new()
      assert Compiler.env() == elixir_env
      assert state_to_map(%State{}) == elixir_ex_to_map(:elixir_env.env_to_ex(elixir_env))
    end

    describe "special forms" do
      test "expands =" do
        assert_expansion("1 = 1")
      end

      test "expands {}" do
        assert_expansion("{}")
        assert_expansion("{1, 2, 3}")
        assert_expansion("{a, b} = {:ok, 1}")
      end

      test "expands %{}" do
        assert_expansion("%{1 => 2}")
        assert_expansion("%{a: 3}")
        assert_expansion("%{a: a} = %{}")
        assert_expansion("%{1 => a} = %{}")
        assert_expansion("%{%{a: 1} | a: 2}")
      end

      test "expands %" do
        assert_expansion("%Date{year: 2024, month: 2, day: 18}")
        assert_expansion("%Date{calendar: Calendar.ISO, year: 2024, month: 2, day: 18}")
        assert_expansion("%{year: x} = %Date{year: 2024, month: 2, day: 18}")
        assert_expansion("%Date{%Date{year: 2024, month: 2, day: 18} | day: 1}")
        assert_expansion("%{%Date{year: 2024, month: 2, day: 18} | day: 1}")
      end

      test "expands <<>>" do
        assert_expansion("<<>>")
        assert_expansion("<<1>>")
        assert_expansion("<<x, rest::binary>> = \"\"")
      end

      test "expands __block__" do
        assert_expansion({:__block__, [], []}, true)
        assert_expansion({:__block__, [], [1]}, true)
        assert_expansion({:__block__, [], [1, 2]}, true)
      end

      test "expands __aliases__" do
        assert_expansion({:__aliases__, [], [:Asd, :Foo]}, true)
        assert_expansion({:__block__, [], [:Asd]}, true)
        assert_expansion({:__block__, [], [Elixir, :Asd]}, true)
      end

      test "expands alias" do
        assert_expansion("alias Foo")
        assert_expansion("alias Foo.Bar")
        assert_expansion("alias Foo.Bar, as: Baz")
      end

      test "expands require" do
        assert_expansion("require Code")
        assert_expansion("require Code.Fragment")
        assert_expansion("require Code.Fragment, as: Baz")
      end

      test "expands import" do
        assert_expansion("import Code")
        assert_expansion("import Code.Fragment")
        assert_expansion("import Code.Fragment, only: :functions")
      end

      test "expands multi alias" do
        assert_expansion("alias Foo.{Bar, Some.Other}")
      end

      test "expands __MODULE__" do
        ast = {:__MODULE__, [], nil}
        {expanded, state, env} = Compiler.expand(ast, %State{}, %{Compiler.env() | module: Foo})
        elixir_env = %{:elixir_env.new() | module: Foo}

        {elixir_expanded, elixir_state, elixir_env} =
          :elixir_expand.expand(ast, :elixir_env.env_to_ex(elixir_env), elixir_env)

        assert expanded == elixir_expanded
        assert env == elixir_env
        assert state_to_map(state) == elixir_ex_to_map(elixir_state)
      end

      test "expands __DIR__" do
        ast = {:__DIR__, [], nil}

        {expanded, state, env} =
          Compiler.expand(ast, %State{}, %{Compiler.env() | file: __ENV__.file})

        elixir_env = %{:elixir_env.new() | file: __ENV__.file}

        {elixir_expanded, elixir_state, elixir_env} =
          :elixir_expand.expand(ast, :elixir_env.env_to_ex(elixir_env), elixir_env)

        assert expanded == elixir_expanded
        assert env == elixir_env
        assert state_to_map(state) == elixir_ex_to_map(elixir_state)
      end

      test "expands __CALLER__" do
        ast = {:__CALLER__, [], nil}
        {expanded, state, env} = Compiler.expand(ast, %State{caller: true}, Compiler.env())
        elixir_env = :elixir_env.new()

        {elixir_expanded, elixir_state, elixir_env} =
          :elixir_expand.expand(
            ast,
            elixir_ex(:elixir_env.env_to_ex(elixir_env), caller: true),
            elixir_env
          )

        assert expanded == elixir_expanded
        assert env == elixir_env
        assert state_to_map(state) == elixir_ex_to_map(elixir_state)
      end

      test "expands __STACKTRACE__" do
        ast = {:__STACKTRACE__, [], nil}
        {expanded, state, env} = Compiler.expand(ast, %State{stacktrace: true}, Compiler.env())
        elixir_env = :elixir_env.new()

        {elixir_expanded, elixir_state, elixir_env} =
          :elixir_expand.expand(
            ast,
            elixir_ex(:elixir_env.env_to_ex(elixir_env), stacktrace: true),
            elixir_env
          )

        assert expanded == elixir_expanded
        assert env == elixir_env
        assert state_to_map(state) == elixir_ex_to_map(elixir_state)
      end

      test "expands __ENV__" do
        ast = {:__ENV__, [], nil}
        {expanded, state, env} = Compiler.expand(ast, %State{}, Compiler.env())
        elixir_env = :elixir_env.new()

        {elixir_expanded, elixir_state, elixir_env} =
          :elixir_expand.expand(ast, :elixir_env.env_to_ex(elixir_env), elixir_env)

        assert expanded == elixir_expanded
        assert env == elixir_env
        assert state_to_map(state) == elixir_ex_to_map(elixir_state)
      end

      test "expands __ENV__.property" do
        assert_expansion("__ENV__.requires")
        assert_expansion("__ENV__.foo")
      end

      test "expands quote literal" do
        assert_expansion("quote do: 2")
        assert_expansion("quote do: :foo")
        assert_expansion("quote do: \"asd\"")
        assert_expansion("quote do: []")
        assert_expansion("quote do: [12]")
        assert_expansion("quote do: [12, 34]")
        assert_expansion("quote do: [12 | 34]")
        assert_expansion("quote do: [12 | [34]]")
        assert_expansion("quote do: {12}")
        assert_expansion("quote do: {12, 34}")
        assert_expansion("quote do: %{a: 12}")
      end

      test "expands quote variable" do
        assert_expansion("quote do: abc")
      end

      test "expands quote quote" do
        assert_expansion("""
        quote do: (quote do: 1)
        """)
      end

      test "expands quote block" do
        assert_expansion("""
        quote do: ()
        """)
      end

      test "expands quote unquote" do
        assert_expansion("""
        a = 1
        quote do: unquote(a)
        """)
      end

      test "expands quote unquote block" do
        assert_expansion("""
        a = 1
        quote do: (unquote(a))
        """)
      end

      test "expands quote unquote_splicing tuple" do
        assert_expansion("""
        quote do: {unquote_splicing([1, 2]), unquote_splicing([2])}
        """)
      end

      test "expands quote unquote_splicing" do
        assert_expansion("""
        a = [1, 2, 3]
        quote do: (unquote_splicing(a))
        """)
      end

      test "expands quote alias" do
        assert_expansion("quote do: Date")
        assert_expansion("quote do: Elixir.Date")
        assert_expansion("quote do: String.Chars")
        assert_expansion("alias String.Chars; quote do: Chars")
        assert_expansion("alias String.Chars; quote do: Chars.foo().A")
      end

      test "expands quote import" do
        assert_expansion("quote do: inspect(1)")
        assert_expansion("quote do: &inspect/1")
      end

      test "expands &super" do
        assert_expansion_env("""
        defmodule Abc do
          use ElixirSense.Core.CompilerTest.Overridable
          
          def foo(a) do
            &super/1
          end
        end
        """)

        assert_expansion_env("""
        defmodule Abc do
          use ElixirSense.Core.CompilerTest.Overridable
          
          def foo(a) do
            &super(&1)
          end
        end
        """)
      end

      test "expands &" do
        assert_expansion("& &1")
        assert_expansion("&Enum.take(&1, 5)")
        assert_expansion("&{&1, &2}")
        assert_expansion("&[&1 | &2]")
        assert_expansion("&inspect/1")
        assert_expansion("&Enum.count/1")
      end

      test "expands fn" do
        assert_expansion("fn -> 1 end")
        assert_expansion("fn a, b -> {a, b} end")

        assert_expansion("""
        fn
          1 -> 1
          a -> a
        end
        """)
      end

      test "expands cond" do
        assert_expansion("""
        cond do
          nil -> 0
          true -> 1
        end
        """)
      end

      test "expands case" do
        assert_expansion("""
        case 1 do
          0 -> 0
          1 -> 1
        end
        """)
      end

      test "expands try" do
        assert_expansion("""
        try do
          inspect(1)
        rescue
          e in ArgumentError ->
            e
        catch
          {k, e} ->
            {k, e}
        else
          _ -> :ok
        after
          IO.puts("")
        end
        """)
      end

      test "expands receive" do
        assert_expansion("""
        receive do
          x -> x
        after
          100 -> IO.puts("")
        end
        """)
      end

      test "expands for" do
        assert_expansion("""
        for i <- [1, 2, 3] do
          i
        end
        """)

        assert_expansion("""
        for i <- [1, 2, 3], j <- [1, 2], true, into: %{}, do: {i, j}
        """)
      end

      test "expands with" do
        assert_expansion("""
        with i <- :ok do
          i
        end
        """)

        assert_expansion("""
        with :ok <- :ok, j = 5 do
          j
        else
          a -> a
        end
        """)
      end

      defmodule Overridable do
        defmacro __using__(args) do
          quote do
            def foo(a) do
              a
            end

            defmacro bar(ast) do
              ast
            end

            defoverridable foo: 1, bar: 1
          end
        end
      end

      test "expands super" do
        assert_expansion_env("""
        defmodule Abc do
          use ElixirSense.Core.CompilerTest.Overridable
          
          def foo(a) do
            super(a + 1)
          end

          defmacro bar(a) do
            quote do
              unquote(super(b)) - 1
            end
          end
        end
        """)
      end

      test "expands var" do
        assert_expansion("_ = 5")
        assert_expansion("a = 5")
        assert_expansion("a = 5; a")
        assert_expansion("a = 5; ^a = 6")
      end

      test "expands local call" do
        assert_expansion("get_in(%{}, [:bar])")
        assert_expansion("length([])")
      end

      test "expands local call macro" do
        # TODO
        # assert_expansion("if true, do: :ok")
        assert_expansion("1 |> IO.inspect")
      end

      test "expands remote call" do
        assert_expansion("Kernel.get_in(%{}, [:bar])")
        assert_expansion("Kernel.length([])")
        assert_expansion("Some.fun().other()")
      end

      test "expands remote call macro" do
        assert_expansion("Kernel.|>(1, IO.inspect)")
      end

      test "expands anonymous call" do
        assert_expansion("foo = fn a -> a end; foo.(1)")
      end

      test "expands 2-tuple" do
        assert_expansion("{1, 2}")
        assert_expansion("{a, b} = {1, 2}")
      end

      test "expands list" do
        assert_expansion("[]")
        assert_expansion("[1, 2]")
        assert_expansion("[1 | [2]]")
        assert_expansion("[a | b] = [1, 2, 3]")
      end

      test "expands function" do
        assert_expansion(&inspect/1, true)
      end

      test "expands pid" do
        assert_expansion(self(), true)
      end

      test "expands number" do
        assert_expansion(1, true)
        assert_expansion(1.5, true)
      end

      test "expands atom" do
        assert_expansion(true, true)
        assert_expansion(:foo, true)
        assert_expansion(Kernel, true)
      end

      test "expands binary" do
        assert_expansion("abc", true)
      end
    end

    describe "Kernel macros" do
      test "defmodule" do
        assert_expansion_env("defmodule Abc, do: :ok")

        assert_expansion_env("""
        defmodule Abc do
          foo = 1
        end
        """)

        assert_expansion_env("""
        defmodule Abc.Some do
          foo = 1
        end
        """)

        assert_expansion_env("""
        defmodule Elixir.Abc.Some do
          foo = 1
        end
        """)

        assert_expansion_env("""
        defmodule Abc.Some do
          defmodule Child do
            foo = 1
          end
        end
        """)

        assert_expansion_env("""
        defmodule Abc.Some do
          defmodule Elixir.Child do
            foo = 1
          end
        end
        """)
      end

      test "context local macro" do
        # TODO this does not expand the macro
        assert_expansion_env("""
        defmodule Abc do
          defmacro foo(x) do
            quote do
              unquote(x) + 1
            end
          end

          def go(z) do
            foo(z)
          end
        end
        """)
      end

      test "context remote macro" do
        # TODO this does not expand the macro
        assert_expansion_env("""
        defmodule Abc do
          defmacro foo(x) do
            quote do
              unquote(x) + 1
            end
          end
        end

        defmodule Cde do
          require Abc
          def go(z) do
            Abc.foo(z)
          end
        end
        """)
      end

      test "def" do
        ast =
          Code.string_to_quoted(
            """
            defmodule Abc do
              def abc, do: :ok
            end
            """,
            columns: true,
            token_metadata: true
          )

        {expanded, state, env} = Compiler.expand(ast, %State{}, %{Compiler.env() | module: Foo})
        # elixir_env = %{:elixir_env.new() | module: Foo}
        # {elixir_expanded, _elixir_state, elixir_env} = :elixir_expand.expand(ast, :elixir_env.env_to_ex(elixir_env), elixir_env)

        # assert expanded == elixir_expanded
        # assert env == elixir_env
      end
    end
  end
end
