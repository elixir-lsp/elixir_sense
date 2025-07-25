defmodule ElixirSense.Core.CompilerTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.Compiler
  alias ElixirSense.Core.Compiler.State
  require Record

  defp to_quoted!(ast, true), do: ast

  defp to_quoted!(string, false),
    do: Code.string_to_quoted!(string, columns: true, token_metadata: true)

  if Version.match?(System.version(), ">= 1.17.0") do
    Record.defrecordp(:elixir_ex,
      caller: false,
      prematch: if(Version.match?(System.version(), ">= 1.18.0"), do: :none, else: :raise),
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
             unused: {_, unused},
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
  else
    Record.defrecordp(:elixir_ex,
      caller: false,
      prematch: %State{
        prematch:
          if Version.match?(System.version(), ">= 1.15.0") do
            Code.get_compiler_option(:on_undefined_variable)
          else
            :warn
          end
      },
      stacktrace: false,
      unused: {%{}, 0},
      vars: {%{}, false}
    )

    defp elixir_ex_to_map(
           elixir_ex(
             caller: caller,
             prematch: prematch,
             stacktrace: stacktrace,
             unused: {_, unused},
             vars: vars
           )
         ) do
      %{
        caller: caller,
        prematch: prematch,
        stacktrace: stacktrace,
        unused: unused,
        runtime_modules: [],
        vars: vars
      }
    end
  end

  defp state_to_map(%State{} = state) do
    Map.take(state, [:caller, :prematch, :stacktrace, :unused, :runtime_modules, :vars])
  end

  defp expand(ast) do
    Compiler.expand(
      ast,
      state_with_prematch(),
      Compiler.env()
    )
  end

  defp elixir_expand(ast) do
    env = :elixir_env.new()
    :elixir_expand.expand(ast, :elixir_env.env_to_ex(env), env)
  end

  defmacrop assert_expansion(code, ast \\ false) do
    quote do
      ast = to_quoted!(unquote(code), unquote(ast))
      {elixir_expanded, elixir_state, elixir_env} = elixir_expand(ast)
      # dbg(elixir_expanded)
      {expanded, state, env} = expand(ast)
      env = %{env | tracers: []}
      # dbg(env.tracers)
      # dbg(expanded)

      assert env.tracers == elixir_env.tracers

      assert clean_capture_arg(expanded) == clean_capture_arg_elixir(elixir_expanded)
      assert env == elixir_env
      assert state_to_map(state) == elixir_ex_to_map(elixir_state)
    end
  end

  defmacrop assert_expansion_env(code, ast \\ false) do
    quote do
      ast = to_quoted!(unquote(code), unquote(ast))
      {elixir_expanded, elixir_state, elixir_env} = elixir_expand(ast)
      # dbg(elixir_expanded)
      # dbg(elixir_ex_to_map(elixir_state))
      {expanded, state, env} = expand(ast)
      env = %{env | tracers: []}
      # dbg(expanded)
      # dbg(state_to_map(state))

      assert env == elixir_env
      assert state_to_map(state) == elixir_ex_to_map(elixir_state)
    end
  end

  setup do
    # Application.put_env(:elixir_sense, :compiler_rewrite, true)
    on_exit(fn ->
      Application.put_env(:elixir_sense, :compiler_rewrite, false)
    end)

    {:ok, %{}}
  end

  defp state_with_prematch do
    %State{
      prematch:
        if Version.match?(System.version(), ">= 1.15.0") do
          if(Version.match?(System.version(), ">= 1.18.0"),
            do: :none,
            else: Code.get_compiler_option(:on_undefined_variable)
          )
        else
          :warn
        end
    }
  end

  test "initial" do
    elixir_env = :elixir_env.new()
    env = %{Compiler.env() | tracers: []}
    assert env == elixir_env

    assert state_to_map(state_with_prematch()) ==
             elixir_ex_to_map(:elixir_env.env_to_ex(elixir_env))
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
      assert_expansion("%{%{\"a\" => 1} | \"a\" => 2}")
    end

    test "expands %" do
      assert_expansion("%Date{year: 2024, month: 2, day: 18}")
      assert_expansion("%Date{calendar: Calendar.ISO, year: 2024, month: 2, day: 18}")
      assert_expansion("%{year: x} = %Date{year: 2024, month: 2, day: 18}")
      assert_expansion("%Date{year: x} = %Date{year: 2024, month: 2, day: 18}")
      assert_expansion("%Date{%Date{year: 2024, month: 2, day: 18} | day: 1}")
      assert_expansion("%x{} = %Date{year: 2024, month: 2, day: 18}")
    end

    if Version.match?(System.version(), ">= 1.15.0") do
      test "expands <<>>" do
        assert_expansion("<<>>")
        assert_expansion("<<1>>")
        assert_expansion("<<x, rest::binary>> = \"\"")
      end

      test "expands <<>> with modifier" do
        assert_expansion("x = 1; y = 1; <<x::size(y)>>")
        assert_expansion("x = 1; y = 1; <<x::size(y)>> = <<>>")
      end
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

      {expanded, state, env} =
        Compiler.expand(ast, state_with_prematch(), %{Compiler.env() | module: Foo})

      env = %{env | tracers: []}
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
        Compiler.expand(ast, state_with_prematch(), %{Compiler.env() | file: __ENV__.file})

      env = %{env | tracers: []}
      elixir_env = %{:elixir_env.new() | file: __ENV__.file}

      {elixir_expanded, elixir_state, elixir_env} =
        :elixir_expand.expand(ast, :elixir_env.env_to_ex(elixir_env), elixir_env)

      assert expanded == elixir_expanded
      assert env == elixir_env
      assert state_to_map(state) == elixir_ex_to_map(elixir_state)
    end

    test "expands __CALLER__" do
      ast = {:__CALLER__, [], nil}

      {expanded, state, env} =
        Compiler.expand(ast, %State{state_with_prematch() | caller: true}, Compiler.env())

      env = %{env | tracers: []}
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

      {expanded, state, env} =
        Compiler.expand(ast, %State{state_with_prematch() | stacktrace: true}, Compiler.env())

      env = %{env | tracers: []}

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
      {expanded, state, env} = Compiler.expand(ast, state_with_prematch(), Compiler.env())
      env = %{env | tracers: []}
      elixir_env = :elixir_env.new()

      {elixir_expanded, elixir_state, elixir_env} =
        :elixir_expand.expand(ast, :elixir_env.env_to_ex(elixir_env), elixir_env)

      assert {:%{}, [], expanded_fields} = expanded
      assert {:%{}, [], elixir_fields} = elixir_expanded

      assert Enum.sort(expanded_fields) == Enum.sort(elixir_fields)
      assert env == elixir_env
      assert state_to_map(state) == elixir_ex_to_map(elixir_state)
    end

    test "expands __ENV__.property" do
      assert_expansion("__ENV__.requires")

      if Version.match?(System.version(), ">= 1.15.0") do
        # elixir 1.14 returns fields in different order
        # we don't test that as the code is invalid anyway
        assert_expansion("__ENV__.foo")
      end
    end

    if Version.match?(System.version(), ">= 1.16.0") do
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
    end

    if Version.match?(System.version(), ">= 1.16.0") do
      test "expands quote variable" do
        assert_expansion("quote do: abc")
      end
    end

    if Version.match?(System.version(), ">= 1.16.0") do
      test "expands quote quote" do
        assert_expansion("""
        quote do: (quote do: 1)
        """)
      end
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

    if Version.match?(System.version(), ">= 1.16.0") do
      test "expands quote unquote_splicing" do
        assert_expansion("""
        a = [1, 2, 3]
        quote do: (unquote_splicing(a))
        """)
      end
    end

    if Version.match?(System.version(), ">= 1.16.0") do
      test "expands quote unquote_splicing in list" do
        assert_expansion("""
        a = [1, 2, 3]
        quote do: [unquote_splicing(a) | [1]]
        """)

        assert_expansion("""
        a = [1, 2, 3]
        quote do: [1 | unquote_splicing(a)]
        """)
      end
    end

    if Version.match?(System.version(), ">= 1.16.0") do
      test "expands quote alias" do
        assert_expansion("quote do: Date")
        assert_expansion("quote do: Elixir.Date")
        assert_expansion("quote do: String.Chars")
        assert_expansion("alias String.Chars; quote do: Chars")
        assert_expansion("alias String.Chars; quote do: Chars.foo().A")
      end
    end

    if Version.match?(System.version(), ">= 1.16.0") do
      test "expands quote import" do
        assert_expansion("quote do: inspect(1)")
        assert_expansion("quote do: &inspect/1")
      end
    end

    if Version.match?(System.version(), ">= 1.17.0") do
      test "expands quote with bind_quoted" do
        assert_expansion("""
        kv = [a: 1]
        quote bind_quoted: [kv: kv] do
          Enum.each(kv, fn {k, v} ->
            def unquote(k)(), do: unquote(v)
          end)
        end
        """)
      end
    end

    if Version.match?(System.version(), ">= 1.16.0") do
      test "expands quote with unquote false" do
        assert_expansion("""
        quote unquote: false do
          unquote("hello")
        end
        """)
      end
    end

    if Version.match?(System.version(), ">= 1.17.0") do
      test "expands quote with file" do
        assert_expansion("""
        quote file: "some.ex", do: bar(1, 2, 3)
        """)
      end
    end

    if Version.match?(System.version(), ">= 1.16.0") do
      test "expands quote with line" do
        assert_expansion("""
        quote line: 123, do: bar(1, 2, 3)
        """)
      end
    end

    if Version.match?(System.version(), ">= 1.16.0") do
      test "expands quote with location: keep" do
        assert_expansion("""
        quote location: :keep, do: bar(1, 2, 3)
        """)
      end
    end

    if Version.match?(System.version(), ">= 1.16.0") do
      test "expands quote with context" do
        assert_expansion("""
        quote context: Foo, do: abc = 3
        """)
      end
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

    if Version.match?(System.version(), ">= 1.17.0") do
      test "expands &" do
        assert_expansion("& &1")
        assert_expansion("&Enum.take(&1, 5)")
        assert_expansion("&{&1, &2}")
        assert_expansion("&[&1 | &2]")
        assert_expansion("&inspect/1")
        assert_expansion("&Enum.count/1")
        assert_expansion("a = %{}; &a.b(&1)")
        assert_expansion("&Enum.count(&1)")
        assert_expansion("&inspect(&1)")
        assert_expansion("&Enum.map(&2, &1)")
        assert_expansion("&inspect([&2, &1])")
      end
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
        k, e ->
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

    if Version.match?(System.version(), ">= 1.15.0") do
      test "expands for with bitstring generator" do
        assert_expansion("""
        for <<r::8, g::8, b::8 <- "foo">> do
          :ok
        end
        """)
      end

      test "expands for with reduce" do
        assert_expansion("""
        for <<x <- "AbCabCABc">>, x in ?a..?z, reduce: %{} do
          acc -> acc
        end
        """)
      end
    end

    test "expands for in block" do
      assert_expansion("""
      for i <- [1, 2, 3] do
        i
      end
      :ok
      """)

      assert_expansion("""
      for i <- [1, 2, 3], uniq: true do
        i
      end
      :ok
      """)

      assert_expansion("""
      _ = for i <- [1, 2, 3] do
        i
      end
      :ok
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
      defmacro __using__(_args) do
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

    test "expands underscored var write" do
      assert_expansion("_ = 5")
    end

    test "expands var write" do
      assert_expansion("a = 5")
    end

    test "expands var read" do
      assert_expansion("a = 5; a")
    end

    test "expands var overwrite" do
      assert_expansion("a = 5; a = 6")
    end

    test "expands var overwrite already overwritten" do
      assert_expansion("[a, a] = [5, 5]")
    end

    test "expands var pin" do
      assert_expansion("a = 5; ^a = 6")
    end

    test "expands nullary call if_undefined: :apply" do
      ast = {:self, [if_undefined: :apply], nil}
      {expanded, state, env} = Compiler.expand(ast, state_with_prematch(), Compiler.env())
      env = %{env | tracers: []}
      elixir_env = :elixir_env.new()

      {elixir_expanded, elixir_state, elixir_env} =
        :elixir_expand.expand(ast, :elixir_env.env_to_ex(elixir_env), elixir_env)

      assert expanded == elixir_expanded
      assert env == elixir_env
      assert state_to_map(state) == elixir_ex_to_map(elixir_state)
    end

    if Version.match?(System.version(), ">= 1.15.0") do
      test "expands nullary call if_undefined: :warn" do
        Code.put_compiler_option(:on_undefined_variable, :warn)
        ast = {:self, [], nil}

        {expanded, state, env} =
          Compiler.expand(
            ast,
            %State{
              prematch:
                if(Version.match?(System.version(), ">= 1.18.0"),
                  do: :none,
                  else: Code.get_compiler_option(:on_undefined_variable)
                ) || :warn
            },
            Compiler.env()
          )

        env = %{env | tracers: []}

        elixir_env = :elixir_env.new()

        {elixir_expanded, elixir_state, elixir_env} =
          :elixir_expand.expand(ast, :elixir_env.env_to_ex(elixir_env), elixir_env)

        assert expanded == elixir_expanded
        assert env == elixir_env
        assert state_to_map(state) == elixir_ex_to_map(elixir_state)
      after
        Code.put_compiler_option(:on_undefined_variable, :raise)
      end
    end

    test "expands local call" do
      assert_expansion("get_in(%{}, [:bar])")
      assert_expansion("length([])")
    end

    test "expands local operator call" do
      assert_expansion("a = b = []; a ++ b")
    end

    test "expands local call macro" do
      assert_expansion("if true, do: :ok")
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
      assert_expansion("[a] ++ [b] = [1, 2]")
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
    test "@" do
      assert_expansion_env("""
      defmodule Abc do
        @foo 1
        @foo
      end
      """)
    end

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
      # this does not expand the macro
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
      # this does not expand the macro
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

      {_expanded, _state, _env} =
        Compiler.expand(ast, %State{}, %{Compiler.env() | module: Foo})

      # elixir_env = %{:elixir_env.new() | module: Foo}
      # {elixir_expanded, _elixir_state, elixir_env} = :elixir_expand.expand(ast, :elixir_env.env_to_ex(elixir_env), elixir_env)

      # assert expanded == elixir_expanded
      # assert env == elixir_env
    end
  end

  defmodule Foo do
    defguard my(a) when is_integer(a) and a > 1

    defmacro aaa(a) do
      quote do
        is_integer(unquote(a)) and unquote(a) > 1
      end
    end
  end

  test "guard" do
    code = """
    require ElixirSense.Core.CompilerTest.Foo, as: Foo
    Foo.my(5)
    """

    assert_expansion(code)
  end

  test "macro" do
    code = """
    require ElixirSense.Core.CompilerTest.Foo, as: Foo
    Foo.aaa(5)
    """

    assert_expansion(code)
  end

  defp clean_capture_arg(ast) do
    {ast, _} =
      Macro.prewalk(ast, nil, fn
        {{:., _, [:elixir_quote, :shallow_validate_ast]}, _, [inner]} = node, state ->
          if Version.match?(System.version(), "< 1.18.0") do
            {inner, state}
          else
            {node, state}
          end

        {{:., dot_meta, target}, call_meta, args}, state ->
          dot_meta = Keyword.delete(dot_meta, :column_correction)
          {{{:., dot_meta, target}, call_meta, args}, state}

        {:fn, meta, args} = _node, state ->
          meta =
            if Version.match?(System.version(), "< 1.19.0") do
              Keyword.delete(meta, :capture)
            else
              meta
            end

          {{:fn, meta, args}, state}

        {atom, meta, nil} = node, state when is_atom(atom) ->
          # elixir changes the name to capture and does different counter tracking
          node =
            with "&" <> int <- to_string(atom), {_, ""} <- Integer.parse(int) do
              meta =
                Keyword.delete(meta, :counter)
                |> Keyword.delete(:capture)

              {:capture, meta, nil}
            else
              _ -> node
            end

          {node, state}

        node, state ->
          {node, state}
      end)

    ast
  end

  defp clean_capture_arg_elixir(ast) do
    {ast, _} =
      Macro.prewalk(ast, nil, fn
        {:->, meta, args}, state ->
          meta =
            if Version.match?(System.version(), "< 1.18.0") do
              Keyword.delete(meta, :generated)
            else
              meta
            end

          {{:->, meta, args}, state}

        {:fn, meta, args} = _node, state ->
          meta =
            if Version.match?(System.version(), ">= 1.19.0-rc.0") do
              Keyword.delete(meta, :capture)
            else
              meta
            end

          {{:fn, meta, args}, state}

        {:capture, meta, nil} = _node, state ->
          # elixir changes the name to capture and does different counter tracking
          meta = Keyword.delete(meta, :counter)

          meta =
            if Version.match?(System.version(), ">= 1.19.0-rc.0") do
              Keyword.delete(meta, :capture)
            else
              meta
            end

          {{:capture, meta, nil}, state}

        node, state ->
          {node, state}
      end)

    ast
  end
end
