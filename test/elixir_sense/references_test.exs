defmodule ElixirSense.Providers.ReferencesTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.References.Tracer

  setup_all do
    {:ok, _} = Tracer.start_link()

    Code.compiler_options(
      tracers: [Tracer],
      ignore_module_conflict: true,
      parser_options: [columns: true]
    )

    Code.compile_file("./test/support/modules_with_references.ex")
    Code.compile_file("./test/support/module_with_builtin_type_shadowing.ex")
    Code.compile_file("./test/support/subscriber.ex")

    trace = Tracer.get()

    %{trace: trace}
  end

  test "finds reference to local function shadowing builtin type", %{trace: trace} do
    buffer = """
    defmodule B.Callee do
      def fun() do
        #  ^
        :ok
      end
      def my_fun() do
        :ok
      end
    end
    """

    references = ElixirSense.references(buffer, 2, 8, trace)

    assert [
             %{
               range: range_1,
               uri: "test/support/module_with_builtin_type_shadowing.ex"
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{column: 14, line: 4}, end: %{column: 17, line: 4}}
    end
  end

  test "find references with cursor over a function call", %{trace: trace} do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee1.func()
        #                                                     ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 59, trace)

    assert [
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_1
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_2
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_3
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{line: 36, column: 60}, end: %{line: 36, column: 64}}
      assert range_2 == %{start: %{line: 65, column: 16}, end: %{line: 65, column: 20}}
      assert range_3 == %{start: %{line: 65, column: 63}, end: %{line: 65, column: 67}}
    end
  end

  test "find references with cursor over a function definition", %{trace: trace} do
    buffer = """
    defmodule ElixirSense.Providers.ReferencesTest.Modules.Callee1 do
      def func() do
        #    ^
        IO.puts ""
      end
      def func(par1) do
        #    ^
        IO.puts par1
      end
    end
    """

    references = ElixirSense.references(buffer, 2, 10, trace)

    assert [
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_1
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_2
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_3
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{line: 36, column: 60}, end: %{line: 36, column: 64}}
      assert range_2 == %{start: %{line: 65, column: 16}, end: %{line: 65, column: 20}}
      assert range_3 == %{start: %{line: 65, column: 63}, end: %{line: 65, column: 67}}
    end

    references = ElixirSense.references(buffer, 6, 10, trace)

    assert [
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_1
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_2
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{line: 42, column: 60}, end: %{line: 42, column: 64}}
      assert range_2 == %{start: %{line: 65, column: 79}, end: %{line: 65, column: 83}}
    end
  end

  test "find references with cursor over a function definition with default arg", %{trace: trace} do
    buffer = """
    defmodule ElixirSenseExample.Subscription do
      def check(resource, models, user, opts \\\\ []) do
        IO.inspect({resource, models, user, opts})
      end
    end
    """

    references = ElixirSense.references(buffer, 2, 10, trace)

    assert [
             %{
               range: range_1,
               uri: "test/support/subscriber.ex"
             },
             %{
               range: range_2,
               uri: "test/support/subscriber.ex"
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{end: %{column: 42, line: 3}, start: %{column: 37, line: 3}}
      assert range_2 == %{end: %{column: 42, line: 4}, start: %{column: 37, line: 4}}
    end
  end

  test "find references with cursor over a function with arity 1", %{trace: trace} do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee1.func("test")
        #                                                     ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 59, trace)

    assert [
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_1
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_2
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{line: 42, column: 60}, end: %{line: 42, column: 64}}
      assert range_2 == %{start: %{line: 65, column: 79}, end: %{line: 65, column: 83}}
    end
  end

  test "find references with cursor over a function called via @attr.call", %{trace: trace} do
    buffer = """
    defmodule Caller do
      @attr ElixirSense.Providers.ReferencesTest.Modules.Callee1
      def func() do
        @attr.func("test")
        #      ^
      end
    end
    """

    references = ElixirSense.references(buffer, 4, 12, trace)

    assert [
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_1
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_2
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{line: 42, column: 60}, end: %{line: 42, column: 64}}
      assert range_2 == %{start: %{line: 65, column: 79}, end: %{line: 65, column: 83}}
    end
  end

  test "find references to function called via @attr.call", %{trace: trace} do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee7.func_noarg()
        #                                                     ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 59, trace)

    assert [
             %{
               range: range_1,
               uri: "test/support/modules_with_references.ex"
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{end: %{column: 23, line: 114}, start: %{column: 13, line: 114}}
    end
  end

  test "find references with cursor over a function with arity 1 called via pipe operator", %{
    trace: trace
  } do
    buffer = """
    defmodule Caller do
      def func() do
        "test"
        |> ElixirSense.Providers.ReferencesTest.Modules.Callee4.func_arg()
        #                                                        ^
      end
    end
    """

    references = ElixirSense.references(buffer, 4, 62, trace)

    assert [
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_1
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{line: 49, column: 63}, end: %{line: 49, column: 71}}
    end
  end

  test "find references with cursor over a function with arity 1 captured", %{trace: trace} do
    buffer = """
    defmodule Caller do
      def func() do
        Task.start(&ElixirSense.Providers.ReferencesTest.Modules.Callee4.func_arg/1)
        #                                                                  ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 72, trace)

    assert [
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_1
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{line: 49, column: 63}, end: %{line: 49, column: 71}}
    end
  end

  test "find references with cursor over a function when caller uses pipe operator", %{
    trace: trace
  } do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee4.func_arg("test")
        #                                                     ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 59, trace)

    assert [
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_1
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{line: 49, column: 63}, end: %{line: 49, column: 71}}
    end
  end

  test "find references with cursor over a function when caller uses capture operator", %{
    trace: trace
  } do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee4.func_no_arg()
        #                                                     ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 59, trace)

    assert [
             %{
               uri: "test/support/modules_with_references.ex",
               range: range
             }
           ] = references

    if Version.match?(System.version(), ">= 1.14.0-rc.0") do
      # before 1.14 tracer reports invalid positions for captures
      # https://github.com/elixir-lang/elixir/issues/12023
      assert range == %{start: %{line: 55, column: 72}, end: %{line: 55, column: 83}}
    end
  end

  test "find references with cursor over a function with deault argument when caller uses default arguments",
       %{trace: trace} do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee5.func_arg()
        ElixirSense.Providers.ReferencesTest.Modules.Callee5.func_arg("test")
        #                                                     ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 59, trace)

    assert [
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_1
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{line: 90, column: 60}, end: %{line: 90, column: 68}}
    end

    references = ElixirSense.references(buffer, 4, 59, trace)

    assert [
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_1
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{line: 90, column: 60}, end: %{line: 90, column: 68}}
    end
  end

  test "find references with cursor over a function with deault argument when caller does not uses default arguments",
       %{trace: trace} do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee5.func_arg1("test")
        ElixirSense.Providers.ReferencesTest.Modules.Callee5.func_arg1()
        #                                                     ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 59, trace)

    assert [
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_1
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{line: 91, column: 60}, end: %{line: 91, column: 69}}
    end

    references = ElixirSense.references(buffer, 4, 59, trace)

    assert [
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_1
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{line: 91, column: 60}, end: %{line: 91, column: 69}}
    end
  end

  test "find references with cursor over a module with funs with deault argument", %{trace: trace} do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee5.func_arg1("test")
        #                                                 ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 55, trace)

    assert [
             %{
               range: range_1,
               uri: "test/support/modules_with_references.ex"
             },
             %{
               range: range_2,
               uri: "test/support/modules_with_references.ex"
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{end: %{column: 68, line: 90}, start: %{column: 60, line: 90}}
      assert range_2 == %{end: %{column: 69, line: 91}, start: %{column: 60, line: 91}}
    end
  end

  test "find references with cursor over a module with 1.2 alias syntax", %{trace: trace} do
    buffer = """
    defmodule Caller do
      alias ElixirSense.Providers.ReferencesTest.Modules.Callee5
      alias ElixirSense.Providers.ReferencesTest.Modules.{Callee5}
    end
    """

    references_1 = ElixirSense.references(buffer, 2, 57, trace)
    references_2 = ElixirSense.references(buffer, 3, 58, trace)

    assert references_1 == references_2
    assert [_, _] = references_1
  end

  test "find references with cursor over a function call from an aliased module", %{trace: trace} do
    buffer = """
    defmodule Caller do
      def my() do
        alias ElixirSense.Providers.ReferencesTest.Modules.Callee1, as: C
        C.func()
        #  ^
      end
    end
    """

    references = ElixirSense.references(buffer, 4, 8, trace)

    assert [
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_1
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_2
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_3
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{line: 36, column: 60}, end: %{line: 36, column: 64}}
      assert range_2 == %{start: %{line: 65, column: 16}, end: %{line: 65, column: 20}}
      assert range_3 == %{start: %{line: 65, column: 63}, end: %{line: 65, column: 67}}
    end
  end

  test "find references with cursor over a function call from an imported module", %{trace: trace} do
    buffer = """
    defmodule Caller do
      def my() do
        import ElixirSense.Providers.ReferencesTest.Modules.Callee1
        func()
        #^
      end
    end
    """

    references = ElixirSense.references(buffer, 4, 6, trace)

    assert [
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_1
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_2
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_3
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{line: 36, column: 60}, end: %{line: 36, column: 64}}
      assert range_2 == %{start: %{line: 65, column: 16}, end: %{line: 65, column: 20}}
      assert range_3 == %{start: %{line: 65, column: 63}, end: %{line: 65, column: 67}}
    end
  end

  test "find references with cursor over a function call pipe from an imported module", %{
    trace: trace
  } do
    buffer = """
    defmodule Caller do
      def my() do
        import ElixirSense.Providers.ReferencesTest.Modules.Callee1
        "" |> func
        #      ^
      end
    end
    """

    references = ElixirSense.references(buffer, 4, 12, trace)

    assert [
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_1
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_2
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{line: 42, column: 60}, end: %{line: 42, column: 64}}
      assert range_2 == %{start: %{line: 65, column: 79}, end: %{line: 65, column: 83}}
    end
  end

  test "find references with cursor over a function capture from an imported module", %{
    trace: trace
  } do
    buffer = """
    defmodule Caller do
      def my() do
        import ElixirSense.Providers.ReferencesTest.Modules.Callee1
        &func/0
        # ^
      end
    end
    """

    references = ElixirSense.references(buffer, 4, 7, trace)

    assert [
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_1
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_2
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_3
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{line: 36, column: 60}, end: %{line: 36, column: 64}}
      assert range_2 == %{start: %{line: 65, column: 16}, end: %{line: 65, column: 20}}
      assert range_3 == %{start: %{line: 65, column: 63}, end: %{line: 65, column: 67}}
    end
  end

  test "find imported references", %{trace: trace} do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee3.func()
        #                                                     ^
      end
    end
    """

    reference = ElixirSense.references(buffer, 3, 59, trace) |> Enum.at(0)

    assert reference == %{
             uri: "test/support/modules_with_references.ex",
             range: %{start: %{line: 65, column: 47}, end: %{line: 65, column: 51}}
           }
  end

  test "find references from remote calls with the function in the next line", %{trace: trace} do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee3.func()
        #                                                     ^
      end
    end
    """

    reference = ElixirSense.references(buffer, 3, 59, trace) |> Enum.at(1)

    assert %{
             uri: "test/support/modules_with_references.ex",
             range: range_1
           } = reference

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{line: 70, column: 9}, end: %{line: 70, column: 13}}
    end
  end

  test "find references when module with __MODULE__ special form", %{trace: trace} do
    buffer = """
    defmodule ElixirSense.Providers.ReferencesTest.Modules do
      def func() do
        __MODULE__.Callee3.func()
        #                   ^
      end
    end
    """

    reference = ElixirSense.references(buffer, 3, 25, trace) |> Enum.at(0)

    assert reference == %{
             uri: "test/support/modules_with_references.ex",
             range: %{start: %{line: 65, column: 47}, end: %{line: 65, column: 51}}
           }
  end

  test "find references with atom module", %{trace: trace} do
    buffer = """
    defmodule Caller do
      def func() do
        :"Elixir.ElixirSense.Providers.ReferencesTest.Modules.Callee3".func()
        #                                                               ^
      end
    end
    """

    reference = ElixirSense.references(buffer, 3, 69, trace) |> Enum.at(0)

    assert reference == %{
             uri: "test/support/modules_with_references.ex",
             range: %{start: %{line: 65, column: 47}, end: %{line: 65, column: 51}}
           }
  end

  test "find references of variables", %{trace: trace} do
    buffer = """
    defmodule MyModule do
      def func do
        var1 = 1
        var2 = 2
        var1 = 3
        IO.puts(var1 + var2)
      end
      def func4(ppp) do

      end
    end
    """

    references = ElixirSense.references(buffer, 6, 13, trace)

    assert references == [
             %{uri: nil, range: %{start: %{line: 3, column: 5}, end: %{line: 3, column: 9}}},
             %{uri: nil, range: %{start: %{line: 5, column: 5}, end: %{line: 5, column: 9}}},
             %{uri: nil, range: %{start: %{line: 6, column: 13}, end: %{line: 6, column: 17}}}
           ]

    references = ElixirSense.references(buffer, 3, 6, trace)

    assert references == [
             %{uri: nil, range: %{start: %{line: 3, column: 5}, end: %{line: 3, column: 9}}},
             %{uri: nil, range: %{start: %{line: 5, column: 5}, end: %{line: 5, column: 9}}},
             %{uri: nil, range: %{start: %{line: 6, column: 13}, end: %{line: 6, column: 17}}}
           ]
  end

  test "find reference for variable split across lines", %{trace: trace} do
    buffer = """
    defmodule MyModule do
      def func do
        var1 = 
          1
        var1
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 6, trace)

    assert references == [
             %{uri: nil, range: %{start: %{line: 3, column: 5}, end: %{line: 3, column: 9}}},
             %{uri: nil, range: %{start: %{line: 5, column: 5}, end: %{line: 5, column: 9}}}
           ]
  end

  test "find references of attributes", %{trace: trace} do
    buffer = """
    defmodule MyModule do
      @attr "abc"
      def fun do
        @attr
      end
    end
    """

    references = ElixirSense.references(buffer, 4, 7, trace)

    assert references == [
             %{range: %{end: %{column: 8, line: 2}, start: %{column: 3, line: 2}}, uri: nil},
             %{range: %{end: %{column: 10, line: 4}, start: %{column: 5, line: 4}}, uri: nil}
           ]

    references = ElixirSense.references(buffer, 2, 4, trace)

    assert references == [
             %{range: %{end: %{column: 8, line: 2}, start: %{column: 3, line: 2}}, uri: nil},
             %{range: %{end: %{column: 10, line: 4}, start: %{column: 5, line: 4}}, uri: nil}
           ]
  end

  test "find references of private functions from definition", %{trace: trace} do
    buffer = """
    defmodule MyModule do
      def calls_private do
        private_fun()
      end

      defp also_calls_private do
        private_fun()
      end

      defp private_fun do
        #     ^
        :ok
      end
    end
    """

    references = ElixirSense.references(buffer, 10, 15, trace)

    assert references == [
             %{uri: nil, range: %{start: %{line: 3, column: 5}, end: %{line: 3, column: 16}}},
             %{uri: nil, range: %{start: %{line: 7, column: 5}, end: %{line: 7, column: 16}}}
           ]
  end

  test "find references of private functions from invocation", %{trace: trace} do
    buffer = """
    defmodule MyModule do
      def calls_private do
        private_fun()
        #     ^
      end

      defp also_calls_private do
        private_fun()
      end

      defp private_fun do
        :ok
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 15, trace)

    assert references == [
             %{uri: nil, range: %{start: %{line: 3, column: 5}, end: %{line: 3, column: 16}}},
             %{uri: nil, range: %{start: %{line: 8, column: 5}, end: %{line: 8, column: 16}}}
           ]
  end

  test "find references with cursor over a module", %{trace: trace} do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee1.func()
        #                                               ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 53, trace)

    assert [
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_1
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_2
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_3
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_4
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: range_5
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{line: 36, column: 60}, end: %{line: 36, column: 64}}
      assert range_2 == %{start: %{line: 42, column: 60}, end: %{line: 42, column: 64}}
      assert range_3 == %{start: %{line: 65, column: 16}, end: %{line: 65, column: 20}}
      assert range_4 == %{start: %{line: 65, column: 63}, end: %{line: 65, column: 67}}
      assert range_5 == %{start: %{line: 65, column: 79}, end: %{line: 65, column: 83}}
    end
  end

  test "find references with cursor over an erlang module", %{trace: trace} do
    buffer = """
    defmodule Caller do
      def func() do
        :ets.new(:s, [])
        # ^
      end
    end
    """

    references =
      ElixirSense.references(buffer, 3, 7, trace)
      |> Enum.filter(&(&1.uri =~ "modules_with_references"))

    assert [
             %{
               range: range_1,
               uri: "test/support/modules_with_references.ex"
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{column: 12, line: 74}, end: %{column: 15, line: 74}}
    end
  end

  test "find references with cursor over an erlang function call", %{trace: trace} do
    buffer = """
    defmodule Caller do
      def func() do
        :ets.new(:s, [])
        #     ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 11, trace)

    assert [
             %{
               range: range_1,
               uri: "test/support/modules_with_references.ex"
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{column: 12, line: 74}, end: %{column: 15, line: 74}}
    end
  end

  test "find references with cursor over builtin function call", %{trace: trace} do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee6.module_info()
        #                                                      ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 60, trace)

    assert [
             %{
               range: range_1,
               uri: "test/support/modules_with_references.ex"
             }
           ] = references

    if Version.match?(System.version(), ">= 1.13.0") do
      # elixir 1.13 changed positions reurned from tracer
      assert range_1 == %{start: %{column: 60, line: 101}, end: %{column: 71, line: 101}}
    end
  end
end
