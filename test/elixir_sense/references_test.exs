defmodule ElixirSense.Providers.ReferencesTest do
  use ExUnit.Case, async: true

  # doctest References

  test "finds reference to local function shadowing builtin type" do
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

    references = ElixirSense.references(buffer, 2, 8)

    assert references == [
             %{
               range: %{start: %{column: 14, line: 4}, end: %{column: 17, line: 4}},
               uri: "test/support/module_with_builtin_type_shadowing.ex"
             }
           ]
  end

  test "find references with cursor over a function call" do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee1.func()
        #                                                     ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 59)

    assert references == [
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 36, column: 60}, end: %{line: 36, column: 64}}
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 65, column: 16}, end: %{line: 65, column: 20}}
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 65, column: 63}, end: %{line: 65, column: 67}}
             }
           ]
  end

  test "find references with cursor over a function definition" do
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

    references = ElixirSense.references(buffer, 2, 10)

    assert references == [
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 36, column: 60}, end: %{line: 36, column: 64}}
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 65, column: 16}, end: %{line: 65, column: 20}}
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 65, column: 63}, end: %{line: 65, column: 67}}
             }
           ]

    references = ElixirSense.references(buffer, 6, 10)

    assert references == [
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 42, column: 60}, end: %{line: 42, column: 64}}
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 65, column: 79}, end: %{line: 65, column: 83}}
             }
           ]
  end

  test "find references with cursor over a function definition with default arg" do
    buffer = """
    defmodule ElixirSenseExample.Subscription do
      def check(resource, models, user, opts \\\\ []) do
        IO.inspect({resource, models, user, opts})
      end
    end
    """

    references = ElixirSense.references(buffer, 2, 10)

    assert references == [
             %{
               range: %{end: %{column: 42, line: 3}, start: %{column: 37, line: 3}},
               uri: "test/support/subscriber.ex"
             },
             %{
               range: %{end: %{column: 42, line: 4}, start: %{column: 37, line: 4}},
               uri: "test/support/subscriber.ex"
             }
           ]
  end

  test "find references with cursor over a function with arity 1" do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee1.func("test")
        #                                                     ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 59)

    assert references == [
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 42, column: 60}, end: %{line: 42, column: 64}}
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 65, column: 79}, end: %{line: 65, column: 83}}
             }
           ]
  end

  test "find references with cursor over a function with arity 1 called via pipe operator" do
    buffer = """
    defmodule Caller do
      def func() do
        "test"
        |> ElixirSense.Providers.ReferencesTest.Modules.Callee4.func_arg()
        #                                                        ^
      end
    end
    """

    references = ElixirSense.references(buffer, 4, 62)

    assert references == [
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 49, column: 63}, end: %{line: 49, column: 71}}
             }
           ]
  end

  test "find references with cursor over a function with arity 1 captured" do
    buffer = """
    defmodule Caller do
      def func() do
        Task.start(&ElixirSense.Providers.ReferencesTest.Modules.Callee4.func_arg/1)
        #                                                                  ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 72)

    assert references == [
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 49, column: 63}, end: %{line: 49, column: 71}}
             }
           ]
  end

  test "find references with cursor over a function when caller uses pipe operator" do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee4.func_arg("test")
        #                                                     ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 59)

    assert references == [
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 49, column: 63}, end: %{line: 49, column: 71}}
             }
           ]
  end

  test "find references with cursor over a function when caller uses capture operator" do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee4.func_no_arg()
        #                                                     ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 59)

    assert references == [
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 55, column: 72}, end: %{line: 55, column: 83}}
             }
           ]
  end

  test "find references with cursor over a function with deault argument when caller uses default arguments" do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee5.func_arg()
        ElixirSense.Providers.ReferencesTest.Modules.Callee5.func_arg("test")
        #                                                     ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 59)

    assert references == [
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 90, column: 60}, end: %{line: 90, column: 68}}
             }
           ]

    references = ElixirSense.references(buffer, 4, 59)

    assert references == [
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 90, column: 60}, end: %{line: 90, column: 68}}
             }
           ]
  end

  test "find references with cursor over a function with deault argument when caller does not uses default arguments" do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee5.func_arg1("test")
        ElixirSense.Providers.ReferencesTest.Modules.Callee5.func_arg1()
        #                                                     ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 59)

    assert references == [
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 91, column: 60}, end: %{line: 91, column: 69}}
             }
           ]

    references = ElixirSense.references(buffer, 4, 59)

    assert references == [
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 91, column: 60}, end: %{line: 91, column: 69}}
             }
           ]
  end

  test "find references with cursor over a module with funs with deault argument" do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee5.func_arg1("test")
        #                                                 ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 55)

    assert references == [
             %{
               range: %{end: %{column: 68, line: 90}, start: %{column: 60, line: 90}},
               uri: "test/support/modules_with_references.ex"
             },
             %{
               range: %{end: %{column: 69, line: 91}, start: %{column: 60, line: 91}},
               uri: "test/support/modules_with_references.ex"
             }
           ]
  end

  test "find references with cursor over a module with 1.2 alias syntax" do
    buffer = """
    defmodule Caller do
      alias ElixirSense.Providers.ReferencesTest.Modules.Callee5
      alias ElixirSense.Providers.ReferencesTest.Modules.{Callee5}
    end
    """

    references_1 = ElixirSense.references(buffer, 2, 57)
    references_2 = ElixirSense.references(buffer, 3, 58)

    assert references_1 == references_2
    assert [_, _] = references_1
  end

  test "find references with cursor over a function call from an aliased module" do
    buffer = """
    defmodule Caller do
      def my() do
        alias ElixirSense.Providers.ReferencesTest.Modules.Callee1, as: C
        C.func()
        #  ^
      end
    end
    """

    references = ElixirSense.references(buffer, 4, 8)

    assert references == [
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 36, column: 60}, end: %{line: 36, column: 64}}
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 65, column: 16}, end: %{line: 65, column: 20}}
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 65, column: 63}, end: %{line: 65, column: 67}}
             }
           ]
  end

  test "find references with cursor over a function call from an imported module" do
    buffer = """
    defmodule Caller do
      def my() do
        import ElixirSense.Providers.ReferencesTest.Modules.Callee1
        func()
        #^
      end
    end
    """

    references = ElixirSense.references(buffer, 4, 6)

    assert references == [
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 36, column: 60}, end: %{line: 36, column: 64}}
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 65, column: 16}, end: %{line: 65, column: 20}}
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 65, column: 63}, end: %{line: 65, column: 67}}
             }
           ]
  end

  test "find references with cursor over a function call pipe from an imported module" do
    buffer = """
    defmodule Caller do
      def my() do
        import ElixirSense.Providers.ReferencesTest.Modules.Callee1
        "" |> func
        #      ^
      end
    end
    """

    references = ElixirSense.references(buffer, 4, 12)

    assert references == [
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 42, column: 60}, end: %{line: 42, column: 64}}
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 65, column: 79}, end: %{line: 65, column: 83}}
             }
           ]
  end

  test "find references with cursor over a function capture from an imported module" do
    buffer = """
    defmodule Caller do
      def my() do
        import ElixirSense.Providers.ReferencesTest.Modules.Callee1
        &func/0
        # ^
      end
    end
    """

    references = ElixirSense.references(buffer, 4, 7)

    assert references == [
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 36, column: 60}, end: %{line: 36, column: 64}}
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 65, column: 16}, end: %{line: 65, column: 20}}
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 65, column: 63}, end: %{line: 65, column: 67}}
             }
           ]
  end

  test "find imported references" do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee3.func()
        #                                                     ^
      end
    end
    """

    reference = ElixirSense.references(buffer, 3, 59) |> Enum.at(0)

    assert reference == %{
             uri: "test/support/modules_with_references.ex",
             range: %{start: %{line: 65, column: 47}, end: %{line: 65, column: 51}}
           }
  end

  test "find references from remote calls with the function in the next line" do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee3.func()
        #                                                     ^
      end
    end
    """

    reference = ElixirSense.references(buffer, 3, 59) |> Enum.at(1)

    assert reference == %{
             uri: "test/support/modules_with_references.ex",
             range: %{start: %{line: 70, column: 8}, end: %{line: 70, column: 12}}
           }
  end

  test "find references when module with __MODULE__ special form" do
    buffer = """
    defmodule ElixirSense.Providers.ReferencesTest.Modules do
      def func() do
        __MODULE__.Callee3.func()
        #                   ^
      end
    end
    """

    reference = ElixirSense.references(buffer, 3, 25) |> Enum.at(0)

    assert reference == %{
             uri: "test/support/modules_with_references.ex",
             range: %{start: %{line: 65, column: 47}, end: %{line: 65, column: 51}}
           }
  end

  test "find references with atom module" do
    buffer = """
    defmodule Caller do
      def func() do
        :"Elixir.ElixirSense.Providers.ReferencesTest.Modules.Callee3".func()
        #                                                               ^
      end
    end
    """

    reference = ElixirSense.references(buffer, 3, 69) |> Enum.at(0)

    assert reference == %{
             uri: "test/support/modules_with_references.ex",
             range: %{start: %{line: 65, column: 47}, end: %{line: 65, column: 51}}
           }
  end

  test "find references of variables" do
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

    references = ElixirSense.references(buffer, 6, 13)

    assert references == [
             %{uri: nil, range: %{start: %{line: 3, column: 5}, end: %{line: 3, column: 9}}},
             %{uri: nil, range: %{start: %{line: 5, column: 5}, end: %{line: 5, column: 9}}},
             %{uri: nil, range: %{start: %{line: 6, column: 13}, end: %{line: 6, column: 17}}}
           ]
  end

  test "find references with cursor over a module" do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee1.func()
        #                                               ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 53)

    assert references == [
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 36, column: 60}, end: %{line: 36, column: 64}}
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 42, column: 60}, end: %{line: 42, column: 64}}
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 65, column: 16}, end: %{line: 65, column: 20}}
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 65, column: 63}, end: %{line: 65, column: 67}}
             },
             %{
               uri: "test/support/modules_with_references.ex",
               range: %{start: %{line: 65, column: 79}, end: %{line: 65, column: 83}}
             }
           ]
  end

  test "find references with cursor over an erlang module" do
    buffer = """
    defmodule Caller do
      def func() do
        :ets.new(:s, [])
        # ^
      end
    end
    """

    references =
      ElixirSense.references(buffer, 3, 7) |> Enum.filter(&(&1.uri =~ "modules_with_references"))

    assert references == [
             %{
               range: %{start: %{column: 12, line: 74}, end: %{column: 15, line: 74}},
               uri: "test/support/modules_with_references.ex"
             }
           ]
  end

  test "find references with cursor over an erlang function call" do
    buffer = """
    defmodule Caller do
      def func() do
        :ets.new(:s, [])
        #     ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 11)

    assert references == [
             %{
               range: %{start: %{column: 12, line: 74}, end: %{column: 15, line: 74}},
               uri: "test/support/modules_with_references.ex"
             }
           ]
  end

  test "find references with cursor over builtin function call" do
    buffer = """
    defmodule Caller do
      def func() do
        ElixirSense.Providers.ReferencesTest.Modules.Callee6.module_info()
        #                                                      ^
      end
    end
    """

    references = ElixirSense.references(buffer, 3, 60)

    assert references == [
             %{
               range: %{start: %{column: 60, line: 101}, end: %{column: 71, line: 101}},
               uri: "test/support/modules_with_references.ex"
             }
           ]
  end
end
