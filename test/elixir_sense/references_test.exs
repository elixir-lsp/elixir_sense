defmodule ElixirSense.Providers.ReferencesTest do

  use ExUnit.Case

  # doctest References

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
        range: %{start: %{line: 26, character: 60}, end: %{line: 26, character: 64}}
      },
      %{
        uri: "test/support/modules_with_references.ex",
        range: %{start: %{line: 32, character: 60}, end: %{line: 32, character: 64}}
      },
      %{
        uri: "test/support/modules_with_references.ex",
        range: %{start: %{line: 42, character: 16}, end: %{line: 42, character: 20}}
      },
      %{
        uri: "test/support/modules_with_references.ex",
        range: %{start: %{line: 42, character: 63}, end: %{line: 42, character: 67}}
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
        range: %{start: %{line: 26, character: 60}, end: %{line: 26, character: 64}}
      },
      %{
        uri: "test/support/modules_with_references.ex",
        range: %{start: %{line: 42, character: 16}, end: %{line: 42, character: 20}}
      },
      %{
        uri: "test/support/modules_with_references.ex",
        range: %{start: %{line: 42, character: 63}, end: %{line: 42, character: 67}}
      }
    ]

    references = ElixirSense.references(buffer, 6, 10)
    assert references == [
      %{
        uri: "test/support/modules_with_references.ex",
        range: %{start: %{line: 32, character: 60}, end: %{line: 32, character: 64}}
      }
    ]
  end

  test "find references with cursor over a function call from an aliased module" do
    buffer = """
    defmodule Caller do
      def func() do
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
        range: %{start: %{line: 26, character: 60}, end: %{line: 26, character: 64}}
      },
      %{
        uri: "test/support/modules_with_references.ex",
        range: %{start: %{line: 32, character: 60}, end: %{line: 32, character: 64}}
      },
      %{
        uri: "test/support/modules_with_references.ex",
        range: %{start: %{line: 42, character: 16}, end: %{line: 42, character: 20}}
      },
      %{
        uri: "test/support/modules_with_references.ex",
        range: %{start: %{line: 42, character: 63}, end: %{line: 42, character: 67}}
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
    references = ElixirSense.references(buffer, 3, 59)

    assert references == [
      %{
        uri: "test/support/modules_with_references.ex",
        range: %{start: %{line: 42, character: 47}, end: %{line: 42, character: 51}}
      }
    ]
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
      %{uri: nil, range: %{start: %{line: 3, character: 5}, end: %{line: 3, character: 9}}},
      %{uri: nil, range: %{start: %{line: 5, character: 5}, end: %{line: 5, character: 9}}},
      %{uri: nil, range: %{start: %{line: 6, character: 13}, end: %{line: 6, character: 17}}},
    ]
  end

end
