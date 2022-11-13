defmodule ElixirSense.Providers.ImplementationTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Providers.Implementation
  alias ElixirSense.Location
  alias ElixirSense.Core.Source

  doctest Implementation

  test "dont crash on empty buffer" do
    assert [] == ElixirSense.implementations("", 1, 1)
  end

  test "dont error on __MODULE__ when no module" do
    assert [] == ElixirSense.implementations("__MODULE__", 1, 1)
  end

  test "dont error on Elixir" do
    assert [] == ElixirSense.implementations("Elixir", 1, 1)
  end

  test "dont error on not existing module" do
    assert [] == ElixirSense.implementations("SomeNotExistingMod", 1, 1)
  end

  test "dont error on non behaviour module" do
    assert [] == ElixirSense.implementations("ElixirSenseExample.EmptyModule", 1, 32)
  end

  test "dont error on erlang function calls" do
    assert [] == ElixirSense.implementations(":ets.new", 1, 8)
  end

  test "dont return implementations for non callback functions on behaviour" do
    assert [] == ElixirSense.implementations("GenServer.start_link", 1, 12)
  end

  test "dont error on non behaviour module function" do
    buffer = """
    defmodule ElixirSenseExample.EmptyModule do
      def abc(), do: :ok
    end
    """

    assert [] == ElixirSense.implementations(buffer, 2, 8)
  end

  test "dont error on builtin macro" do
    buffer = """
    defmodule ElixirSenseExample.EmptyModule do
      def abc(), do: :ok
    end
    """

    assert [] == ElixirSense.implementations(buffer, 1, 8)
  end

  test "find implementations of behaviour module" do
    buffer = """
    defmodule ElixirSenseExample.ExampleBehaviourWithDoc do
    end
    """

    [
      %Location{type: :module, file: file1, line: line1, column: column1},
      %Location{type: :module, file: file2, line: line2, column: column2}
    ] = ElixirSense.implementations(buffer, 1, 32)

    assert file1 =~ "elixir_sense/test/support/example_behaviour.ex"

    assert read_line(file1, {line1, column1}) =~
             "ElixirSenseExample.ExampleBehaviourWithDocCallbackImpl"

    assert file2 =~ "elixir_sense/test/support/example_behaviour.ex"

    assert read_line(file2, {line2, column2}) =~
             "ElixirSenseExample.ExampleBehaviourWithDocCallbackNoImpl"
  end

  test "find protocol implementations" do
    buffer = """
    defprotocol ElixirSenseExample.ExampleProtocol do
    end
    """

    [
      %Location{type: :module, file: file1, line: line1, column: column1},
      %Location{type: :module, file: file2, line: line2, column: column2}
    ] = ElixirSense.implementations(buffer, 1, 32) |> Enum.sort()

    assert file1 =~ "elixir_sense/test/support/example_protocol.ex"
    assert read_line(file1, {line1, column1}) =~ "ElixirSenseExample.ExampleProtocol, for: List"

    assert file2 =~ "elixir_sense/test/support/example_protocol.ex"
    assert read_line(file2, {line2, column2}) =~ "ElixirSenseExample.ExampleProtocol, for: Map"
  end

  test "find implementations of behaviour module callback" do
    buffer = """
    defmodule ElixirSenseExample.ExampleBehaviourWithDoc do
      @callback foo() :: :ok
    end
    """

    [
      %Location{type: :function, file: file1, line: line1, column: column1},
      %Location{type: :function, file: file2, line: line2, column: column2}
    ] = ElixirSense.implementations(buffer, 2, 14)

    assert file1 =~ "elixir_sense/test/support/example_behaviour.ex"

    assert read_line(file1, {line1, column1}) =~
             "foo(), do: :ok"

    assert file2 =~ "elixir_sense/test/support/example_behaviour.ex"

    assert read_line(file2, {line2, column2}) =~
             "foo(), do: :ok"
  end

  test "find implementations of behaviour module on callback in implementation" do
    buffer = """
    defmodule Some do
      @behaviour ElixirSenseExample.ExampleBehaviourWithDoc
      def foo(), do: :ok
    end
    """

    [
      %Location{type: :function, file: file1, line: line1, column: column1},
      %Location{type: :function, file: file2, line: line2, column: column2}
    ] = ElixirSense.implementations(buffer, 3, 8)

    assert file1 =~ "elixir_sense/test/support/example_behaviour.ex"

    assert read_line(file1, {line1, column1}) =~
             "foo(), do: :ok"

    assert file2 =~ "elixir_sense/test/support/example_behaviour.ex"

    assert read_line(file2, {line2, column2}) =~
             "foo(), do: :ok"
  end

  test "find protocol implementation functions" do
    buffer = """
    defprotocol ElixirSenseExample.ExampleProtocol do
      @spec some(t) :: any
      def some(t)
    end
    """

    [
      %Location{type: :function, file: file1, line: line1, column: column1},
      %Location{type: :function, file: file2, line: line2, column: column2}
    ] = ElixirSense.implementations(buffer, 3, 8)

    assert file1 =~ "elixir_sense/test/support/example_protocol.ex"
    assert read_line(file1, {line1, column1}) =~ "some(t), do: t"

    assert file2 =~ "elixir_sense/test/support/example_protocol.ex"
    assert read_line(file2, {line2, column2}) =~ "some(t), do: t"
  end

  test "find protocol implementation functions on implementation function" do
    buffer = """
    defimpl ElixirSenseExample.ExampleProtocol, for: String do
      def some(t), do: t
    end
    """

    [
      %Location{type: :function, file: file1, line: line1, column: column1},
      %Location{type: :function, file: file2, line: line2, column: column2}
    ] = ElixirSense.implementations(buffer, 2, 8)

    assert file1 =~ "elixir_sense/test/support/example_protocol.ex"
    assert read_line(file1, {line1, column1}) =~ "some(t), do: t"

    assert file2 =~ "elixir_sense/test/support/example_protocol.ex"
    assert read_line(file2, {line2, column2}) =~ "some(t), do: t"
  end

  test "find protocol implementation functions on call" do
    buffer = """
    ElixirSenseExample.ExampleProtocol.some()
    """

    [
      %Location{type: :function, file: file1, line: line1, column: column1},
      %Location{type: :function, file: file2, line: line2, column: column2}
    ] = ElixirSense.implementations(buffer, 1, 37)

    assert file1 =~ "elixir_sense/test/support/example_protocol.ex"
    assert read_line(file1, {line1, column1}) =~ "some(t), do: t"

    assert file2 =~ "elixir_sense/test/support/example_protocol.ex"
    assert read_line(file2, {line2, column2}) =~ "some(t), do: t"
  end

  test "find protocol implementation functions on call with alias" do
    buffer = """
    defmodule Some do
      alias ElixirSenseExample.ExampleProtocol, as: A
      A.some()
    end
    """

    [
      %Location{type: :function, file: file1, line: line1, column: column1},
      %Location{type: :function, file: file2, line: line2, column: column2}
    ] = ElixirSense.implementations(buffer, 3, 6)

    assert file1 =~ "elixir_sense/test/support/example_protocol.ex"
    assert read_line(file1, {line1, column1}) =~ "some(t), do: t"

    assert file2 =~ "elixir_sense/test/support/example_protocol.ex"
    assert read_line(file2, {line2, column2}) =~ "some(t), do: t"
  end

  test "find protocol implementation functions on call via @attr" do
    buffer = """
    defmodule Some do
      @attr ElixirSenseExample.ExampleProtocol
      @attr.some()
    end
    """

    [
      %Location{type: :function, file: file1, line: line1, column: column1},
      %Location{type: :function, file: file2, line: line2, column: column2}
    ] = ElixirSense.implementations(buffer, 3, 10)

    assert file1 =~ "elixir_sense/test/support/example_protocol.ex"
    assert read_line(file1, {line1, column1}) =~ "some(t), do: t"

    assert file2 =~ "elixir_sense/test/support/example_protocol.ex"
    assert read_line(file2, {line2, column2}) =~ "some(t), do: t"
  end

  test "find implementation of delegated functions" do
    buffer = """
    defmodule MyModule do
      alias ElixirSenseExample.ModuleWithFunctions, as: MyMod
      MyMod.delegated_function()
      #        ^
    end
    """

    [%Location{type: :function, file: file, line: line, column: column}] =
      ElixirSense.implementations(buffer, 3, 11)

    assert file =~ "elixir_sense/test/support/module_with_functions.ex"
    assert read_line(file, {line, column}) =~ "delegated_function do"
  end

  test "find implementation of delegated functions via @attr" do
    buffer = """
    defmodule MyModule do
      @attr ElixirSenseExample.ModuleWithFunctions
      def a do
        @attr.delegated_function()
      end
    end
    """

    [%Location{type: :function, file: file, line: line, column: column}] =
      ElixirSense.implementations(buffer, 4, 13)

    assert file =~ "elixir_sense/test/support/module_with_functions.ex"
    assert read_line(file, {line, column}) =~ "delegated_function do"
  end

  test "handle defdelegate" do
    buffer = """
    defmodule MyModule do
      defdelegate delegated_function, to: ElixirSenseExample.ModuleWithFunctions.DelegatedModule
      #            ^
    end
    """

    [%Location{type: :function, file: file, line: line, column: column}] =
      ElixirSense.implementations(buffer, 2, 15)

    assert file =~ "elixir_sense/test/support/module_with_functions.ex"
    assert read_line(file, {line, column}) =~ "delegated_function"
  end

  test "handle defdelegate with `as`" do
    buffer = """
    defmodule MyModule do
      defdelegate my_function, to: ElixirSenseExample.ModuleWithFunctions.DelegatedModule, as: :delegated_function
      #            ^
    end
    """

    [%Location{type: :function, file: file, line: line, column: column}] =
      ElixirSense.implementations(buffer, 2, 15)

    assert file =~ "elixir_sense/test/support/module_with_functions.ex"
    assert read_line(file, {line, column}) =~ "delegated_function"
  end

  test "handle recursion in defdelegate" do
    buffer = """
    defmodule MyModule do
      defdelegate delegated_function, to: MyModule
      #            ^
    end
    """

    assert [] == ElixirSense.implementations(buffer, 2, 15)
  end

  defp read_line(file, {line, column}) do
    file
    |> File.read!()
    |> Source.split_lines()
    |> Enum.at(line - 1)
    |> String.slice((column - 1)..-1)
  end
end
