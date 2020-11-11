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
    ] = ElixirSense.implementations(buffer, 1, 32)

    assert file1 =~ "elixir_sense/test/support/example_protocol.ex"
    assert read_line(file1, {line1, column1}) =~ "ElixirSenseExample.ExampleProtocol, for: List"

    assert file2 =~ "elixir_sense/test/support/example_protocol.ex"
    assert read_line(file2, {line2, column2}) =~ "ElixirSenseExample.ExampleProtocol, for: Map"
  end

  defp read_line(file, {line, column}) do
    file
    |> File.read!()
    |> Source.split_lines()
    |> Enum.at(line - 1)
    |> String.slice((column - 1)..-1)
  end
end
