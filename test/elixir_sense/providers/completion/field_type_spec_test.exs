defmodule ElixirSense.Providers.Completion.FieldTypeSpecTest do
  @moduledoc """
  Map/struct field completions fall back to the inferred (rendered) field type
  for `type_spec` when there is no declared typespec — e.g. plain maps.
  """
  use ExUnit.Case, async: true

  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Parser
  alias ElixirSense.Providers.Completion.CompletionEngine

  defp field(code, position, dot_hint, field_name) do
    metadata = Parser.parse_string(code, true, true, position)
    env = Metadata.get_env(metadata, position)

    dot_hint
    |> CompletionEngine.complete(env, metadata, position)
    |> Enum.find(&(&1[:type] == :field and &1[:name] == field_name))
  end

  test "plain map field completion carries the inferred field type as type_spec" do
    code = """
    defmodule M do
      def f do
        m = %{name: "x", age: 30}
        m
      end
    end
    """

    name = field(code, {4, 5}, "m.", "name")
    age = field(code, {4, 5}, "m.", "age")

    assert name.type_spec == ~s("x")
    assert age.type_spec == "30"
  end
end
