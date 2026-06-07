defmodule ElixirSense.Providers.Hover.DocsTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Providers.Hover.Docs

  defp variable_doc(code, line, column) do
    %{docs: docs} = Docs.docs(code, line, column)
    Enum.find(docs, &(&1.kind == :variable))
  end

  test "hover on a variable includes its rendered inferred type" do
    code = """
    defmodule M do
      def f do
        m = %{a: 1, b: "x"}
        IO.inspect(m)
      end
    end
    """

    doc = variable_doc(code, 4, 16)
    assert doc.name == "m"
    assert doc.type == ~s(%{a: 1, b: "x"})
  end

  test "hover variable type is nil when nothing useful is inferred" do
    code = """
    defmodule M do
      def f(x) do
        IO.inspect(x)
      end
    end
    """

    doc = variable_doc(code, 3, 16)
    assert doc.name == "x"
    assert doc.type == nil
  end
end
