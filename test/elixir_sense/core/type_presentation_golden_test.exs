defmodule ElixirSense.Core.TypePresentationGoldenTest do
  @moduledoc """
  End-to-end "golden" fixtures: real source -> metadata -> env -> rendered hint.
  These assert the DISPLAYED text an LSP layer would show, never raw internal
  shapes, exercising the whole resolve+render pipeline.
  """
  use ExUnit.Case, async: true

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Parser
  alias ElixirSense.Core.TypePresentation, as: TP

  # Render the inlay hint for `var_name` as seen at `position`.
  defp hint(code, var_name, position) do
    metadata = Parser.parse_string(code, true, true, position)
    env = Metadata.get_env(metadata, position)
    binding = Binding.from_env(env, metadata, position)

    case Enum.find(env.vars, &(&1.name == var_name)) do
      nil -> {:no_var, Enum.map(env.vars, & &1.name)}
      var -> TP.render_hint(binding, var)
    end
  end

  test "cross-clause narrowing: a later case clause subtracts earlier patterns" do
    code = """
    defmodule M do
      def f(x) when x in [:a, :b, :c] do
        case x do
          :a -> IO.puts("a")
          other -> IO.inspect(other)
        end
      end
    end
    """

    # at the `other` clause body, `other` is the scrutinee minus `:a`
    assert hint(code, :other, {5, 18}) == {:ok, ":b | :c"}
  end

  test "map variable renders its fields" do
    code = """
    defmodule M do
      def f do
        m = %{a: 1, b: "x"}
        IO.inspect(m)
      end
    end
    """

    assert hint(code, :m, {4, 16}) == {:ok, ~s(%{a: 1, b: "x"})}
  end

  test "struct variable renders as the struct" do
    code = """
    defmodule M do
      def f do
        u = %URI{host: "h"}
        IO.inspect(u)
      end
    end
    """

    assert {:ok, text} = hint(code, :u, {4, 16})
    assert String.starts_with?(text, "%URI{")
    assert text =~ ~s(host: "h")
  end

  test "function variable renders as an arrow" do
    code = """
    defmodule M do
      def f do
        g = fn x -> x end
        IO.inspect(g)
      end
    end
    """

    assert hint(code, :g, {4, 16}) == {:ok, "(term() -> term())"}
  end

  test "pattern-bound variable resolves through the matched value" do
    code = """
    defmodule M do
      def f do
        t = {:ok, 5}
        {:ok, v} = t
        IO.inspect(v)
      end
    end
    """

    assert hint(code, :v, {5, 16}) == {:ok, "5"}
  end

  test "with/else narrows to the failure space" do
    code = """
    defmodule M do
      def f(x) when x in [:ok, :error] do
        with :ok <- x do
          :done
        else
          other -> IO.inspect(other)
        end
      end
    end
    """

    # `else` matches the failure space: (:ok | :error) with :ok subtracted = :error
    assert hint(code, :other, {6, 20}) == {:ok, ":error"}
  end

  test "fields_for_receiver exposes a receiver's fields for property completion" do
    code = """
    defmodule M do
      def f do
        m = %{name: "x", age: 30}
        IO.inspect(m)
      end
    end
    """

    position = {4, 16}
    metadata = Parser.parse_string(code, true, true, position)
    env = Metadata.get_env(metadata, position)
    binding = Binding.from_env(env, metadata, position)
    var = Enum.find(env.vars, &(&1.name == :m))

    assert TP.fields_for_receiver(binding, var.type) == %{name: ~s("x"), age: "30"}
  end
end
