defmodule ElixirSense.Core.TypePresentationNativeTest do
  @moduledoc """
  L2 (native precision) golden fixtures: with `Module.Types` typing enabled, the
  scrutinee of a `case` is typed precisely by the native engine, and L1
  cross-clause subtraction narrows the catch-all branch. Together that yields a
  precise rendered hint *without* re-implementing Elixir's private reverse-arrow
  orchestration.
  """
  use ExUnit.Case, async: false

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.ElixirTypes
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Parser
  alias ElixirSense.Core.TypePresentation, as: TP

  setup do
    original = Application.get_env(:elixir_sense, :use_elixir_types, false)
    Application.put_env(:elixir_sense, :use_elixir_types, true)
    on_exit(fn -> Application.put_env(:elixir_sense, :use_elixir_types, original) end)
    :ok
  end

  defp build(code, position) do
    metadata = Parser.parse_string(code, true, true, position)
    env = Metadata.get_env(metadata, position)
    {env, Binding.from_env(env, metadata, position)}
  end

  defp hint(code, var_name, position) do
    {env, binding} = build(code, position)

    case Enum.find(env.vars, &(&1.name == var_name)) do
      nil -> {:no_var, Enum.map(env.vars, & &1.name)}
      var -> TP.render_hint(binding, var)
    end
  end

  test "native scrutinee type + cross-clause subtraction narrows the catch-all" do
    if ElixirTypes.available?() do
      code = """
      defmodule M do
        def f do
          case System.get_env("X") do
            nil -> :missing
            value -> IO.inspect(value)
          end
        end
      end
      """

      # System.get_env/1 is typed binary() | nil by the native engine; the `nil`
      # clause is subtracted, leaving binary() in the `value` clause.
      assert hint(code, :value, {5, 22}) == {:ok, "binary()"}
    end
  end

  test "native typing does not drop case clause body vars (build_env_context regression)" do
    # Regression: build_env_context used `env[:key]` (Access) on a struct env,
    # which raised and abandoned the whole case body under native typing.
    if ElixirTypes.available?() do
      code = """
      defmodule M do
        def f(x) do
          case x do
            nil -> :missing
            value -> IO.inspect(value)
          end
        end
      end
      """

      {env, _binding} = build(code, {5, 22})

      assert Enum.any?(env.vars, &(&1.name == :value)),
             "expected `value` in scope; got #{inspect(Enum.map(env.vars, & &1.name))}"
    end
  end
end
