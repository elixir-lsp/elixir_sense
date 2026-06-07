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

  # --- Native-on full-pipeline sweep ------------------------------------------
  #
  # The build_env_context crash showed that native typing can silently abandon a
  # whole clause body. This sweep runs the full metadata pipeline native-on over
  # the clause-bearing constructs and asserts their binding variables survive
  # (no silent drop). Native typing may still degrade gracefully for some
  # patterns (logging a caught warning), which is fine — the invariant is that
  # variables stay in scope.

  # All variable names that appear in scope anywhere in the module (robust to
  # exact positions). Logs from graceful native degradation are swallowed.
  defp scoped_var_names(code) do
    {:ok, ast} = Code.string_to_quoted(code, columns: true, token_metadata: true)

    ExUnit.CaptureLog.capture_log(fn ->
      st = ElixirSense.Core.MetadataBuilder.build(ast)

      send(
        self(),
        {:vars,
         st.lines_to_env
         |> Enum.flat_map(fn {_l, e} -> Enum.map(e.vars, & &1.name) end)
         |> MapSet.new()}
      )
    end)

    receive do
      {:vars, names} -> names
    end
  end

  defp assert_in_scope(code, names) do
    if ElixirTypes.available?() do
      scoped = scoped_var_names(code)

      for name <- names do
        assert MapSet.member?(scoped, name),
               "expected `#{name}` in scope native-on; got #{inspect(MapSet.to_list(scoped))}"
      end
    end
  end

  test "native-on: with binds generator and else vars" do
    assert_in_scope(
      """
      defmodule M do
        def f(a) do
          with {:ok, x} <- a,
               {:ok, y} <- x do
            IO.inspect({x, y})
          else
            err -> IO.inspect(err)
          end
        end
      end
      """,
      [:x, :y, :err]
    )
  end

  test "native-on: cond keeps tested vars" do
    assert_in_scope(
      """
      defmodule M do
        def f(a) do
          cond do
            is_integer(a) -> IO.inspect(a)
            true -> :other
          end
        end
      end
      """,
      [:a]
    )
  end

  test "native-on: for comprehension binds pattern vars" do
    assert_in_scope(
      """
      defmodule M do
        def f(list) do
          for {:ok, v} <- list do
            IO.inspect(v)
          end
        end
      end
      """,
      [:v]
    )
  end

  test "native-on: nested patterns bind inner vars" do
    assert_in_scope(
      """
      defmodule M do
        def f(a) do
          case a do
            {:ok, %{id: id}} -> IO.inspect(id)
            _ -> :other
          end
        end
      end
      """,
      [:id]
    )
  end

  test "native-on: nested patterns / underscore produce no log noise" do
    if ElixirTypes.available?() do
      code = """
      defmodule M do
        def f(a) do
          case a do
            {:ok, %{id: id}} -> IO.inspect(id)
            _ -> :other
          end
        end
      end
      """

      {:ok, ast} = Code.string_to_quoted(code, columns: true, token_metadata: true)

      log =
        ExUnit.CaptureLog.capture_log(fn ->
          ElixirSense.Core.MetadataBuilder.build(ast)
        end)

      refute log =~ "version not found"
      refute log =~ "Unable to infer"
    end
  end

  test "native-on: try/rescue binds the exception var" do
    assert_in_scope(
      """
      defmodule M do
        def f(g) do
          try do
            g.()
          rescue
            e in RuntimeError -> IO.inspect(e)
          end
        end
      end
      """,
      [:e]
    )
  end

  test "native-on: cond narrows a tested variable via guard" do
    if ElixirTypes.available?() do
      code = """
      defmodule M do
        def f(a) do
          cond do
            is_integer(a) -> IO.inspect(a)
            true -> :other
          end
        end
      end
      """

      ExUnit.CaptureLog.capture_log(fn ->
        send(self(), {:hint, hint(code, :a, {4, 22})})
      end)

      assert_received {:hint, {:ok, "integer()"}}
    end
  end
end
