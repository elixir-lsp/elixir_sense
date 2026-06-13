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
  alias ElixirSense.Core.MetadataBuilder
  alias ElixirSense.Core.Parser
  alias ElixirSense.Core.State.VarInfo
  alias ElixirSense.Core.TypePresentation, as: TP
  alias Module.Types.Descr

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

  test "render_hint/3 attributes a native-descr-backed var as source: :native" do
    if ElixirTypes.available?() do
      descr =
        Descr.union(Descr.binary(), Descr.atom([nil]))

      var = %VarInfo{
        version: 1,
        name: :x,
        type: nil,
        elixir_types_descr: descr
      }

      env = %Binding{functions: __ENV__.functions, macros: __ENV__.macros}

      assert {:ok, %{source: :native, full: full}} = TP.render_hint(env, var, [])
      assert full =~ "binary()"
    end
  end

  test "native scrutinee type + cross-clause subtraction narrows the catch-all" do
    # Requires Elixir 1.20+ reverse-arrow mechanism.
    if ElixirTypes.available?(:previous) do
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

      assert hint(code, :value, {5, 22}) == {:ok, "binary()"}
    end
  end

  test "native typing does not drop case clause body vars" do
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

  # Native typing must not silently drop clause-body variables.
  defp scoped_var_names(code) do
    {:ok, ast} = Code.string_to_quoted(code, columns: true, token_metadata: true)

    ExUnit.CaptureLog.capture_log(fn ->
      st = MetadataBuilder.build(ast)

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

  # ── fields_for_receiver/2 — descriptor-aware merge tests ─────────────────────

  describe "fields_for_receiver/2 with VarInfo (native-on)" do
    test "descr-only fields appear when structural shape has none" do
      if ElixirTypes.available?() do
        # Build a descr for %{descr_key: integer(), opt_key: if_set(binary())}.
        # The structural type is nil (no shape), so everything comes from the descr.
        descr =
          Descr.closed_map([
            {:descr_key, Descr.integer()},
            {:opt_key, Descr.if_set(Descr.binary())}
          ])

        var = %VarInfo{
          version: 1,
          name: :x,
          type: nil,
          elixir_types_descr: descr
        }

        env = %Binding{functions: __ENV__.functions, macros: __ENV__.macros}
        fields = TP.fields_for_receiver(env, var)

        # descr_key is a required integer field
        assert Map.has_key?(fields, :descr_key)
        assert fields[:descr_key] =~ "integer"

        # opt_key is optional (if_set)
        assert Map.has_key?(fields, :opt_key)
        assert fields[:opt_key] == "if_set(binary())"
      end
    end

    test "structural field wins over descr-derived field for same key (merge precedence)" do
      if ElixirTypes.available?() do
        # Structural shape has key :a with a literal value; descr has :a as integer()
        # and also :b which structural doesn't know about.
        structural_shape = {:map, [a: {:integer, 42}, b: :not_set], nil}

        descr =
          Descr.closed_map([
            {:a, Descr.integer()},
            {:b, Descr.binary()}
          ])

        var = %VarInfo{
          version: 1,
          name: :x,
          type: structural_shape,
          elixir_types_descr: descr
        }

        env = %Binding{functions: __ENV__.functions, macros: __ENV__.macros}
        fields = TP.fields_for_receiver(env, var)

        # Structural literal "42" must win over descr's "integer()"
        assert fields[:a] == "42"

        # :b is :not_set in structural so field_map drops it from structural;
        # descr adds it as binary().
        assert fields[:b] == "binary()"
      end
    end

    test "struct receiver: descr supplies full fields, structural shape knows a subset" do
      if ElixirTypes.available?() do
        # Structural shape only knows :host; descr knows :host and :scheme.
        structural_shape =
          {:struct, [__struct__: {:atom, URI}, host: {:binary, "h"}], {:atom, URI}, nil}

        descr =
          Descr.closed_map([
            {:__struct__, Descr.atom([URI])},
            {:host, Descr.binary()},
            {:scheme, Descr.binary()}
          ])

        var = %VarInfo{
          version: 1,
          name: :x,
          type: structural_shape,
          elixir_types_descr: descr
        }

        env = %Binding{functions: __ENV__.functions, macros: __ENV__.macros}
        fields = TP.fields_for_receiver(env, var)

        # :__struct__ must be dropped
        refute Map.has_key?(fields, :__struct__)

        # Structural literal "h" wins for :host
        assert fields[:host] == ~s("h")

        # :scheme comes from descr
        assert Map.has_key?(fields, :scheme)
        assert fields[:scheme] =~ "binary"
      end
    end
  end

  test "native-on: matches referencing earlier body vars produce no crash noise" do
    if ElixirTypes.available?() do
      code = """
      defmodule M do
        @sep 0x3A
        def create_checksum(hrp, data) do
          payload = expand_hrp(hrp) ++ data
          values = payload ++ [0, 0, 0]
          mod = polymod(values)
          for p <- 0..7, do: mod
        end

        def decode(bech) do
          with {_, false} <- {:mixed, bech != bech},
               <<hrp::binary-size(1), @sep, data::binary>> = bech do
            {hrp, data}
          end
        end

        defp expand_hrp(hrp), do: :binary.bin_to_list(hrp) ++ [0]
        defp polymod(data), do: Enum.reduce(data, 1, fn d, c -> c + d end)
      end
      """

      {:ok, ast} = Code.string_to_quoted(code, columns: true, token_metadata: true)

      log =
        ExUnit.CaptureLog.capture_log(fn ->
          MetadataBuilder.build(ast)
        end)

      refute log =~ "MatchError"
      refute log =~ "FunctionClauseError"
      refute log =~ "Unable to infer"
      refute log =~ "fallback_match failed"
    end
  end

  test "native-on: call/operator-shaped patterns (quoted code) produce no crash noise" do
    if ElixirTypes.available?() do
      code = """
      defmodule M do
        defmacro gen(x) do
          quote do
            case unquote(x) do
              decompose_args({name, _, args}) -> {name, args}
              Kernel.to_timeout(timeout) -> timeout
              split_words(string, [mod], caller) -> {string, mod, caller}
              other -> other
            end
          end
        end
      end
      """

      {:ok, ast} = Code.string_to_quoted(code, columns: true, token_metadata: true)

      log =
        ExUnit.CaptureLog.capture_log(fn ->
          MetadataBuilder.build(ast)
        end)

      refute log =~ "FunctionClauseError"
      refute log =~ "of_pattern"
      refute log =~ "fallback_match failed"
    end
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
          MetadataBuilder.build(ast)
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

  test "native-on: fn clauses keep their pattern vars" do
    assert_in_scope(
      """
      defmodule M do
        def f do
          fn
            {:ok, x} -> IO.inspect(x)
            {:error, y} -> IO.inspect(y)
          end
        end
      end
      """,
      [:x, :y]
    )
  end

  test "native-on: receive binds the message-pattern var" do
    assert_in_scope(
      """
      defmodule M do
        def f do
          receive do
            {:msg, payload} -> IO.inspect(payload)
          after
            1000 -> :timeout
          end
        end
      end
      """,
      [:payload]
    )
  end

  test "native-on: try else/catch/after keep all their vars" do
    assert_in_scope(
      """
      defmodule M do
        def f(g) do
          try do
            g.()
          else
            result -> IO.inspect(result)
          catch
            kind, value -> IO.inspect({kind, value})
          after
            :ok
          end
        end
      end
      """,
      [:result, :kind, :value]
    )
  end

  test "native-on: binary-segment and bitstring-generator vars stay in scope and typed" do
    code = """
    defmodule M do
      def f(bin) do
        <<n::integer, rest::binary>> = bin

        for <<b <- rest>> do
          IO.inspect({n, b})
        end
      end
    end
    """

    assert_in_scope(code, [:n, :rest, :b])

    if ElixirTypes.available?() do
      # Segment/generator types are intrinsic to the spec, so off==on.
      assert hint(code, :n, {4, 18}) == {:ok, "integer()"}
      assert hint(code, :rest, {4, 18}) == {:ok, "binary()"}
      assert hint(code, :b, {6, 22}) == {:ok, "integer()"}
    end
  end

  test "native-on: for ... reduce: accumulator stays in scope and typed" do
    code = """
    defmodule M do
      def f(list) do
        for x <- list, reduce: %{} do
          acc -> Map.put(acc, x, 1)
        end
      end
    end
    """

    assert_in_scope(code, [:acc])

    if ElixirTypes.available?() do
      # 1.20+ renders empty map as `empty_map()`; 1.18/1.19 use `map()`.
      assert hint(code, :acc, {4, 9}) in [{:ok, "empty_map()"}, {:ok, "map()"}]
    end
  end

  test "native-on: try/else and clause-result assignment keep their vars" do
    assert_in_scope(
      """
      defmodule M do
        def f(a) do
          x =
            try do
              a
            else
              v -> v
            end

          IO.inspect(x)
        end
      end
      """,
      [:x, :v]
    )
  end

  test "native-on: improper cons `[h | non_list]` renders non_empty_list(elem, tail)" do
    code = """
    defmodule M do
      def f do
        x = [1 | :a]
        x
      end
    end
    """

    # 1.20 widens to `integer()`; 1.18/1.19 keep the literal `1`.
    assert hint(code, :x, {4, 5}) in [
             {:ok, "non_empty_list(integer(), :a)"},
             {:ok, "non_empty_list(1, :a)"}
           ]
  end

  test "native-on: `[proper] ++ non_list` renders an improper non_empty_list" do
    # Requires 1.19+ expected-type expression API (gated on `:expr`).
    code = """
    defmodule M do
      def f do
        x = [1] ++ :a
        x
      end
    end
    """

    if ElixirTypes.available?(:expr) do
      assert hint(code, :x, {4, 5}) in [
               {:ok, "non_empty_list(integer(), :a)"},
               {:ok, "non_empty_list(1, :a)"}
             ]
    end
  end
end
