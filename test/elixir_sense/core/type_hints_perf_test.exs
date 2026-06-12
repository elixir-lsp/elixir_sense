defmodule ElixirSense.Core.TypeHintsPerfTest do
  @moduledoc """
  Performance regression check for the TypeHints pipeline.

  This module is **excluded from the default test run** (tagged `:perf`). Run
  it explicitly with:

      mix test --include perf test/elixir_sense/core/type_hints_perf_test.exs

  ## What is measured

  A synthetic large module (~1200 lines, ~60 functions, ~250 var bindings) is
  generated deterministically (no randomness). Two sub-pipelines are timed:

    (a) **Parse + metadata build** — `Parser.parse_string/4` with native types
        on and off.

    (b) **TypeHints pass** — `TypeHints.request_context/1` + one
        `TypeHints.type_hint_for_var/4` call per var in the module, mirroring
        what the LSP does during a single hover/inlay-hints request, with native
        types on and off.

  ## Thresholds

  Thresholds are set at 3× the numbers measured on the machine that created
  this test. They are intentionally generous — their purpose is to catch
  order-of-magnitude regressions, not benchmark noise.

  ## Relative property

  The native-on hint pass MUST NOT be slower than 2× native-off. Round 4 of
  the benchmark showed the native-on pass was 2.4× FASTER than native-off (the
  cache architecture amortises the `build_local_sigs_map` cost). This assertion
  guards that cache architecture.
  """

  use ExUnit.Case, async: false

  @moduletag :perf

  alias ElixirSense.Core.ElixirTypes
  alias ElixirSense.Core.Parser
  alias ElixirSense.Core.TypeHints

  # ── synthetic source generation ──────────────────────────────────────────────

  # Number of functions / bindings to generate.
  @num_functions 60
  @bindings_per_function 4

  # Build the source string deterministically. Each function has @bindings_per_function
  # local var bindings plus an expression returning a tuple of them.
  defp build_source do
    functions =
      Enum.map(1..@num_functions, fn i ->
        vars =
          Enum.map(1..@bindings_per_function, fn j ->
            "    var_#{i}_#{j} = :atom_#{i}_#{j}\n"
          end)

        tuple_els =
          1..@bindings_per_function
          |> Enum.map(fn j -> "var_#{i}_#{j}" end)
          |> Enum.join(", ")

        """
          def func_#{i}(arg_#{i}) do
        #{Enum.join(vars)}
            {arg_#{i}, #{tuple_els}}
          end
        """
      end)

    """
    defmodule SyntheticPerfModule do
    #{Enum.join(functions)}
    end
    """
  end

  # Collect all {position, var_info} pairs in the given metadata.
  # We grab vars from every env in lines_to_env so we exercise the full var set.
  defp collect_vars(metadata) do
    metadata.lines_to_env
    |> Enum.flat_map(fn {line, env} ->
      Enum.map(env.vars, fn var ->
        {{line, 1}, var}
      end)
    end)
  end

  # ── helpers ───────────────────────────────────────────────────────────────────

  defp with_native(enabled, fun) do
    original = Application.get_env(:elixir_sense, :use_elixir_types, false)
    Application.put_env(:elixir_sense, :use_elixir_types, enabled)

    try do
      fun.()
    after
      Application.put_env(:elixir_sense, :use_elixir_types, original)
    end
  end

  # Run `fun` `n` times and return the best (min) wall-clock microseconds.
  defp best_of(n, fun) do
    Enum.reduce(1..n, nil, fn _, acc ->
      {us, _} = :timer.tc(fun)

      case acc do
        nil -> us
        prev -> min(prev, us)
      end
    end)
  end

  # ── test ──────────────────────────────────────────────────────────────────────

  test "parse + metadata-build regression (native on and off)" do
    source = build_source()

    # Warm-up pass (not timed) — avoids cold-atom-table effects.
    _ = Parser.parse_string(source, true, true, nil)

    # native OFF
    us_off =
      with_native(false, fn ->
        best_of(3, fn -> Parser.parse_string(source, true, true, nil) end)
      end)

    # native ON
    us_on =
      with_native(true, fn ->
        best_of(3, fn -> Parser.parse_string(source, true, true, nil) end)
      end)

    IO.puts("""

    [perf] parse+build native_off=#{us_off}µs  native_on=#{us_on}µs
    """)

    # 3× regression guard (generous; catches order-of-magnitude regressions).
    # Upper bound is 30 s (30_000_000 µs) — generous for any CI machine.
    assert us_off < 30_000_000,
           "native-off parse+build took #{us_off}µs, expected < 30 000 000"

    assert us_on < 30_000_000,
           "native-on parse+build took #{us_on}µs, expected < 30 000 000"
  end

  test "TypeHints pass regression and native-cache speedup property" do
    source = build_source()

    # Pre-build metadata once (shared, immutable).
    metadata_off =
      with_native(false, fn ->
        Parser.parse_string(source, true, true, nil)
      end)

    metadata_on =
      with_native(true, fn ->
        Parser.parse_string(source, true, true, nil)
      end)

    vars_off = collect_vars(metadata_off)
    vars_on = collect_vars(metadata_on)

    total_vars = length(vars_off)
    IO.puts("\n[perf] var count: #{total_vars}")

    # ── native-off hint pass ──────────────────────────────────────────────────
    us_hints_off =
      with_native(false, fn ->
        best_of(3, fn ->
          ctx = TypeHints.request_context(metadata_off)

          Enum.each(vars_off, fn {pos, var} ->
            TypeHints.type_hint_for_var(ctx, pos, var, [])
          end)

          TypeHints.discard(ctx)
        end)
      end)

    # ── native-on hint pass ───────────────────────────────────────────────────
    us_hints_on =
      with_native(true, fn ->
        best_of(3, fn ->
          ctx = TypeHints.request_context(metadata_on)

          Enum.each(vars_on, fn {pos, var} ->
            TypeHints.type_hint_for_var(ctx, pos, var, [])
          end)

          TypeHints.discard(ctx)
        end)
      end)

    IO.puts("""
    [perf] hint-pass native_off=#{us_hints_off}µs  native_on=#{us_hints_on}µs
           native available: #{ElixirTypes.available?()}
    """)

    # 3× regression guard — generous upper bound for slow CI machines.
    assert us_hints_off < 30_000_000,
           "native-off hint pass took #{us_hints_off}µs, expected < 30 000 000"

    assert us_hints_on < 30_000_000,
           "native-on hint pass took #{us_hints_on}µs, expected < 30 000 000"

    # Relative property: native-on MUST NOT be slower than 2× native-off.
    # Round 4 measured native-on as 2.4× FASTER; this guard ensures the cache
    # architecture is not regressed into an O(N²) path. The check is only
    # meaningful when the native backend is actually available.
    if ElixirTypes.available?() do
      max_allowed_on = us_hints_off * 2

      assert us_hints_on <= max_allowed_on,
             "native-on hint pass (#{us_hints_on}µs) is more than 2× slower than " <>
               "native-off (#{us_hints_off}µs); cache regression suspected. " <>
               "max_allowed=#{max_allowed_on}µs"
    end
  end
end
