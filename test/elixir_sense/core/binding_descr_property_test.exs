defmodule ElixirSense.Core.BindingDescrPropertyTest do
  @moduledoc """
  Property harness checking Binding's custom (approximate) shape algebra
  against the native `Module.Types.Descr` set-theoretic operations as ground
  truth.

  Why this matters: when the native backend is enabled and both operands are
  descr-exact, production dispatch goes straight to Descr and the custom
  algebra is bypassed. The custom algebra is exactly what runs on Elixir
  versions without the native typesystem (and for non-exact shapes), so these
  properties validate the *fallback* semantics using the newest compiler's
  Descr as the oracle.

  Soundness contract being checked (custom ops may over-approximate, but must
  never fabricate impossibility):

    * I1 — no false disjointness: `combine_intersection(a, b) == :none` only
      when the ground-truth intersection is empty.
    * I2 — intersection over-approximates: a non-`:none` exact result must
      contain the ground-truth intersection.
    * U1 — union covers operands: the (exact) union result must contain both
      operands.
    * C1 — covers? is sound: `covers?(a, b)` implies `b` is a subtype of `a`
      in ground truth.
    * T1 — totality: no op raises on any shape the grammar can produce,
      including domain keys, optional fields, open/partial/closed map tails,
      structs, and improper lists.

  Semantic properties (I/U/C) restrict operands to descr-exact shapes — for
  those the shape→descr coercion is lossless, so Descr answers are ground
  truth rather than an approximation of an approximation. Crash-freedom (T1)
  runs on the broadest generator precisely because non-exact shapes are where
  the custom algebra is the only line of defense.
  """
  use ExUnit.Case, async: false
  use ExUnitProperties

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.ElixirTypes

  @moduletag :requires_native_types

  # Tunable for local stress runs: PROP_MAX_RUNS=5000 mix test <this file>
  @max_runs String.to_integer(System.get_env("PROP_MAX_RUNS", "200"))

  setup do
    # Force the custom algebra: with :use_elixir_types on, exact operands
    # would short-circuit to Descr and the properties would test Descr
    # against itself.
    original = Application.get_env(:elixir_sense, :use_elixir_types, false)
    Application.put_env(:elixir_sense, :use_elixir_types, false)
    on_exit(fn -> Application.put_env(:elixir_sense, :use_elixir_types, original) end)
    :ok
  end

  # ── Generators ────────────────────────────────────────────────────────────

  @exact_scalars [
    :atom,
    :boolean,
    :integer,
    :float,
    :number,
    :binary,
    :pid,
    :port,
    :reference,
    :empty_list,
    {:list, :empty},
    :empty_map
  ]

  @literal_atoms [:a, :b, :ok, :error, nil, true, false]

  # Shapes for which coerce_var_type is lossless (mirrors Binding.descr_exact?).
  defp exact_shape do
    scalar =
      StreamData.one_of([
        StreamData.member_of(@exact_scalars),
        StreamData.map(StreamData.member_of(@literal_atoms), &{:atom, &1})
      ])

    StreamData.tree(scalar, fn child ->
      StreamData.one_of([
        StreamData.map(child, &{:list, &1}),
        StreamData.map(child, &{:nonempty_list, &1}),
        StreamData.map(
          StreamData.list_of(child, min_length: 1, max_length: 3),
          fn elems -> {:tuple, length(elems), elems} end
        ),
        StreamData.map(
          StreamData.list_of(child, min_length: 1, max_length: 3),
          fn members -> {:union, members} end
        )
      ])
    end)
  end

  # The full grammar, including everything descr_exact? rejects: literal
  # numbers/binaries, unknowns, maps with all three tail markers, domain
  # keys, optional fields, structs, and improper (3-tuple) lists.
  defp any_shape do
    scalar =
      StreamData.one_of([
        StreamData.member_of(@exact_scalars),
        StreamData.member_of([
          nil,
          :none,
          :term,
          :tuple,
          :list,
          :fun,
          :bitstring,
          :non_struct_map
        ]),
        StreamData.map(StreamData.member_of(@literal_atoms), &{:atom, &1}),
        StreamData.map(StreamData.integer(-5..5), &{:integer, &1}),
        StreamData.map(StreamData.member_of([1.0, -0.5, 3.25]), &{:float, &1}),
        StreamData.map(StreamData.member_of(["", "x", "abc"]), &{:binary, &1})
      ])

    StreamData.tree(scalar, fn child ->
      StreamData.one_of([
        StreamData.map(child, &{:list, &1}),
        StreamData.map(child, &{:nonempty_list, &1}),
        StreamData.map(StreamData.tuple({child, child}), fn {e, t} -> {:nonempty_list, e, t} end),
        StreamData.map(
          StreamData.list_of(child, min_length: 1, max_length: 3),
          fn elems -> {:tuple, length(elems), elems} end
        ),
        StreamData.map(
          StreamData.list_of(child, min_length: 1, max_length: 3),
          fn members -> {:union, members} end
        ),
        map_shape(child),
        struct_shape(child)
      ])
    end)
  end

  defp map_shape(child) do
    StreamData.map(
      StreamData.tuple({fields(child), StreamData.member_of([:closed, nil, :open])}),
      fn {fields, tail} -> {:map, fields, tail} end
    )
  end

  defp struct_shape(child) do
    StreamData.map(
      StreamData.tuple({fields(child), StreamData.member_of([nil, {:atom, URI}])}),
      fn {fields, type} -> {:struct, fields, type, nil} end
    )
  end

  defp fields(child) do
    key =
      StreamData.one_of([
        StreamData.member_of([:k1, :k2, :k3]),
        # Non-atom (domain) keys — the PR-review crash class.
        StreamData.map(StreamData.member_of([:integer, :binary, :atom]), &{:domain, &1})
      ])

    value =
      StreamData.one_of([
        child,
        StreamData.map(child, &{:optional, &1})
      ])

    StreamData.uniq_list_of(StreamData.tuple({key, value}),
      uniq_fun: fn {k, _v} -> k end,
      min_length: 0,
      max_length: 3
    )
  end

  # ── Ground truth ──────────────────────────────────────────────────────────

  defp descr(shape), do: ElixirTypes.coerce_var_type_public(shape)

  defp ground_intersection(a, b),
    do: ElixirTypes.descr_intersection(descr(a), descr(b))

  # ── T1: totality — the custom algebra must never raise ──────────────────

  property "combine_intersection/normalize_union/covers? never raise on any shape pair" do
    check all(a <- any_shape(), b <- any_shape(), max_runs: @max_runs, max_run_time: 60_000) do
      _ = Binding.__combine_intersection__(a, b)
      _ = Binding.__combine_intersection__(b, a)
      _ = Binding.__normalize_union__([a, b])
      _ = Binding.__covers__?(a, b)
      _ = Binding.__covers__?(b, a)
    end
  end

  # ── I1: no false disjointness ────────────────────────────────────────────

  property "intersection :none implies ground-truth emptiness (exact shapes)" do
    check all(a <- exact_shape(), b <- exact_shape(), max_runs: @max_runs, max_run_time: 60_000) do
      if Binding.__combine_intersection__(a, b) == :none do
        assert ElixirTypes.descr_empty?(ground_intersection(a, b)),
               "combine_intersection(#{inspect(a)}, #{inspect(b)}) claimed disjoint, " <>
                 "but Descr says the intersection is non-empty"
      end
    end
  end

  # ── I2: non-:none intersection over-approximates ground truth ────────────

  property "exact intersection result contains the ground-truth intersection" do
    check all(a <- exact_shape(), b <- exact_shape(), max_runs: @max_runs, max_run_time: 60_000) do
      result = Binding.__combine_intersection__(a, b)

      if result != :none and Binding.__descr_exact__?(result) do
        assert ElixirTypes.descr_subtype?(ground_intersection(a, b), descr(result)),
               "combine_intersection(#{inspect(a)}, #{inspect(b)}) = #{inspect(result)} " <>
                 "does not contain the ground-truth intersection"
      end
    end
  end

  # ── U1: union covers both operands ────────────────────────────────────────

  property "exact union result covers both operands" do
    check all(a <- exact_shape(), b <- exact_shape(), max_runs: @max_runs, max_run_time: 60_000) do
      result = Binding.__normalize_union__([a, b])

      if Binding.__descr_exact__?(result) do
        result_descr = descr(result)

        assert ElixirTypes.descr_subtype?(descr(a), result_descr),
               "normalize_union([#{inspect(a)}, #{inspect(b)}]) = #{inspect(result)} " <>
                 "does not cover the first operand"

        assert ElixirTypes.descr_subtype?(descr(b), result_descr),
               "normalize_union([#{inspect(a)}, #{inspect(b)}]) = #{inspect(result)} " <>
                 "does not cover the second operand"
      end
    end
  end

  # ── C1: covers? soundness ────────────────────────────────────────────────

  property "covers?(a, b) implies subtype in ground truth (exact shapes)" do
    check all(a <- exact_shape(), b <- exact_shape(), max_runs: @max_runs, max_run_time: 60_000) do
      if Binding.__covers__?(a, b) do
        assert ElixirTypes.descr_subtype?(descr(b), descr(a)),
               "covers?(#{inspect(a)}, #{inspect(b)}) claimed true, " <>
                 "but Descr says #{inspect(b)} is not a subtype of #{inspect(a)}"
      end
    end
  end
end
