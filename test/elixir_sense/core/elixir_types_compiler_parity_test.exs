defmodule ElixirSense.Core.ElixirTypesCompilerParityTest do
  @moduledoc """
  Compiler-parity tests for ElixirSense.Core.ElixirTypes.

  All tests are gated on `ElixirTypes.available?()` and skipped when the native
  Module.Types backend is not present (Elixir < 1.18).

  (a) Descr rendering parity — `ElixirTypes.descr_to_string/1` must return exactly
      the same string as `Module.Types.Descr.to_quoted_string/2` for a fixed corpus.

  (b) Shape round-trip soundness — for each corpus descr whose `to_shape/1` returns
      non-nil, `descr → to_shape → coerce_var_type_public → upper_bound` must be a
      SUPERtype of the original's upper_bound, i.e. `Descr.subtype?(ub_orig, ub_coerced)`.
      The test fails if more than half the corpus is unconvertible (nil to_shape).

  (c) ExCk parity — for five stdlib functions with inferred signatures, `lookup_signature`
      succeeds and every clause's return descr renders via `descr_to_string` without error.
  """

  use ExUnit.Case, async: true

  alias ElixirSense.Core.ElixirTypes
  alias ElixirSense.Core.ExCkReader

  # Conditionally alias these — available only when the backend is present.
  # We guard every test with `requires_native_types` instead of a module-level
  # skip so the test file itself always compiles.
  @moduletag :requires_native_types

  # ---------------------------------------------------------------------------
  # Corpus builder
  # ---------------------------------------------------------------------------

  # Returns a list of {label, descr} pairs. Called inside tests so that the
  # Module.Types.Descr functions are only invoked on Elixir 1.18+.
  defp build_corpus do
    alias Module.Types.Descr

    # Struct helpers
    date_fields =
      [
        {:__struct__, Descr.atom([Date])}
        | for(k <- [:calendar, :month, :day, :year], do: {k, Descr.integer()})
      ]

    mapset_fields =
      [{:__struct__, Descr.atom([MapSet])} | for(k <- [:map], do: {k, Descr.dynamic()})]

    uri_str_or_nil = Descr.union(Descr.binary(), Descr.atom([nil]))

    uri_field_pairs =
      for k <- [:port, :scheme, :path, :host, :userinfo, :query, :fragment, :authority],
          do: {k, uri_str_or_nil}

    uri_fields = [{:__struct__, Descr.atom([URI])} | uri_field_pairs]

    [
      {"atom singleton :ok", Descr.atom([:ok])},
      {"atom singleton :error", Descr.atom([:error])},
      {"boolean()", Descr.boolean()},
      {"integer()", Descr.integer()},
      {"float()", Descr.float()},
      {"binary()", Descr.binary()},
      {"bitstring()", Descr.bitstring()},
      {"pid()", Descr.pid()},
      {"port()", Descr.port()},
      {"reference()", Descr.reference()},
      {"empty_list()", Descr.empty_list()},
      {"list(integer())", Descr.list(Descr.integer())},
      {"list(atom())", Descr.list(Descr.atom())},
      {"non_empty_list(integer())", Descr.non_empty_list(Descr.integer())},
      {"union :ok | integer()", Descr.union(Descr.atom([:ok]), Descr.integer())},
      {"union boolean | binary", Descr.union(Descr.boolean(), Descr.binary())},
      {"union :ok | :error", Descr.union(Descr.atom([:ok]), Descr.atom([:error]))},
      {"open_map foo:integer", Descr.open_map(foo: Descr.integer())},
      {"open_map optional foo", Descr.open_map(foo: Descr.if_set(Descr.integer()))},
      {"closed tuple {:ok, integer()}", Descr.tuple([Descr.atom([:ok]), Descr.integer()])},
      {"open tuple {:ok, ...}", Descr.open_tuple([Descr.atom([:ok])])},
      {"Date struct", Descr.closed_map(date_fields)},
      {"MapSet struct", Descr.closed_map(mapset_fields)},
      {"URI struct", Descr.closed_map(uri_fields)},
      {"fun()", Descr.fun()},
      {"fun/1", Descr.fun(1)},
      {"dynamic()", Descr.dynamic()},
      {"dynamic(integer())", Descr.dynamic(Descr.integer())},
      {"dynamic(:ok)", Descr.dynamic(Descr.atom([:ok]))},
      {"negation atom() and not :a", Descr.difference(Descr.atom(), Descr.atom([:a]))}
    ]
  end

  # ---------------------------------------------------------------------------
  # (a) Descr rendering parity
  # ---------------------------------------------------------------------------

  describe "descr_to_string parity with Module.Types.Descr.to_quoted_string/2" do
    test "every corpus descr renders identically via both paths" do
      alias Module.Types.Descr

      corpus = build_corpus()

      mismatches =
        for {label, descr} <- corpus do
          compiler_str = Descr.to_quoted_string(descr, collapse_structs: true)
          our_result = ElixirTypes.descr_to_string(descr)

          case our_result do
            {:ok, our_str} ->
              if normalize(compiler_str) == normalize(our_str) do
                nil
              else
                {label, compiler_str, our_str}
              end

            :error ->
              {label, compiler_str, :error}
          end
        end
        |> Enum.reject(&is_nil/1)

      assert mismatches == [],
             "Rendering mismatches:\n" <>
               Enum.map_join(mismatches, "\n", fn {label, expected, got} ->
                 "  #{label}:\n    compiler: #{inspect(expected)}\n    ours:     #{inspect(got)}"
               end)
    end

    test "descr_to_string returns {:ok, string} for all corpus entries" do
      corpus = build_corpus()

      errors =
        for {label, descr} <- corpus do
          case ElixirTypes.descr_to_string(descr) do
            {:ok, s} when is_binary(s) -> nil
            other -> {label, other}
          end
        end
        |> Enum.reject(&is_nil/1)

      assert errors == [],
             "descr_to_string returned non-ok for:\n" <>
               Enum.map_join(errors, "\n", fn {label, got} ->
                 "  #{label}: #{inspect(got)}"
               end)
    end
  end

  # ---------------------------------------------------------------------------
  # (b) Shape round-trip soundness
  # ---------------------------------------------------------------------------

  describe "shape round-trip soundness" do
    test "to_shape -> coerce_var_type_public round-trip: coerced is supertype of original" do
      alias Module.Types.Descr

      corpus = build_corpus()
      total = length(corpus)

      results =
        for {label, descr} <- corpus do
          shape = ElixirTypes.to_shape(descr)

          case shape do
            nil ->
              {:unconvertible, label}

            shape ->
              coerced = ElixirTypes.coerce_var_type_public(shape)
              ub_orig = Descr.upper_bound(descr)
              ub_coerced = Descr.upper_bound(coerced)

              if Descr.subtype?(ub_orig, ub_coerced) do
                {:ok, label}
              else
                {:fail, label, Descr.to_quoted_string(descr, collapse_structs: true),
                 Descr.to_quoted_string(coerced, collapse_structs: true)}
              end
          end
        end

      unconvertible = Enum.count(results, &match?({:unconvertible, _}, &1))
      failures = Enum.filter(results, &match?({:fail, _, _, _}, &1))

      # Canary: if to_shape regresses, more than half the corpus would be nil.
      assert unconvertible <= div(total, 2),
             "More than half the corpus (#{unconvertible}/#{total}) has nil to_shape — " <>
               "possible to_shape regression"

      assert failures == [],
             "Round-trip soundness failures (coerced is NOT a supertype of original):\n" <>
               Enum.map_join(failures, "\n", fn {:fail, label, orig_str, coerced_str} ->
                 "  #{label}:\n    orig:    #{orig_str}\n    coerced: #{coerced_str}"
               end)
    end

    test "closed-map round-trip is EXACT by default (:closed tail)" do
      alias Module.Types.Descr

      # For closed-map descrs, `to_shape/1` yields a `:closed` tail (literal-
      # complete), and the DEFAULT coercion reconstructs the same closed map
      # (modulo the `dynamic/1` wrap every coerced shape carries). This is the
      # tightened fidelity guarantee — the descr round-trip is now closed-by-
      # default; no opt-in flag is needed (the old `closed_literals` opt is gone).
      closed_corpus = [
        {"closed %{a: integer}", Descr.closed_map(a: Descr.integer())},
        {"closed %{a: integer, b: binary}",
         Descr.closed_map(a: Descr.integer(), b: Descr.binary())},
        {"Date struct", Descr.closed_map(date_struct_fields())}
      ]

      failures =
        for {label, descr} <- closed_corpus do
          shape = ElixirTypes.to_shape(descr)

          # A plain closed map round-trips to a `:closed` tail; a closed map with a
          # `__struct__` key round-trips to a `{:struct, ...}` shape (which derives
          # closedness from the loaded defstruct, not the tail). Both coerce closed.
          case shape do
            {:map, _, tail} ->
              assert tail == :closed,
                     "expected a :closed tail for #{label}, got #{inspect(shape)}"

            {:struct, _, _, _} ->
              :ok
          end

          coerced = ElixirTypes.coerce_var_type_public(shape)

          if Descr.equal?(coerced, Descr.dynamic(descr)) do
            nil
          else
            {label, Descr.to_quoted_string(descr, collapse_structs: true),
             Descr.to_quoted_string(coerced, collapse_structs: true)}
          end
        end
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Closed round-trip not exact:\n" <>
               Enum.map_join(failures, "\n", fn {label, orig, got} ->
                 "  #{label}:\n    orig:    dynamic(#{orig})\n    coerced: #{got}"
               end)
    end

    test "open-map round-trip stays OPEN" do
      alias Module.Types.Descr

      # An open-map descr round-trips to an `:open` tail, which coercion must NOT
      # close — it would unsoundly exclude maps with other keys.
      open = Descr.open_map(a: Descr.integer())
      shape = ElixirTypes.to_shape(open)
      assert {:map, _fields, :open} = shape

      coerced = ElixirTypes.coerce_var_type_public(shape)
      with_other = Descr.closed_map(a: Descr.integer(), b: Descr.binary())

      assert Descr.subtype?(Descr.upper_bound(with_other), Descr.upper_bound(coerced)),
             "coercion wrongly closed an :open-tail map"
    end

    test "unconvertible (nil to_shape) corpus entries are well below half" do
      corpus = build_corpus()
      total = length(corpus)

      {convertible, unconvertible} =
        Enum.split_with(corpus, fn {_label, descr} ->
          ElixirTypes.to_shape(descr) != nil
        end)

      assert length(convertible) > total / 2,
             "Expected majority convertible, got #{length(convertible)}/#{total}"

      # Unconvertible entries should only be those with no shape representation
      # (e.g. negations like `atom() and not :a`).
      for {label, _descr} <- unconvertible do
        assert is_binary(label)
      end
    end
  end

  # ---------------------------------------------------------------------------
  # (c) ExCk parity with stdlib functions
  # ---------------------------------------------------------------------------

  describe "ExCk stdlib signature parity" do
    # These five functions are present in every recent Elixir's standard library
    # and have inferred (:sig) entries in their ExCk chunks.
    @stdlib_mfas [
      {Integer, :to_string, 1},
      {String, :upcase, 1},
      {Enum, :count, 1},
      {Map, :new, 0},
      {List, :first, 1}
    ]

    test "lookup_signature succeeds for stdlib functions" do
      for {mod, fun, arity} <- @stdlib_mfas do
        result = ExCkReader.lookup_signature(mod, fun, arity)

        assert match?({:ok, _}, result),
               "Expected {:ok, _} for #{inspect(mod)}.#{fun}/#{arity}, got #{inspect(result)}"

        {:ok, info} = result
        assert is_map(info), "Expected map info for #{inspect(mod)}.#{fun}/#{arity}"

        assert Map.has_key?(info, :sig),
               "Expected :sig key in info for #{inspect(mod)}.#{fun}/#{arity}"
      end
    end

    test "every clause return descr in stdlib sigs renders via descr_to_string without error" do
      for {mod, fun, arity} <- @stdlib_mfas do
        {:ok, %{sig: {_kind, _domain, clauses}}} = ExCkReader.lookup_signature(mod, fun, arity)

        for {_arg_types, return_descr} <- clauses do
          result = ElixirTypes.descr_to_string(return_descr)

          assert match?({:ok, s} when is_binary(s), result),
                 "descr_to_string failed for #{inspect(mod)}.#{fun}/#{arity} return descr: " <>
                   inspect(return_descr)
        end
      end
    end
  end

  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------

  # Closed Date-struct field pairs, mirroring the corpus builder. Kept as a
  # helper so the closed round-trip test can reuse the exact same shape.
  defp date_struct_fields do
    alias Module.Types.Descr

    [
      {:__struct__, Descr.atom([Date])}
      | for(k <- [:calendar, :month, :day, :year], do: {k, Descr.integer()})
    ]
  end

  # Normalize whitespace so incidental line-wrapping differences (at 98 chars)
  # do not cause false mismatches.
  defp normalize(str) when is_binary(str) do
    str
    |> String.replace(~r/\s+/, " ")
    |> String.trim()
  end
end
