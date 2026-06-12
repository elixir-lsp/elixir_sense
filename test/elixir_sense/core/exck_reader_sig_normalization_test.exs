defmodule ElixirSense.Core.ExCkReaderSigNormalizationTest do
  @moduledoc """
  Boundary-normalization tests for `ExCkReader`: 1.18 ExCk chunks record inferred
  signatures as 2-TUPLES (`{:infer, clauses}` / `{:strong, clauses}`), with no
  domain element, while 1.19/1.20 use the 3-tuple `{kind, domain, clauses}`. The
  reader canonicalizes 1.18 2-tuples to `{kind, nil, clauses}` so the rest of the
  codebase stays version-agnostic.

  These tests hand-build the chunk binary (no real compiler involved), so they
  run on EVERY Elixir version — including 1.16/1.17 where `Module.Types.Descr`
  does not exist — and pin the normalization contract directly.
  """

  use ExUnit.Case, async: false

  alias ElixirSense.Core.ExCkReader

  # Build a raw ExCk chunk binary tagged with the running runtime's checker
  # version (so it is NOT rejected as a version mismatch), carrying the given
  # exports map.
  defp chunk_with_exports(exports) do
    tag = ExCkReader.runtime_checker_version()
    :erlang.term_to_binary({tag, %{exports: exports}})
  end

  describe "1.18 two-tuple :sig normalization" do
    test "{:infer, clauses} is canonicalized to {:infer, nil, clauses}" do
      clauses = [{[:fake_arg_descr], :fake_return_descr}]

      chunk =
        chunk_with_exports([
          {{:f, 1}, %{sig: {:infer, clauses}}}
        ])

      assert {:ok, sigs} = ExCkReader.read_chunk(:fake_v18_infer, chunk: chunk)
      assert %{sig: {:infer, nil, ^clauses}} = Map.fetch!(sigs, {:f, 1})
    end

    test "{:strong, clauses} is canonicalized to {:strong, nil, clauses}" do
      clauses = [{[:a], :b}]

      chunk =
        chunk_with_exports([
          {{:g, 1}, %{sig: {:strong, clauses}}}
        ])

      assert {:ok, sigs} = ExCkReader.read_chunk(:fake_v18_strong, chunk: chunk)
      assert %{sig: {:strong, nil, ^clauses}} = Map.fetch!(sigs, {:g, 1})
    end

    test "an already-3-tuple :sig (1.19/1.20) is left unchanged" do
      sig = {:infer, :some_domain, [{[:a], :b}]}

      chunk =
        chunk_with_exports([
          {{:h, 1}, %{sig: sig}}
        ])

      assert {:ok, sigs} = ExCkReader.read_chunk(:fake_v20, chunk: chunk)
      assert %{sig: ^sig} = Map.fetch!(sigs, {:h, 1})
    end

    test "a :none sig is left unchanged" do
      chunk =
        chunk_with_exports([
          {{:n, 0}, %{sig: :none}}
        ])

      assert {:ok, sigs} = ExCkReader.read_chunk(:fake_none, chunk: chunk)
      assert %{sig: :none} = Map.fetch!(sigs, {:n, 0})
    end

    test "mixed 2-tuple and 3-tuple sigs in the same chunk normalize independently" do
      two = {:infer, [{[:a], :b}]}
      three = {:infer, :dom, [{[:c], :d}]}

      chunk =
        chunk_with_exports([
          {{:two, 1}, %{sig: two}},
          {{:three, 1}, %{sig: three}}
        ])

      assert {:ok, sigs} = ExCkReader.read_chunk(:fake_mixed, chunk: chunk)
      assert %{sig: {:infer, nil, [{[:a], :b}]}} = Map.fetch!(sigs, {:two, 1})
      assert %{sig: ^three} = Map.fetch!(sigs, {:three, 1})
    end
  end

  describe "runtime_checker_version/0" do
    test "returns an :elixir_checker_v<N> atom on every supported version" do
      tag = ExCkReader.runtime_checker_version()
      assert is_atom(tag)
      assert tag |> Atom.to_string() |> String.starts_with?("elixir_checker_v")
    end
  end
end
