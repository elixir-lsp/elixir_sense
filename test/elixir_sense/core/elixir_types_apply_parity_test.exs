defmodule ElixirSense.Core.ElixirTypesApplyParityTest do
  @moduledoc """
  Apply-mirror parity harness (consolidated backlog 1.3).

  Strategy: compile fixture modules in-memory with `Code.compile_string`, which
  emits a real ExCk chunk per module. We read BOTH the callee's signature and
  the caller's recorded inferred return from those chunks (via
  `ElixirSense.Core.ExCkReader.read_chunk/2` with the raw chunk binary), then
  assert that `ElixirTypes.apply_signature(callee_sig, [arg_shape])` reproduces
  the compiler's own recorded return for `def use_*, do: callee(<literal>)`.

  Critical mechanism finding (documented for future drift): the ground-truth
  comparison only works for LOCAL (same-module) calls. For a REMOTE call the
  compiler records the caller's inferred return as a bare `dynamic(term())` —
  remote return types are checked for errors but NOT narrowed into the caller's
  recorded sig. Local calls go through the local handler, which applies the
  callee sig and records the precise applied (dynamic-wrapped) return. So every
  fixture below makes a LOCAL call.

  Second finding: every signature the 1.20 compiler emits for these fixtures is
  `:infer` (never `:strong`). `apply_infer` always `dynamic()`-wraps its return
  (apply.ex:1827), which is exactly what `apply_signature/2` does for `:infer`.
  The `:strong` return/3-conditional wrapping (Task 2.1) is therefore exercised
  by `elixir_types_test.exs` against hand-built `:strong` sigs; here we assert
  the `:infer` parity that the compiler actually produces.
  """
  use ExUnit.Case, async: false

  alias ElixirSense.Core.ElixirTypes
  alias ElixirSense.Core.ExCkReader

  @moduletag :requires_native_types

  setup_all do
    if ElixirTypes.available?() do
      original = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, true)
      on_exit(fn -> Application.put_env(:elixir_sense, :use_elixir_types, original) end)
      :ok
    else
      :ok
    end
  end

  # Compile `src` in-memory and return a map of {fun, arity} => sig read from the
  # real ExCk chunk of the module(s) it defines. Asserts the chunk exists.
  #
  # Mix runs the test compiler with `infer_signatures: false`, which makes the
  # checker record `:none` for every function (no inferred sig in the ExCk
  # chunk). We must turn inference ON for the fixtures so the compiler records
  # the real applied returns we compare against; we restore the prior value
  # afterward so the rest of the suite is unaffected.
  defp compile_and_read_sigs(src) do
    previous = Code.get_compiler_option(:infer_signatures)
    Code.put_compiler_option(:infer_signatures, true)

    results =
      try do
        Code.compile_string(src)
      after
        Code.put_compiler_option(:infer_signatures, previous)
      end

    Enum.reduce(results, %{}, fn {mod, beam}, acc ->
      assert {:ok, {^mod, [{~c"ExCk", chunk}]}} = :beam_lib.chunks(beam, [~c"ExCk"]),
             "expected an ExCk chunk for #{inspect(mod)}"

      assert {:ok, sigs} = ExCkReader.read_chunk(mod, chunk: chunk)
      Map.merge(acc, Map.new(sigs, fn {fa, info} -> {fa, info.sig} end))
    end)
  end

  # The recorded return descr for a zero-arg caller `def use_x, do: ...`: the
  # single clause's return part.
  defp recorded_return(sigs, fun) do
    {:infer, _domain, [{[], return}]} = Map.fetch!(sigs, {fun, 0})
    return
  end

  # apply_signature on the callee sig, asserting :ok and returning the descr.
  defp applied(sigs, callee_fa, arg_descrs) do
    sig = Map.fetch!(sigs, callee_fa)
    assert {:ok, descr} = ElixirTypes.apply_signature(sig, arg_descrs)
    descr
  end

  describe "apply_signature parity with the real compiler (local calls)" do
    setup do
      unless ElixirTypes.available?() do
        # Should not happen under 1.20; guard for safety so the suite stays
        # deterministic on runtimes without the native typesystem.
        :ok
      end

      :ok
    end

    test "scenario 1+2: multi-clause :infer selection by atom tag" do
      alias Module.Types.Descr

      sigs =
        compile_and_read_sigs("""
        defmodule ParityAtomTag do
          def classify(:a), do: 1
          def classify(:b), do: 2
          def classify(x) when is_integer(x), do: :int
          def use_a, do: classify(:a)
          def use_int, do: classify(99)
        end
        """)

      # :a selects the atom clause (returns the integer 1|2 widened to integer()).
      assert Descr.equal?(
               applied(sigs, {:classify, 1}, [Descr.atom([:a])]),
               recorded_return(sigs, :use_a)
             )

      # 99 selects the is_integer guard clause (returns :int).
      assert Descr.equal?(
               applied(sigs, {:classify, 1}, [Descr.integer()]),
               recorded_return(sigs, :use_int)
             )
    end

    test "scenario 3: tuple-tag dispatch" do
      alias Module.Types.Descr

      sigs =
        compile_and_read_sigs("""
        defmodule ParityTupleTag do
          def handle({:ok, v}), do: v
          def handle({:error, _}), do: :failed
          def use_ok, do: handle({:ok, 5})
          def use_err, do: handle({:error, :nope})
        end
        """)

      ok_arg = Descr.tuple([Descr.atom([:ok]), Descr.integer()])
      err_arg = Descr.tuple([Descr.atom([:error]), Descr.atom([:nope])])

      assert Descr.equal?(applied(sigs, {:handle, 1}, [ok_arg]), recorded_return(sigs, :use_ok))
      assert Descr.equal?(applied(sigs, {:handle, 1}, [err_arg]), recorded_return(sigs, :use_err))
    end

    test "scenario 4: zero-match (ill-typed call) yields :error" do
      alias Module.Types.Descr

      sigs =
        compile_and_read_sigs("""
        defmodule ParityZeroMatch do
          def only_int(x) when is_integer(x), do: x
          def use_ok, do: only_int(7)
        end
        """)

      # A statically-known atom arg is disjoint from the integer-only domain:
      # apply_signature reports :error (an ill-typed call), consistent with the
      # compiler treating it as a domain violation rather than inventing a type.
      assert :error =
               ElixirTypes.apply_signature(Map.fetch!(sigs, {:only_int, 1}), [Descr.atom([:nope])])

      # And the well-typed call reproduces the recorded return.
      assert Descr.equal?(
               applied(sigs, {:only_int, 1}, [Descr.integer()]),
               recorded_return(sigs, :use_ok)
             )
    end

    test "scenario 5: gradual arg (caller passes a parameter) widens to all clauses" do
      alias Module.Types.Descr

      sigs =
        compile_and_read_sigs("""
        defmodule ParityGradual do
          def classify(:a), do: 1
          def classify(:b), do: 2
          def classify(x) when is_integer(x), do: :int
        end
        """)

      sig = Map.fetch!(sigs, {:classify, 1})

      # An unknown/gradual arg (nil shape coerces to dynamic()) is non-disjoint
      # with every clause, so the return is the dynamic-wrapped union of all
      # clause returns: integer() | :int.
      assert {:ok, descr} = ElixirTypes.apply_signature(sig, [nil])
      assert Descr.gradual?(descr)
      assert Descr.equal?(descr, Descr.dynamic(Descr.union(Descr.integer(), Descr.atom([:int]))))
    end

    test "scenario 6: union arg selects multiple clauses" do
      alias Module.Types.Descr

      sigs =
        compile_and_read_sigs("""
        defmodule ParityUnion do
          def classify(:a), do: 1
          def classify(:b), do: 2
          def classify(x) when is_integer(x), do: :int
        end
        """)

      sig = Map.fetch!(sigs, {:classify, 1})

      # atom | integer is non-disjoint with both the atom clause AND the integer
      # clause, so both are selected and their returns unioned (integer() | :int),
      # dynamic-wrapped (apply_infer always wraps).
      union_arg = Descr.union(Descr.atom([:a]), Descr.integer())
      assert {:ok, descr} = ElixirTypes.apply_signature(sig, [union_arg])
      assert Descr.equal?(descr, Descr.dynamic(Descr.union(Descr.integer(), Descr.atom([:int]))))
    end

    test "scenario 7: map / struct args" do
      alias Module.Types.Descr

      sigs =
        compile_and_read_sigs("""
        defmodule ParityMap do
          def mapfun(%{k: v}), do: v
          def use_map, do: mapfun(%{k: 1})
        end
        """)

      # The recorded callee domain is an OPEN map %{k => term}; pass a closed map
      # with an integer at :k (what the caller statically constructs).
      arg = Descr.closed_map(k: Descr.integer())
      assert Descr.equal?(applied(sigs, {:mapfun, 1}, [arg]), recorded_return(sigs, :use_map))
    end

    test "scenario 8: list arg" do
      alias Module.Types.Descr

      sigs =
        compile_and_read_sigs("""
        defmodule ParityList do
          def listfun([h | _]), do: h
          def use_list, do: listfun([1, 2, 3])
        end
        """)

      arg = Descr.non_empty_list(Descr.integer())
      assert Descr.equal?(applied(sigs, {:listfun, 1}, [arg]), recorded_return(sigs, :use_list))
    end

    test "scenario 9: multi-arity dispatch" do
      alias Module.Types.Descr

      sigs =
        compile_and_read_sigs("""
        defmodule ParityArity do
          def ar(a), do: a
          def ar(a, b), do: {a, b}
          def use_ar1, do: ar(1)
          def use_ar2, do: ar(1, 2)
        end
        """)

      # ar/1 and ar/2 are distinct sigs; each applies independently.
      assert Descr.equal?(
               applied(sigs, {:ar, 1}, [Descr.integer()]),
               recorded_return(sigs, :use_ar1)
             )

      assert Descr.equal?(
               applied(sigs, {:ar, 2}, [Descr.integer(), Descr.integer()]),
               recorded_return(sigs, :use_ar2)
             )
    end

    test "scenario 10: single-clause numeric function" do
      alias Module.Types.Descr

      sigs =
        compile_and_read_sigs("""
        defmodule ParityNumeric do
          def lit(x), do: x + 1
          def use_lit, do: lit(3)
        end
        """)

      # x + 1 is typed as number() (integer | float, bitmap 24). The recorded
      # return for use_lit is dynamic(number()); apply_signature must match.
      assert Descr.equal?(
               applied(sigs, {:lit, 1}, [Descr.integer()]),
               recorded_return(sigs, :use_lit)
             )
    end
  end
end
