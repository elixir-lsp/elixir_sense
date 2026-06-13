defmodule ElixirSense.Core.ElixirTypesConstructFixturesTest do
  use ExUnit.Case, async: false

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.ElixirTypes
  alias ElixirSense.Core.ExCkReader
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.MetadataBuilder
  alias ElixirSense.Core.Parser
  alias ElixirSense.Core.TypePresentation, as: TP
  alias ElixirSense.Test.DescrCompat

  @moduletag :requires_native_types

  setup do
    original = Application.get_env(:elixir_sense, :use_elixir_types, false)
    Application.put_env(:elixir_sense, :use_elixir_types, true)
    on_exit(fn -> Application.put_env(:elixir_sense, :use_elixir_types, original) end)
    :ok
  end

  defp hint(code, var_name, position) do
    metadata = Parser.parse_string(code, true, true, position)
    env = Metadata.get_env(metadata, position)
    binding = Binding.from_env(env, metadata, position)

    case Enum.find(env.vars, &(&1.name == var_name)) do
      nil -> {:no_var, Enum.map(env.vars, & &1.name)}
      var -> TP.render_hint(binding, var)
    end
  end

  defp scoped_var_names(code) do
    {:ok, ast} = Code.string_to_quoted(code, columns: true, token_metadata: true)

    # Build metadata synchronously; any native-typing log noise is irrelevant.
    st = MetadataBuilder.build(ast)

    st.lines_to_env
    |> Enum.flat_map(fn {_l, e} -> Enum.map(e.vars, & &1.name) end)
    |> MapSet.new()
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

  # Recorded return descr for a zero-arg caller function.
  defp recorded_return(sigs, fun) do
    {:infer, _domain, [{[], return}]} = Map.fetch!(sigs, {fun, 0})
    return
  end

  # Apply sig and return the descr (asserts :ok).
  defp applied(sigs, callee_fa, arg_descrs) do
    sig = Map.fetch!(sigs, callee_fa)
    assert {:ok, descr} = ElixirTypes.apply_signature(sig, arg_descrs)
    descr
  end

  test "for with :into binary produces bitstring() hint" do
    if ElixirTypes.available?() do
      code = """
      defmodule M do
        def f(list) do
          result = for x <- list, into: "" do
            to_string(x)
          end
          result
        end
      end
      """

      # `result` is on line 6 (the `result` reference after the comprehension).
      assert {:ok, hint_text} = hint(code, :result, {6, 5})
      # bitstring() is the compiler's rendered form of the into-binary result on
      # 1.20; on 1.18/1.19 the inferred accumulator type widens to binary()
      # instead (no bitstring() constructor / different Collectable inference).
      # Both are sound.
      assert hint_text in ["bitstring()", "binary()"]
    end
  end

  test "for with :into map produces map() hint" do
    if ElixirTypes.available?() do
      code = """
      defmodule M do
        def f(list) do
          result = for x <- list, into: %{} do
            {to_string(x), x}
          end
          result
        end
      end
      """

      assert {:ok, hint_text} = hint(code, :result, {6, 5})
      assert hint_text == "map()"
    end
  end

  test "for with :uniq produces list(term()) hint" do
    if ElixirTypes.available?() do
      code = """
      defmodule M do
        def f(list) do
          result = for x <- list, uniq: true do
            x
          end
          result
        end
      end
      """

      assert {:ok, hint_text} = hint(code, :result, {6, 5})
      assert hint_text == "list(term())"
    end
  end

  test "for with multiple generators produces list of pair tuples hint" do
    if ElixirTypes.available?() do
      code = """
      defmodule M do
        def f(xs, ys) do
          pairs = for x <- xs, y <- ys do
            {x, y}
          end
          pairs
        end
      end
      """

      assert {:ok, hint_text} = hint(code, :pairs, {6, 5})
      # 1.20 infers the element shape as a 2-tuple, `list({term(), term()})`;
      # 1.18/1.19's comprehension inference is coarser and only recovers
      # `list(term())`. Both are sound.
      assert hint_text in ["list({term(), term()})", "list(term())"]
    end
  end

  test "for with multiple generators and uniq filter keeps list({term(), term()}) hint" do
    if ElixirTypes.available?() do
      code = """
      defmodule M do
        def f(xs, ys) do
          result = for x <- xs, y <- ys, x != y, uniq: true do
            {x, y}
          end
          result
        end
      end
      """

      assert {:ok, hint_text} = hint(code, :result, {6, 5})
      # See note above: 1.18/1.19 recover only `list(term())` here.
      assert hint_text in ["list({term(), term()})", "list(term())"]
    end
  end

  test "for with is_integer filter produces list(term()) hint" do
    if ElixirTypes.available?() do
      code = """
      defmodule M do
        def f(list) do
          result = for x <- list, is_integer(x) do
            x
          end
          result
        end
      end
      """

      assert {:ok, hint_text} = hint(code, :result, {6, 5})
      assert hint_text == "list(term())"
    end
  end

  test "case over remote scrutinee: nil clause subtracted, value is binary()" do
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

  test "receive with after: result is :timeout or {:got, term()}" do
    if ElixirTypes.available?() do
      code = """
      defmodule M do
        def f do
          result = receive do
            {:msg, x} -> {:got, x}
          after
            100 -> :timeout
          end
          result
        end
      end
      """

      # `result` appears on line 8. The union member order in the rendered string
      # differs across versions (1.18 emits `{:got, term()} or :timeout`); both
      # describe the same union.
      assert {:ok, hint_text} = hint(code, :result, {8, 5})
      assert hint_text in [":timeout or {:got, term()}", "{:got, term()} or :timeout"]
    end
  end

  test "with chain result is {:error, term()} or {:ok, float() or integer()}" do
    if ElixirTypes.available?() do
      code = """
      defmodule M do
        def f(a, b) do
          result = with {:ok, x} <- a,
                        {:ok, y} <- b do
                     {:ok, x + y}
                   else
                     {:error, reason} -> {:error, reason}
                   end
          result
        end
      end
      """

      # `result` appears on line 9.
      assert {:ok, hint_text} = hint(code, :result, {9, 5})
      # 1.20 renders the success branch as `{:ok, float() or integer()}`; 1.18/1.19
      # collapse it to `{:ok, number()}` and order the union differently. Both
      # describe the same union of the success and error branches.
      assert hint_text in [
               "{:error, term()} or {:ok, float() or integer()}",
               "{:ok, float() or integer()} or {:error, term()}",
               "{:ok, number()} or {:error, term()}"
             ]
    end
  end

  test "struct field access chain: literal host value hint" do
    if ElixirTypes.available?() do
      code = """
      defmodule M do
        def f do
          x = %URI{host: "h.com"}
          y = x.host
          y
        end
      end
      """

      # `y` appears on line 5.
      assert {:ok, hint_text} = hint(code, :y, {5, 5})
      # The structural engine carries the literal value; widening to binary() is
      # also sound but the literal is more precise.
      assert hint_text == ~s("h.com")
    end
  end

  test "binary utf8 segment: h is integer(), rest is binary()" do
    if ElixirTypes.available?() do
      code = """
      defmodule M do
        def f(s) do
          <<h::utf8, rest::binary>> = s
          h
        end
      end
      """

      # `h` and `rest` are on line 3; check at line 4 where `h` remains visible.
      assert hint(code, :h, {4, 5}) == {:ok, "integer()"}
      assert hint(code, :rest, {4, 5}) == {:ok, "binary()"}
    end
  end

  test "capture &String.upcase/1 bound: hint is (binary() -> term())" do
    # Typing a captured remote function needs the expected-type expression API
    # (1.19+); on 1.18 the hint pipeline declines (:skip), so this assertion only
    # applies where `of_expr/5` is available.
    if ElixirTypes.available?(:expr) do
      code = """
      defmodule M do
        def f do
          fun = &String.upcase/1
          fun
        end
      end
      """

      assert {:ok, hint_text} = hint(code, :fun, {4, 5})
      # 1.20 recovers the capture's binary() domain; 1.19 types it as the
      # unrefined `(term() -> term())`. Both are sound.
      assert hint_text in ["(binary() -> term())", "(term() -> term())"]
    end
  end

  test "capture &(&1 + 1) bound: hint is (float() or integer() -> float() or integer())" do
    if ElixirTypes.available?() do
      code = """
      defmodule M do
        def f do
          fun = &(&1 + 1)
          fun
        end
      end
      """

      assert {:ok, hint_text} = hint(code, :fun, {4, 5})
      # 1.20's native capture typing recovers both the numeric domain and range;
      # 1.19 recovers only the range (`(term() -> float() or integer())`); 1.18
      # types the anonymous capture as the unrefined `(term() -> term())`.
      assert hint_text in [
               "(float() or integer() -> float() or integer())",
               "(term() -> float() or integer())",
               "(term() -> term())"
             ]
    end
  end

  test "invoking anon &(&1 + 1) with integer yields float() or integer() hint" do
    # Invoking a captured anonymous fun and typing its result needs the
    # expected-type expression API (1.19+); on 1.18 the pipeline declines (:skip).
    if ElixirTypes.available?(:expr) do
      code = """
      defmodule M do
        def f do
          fun = &(&1 + 1)
          result = fun.(3)
          result
        end
      end
      """

      assert {:ok, hint_text} = hint(code, :result, {5, 5})
      assert hint_text == "float() or integer()"
    end
  end

  test "native-on: try/rescue exception var stays in scope" do
    assert_in_scope(
      """
      defmodule M do
        def f(g) do
          try do
            g.()
          rescue
            e in RuntimeError -> IO.inspect(e.message)
          end
        end
      end
      """,
      [:e]
    )
  end

  test "native-on: try/catch kind and val vars stay in scope" do
    assert_in_scope(
      """
      defmodule M do
        def f(g) do
          try do
            g.()
          catch
            kind, val -> IO.inspect({kind, val})
          end
        end
      end
      """,
      [:kind, :val]
    )
  end

  test "native-on: receive pattern var stays in scope with after" do
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

  test "native-on: with generator vars and else var stay in scope" do
    assert_in_scope(
      """
      defmodule M do
        def f(a, b) do
          with {:ok, x} <- a,
               {:ok, y} <- b do
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

  test "native-on: for comprehension generator var stays in scope" do
    assert_in_scope(
      """
      defmodule M do
        def f(list) do
          for x <- list, into: "" do
            to_string(x)
          end
        end
      end
      """,
      [:x]
    )
  end

  test "native-on: for with multiple generators keeps all generator vars in scope" do
    assert_in_scope(
      """
      defmodule M do
        def f(xs, ys) do
          for x <- xs, y <- ys do
            {x, y}
          end
        end
      end
      """,
      [:x, :y]
    )
  end

  describe "apply_signature parity for constructs not in the parity suite" do
    test "try/rescue/catch: recorded return is dynamic union of all clause bodies" do
      alias Module.Types.Descr

      sigs =
        compile_and_read_sigs("""
        defmodule TryRescueCatchFixture do
          def f(g) do
            try do
              g.()
            rescue
              _e in RuntimeError -> :rescued
            catch
              :throw, _val -> :caught
            end
          end
          def use_f, do: f(fn -> :ok end)
        end
        """)

      # The recorded return for use_f is dynamic() (gradual arg → all clauses
      # selected, dynamic-wrapped union).
      recorded = recorded_return(sigs, :use_f)
      assert Descr.gradual?(recorded)

      # apply_signature with a nil (gradual) arg must reproduce the recorded
      # return (dynamic(union_of_clause_bodies)).
      applied_result = applied(sigs, {:f, 1}, [nil])
      assert Descr.equal?(applied_result, recorded)
    end

    test "receive with after: recorded return is dynamic(:timeout or {:got, term()})" do
      alias Module.Types.Descr

      sigs =
        compile_and_read_sigs("""
        defmodule ReceiveAfterFixture do
          def f do
            receive do
              {:msg, x} -> {:got, x}
            after
              100 -> :timeout
            end
          end
          def use_f, do: f()
        end
        """)

      recorded = recorded_return(sigs, :use_f)

      # The return is dynamic(:timeout or {:got, term()}).
      assert Descr.gradual?(recorded)

      expected =
        Descr.dynamic(
          Descr.union(
            Descr.atom([:timeout]),
            Descr.tuple([Descr.atom([:got]), Descr.term()])
          )
        )

      assert Descr.equal?(recorded, expected)
    end

    test "for with :into binary: recorded return is dynamic(bitstring())" do
      alias Module.Types.Descr

      # The compiler records `dynamic(bitstring())` only from 1.20; earlier
      # versions record `dynamic(binary())` and lack the `Descr.bitstring/0`
      # constructor entirely, so this exact-value parity check is 1.20-only.
      if DescrCompat.bitstring?() do
        sigs =
          compile_and_read_sigs("""
          defmodule ForIntoBinaryFixture do
            def f(list), do: for(x <- list, into: "", do: to_string(x))
            def use_f, do: f(["a", "b"])
          end
          """)

        recorded = recorded_return(sigs, :use_f)
        assert Descr.gradual?(recorded)

        # bitstring is the supertype of binary; the recorded return is
        # dynamic(bitstring()), which is the sound widening the compiler applies
        # for Collectable-based `into:` comprehensions.
        assert Descr.equal?(recorded, Descr.dynamic(Descr.bitstring()))
      end
    end

    test "multi-clause: :string arg selects string clause, :integer selects integer clause" do
      alias Module.Types.Descr

      sigs =
        compile_and_read_sigs("""
        defmodule MultiClauseSelectFixture do
          def process(:string, v), do: String.upcase(v)
          def process(:integer, v), do: v + 1
          def use_string, do: process(:string, "hello")
          def use_integer, do: process(:integer, 42)
        end
        """)

      # :string + binary → recorded return for use_string
      assert Descr.equal?(
               applied(sigs, {:process, 2}, [Descr.atom([:string]), Descr.binary()]),
               recorded_return(sigs, :use_string)
             )

      # :integer + integer → recorded return for use_integer
      assert Descr.equal?(
               applied(sigs, {:process, 2}, [Descr.atom([:integer]), Descr.integer()]),
               recorded_return(sigs, :use_integer)
             )
    end

    test "macro-generated function: apply_signature reproduces recorded return" do
      alias Module.Types.Descr

      sigs =
        compile_and_read_sigs("""
        defmodule MacroParity do
          defmacro make_adder(n) do
            quote do
              def add_n(x), do: x + unquote(n)
            end
          end
        end

        defmodule MacroParityUser do
          require MacroParity
          MacroParity.make_adder(10)
          def use_add10, do: add_n(5)
        end
        """)

      # add_n/1 is generated by the macro; its inferred sig has float|integer domain.
      assert Descr.equal?(
               applied(sigs, {:add_n, 1}, [Descr.integer()]),
               recorded_return(sigs, :use_add10)
             )
    end
  end
end
