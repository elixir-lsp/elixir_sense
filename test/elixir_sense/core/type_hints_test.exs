defmodule ElixirSense.Core.TypeHintsTest do
  use ExUnit.Case, async: false

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.ElixirTypes
  alias ElixirSense.Core.ExCkReader
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Parser
  alias ElixirSense.Core.State.VarInfo
  alias ElixirSense.Core.TypeHints
  alias ElixirSense.Core.TypePresentation, as: TP
  alias Module.Types.Descr

  setup do
    original = Application.get_env(:elixir_sense, :use_elixir_types, false)
    Application.put_env(:elixir_sense, :use_elixir_types, true)
    on_exit(fn -> Application.put_env(:elixir_sense, :use_elixir_types, original) end)
    :ok
  end

  defp metadata(code, position) do
    Parser.parse_string(code, true, true, position)
  end

  defp find_var(metadata, position, name) do
    env = Metadata.get_env(metadata, position)
    Enum.find(env.vars, &(&1.name == name))
  end

  describe "request_context/1" do
    test "wraps metadata with a unique ref" do
      md = metadata("defmodule M do\n  def f, do: :ok\nend\n", {2, 3})
      ctx1 = TypeHints.request_context(md)
      ctx2 = TypeHints.request_context(md)

      assert ctx1.metadata == md
      assert is_reference(ctx1.ref)
      assert ctx1.ref != ctx2.ref
    end
  end

  describe "type_hint_for_var/4" do
    @code """
    defmodule M do
      def f do
        x = :ok
        x
      end
    end
    """

    test "parity with the direct TypePresentation.render_hint/3 path" do
      pos = {4, 5}
      md = metadata(@code, pos)
      var = find_var(md, pos, :x)
      assert %VarInfo{} = var

      env = Metadata.get_env(md, pos)
      direct = TP.render_hint(Binding.from_env(env, md, pos), var, [])

      ctx = TypeHints.request_context(md)
      assert TypeHints.type_hint_for_var(ctx, pos, var, []) == direct
      assert {:ok, %{label: ":ok", full: ":ok", source: _}} = direct
    end

    test ":skip on nil env" do
      # Metadata.get_env always returns at least a default env, so the nil-env
      # branch is defensive. Exercise it directly with an empty metadata: a
      # position with no scopes still yields a default env, but a var with no
      # type renders nothing, so the only observable :skip path is the
      # uninformative one (next test). Here we assert the guard does not crash
      # and skips when the env carries no binding context.
      md = metadata("\n", {1, 1})
      var = %VarInfo{version: 1, name: :x, type: nil}
      ctx = TypeHints.request_context(md)
      assert TypeHints.type_hint_for_var(ctx, {1, 1}, var, []) == :skip
    end

    test ":skip on uninformative var" do
      pos = {4, 5}
      md = metadata(@code, pos)
      unknown = %VarInfo{version: 1, name: :x, type: nil}
      ctx = TypeHints.request_context(md)
      assert TypeHints.type_hint_for_var(ctx, pos, unknown, []) == :skip
    end

    test "the local-sigs cache is populated and reused across hints" do
      pos = {4, 5}
      md = metadata(@code, pos)
      var = find_var(md, pos, :x)
      ctx = TypeHints.request_context(md)

      sigs_key = {TypeHints, ctx.ref, :local_sigs, M}
      env_key = {TypeHints, ctx.ref, :env, pos}
      assert Process.get(sigs_key) == nil

      r1 = TypeHints.type_hint_for_var(ctx, pos, var, [])

      # After the first call both caches are populated.
      assert Process.get(sigs_key) != nil
      assert Process.get(env_key) != nil

      # A second call returns a consistent result (served from cache).
      r2 = TypeHints.type_hint_for_var(ctx, pos, var, [])
      assert r1 == r2
    end
  end

  describe "source provenance classification" do
    defp hint_source(code, pos, name) do
      md = metadata(code, pos)
      var = find_var(md, pos, name)
      assert %VarInfo{} = var
      ctx = TypeHints.request_context(md)
      {ctx, var, TypeHints.type_hint_for_var(ctx, pos, var, [])}
    end

    @tag :requires_native_types
    test "remote call to an ExCk-backed stdlib function → :native_exck" do
      # Sanity: File.read/1 must actually have an ExCk :sig in this build.
      assert match?(
               {:ok, _},
               ExCkReader.lookup_signature(File, :read, 1)
             )

      # NOTE on the fixture layout: the native engine collapses many resolvable
      # remote calls to a plain descr at the usage site, discarding the call
      # thunk we attribute from (see the "gaps" note in the report). A remote
      # call whose return the native engine leaves as a `{:call, ...}` thunk AND
      # that Binding can still render is required to exercise this path; `File.read/1`
      # in this two-assign-then-tuple layout is such a case.
      code = """
      defmodule M do
        def f do
          x = File.read("path")
          z = File.read("path")
          {x, z}
        end
      end
      """

      {_ctx, _var, result} = hint_source(code, {5, 5}, :x)
      assert {:ok, %{source: :native_exck}} = result
    end

    @tag :requires_native_types
    test "local call to a function with only a @spec (no native sig) → :spec" do
      # `defdelegate` produces no natively-inferred sig (sig_source nil), but the
      # @spec is reachable via spec_signature_from_metadata. Local-call thunks are
      # preserved at the usage site, so the local classification path fires.
      code = """
      defmodule M do
        @spec b() :: atom
        defdelegate b(), to: SomeMod
        def f do
          y = b()
          z = b()
          {y, z}
        end
      end
      """

      {_ctx, _var, result} = hint_source(code, {7, 5}, :y)
      assert {:ok, %{source: :spec}} = result
    end

    @tag :requires_native_types
    test "local call with a native-inferred sig → :native_inferred" do
      code = """
      defmodule M do
        def h(n), do: n + 1
        def f do
          z = h(1)
          w = h(2)
          {z, w}
        end
      end
      """

      {_ctx, _var, result} = hint_source(code, {6, 5}, :z)
      assert {:ok, %{source: :native_inferred}} = result
    end

    test "native-descr var (render said :native) → :native_inferred" do
      # A var whose displayed text comes from the native descriptor path (render
      # reports :native) is classified :native_inferred. Build such a var with a
      # real Module.Types descriptor and no structural type, mirroring the
      # TypePresentation native golden.
      if ElixirTypes.available?() do
        descr =
          Descr.union(Descr.binary(), Descr.atom([nil]))

        code = """
        defmodule M do
          def f do
            x = "literal"
            x
          end
        end
        """

        pos = {4, 5}
        md = metadata(code, pos)

        var = %VarInfo{version: 1, name: :x, type: nil, elixir_types_descr: descr}
        ctx = TypeHints.request_context(md)

        assert {:ok, %{source: :native_inferred, full: full}} =
                 TypeHints.type_hint_for_var(ctx, pos, var, [])

        assert full =~ "binary()"
      end
    end

    test "plain literal-derived var (container/literal) → :shape" do
      code = """
      defmodule M do
        def f do
          w = [1, 2, 3]
          w
        end
      end
      """

      {_ctx, _var, result} = hint_source(code, {4, 5}, :w)
      assert {:ok, %{source: :shape}} = result
    end

    test "trust_rank/1 imposes the documented ordering" do
      assert TypeHints.trust_rank(:native_exck) == 0
      assert TypeHints.trust_rank(:native_inferred) == 1
      assert TypeHints.trust_rank(:spec) == 2
      assert TypeHints.trust_rank(:shape) == 3

      assert TypeHints.trust_rank(:native_exck) < TypeHints.trust_rank(:native_inferred)
      assert TypeHints.trust_rank(:native_inferred) < TypeHints.trust_rank(:spec)
      assert TypeHints.trust_rank(:spec) < TypeHints.trust_rank(:shape)
    end

    test "classification failure (nonexistent remote module) falls back to :shape, no crash" do
      code = """
      defmodule M do
        def f do
          x = NoSuchModule.nope("a")
          x
        end
      end
      """

      md = metadata(code, {4, 5})
      var = find_var(md, {4, 5}, :x)
      assert %VarInfo{} = var
      ctx = TypeHints.request_context(md)

      # Must not raise; if a hint is produced its source is the safe :shape.
      case TypeHints.type_hint_for_var(ctx, {4, 5}, var, []) do
        {:ok, %{source: source}} -> assert source == :shape
        :skip -> :ok
      end
    end

    @tag :requires_native_types
    test "attribution is cached per {ref, mfa} in the process dictionary" do
      code = """
      defmodule M do
        def f do
          x = File.read("path")
          z = File.read("path")
          {x, z}
        end
      end
      """

      pos = {5, 5}
      md = metadata(code, pos)
      var = find_var(md, pos, :x)
      ctx = TypeHints.request_context(md)

      cache_key = {TypeHints, ctx.ref, :source, {File, :read, 1}}
      assert Process.get(cache_key) == nil

      TypeHints.type_hint_for_var(ctx, pos, var, [])
      assert Process.get(cache_key) == :native_exck
    end
  end

  describe "effective_params/4 — metadata modules" do
    defp ctx_for(code) do
      code |> metadata({1, 1}) |> TypeHints.request_context()
    end

    test "trailing default elided at lower arity" do
      ctx = ctx_for("defmodule M do\n  def f(a, b \\\\ 1), do: {a, b}\nend\n")

      assert TypeHints.effective_params(ctx, M, :f, 2) ==
               {:ok, [%{name: "a", has_default: false}, %{name: "b", has_default: true}]}

      assert TypeHints.effective_params(ctx, M, :f, 1) ==
               {:ok, [%{name: "a", has_default: false}]}
    end

    test "non-trailing default: f(a, b \\\\ 1, c) at arity 2 → [a, c]" do
      ctx = ctx_for("defmodule M do\n  def f(a, b \\\\ 1, c), do: {a, b, c}\nend\n")

      assert TypeHints.effective_params(ctx, M, :f, 2) ==
               {:ok, [%{name: "a", has_default: false}, %{name: "c", has_default: false}]}

      assert TypeHints.effective_params(ctx, M, :f, 3) ==
               {:ok,
                [
                  %{name: "a", has_default: false},
                  %{name: "b", has_default: true},
                  %{name: "c", has_default: false}
                ]}
    end

    test "two defaults at lower arity matches the empirical left-to-right rule" do
      # g(a \\ 1, b \\ 2, c): /1 → [c]; /2 → [a, c] (leftmost default kept).
      ctx = ctx_for("defmodule M do\n  def g(a \\\\ 1, b \\\\ 2, c), do: {a, b, c}\nend\n")

      assert TypeHints.effective_params(ctx, M, :g, 1) ==
               {:ok, [%{name: "c", has_default: false}]}

      assert TypeHints.effective_params(ctx, M, :g, 2) ==
               {:ok, [%{name: "a", has_default: true}, %{name: "c", has_default: false}]}
    end

    test "pattern-match default %{} = opts \\\\ %{} → name \"opts\" (AST-level)" do
      ctx = ctx_for("defmodule M do\n  def h(%{} = opts \\\\ %{}), do: opts\nend\n")

      assert TypeHints.effective_params(ctx, M, :h, 1) ==
               {:ok, [%{name: "opts", has_default: true}]}

      assert TypeHints.effective_params(ctx, M, :h, 0) == {:ok, []}
    end

    test "unnamed literal pattern → nil name entry" do
      ctx = ctx_for("defmodule M do\n  def lit(1, x), do: {x}\nend\n")

      assert TypeHints.effective_params(ctx, M, :lit, 2) ==
               {:ok, [%{name: nil, has_default: false}, %{name: "x", has_default: false}]}
    end

    test ":error for an unknown function" do
      ctx = ctx_for("defmodule M do\n  def f(a), do: a\nend\n")
      assert TypeHints.effective_params(ctx, M, :nope, 1) == :error
    end

    test "result is cached per {ref, module, fun, arity}" do
      ctx = ctx_for("defmodule M do\n  def f(a, b \\\\ 1), do: {a, b}\nend\n")
      key = {TypeHints, ctx.ref, :params, M, :f, 1}
      assert Process.get(key) == nil
      result = TypeHints.effective_params(ctx, M, :f, 1)
      assert Process.get(key) == result
    end
  end

  describe "source provenance — default-arity attribution (Task 1)" do
    # Regression: `attribute_thunk` for `:local_call` previously looked up
    # `mods_funs_to_positions[{module, fun, call_arity}]` directly. Default-arg
    # functions are keyed only under max arity, so `f(1)` for `def f(a, b \\ 1)`
    # missed the entry and fell back to `:shape`. The fix uses
    # `find_mod_fun_info/4` which searches all {module, fun, *} entries and checks
    # the defaults window.
    test "f(x) for def f(a, b \\\\ 1) classifies :native_inferred, not :shape" do
      code = """
      defmodule DefaultArity do
        def f(a, b \\\\ 1), do: {a, b}
        def g do
          x = f(1)
          y = f(2)
          {x, y}
        end
      end
      """

      pos = {6, 5}
      md = metadata(code, pos)
      var = find_var(md, pos, :x)
      assert %VarInfo{} = var
      ctx = TypeHints.request_context(md)
      result = TypeHints.type_hint_for_var(ctx, pos, var, [])

      case result do
        {:ok, %{source: source}} ->
          # Must not be :shape — the function has a native-inferred sig.
          assert source in [:native_inferred, :native_exck, :spec],
                 "expected :native_inferred (or better), got :shape — default-arity attribution bug"

        :skip ->
          # If the type is not rendered at all that is acceptable (not a regression).
          :ok
      end
    end
  end

  describe "discard/1 (Task 2)" do
    test "discard erases all process-dictionary entries belonging to ctx" do
      pos = {4, 5}
      md = metadata(@code, pos)
      var = find_var(md, pos, :x)
      ctx = TypeHints.request_context(md)

      # Populate caches by making a real call.
      TypeHints.type_hint_for_var(ctx, pos, var, [])

      # At least the env and local_sigs entries should be present.
      keys_before =
        :erlang.get_keys()
        |> Enum.filter(fn
          key when is_tuple(key) and tuple_size(key) >= 3 ->
            elem(key, 0) == TypeHints and elem(key, 1) == ctx.ref

          _ ->
            false
        end)

      assert keys_before != [], "expected some pdict entries to exist before discard"

      TypeHints.discard(ctx)

      keys_after =
        :erlang.get_keys()
        |> Enum.filter(fn
          key when is_tuple(key) and tuple_size(key) >= 3 ->
            elem(key, 0) == TypeHints and elem(key, 1) == ctx.ref

          _ ->
            false
        end)

      assert keys_after == [], "expected all pdict entries to be gone after discard"
    end

    test "a call after discard still works (re-populates cache)" do
      pos = {4, 5}
      md = metadata(@code, pos)
      var = find_var(md, pos, :x)
      ctx = TypeHints.request_context(md)

      # First call, then discard.
      result1 = TypeHints.type_hint_for_var(ctx, pos, var, [])
      TypeHints.discard(ctx)

      # Call again — must re-populate and return the same result.
      result2 = TypeHints.type_hint_for_var(ctx, pos, var, [])
      assert result1 == result2

      # Cache must be re-populated.
      keys =
        :erlang.get_keys()
        |> Enum.filter(fn
          key when is_tuple(key) and tuple_size(key) >= 3 ->
            elem(key, 0) == TypeHints and elem(key, 1) == ctx.ref

          _ ->
            false
        end)

      assert keys != []
    end

    test "discard on a fresh (unused) context is a no-op" do
      md = metadata(@code, {4, 5})
      ctx = TypeHints.request_context(md)
      # Must not raise.
      assert TypeHints.discard(ctx) == :ok
    end
  end

  describe "type_hint_at/4 — flow-sensitive read-position API" do
    # Empirical probe result: inside `cond do is_integer(x) ->` at line 5,
    # env.vars contains VarInfo{name: :x, type: :integer} (narrowed), whereas
    # at the binding site (line 2) env.vars contains VarInfo{name: :x, type: nil}.
    # The metadata model DOES support flow-sensitivity at read positions via
    # the per-line lines_to_env map populated by the compiler walk.

    @flow_code """
    defmodule FlowM do
      def f(x) do
        cond do
          is_integer(x) ->
            x
          true ->
            x
        end
      end
    end
    """

    test "returns :skip when var name is not in scope at position" do
      md = metadata(@flow_code, {5, 9})
      ctx = TypeHints.request_context(md)
      assert TypeHints.type_hint_at(ctx, {5, 9}, :no_such_var, []) == :skip
    end

    test "flow-sensitivity: hint at read position inside narrowing branch shows narrowed type" do
      # At position {5, 9} (x inside `is_integer(x) ->` branch), the env.vars
      # entry for x has type: :integer — the narrowed type, not nil/term().
      md = metadata(@flow_code, {5, 9})
      ctx = TypeHints.request_context(md)
      result = TypeHints.type_hint_at(ctx, {5, 9}, :x, [])

      # The hint should be produced and show integer narrowing.
      assert {:ok, %{label: label, full: full, source: source}} = result

      assert label =~ "integer" or full =~ "integer",
             "Expected integer type in hint, got label=#{inspect(label)} full=#{inspect(full)}"

      # Source must be one of the valid provenance values.
      assert source in [:native_exck, :native_inferred, :spec, :shape]
    end

    test "un-narrowed branch shows broader/nil type (different from narrowed)" do
      # At position {7, 9} (x inside `true ->` branch), the env.vars entry for x
      # has type: nil (no narrowing), so the hint may be :skip or a broader type.
      md = metadata(@flow_code, {7, 9})
      ctx = TypeHints.request_context(md)
      result_broad = TypeHints.type_hint_at(ctx, {7, 9}, :x, [])

      md2 = metadata(@flow_code, {5, 9})
      ctx2 = TypeHints.request_context(md2)
      result_narrow = TypeHints.type_hint_at(ctx2, {5, 9}, :x, [])

      # The two results must differ (flow-sensitivity is observable).
      # result_narrow must be {:ok, ...} with integer; result_broad may be :skip
      # (if nil type renders nothing) or a non-integer hint.
      assert result_narrow != result_broad,
             "Flow-sensitivity not observable: both positions returned #{inspect(result_narrow)}"
    end

    test "result is cached per {ref, :hint_at, position, var_name}" do
      md = metadata(@flow_code, {5, 9})
      pos = {5, 9}
      ctx = TypeHints.request_context(md)
      cache_key = {TypeHints, ctx.ref, :hint_at, pos, :x}

      assert Process.get(cache_key) == nil
      result = TypeHints.type_hint_at(ctx, pos, :x, [])
      assert Process.get(cache_key) == result
    end

    test "discard/1 also erases hint_at cache entries" do
      md = metadata(@flow_code, {5, 9})
      pos = {5, 9}
      ctx = TypeHints.request_context(md)

      TypeHints.type_hint_at(ctx, pos, :x, [])
      cache_key = {TypeHints, ctx.ref, :hint_at, pos, :x}
      assert Process.get(cache_key) != nil

      TypeHints.discard(ctx)
      assert Process.get(cache_key) == nil
    end

    test "parity: type_hint_at matches type_hint_for_var when called with the env's own VarInfo" do
      # type_hint_at(ctx, pos, :x) should produce the same result as
      # type_hint_for_var(ctx, pos, env.vars[x]) at the same position.
      pos = {5, 9}
      md = metadata(@flow_code, pos)
      env = Metadata.get_env(md, pos)
      var_info = Enum.find(env.vars, &(&1.name == :x))
      assert %VarInfo{} = var_info

      ctx = TypeHints.request_context(md)
      result_at = TypeHints.type_hint_at(ctx, pos, :x, [])
      result_for = TypeHints.type_hint_for_var(ctx, pos, var_info, [])

      assert result_at == result_for
    end

    test "works for a plain (non-flow-sensitive) var too" do
      code = """
      defmodule PlainM do
        def f do
          x = :ok
          x
        end
      end
      """

      pos = {4, 5}
      md = metadata(code, pos)
      ctx = TypeHints.request_context(md)

      assert {:ok, %{label: ":ok"}} = TypeHints.type_hint_at(ctx, pos, :x, [])
    end
  end

  describe "effective_params/4 — remote / compiled modules" do
    test "String.split/2 vs /3 default detection" do
      ctx = ctx_for("defmodule M do\n  def x, do: :ok\nend\n")

      # String.split/3 head is (string, pattern, options \\ []); /2 elides the default.
      assert {:ok, params2} = TypeHints.effective_params(ctx, String, :split, 2)
      assert length(params2) == 2
      assert Enum.all?(params2, &(&1.has_default == false))

      assert {:ok, params3} = TypeHints.effective_params(ctx, String, :split, 3)
      assert length(params3) == 3
      assert List.last(params3).has_default == true
    end

    test "GenServer.start_link/3 has a trailing default option" do
      ctx = ctx_for("defmodule M do\n  def x, do: :ok\nend\n")

      assert {:ok, params} = TypeHints.effective_params(ctx, GenServer, :start_link, 3)
      assert length(params) == 3
      assert List.last(params).has_default == true
      assert List.last(params).name == "options"
    end
  end
end
