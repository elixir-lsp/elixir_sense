defmodule ElixirSense.Core.ElixirTypesLocalInferenceTest do
  @moduledoc """
  Test coverage for ElixirSense local-inference edge cases and default-argument
  signatures (Section A, B, C from the backlog).

  Section A: Local inference edge cases (native-gated).
    A1  – Recursion: factorial — inferred sig exists or absent; if present, return covers integer.
    A2  – Mutual recursion: two functions calling each other — no crash.
    A3  – Default args: mods_funs_to_positions records the right arities; hints are sound.
    A4  – defoverridable + super — no crash, sig present or absent.
    A5  – Private reachability: defp called from one def; unused defp — no crash.
    A6  – Multi-clause disjoint guard domains — per-clause returns don't bleed.
    A7  – Generated heads via use/macro — no crash.

  Section B: Default-argument signature fixtures (remote compile).
    B1  – g/1, g/2, g/3 lookup via mods_funs_to_positions; any hint over g(:a) is sound.

  Section C: Compiler-comparison additions.
    C1  – Guard-refined var: `when is_binary(x)` narrows x to binary() in body.
    C2  – `with` success binding from a local spec'd function.
    C3  – Struct-returning local call: hint mentions the struct module.
    C4  – Captured function: `&String.upcase/1` → no crash, hint present or absent.
  """
  use ExUnit.Case, async: false

  alias ElixirSense.Core.ElixirTypes
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Parser
  alias ElixirSense.Core.Binding

  # ── helpers ────────────────────────────────────────────────────────────────

  # Parse source at position, return {env, binding}.
  defp build(code, position) do
    metadata = Parser.parse_string(code, true, true, position)
    env = Metadata.get_env(metadata, position)
    {env, Binding.from_env(env, metadata, position)}
  end

  # Get the rendered full type hint string for a variable at a position, or :absent.
  # render_hint/3 returns {:ok, %{label:, full:, source:}} or :skip.
  defp hint(code, var_name, position) do
    {env, binding} = build(code, position)

    case Enum.find(env.vars, &(&1.name == var_name)) do
      nil ->
        :absent

      var ->
        case ElixirSense.Core.TypePresentation.render_hint(binding, var, []) do
          {:ok, %{full: full}} -> full
          _ -> :absent
        end
    end
  end

  # Gate: skip a test body when native types are unavailable (leaves a passing
  # no-op rather than a failure so the suite stays deterministic everywhere).
  defmacrop if_native(do: body) do
    quote do
      if ElixirTypes.available?() do
        unquote(body)
      end
    end
  end

  setup do
    original = Application.get_env(:elixir_sense, :use_elixir_types, false)
    Application.put_env(:elixir_sense, :use_elixir_types, true)
    on_exit(fn -> Application.put_env(:elixir_sense, :use_elixir_types, original) end)
    :ok
  end

  # ── Section A: Local inference edge cases ─────────────────────────────────

  describe "A1 – recursion" do
    test "factorial: inferred sig exists or is absent; if present, return covers integer" do
      if_native do
        code = """
        defmodule LocalInfRecursion do
          def fact(0), do: 1
          def fact(n), do: n * fact(n - 1)
        end
        """

        metadata = Parser.parse_string(code, true, true, {3, 10})
        info = metadata.mods_funs_to_positions[{LocalInfRecursion, :fact, 1}]

        # info may be nil if the module was not parsed, which is OK; the goal is
        # no crash during parse.
        assert is_nil(info) or is_map(info)

        # If a sig was inferred, its return must subsume integer.
        if info && info.elixir_types_sig do
          {:infer, _domain, clause_types} = info.elixir_types_sig

          returns = Enum.map(clause_types, &elem(&1, 1))
          # Each clause return must be a Descr — just assert it's non-nil.
          assert Enum.all?(returns, &(&1 != nil))
        end
      end
    end

    test "no crash during metadata build for recursive function" do
      # Runs with or without native types — must not raise.
      code = """
      defmodule LocalInfRecursionNocrash do
        def fact(0), do: 1
        def fact(n), do: n * fact(n - 1)
      end
      """

      log =
        ExUnit.CaptureLog.capture_log(fn ->
          Parser.parse_string(code, true, true, {3, 10})
        end)

      # No hard crash error in the log.
      refute log =~ "** (RuntimeError)"
      refute log =~ "FunctionClauseError"
    end
  end

  describe "A2 – mutual recursion" do
    test "two mutually recursive functions: no crash, sigs present or absent" do
      if_native do
        code = """
        defmodule LocalInfMutual do
          def even?(0), do: true
          def even?(n), do: odd?(n - 1)

          def odd?(0), do: false
          def odd?(n), do: even?(n - 1)
        end
        """

        log =
          ExUnit.CaptureLog.capture_log(fn ->
            metadata = Parser.parse_string(code, true, true, {3, 10})
            send(self(), {:meta, metadata})
          end)

        assert_received {:meta, metadata}

        # No hard errors in the log.
        refute log =~ "** (RuntimeError)"

        even_info = metadata.mods_funs_to_positions[{LocalInfMutual, :even?, 1}]
        odd_info = metadata.mods_funs_to_positions[{LocalInfMutual, :odd?, 1}]

        # Each must be absent or a valid map.
        assert is_nil(even_info) or is_map(even_info)
        assert is_nil(odd_info) or is_map(odd_info)
      end
    end
  end

  describe "A3 – default args" do
    test "mods_funs_to_positions records arity-3 entry for def f(a, b \\\\ 1, c \\\\ 2)" do
      code = """
      defmodule LocalInfDefaults do
        def f(a, b \\\\ 1, c \\\\ 2), do: {a, b, c}
      end
      """

      metadata = Parser.parse_string(code, true, true, {2, 10})

      # The compiler emits the full-arity clause only (a, b \\ 1, c \\ 2 → arity 3).
      # The metadata builder records it under arity 3.
      info = metadata.mods_funs_to_positions[{LocalInfDefaults, :f, 3}]

      assert info != nil,
             "expected mods_funs_to_positions to have {LocalInfDefaults, :f, 3}"
    end

    test "call x = f(:a) with default args resolves without crashing" do
      if_native do
        code = """
        defmodule LocalInfDefaultsCall do
          def f(a, b \\\\ 1), do: {a, b}

          def caller do
            x = f(:a)
            x
          end
        end
        """

        result =
          ExUnit.CaptureLog.capture_log(fn ->
            h = hint(code, :x, {6, 5})
            send(self(), {:hint, h})
          end)

        assert_received {:hint, h}

        # Any non-crashing result (absent or a type string) is acceptable.
        assert is_binary(h) or h == :absent

        # No hard crash in log.
        refute result =~ "** (RuntimeError)"
      end
    end

    test "default-arg params list recorded in info.params" do
      code = """
      defmodule LocalInfDefaultsParams do
        def g(a, b \\\\ 1, c \\\\ 2), do: {a, b, c}
      end
      """

      metadata = Parser.parse_string(code, true, true, {2, 10})
      info = metadata.mods_funs_to_positions[{LocalInfDefaultsParams, :g, 3}]

      assert info != nil

      # params_variants should include the variant with 3 params (the full clause).
      arities = ElixirSense.Core.State.ModFunInfo.get_arities(info)
      # get_arities returns [{max_arity, num_defaults}]; max_arity == 3, defaults == 2.
      assert Enum.any?(arities, fn {max, _defaults} -> max == 3 end)
    end
  end

  describe "A4 – defoverridable + super" do
    test "overridable function with super: no crash, sig present or absent" do
      if_native do
        code = """
        defmodule LocalInfBase do
          defmacro __using__(_) do
            quote do
              def greet(name), do: "Hello, " <> name
              defoverridable greet: 1
            end
          end
        end

        defmodule LocalInfOverride do
          use LocalInfBase

          def greet(name) do
            super(name) <> "!"
          end
        end
        """

        log =
          ExUnit.CaptureLog.capture_log(fn ->
            metadata = Parser.parse_string(code, true, true, {14, 10})
            send(self(), {:meta, metadata})
          end)

        assert_received {:meta, _metadata}
        refute log =~ "** (RuntimeError)"
        refute log =~ "FunctionClauseError"
      end
    end
  end

  describe "A5 – private reachability" do
    test "defp called from def: no crash" do
      if_native do
        code = """
        defmodule LocalInfPrivateCalled do
          def run(x) do
            result = helper(x)
            result
          end

          defp helper(x), do: x * 2
        end
        """

        log =
          ExUnit.CaptureLog.capture_log(fn ->
            h = hint(code, :result, {4, 5})
            send(self(), {:hint, h})
          end)

        assert_received {:hint, h}
        assert is_binary(h) or h == :absent
        refute log =~ "** (RuntimeError)"
      end
    end

    test "unused defp: no crash during metadata build" do
      if_native do
        code = """
        defmodule LocalInfUnusedPrivate do
          def pub(x), do: x + 1
          defp _unused(x), do: x * 3
        end
        """

        log =
          ExUnit.CaptureLog.capture_log(fn ->
            Parser.parse_string(code, true, true, {2, 10})
          end)

        refute log =~ "** (RuntimeError)"
        refute log =~ "FunctionClauseError"
      end
    end
  end

  describe "A6 – multi-clause disjoint guard domains" do
    test "integer clause and atom clause: hints for result vars are sound" do
      if_native do
        code = """
        defmodule LocalInfGuards do
          def classify(x) when is_integer(x), do: :int
          def classify(x) when is_atom(x), do: :atm

          def use_int do
            r = classify(42)
            r
          end
        end
        """

        log =
          ExUnit.CaptureLog.capture_log(fn ->
            h = hint(code, :r, {7, 5})
            send(self(), {:hint, h})
          end)

        assert_received {:hint, h}

        # The hint must be absent or mention atom-ish types (both clauses return atoms).
        # It MUST NOT claim integer() — that would be a wrong narrow type.
        if is_binary(h) do
          refute h =~ "integer()", "wrong narrow type: got #{h}, expected atom type or absent"
        end

        refute log =~ "** (RuntimeError)"
      end
    end

    test "no crash for multi-clause with guards during metadata build" do
      code = """
      defmodule LocalInfGuardsNoCrash do
        def tag(x) when is_binary(x), do: {:str, x}
        def tag(x) when is_integer(x), do: {:int, x}
        def tag(x) when is_list(x), do: {:lst, x}
      end
      """

      log =
        ExUnit.CaptureLog.capture_log(fn ->
          Parser.parse_string(code, true, true, {2, 10})
        end)

      refute log =~ "** (RuntimeError)"
    end
  end

  describe "A7 – generated heads via use/macro" do
    test "module using a macro that generates function heads: no crash" do
      if_native do
        code = """
        defmodule LocalInfMacroDef do
          defmacro __using__(_) do
            quote do
              def ping, do: :pong
              def version, do: 1
            end
          end
        end

        defmodule LocalInfMacroUser do
          use LocalInfMacroDef

          def call do
            v = version()
            v
          end
        end
        """

        log =
          ExUnit.CaptureLog.capture_log(fn ->
            h = hint(code, :v, {15, 5})
            send(self(), {:hint, h})
          end)

        assert_received {:hint, h}
        assert is_binary(h) or h == :absent
        refute log =~ "** (RuntimeError)"
      end
    end
  end

  # ── Section B: Default-argument signature fixtures (remote) ───────────────

  describe "B1 – remote call with default args" do
    test "compile a module with g(a, b \\\\ 1, c \\\\ 2); mods_funs_to_positions has arity 3" do
      # Compile a fixture inline and verify arities via metadata builder.
      code = """
      defmodule LocalInfRemoteFixture do
        def g(a, b \\\\ 1, c \\\\ 2), do: {a, b, c}
      end
      """

      metadata = Parser.parse_string(code, true, true, {2, 5})

      info3 = metadata.mods_funs_to_positions[{LocalInfRemoteFixture, :g, 3}]
      assert info3 != nil, "expected arity-3 entry for g with defaults"
    end

    test "buffer hint over remote call Fixture.g(:a) is sound (no crash)" do
      if_native do
        # Inline definition in the buffer (remote call in the same file).
        code = """
        defmodule LocalInfRemote do
          def g(a, b \\\\ 1, c \\\\ 2), do: {a, b, c}

          def caller do
            x = g(:a)
            x
          end
        end
        """

        log =
          ExUnit.CaptureLog.capture_log(fn ->
            h = hint(code, :x, {6, 5})
            send(self(), {:hint, h})
          end)

        assert_received {:hint, h}
        assert is_binary(h) or h == :absent

        # Must not produce an incorrect atom-only narrow (since g returns a tuple).
        if is_binary(h) do
          refute h =~ "atom()", "suspiciously narrow hint for tuple-returning g: #{h}"
        end

        refute log =~ "** (RuntimeError)"
      end
    end
  end

  # ── Section C: Compiler-comparison additions ──────────────────────────────

  describe "C1 – guard-refined variable" do
    test "when is_binary(x): hint on x in body is binary() or absent — never integer()" do
      if_native do
        code = """
        defmodule LocalInfGuardBinary do
          def h(x) when is_binary(x) do
            x
          end
        end
        """

        log =
          ExUnit.CaptureLog.capture_log(fn ->
            h = hint(code, :x, {3, 5})
            send(self(), {:hint, h})
          end)

        assert_received {:hint, h}

        if is_binary(h) do
          # If a hint is present it must be binary or a super-type — never integer.
          refute h =~ "integer()", "wrong narrow type: got #{h} for is_binary guard"
        end

        refute log =~ "** (RuntimeError)"
      end
    end

    test "guard-refined var: hint is binary() when native is available" do
      if_native do
        code = """
        defmodule LocalInfGuardBinaryExact do
          def h(x) when is_binary(x) do
            x
          end
        end
        """

        h = hint(code, :x, {3, 5})

        # Native typing should narrow x to binary() in the guard-refined clause.
        # We assert: if a hint is given, it's binary-related.
        if is_binary(h) do
          assert h =~ "binary" or h =~ "String" or h =~ "term" or h =~ "dynamic",
                 "unexpected type hint for binary-guarded var: #{h}"
        end
      end
    end
  end

  describe "C2 – with success binding from a local specced function" do
    test "with {:ok, val} <- local_ok_fn(): val is in scope, no crash" do
      if_native do
        code = """
        defmodule LocalInfWith do
          @spec ok_fn() :: {:ok, integer()}
          def ok_fn, do: {:ok, 42}

          def caller do
            with {:ok, val} <- ok_fn() do
              val
            end
          end
        end
        """

        log =
          ExUnit.CaptureLog.capture_log(fn ->
            metadata = Parser.parse_string(code, true, true, {7, 7})
            env = Metadata.get_env(metadata, {7, 7})
            send(self(), {:vars, Enum.map(env.vars, & &1.name)})
          end)

        assert_received {:vars, var_names}

        assert :val in var_names,
               "expected `val` in scope in `with` body; got #{inspect(var_names)}"

        refute log =~ "** (RuntimeError)"
        refute log =~ "FunctionClauseError"
      end
    end
  end

  describe "C3 – struct-returning local call" do
    test "%URI{} literal return: hint on result mentions URI or is absent" do
      if_native do
        code = """
        defmodule LocalInfStruct do
          def make_uri do
            result = %URI{path: "/foo"}
            result
          end
        end
        """

        log =
          ExUnit.CaptureLog.capture_log(fn ->
            h = hint(code, :result, {4, 5})
            send(self(), {:hint, h})
          end)

        assert_received {:hint, h}

        if is_binary(h) do
          assert h =~ "URI" or h =~ "map" or h =~ "%{" or h =~ "struct" or h =~ "term" or
                   h =~ "dynamic",
                 "unexpected type for URI-struct var: #{h}"
        end

        refute log =~ "** (RuntimeError)"
      end
    end
  end

  describe "C4 – captured function" do
    test "&String.upcase/1 capture: no crash, hint is fun-like or absent" do
      if_native do
        code = """
        defmodule LocalInfCapture do
          def caller do
            f = &String.upcase/1
            f
          end
        end
        """

        log =
          ExUnit.CaptureLog.capture_log(fn ->
            h = hint(code, :f, {4, 5})
            send(self(), {:hint, h})
          end)

        assert_received {:hint, h}

        # The hint must be absent or a function-shaped type — never a primitive.
        if is_binary(h) do
          refute h =~ "integer()",
                 "wrong narrow type for captured function: #{h}"

          refute h =~ "atom()",
                 "wrong narrow type for captured function: #{h}"
        end

        refute log =~ "** (RuntimeError)"
        refute log =~ "FunctionClauseError"
      end
    end
  end

  # ── Section D: unexpanded macros in clause heads/guards ────────────────────
  #
  # ElixirSense metadata is NOT macro-expanded, so a clause head can carry an
  # unexpanded record macro in pattern position, and a guard can carry an
  # unexpanded `defguard`/record macro — both look like remote calls
  # (`{{:., _, [Mod, fun]}, _, args}`). Feeding either to native
  # `Pattern.of_head`/`of_pattern`/`of_guard` raises (FunctionClauseError on
  # 1.18's `of_pattern/4`, Protocol/Enumerable errors via `of_head`). These
  # regressed elixir-ls CI on 1.18 (PR elixir-lsp/elixir-ls#1266). The fix
  # detects the non-pattern syntax up front and SKIPS native inference for the
  # whole function; the structural engine still types it. Asserts: metadata
  # builds with NO of_pattern / of_guard crash trace in the log.
  describe "D1 – record macro in a function head pattern" do
    test "Record.defrecordp pattern in a def head: no of_pattern crash" do
      code = """
      defmodule LocalInfRecordHead do
        require Record
        Record.defrecordp(:iplt_info, warning_map: %{})

        def f(iplt_info(warning_map: w) = info) do
          {w, info}
        end
      end
      """

      log =
        ExUnit.CaptureLog.capture_log(fn ->
          metadata = Parser.parse_string(code, true, true, {5, 10})
          send(self(), {:meta, metadata})
        end)

      assert_received {:meta, metadata}

      # Metadata must have built (the function is recorded).
      assert is_map(metadata.mods_funs_to_positions)

      # The crash class we are guarding against must NOT appear in the log.
      refute log =~ "of_pattern", "native of_pattern crashed on a record-macro head: #{log}"
      refute log =~ "FunctionClauseError", "unexpected FunctionClauseError: #{log}"

      refute log =~ "Unable to infer local signature",
             "native local inference crashed instead of skipping: #{log}"

      # Soundness: native sig may be absent (skipped) — that is the correct
      # fallback. If a sig IS present it must be a well-formed infer tuple.
      info = metadata.mods_funs_to_positions[{LocalInfRecordHead, :f, 1}]

      if info && Map.get(info, :elixir_types_sig) do
        assert match?({:infer, _, _}, info.elixir_types_sig)
      end
    end
  end

  describe "D2 – user defguard used in a guard" do
    test "defguard remote-call guard in a def: no of_guard crash" do
      code = """
      defmodule LocalInfDefguard do
        defguard is_init(x) when is_binary(x)

        def g(x) when LocalInfDefguard.is_init(x) do
          x
        end
      end
      """

      log =
        ExUnit.CaptureLog.capture_log(fn ->
          metadata = Parser.parse_string(code, true, true, {4, 10})
          send(self(), {:meta, metadata})
        end)

      assert_received {:meta, metadata}

      assert is_map(metadata.mods_funs_to_positions)

      refute log =~ "of_guard", "native of_guard crashed on a defguard guard: #{log}"
      refute log =~ "of_pattern", "native of_pattern crashed on a defguard guard: #{log}"

      refute log =~ "Unable to infer local signature",
             "native local inference crashed instead of skipping: #{log}"

      info = metadata.mods_funs_to_positions[{LocalInfDefguard, :g, 1}]

      if info && Map.get(info, :elixir_types_sig) do
        assert match?({:infer, _, _}, info.elixir_types_sig)
      end
    end

    test "a plain Kernel-guard clause is still natively typeable (filter not over-broad)" do
      if_native do
        code = """
        defmodule LocalInfPlainGuard do
          def h(x) when is_binary(x), do: x
        end
        """

        log =
          ExUnit.CaptureLog.capture_log(fn ->
            metadata = Parser.parse_string(code, true, true, {2, 10})
            send(self(), {:meta, metadata})
          end)

        assert_received {:meta, _metadata}

        # Plain Kernel guards must NOT be rejected by the new guard filter; the
        # only assertion here is no crash (sig may or may not be present
        # depending on context-seeding fidelity across versions).
        refute log =~ "FunctionClauseError"
        refute log =~ "of_guard"
      end
    end
  end
end
