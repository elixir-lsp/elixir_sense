defmodule ElixirSense.Core.ElixirTypesFailureModeTest do
  @moduledoc """
  Failure-mode and graceful-degradation tests for ElixirSense.Core.ElixirTypes
  and related modules.

  Each test asserts that the system degrades gracefully — returning an error
  tuple or nil — rather than raising an exception.
  """

  use ExUnit.Case, async: false

  alias ElixirSense.Core.ElixirTypes
  alias ElixirSense.Core.ExCkReader
  alias ElixirSense.Core.TypePresentation
  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.State.VarInfo

  # ---------------------------------------------------------------------------
  # Private API drift canaries
  #
  # These tests do NOT attempt to unload compiler modules (that is not possible
  # at runtime without restarting the VM). Instead they exercise the adaptor's
  # own guard/fallback paths by feeding malformed or boundary inputs — the same
  # inputs that have historically triggered raises in the wild — and assert that
  # every public entry point returns {:error} / :error / nil rather than raising.
  # They also pin the capabilities() key-set so any upstream addition or removal
  # fails loudly here instead of silently regressing ElixirSense.
  # ---------------------------------------------------------------------------

  describe "private API drift canaries" do
    # -----------------------------------------------------------------------
    # (f) capabilities/0 key-set is exactly the documented set
    #
    # If Elixir adds or removes a probed API the capabilities map changes size.
    # That would silently mis-report availability unless caught here. Any diff
    # between the returned keys and the set below requires a CONSCIOUS update to
    # the capability type, the probes, and this assertion.
    # -----------------------------------------------------------------------
    test "capabilities/0 returns exactly the documented key set" do
      caps = ElixirTypes.capabilities()

      expected_keys =
        MapSet.new([
          # Public boolean capabilities (queried via available?/1).
          :expr,
          :expr_basic,
          :pattern_match,
          :head,
          :local_signature,
          :previous,
          # Memoized internal dispatch variants (atoms, not booleans).
          :expr_api,
          :pattern_api,
          # Memoized Descr private-API probe booleans.
          :descr_gradual,
          :descr_disjoint,
          :descr_compatible,
          :descr_only_gradual,
          :descr_bitstring,
          :descr_fun_1
        ])

      actual_keys = MapSet.new(Map.keys(caps))

      assert actual_keys == expected_keys,
             "capabilities() key drift: expected #{inspect(MapSet.to_list(expected_keys))}, " <>
               "got #{inspect(MapSet.to_list(actual_keys))}. " <>
               "Update the @type capability typespec and this assertion consciously."
    end

    # -----------------------------------------------------------------------
    # (g) descr_to_string with garbage inputs degrades to :error
    # These complement the existing (d) describe block which already covers
    # several garbage inputs; these cover a few additional corner cases that
    # exercise the deepest rescue paths in descr_to_string.
    # -----------------------------------------------------------------------
    test "descr_to_string with a list (not a descr) returns :error, no raise" do
      assert ElixirTypes.descr_to_string([1, 2, 3]) == :error
    end

    test "descr_to_string with a tuple (not a descr) returns :error, no raise" do
      assert ElixirTypes.descr_to_string({:ok, :value}) == :error
    end

    # -----------------------------------------------------------------------
    # (h) apply_signature with malformed sig tuples returns :error, not raise
    # -----------------------------------------------------------------------
    test "apply_signature with a 2-tuple sig returns :error, no raise" do
      # Only 3-tuples {:kind, domain, clauses} are valid — a 2-tuple must not raise.
      result = ElixirTypes.apply_signature({:infer, []})
      assert result == :error
    end

    test "apply_signature with an atom sig (not :none) returns :error, no raise" do
      result = ElixirTypes.apply_signature(:garbage_sig)
      assert result == :error
    end

    test "apply_signature with completely wrong sig shape returns :error, no raise" do
      result = ElixirTypes.apply_signature(%{not: "a sig"})
      assert result == :error
    end

    test "apply_signature with clauses containing non-list arg types degrades, no raise" do
      # A malformed clause: arg types should be a list but here it's an atom.
      # This exercises the clause_arg_types/1 catch-all clause.
      sig = {:infer, nil, [{:not_a_list_of_args, :some_return}]}
      # Should not raise — either :error or {:ok, dynamic()}
      result = ElixirTypes.apply_signature(sig, [nil])
      assert result == :error or match?({:ok, _}, result)
    end

    # -----------------------------------------------------------------------
    # (i) of_expr with inputs that historically triggered raises
    # -----------------------------------------------------------------------
    test "of_expr with an improper list (not a valid AST) returns :error, no raise" do
      # An improper list is NOT a valid Elixir list AST. The pre-check in
      # of_expr_impl rejects it before calling into native typing.
      result = ElixirTypes.of_expr([1 | :not_a_list])
      assert result == :error
    end

    test "of_expr with an AST carrying non-list metadata returns :error or {:ok,_}, no raise" do
      # An AST node with non-list meta can trigger an error in of_expr's AST walk.
      # The rescue block must catch it and return :error.
      weird_ast = {:some_call, :not_a_list_meta, []}
      result = ElixirTypes.of_expr(weird_ast)
      assert result == :error or match?({:ok, _}, result)
    end

    test "of_expr with a huge nested tuple (> 3 elements) returns :error, no raise" do
      # A 5-element tuple is not a valid AST node.
      result = ElixirTypes.of_expr({:a, :b, :c, :d, :e})
      assert result == :error
    end

    # -----------------------------------------------------------------------
    # (j) of_match with inputs that historically triggered raises
    # -----------------------------------------------------------------------
    test "of_match with a non-pattern call AST returns :error, no raise" do
      # A function call in the pattern position is not a valid match pattern.
      # native_typeable_pattern? should reject it before calling of_match.
      pattern = {:my_fun, [], [:arg]}
      match = {:=, [], [pattern, :something]}
      result = ElixirTypes.of_match(pattern, nil, match)
      assert result == :error
    end

    test "of_match with a pattern containing a typespec operator returns :error, no raise" do
      # Typespec operators like :"__::__" and :"__|__" can appear in quoted macro
      # code routed through of_match. native_typeable_pattern? must reject them.
      pattern = {:"__::__", [], [:type_a, :type_b]}
      match = {:=, [], [pattern, :something]}
      result = ElixirTypes.of_match(pattern, nil, match)
      assert result == :error
    end

    # -----------------------------------------------------------------------
    # (k) infer_local_signature with edge-case inputs returns :error, no raise
    # -----------------------------------------------------------------------
    test "infer_local_signature with empty clauses returns :error, no raise" do
      result = ElixirTypes.infer_local_signature(SomeMod, {:f, 1}, [], "nofile")
      assert result == :error
    end

    test "infer_local_signature with negative arity returns :error, no raise" do
      clauses = [%{meta: [], args: [], guards: nil, body: 42}]
      result = ElixirTypes.infer_local_signature(SomeMod, {:f, -1}, clauses, "nofile")
      assert result == :error
    end

    test "infer_local_signature with a clause containing a non-AST body returns :error or sig, no raise" do
      # A body that is not valid Elixir AST can crash native typing; the catch
      # block in do_infer_local_signature must absorb it and skip the clause.
      clauses = [%{meta: [], args: [], guards: nil, body: :not_valid_ast_at_all}]

      result =
        ElixirTypes.infer_local_signature(SomeMod, {:f, 0}, clauses, "nofile")

      assert result == :error or match?({:infer, _, _}, result)
    end

    # -----------------------------------------------------------------------
    # (l) descr_to_shape / to_shape with garbage returns nil, not raise
    # -----------------------------------------------------------------------
    test "to_shape with nil returns nil, no raise" do
      # nil is not a descr; to_shape must return nil without raising.
      result = ElixirTypes.to_shape(nil)
      assert result == nil
    end

    test "to_shape with a plain atom returns nil, no raise" do
      result = ElixirTypes.to_shape(:garbage)
      assert result == nil
    end

    test "to_shape with an integer returns nil, no raise" do
      result = ElixirTypes.to_shape(42)
      assert result == nil
    end

    # -----------------------------------------------------------------------
    # (m) args_compatible? CaseClauseError→true fallback is deliberate
    #
    # The CaseClauseError (or any raise) from descr_arg_compatible? inside
    # args_compatible? is intentionally caught and returns `true` — treating
    # the clause as compatible. This prevents a crash in an unknown descr
    # format from falsely classifying a clause as incompatible and returning
    # :error from apply_signature. The behavior is conservative (keeps the
    # clause candidate) rather than aggressive (drops it on error).
    # -----------------------------------------------------------------------
    test "apply_signature with a :strong sig and garbage arg descr degrades, no raise" do
      # If descr_arg_compatible? crashes on a garbage descr (e.g., one we can't
      # interpret), args_compatible? falls back to true (compatible), so the
      # clause is kept. This is the deliberate conservative behavior.
      # We can't easily craft a descr that crashes compatible? without
      # reaching into internals, so we just verify the overall contract:
      # :strong sig with a descr-shaped (but not Descr-content) map doesn't crash.
      if ElixirTypes.available?() do
        sig = {:strong, nil, [{[Module.Types.Descr.integer()], Module.Types.Descr.atom([:ok])}]}
        # A map that looks like a descr structurally but has garbage content.
        garbage_descr = %{dynamic: {0, 0, 0, 0, 0, 0, :garbage}, atom: :surprise}
        result = ElixirTypes.apply_signature(sig, [garbage_descr])
        # Either :error (domain violation found) or {:ok, _} (fallback kept it) — no raise.
        assert result == :error or match?({:ok, _}, result)
      end
    end
  end

  # ---------------------------------------------------------------------------
  # (a) lookup_signature on a module with no BEAM at all
  # ---------------------------------------------------------------------------

  describe "lookup_signature — module with no beam" do
    test "returns :error for a made-up atom module, no raise" do
      result = ExCkReader.lookup_signature(ElixirSense.Core.Fixtures.DoesNotExist12345, :foo, 1)
      assert result == :error
    end

    test "returns :error for a non-existent Erlang-style module, no raise" do
      result = ExCkReader.lookup_signature(:elixir_sense_totally_fake_module, :bar, 0)
      assert result == :error
    end
  end

  # ---------------------------------------------------------------------------
  # (b) lookup_signature on a module whose beam lacks the ExCk chunk
  # ---------------------------------------------------------------------------

  describe "lookup_signature — module without ExCk chunk" do
    test "Erlang :lists module has no ExCk chunk — returns :error, no raise" do
      # :lists is compiled by the Erlang compiler; its BEAM has no ExCk chunk.
      result = ExCkReader.lookup_signature(:lists, :map, 2)
      assert result == :error
    end

    test "Erlang :erlang module has no ExCk chunk — returns :error, no raise" do
      result = ExCkReader.lookup_signature(:erlang, :+, 2)
      assert result == :error
    end

    test "read_chunk with a beam binary containing no ExCk chunk returns error" do
      # Compile a tiny module in-memory; its beam will have an ExCk chunk.
      # Instead, supply a valid IFF/BEAM binary that has *no* ExCk chunk.
      # We build a minimal BEAM with only an "Attr" chunk.
      attr_payload = :erlang.term_to_binary(vsn: [1])
      attr_size = byte_size(attr_payload)
      attr_pad_count = rem(4 - rem(attr_size, 4), 4)
      attr_pad = :binary.copy(<<0>>, attr_pad_count)

      chunks_bin =
        <<"Attr", attr_size::unsigned-big-32, attr_payload::binary, attr_pad::binary>>

      beam_bin = <<"FOR1", byte_size(chunks_bin)::unsigned-big-32, "BEAM", chunks_bin::binary>>

      # Pass the raw beam binary as the chunk override — since it's not valid
      # ExCk term-encoding, this should return an error gracefully.
      result = ExCkReader.read_chunk(:fake_no_exck, chunk: beam_bin)
      assert match?({:error, _}, result), "Expected {:error, _}, got #{inspect(result)}"
    end
  end

  # ---------------------------------------------------------------------------
  # (c) Version-mismatch chunk
  # ---------------------------------------------------------------------------

  describe "version mismatch handling" do
    test "chunk with elixir_checker_v0 tag returns {:error, :version_mismatch}, no raise" do
      fake_tag = :elixir_checker_v0
      chunk_bin = :erlang.term_to_binary({fake_tag, %{exports: []}})

      result = ExCkReader.read_chunk(:fake_version, chunk: chunk_bin)
      assert result == {:error, :version_mismatch}
    end

    test "chunk with a completely unknown tag returns an error, no raise" do
      chunk_bin = :erlang.term_to_binary({:totally_unknown_tag_xyz, %{exports: []}})

      result = ExCkReader.read_chunk(:fake_unknown, chunk: chunk_bin)
      assert match?({:error, _}, result)
    end

    test "chunk with non-atom tag returns an error, no raise" do
      chunk_bin = :erlang.term_to_binary({"string_tag", %{exports: []}})

      result = ExCkReader.read_chunk(:fake_string_tag, chunk: chunk_bin)
      assert match?({:error, _}, result)
    end
  end

  # ---------------------------------------------------------------------------
  # (d) ElixirTypes.of_expr / descr_to_string with garbage input
  # ---------------------------------------------------------------------------

  describe "of_expr and descr_to_string — garbage input" do
    test "of_expr with a non-AST map returns :error, no raise" do
      result = ElixirTypes.of_expr(%{not_a_valid_ast: true})
      assert result == :error
    end

    test "descr_to_string with a non-descr map returns :error, no raise" do
      result = ElixirTypes.descr_to_string(%{garbage: :not_a_descr_map})
      assert result == :error
    end

    test "descr_to_string with an atom returns :error, no raise" do
      result = ElixirTypes.descr_to_string(:just_an_atom)
      assert result == :error
    end

    test "descr_to_string with nil returns :error, no raise" do
      result = ElixirTypes.descr_to_string(nil)
      assert result == :error
    end

    test "descr_to_string with an integer returns :error, no raise" do
      result = ElixirTypes.descr_to_string(42)
      assert result == :error
    end

    test "descr_to_string with a binary string returns :error, no raise" do
      result = ElixirTypes.descr_to_string("not a descr")
      assert result == :error
    end

    test "of_expr with an oversized tuple (not a 2 or 3-element AST node) returns :error" do
      # A 4-element tuple is not a valid AST node; of_expr should reject it.
      result = ElixirTypes.of_expr({:a, :b, :c, :d})
      assert result == :error
    end
  end

  # ---------------------------------------------------------------------------
  # (e) TypePresentation.render_hint with use_elixir_types: false
  # ---------------------------------------------------------------------------

  describe "TypePresentation.render_hint with ElixirTypes disabled" do
    setup do
      Application.put_env(:elixir_sense, :use_elixir_types, false)

      on_exit(fn ->
        Application.delete_env(:elixir_sense, :use_elixir_types)
      end)

      :ok
    end

    test "ElixirTypes.enabled?() is false when use_elixir_types is false" do
      assert ElixirTypes.enabled?() == false
    end

    test "render_hint still works on structural integer shape" do
      binding = %Binding{}
      var = %VarInfo{type: {:integer, nil}, elixir_types_descr: nil}
      result = TypePresentation.render_hint(binding, var)
      assert result == {:ok, "integer()"}
    end

    test "render_hint still works on structural atom shape" do
      binding = %Binding{}
      var = %VarInfo{type: {:atom, :ok}, elixir_types_descr: nil}
      result = TypePresentation.render_hint(binding, var)
      assert result == {:ok, ":ok"}
    end

    test "render_hint still works on structural map shape" do
      binding = %Binding{}
      var = %VarInfo{type: {:map, [foo: {:integer, nil}], nil}, elixir_types_descr: nil}
      result = TypePresentation.render_hint(binding, var)
      assert match?({:ok, s} when is_binary(s), result)
      {:ok, text} = result
      assert String.contains?(text, "foo")
    end

    test "render_hint still works on structural list shape" do
      binding = %Binding{}
      var = %VarInfo{type: {:list, {:integer, nil}}, elixir_types_descr: nil}
      result = TypePresentation.render_hint(binding, var)
      assert result == {:ok, "list(integer())"}
    end

    test "render_hint still works on structural tuple shape" do
      binding = %Binding{}
      var = %VarInfo{type: {:tuple, 2, [{:atom, :ok}, {:integer, nil}]}, elixir_types_descr: nil}
      result = TypePresentation.render_hint(binding, var)
      assert result == {:ok, "{:ok, integer()}"}
    end

    test "render_hint skips uninformative term() shape even when native backend is off" do
      binding = %Binding{}
      # nil type resolves to term(), which render_hint should skip
      var = %VarInfo{type: nil, elixir_types_descr: nil}
      result = TypePresentation.render_hint(binding, var)
      assert result == :skip
    end

    test "render_hint with a native descr present falls back to structural shape" do
      # With native typing disabled, the native descr is not used;
      # the structural type still drives rendering.
      binding = %Binding{}

      # Build a dummy descr-like map; with typing disabled it won't be rendered
      # but the function must not raise.
      dummy_descr = %{bitmap: 8}

      var = %VarInfo{type: {:binary, nil}, elixir_types_descr: dummy_descr}
      result = TypePresentation.render_hint(binding, var)
      assert result == {:ok, "binary()"}
    end
  end
end
