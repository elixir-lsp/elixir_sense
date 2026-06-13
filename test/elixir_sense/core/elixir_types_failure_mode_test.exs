defmodule ElixirSense.Core.ElixirTypesFailureModeTest do
  use ExUnit.Case, async: false

  alias ElixirSense.Core.ElixirTypes
  alias ElixirSense.Core.ExCkReader
  alias ElixirSense.Core.TypePresentation
  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.State.VarInfo

  describe "private API drift canaries" do
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

    test "descr_to_string with a list (not a descr) returns :error, no raise" do
      assert ElixirTypes.descr_to_string([1, 2, 3]) == :error
    end

    test "descr_to_string with a tuple (not a descr) returns :error, no raise" do
      assert ElixirTypes.descr_to_string({:ok, :value}) == :error
    end

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

    test "of_expr with an improper list (not a valid AST) returns :error, no raise" do
      result = ElixirTypes.of_expr([1 | :not_a_list])
      assert result == :error
    end

    test "of_expr with an AST carrying non-list metadata returns :error or {:ok,_}, no raise" do
      weird_ast = {:some_call, :not_a_list_meta, []}
      result = ElixirTypes.of_expr(weird_ast)
      assert result == :error or match?({:ok, _}, result)
    end

    test "of_expr with a huge nested tuple (> 3 elements) returns :error, no raise" do
      result = ElixirTypes.of_expr({:a, :b, :c, :d, :e})
      assert result == :error
    end

    test "of_match with a non-pattern call AST returns :error, no raise" do
      pattern = {:my_fun, [], [:arg]}
      match = {:=, [], [pattern, :something]}
      result = ElixirTypes.of_match(pattern, nil, match)
      assert result == :error
    end

    test "of_match with a pattern containing a typespec operator returns :error, no raise" do
      pattern = {:"__::__", [], [:type_a, :type_b]}
      match = {:=, [], [pattern, :something]}
      result = ElixirTypes.of_match(pattern, nil, match)
      assert result == :error
    end

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
      clauses = [%{meta: [], args: [], guards: nil, body: :not_valid_ast_at_all}]

      result =
        ElixirTypes.infer_local_signature(SomeMod, {:f, 0}, clauses, "nofile")

      assert result == :error or match?({:infer, _, _}, result)
    end

    test "to_shape with nil returns nil, no raise" do
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

    test "apply_signature with a :strong sig and garbage arg descr degrades, no raise" do
      if ElixirTypes.available?() do
        sig = {:strong, nil, [{[Module.Types.Descr.integer()], Module.Types.Descr.atom([:ok])}]}
        garbage_descr = %{dynamic: {0, 0, 0, 0, 0, 0, :garbage}, atom: :surprise}
        result = ElixirTypes.apply_signature(sig, [garbage_descr])
        assert result == :error or match?({:ok, _}, result)
      end
    end
  end

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

  describe "lookup_signature — module without ExCk chunk" do
    test "Erlang :lists module has no ExCk chunk — returns :error, no raise" do
      result = ExCkReader.lookup_signature(:lists, :map, 2)
      assert result == :error
    end

    test "Erlang :erlang module has no ExCk chunk — returns :error, no raise" do
      result = ExCkReader.lookup_signature(:erlang, :+, 2)
      assert result == :error
    end

    test "read_chunk with a beam binary containing no ExCk chunk returns error" do
      attr_payload = :erlang.term_to_binary(vsn: [1])
      attr_size = byte_size(attr_payload)
      attr_pad_count = rem(4 - rem(attr_size, 4), 4)
      attr_pad = :binary.copy(<<0>>, attr_pad_count)

      chunks_bin =
        <<"Attr", attr_size::unsigned-big-32, attr_payload::binary, attr_pad::binary>>

      beam_bin = <<"FOR1", byte_size(chunks_bin)::unsigned-big-32, "BEAM", chunks_bin::binary>>

      result = ExCkReader.read_chunk(:fake_no_exck, chunk: beam_bin)
      assert match?({:error, _}, result), "Expected {:error, _}, got #{inspect(result)}"
    end
  end

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
      result = ElixirTypes.of_expr({:a, :b, :c, :d})
      assert result == :error
    end
  end

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
      binding = %Binding{}

      dummy_descr = %{bitmap: 8}

      var = %VarInfo{type: {:binary, nil}, elixir_types_descr: dummy_descr}
      result = TypePresentation.render_hint(binding, var)
      assert result == {:ok, "binary()"}
    end
  end
end
