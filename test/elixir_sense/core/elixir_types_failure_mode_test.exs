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
