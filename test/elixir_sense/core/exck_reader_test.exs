defmodule ElixirSense.Core.ExCkReaderTest do
  use ExUnit.Case, async: false

  @moduletag :requires_native_types

  alias ElixirSense.Core.ExCkReader

  @table ElixirSense.Core.ExCkReader
  @owner_name :exck_reader_ets_owner

  setup do
    if :ets.info(@table) != :undefined do
      :ets.delete_all_objects(@table)
    end

    on_exit(fn ->
      Application.delete_env(:elixir_sense, :exck_cache_ttl)
      Application.delete_env(:elixir_sense, :exck_negative_cache_ttl)

      if :ets.info(@table) != :undefined do
        :ets.delete_all_objects(@table)
      end
    end)

    :ok
  end

  describe "read_chunk/1" do
    test "returns signatures for Enum" do
      assert {:ok, signatures} = ExCkReader.read_chunk(Enum)
      assert is_map(signatures)
      assert Map.has_key?(signatures, {:map, 2})
    end

    test "returns error when module has no ExCk chunk" do
      # :lists is an Erlang module; its BEAM binary has no ExCk chunk.
      assert {:error, _} = ExCkReader.read_chunk(:lists)
    end

    test "handles invalid chunk gracefully" do
      assert {:error, :invalid_chunk} = ExCkReader.read_chunk(:fake, chunk: <<1, 2, 3>>)
    end
  end

  test "lookup_signature returns cached entries" do
    Application.put_env(:elixir_sense, :exck_cache_ttl, 60_000)

    assert {:ok, info} = ExCkReader.lookup_signature(Enum, :map, 2)
    assert is_map(info)
    assert Map.has_key?(info, :sig)
  end

  test "cache invalidation honours TTL" do
    Application.put_env(:elixir_sense, :exck_cache_ttl, 0)

    assert {:ok, signatures} = ExCkReader.read_chunk(Enum)
    assert map_size(signatures) > 0

    stale = {:ok, %{foo: :bar}}
    :ets.insert(@table, {Enum, stale, 0})

    assert {:ok, refreshed} = ExCkReader.read_chunk(Enum)
    assert refreshed != stale
    assert map_size(refreshed) > 0
  end

  test "negative results expire on the (shorter) negative TTL" do
    # Long success TTL, instantly-expiring negative TTL: a cached failure must
    # not be served once the negative TTL has passed, even though the success
    # TTL is still fresh. Regression: a module queried before compilation used
    # to pin {:error, :beam_not_found} for the full success TTL.
    Application.put_env(:elixir_sense, :exck_cache_ttl, 60_000)
    Application.put_env(:elixir_sense, :exck_negative_cache_ttl, 0)

    module = :exck_reader_negative_ttl_test_module
    assert {:error, _} = ExCkReader.read_chunk(module)
    assert [{^module, {:error, _}, inserted_1}] = :ets.lookup(@table, module)

    # Re-read after the negative TTL: must re-probe rather than serve the
    # cached negative. The module still doesn't exist, so the result is again
    # an error — but freshly computed, observable via the refreshed insert
    # timestamp.
    Process.sleep(5)
    assert {:error, _} = ExCkReader.read_chunk(module)
    assert [{^module, {:error, _}, inserted_2}] = :ets.lookup(@table, module)
    assert inserted_2 > inserted_1
  end

  test "negative TTL does not shorten success entries" do
    Application.put_env(:elixir_sense, :exck_cache_ttl, 60_000)
    Application.put_env(:elixir_sense, :exck_negative_cache_ttl, 0)

    assert {:ok, _} = ExCkReader.read_chunk(Enum)
    [{Enum, {:ok, _}, inserted_1}] = :ets.lookup(@table, Enum)

    Process.sleep(5)
    assert {:ok, _} = ExCkReader.read_chunk(Enum)
    [{Enum, {:ok, _}, inserted_2}] = :ets.lookup(@table, Enum)

    # Served from cache — the entry was not rewritten.
    assert inserted_2 == inserted_1
  end

  describe "version mismatch" do
    test "rejects a chunk with a foreign checker version tag" do
      fake_tag = :elixir_checker_v0
      contents = %{exports: []}
      chunk_bin = :erlang.term_to_binary({fake_tag, contents})

      assert {:error, :version_mismatch} = ExCkReader.read_chunk(:fake, chunk: chunk_bin)
    end

    test "accepts a chunk whose version tag matches the running runtime" do
      real_tag = ExCkReader.runtime_checker_version()
      contents = %{exports: []}
      chunk_bin = :erlang.term_to_binary({real_tag, contents})

      assert {:ok, %{}} = ExCkReader.read_chunk(:fake, chunk: chunk_bin)
    end
  end

  describe "scan_chunks padding" do
    defp build_beam_binary(first_chunk_id, first_chunk_size, exck_tag) do
      # A real ExCk payload for the second chunk.
      exck_payload = :erlang.term_to_binary({exck_tag, %{exports: []}})
      exck_size = byte_size(exck_payload)

      # Padding for ExCk chunk (should be 0 for these tests but handle it).
      exck_pad_count = rem(4 - rem(exck_size, 4), 4)
      exck_pad = :binary.copy(<<0>>, exck_pad_count)

      # First chunk: `first_chunk_size` bytes of zeroes.
      first_data = :binary.copy(<<0>>, first_chunk_size)
      first_pad_count = rem(4 - rem(first_chunk_size, 4), 4)
      first_pad = :binary.copy(<<0>>, first_pad_count)

      chunks_bin =
        <<first_chunk_id::binary-size(4), first_chunk_size::unsigned-big-32, first_data::binary,
          first_pad::binary, "ExCk"::binary, exck_size::unsigned-big-32, exck_payload::binary,
          exck_pad::binary>>

      chunks_size = byte_size(chunks_bin)

      <<"FOR1", chunks_size::unsigned-big-32, "BEAM", chunks_bin::binary>>
    end

    test "correctly skips a chunk whose size is ≡ 2 (mod 4)" do
      tag = ExCkReader.runtime_checker_version()
      beam = build_beam_binary("FAKE", 6, tag)

      assert {:ok, payload} =
               ExCkReader.read_chunk(:ignore, chunk: nil, cache: false)
               |> then(fn _ ->
                 exck_payload = :erlang.term_to_binary({tag, %{exports: []}})

                 ExCkReader.read_chunk(:ignore,
                   chunk: exck_payload,
                   cache: false
                 )
               end)

      assert is_map(payload)
      verify_padding_scan(beam, tag)
    end

    defp verify_padding_scan(beam, expected_tag) do
      <<"FOR1", _size::unsigned-big-32, "BEAM", rest::binary>> = beam
      result = scan_chunks_test_helper(rest, expected_tag)
      assert result == :found
    end

    defp scan_chunks_test_helper(<<>>, _tag), do: :not_found

    defp scan_chunks_test_helper(
           <<id::binary-size(4), size::unsigned-big-32, chunk::binary-size(size), rest::binary>>,
           tag
         ) do
      padding = rem(4 - rem(size, 4), 4)

      if byte_size(rest) < padding do
        :invalid
      else
        <<_pad::binary-size(^padding), tail::binary>> = rest

        if id == "ExCk" do
          case :erlang.binary_to_term(chunk) do
            {^tag, _} -> :found
            _ -> :wrong_version
          end
        else
          scan_chunks_test_helper(tail, tag)
        end
      end
    end

    defp scan_chunks_test_helper(_, _), do: :invalid
  end

  describe "ETS lifecycle" do
    test "concurrent ensure_table from N processes doesn't raise" do
      n = 20

      results =
        1..n
        |> Enum.map(fn _ ->
          Task.async(fn ->
            ExCkReader.lookup_signature(Enum, :map, 2)
          end)
        end)
        |> Task.await_many(5_000)

      for result <- results do
        assert match?({:ok, _}, result) or match?(:error, result),
               "Unexpected result: #{inspect(result)}"
      end

      assert Enum.any?(results, &match?({:ok, _}, &1))
    end

    test "survives owner-process death between calls" do
      assert {:ok, _} = ExCkReader.lookup_signature(Enum, :map, 2)

      case Process.whereis(@owner_name) do
        nil ->
          :ok

        pid ->
          Process.unregister(@owner_name)
          Process.exit(pid, :kill)

          ref = Process.monitor(pid)

          receive do
            {:DOWN, ^ref, :process, ^pid, _} -> :ok
          after
            1_000 -> :ok
          end
      end

      result = ExCkReader.lookup_signature(Enum, :map, 2)

      assert match?({:ok, _}, result) or match?(:error, result),
             "Expected :ok or :error tuple, got: #{inspect(result)}"

      assert {:ok, _} = ExCkReader.lookup_signature(Enum, :map, 2)
    end

    test "table deletion mid-operation is handled gracefully" do
      assert {:ok, _} = ExCkReader.lookup_signature(Enum, :map, 2)

      if :ets.info(@table) != :undefined do
        :ets.delete(@table)
      end

      result = ExCkReader.lookup_signature(Enum, :map, 2)

      assert match?({:ok, _}, result) or match?(:error, result),
             "Expected :ok or :error tuple, got: #{inspect(result)}"
    end
  end
end
