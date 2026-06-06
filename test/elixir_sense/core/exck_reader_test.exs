defmodule ElixirSense.Core.ExCkReaderTest do
  use ExUnit.Case, async: true

  # ExCk remote-signature reading is part of the native typing path; 1.17's
  # stdlib chunks aren't usable by the adaptor (native backend off there).
  @moduletag :requires_native_types

  alias ElixirSense.Core.ExCkReader

  @table ElixirSense.Core.ExCkReader

  setup do
    if :ets.info(@table) != :undefined do
      :ets.delete_all_objects(@table)
    end

    on_exit(fn ->
      Application.delete_env(:elixir_sense, :exck_cache_ttl)

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
      assert {:error, :invalid_beam} = ExCkReader.read_chunk(:lists)
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
end
