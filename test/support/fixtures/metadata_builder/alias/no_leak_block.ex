defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasNoLeakBlock do
  def some do
    try do
      alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
    rescue
      _ ->
        alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
        :ok
    catch
      _, _ ->
        alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
        :ok
    end

    receive do
      :x -> alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
    after
      0 ->
        alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
        :ok
    end

    if true do
      alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
    else
      alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
    end

    __ENV__
  end

  def env, do: some()
end
