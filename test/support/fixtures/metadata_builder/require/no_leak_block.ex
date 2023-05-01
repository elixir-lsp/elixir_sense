defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Require.RequireNoLeakBlock do
  def some do
    try do
      require ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
    rescue
      _ ->
        require ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
        :ok
    catch
      _, _ ->
        require ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
        :ok
    end

    receive do
      :x -> require ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
    after
      0 ->
        require ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
        :ok
    end

    if true do
      require ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
    else
      require ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
    end

    __ENV__
  end

  def env, do: some()
end
