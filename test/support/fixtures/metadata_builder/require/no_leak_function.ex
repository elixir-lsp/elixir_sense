defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Require.RequireNoLeakFunction do
  def some do
    require ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
    :ok
  end

  @env __ENV__
  def env, do: @env
end
