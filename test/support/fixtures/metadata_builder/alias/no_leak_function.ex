defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasNoLeakFunction do
  def some do
    alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
    :ok
  end

  @env __ENV__
  def env, do: @env
end
