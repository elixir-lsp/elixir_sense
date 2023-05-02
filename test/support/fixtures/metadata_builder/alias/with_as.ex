defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasWithAs do
  alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased, as: Some
  @env __ENV__
  def env, do: @env
end
