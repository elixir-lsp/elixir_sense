defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasWithWarn do
  alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased, warn: false
  @env __ENV__
  def env, do: @env
end
