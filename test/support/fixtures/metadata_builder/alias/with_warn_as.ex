defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasWithWarnAs do
  alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased, warn: false, as: Some
  @env __ENV__
  def env, do: @env
end
