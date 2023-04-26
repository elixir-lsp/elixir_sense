defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasWithAs do
  alias ElixirSenseExample.MacroExpanderFixtures.Macros.Aliased, as: Some
  @env __ENV__
  def env, do: @env
end
