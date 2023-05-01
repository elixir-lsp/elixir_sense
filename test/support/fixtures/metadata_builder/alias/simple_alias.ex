defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.SimpleAlias do
  alias ElixirSenseExample.MacroExpanderFixtures.Macros.Aliased
  @env __ENV__
  def env, do: @env
end
