defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.Realias do
  alias ElixirSenseExample.MacroExpanderFixtures.Macros.Aliased
  alias Enum, as: Aliased
  @env __ENV__
  def env, do: @env
end
