defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.Unalias do
  alias ElixirSenseExample.MacroExpanderFixtures.Macros.Aliased
  alias Elixir.Aliased
  @env __ENV__
  def env, do: @env
end
