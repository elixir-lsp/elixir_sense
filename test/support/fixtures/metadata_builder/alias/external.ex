defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasExternal do
  alias ElixirSenseExample.MacroExpanderFixtures.Macros.Aliased
  alias Elixir.Aliased.Child
  @env __ENV__
  def env, do: @env
end
