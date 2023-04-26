defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasOfAlias do
  alias ElixirSenseExample.MacroExpanderFixtures.Macros.Aliased
  alias Aliased, as: Some
  alias Aliased.Child
  @env __ENV__
  def env, do: @env
end
