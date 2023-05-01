defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasNoLeakFunction do
  def some do
    alias ElixirSenseExample.MacroExpanderFixtures.Macros.Aliased
    :ok
  end

  @env __ENV__
  def env, do: @env
end
