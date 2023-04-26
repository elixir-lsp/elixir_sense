defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasInheritFunction do
  alias ElixirSenseExample.MacroExpanderFixtures.Macros.Aliased

  def some do
    __ENV__
  end

  def env, do: some()
end
