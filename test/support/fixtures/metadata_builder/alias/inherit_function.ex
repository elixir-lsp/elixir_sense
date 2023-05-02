defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasInheritFunction do
  alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased

  def some do
    __ENV__
  end

  def env, do: some()
end
