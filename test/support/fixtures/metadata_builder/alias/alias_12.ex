defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.Alias12 do
  alias ElixirSenseExample.Fixtures.MetadataBuilder.{
    Aliased,
    AliasedSibling,
    Aliased.Child
  }

  @env __ENV__
  def env, do: @env
end
