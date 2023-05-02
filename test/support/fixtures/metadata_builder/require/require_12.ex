defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Require.Require12 do
  require ElixirSenseExample.Fixtures.MetadataBuilder.{
    Aliased,
    AliasedSibling,
    Aliased.Child
  }

  @env __ENV__
  def env, do: @env
end
