defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Require.RequireOfAlias do
  alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
  require Aliased, as: Some
  require Aliased.Child
  @env __ENV__
  def env, do: @env
end
