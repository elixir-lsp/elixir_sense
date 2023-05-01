defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasWithAsOnePart do
  alias Enum, as: Some
  @env __ENV__
  def env, do: @env
end
