defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Require.RequireWithAsOnePart do
  require Enum, as: Some
  @env __ENV__
  def env, do: @env
end
