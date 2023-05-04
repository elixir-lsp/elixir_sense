defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Require.RequireOnePart do
  require Enum
  @env __ENV__
  def env, do: @env
end
