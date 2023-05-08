defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Require.RequireExternal do
  alias ElixirSenseExample.Fixtures.MetadataBuilder.Logger
  require Elixir.Logger
  @env __ENV__
  def env, do: @env
end
