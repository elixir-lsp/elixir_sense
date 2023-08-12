defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Require.RequireWithWarn do
  require ElixirSenseExample.ExampleBehaviour, warn: false
  @env __ENV__
  def env, do: @env
end
