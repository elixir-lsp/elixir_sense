defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.RequireWithWarnAs do
  require ElixirSenseExample.ExampleBehaviour, warn: false, as: Some
  @env __ENV__
  def env, do: @env
end
