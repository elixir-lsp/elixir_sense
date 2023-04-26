defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.RequireWithAs do
  require ElixirSenseExample.ExampleBehaviour, as: Some
  @env __ENV__
  def env, do: @env
end
