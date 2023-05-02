defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Require.RequireWithAs do
  require ElixirSenseExample.ExampleBehaviour, as: MyModule
  @env __ENV__
  def env, do: @env
end
