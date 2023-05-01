defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Require.SimpleRequire do
  require ElixirSenseExample.ExampleBehaviour
  @env __ENV__
  def env, do: @env
end
