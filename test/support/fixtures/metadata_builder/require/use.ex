defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Require.Use do
  use ElixirSenseExample.ExampleBehaviourWithException
  @env __ENV__
  def env, do: @env
end
