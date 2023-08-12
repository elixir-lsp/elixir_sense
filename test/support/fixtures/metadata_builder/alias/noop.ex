defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.Noop do
  alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
  alias Aliased
  @env __ENV__
  def env, do: @env
end
