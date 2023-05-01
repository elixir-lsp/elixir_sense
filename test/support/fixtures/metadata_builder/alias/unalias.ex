defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.Unalias do
  alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
  alias Elixir.Aliased
  @env __ENV__
  def env, do: @env
end
