defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.NoUnaliasNested do
  alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
  alias Elixir.Aliased.Some
  @env __ENV__
  def env, do: @env
end
