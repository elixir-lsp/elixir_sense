defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasExternal do
  alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
  alias Elixir.Aliased.Child
  @env __ENV__
  def env, do: @env
end
