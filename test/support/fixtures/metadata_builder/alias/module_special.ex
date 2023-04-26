defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasModuleSpecial do
  alias __MODULE__
  @env __ENV__
  def env, do: @env
end
