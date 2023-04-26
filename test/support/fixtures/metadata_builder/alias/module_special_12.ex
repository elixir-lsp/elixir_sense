defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasModuleSpecial12 do
  alias __MODULE__.{A, B.C}
  @env __ENV__
  def env, do: @env
end
