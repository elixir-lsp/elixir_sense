defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasModuleSpecialSubmodule do
  alias __MODULE__.Submodule
  @env __ENV__
  def env, do: @env
end
