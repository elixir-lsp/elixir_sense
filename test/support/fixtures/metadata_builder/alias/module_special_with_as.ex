defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasModuleSpecialWithAs do
  alias __MODULE__.Submodule, as: Some
  alias __MODULE__, as: Other
  @env __ENV__
  def env, do: @env
end
