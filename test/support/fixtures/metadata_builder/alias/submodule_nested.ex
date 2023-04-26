defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasSubmoduleNested do
  defmodule Submodule.Child do
  end

  @env __ENV__
  def env, do: @env
end
