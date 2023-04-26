defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasSubmodule do
  defmodule Submodule do
  end

  @env __ENV__
  def env, do: @env
end
