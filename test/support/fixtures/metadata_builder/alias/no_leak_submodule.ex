defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasNoLeakSubmodule do
  defmodule Submodule do
    alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
  end

  @env __ENV__
  def env, do: @env
end
