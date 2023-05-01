defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasNoLeakSubmodule do
  defmodule Submodule do
    alias ElixirSenseExample.MacroExpanderFixtures.Macros.Aliased
  end

  @env __ENV__
  def env, do: @env
end
