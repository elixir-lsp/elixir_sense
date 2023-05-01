defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasInheritSubmodule do
  alias ElixirSenseExample.MacroExpanderFixtures.Macros.Aliased

  defmodule Submodule do
    @env __ENV__
    def env, do: @env
  end

  def env, do: Submodule.env()
end
