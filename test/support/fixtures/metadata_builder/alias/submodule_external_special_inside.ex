defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasSubmoduleExternalSpecialInside do
  alias Some.Sub

  defmodule __MODULE__.SubmoduleExternal do
    @env __ENV__
    def env, do: @env
  end

  def env, do: __MODULE__.SubmoduleExternal.env()
end
