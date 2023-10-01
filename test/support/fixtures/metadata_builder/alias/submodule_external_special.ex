defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasSubmoduleExternalSpecial do
  alias Some.Sub

  defmodule __MODULE__.SubmoduleExternal do
  end

  @env __ENV__
  def env, do: @env
end
