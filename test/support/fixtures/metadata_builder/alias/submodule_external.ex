defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasSubmoduleExternal do
  defmodule Elixir.SubmoduleExternal do
  end

  @env __ENV__
  def env, do: @env
end
