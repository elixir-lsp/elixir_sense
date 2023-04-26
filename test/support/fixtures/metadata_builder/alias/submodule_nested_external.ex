defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasSubmoduleNestedExternal do
  defmodule Elixir.SubmoduleExternal.ChildExternal do
  end

  @env __ENV__
  def env, do: @env
end
