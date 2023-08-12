defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasSubmoduleExternalWithAliasSpecial do
  defmodule Elixir.SubmoduleExternalWithAliasSpecial do
    alias Abc, as: SubmoduleExternalWithAlias
    alias __MODULE__
    @env __ENV__
    def env, do: @env
  end

  def env, do: Elixir.SubmoduleExternalWithAliasSpecial.env()
end
