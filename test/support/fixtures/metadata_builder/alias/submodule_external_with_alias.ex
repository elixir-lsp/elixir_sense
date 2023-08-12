defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasSubmoduleExternalWithAlias do
  alias Abc, as: SubmoduleExternalWithAlias

  defmodule Elixir.SubmoduleExternalWithAlias do
    @env __ENV__
    def env, do: @env
  end

  def env, do: Elixir.SubmoduleExternalWithAlias.env()
end
