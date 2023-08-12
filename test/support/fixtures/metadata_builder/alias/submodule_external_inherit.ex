defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasSubmoduleExternalInherit do
  alias Abc, as: Cde

  defmodule Elixir.SubmoduleExternalInherit do
    @env __ENV__
    def env, do: @env
  end

  @env __ENV__
  def env, do: Elixir.SubmoduleExternalInherit.env()
end
