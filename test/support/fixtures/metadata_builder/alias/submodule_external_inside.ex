defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasSubmoduleExternalInside do
  alias Some.Sub

  defmodule Elixir.SubmoduleExternalInside do
    @env __ENV__
    def env, do: @env
  end

  @env __ENV__
  def env, do: Elixir.SubmoduleExternalInside.env()
end
