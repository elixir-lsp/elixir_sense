defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.RealiasInScope do
  alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased

  defmodule Child do
    alias Enum, as: Aliased
  end

  @env __ENV__
  def env, do: @env
end
