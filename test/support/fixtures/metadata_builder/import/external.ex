defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportExternal do
  alias ElixirSenseExample.Fixtures.MetadataBuilder.Logger
  import Elixir.Logger
  @env __ENV__
  def env, do: @env
end
