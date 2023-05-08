defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportElixirBehaviour do
  import GenServer
  @env __ENV__
  def env, do: @env
end
