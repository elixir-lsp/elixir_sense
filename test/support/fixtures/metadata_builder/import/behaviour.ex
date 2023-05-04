defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportBehaviour do
  import :gen_server
  @env __ENV__
  def env, do: @env
end
