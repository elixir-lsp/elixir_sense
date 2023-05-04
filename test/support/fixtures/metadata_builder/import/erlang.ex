defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportErlang do
  import :erlang, except: [alias: 1]
  @env __ENV__
  def env, do: @env
end
