defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportWithWarn do
  import ElixirSenseExample.Fixtures.MetadataBuilder.Imported, warn: false
  @env __ENV__
  def env, do: @env
end
