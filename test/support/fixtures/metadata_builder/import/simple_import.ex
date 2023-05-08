defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.SimpleImport do
  import ElixirSenseExample.Fixtures.MetadataBuilder.Imported
  @env __ENV__
  def env, do: @env
end
