defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportOnlyFunctions do
  import ElixirSenseExample.Fixtures.MetadataBuilder.Imported, only: :functions
  @env __ENV__
  def env, do: @env
end
