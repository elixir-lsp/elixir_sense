defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.Import12 do
  import ElixirSenseExample.Fixtures.MetadataBuilder.{
    Imported,
    ImportedSibling,
    Imported.Child
  }

  @env __ENV__
  def env, do: @env
end
