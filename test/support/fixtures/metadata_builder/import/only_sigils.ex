defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportOnlySigils do
  import ElixirSenseExample.Fixtures.MetadataBuilder.Imported, only: :sigils
  @env __ENV__
  def env, do: @env
end
