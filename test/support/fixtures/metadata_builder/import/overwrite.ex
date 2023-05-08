defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportOverwrite do
  import ElixirSenseExample.Fixtures.MetadataBuilder.Imported, only: :macros
  import ElixirSenseExample.Fixtures.MetadataBuilder.Imported, only: [public_fun: 0]
  @env __ENV__
  def env, do: @env
end
