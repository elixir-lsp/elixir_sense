defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportOnlyMacros do
  import ElixirSenseExample.Fixtures.MetadataBuilder.Imported, only: :macros
  @env __ENV__
  def env, do: @env
end
