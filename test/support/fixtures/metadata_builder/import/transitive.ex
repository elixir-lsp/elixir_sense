defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.Transitive do
  import ElixirSenseExample.Fixtures.MetadataBuilder.Imported.Transitive
  @env __ENV__
  def env, do: @env
end
