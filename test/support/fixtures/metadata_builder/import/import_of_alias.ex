defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportOfAlias do
  alias ElixirSenseExample.Fixtures.MetadataBuilder.Imported
  import Imported
  @env __ENV__
  def env, do: @env
end
