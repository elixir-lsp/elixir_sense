defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportOnlyUnderscored do
  import ElixirSenseExample.Fixtures.MetadataBuilder.Imported, only: [_underscored_fun: 0]
  @env __ENV__
  def env, do: @env
end
