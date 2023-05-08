defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportExceptModify do
  import ElixirSenseExample.Fixtures.MetadataBuilder.Imported
  import ElixirSenseExample.Fixtures.MetadataBuilder.Imported, except: [public_fun: 0]
  @env __ENV__
  def env, do: @env
end
