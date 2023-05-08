defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Require.Import do
  import ElixirSenseExample.Fixtures.MetadataBuilder.Imported, only: :functions
  @env __ENV__
  def env, do: @env
end
