defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportNoLeakFunction do
  def some do
    import ElixirSenseExample.Fixtures.MetadataBuilder.Imported
    :ok
  end

  @env __ENV__
  def env, do: @env
end
