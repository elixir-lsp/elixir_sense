defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportOnlySigils do
  if Version.match?(System.version(), ">= 1.13.0") do
    import ElixirSenseExample.Fixtures.MetadataBuilder.Imported, only: :sigils
  end

  @env __ENV__
  def env, do: @env
end
