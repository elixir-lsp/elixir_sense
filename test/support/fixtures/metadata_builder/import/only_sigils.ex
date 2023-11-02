if Version.match?(System.version(), ">= 1.13.0") do
  defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportOnlySigils do
    import ElixirSenseExample.Fixtures.MetadataBuilder.Imported, only: :sigils

    @env __ENV__
    def env, do: @env
  end
else
  defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportOnlySigils do
    @env __ENV__
    def env, do: @env
  end
end
