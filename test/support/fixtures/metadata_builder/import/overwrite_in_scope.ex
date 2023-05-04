defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportOverwriteInScope do
  import ElixirSenseExample.Fixtures.MetadataBuilder.Imported, only: :macros

  defmodule Submodule do
    import ElixirSenseExample.Fixtures.MetadataBuilder.Imported, only: [public_fun: 0]
    @env __ENV__
    def env, do: @env
  end

  def env, do: Submodule.env()
end
