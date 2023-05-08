defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportOverwriteNoLeak do
  import ElixirSenseExample.Fixtures.MetadataBuilder.Imported, only: :macros

  defmodule Submodule do
    import ElixirSenseExample.Fixtures.MetadataBuilder.Imported, only: [public_fun: 0]
  end

  @env __ENV__
  def env, do: @env
end
