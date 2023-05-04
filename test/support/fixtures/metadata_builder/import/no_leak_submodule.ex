defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportNoLeakSubmodule do
  defmodule Submodule do
    import ElixirSenseExample.Fixtures.MetadataBuilder.Imported
  end

  @env __ENV__
  def env, do: @env
end
