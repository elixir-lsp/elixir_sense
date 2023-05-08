defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportInheritSubmodule do
  import ElixirSenseExample.Fixtures.MetadataBuilder.Imported

  defmodule Submodule do
    @env __ENV__
    def env, do: @env
  end

  def env, do: Submodule.env()
end
