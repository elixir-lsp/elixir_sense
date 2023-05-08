defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportInSubmodule do
  defmodule Submodule do
    @env __ENV__
    def env, do: @env
  end

  def env, do: Submodule.env()
end
