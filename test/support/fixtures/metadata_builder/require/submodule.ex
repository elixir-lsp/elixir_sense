defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Require.RequireSubmodule do
  defmodule Submodule do
  end

  @env __ENV__
  def env, do: @env
end
