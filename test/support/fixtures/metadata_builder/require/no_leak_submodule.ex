defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Require.RequireNoLeakSubmodule do
  defmodule Submodule do
    require ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
  end

  @env __ENV__
  def env, do: @env
end
