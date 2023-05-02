defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Require.RequireInheritFunction do
  require ElixirSenseExample.Fixtures.MetadataBuilder.Aliased

  def some do
    __ENV__
  end

  def env, do: some()
end
