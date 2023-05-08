defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportInheritFunction do
  import ElixirSenseExample.Fixtures.MetadataBuilder.Imported

  def some do
    __ENV__
  end

  def env, do: some()
end
