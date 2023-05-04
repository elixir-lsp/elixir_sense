defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportNoLeakClause do
  def some do
    fn a ->
      import ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
    end

    case true do
      a ->
        import ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
    end

    cond do
      true ->
        import ElixirSenseExample.Fixtures.MetadataBuilder.Imported
    end

    __ENV__
  end

  def env, do: some()
end
