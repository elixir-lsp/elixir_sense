defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasNoLeakClause do
  def some do
    fn a ->
      alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
    end

    case true do
      a ->
        alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
    end

    cond do
      true ->
        alias ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
    end

    __ENV__
  end

  def env, do: some()
end
