defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Require.RequireNoLeakClause do
  def some do
    fn a ->
      require ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
    end

    case true do
      a ->
        require ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
    end

    cond do
      true ->
        require ElixirSenseExample.Fixtures.MetadataBuilder.Aliased
    end

    __ENV__
  end

  def env, do: some()
end
