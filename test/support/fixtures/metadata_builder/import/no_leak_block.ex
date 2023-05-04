defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Import.ImportNoLeakBlock do
  def some do
    try do
      import ElixirSenseExample.Fixtures.MetadataBuilder.Imported
    rescue
      _ ->
        import ElixirSenseExample.Fixtures.MetadataBuilder.Imported
        :ok
    catch
      _, _ ->
        import ElixirSenseExample.Fixtures.MetadataBuilder.Imported
        :ok
    end

    receive do
      :x -> import ElixirSenseExample.Fixtures.MetadataBuilder.Imported
    after
      0 ->
        import ElixirSenseExample.Fixtures.MetadataBuilder.Imported
        :ok
    end

    if true do
      import ElixirSenseExample.Fixtures.MetadataBuilder.Imported
    else
      import ElixirSenseExample.Fixtures.MetadataBuilder.Imported
    end

    __ENV__
  end

  def env, do: some()
end
