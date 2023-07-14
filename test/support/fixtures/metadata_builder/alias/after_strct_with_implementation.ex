defprotocol ElixirSenseExample.ExampleProtocol1 do
  @spec some(t) :: any
  def some(t)
end

defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.StructWithImplementation do
  defstruct name_version: "", github_relative_path: ""

  defimpl ElixirSenseExample.ExampleProtocol1 do
    def some(t), do: t
  end
end

defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AfterStructWithImplementation do
  @env __ENV__
  def env, do: @env
end
