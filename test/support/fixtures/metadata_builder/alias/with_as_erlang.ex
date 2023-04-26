defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Alias.AliasWithAsErlang do
  alias :lists, as: Some
  @env __ENV__
  def env, do: @env
end
