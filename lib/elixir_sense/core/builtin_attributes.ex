defmodule ElixirSense.Core.BuiltinAttributes do
  @moduledoc false

  @list Map.keys(Module.reserved_attributes())

  def all, do: @list
end
