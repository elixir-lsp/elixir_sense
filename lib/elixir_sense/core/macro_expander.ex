# TODO replace this module
defmodule ElixirSense.Core.MacroExpander do
  @moduledoc false

  def add_default_meta(expr) do
    Macro.update_meta(expr, fn keyword ->
      Keyword.merge(keyword, context: Elixir, import: Kernel)
    end)
  end
end
