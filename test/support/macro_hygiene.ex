defmodule ElixirSenseExample.Math do
  defmacro squared(x) do
    quote do
      x = unquote(x)
      x * x
    end
  end
end
