defmodule ElixirSenseExample.FunctionsWithDefaultArgs do
  def my_func, do: "not this one"
  def my_func(a, b \\ "")
  def my_func(1, b), do: "1" <> b
  def my_func(2, b), do: "2" <> b
end
