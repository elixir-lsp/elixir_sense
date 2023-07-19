defmodule ElixirSenseExample.TypesWithMultipleArity do
  @type my_type :: integer
  @type my_type(a) :: {integer, a}
  @type my_type(a, b) :: {integer, a, b}
end
