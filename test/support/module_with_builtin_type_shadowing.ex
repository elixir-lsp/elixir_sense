defmodule ElixirSenseExample.ModuleWithBuiltinTypeShadowing do
  def plain_fun do
    B.Callee.fun()
  end
end
