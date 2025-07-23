defmodule ElixirSense.Core.Compiler.FakeLocal do
  for arity <- 0..255 do
    def ok_fun(unquote(arity)),
      do: fn unquote_splicing(Macro.generate_arguments(arity, __MODULE__)) -> :ok end
  end
end
