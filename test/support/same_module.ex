defmodule ElixirSenseExample.SameModule do
  def test_fun(), do: :ok

  defmacro some_test_macro() do
    quote do
      @attr "val"
    end
  end
end

defmodule ElixirSenseExample.SameModuleWithSecMacro do
  @spec some_test_macro() :: Macro.t()
  defmacro some_test_macro() do
    quote do
      @attr "val"
    end
  end
end
