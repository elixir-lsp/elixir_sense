defmodule ElixirSenseExample.UsingMacroExample do
  defmacro __using__(_opts) do
    quote do
      def using_macro_function(), do: :ok
    end
  end
end

defmodule ElixirSenseExample.ModuleUsingMacroExample do
  use ElixirSenseExample.UsingMacroExample
end
