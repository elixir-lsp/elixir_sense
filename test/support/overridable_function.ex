defmodule ElixirSenseExample.OverridableFunctions do
  defmacro __using__(_opts) do
    quote do
      @spec test(number, number) :: number
      def test(x, y) do
        x + y
      end

      defmacro required(var), do: Macro.expand(var, __CALLER__)

      defoverridable test: 2, required: 1
    end
  end
end

defmodule ElixirSenseExample.OverridableBehaviour do
  @callback foo :: any
end

defmodule ElixirSenseExample.OverridableImplementation do
  defmacro __using__(_opts) do
    quote do
      @behaviour ElixirSenseExample.OverridableBehaviour

      def foo do
        "Override me"
      end

      defoverridable ElixirSenseExample.OverridableBehaviour
    end
  end
end
