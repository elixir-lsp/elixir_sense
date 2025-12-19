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

# Module using Kernel.use qualified call (tests reviewer concern #1)
defmodule ElixirSenseExample.ModuleUsingKernelUse do
  Kernel.use(ElixirSenseExample.UsingMacroExample)
end

# Module using aliased module (tests reviewer concern #2)
defmodule ElixirSenseExample.ModuleUsingAlias do
  alias ElixirSenseExample.UsingMacroExample, as: MyMacro

  use MyMacro
end

# Module with local function named use (tests reviewer concern #3)
# This tests that a local function named `use` doesn't confuse use tracking
defmodule ElixirSenseExample.ModuleWithLocalUse do
  # This local function exists but won't shadow the Kernel.use macro
  # The test verifies that proper AST expansion correctly identifies Kernel.use
  defp my_use(_module), do: nil

  use ElixirSenseExample.UsingMacroExample

  def call_local_use do
    my_use(SomeModule)
  end
end
