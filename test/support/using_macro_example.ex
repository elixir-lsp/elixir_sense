defmodule ElixirSenseExample.UsingMacroExample do
  defmacro __using__(_opts) do
    quote do
      def using_macro_function(), do: :ok
    end
  end

  # Unrelated regular function sharing a name with a purely-local function in
  # a consuming module. It lives outside `__using__` and must never be the
  # target of go-to-definition for that local function.
  def using_macro_function_unrelated(), do: :unrelated
end

defmodule ElixirSenseExample.ModuleUsingMacroExample do
  use ElixirSenseExample.UsingMacroExample
end

# A plain module that does not `use` anything, with an ordinary function.
# Used to assert go-to-definition of a regular remote function is unaffected.
defmodule ElixirSenseExample.PlainModuleExample do
  def plain_function(), do: :ok
end

# Module using Kernel.use qualified call
defmodule ElixirSenseExample.ModuleUsingKernelUse do
  Kernel.use(ElixirSenseExample.UsingMacroExample)
end

# Module using aliased module
defmodule ElixirSenseExample.ModuleUsingAlias do
  alias ElixirSenseExample.UsingMacroExample, as: MyMacro

  use MyMacro
end

# Module with a local function that shares a name with the unrelated function
# in the used module's file. Go-to-definition for the local one must resolve to
# the local def, not the unrelated def in using_macro_example.ex.
defmodule ElixirSenseExample.ModuleWithLocalUse do
  use ElixirSenseExample.UsingMacroExample

  def using_macro_function_unrelated(), do: :local
end
