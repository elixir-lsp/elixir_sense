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

# Injects definitions via `defdelegate` and `defguard` (not just `def`) so the
# go-to-definition search must recognise those forms too.
defmodule ElixirSenseExample.UsingMacroOtherForms do
  def delegated_target(), do: :ok

  defmacro __using__(_opts) do
    quote do
      defdelegate delegated_function(),
        to: ElixirSenseExample.UsingMacroOtherForms,
        as: :delegated_target

      defguard is_even(value) when is_integer(value) and rem(value, 2) == 0
    end
  end
end

defmodule ElixirSenseExample.ModuleUsingOtherForms do
  use ElixirSenseExample.UsingMacroOtherForms
end

# Transitive `use` chain: the injected function is defined several `use` hops
# away. `UsingMacroOuter.__using__` itself does `use UsingMacroInner`, which is
# where `nested_using_function` actually lives (mirrors MyApp.Repo ->
# AshPostgres.Repo -> Ecto.Repo). Go-to-definition must follow the chain.
defmodule ElixirSenseExample.UsingMacroInner do
  defmacro __using__(_opts) do
    quote do
      def nested_using_function(), do: :nested
    end
  end
end

defmodule ElixirSenseExample.UsingMacroOuter do
  defmacro __using__(_opts) do
    quote do
      use ElixirSenseExample.UsingMacroInner
    end
  end
end

defmodule ElixirSenseExample.ModuleUsingNested do
  use ElixirSenseExample.UsingMacroOuter
end

# Multi-line `use` with options. The recorded definition line is the `use`
# keyword line (`use ...,`), which is not valid Elixir on its own, so the
# use-site detection must not rely on parsing that line in isolation.
defmodule ElixirSenseExample.UsingMacroWithOpts do
  defmacro __using__(_opts) do
    quote do
      def opted_using_function(), do: :ok
    end
  end
end

defmodule ElixirSenseExample.ModuleUsingWithOpts do
  use ElixirSenseExample.UsingMacroWithOpts,
    some: :opt
end
