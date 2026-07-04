defmodule ElixirSense.Providers.Definition.LocatorUsingMacroTest.MyBehaviour do
  defmacro __using__(_opts) do
    quote do
      def my_function(), do: :ok
    end
  end

  # Unrelated regular function (outside __using__) sharing a name with a
  # purely-local function in a consuming module.
  def my_function_unrelated(), do: :unrelated
end

defmodule ElixirSense.Providers.Definition.LocatorUsingMacroTest.ModUsingBehaviour do
  use ElixirSense.Providers.Definition.LocatorUsingMacroTest.MyBehaviour
end

defmodule ElixirSense.Providers.Definition.LocatorUsingMacroTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Providers.Definition.Locator

  @behaviour_mod ElixirSense.Providers.Definition.LocatorUsingMacroTest.MyBehaviour

  describe "function injected via __using__ (the cases #330 targets)" do
    test "finds definition of function defined in __using__ macro (current source)" do
      code = """
      defmodule MyModule do
        use #{inspect(@behaviour_mod)}

        def test do
          my_function()
        end
      end
      """

      location = Locator.definition(code, 5, 5)

      assert location != nil
      assert location.type == :function
      assert location.file =~ "locator_using_macro_test.exs"
      assert location.line == 4
      assert location.column == 11
    end

    test "finds definition via another in-source module that uses the behaviour" do
      code = """
      defmodule MyModule do
        def test do
          ElixirSense.Providers.Definition.LocatorUsingMacroTest.ModUsingBehaviour.my_function()
        end
      end
      """

      location = Locator.definition(code, 3, 78)

      assert location != nil
      assert location.type == :function
      assert location.line == 4
      assert location.column == 11
    end

    test "finds definition of function defined in __using__ macro from external file" do
      code = """
      defmodule MyModule do
        use ElixirSenseExample.UsingMacroExample

        def test do
          using_macro_function()
        end
      end
      """

      location = Locator.definition(code, 5, 5)

      assert location != nil
      assert location.type == :function
      assert location.file =~ "using_macro_example.ex"
      assert location.line == 4
      assert location.column == 11
    end

    test "finds definition via external module that uses the behaviour" do
      code = """
      defmodule MyModule do
        def test do
          ElixirSenseExample.ModuleUsingMacroExample.using_macro_function()
        end
      end
      """

      location = Locator.definition(code, 3, 50)

      assert location != nil
      assert location.type == :function
      assert location.file =~ "using_macro_example.ex"
      assert location.line == 4
      assert location.column == 11
    end

    test "resolves aliased use" do
      code = """
      defmodule MyModule do
        def test do
          ElixirSenseExample.ModuleUsingAlias.using_macro_function()
        end
      end
      """

      location = Locator.definition(code, 3, 42)

      assert location != nil
      assert location.file =~ "using_macro_example.ex"
      assert location.line == 4
      assert location.column == 11
    end

    test "resolves Kernel.use qualified call" do
      code = """
      defmodule MyModule do
        def test do
          ElixirSenseExample.ModuleUsingKernelUse.using_macro_function()
        end
      end
      """

      location = Locator.definition(code, 3, 45)

      assert location != nil
      assert location.file =~ "using_macro_example.ex"
      assert location.line == 4
      assert location.column == 11
    end

    test "finds definition injected via defdelegate" do
      code = """
      defmodule MyModule do
        def test do
          ElixirSenseExample.ModuleUsingOtherForms.delegated_function()
        end
      end
      """

      location = Locator.definition(code, 3, 48)

      assert location != nil
      assert location.file =~ "using_macro_example.ex"
      # `defdelegate delegated_function(), ...` inside __using__
      assert location.line == 52
      assert location.column == 19
    end

    test "finds definition injected through a transitive use chain (current source)" do
      code = """
      defmodule MyModule do
        use ElixirSenseExample.UsingMacroOuter

        def test do
          nested_using_function()
        end
      end
      """

      location = Locator.definition(code, 5, 5)

      assert location != nil
      assert location.type == :function
      assert location.file =~ "using_macro_example.ex"
      # `def nested_using_function` lives in UsingMacroInner.__using__, two hops
      # away (MyModule -> UsingMacroOuter -> UsingMacroInner)
      assert location.line == 72
      assert location.column == 11
    end

    test "finds definition through a transitive use chain via external module" do
      code = """
      defmodule MyModule do
        def test do
          ElixirSenseExample.ModuleUsingNested.nested_using_function()
        end
      end
      """

      location = Locator.definition(code, 3, 48)

      assert location != nil
      assert location.type == :function
      assert location.file =~ "using_macro_example.ex"
      assert location.line == 72
      assert location.column == 11
    end

    test "finds definition when the use spans multiple lines (current source)" do
      code = """
      defmodule MyModule do
        use ElixirSenseExample.UsingMacroWithOpts,
          some: :opt

        def test do
          opted_using_function()
        end
      end
      """

      location = Locator.definition(code, 6, 5)

      assert location != nil
      assert location.type == :function
      assert location.file =~ "using_macro_example.ex"
      assert location.line == 95
      assert location.column == 11
    end

    test "finds definition when the use spans multiple lines (external module)" do
      code = """
      defmodule MyModule do
        def test do
          ElixirSenseExample.ModuleUsingWithOpts.opted_using_function()
        end
      end
      """

      location = Locator.definition(code, 3, 45)

      assert location != nil
      assert location.type == :function
      assert location.file =~ "using_macro_example.ex"
      assert location.line == 95
      assert location.column == 11
    end

    test "finds definition injected via defguard" do
      # A remote guard must be called from a guard context (and the module
      # required) to resolve as a macro — unrelated to the using-macro search.
      code = """
      defmodule MyModule do
        require ElixirSenseExample.ModuleUsingOtherForms
        def test(x) when ElixirSenseExample.ModuleUsingOtherForms.is_even(x), do: x
      end
      """

      location = Locator.definition(code, 3, 62)

      assert location != nil
      assert location.file =~ "using_macro_example.ex"
      # `defguard is_even(value) when ...` inside __using__
      assert location.line == 56
      assert location.column == 16
    end
  end

  describe "regression guards (must NOT misfire)" do
    test "purely-local function is not redirected to an unrelated def in the used module's file" do
      # MyModule uses a module whose file also contains an unrelated
      # `def my_function_unrelated` (outside __using__). The local definition
      # must win.
      code = """
      defmodule MyModule do
        use #{inspect(@behaviour_mod)}

        def my_function_unrelated(), do: :my_local

        def test do
          my_function_unrelated()
        end
      end
      """

      location = Locator.definition(code, 7, 5)

      assert location != nil
      # local def is on line 4 of the in-memory buffer, file == nil
      assert location.file == nil
      assert location.line == 4
    end

    test "external local function is not redirected when it is a real def, not injected" do
      # ModuleWithLocalUse defines `using_macro_function_unrelated` itself; the
      # used module's file also has an unrelated def of the same name. The real
      # local def in ModuleWithLocalUse must win.
      code = """
      defmodule MyModule do
        def test do
          ElixirSenseExample.ModuleWithLocalUse.using_macro_function_unrelated()
        end
      end
      """

      location = Locator.definition(code, 3, 45)

      assert location != nil
      assert location.file =~ "using_macro_example.ex"
      # must land on the local def in ModuleWithLocalUse (`:local`), which is the
      # last def in the file, NOT the unrelated def near the top
      assert location.line > 30
    end

    test "ordinary remote function (module without use) is unaffected" do
      code = """
      defmodule MyModule do
        def test do
          ElixirSenseExample.PlainModuleExample.plain_function()
        end
      end
      """

      location = Locator.definition(code, 3, 45)

      assert location != nil
      assert location.file =~ "using_macro_example.ex"
      assert location.type in [:function, :macro]
      # resolves to the real def, not redirected anywhere
      refute is_nil(location.line)
    end

    @tag requires_source: true
    test "ordinary stdlib remote function is unaffected" do
      code = """
      defmodule MyModule do
        def test do
          Enum.map([1], & &1)
        end
      end
      """

      location = Locator.definition(code, 3, 10)

      assert location != nil
      assert location.file =~ "enum.ex"
      assert location.type in [:function, :macro]
    end
  end
end
