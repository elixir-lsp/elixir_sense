defmodule ElixirSense.Providers.Definition.LocatorTest.MyBehaviour do
  defmacro __using__(_opts) do
    quote do
      def my_function(), do: :ok
    end
  end
end

defmodule ElixirSense.Providers.Definition.LocatorTest.ModUsingBehaviour do
  use ElixirSense.Providers.Definition.LocatorTest.MyBehaviour
end

defmodule ElixirSense.Providers.Definition.LocatorTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Providers.Definition.Locator

  test "finds definition of function defined in __using__ macro" do
    code = """
    defmodule MyModule do
      use ElixirSense.Providers.Definition.LocatorTest.MyBehaviour

      def test do
        my_function()
      end
    end
    """

    {line, column} = {5, 5}

    location = Locator.definition(code, line, column)

    assert location != nil
    assert location.type == :function
    assert location.line == 4
    assert location.column == 11
  end

  test "finds definition of function defined in __using__ macro via another module" do
    code = """
    defmodule MyModule do
      def test do
        ElixirSense.Providers.Definition.LocatorTest.ModUsingBehaviour.my_function()
      end
    end
    """

    {line, column} = {3, 70}

    location = Locator.definition(code, line, column)

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

    {line, column} = {5, 5}

    location = Locator.definition(code, line, column)

    assert location != nil
    assert location.type == :function
    assert location.file =~ "using_macro_example.ex"
    assert location.line == 4
    assert location.column == 11
  end

  test "finds definition of function defined in __using__ macro via module from external file" do
    code = """
    defmodule MyModule do
      def test do
        ElixirSenseExample.ModuleUsingMacroExample.using_macro_function()
      end
    end
    """

    {line, column} = {3, 49}

    location = Locator.definition(code, line, column)

    assert location != nil
    assert location.type == :function
    assert location.file =~ "using_macro_example.ex"
    assert location.line == 4
    assert location.column == 11
  end
end
