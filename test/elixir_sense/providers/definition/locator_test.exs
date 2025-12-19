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

  test "finds definition when using Kernel.use qualified call" do
    code = """
    defmodule MyModule do
      Kernel.use(ElixirSense.Providers.Definition.LocatorTest.MyBehaviour)

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

  test "finds definition when using module alias" do
    code = """
    defmodule MyModule do
      alias ElixirSense.Providers.Definition.LocatorTest.MyBehaviour

      use MyBehaviour

      def test do
        my_function()
      end
    end
    """

    {line, column} = {7, 5}

    location = Locator.definition(code, line, column)

    assert location != nil
    assert location.type == :function
    assert location.line == 4
    assert location.column == 11
  end

  test "finds definition when using module with custom alias" do
    code = """
    defmodule MyModule do
      alias ElixirSense.Providers.Definition.LocatorTest.MyBehaviour, as: MyB

      use MyB

      def test do
        my_function()
      end
    end
    """

    {line, column} = {7, 5}

    location = Locator.definition(code, line, column)

    assert location != nil
    assert location.type == :function
    assert location.line == 4
    assert location.column == 11
  end

  test "no false positive when local function named use exists" do
    code = """
    defmodule MyModule do
      defp use(_module), do: nil

      use ElixirSense.Providers.Definition.LocatorTest.MyBehaviour

      def test do
        use(SomeModule)
        my_function()
      end
    end
    """

    {line, column} = {8, 5}

    location = Locator.definition(code, line, column)

    assert location != nil
    assert location.type == :function
    # should find the function from __using__ macro, not be confused by local use/1
    assert location.line == 4
    assert location.column == 11
  end

  describe "modules in external file" do
    test "finds definition via external module using Kernel.use qualified call" do
      code = """
      defmodule MyModule do
        def test do
          ElixirSenseExample.ModuleUsingKernelUse.using_macro_function()
        end
      end
      """

      {line, column} = {3, 45}

      location = Locator.definition(code, line, column)

      assert location != nil
      assert location.type == :function
      assert location.file =~ "using_macro_example.ex"
      assert location.line == 4
      assert location.column == 11
    end

    test "finds definition via external module using aliased module" do
      code = """
      defmodule MyModule do
        def test do
          ElixirSenseExample.ModuleUsingAlias.using_macro_function()
        end
      end
      """

      {line, column} = {3, 42}

      location = Locator.definition(code, line, column)

      assert location != nil
      assert location.type == :function
      assert location.file =~ "using_macro_example.ex"
      assert location.line == 4
      assert location.column == 11
    end

    test "finds definition via external module that has other local functions" do
      code = """
      defmodule MyModule do
        def test do
          ElixirSenseExample.ModuleWithLocalUse.using_macro_function()
        end
      end
      """

      {line, column} = {3, 44}

      location = Locator.definition(code, line, column)

      assert location != nil
      assert location.type == :function
      assert location.file =~ "using_macro_example.ex"
      assert location.line == 4
      assert location.column == 11
    end
  end
end
