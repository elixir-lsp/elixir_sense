defmodule ElixirSense.Core.MacroExpanderTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Core.MacroExpander

  test "expand use" do
    ast =
      quote do
        use ElixirSenseExample.OverridableFunctions
      end

    expanded =
      ast
      |> MacroExpander.add_default_meta()
      |> MacroExpander.expand_use(MyModule, [], line: 2, column: 1)

    if Version.match?(System.version(), ">= 1.13.0") do
      assert Macro.to_string(expanded) =~ "defmacro required(var)"
    end
  end

  test "expand use with alias" do
    ast =
      quote do
        use OverridableFunctions
      end

    expanded =
      ast
      |> MacroExpander.add_default_meta()
      |> MacroExpander.expand_use(
        MyModule,
        [{OverridableFunctions, ElixirSenseExample.OverridableFunctions}],
        line: 2,
        column: 1
      )

    if Version.match?(System.version(), ">= 1.13.0") do
      assert Macro.to_string(expanded) =~ "defmacro required(var)"
    end
  end

  test "expand use calling use" do
    ast =
      quote do
        use ElixirSenseExample.Overridable.Using
      end

    expanded =
      ast
      |> MacroExpander.add_default_meta()
      |> MacroExpander.expand_use(MyModule, [], line: 2, column: 1)

    if Version.match?(System.version(), ">= 1.13.0") do
      assert Macro.to_string(expanded) =~ "defmacro bar(var)"
    end
  end

  test "expand use when module does not define __using__ macro" do
    ast =
      quote do
        use ElixirSenseExample.OverridableBehaviour
      end

    expanded =
      ast
      |> MacroExpander.add_default_meta()
      |> MacroExpander.expand_use(MyModule, [], line: 2, column: 1)

    if Version.match?(System.version(), ">= 1.13.0") do
      assert Macro.to_string(expanded) =~ "require ElixirSenseExample.OverridableBehaviour"
    end
  end
end
