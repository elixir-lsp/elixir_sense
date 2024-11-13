defmodule ElixirSense.Core.OptionsTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.Options
  alias ElixirSense.Core.Parser

  defp get_options(code, module, function, arity) do
    metadata = Parser.parse_string(code, true, true, {1, 1})
    Options.get_param_options(module, function, arity, metadata)
  end

  describe "metadata" do
    test "gets options only one option" do
      code = """
      defmodule Foo do
        @spec bar([{:option1, integer()}]) :: :ok
        def bar(options), do: :ok
      end
      """

      assert [
               option1: {:integer, _, []}
             ] = get_options(code, Foo, :bar, 1)
    end

    test "gets options union" do
      code = """
      defmodule Foo do
        @spec bar([{:option1, integer()} | {:option2, String.t()}]) :: :ok
        def bar(options), do: :ok
      end
      """

      assert [
               option1: {:integer, _, []},
               option2: {{:., _, [{:__aliases__, _, [:String]}, :t]}, _, []}
             ] = get_options(code, Foo, :bar, 1)
    end

    test "gets options list" do
      code = """
      defmodule Foo do
        @spec bar([{:option1, integer()}, {:option2, String.t()}]) :: :ok
        def bar(options), do: :ok
      end
      """

      assert [
               option1: {:integer, _, []},
               option2: {{:., _, [{:__aliases__, _, [:String]}, :t]}, _, []}
             ] = get_options(code, Foo, :bar, 1)
    end

    test "gets options keyword" do
      code = """
      defmodule Foo do
        @spec bar([option1: integer(), option2: String.t()]) :: :ok
        def bar(options), do: :ok
      end
      """

      assert [
               option1: {:integer, _, []},
               option2: {{:., _, [{:__aliases__, _, [:String]}, :t]}, _, []}
             ] = get_options(code, Foo, :bar, 1)
    end

    test "skips non option types in list" do
      code = """
      defmodule Foo do
        @spec bar([:foo | {:option1, integer()} | 1]) :: :ok
        def bar(options), do: :ok
      end
      """

      assert [
               option1: {:integer, _, []}
             ] = get_options(code, Foo, :bar, 1)
    end

    test "skips non option types" do
      code = """
      defmodule Foo do
        @spec bar(keyword()) :: :ok
        def bar(options), do: :ok
      end
      """

      assert [] == get_options(code, Foo, :bar, 1)
    end

    test "skips functions without spec" do
      code = """
      defmodule Foo do
        def bar(options), do: :ok
      end
      """

      assert [] == get_options(code, Foo, :bar, 1)
    end

    test "gets options by expanding list type" do
      code = """
      defmodule Foo do
        @type options_t() :: [{:option1, integer()} | {:option2, String.t()}]
        @spec bar(options_t()) :: :ok
        def bar(options), do: :ok
      end
      """

      assert [
               option1: {:integer, _, []},
               option2: {{:., _, [{:__aliases__, _, [:String]}, :t]}, _, []}
             ] = get_options(code, Foo, :bar, 1)
    end

    test "gets options by expanding option type" do
      code = """
      defmodule Foo do
        @type option1_t() :: {:option1, integer()}
        @type option2_t() :: {:option2, String.t()}
        @spec bar([option1_t() | option2_t()]) :: :ok
        def bar(options), do: :ok
      end
      """

      assert [
               option1: {:integer, _, []},
               option2: {{:., _, [{:__aliases__, _, [:String]}, :t]}, _, []}
             ] = get_options(code, Foo, :bar, 1)
    end
  end

  describe "typescpec chunk" do
    test "gets options only one option" do
      assert [
               option1: {:integer, _, []}
             ] = get_options("", ElixirSenseExample.Options.Foo1, :bar, 1)
    end

    test "gets options" do
      assert [
               option1: {:integer, _, []},
               option2: {{:., _, [String, :t]}, _, []}
             ] = get_options("", ElixirSenseExample.Options.Foo, :bar, 1)
    end
  end

  describe "expand type" do
    defp expand(code, type, module) do
      metadata = Parser.parse_string(code, true, true, {1, 1})
      Options.expand_type(Code.string_to_quoted!(type), metadata, module, [])
    end

    test "builtin metadata type" do
      code = """
      defmodule Foo do
      end
      """
      assert {:integer, _, []} = expand(code, "integer()", Foo)
    end

    test "local metadata type" do
      code = """
      defmodule Foo do
        @type foo() :: :bar
      end
      """
      assert expand(code, "foo()", Foo) == :bar
    end

    test "local metadata type with arg" do
      code = """
      defmodule Foo do
        @type foo(t) :: t
      end
      """
      assert {:integer, _, []} = expand(code, "foo(integer())", Foo)
    end

    test "undefined local metadata type" do
      code = """
      defmodule Foo do
      end
      """
      assert {{:., _, [Foo, :foo]}, _, []} = expand(code, "foo()", Foo)
    end

    test "remote metadata type" do
      code = """
      defmodule Foo do
        @type foo() :: :bar
      end
      """
      assert expand(code, "Foo.foo()", Bar) == :bar
    end

    test "undefined remote metadata type" do
      code = """
      defmodule Foo do
      end
      """
      assert {{:., _, [Foo, :foo]}, _, []} = expand(code, "Foo.foo()", Bar)
    end

    test "nested metadata type" do
      code = """
      defmodule Foo do
        @type bar() :: :baz
        @type foo() :: bar()
      end
      """
      assert expand(code, "foo()", Foo) == :baz
    end
  end
end
