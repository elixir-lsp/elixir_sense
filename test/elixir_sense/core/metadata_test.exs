defmodule ElixirSense.Core.MetadataTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Core.Parser
  alias ElixirSense.Core.Metadata

  test "get_function_params" do
    code = """
    defmodule MyModule do
      defp func(1) do
        IO.puts ""
      end

      defp func(par1) do
        IO.puts par1
      end

      defp func(par1, {a, _b} = par2) do
        IO.puts par1 <> a <> par2
      end

      defp func([head|_], par2) do
        IO.puts head <> par2
      end
    end
    """

    params =
      Parser.parse_string(code, true, true, 1)
      |> Metadata.get_function_params(MyModule, :func)

    assert params == [
             "1",
             "par1",
             "par1, {a, _b} = par2",
             "[head | _], par2"
           ]
  end

  test "get_function_signatures" do
    code = """
    defmodule MyModule do
      defp func(par) do
        IO.inspect par
      end

      defp func([] = my_list) do
        IO.inspect my_list
      end

      defp func({_, _, _}, optional \\\\ true)

      defp func(par1 = {a, _}, {_b, _c} = par2) do
        IO.inspect {a, par2}
      end

      defp func([head|_], par2) do
        IO.inspect head <> par2
      end

      defp func(par1, [head|_]) do
        IO.inspect {par1, head}
      end

      defp func("a_string", par2) do
        IO.inspect par2
      end
    end
    """

    signatures =
      Parser.parse_string(code, true, true, 1)
      |> Metadata.get_function_signatures(MyModule, :func)

    assert signatures == [
             %{name: "func", params: ["par"], documentation: "", spec: ""},
             %{
               name: "func",
               params: ["tuple", "optional \\\\ true"],
               documentation: "",
               spec: ""
             }
           ]
  end

  test "at_module_body?" do
    code = """
    defmodule MyModule do # 1
      @type a :: atom() # 2

      defp func(1) do
        IO.puts "" # 5
      end

      IO.puts "" # 8

      schema do
        IO.puts "" # 11
      end

      def go, do: :ok # 14

      def go, # 16
        do: :ok # 17
    end
    IO.puts ""
    """

    metadata = Parser.parse_string(code, true, true, 1)

    env = Metadata.get_env(metadata, {1, 22})
    assert Metadata.at_module_body?(env)

    env = Metadata.get_env(metadata, {2, 20})
    refute Metadata.at_module_body?(env)

    env = Metadata.get_env(metadata, {8, 13})
    assert Metadata.at_module_body?(env)

    env = Metadata.get_env(metadata, {5, 5})
    refute Metadata.at_module_body?(env)

    env = Metadata.get_env(metadata, {11, 5})
    assert Metadata.at_module_body?(env)

    env = Metadata.get_env(metadata, {14, 18})
    refute Metadata.at_module_body?(env)

    env = Metadata.get_env(metadata, {16, 10})
    refute Metadata.at_module_body?(env)

    env = Metadata.get_env(metadata, {17, 12})
    refute Metadata.at_module_body?(env)

    env = Metadata.get_env(metadata, {19, 1})
    refute Metadata.at_module_body?(env)
  end

  test "env is correct in scopes" do
    code = """
    IO.puts ""

    defmodule MyModule1 do
      IO.puts ""
    end

    IO.puts ""

    defmodule MyModule2 do
      @type a :: atom()

      defp func(1) do
        IO.puts ""
      end #

      IO.puts ""

      def go1, do: :ok

      def go2(a) when is_integer(a),
        do: :ok

      IO.puts ""

      def go2, do: :ok #

      IO.puts ""

      @spec go31(:a) :: :ok
      def go31(:a), do: :ok
      def go32(:b), do: :ok
      def go33(:c) do
        :ok
      end

      @spec some(1) :: :ok
      defp some(1) do
        IO.puts ""
      end

      @type x :: atom()

      @type y(a) ::
        atom()

      IO.puts ""
    end

    defprotocol Pr do
      @spec x(t) :: :ok
      def x(t)
    end

    defimpl Pr, for: [String, List] do
      def x(t), do: :ok
    end
    """

    metadata = Parser.parse_string(code, true, true, 1)

    env = Metadata.get_env(metadata, {1, 1})
    assert env.scope == Elixir

    env = Metadata.get_env(metadata, {4, 3})
    assert env.scope == :MyModule1

    env = Metadata.get_env(metadata, {7, 1})
    assert env.scope == Elixir

    env = Metadata.get_env(metadata, {9, 1})
    assert env.scope == :MyModule2

    env = Metadata.get_env(metadata, {10, 2})
    assert env.scope == :MyModule2

    env = Metadata.get_env(metadata, {10, 3})
    assert env.scope == {:typespec, :a, 0}

    env = Metadata.get_env(metadata, {10, 20})
    assert env.scope == {:typespec, :a, 0}

    env = Metadata.get_env(metadata, {12, 3})
    assert env.scope == {:func, 1}

    env = Metadata.get_env(metadata, {14, 6})
    assert env.scope == {:func, 1}

    env = Metadata.get_env(metadata, {14, 7})
    assert env.scope == :MyModule2

    env = Metadata.get_env(metadata, {16, 3})
    assert env.scope == :MyModule2

    env = Metadata.get_env(metadata, {18, 3})
    assert env.scope == {:go1, 0}

    env = Metadata.get_env(metadata, {20, 3})
    assert env.scope == {:go2, 1}

    env = Metadata.get_env(metadata, {21, 12})
    assert env.scope == {:go2, 1}

    env = Metadata.get_env(metadata, {23, 3})
    assert env.scope == :MyModule2

    env = Metadata.get_env(metadata, {25, 3})
    assert env.scope == {:go2, 0}

    env = Metadata.get_env(metadata, {25, 3})
    assert env.scope == {:go2, 0}

    env = Metadata.get_env(metadata, {25, 19})
    assert env.scope == {:go2, 0}

    env = Metadata.get_env(metadata, {25, 20})
    assert env.scope == {:go2, 0}

    env = Metadata.get_env(metadata, {27, 3})
    assert env.scope == :MyModule2

    env = Metadata.get_env(metadata, {29, 3})
    assert env.scope == {:typespec, :go31, 1}

    env = Metadata.get_env(metadata, {29, 23})
    assert env.scope == {:typespec, :go31, 1}

    env = Metadata.get_env(metadata, {30, 3})
    assert env.scope == {:go31, 1}

    env = Metadata.get_env(metadata, {30, 23})
    assert env.scope == {:go31, 1}

    env = Metadata.get_env(metadata, {31, 3})
    assert env.scope == {:go32, 1}

    env = Metadata.get_env(metadata, {32, 3})
    assert env.scope == {:go33, 1}

    env = Metadata.get_env(metadata, {36, 3})
    assert env.scope == {:typespec, :some, 1}

    env = Metadata.get_env(metadata, {37, 3})
    assert env.scope == {:some, 1}

    env = Metadata.get_env(metadata, {41, 3})
    assert env.scope == {:typespec, :x, 0}

    env = Metadata.get_env(metadata, {43, 3})
    assert env.scope == {:typespec, :y, 1}

    env = Metadata.get_env(metadata, {43, 3})
    assert env.scope == {:typespec, :y, 1}

    env = Metadata.get_env(metadata, {44, 11})
    assert env.scope == {:typespec, :y, 1}

    env = Metadata.get_env(metadata, {46, 3})
    assert env.scope == :MyModule2

    env = Metadata.get_env(metadata, {49, 1})
    assert env.scope == :Pr

    env = Metadata.get_env(metadata, {50, 3})
    assert env.scope == {:typespec, :x, 1}

    env = Metadata.get_env(metadata, {51, 3})
    assert env.scope == {:x, 1}

    env = Metadata.get_env(metadata, {51, 11})
    assert env.scope == {:x, 1}

    env = Metadata.get_env(metadata, {54, 3})
    assert env.scope == :"String(__or__)List"

    env = Metadata.get_env(metadata, {55, 3})
    assert env.scope == {:x, 1}
  end

  test "env is correct in module with do:" do
    code = """
    defmodule A, do: x
    """

    metadata = Parser.parse_string(code, true, true, 1)

    env = Metadata.get_env(metadata, {1, 19})
    assert env.scope == :A
  end

  test "get_position_to_insert_alias when aliases exist" do
    code = """
    defmodule MyModule do
      alias Foo.Bar #2

      def foo do
        IO.puts() #5
      end

      defmodule Inner do
        alias Foo.Bar #9
        def bar do
          IO.puts() #11
        end
      end
    end
    """

    line_number = 5
    metadata = Parser.parse_string(code, true, true, line_number)
    position = Metadata.get_position_to_insert_alias(metadata, {line_number, 6})

    assert {2, 3} == position

    line_number = 11
    metadata = Parser.parse_string(code, true, true, line_number)
    position = Metadata.get_position_to_insert_alias(metadata, {line_number, 8})

    assert {9, 5} == position
  end

  test "get_position_to_insert_alias when moduledoc exists" do
    code = """
    defmodule MyModule do
      @moduledoc \"\"\"
        New module without any aliases
      \"\"\"

      def foo do
         #7
      end
    end
    """

    line_number = 7
    metadata = Parser.parse_string(code, true, true, line_number)
    position = Metadata.get_position_to_insert_alias(metadata, {line_number, 6})

    assert {5, 3} == position
  end

  test "get_position_to_insert_alias when neither alias nor moduledoc exists" do
    code = """
    defmodule MyModule do
      def foo do
         #3
      end
    end
    """

    line_number = 3
    metadata = Parser.parse_string(code, true, true, line_number)
    position = Metadata.get_position_to_insert_alias(metadata, {line_number, 6})

    assert {2, 3} == position
  end
end
