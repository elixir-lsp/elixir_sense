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

      defp func({_, _, _}, optional \\\\ true) do
        IO.inspect optional
      end
    end
    """

    signatures =
      Parser.parse_string(code, true, true, 1)
      |> Metadata.get_function_signatures(MyModule, :func)

    assert signatures == [
             %{name: "func", params: ["par"], documentation: "", spec: ""},
             %{name: "func", params: ["my_list"], documentation: "", spec: ""},
             %{name: "func", params: ["par1", "par2"], documentation: "", spec: ""},
             %{name: "func", params: ["list", "par2"], documentation: "", spec: ""},
             %{name: "func", params: ["par1", "list"], documentation: "", spec: ""},
             %{name: "func", params: ["arg1", "par2"], documentation: "", spec: ""},
             %{name: "func", params: ["{}", "optional \\\\ true"], documentation: "", spec: ""}
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
    end
    """

    metadata = Parser.parse_string(code, true, true, 1)

    env = Metadata.get_env(metadata, 1)
    assert Metadata.at_module_body?(metadata, env)

    env = Metadata.get_env(metadata, 2)
    assert Metadata.at_module_body?(metadata, env)

    env = Metadata.get_env(metadata, 8)
    assert Metadata.at_module_body?(metadata, env)

    env = Metadata.get_env(metadata, 5)
    refute Metadata.at_module_body?(metadata, env)

    env = Metadata.get_env(metadata, 11)
    refute Metadata.at_module_body?(metadata, env)
  end
end
