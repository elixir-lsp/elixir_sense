defmodule ElixirSense.Core.MetadataTest do

  use ExUnit.Case

  alias ElixirSense.Core.Parser
  alias ElixirSense.Core.Metadata

  test "params" do
    code =
      """
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
      Parser.parse_string(code, true, true, 0)
      |> Metadata.get_function_params(MyModule, :func)

    assert params == [
      "1",
      "par1",
      "par1, {a, _b} = par2",
      "[head | _], par2"
    ]
  end

end
