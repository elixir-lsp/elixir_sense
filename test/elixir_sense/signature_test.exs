defmodule ElixirSense.SignatureTest do

  use ExUnit.Case
  alias ElixirSense.Providers.Signature

  doctest Signature

  describe "signature" do

    test "find signatures from aliased modules" do
      code = """
      defmodule MyModule do
        alias List, as: MyList
        MyList.flatten(par1,
      end
      """
      assert ElixirSense.signature(code, 3, 23) == %{
        active_param: 1,
        pipe_before: false,
        signatures: [
          %{name: "flatten", params: ["list"]},
          %{name: "flatten", params: ["list", "tail"]}
        ]
      }
    end

    test "finds signatures from Kernel functions" do
      code = """
      defmodule MyModule do
        apply(par1,
      end
      """

      assert ElixirSense.signature(code, 2, 14) == %{
        active_param: 1,
        pipe_before: false,
        signatures: [
          %{name: "apply", params: ["fun", "args"]},
          %{name: "apply", params: ["module", "fun", "args"]}
        ]
      }
    end

    test "returns :none when it cannot identify a function call" do
      code = """
      defmodule MyModule do
        fn(a,
      end
      """
      assert ElixirSense.signature(code, 2, 8) == :none
    end

    test "return empty signature list when no signature is found" do
      code = """
      defmodule MyModule do
        a_func(
      end
      """
      assert ElixirSense.signature(code, 2, 10) == %{active_param: 0, signatures: [], pipe_before: false}
    end

    test "after |>" do
      code = """
      defmodule MyModule do
        {1, 2} |> IO.inspect(
      end
      """
      assert ElixirSense.signature(code, 2, 24) == %{
        active_param: 1,
        pipe_before: true,
        signatures: [
          %{name: "inspect", params: ["item", "opts \\\\ []"]},
          %{name: "inspect", params: ["device", "item", "opts"]}
        ]
      }
    end

  end

end
