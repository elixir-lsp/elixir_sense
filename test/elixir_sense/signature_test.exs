defmodule ElixirSense.SignatureTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Providers.Signature

  doctest Signature

  describe "type signature" do
    test "find signatures from local type" do
      code = """
      defmodule MyModule do
        @typep my(a) :: {a, nil}
        @typep my(a, b) :: {a, b}
        @type a :: my(
      end
      """

      assert ElixirSense.signature(code, 4, 19) == %{
               active_param: 0,
               pipe_before: false,
               signatures: [
                 %{documentation: "", name: "my", params: ["a", "b"], spec: ""},
                 %{documentation: "", name: "my", params: ["a"], spec: ""}
               ]
             }
    end

    test "find signatures from local remote type" do
      code = """
      defmodule ElixirSenseExample.ModuleWithTypespecs.Remote do
        @type remote_t(a, b) :: {a, b}
        @type a :: remote_t(
      end
      """

      assert ElixirSense.signature(code, 3, 23) == %{
               active_param: 0,
               pipe_before: false,
               signatures: [
                 %{
                   name: "remote_t",
                   params: ["a", "b"],
                   documentation: "Remote type with params",
                   spec: "@type remote_t(a, b) :: {a, b}"
                 }
               ]
             }
    end

    test "find type signatures" do
      code = """
      defmodule MyModule do
        @type a :: ElixirSenseExample.ModuleWithTypespecs.Remote.remote_t(
      end
      """

      assert ElixirSense.signature(code, 2, 69) == %{
               pipe_before: false,
               active_param: 0,
               signatures: [
                 %{
                   documentation: "Remote type",
                   name: "remote_t",
                   params: [],
                   spec: "@type remote_t :: atom"
                 },
                 %{
                   documentation: "Remote type with params",
                   name: "remote_t",
                   params: ["a", "b"],
                   spec: "@type remote_t(a, b) :: {a, b}"
                 }
               ]
             }
    end

    test "find builtin type signatures with Elixir prefix" do
      code = """
      defmodule MyModule do
        @type a :: Elixir.keyword(
      end
      """

      assert ElixirSense.signature(code, 2, 29) == %{active_param: 0, pipe_before: false, signatures: [%{params: [], documentation: "No documentation available", name: "keyword", spec: "@type keyword :: [{atom, any}]"}, %{documentation: "No documentation available", name: "keyword", params: ["T"], spec: "@type keyword(t) :: [{atom, t}]"}]}
    end

    test "find type signatures from erlang module" do
      code = """
      defmodule MyModule do
        @type a :: :erlang.time_unit(
      end
      """

      assert ElixirSense.signature(code, 2, 32) == %{
               pipe_before: false,
               active_param: 0,
               signatures: [
                 %{
                   documentation: "No documentation available",
                   name: "time_unit",
                   params: [],
                   spec:
                     "@type time_unit :: pos_integer | :second | :millisecond | :microsecond | :nanosecond | :native | :perf_counter | deprecated_time_unit"
                 }
               ]
             }
    end

    test "find type signatures from builtin type" do
      code = """
      defmodule MyModule do
        @type a :: number(
      end
      """

      assert ElixirSense.signature(code, 2, 21) == %{
               active_param: 0,
               pipe_before: false,
               signatures: [
                 %{
                   params: [],
                   documentation: "An integer or a float",
                   name: "number",
                   spec: "@type number :: integer | float"
                 }
               ]
             }
    end
  end

  describe "macro signature" do
    test "find signatures from aliased modules" do
      code = """
      defmodule MyModule do
        require ElixirSenseExample.BehaviourWithMacrocallback.Impl, as: Macros
        Macros.some(
      end
      """

      assert ElixirSense.signature(code, 3, 15) == %{
               active_param: 0,
               pipe_before: false,
               signatures: [
                 %{
                   documentation: "some macro\n",
                   name: "some",
                   params: ["var"],
                   spec: "@spec some(integer) :: Macro.t\n@spec some(b) :: Macro.t when b: float"
                 }
               ]
             }
    end
  end

  describe "function signature" do
    test "find signatures from erlang module" do
      code = """
      defmodule MyModule do
        :lists.flatten(par1,
      end
      """

      assert ElixirSense.signature(code, 2, 24) == %{
               active_param: 1,
               pipe_before: false,
               signatures: [
                 %{
                   documentation: "No documentation available",
                   name: "flatten",
                   params: ["DeepList"],
                   spec:
                     "@spec flatten(deepList) :: list when deepList: [term | deepList], list: [term]"
                 },
                 %{
                   documentation: "No documentation available",
                   name: "flatten",
                   params: ["DeepList", "Tail"],
                   spec:
                     "@spec flatten(deepList, tail) :: list when deepList: [term | deepList], tail: [term], list: [term]"
                 }
               ]
             }
    end

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
                 %{
                   name: "flatten",
                   params: ["list"],
                   documentation: "Flattens the given `list` of nested lists.",
                   spec: "@spec flatten(deep_list) :: list when deep_list: [any | deep_list]"
                 },
                 %{
                   name: "flatten",
                   params: ["list", "tail"],
                   documentation:
                     "Flattens the given `list` of nested lists.\nThe list `tail` will be added at the end of\nthe flattened list.",
                   spec:
                     "@spec flatten(deep_list, [elem]) :: [elem] when deep_list: [elem | deep_list], elem: var"
                 }
               ]
             }
    end

    test "find signatures from imported modules" do
      code = """
      defmodule MyModule do
        import List
        flatten(par1,
      end
      """

      assert ElixirSense.signature(code, 3, 16) == %{
               active_param: 1,
               pipe_before: false,
               signatures: [
                 %{
                   name: "flatten",
                   params: ["list"],
                   documentation: "Flattens the given `list` of nested lists.",
                   spec: "@spec flatten(deep_list) :: list when deep_list: [any | deep_list]"
                 },
                 %{
                   name: "flatten",
                   params: ["list", "tail"],
                   documentation:
                     "Flattens the given `list` of nested lists.\nThe list `tail` will be added at the end of\nthe flattened list.",
                   spec:
                     "@spec flatten(deep_list, [elem]) :: [elem] when deep_list: [elem | deep_list], elem: var"
                 }
               ]
             }
    end

    test "find signatures when function with default args" do
      code = """
      defmodule MyModule do
        List.pop_at(par1,
      end
      """

      assert ElixirSense.signature(code, 2, 21) == %{
               active_param: 1,
               pipe_before: false,
               signatures: [
                 %{
                   documentation:
                     "Returns and removes the value at the specified `index` in the `list`.",
                   name: "pop_at",
                   params: ["list", "index", "default \\\\ nil"],
                   spec: "@spec pop_at(list, integer, any) :: {any, list}"
                 }
               ]
             }
    end

    test "find signatures when function with many clausess" do
      code = """
      defmodule MyModule do
        List.starts_with?(
      end
      """

      assert ElixirSense.signature(code, 2, 21) == %{
               active_param: 0,
               pipe_before: false,
               signatures: [
                 %{
                   documentation:
                     "Returns `true` if `list` starts with the given `prefix` list; otherwise returns `false`.",
                   name: "starts_with?",
                   params: ["list", "prefix"],
                   spec:
                     "@spec starts_with?([...], [...]) :: boolean\n@spec starts_with?(list, []) :: true\n@spec starts_with?([], [...]) :: false"
                 }
               ]
             }
    end

    test "find signatures from atom modules" do
      code = """
      defmodule MyModule do
        :"Elixir.List".flatten(par1,
      end
      """

      assert ElixirSense.signature(code, 2, 31) == %{
               active_param: 1,
               pipe_before: false,
               signatures: [
                 %{
                   name: "flatten",
                   params: ["list"],
                   documentation: "Flattens the given `list` of nested lists.",
                   spec: "@spec flatten(deep_list) :: list when deep_list: [any | deep_list]"
                 },
                 %{
                   name: "flatten",
                   params: ["list", "tail"],
                   documentation:
                     "Flattens the given `list` of nested lists.\nThe list `tail` will be added at the end of\nthe flattened list.",
                   spec:
                     "@spec flatten(deep_list, [elem]) :: [elem] when deep_list: [elem | deep_list], elem: var"
                 }
               ]
             }
    end

    test "find signatures from __MODULE__" do
      code = """
      defmodule Inspect do
        __MODULE__.Algebra.glue(par1,
      end
      """

      assert ElixirSense.signature(code, 2, 32) == %{
               active_param: 1,
               pipe_before: false,
               signatures: [
                 %{
                   documentation:
                     "Glues two documents (`doc1` and `doc2`) inserting the given\nbreak `break_string` between them.",
                   name: "glue",
                   params: ["doc1", "break_string \\\\ \" \"", "doc2"],
                   spec: "@spec glue(t, binary, t) :: t"
                 }
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
                 %{
                   name: "apply",
                   params: ["fun", "args"],
                   documentation:
                     "Invokes the given anonymous function `fun` with the list of\narguments `args`.",
                   spec: "@spec apply((... -> any), [any]) :: any"
                 },
                 %{
                   name: "apply",
                   params: ["module", "function_name", "args"],
                   documentation:
                     "Invokes the given function from `module` with the list of\narguments `args`.",
                   spec: "@spec apply(module, function_name :: atom, [any]) :: any"
                 }
               ]
             }
    end

    test "finds signatures from local functions" do
      code = """
      defmodule MyModule do

        def run do
          sum(a,
        end

        defp sum(a, b) do
          a + b
        end

        defp sum({a, b}) do
          a + b
        end
      end
      """

      assert ElixirSense.signature(code, 4, 12) == %{
               active_param: 1,
               pipe_before: false,
               signatures: [
                 %{
                   name: "sum",
                   params: ["a", "b"],
                   documentation: "",
                   spec: ""
                 },
                 %{
                   name: "sum",
                   params: ["tuple"],
                   documentation: "",
                   spec: ""
                 }
               ]
             }
    end

    test "finds signatures from module with many function clauses" do
      code = """
      defmodule Other do
        alias ElixirSenseExample.ModuleWithManyClauses, as: MyModule
        def run do
          MyModule.sum(a,
        end
      end
      """

      assert ElixirSense.signature(code, 4, 21) == %{
               active_param: 1,
               pipe_before: false,
               signatures: [
                 %{documentation: "", name: "sum", spec: "", params: ["s \\\\ nil", "f"]},
                 %{documentation: "", name: "sum", spec: "", params: ["arg", "x", "y"]}
               ]
             }
    end

    test "finds signatures from metadata module functions" do
      code = """
      defmodule MyModule do
        def sum(s \\\\ nil, f)
        def sum(a, nil), do: nil
        def sum(a, b) do
          a + b
        end

        def sum({a, b}, x, y) do
          a + b + x + y
        end
      end

      defmodule Other do
        def run do
          MyModule.sum(a,
        end
      end
      """

      assert ElixirSense.signature(code, 15, 21) == %{
               active_param: 1,
               pipe_before: false,
               signatures: [
                 %{documentation: "", name: "sum", params: ["s \\\\ nil", "f"], spec: ""},
                 %{documentation: "", name: "sum", spec: "", params: ["a", "atom"]},
                 %{documentation: "", name: "sum", params: ["a", "b"], spec: ""},
                 %{documentation: "", name: "sum", params: ["tuple", "x", "y"], spec: ""}
               ]
             }
    end

    test "does not finds signatures from metadata module private functions" do
      code = """
      defmodule MyModule do
        defp sum(a, nil), do: nil
        defp sum(a, b) do
          a + b
        end

        defp sum({a, b}) do
          a + b
        end
      end

      defmodule Other do
        def run do
          MyModule.sum(a,
        end
      end
      """

      assert ElixirSense.signature(code, 14, 21) == :none
    end

    test "finds signatures from metadata module functions with default param" do
      code = """
      defmodule MyModule do
        defp sum(a, b \\\\ 0) do
          a + b
        end

        def run do
          sum(a,
        end
      end
      """

      assert ElixirSense.signature(code, 7, 11) == %{
               active_param: 1,
               pipe_before: false,
               signatures: [
                 %{
                   name: "sum",
                   params: ["a", "b \\\\ 0"],
                   documentation: "",
                   spec: ""
                 }
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

    test "return :none when no signature is found" do
      code = """
      defmodule MyModule do
        a_func(
      end
      """

      assert ElixirSense.signature(code, 2, 10) == :none
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
                 %{
                   name: "inspect",
                   params: ["item", "opts \\\\ []"],
                   documentation: "Inspects and writes the given `item` to the device.",
                   spec: "@spec inspect(item, keyword) :: item when item: var"
                 },
                 %{
                   name: "inspect",
                   params: ["device", "item", "opts"],
                   documentation:
                     "Inspects `item` according to the given options using the IO `device`.",
                   spec: "@spec inspect(device, item, keyword) :: item when item: var"
                 }
               ]
             }
    end
  end
end
