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
               signatures: [
                 %{
                   documentation: "",
                   name: "my",
                   params: ["a"],
                   spec: "@typep my(a) :: {a, nil}"
                 },
                 %{
                   documentation: "",
                   name: "my",
                   params: ["a", "b"],
                   spec: "@typep my(a, b) :: {a, b}"
                 }
               ]
             }
    end

    test "find signatures from local type, filter by arity" do
      code = """
      defmodule MyModule do
        @typep my(a) :: {a, nil}
        @typep my(a, b) :: {a, b}
        @type a :: my(atom,
      end
      """

      assert ElixirSense.signature(code, 4, 25) == %{
               active_param: 1,
               signatures: [
                 %{
                   documentation: "",
                   name: "my",
                   params: ["a", "b"],
                   spec: "@typep my(a, b) :: {a, b}"
                 }
               ]
             }
    end

    test "find signatures from local type, filter by arity unfinished param" do
      code = """
      defmodule MyModule do
        @typep my(a) :: {a, nil}
        @typep my(a, b) :: {a, b}
        @type a :: my(atom
      end
      """

      assert ElixirSense.signature(code, 4, 24) == %{
               active_param: 0,
               signatures: [
                 %{
                   documentation: "",
                   name: "my",
                   params: ["a"],
                   spec: "@typep my(a) :: {a, nil}"
                 },
                 %{
                   documentation: "",
                   name: "my",
                   params: ["a", "b"],
                   spec: "@typep my(a, b) :: {a, b}"
                 }
               ]
             }
    end

    test "find signatures from local type, filter by arity unfinished params" do
      code = """
      defmodule MyModule do
        @typep my(a) :: {a, nil}
        @typep my(a, b) :: {a, b}
        @type a :: my(atom, atom
      end
      """

      assert ElixirSense.signature(code, 4, 30) == %{
               active_param: 1,
               signatures: [
                 %{
                   documentation: "",
                   name: "my",
                   params: ["a", "b"],
                   spec: "@typep my(a, b) :: {a, b}"
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

    test "find type signatures with @typedoc false" do
      code = """
      defmodule MyModule do
        @type a :: ElixirSenseExample.ModuleWithDocs.some_type_doc_false(
      end
      """

      assert ElixirSense.signature(code, 2, 68) == %{
               active_param: 0,
               signatures: [
                 %{
                   documentation: "",
                   name: "some_type_doc_false",
                   params: '',
                   spec: "@type some_type_doc_false :: integer"
                 }
               ]
             }
    end

    test "does not find builtin type signatures with Elixir prefix" do
      code = """
      defmodule MyModule do
        @type a :: Elixir.keyword(
      end
      """

      assert ElixirSense.signature(code, 2, 29) == :none
    end

    test "find type signatures from erlang module" do
      code = """
      defmodule MyModule do
        @type a :: :erlang.time_unit(
      end
      """

      assert %{
               active_param: 0,
               signatures: [
                 %{
                   documentation: summary,
                   name: "time_unit",
                   params: [],
                   spec: "@type time_unit ::" <> _
                 }
               ]
             } = ElixirSense.signature(code, 2, 32)

      if ExUnitConfig.erlang_eep48_supported() do
        assert "Supported time unit representations:" <> _ = summary
      end
    end

    test "find type signatures from builtin type" do
      code = """
      defmodule MyModule do
        @type a :: number(
      end
      """

      assert ElixirSense.signature(code, 2, 21) == %{
               active_param: 0,
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

    test "find signatures special forms" do
      code = """
      defmodule MyModule do
        __MODULE__(
      end
      """

      assert ElixirSense.signature(code, 2, 14) == %{
               active_param: 0,
               signatures: [
                 %{
                   documentation:
                     "Returns the current module name as an atom or `nil` otherwise.",
                   name: "__MODULE__",
                   params: [],
                   spec: ""
                 }
               ]
             }
    end
  end

  describe "function signature" do
    test "find signatures from erlang module" do
      code = """
      defmodule MyModule do
        :lists.flatten(
      end
      """

      assert %{
               active_param: 0,
               signatures: [
                 %{
                   documentation: summary1,
                   name: "flatten",
                   params: ["deepList"],
                   spec:
                     "@spec flatten(deepList) :: list when deepList: [term | deepList], list: [term]"
                 },
                 %{
                   documentation: summary2,
                   name: "flatten",
                   params: ["deepList", "tail"],
                   spec:
                     "@spec flatten(deepList, tail) :: list when deepList: [term | deepList], tail: [term], list: [term]"
                 }
               ]
             } = ElixirSense.signature(code, 2, 24)

      if ExUnitConfig.erlang_eep48_supported() do
        assert "Returns a flattened version of `DeepList`\\." <> _ = summary1

        assert "Returns a flattened version of `DeepList` with tail `Tail` appended\\." <> _ =
                 summary2
      end
    end

    test "find signatures from aliased modules" do
      code = """
      defmodule MyModule do
        alias List, as: MyList
        MyList.flatten(
      end
      """

      assert ElixirSense.signature(code, 3, 23) == %{
               active_param: 0,
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

    test "find signatures from aliased modules aaa" do
      code = """
      defmodule MyModule do
        alias NonExisting, as: List
        Elixir.List.flatten(
      end
      """

      assert ElixirSense.signature(code, 3, 28) == %{
               active_param: 0,
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
        flatten(
      end
      """

      assert ElixirSense.signature(code, 3, 16) == %{
               active_param: 0,
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

    test "find signatures for function with @doc false" do
      code = """
      defmodule MyModule do
        ElixirSenseExample.ModuleWithDocs.some_fun_doc_false(
      end
      """

      assert ElixirSense.signature(code, 2, 56) == %{
               active_param: 0,
               signatures: [
                 %{
                   documentation: "",
                   name: "some_fun_doc_false",
                   params: ["a", "b \\\\ nil"],
                   spec: ""
                 }
               ]
             }
    end

    test "find signatures from atom modules" do
      code = """
      defmodule MyModule do
        :"Elixir.List".flatten(
      end
      """

      assert ElixirSense.signature(code, 2, 31) == %{
               active_param: 0,
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
      defmodule Inspect.Algebra do
        __MODULE__.glue(par1,
      end
      """

      assert ElixirSense.signature(code, 2, 24) == %{
               active_param: 1,
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

    test "find signatures from __MODULE__ submodule" do
      code = """
      defmodule Inspect do
        __MODULE__.Algebra.glue(par1,
      end
      """

      assert ElixirSense.signature(code, 2, 32) == %{
               active_param: 1,
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

    test "find signatures from attribute" do
      code = """
      defmodule MyMod do
        @attribute Inspect.Algebra
        @attribute.glue(par1,
      end
      """

      assert ElixirSense.signature(code, 3, 24) == %{
               active_param: 1,
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

    @tag :capture_log
    test "find signatures from attribute submodule" do
      code = """
      defmodule Inspect do
        @attribute Inspect
        @attribute.Algebra.glue(par1,
      end
      """

      assert ElixirSense.signature(code, 3, 32) == %{
               active_param: 1,
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

    test "find signatures from variable" do
      code = """
      defmodule MyMod do
        myvariable = Inspect.Algebra
        myvariable.glue(par1,
      end
      """

      assert ElixirSense.signature(code, 3, 24) == %{
               active_param: 1,
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

    @tag :capture_log
    test "find signatures from variable submodule - don't crash" do
      code = """
      defmodule Inspect do
        myvariable = Inspect
        myvariable.Algebra.glue(par1,
      end
      """

      assert ElixirSense.signature(code, 3, 32) == :none
    end

    test "find signatures from variable call" do
      code = """
      defmodule Inspect do
        myvariable = &Inspect.Algebra.glue/2
        myvariable.(par1,
      end
      """

      # TODO?
      assert ElixirSense.signature(code, 3, 20) == :none
    end

    test "find signatures from attribute call" do
      code = """
      defmodule Inspect do
        @attribute &Inspect.Algebra.glue/2
        @attribute.(par1,
      end
      """

      # TODO?
      assert ElixirSense.signature(code, 3, 20) == :none
    end

    test "finds signatures from Kernel functions" do
      code = """
      defmodule MyModule do
        apply(par1,
      end
      """

      assert ElixirSense.signature(code, 2, 14) == %{
               active_param: 1,
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
          sum(
        end

        defp sum(a, b) do
          a + b
        end

        defp sum({a, b}) do
          a + b
        end
      end
      """

      assert ElixirSense.signature(code, 4, 9) == %{
               active_param: 0,
               signatures: [
                 %{
                   name: "sum",
                   params: ["tuple"],
                   documentation: "",
                   spec: ""
                 },
                 %{
                   name: "sum",
                   params: ["a", "b"],
                   documentation: "",
                   spec: ""
                 }
               ]
             }
    end

    test "finds signatures from local functions, filter by arity" do
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
               signatures: [
                 %{
                   name: "sum",
                   params: ["a", "b"],
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
        @spec sum(integer, integer) :: integer
        defp sum(a, b \\\\ 0) do
          a + b
        end

        def run do
          sum(a,
        end
      end
      """

      assert ElixirSense.signature(code, 8, 11) == %{
               active_param: 1,
               signatures: [
                 %{
                   name: "sum",
                   params: ["a", "b \\\\ 0"],
                   documentation: "",
                   spec: "@spec sum(integer, integer) :: integer"
                 }
               ]
             }
    end

    test "finds signatures from metadata elixir behaviour call" do
      code = """
      defmodule MyModule do
        use GenServer

        def handle_call(request, _from, state) do
          terminate()
        end

        def init(arg), do: arg

        def handle_cast(arg, _state) when is_atom(arg) do
          :ok
        end
      end
      """

      assert %{
               active_param: 0,
               signatures: [
                 %{
                   name: "terminate",
                   params: ["_reason", "_state"],
                   documentation: "Invoked when the server is about to exit" <> _,
                   spec: "@spec terminate(reason, state :: term) :: term" <> _
                 }
               ]
             } = ElixirSense.signature(code, 5, 15)
    end

    test "finds signatures from metadata erlang behaviour call" do
      code = """
      defmodule MyModule do
        @behaviour :gen_server

        def handle_call(request, _from, state) do
          init()
        end

        def init(arg), do: arg

        def handle_cast(arg, _state) when is_atom(arg) do
          :ok
        end
      end
      """

      assert %{
               active_param: 0,
               signatures: [
                 %{
                   name: "init",
                   params: ["arg"],
                   documentation: summary,
                   spec: "@spec init(args :: term) ::" <> _
                 }
               ]
             } = ElixirSense.signature(code, 5, 10)

      if ExUnitConfig.erlang_eep48_supported() do
        assert "- Args = term\\(\\)\n- Result" <> _ = summary
      end
    end

    test "finds signatures from metadata elixir behaviour call from outside" do
      code = """
      ElixirSenseExample.ExampleBehaviourWithDocCallbackImpl.bar()
      """

      assert %{
               active_param: 0,
               signatures: [
                 %{
                   documentation: "Docs for bar",
                   name: "bar",
                   params: ["b"],
                   spec: "@spec bar(integer) :: Macro.t"
                 }
               ]
             } = ElixirSense.signature(code, 1, 60)
    end

    test "finds signatures from metadata erlang behaviour implemented in elixir call from outside" do
      code = """
      ElixirSenseExample.ExampleBehaviourWithDocCallbackErlang.init()
      """

      res = ElixirSense.signature(code, 1, 63)

      if ExUnitConfig.erlang_eep48_supported() do
        assert %{
                 active_param: 0,
                 signatures: [
                   %{
                     documentation: "- Args = " <> _,
                     name: "init",
                     params: ["_"],
                     spec: "@spec init(args :: term) :: init_result(state)"
                   }
                 ]
               } = res
      end
    end

    @tag requires_otp_25: true
    test "finds signatures from metadata erlang behaviour call from outside" do
      code = """
      :file_server.init()
      """

      res = ElixirSense.signature(code, 1, 19)

      assert %{
               active_param: 0,
               signatures: [
                 %{
                   documentation: "- Args = " <> _,
                   name: "init",
                   params: ["args"],
                   spec: "@spec init(args :: term) ::" <> _
                 }
               ]
             } = res
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

      assert %{
               active_param: 1,
               signatures: [
                 %{
                   name: "inspect",
                   params: ["item", "opts \\\\ []"],
                   documentation: "Inspects and writes the given `item` to the device.",
                   spec: "@spec inspect(" <> _
                 },
                 %{
                   name: "inspect",
                   params: ["device", "item", "opts"],
                   documentation:
                     "Inspects `item` according to the given options using the IO `device`.",
                   spec: "@spec inspect(device, item, keyword) :: item when item: var"
                 }
               ]
             } = ElixirSense.signature(code, 2, 24)
    end

    test "after |> variable" do
      code = """
      s |> String.replace_prefix(
      """

      assert %{
               active_param: 1
             } = ElixirSense.signature(code, 1, 28)
    end

    test "find built-in functions" do
      # module_info is defined by default for every elixir and erlang module
      # __info__ is defined for every elixir module
      # behaviour_info is defined for every behaviour and every protocol
      buffer = """
      defmodule MyModule do
        ElixirSenseExample.ModuleWithFunctions.module_info()
        #                                                  ^
        ElixirSenseExample.ModuleWithFunctions.__info__(:macros)
        #                                               ^
        ElixirSenseExample.ExampleBehaviour.behaviour_info(:callbacks)
        #                                                  ^
      end
      """

      assert ElixirSense.signature(buffer, 2, 54) == %{
               active_param: 0,
               signatures: [
                 %{
                   documentation: "Built-in function",
                   name: "module_info",
                   params: [],
                   spec:
                     "@spec module_info :: [{:module | :attributes | :compile | :exports | :md5 | :native, term}]"
                 },
                 %{
                   documentation: "Built-in function",
                   name: "module_info",
                   params: ["key"],
                   spec: """
                   @spec module_info(:module) :: atom
                   @spec module_info(:attributes | :compile) :: [{atom, term}]
                   @spec module_info(:md5) :: binary
                   @spec module_info(:exports | :functions | :nifs) :: [{atom, non_neg_integer}]
                   @spec module_info(:native) :: boolean\
                   """
                 }
               ]
             }

      assert ElixirSense.signature(buffer, 4, 51) == %{
               active_param: 0,
               signatures: [
                 %{
                   documentation: "Built-in function",
                   name: "__info__",
                   params: ["atom"],
                   spec: """
                   @spec __info__(:attributes) :: keyword()
                   @spec __info__(:compile) :: [term()]
                   @spec __info__(:functions) :: [{atom, non_neg_integer}]
                   @spec __info__(:macros) :: [{atom, non_neg_integer}]
                   @spec __info__(:md5) :: binary()
                   @spec __info__(:module) :: module()\
                   """
                 }
               ]
             }

      assert ElixirSense.signature(buffer, 6, 54) == %{
               active_param: 0,
               signatures: [
                 %{
                   documentation: "Built-in function",
                   name: "behaviour_info",
                   params: ["key"],
                   spec:
                     "@spec behaviour_info(:callbacks | :optional_callbacks) :: [{atom, non_neg_integer}]"
                 }
               ]
             }
    end

    test "built-in functions cannot be called locally" do
      # module_info is defined by default for every elixir and erlang module
      # __info__ is defined for every elixir module
      # behaviour_info is defined for every behaviour and every protocol
      buffer = """
      defmodule MyModule do
        import GenServer
        @ callback cb() :: term
        module_info()
        #           ^
        __info__(:macros)
        #        ^
        behaviour_info(:callbacks)
        #              ^
      end
      """

      assert :none = ElixirSense.signature(buffer, 4, 15)

      assert :none = ElixirSense.signature(buffer, 6, 12)

      assert :none = ElixirSense.signature(buffer, 8, 18)
    end

    @tag requires_otp_23: true
    test "find built-in erlang functions" do
      buffer = """
      defmodule MyModule do
        :erlang.orelse()
        #             ^
        :erlang.or()
        #         ^
      end
      """

      %{
        active_param: 0,
        signatures: [
          %{
            documentation: "",
            name: "orelse",
            params: ["term", "term"],
            spec: ""
          }
        ]
      } = ElixirSense.signature(buffer, 2, 18)

      assert %{
               active_param: 0,
               signatures: [
                 %{
                   documentation: "",
                   name: "or",
                   params: [_, _],
                   spec: "@spec boolean or boolean :: boolean"
                 }
               ]
             } = ElixirSense.signature(buffer, 4, 14)
    end

    test "find :erlang module functions with different forms of typespecs" do
      buffer = """
      defmodule MyModule do
        :erlang.date()
        #           ^
        :erlang.cancel_timer()
        #                   ^
      end
      """

      %{
        active_param: 0,
        signatures: [
          %{
            documentation: summary,
            name: "date",
            params: [],
            spec: "@spec date :: date when date: :calendar.date"
          }
        ]
      } = ElixirSense.signature(buffer, 2, 16)

      if ExUnitConfig.erlang_eep48_supported() do
        assert "Returns the current date as" <> _ = summary
      end

      assert %{
               active_param: 0,
               signatures: [
                 %{
                   documentation: summary1,
                   name: "cancel_timer",
                   params: ["timerRef"],
                   spec: "@spec cancel_timer(timerRef) :: result" <> _
                 },
                 %{
                   documentation: summary2,
                   name: "cancel_timer",
                   params: ["timerRef", "options"],
                   spec: "@spec cancel_timer(timerRef, options) :: result" <> _
                 }
               ]
             } = ElixirSense.signature(buffer, 4, 24)

      if ExUnitConfig.erlang_eep48_supported() do
        assert "Cancels a timer\\." <> _ = summary1
        assert "Cancels a timer that has been created by" <> _ = summary2
      end
    end
  end
end
