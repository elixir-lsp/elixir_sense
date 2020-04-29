defmodule ElixirSense.SuggestionsTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

  test "empty hint" do
    buffer = """
    defmodule MyModule do

    end
    """

    list = ElixirSense.suggestions(buffer, 2, 7)

    assert Enum.find(list, fn s -> match?(%{name: "import", arity: 2}, s) end) == %{
             args: "module, opts",
             arity: 2,
             name: "import",
             origin: "Kernel.SpecialForms",
             spec: "",
             summary: "Imports functions and macros from other modules.",
             type: :macro,
             metadata: %{}
           }

    assert Enum.find(list, fn s -> match?(%{name: "quote", arity: 2}, s) end) == %{
             arity: 2,
             origin: "Kernel.SpecialForms",
             spec: "",
             type: :macro,
             args: "opts, block",
             name: "quote",
             summary: "Gets the representation of any expression.",
             metadata: %{}
           }

    assert Enum.find(list, fn s -> match?(%{name: "require", arity: 2}, s) end) == %{
             arity: 2,
             origin: "Kernel.SpecialForms",
             spec: "",
             type: :macro,
             args: "module, opts",
             name: "require",
             summary: "Requires a module in order to use its macros.",
             metadata: %{}
           }
  end

  test "without empty hint" do
    buffer = """
    defmodule MyModule do
      is_b
    end
    """

    list = ElixirSense.suggestions(buffer, 2, 7)

    assert list == [
             %{type: :hint, value: "is_b"},
             %{
               args: "term",
               arity: 1,
               name: "is_binary",
               origin: "Kernel",
               spec: "@spec is_binary(term) :: boolean",
               summary: "Returns `true` if `term` is a binary; otherwise returns `false`.",
               type: :function,
               metadata: %{guard: true}
             },
             %{
               args: "term",
               arity: 1,
               name: "is_bitstring",
               origin: "Kernel",
               spec: "@spec is_bitstring(term) :: boolean",
               summary:
                 "Returns `true` if `term` is a bitstring (including a binary); otherwise returns `false`.",
               type: :function,
               metadata: %{guard: true}
             },
             %{
               args: "term",
               arity: 1,
               name: "is_boolean",
               origin: "Kernel",
               spec: "@spec is_boolean(term) :: boolean",
               summary:
                 "Returns `true` if `term` is either the atom `true` or the atom `false` (i.e.,\na boolean); otherwise returns `false`.",
               type: :function,
               metadata: %{guard: true}
             }
           ]
  end

  test "with an alias" do
    buffer = """
    defmodule MyModule do
      alias List, as: MyList
      MyList.flat
    end
    """

    list = ElixirSense.suggestions(buffer, 3, 14)

    assert list == [
             %{type: :hint, value: "MyList.flatten"},
             %{
               args: "list",
               arity: 1,
               name: "flatten",
               origin: "List",
               spec: "@spec flatten(deep_list) :: list when deep_list: [any | deep_list]",
               summary: "Flattens the given `list` of nested lists.",
               type: :function,
               metadata: %{}
             },
             %{
               args: "list, tail",
               arity: 2,
               name: "flatten",
               origin: "List",
               spec:
                 "@spec flatten(deep_list, [elem]) :: [elem] when deep_list: [elem | deep_list], elem: var",
               summary:
                 "Flattens the given `list` of nested lists.\nThe list `tail` will be added at the end of\nthe flattened list.",
               type: :function,
               metadata: %{}
             }
           ]
  end

  test "with a require" do
    buffer = """
    defmodule MyModule do
      require ElixirSenseExample.BehaviourWithMacrocallback.Impl, as: Macros
      Macros.so
    end
    """

    list = ElixirSense.suggestions(buffer, 3, 12)

    assert list == [
             %{type: :hint, value: "Macros.some"},
             %{
               args: "var",
               arity: 1,
               name: "some",
               origin: "ElixirSenseExample.BehaviourWithMacrocallback.Impl",
               spec: "@spec some(integer) :: Macro.t\n@spec some(b) :: Macro.t when b: float",
               summary: "some macro\n",
               type: :macro,
               metadata: %{}
             }
           ]
  end

  test "with a module hint" do
    buffer = """
    defmodule MyModule do
      ElixirSenseExample.ModuleWithD
    end
    """

    list = ElixirSense.suggestions(buffer, 2, 33)

    assert list == [
             %{type: :hint, value: "ElixirSenseExample.ModuleWithDoc"},
             %{
               name: "ModuleWithDocFalse",
               subtype: nil,
               summary: "",
               type: :module,
               metadata: %{}
             },
             %{
               name: "ModuleWithDocs",
               subtype: :behaviour,
               summary: "An example module\n",
               type: :module,
               metadata: %{since: "1.2.3"}
             }
           ]
  end

  test "lists callbacks" do
    buffer = """
    defmodule MyServer do
      use GenServer

    end
    """

    list =
      ElixirSense.suggestions(buffer, 3, 7)
      |> Enum.filter(fn s -> s.type == :callback && s.name == "code_change" end)

    assert [
             %{
               args: "old_vsn,state,extra",
               arity: 3,
               name: "code_change",
               origin: "GenServer",
               spec: "@callback code_change(old_vsn, state :: term, extra :: term) ::" <> _,
               summary:
                 "Invoked to change the state of the `GenServer` when a different version of a\nmodule is loaded (hot code swapping) and the state's term structure should be\nchanged.",
               type: :callback
             }
           ] = list
  end

  test "lists macrocallbacks" do
    buffer = """
    defmodule MyServer do
      @behaviour ElixirSenseExample.BehaviourWithMacrocallback

    end
    """

    list =
      ElixirSense.suggestions(buffer, 3, 7)
      |> Enum.filter(fn s -> s.type == :callback end)

    assert [
             %{
               args: "a",
               arity: 1,
               name: "optional",
               origin: "ElixirSenseExample.BehaviourWithMacrocallback",
               spec: "@macrocallback optional(a) :: Macro.t when a: atom",
               summary: "An optional macrocallback\n",
               type: :callback,
               metadata: %{optional: true}
             },
             %{
               args: "atom",
               arity: 1,
               name: "required",
               origin: "ElixirSenseExample.BehaviourWithMacrocallback",
               spec: "@macrocallback required(atom) :: Macro.t",
               summary: "A required macrocallback\n",
               type: :callback,
               metadata: %{optional: false}
             }
           ] == list
  end

  test "lists erlang callbacks" do
    buffer = """
    defmodule MyServer do
      @behaviour :gen_statem

    end
    """

    list =
      ElixirSense.suggestions(buffer, 3, 7)
      |> Enum.filter(fn s -> s.type == :callback && s.name == "code_change" end)

    assert [
             %{
               args: "oldVsn,oldState,oldData,extra",
               arity: 4,
               name: "code_change",
               origin: ":gen_statem",
               spec:
                 "@callback code_change(oldVsn :: term | {:down, term}, oldState :: state, oldData :: data, extra :: term) ::\n  {:ok, newState :: state, newData :: data} |\n  reason :: term",
               summary: "",
               type: :callback
             }
           ] = list
  end

  test "callback suggestions should not crash with unquote(__MODULE__)" do
    buffer = """
    defmodule Dummy do
      @doc false
      defmacro __using__() do
        quote location: :keep do
          @behaviour unquote(__MODULE__)
        end
      end
    end
    """

    assert [%{} | _] = ElixirSense.suggestions(buffer, 8, 5)
  end

  test "lists protocol functions" do
    buffer = """
    defimpl Enumerable, for: MyStruct do

    end
    """

    list =
      ElixirSense.suggestions(buffer, 2, 3)
      |> Enum.filter(fn s -> s[:name] == "reduce" end)

    assert [
             %{
               args: "enumerable, acc, fun",
               arity: 3,
               name: "reduce",
               origin: "Enumerable",
               spec: "@spec reduce(t, acc, reducer) :: result",
               summary: "Reduces the `enumerable` into an element.",
               type: :protocol_function,
               metadata: %{}
             }
           ] = list
  end

  test "lists function return values" do
    buffer = """
    defmodule MyServer do
      use ElixirSenseExample.ExampleBehaviour

      def handle_call(request, from, state) do

      end
    end
    """

    list =
      ElixirSense.suggestions(buffer, 5, 5)
      |> Enum.filter(fn s -> s.type == :return end)

    assert list == [
             %{
               description: "{:reply, reply, new_state}",
               snippet: "{:reply, \"${1:reply}$\", \"${2:new_state}$\"}",
               spec: "{:reply, reply, new_state} when reply: term, new_state: term, reason: term",
               type: :return
             },
             %{
               description:
                 "{:reply, reply, new_state, timeout | :hibernate | {:continue, term}}",
               snippet:
                 "{:reply, \"${1:reply}$\", \"${2:new_state}$\", \"${3:timeout | :hibernate | {:continue, term}}$\"}",
               spec:
                 "{:reply, reply, new_state, timeout | :hibernate | {:continue, term}} when reply: term, new_state: term, reason: term",
               type: :return
             },
             %{
               description: "{:noreply, new_state}",
               snippet: "{:noreply, \"${1:new_state}$\"}",
               spec: "{:noreply, new_state} when reply: term, new_state: term, reason: term",
               type: :return
             },
             %{
               description: "{:noreply, new_state, timeout | :hibernate | {:continue, term}}",
               snippet:
                 "{:noreply, \"${1:new_state}$\", \"${2:timeout | :hibernate | {:continue, term}}$\"}",
               spec:
                 "{:noreply, new_state, timeout | :hibernate | {:continue, term}} when reply: term, new_state: term, reason: term",
               type: :return
             },
             %{
               description: "{:stop, reason, reply, new_state}",
               snippet: "{:stop, \"${1:reason}$\", \"${2:reply}$\", \"${3:new_state}$\"}",
               spec:
                 "{:stop, reason, reply, new_state} when reply: term, new_state: term, reason: term",
               type: :return
             },
             %{
               description: "{:stop, reason, new_state}",
               snippet: "{:stop, \"${1:reason}$\", \"${2:new_state}$\"}",
               spec: "{:stop, reason, new_state} when reply: term, new_state: term, reason: term",
               type: :return
             }
           ]
  end

  test "lists macro return values" do
    buffer = """
    defmodule MyServer do
      @behaviour ElixirSenseExample.BehaviourWithMacrocallback

      defmacro required(arg) do

      end
    end
    """

    list =
      ElixirSense.suggestions(buffer, 5, 5)
      |> Enum.filter(fn s -> s.type == :return end)

    assert list == [
             %{
               description: "Macro.t",
               snippet: "\"${1:Macro.t}$\"",
               spec: "Macro.t",
               type: :return
             }
           ]
  end

  test "lists protocol implementation return values" do
    buffer = """
    defimpl Enumerable, for: MyStruct do
      def count(t) do

      end
    end
    """

    list =
      ElixirSense.suggestions(buffer, 3, 6)
      |> Enum.filter(fn s -> s.type == :return end)

    assert [
             %{
               description: "{:ok, non_neg_integer}",
               snippet: "{:ok, non_neg_integer()}",
               spec: "{:ok, non_neg_integer}",
               type: :return
             },
             %{
               description: "{:error, module}",
               snippet: "{:error, module()}",
               spec: "{:error, module}",
               type: :return
             }
           ] == list
  end

  test "lists function with spec return values" do
    buffer = """
    defmodule SomeModule do
      @spec count(atom) :: :ok | {:error, any}
      def count(t) do

      end
    end
    """

    list =
      ElixirSense.suggestions(buffer, 4, 6)
      |> Enum.filter(fn s -> s.type == :return end)

    assert [
             %{description: ":ok", snippet: ":ok", spec: ":ok", type: :return},
             %{
               description: "{:error, any}",
               snippet: "{:error, \"${1:any}$\"}",
               spec: "{:error, any}",
               type: :return
             }
           ] == list
  end

  test "lists params and vars" do
    buffer = """
    defmodule MyServer do
      use GenServer

      def handle_call(request, _from, state) do
        var1 = true

      end

      def init(arg), do: arg

      def handle_cast(arg, _state) when is_atom(arg) do
        :ok
      end
    end
    """

    list =
      ElixirSense.suggestions(buffer, 6, 5)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "request", type: :variable},
             %{name: "state", type: :variable},
             %{name: "var1", type: :variable}
           ]

    list =
      ElixirSense.suggestions(buffer, 9, 22)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "arg", type: :variable}
           ]

    list =
      ElixirSense.suggestions(buffer, 11, 45)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "arg", type: :variable}
           ]
  end

  test "lists params in fn's" do
    buffer = """
    defmodule MyServer do
      my = fn arg -> arg + 1 end
    end
    """

    list =
      ElixirSense.suggestions(buffer, 2, 19)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "arg", type: :variable}
           ]
  end

  test "lists params in protocol implementations" do
    buffer = """
    defimpl Enum, for: [MyStruct, MyOtherStruct] do
      def count(term), do:
    end
    """

    list =
      ElixirSense.suggestions(buffer, 2, 24)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "term", type: :variable}
           ]
  end

  test "lists vars in []" do
    buffer = """
    defmodule MyServer do
      my = %{}
      x = 4
      my[]

    end
    """

    list =
      ElixirSense.suggestions(buffer, 4, 6)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "my", type: :variable},
             %{name: "x", type: :variable}
           ]
  end

  test "lists vars in unfinished []" do
    buffer = """
    defmodule MyServer do
      my = %{}
      x = 4
      my[

    end
    """

    list =
      ElixirSense.suggestions(buffer, 4, 6)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "my", type: :variable},
             %{name: "x", type: :variable}
           ]
  end

  test "lists vars in string interpolation" do
    buffer = """
    defmodule MyServer do
      x = 4
      "abc\#{}"

    end
    """

    list =
      ElixirSense.suggestions(buffer, 3, 9)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "x", type: :variable}
           ]
  end

  test "lists vars in unfinished string interpolation" do
    buffer = """
    defmodule MyServer do
      x = 4
      "abc\#{

    end
    """

    list =
      ElixirSense.suggestions(buffer, 3, 9)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "x", type: :variable}
           ]

    buffer = """
    defmodule MyServer do
      x = 4
      "abc\#{"

    end
    """

    list =
      ElixirSense.suggestions(buffer, 3, 9)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "x", type: :variable}
           ]

    buffer = """
    defmodule MyServer do
      x = 4
      "abc\#{}

    end
    """

    list =
      ElixirSense.suggestions(buffer, 3, 9)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "x", type: :variable}
           ]

    buffer = """
    defmodule MyServer do
      x = 4
      "abc\#{x[

    end
    """

    list =
      ElixirSense.suggestions(buffer, 3, 9)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "x", type: :variable}
           ]
  end

  test "lists vars in heredoc interpolation" do
    buffer = """
    defmodule MyServer do
      x = 4
      \"\"\"
      abc\#{}
      \"\"\"

    end
    """

    list =
      ElixirSense.suggestions(buffer, 4, 8)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "x", type: :variable}
           ]
  end

  test "lists vars in unfinished heredoc interpolation" do
    # The 2 cases below are only supported on elixir >= 1.10
    # see https://github.com/elixir-lang/elixir/issues/9252

    if Version.match?(System.version(), ">= 1.10.0") do
      buffer = """
      defmodule MyServer do
        x = 4
        \"\"\"
        abc\#{
        \"\"\"

      end
      """

      list =
        ElixirSense.suggestions(buffer, 4, 8)
        |> Enum.filter(fn s -> s.type == :variable end)

      assert list == [
               %{name: "x", type: :variable}
             ]

      buffer = """
      defmodule MyServer do
        x = 4
        \"\"\"
        abc\#{

      end
      """

      list =
        ElixirSense.suggestions(buffer, 4, 8)
        |> Enum.filter(fn s -> s.type == :variable end)

      assert list == [
               %{name: "x", type: :variable}
             ]
    end

    buffer = """
    defmodule MyServer do
      x = 4
      \"\"\"
      abc\#{}

    end
    """

    list =
      ElixirSense.suggestions(buffer, 4, 8)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "x", type: :variable}
           ]
  end

  test "lists params in fn's not finished multiline" do
    buffer = """
    defmodule MyServer do
      my = fn arg ->

    end
    """

    assert capture_io(:stderr, fn ->
             list =
               ElixirSense.suggestions(buffer, 3, 5)
               |> Enum.filter(fn s -> s.type == :variable end)

             send(self(), {:result, list})
           end) =~ "an expression is always required on the right side of ->"

    assert_received {:result, list}

    assert list == [
             %{name: "arg", type: :variable},
             %{name: "my", type: :variable}
           ]
  end

  test "lists params in fn's not finished" do
    buffer = """
    defmodule MyServer do
      my = fn arg ->
    end
    """

    assert capture_io(:stderr, fn ->
             list =
               ElixirSense.suggestions(buffer, 2, 19)
               |> Enum.filter(fn s -> s.type == :variable end)

             send(self(), {:result, list})
           end) =~ "an expression is always required on the right side of ->"

    assert_received {:result, list}

    assert list == [
             %{name: "arg", type: :variable},
             # FIXME my is not defined, should not be in the list
             %{name: "my", type: :variable}
           ]
  end

  test "lists params in defs not finished" do
    buffer = """
    defmodule MyServer do
      def my(arg), do:
    end
    """

    list =
      ElixirSense.suggestions(buffer, 2, 20)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "arg", type: :variable}
           ]
  end

  test "lists params and vars in case clauses" do
    buffer = """
    defmodule MyServer do
      def fun(request) do
        case request do
          {:atom1, vara} ->
            :ok
          {:atom2, varb} -> :ok
          abc when is_atom(a)
        end

      end
    end
    """

    list =
      ElixirSense.suggestions(buffer, 5, 9)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "request", type: :variable},
             %{name: "vara", type: :variable}
           ]

    list =
      ElixirSense.suggestions(buffer, 6, 25)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "request", type: :variable},
             %{name: "varb", type: :variable}
           ]

    list =
      ElixirSense.suggestions(buffer, 9, 4)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "request", type: :variable}
           ]

    list =
      ElixirSense.suggestions(buffer, 7, 25)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "abc", type: :variable}
           ]
  end

  test "lists params and vars in cond clauses" do
    buffer = """
    defmodule MyServer do
      def fun(request) do
        cond do
          vara = Enum.find(request, 4) ->
            :ok
          varb = Enum.find(request, 5) -> :ok
          true -> :error
        end

      end
    end
    """

    list =
      ElixirSense.suggestions(buffer, 5, 9)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "request", type: :variable},
             %{name: "vara", type: :variable}
           ]

    list =
      ElixirSense.suggestions(buffer, 6, 39)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "request", type: :variable},
             %{name: "varb", type: :variable}
           ]

    list =
      ElixirSense.suggestions(buffer, 9, 4)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
             %{name: "request", type: :variable}
           ]
  end

  test "only list defined params in guard" do
    buffer = """
    defmodule MyServer do
      def new(my_var) when is_integer(my
    end
    """

    list =
      ElixirSense.suggestions(buffer, 2, 37)
      |> Enum.filter(fn s -> s.type in [:variable, :hint] end)

    assert list == [%{type: :hint, value: "my_var"}, %{name: "my_var", type: :variable}]
  end

  test "variable shadowing function" do
    buffer = """
    defmodule Mod do
      def my_fun(), do: :ok
      def some() do
        my_fun = 1
        my_f
      end
    end
    """

    assert [
             %{type: :hint, value: "my_fun"},
             %{name: "my_fun", type: :variable},
             %{name: "my_fun", type: :function}
           ] = ElixirSense.suggestions(buffer, 5, 9)
  end

  test "lists attributes" do
    buffer = """
    defmodule MyModule do
      @my_attribute1 true
      @my_attribute2 false
      @
    end
    """

    list =
      ElixirSense.suggestions(buffer, 4, 4)
      |> Enum.filter(fn s -> s.type == :attribute and s.name |> String.starts_with?("@my") end)

    assert list == [
             %{name: "@my_attribute1", type: :attribute},
             %{name: "@my_attribute2", type: :attribute}
           ]
  end

  test "lists module attributes in module scope" do
    buffer = """
    defmodule MyModule do
      @myattr "asd"
      @moduledoc "asdf"
      def some do
        @m
      end
    end
    """

    list =
      ElixirSense.suggestions(buffer, 2, 5)
      |> Enum.filter(fn s -> s.type == :attribute end)

    assert list == [
             %{name: "@macrocallback", type: :attribute},
             %{name: "@moduledoc", type: :attribute},
             %{name: "@myattr", type: :attribute}
           ]

    list =
      ElixirSense.suggestions(buffer, 5, 7)
      |> Enum.filter(fn s -> s.type == :attribute end)

    assert list == [
             %{name: "@myattr", type: :attribute}
           ]
  end

  test "functions defined in the module" do
    buffer = """
    defmodule ElixirSenseExample.ModuleA do
      def test_fun_pub(a), do: :ok

      def some_fun() do
        te
        a = &test_fun_pr
        is_bo
        del
        my_
      end

      defp test_fun_priv(), do: :ok
      defp is_boo_overlaps_kernel(), do: :ok
      defdelegate delegate_defined, to: Kernel, as: :is_binary
      defdelegate delegate_not_defined, to: Dummy, as: :hello
      defguard my_guard_pub(value) when is_integer(value) and rem(value, 2) == 0
      defguardp my_guard_priv(value) when is_integer(value)
      defmacro some_macro(a) do
        quote do: :ok
      end
    end
    """

    assert [
             %{type: :hint, value: "test_fun_p"},
             %{
               arity: 0,
               name: "test_fun_priv",
               origin: "ElixirSenseExample.ModuleA",
               type: :function
             },
             %{
               arity: 1,
               name: "test_fun_pub",
               origin: "ElixirSenseExample.ModuleA",
               type: :function
             }
           ] = ElixirSense.suggestions(buffer, 5, 7)

    assert [
             %{type: :hint, value: "test_fun_priv"},
             %{
               arity: 0,
               name: "test_fun_priv",
               origin: "ElixirSenseExample.ModuleA",
               type: :function
             }
           ] = ElixirSense.suggestions(buffer, 6, 21)

    assert [
             %{type: :hint, value: "is_boo"},
             %{
               arity: 1,
               name: "is_boolean",
               origin: "Kernel",
               type: :function
             },
             %{
               arity: 0,
               name: "is_boo_overlaps_kernel",
               origin: "ElixirSenseExample.ModuleA",
               type: :function
             }
           ] = ElixirSense.suggestions(buffer, 7, 10)

    assert [
             %{type: :hint, value: "delegate_"},
             %{
               arity: 0,
               name: "delegate_defined",
               origin: "ElixirSenseExample.ModuleA",
               type: :function
             },
             %{
               arity: 0,
               name: "delegate_not_defined",
               origin: "ElixirSenseExample.ModuleA",
               type: :function
             }
           ] = ElixirSense.suggestions(buffer, 8, 8)

    assert [
             %{type: :hint, value: "my_guard_p"},
             %{
               args: "value",
               arity: 1,
               name: "my_guard_priv",
               origin: "ElixirSenseExample.ModuleA",
               spec: "",
               summary: "",
               type: :macro
             },
             %{
               args: "value",
               arity: 1,
               name: "my_guard_pub",
               origin: "ElixirSenseExample.ModuleA",
               spec: "",
               summary: "",
               type: :macro
             }
           ] = ElixirSense.suggestions(buffer, 9, 8)
  end

  test "functions defined in other module fully qualified" do
    buffer = """
    defmodule ElixirSenseExample.ModuleO do
      def test_fun_pub(a), do: :ok
      defp test_fun_priv(), do: :ok
    end

    defmodule ElixirSenseExample.ModuleA do
      def some_fun() do
        ElixirSenseExample.ModuleO.te
      end
    end
    """

    assert [
             %{type: :hint, value: "ElixirSenseExample.ModuleO.test_fun_pub"},
             %{
               arity: 1,
               name: "test_fun_pub",
               origin: "ElixirSenseExample.ModuleO",
               type: :function
             }
           ] = ElixirSense.suggestions(buffer, 8, 34)
  end

  test "functions defined in other module aliased" do
    buffer = """
    defmodule ElixirSenseExample.ModuleO do
      def test_fun_pub(a), do: :ok
      defp test_fun_priv(), do: :ok
    end

    defmodule ElixirSenseExample.ModuleA do
      alias ElixirSenseExample.ModuleO
      def some_fun() do
        ModuleO.te
      end
    end
    """

    assert [
             %{type: :hint, value: "ModuleO.test_fun_pub"},
             %{
               arity: 1,
               name: "test_fun_pub",
               origin: "ElixirSenseExample.ModuleO",
               type: :function
             }
           ] = ElixirSense.suggestions(buffer, 9, 15)
  end

  test "functions defined in other module imported" do
    buffer = """
    defmodule ElixirSenseExample.ModuleO do
      @spec test_fun_pub(integer) :: atom
      def test_fun_pub(a), do: :ok
      defp test_fun_priv(), do: :ok
    end

    defmodule ElixirSenseExample.ModuleA do
      import ElixirSenseExample.ModuleO
      def some_fun() do
        te
        __i
      end
    end
    """

    assert [
             %{type: :hint, value: "test_fun_pub"},
             %{
               arity: 1,
               name: "test_fun_pub",
               origin: "ElixirSenseExample.ModuleO",
               type: :function,
               args: "a",
               spec: "@spec test_fun_pub(integer) :: atom",
               summary: "",
               metadata: %{}
             }
           ] == ElixirSense.suggestions(buffer, 10, 7)

    # builtin functions not called locally
    assert [%{type: :hint, value: "__i"}] == ElixirSense.suggestions(buffer, 11, 8)
  end

  test "functions and module suggestions with __MODULE__" do
    buffer = """
    defmodule ElixirSenseExample.SmodO do
      def test_fun_pub(a), do: :ok
      defp test_fun_priv(), do: :ok
    end

    defmodule ElixirSenseExample do
      defp test_fun_priv1(a), do: :ok
      def some_fun() do
        __MODULE__.Sm
        __MODULE__.SmodO.te
        __MODULE__.te
        __MODULE__.__in
      end
    end
    """

    assert [
             %{type: :hint, value: "__MODULE__.SmodO"},
             %{
               name: "SmodO",
               type: :module
             }
           ] = ElixirSense.suggestions(buffer, 9, 18)

    assert [
             %{type: :hint, value: "__MODULE__.SmodO.test_fun_pub"},
             %{
               arity: 1,
               name: "test_fun_pub",
               origin: "ElixirSenseExample.SmodO",
               type: :function
             }
           ] = ElixirSense.suggestions(buffer, 10, 24)

    # no private on external call
    assert [%{type: :hint, value: "__MODULE__.te"}] = ElixirSense.suggestions(buffer, 11, 18)

    assert [
             %{type: :hint, value: "__MODULE__.__info__"},
             %{
               arity: 1,
               name: "__info__",
               origin: "ElixirSenseExample",
               type: :function
             }
           ] = ElixirSense.suggestions(buffer, 12, 20)
  end

  test "Elixir module" do
    buffer = """
    defmodule MyModule do
      El
    end
    """

    list = ElixirSense.suggestions(buffer, 2, 5)

    assert Enum.at(list, 0) == %{type: :hint, value: "Elixir"}

    assert Enum.at(list, 1) == %{
             type: :module,
             name: "Elixir",
             subtype: nil,
             summary: "",
             metadata: %{}
           }
  end

  test "suggestion for aliases modules defined by require clause" do
    buffer = """
    defmodule Mod do
      require Integer, as: I
      I.is_o
    end
    """

    list = ElixirSense.suggestions(buffer, 3, 9)
    assert Enum.at(list, 1).name == "is_odd"
  end

  test "suggestion for struct fields" do
    buffer = """
    defmodule Mod do
      %IO.Stream{}
      %ArgumentError{}
    end
    """

    list =
      ElixirSense.suggestions(buffer, 2, 14)
      |> Enum.filter(&(&1.type in [:field, :hint]))

    assert list == [
             %{type: :hint, value: ""},
             %{
               name: "device",
               origin: "IO.Stream",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "line_or_bytes",
               origin: "IO.Stream",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "raw",
               origin: "IO.Stream",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "__struct__",
               origin: "IO.Stream",
               type: :field,
               call?: false,
               subtype: :struct_field
             }
           ]

    list =
      ElixirSense.suggestions(buffer, 3, 18)
      |> Enum.filter(&(&1.type in [:field, :hint]))

    assert list == [
             %{type: :hint, value: ""},
             %{
               name: "__exception__",
               origin: "ArgumentError",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "message",
               origin: "ArgumentError",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "__struct__",
               origin: "ArgumentError",
               type: :field,
               call?: false,
               subtype: :struct_field
             }
           ]
  end

  test "suggestion for aliased struct fields" do
    buffer = """
    defmodule Mod do
      alias IO.Stream
      %Stream{
    end
    """

    list =
      ElixirSense.suggestions(buffer, 3, 11)
      |> Enum.filter(&(&1.type in [:field, :hint]))

    assert list == [
             %{type: :hint, value: ""},
             %{
               name: "device",
               origin: "IO.Stream",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "line_or_bytes",
               origin: "IO.Stream",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "raw",
               origin: "IO.Stream",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "__struct__",
               origin: "IO.Stream",
               type: :field,
               call?: false,
               subtype: :struct_field
             }
           ]
  end

  test "suggestion for builtin fields in struct pattern match" do
    buffer = """
    defmodule Mod do
      def my(%_{}), do: :ok
      def my(%var{}), do: var
    end
    """

    list =
      ElixirSense.suggestions(buffer, 2, 13)
      |> Enum.filter(&(&1.type in [:field, :hint]))

    assert list == [
             %{type: :hint, value: ""},
             %{
               name: "__struct__",
               origin: nil,
               type: :field,
               call?: false,
               subtype: :struct_field
             }
           ]

    list =
      ElixirSense.suggestions(buffer, 3, 15)
      |> Enum.filter(&(&1.type in [:field, :hint]))

    assert list == [
             %{type: :hint, value: ""},
             %{
               name: "__struct__",
               origin: nil,
               type: :field,
               call?: false,
               subtype: :struct_field
             }
           ]
  end

  test "suggestion for aliased struct fields atom module" do
    buffer = """
    defmodule Mod do
      alias IO.Stream
      %:"Elixir.Stream"{
    end
    """

    list =
      ElixirSense.suggestions(buffer, 3, 21)
      |> Enum.filter(&(&1.type in [:field, :hint]))

    assert list == [
             %{type: :hint, value: ""},
             %{
               name: "device",
               origin: "IO.Stream",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "line_or_bytes",
               origin: "IO.Stream",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "raw",
               origin: "IO.Stream",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "__struct__",
               origin: "IO.Stream",
               type: :field,
               call?: false,
               subtype: :struct_field
             }
           ]
  end

  test "suggestion for metadata struct fields" do
    buffer = """
    defmodule MyServer do
      defstruct [
        field_1: nil,
        field_2: ""
      ]

      def func do
        %MyServer{}
        %MyServer{field_2: "2", }
      end
    end
    """

    list =
      ElixirSense.suggestions(buffer, 8, 15)
      |> Enum.filter(&(&1.type in [:field, :hint]))

    assert list == [
             %{type: :hint, value: ""},
             %{
               name: "field_1",
               origin: "MyServer",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "field_2",
               origin: "MyServer",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "__struct__",
               origin: "MyServer",
               type: :field,
               call?: false,
               subtype: :struct_field
             }
           ]

    list = ElixirSense.suggestions(buffer, 9, 28)

    assert list == [
             %{type: :hint, value: ""},
             %{
               name: "field_1",
               origin: "MyServer",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "__struct__",
               origin: "MyServer",
               type: :field,
               call?: false,
               subtype: :struct_field
             }
           ]
  end

  test "suggestion for metadata struct fields atom module" do
    buffer = """
    defmodule :my_server do
      defstruct [
        field_1: nil,
        field_2: ""
      ]

      def func do
        %:my_server{}
        %:my_server{field_2: "2", }
      end
    end
    """

    list =
      ElixirSense.suggestions(buffer, 8, 17)
      |> Enum.filter(&(&1.type in [:field, :hint]))

    assert list == [
             %{type: :hint, value: ""},
             %{
               name: "field_1",
               origin: ":my_server",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "field_2",
               origin: ":my_server",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "__struct__",
               origin: ":my_server",
               type: :field,
               call?: false,
               subtype: :struct_field
             }
           ]

    list = ElixirSense.suggestions(buffer, 9, 30)

    assert list == [
             %{type: :hint, value: ""},
             %{
               name: "field_1",
               origin: ":my_server",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "__struct__",
               origin: ":my_server",
               type: :field,
               call?: false,
               subtype: :struct_field
             }
           ]
  end

  test "suggestion for metadata struct fields multiline" do
    buffer = """
    defmodule MyServer do
      defstruct [
        field_1: nil,
        field_2: ""
      ]

      def func do
        %MyServer{
          field_2: "2",

        }
      end
    end
    """

    list = ElixirSense.suggestions(buffer, 10, 7)

    assert list == [
             %{type: :hint, value: ""},
             %{
               name: "field_1",
               origin: "MyServer",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "__struct__",
               origin: "MyServer",
               type: :field,
               call?: false,
               subtype: :struct_field
             }
           ]
  end

  test "suggestion for metadata struct fields when using `__MODULE__`" do
    buffer = """
    defmodule MyServer do
      defstruct [
        field_1: nil,
        field_2: ""
      ]

      def func do
        %__MODULE__{field_2: "2", }
      end
    end
    """

    list = ElixirSense.suggestions(buffer, 8, 31)

    assert list == [
             %{type: :hint, value: ""},
             %{
               name: "field_1",
               origin: "MyServer",
               type: :field,
               call?: false,
               subtype: :struct_field
             },
             %{
               name: "__struct__",
               origin: "MyServer",
               type: :field,
               call?: false,
               subtype: :struct_field
             }
           ]
  end

  test "suggestion for struct fields in variable.key call syntax" do
    buffer = """
    defmodule MyServer do
      defstruct [
        field_1: nil,
        field_2: ""
      ]

      def func do
        var_1 = %MyServer{}
        var_1.f
      end
    end
    """

    list =
      ElixirSense.suggestions(buffer, 9, 12)
      |> Enum.filter(&(&1.type in [:field, :hint]))

    assert list == [
             %{type: :hint, value: "var_1.field_"},
             %{
               name: "field_1",
               origin: "MyServer",
               type: :field,
               call?: true,
               subtype: :struct_field
             },
             %{
               name: "field_2",
               origin: "MyServer",
               type: :field,
               call?: true,
               subtype: :struct_field
             }
           ]
  end

  test "suggestion for map fields in variable.key call syntax" do
    buffer = """
    defmodule MyServer do
      def func do
        var_1 = %{key_1: 1, key_2: %{abc: 123}}
        var_1.k
      end
    end
    """

    list =
      ElixirSense.suggestions(buffer, 4, 12)
      |> Enum.filter(&(&1.type in [:field, :hint]))

    assert list == [
             %{type: :hint, value: "var_1.key_"},
             %{name: "key_1", origin: nil, type: :field, call?: true, subtype: :map_key},
             %{name: "key_2", origin: nil, type: :field, call?: true, subtype: :map_key}
           ]
  end

  test "suggestion for map fields in @attribute.key call syntax" do
    buffer = """
    defmodule MyServer do
      @var_1 %{key_1: 1, key_2: %{abc: 123}}
      def func do
        @var_1.k
      end
    end
    """

    list =
      ElixirSense.suggestions(buffer, 4, 13)
      |> Enum.filter(&(&1.type in [:field, :hint]))

    assert list == [
             %{type: :hint, value: "@var_1.key_"},
             %{name: "key_1", origin: nil, type: :field, call?: true, subtype: :map_key},
             %{name: "key_2", origin: nil, type: :field, call?: true, subtype: :map_key}
           ]
  end

  test "suggestion for functions in variable.key call syntax" do
    buffer = """
    defmodule MyServer do
      def func do
        var_1 = Atom
        var_1.to_s
      end
    end
    """

    list =
      ElixirSense.suggestions(buffer, 4, 15)
      |> Enum.filter(&(&1.type in [:function, :hint]))

    assert [
             %{type: :hint, value: "var_1.to_string"},
             %{name: "to_string", origin: "Atom", type: :function}
           ] = list
  end

  test "suggestion for vars in struct update" do
    buffer = """
    defmodule MyServer do
      defstruct [
        field_1: nil,
        some_field: ""
      ]

      def func(%MyServer{} = some_arg) do
        %MyServer{so
      end
    end
    """

    list = ElixirSense.suggestions(buffer, 8, 17)

    assert list == [
             %{type: :hint, value: "so"},
             %{
               origin: "MyServer",
               type: :field,
               name: "some_field",
               call?: false,
               subtype: :struct_field
             },
             %{name: "some_arg", type: :variable}
           ]
  end

  test "suggestion for fields in struct update" do
    buffer = """
    defmodule MyServer do
      defstruct [
        field_1: nil,
        some_field: ""
      ]

      def func(%MyServer{} = some_arg) do
        %MyServer{some_arg | fi
      end
    end
    """

    list = ElixirSense.suggestions(buffer, 8, 28)

    assert list == [
             %{type: :hint, value: "fi"},
             %{
               call?: false,
               name: "field_1",
               origin: "MyServer",
               subtype: :struct_field,
               type: :field
             }
           ]
  end

  test "suggestion for fields in struct update when module not set" do
    buffer = """
    defmodule MyServer do
      defstruct [
        field_1: nil,
        some_field: ""
      ]

      def func(%MyServer{} = some_arg) do
        %{some_arg | fi
      end
    end
    """

    list = ElixirSense.suggestions(buffer, 8, 20)

    assert list == [
             %{type: :hint, value: "fi"},
             %{
               call?: false,
               name: "field_1",
               origin: "MyServer",
               subtype: :struct_field,
               type: :field
             }
           ]
  end

  test "suggestion for fields in struct update when struct type is var" do
    buffer = """
    defmodule MyServer do
      def func(%var{field_1: "asd"} = some_arg) do
        %{some_arg | fi
      end
    end
    """

    list = ElixirSense.suggestions(buffer, 3, 20)

    assert list == [
             %{type: :hint, value: "fi"},
             %{call?: false, name: "field_1", origin: nil, subtype: :struct_field, type: :field}
           ]
  end

  test "suggestion for keys in map update" do
    buffer = """
    defmodule MyServer do
      def func(%{field_1: "asd"} = some_arg) do
        %{some_arg | fi
      end
    end
    """

    list = ElixirSense.suggestions(buffer, 3, 20)

    assert list == [
             %{type: :hint, value: "fi"},
             %{call?: false, name: "field_1", origin: nil, subtype: :map_key, type: :field}
           ]
  end

  test "suggestion for funcs and vars in struct" do
    buffer = """
    defmodule MyServer do
      defstruct [
        field_1: nil,
        some_field: ""
      ]

      def other_func(), do: :ok

      def func(%MyServer{} = some_arg, other_arg) do
        %MyServer{some_arg |
          field_1: ot
      end
    end
    """

    list = ElixirSense.suggestions(buffer, 11, 18)

    assert list == [
             %{type: :hint, value: "other_"},
             %{name: "other_arg", type: :variable},
             %{
               name: "other_func",
               type: :function,
               args: "",
               arity: 0,
               origin: "MyServer",
               spec: "",
               summary: "",
               metadata: %{}
             }
           ]
  end

  test "no suggestion of fields when the module is not a struct" do
    buffer = """
    defmodule Mod do
      %Enum{
    end
    """

    list = ElixirSense.suggestions(buffer, 2, 9)
    assert Enum.any?(list, fn %{type: type} -> type == :field end) == false
  end

  test "suggest modules to alias" do
    buffer = """
    defmodule MyModule do
      alias Str
    end
    """

    list =
      ElixirSense.suggestions(buffer, 2, 12)
      |> Enum.filter(fn s -> s.type == :module end)

    assert [
             %{name: "Stream"},
             %{name: "String"},
             %{name: "StringIO"}
           ] = list
  end

  test "suggest modules to alias with __MODULE__" do
    buffer = """
    defmodule Stream do
      alias __MODULE__.Re
    end
    """

    list = ElixirSense.suggestions(buffer, 2, 22)

    assert [%{type: :hint, value: "__MODULE__.Reducers"}, %{name: "Reducers", type: :module} | _] =
             list
  end

  test "suggest modules to alias v1.2 syntax" do
    buffer = """
    defmodule MyModule do
      alias Stream.{Re
    end
    """

    list = ElixirSense.suggestions(buffer, 2, 19)

    assert [%{type: :hint, value: "Reducers"}, %{name: "Reducers", type: :module}] = list
  end

  test "suggest modules to alias v1.2 syntax with __MODULE__" do
    buffer = """
    defmodule Stream do
      alias __MODULE__.{Re
    end
    """

    list = ElixirSense.suggestions(buffer, 2, 23)

    assert [%{type: :hint, value: "Reducers"}, %{name: "Reducers", type: :module}] = list
  end

  describe "suggestion for param options" do
    test "suggest more than one option" do
      buffer = "Local.func_with_options("

      list = suggestions_by_type(:param_option, buffer)
      assert length(list) > 1
    end

    test "suggest the same list when options are already set" do
      buffer1 = "Local.func_with_options("
      buffer2 = "Local.func_with_options(local_o: :an_atom, "

      assert capture_io(:stderr, fn ->
               result1 = suggestions_by_type(:param_option, buffer1)
               result2 = suggestions_by_type(:param_option, buffer2)
               send(self(), {:results, result1, result2})
             end) =~ "trailing commas are not allowed inside function/macro"

      assert_received {:results, result1, result2}
      assert result1 == result2
    end

    test "options as inline list" do
      buffer = "Local.func_with_options_as_inline_list("

      assert %{type_spec: "local_t()", expanded_spec: "@type local_t() :: atom()"} =
               suggestion_by_name(:local_o, buffer)

      assert %{
               type_spec: "keyword()",
               expanded_spec: """
               @type keyword() :: [
                 {atom(), any()}
               ]\
               """
             } = suggestion_by_name(:builtin_o, buffer)
    end

    test "options vars defined in when" do
      type_spec = "local_t()"
      origin = "ElixirSenseExample.ModuleWithTypespecs.Local"
      spec = "@type local_t() :: atom()"

      buffer = "Local.func_with_option_var_defined_in_when("
      suggestion = suggestion_by_name(:local_o, buffer)

      assert suggestion.type_spec == type_spec
      assert suggestion.origin == origin
      assert suggestion.expanded_spec == spec

      buffer = "Local.func_with_options_var_defined_in_when("
      suggestion = suggestion_by_name(:local_o, buffer)

      assert suggestion.type_spec == type_spec
      assert suggestion.origin == origin
      assert suggestion.expanded_spec == spec
    end

    test "opaque type internal structure is not revealed" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:opaque_o, buffer)

      assert suggestion.type_spec == "opaque_t()"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Local"
      assert suggestion.expanded_spec == "@opaque opaque_t()"
      assert suggestion.doc == "Local opaque type"
    end

    test "private type" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:private_o, buffer)

      assert suggestion.type_spec == "private_t()"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Local"
      assert suggestion.expanded_spec == "@typep private_t() :: atom()"
      assert suggestion.doc == ""
    end

    test "local type" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:local_o, buffer)

      assert suggestion.type_spec == "local_t()"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Local"
      assert suggestion.expanded_spec == "@type local_t() :: atom()"
      assert suggestion.doc == "Local type"
    end

    test "local type with params" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:local_with_params_o, buffer)

      assert suggestion.type_spec == "local_t(atom(), integer())"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Local"
      assert suggestion.expanded_spec == "@type local_t(a, b) :: {a, b}"
    end

    test "basic type" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:basic_o, buffer)

      assert suggestion.type_spec == "pid()"
      assert suggestion.origin == ""
      assert suggestion.expanded_spec == ""
      assert suggestion.doc == "A process identifier, pid, identifies a process"
    end

    test "basic type with params" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:basic_with_params_o, buffer)

      assert suggestion.type_spec == "[atom(), ...]"
      assert suggestion.origin == ""
      assert suggestion.expanded_spec == ""
      assert suggestion.doc == "Non-empty proper list"
    end

    test "built-in type" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:builtin_o, buffer)

      assert suggestion.type_spec == "keyword()"
      assert suggestion.origin == ""

      assert suggestion.expanded_spec == """
             @type keyword() :: [
               {atom(), any()}
             ]\
             """

      assert suggestion.doc == "A keyword list"
    end

    test "built-in type with params" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:builtin_with_params_o, buffer)

      assert suggestion.type_spec == "keyword(term())"
      assert suggestion.origin == ""
      assert suggestion.expanded_spec == "@type keyword(t) :: [{atom(), t}]"
      assert suggestion.doc == "A keyword list with values of type `t`"
    end

    test "union type" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:union_o, buffer)

      assert suggestion.type_spec == "union_t()"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Local"

      assert suggestion.expanded_spec == """
             @type union_t() ::
               atom() | integer()\
             """
    end

    test "list type" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:list_o, buffer)

      assert suggestion.type_spec == "list_t()"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Local"
      assert suggestion.expanded_spec == "@type list_t() :: [:trace | :log]"
    end

    test "remote type" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:remote_o, buffer)

      assert suggestion.type_spec == "ElixirSenseExample.ModuleWithTypespecs.Remote.remote_t()"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Remote"
      assert suggestion.expanded_spec == "@type remote_t() :: atom()"
      assert suggestion.doc == "Remote type"
    end

    test "remote type with args" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:remote_with_params_o, buffer)

      assert suggestion.type_spec ==
               "ElixirSenseExample.ModuleWithTypespecs.Remote.remote_t(atom(), integer())"

      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Remote"
      assert suggestion.expanded_spec == "@type remote_t(a, b) :: {a, b}"
      assert suggestion.doc == "Remote type with params"
    end

    test "remote erlang type with edoc" do
      buffer = "Local.func_with_edoc_options("
      suggestion = suggestion_by_name(:edoc_t, buffer)

      assert suggestion.type_spec ==
               ":docsh_edoc_xmerl.xml_element_content()"

      assert suggestion.origin == ":docsh_edoc_xmerl"

      assert suggestion.expanded_spec ==
               "@type xml_element_content() :: [\n  record(:xmlElement)\n  | record(:xmlText)\n  | record(:xmlPI)\n  | record(:xmlComment)\n  | record(:xmlDecl)\n]"

      assert suggestion.doc == "#xmlElement.content as defined by xmerl.hrl.\n\n"
    end

    test "remote aliased type" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:remote_aliased_o, buffer)

      assert suggestion.type_spec == "remote_aliased_t()"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Local"

      assert suggestion.expanded_spec == """
             @type remote_aliased_t() ::
               ElixirSenseExample.ModuleWithTypespecs.Remote.remote_t()
               | ElixirSenseExample.ModuleWithTypespecs.Remote.remote_list_t()\
             """

      assert suggestion.doc == "Remote type from aliased module"
    end

    test "remote aliased inline type" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:remote_aliased_inline_o, buffer)

      assert suggestion.type_spec == "ElixirSenseExample.ModuleWithTypespecs.Remote.remote_t()"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Remote"
      assert suggestion.expanded_spec == "@type remote_t() :: atom()"
      assert suggestion.doc == "Remote type"
    end

    test "inline list type" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:inline_list_o, buffer)

      assert suggestion.type_spec == "[:trace | :log]"
      assert suggestion.origin == ""
      assert suggestion.expanded_spec == ""
      assert suggestion.doc == ""
    end

    test "non existent type" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:non_existent_o, buffer)

      assert suggestion.type_spec ==
               "ElixirSenseExample.ModuleWithTypespecs.Remote.non_existent()"

      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Remote"
      assert suggestion.expanded_spec == ""
      assert suggestion.doc == ""
    end

    test "named options" do
      buffer = "Local.func_with_named_options("
      assert suggestion_by_name(:local_o, buffer).type_spec == "local_t()"
    end

    test "options with only one option" do
      buffer = "Local.func_with_one_option("
      assert suggestion_by_name(:option_1, buffer).type_spec == "integer()"
    end

    test "union of options" do
      buffer = "Local.func_with_union_of_options("

      assert suggestion_by_name(:local_o, buffer).type_spec == "local_t()"
      assert suggestion_by_name(:option_1, buffer).type_spec == "atom()"
    end

    test "union of options inline" do
      buffer = "Local.func_with_union_of_options_inline("

      assert suggestion_by_name(:local_o, buffer).type_spec == "local_t()"
      assert suggestion_by_name(:option_1, buffer).type_spec == "atom()"
    end

    test "union of options (local and remote) as type + inline" do
      buffer = "Local.func_with_union_of_options_as_type("
      assert suggestion_by_name(:option_1, buffer).type_spec == "boolean()"

      suggestion = suggestion_by_name(:remote_option_1, buffer)
      assert suggestion.type_spec == "ElixirSenseExample.ModuleWithTypespecs.Remote.remote_t()"
      assert suggestion.expanded_spec == "@type remote_t() :: atom()"
      assert suggestion.doc == "Remote type"
    end

    test "atom only options" do
      buffer = ":ets.new(:name,"

      assert suggestion_by_name(:duplicate_bag, buffer).type_spec == ""
      assert suggestion_by_name(:named_table, buffer).doc == ""
    end

    test "format type spec" do
      buffer = "Local.func_with_options("

      assert suggestion_by_name(:large_o, buffer).expanded_spec == """
             @type large_t() ::
               pid()
               | port()
               | (registered_name ::
                    atom())
               | {registered_name ::
                    atom(), node()}\
             """
    end
  end

  describe "suggestions for typespecs" do
    test "remote types - filter list of typespecs" do
      buffer = """
      defmodule My do
        Remote.remote_t\
      """

      list = suggestions_by_type(:type_spec, buffer)
      assert length(list) == 2
    end

    test "remote types - retrieve info from typespecs" do
      buffer = """
      defmodule My do
        Remote.\
      """

      suggestion = suggestion_by_name(:remote_list_t, buffer)

      assert suggestion.spec == """
             @type remote_list_t() :: [
               remote_t()
             ]\
             """

      assert suggestion.signature == "remote_list_t()"
      assert suggestion.arity == 0
      assert suggestion.doc == "Remote list type"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Remote"
    end

    test "remote types - retrieve info from typespecs with params" do
      buffer = """
      defmodule My do
        Remote.\
      """

      [suggestion_1, suggestion_2] = suggestions_by_name(:remote_t, buffer)

      assert suggestion_1.spec == "@type remote_t() :: atom()"
      assert suggestion_1.signature == "remote_t()"
      assert suggestion_1.arity == 0
      assert suggestion_1.doc == "Remote type"
      assert suggestion_1.origin == "ElixirSenseExample.ModuleWithTypespecs.Remote"

      assert suggestion_2.spec == "@type remote_t(a, b) :: {a, b}"
      assert suggestion_2.signature == "remote_t(a, b)"
      assert suggestion_2.arity == 2
      assert suggestion_2.doc == "Remote type with params"
      assert suggestion_2.origin == "ElixirSenseExample.ModuleWithTypespecs.Remote"
    end

    test "local types - filter list of typespecs" do
      buffer = """
      defmodule ElixirSenseExample.ModuleWithTypespecs.Local do
        # The types are defined in `test/support/module_with_typespecs.ex`
        @type my_type :: local_
        #                      ^
      end
      """

      list =
        ElixirSense.suggestions(buffer, 3, 26)
        |> Enum.filter(fn %{type: t} -> t == :type_spec end)

      assert length(list) == 2
    end

    test "local types - retrieve info from typespecs" do
      buffer = """
      defmodule ElixirSenseExample.ModuleWithTypespecs.Local do
        # The types are defined in `test/support/module_with_typespecs.ex`
        @type my_type :: local_t
        #                       ^
      end
      """

      list =
        ElixirSense.suggestions(buffer, 3, 27)
        |> Enum.filter(fn %{type: t} -> t == :type_spec end)

      [suggestion, _] = list

      assert suggestion.spec == "@type local_t() :: atom()"
      assert suggestion.signature == "local_t()"
      assert suggestion.arity == 0
      assert suggestion.doc == "Local type"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Local"
    end

    test "builtin types - filter list of typespecs" do
      buffer = "defmodule My, do: @type my_type :: lis"

      list = suggestions_by_type(:type_spec, buffer)
      assert length(list) == 2
    end

    test "builtin types - retrieve info from typespecs" do
      buffer = "defmodule My, do: @type my_type :: lis"

      [suggestion | _] = suggestions_by_type(:type_spec, buffer)

      assert suggestion.spec == "@type list() :: [any()]"
      assert suggestion.signature == "list()"
      assert suggestion.arity == 0
      assert suggestion.doc == "A list"
      assert suggestion.origin == ""
    end

    test "builtin types - retrieve info from typespecs with params" do
      buffer = "defmodule My, do: @type my_type :: lis"

      [_, suggestion | _] = suggestions_by_type(:type_spec, buffer)

      assert suggestion.spec == ""
      assert suggestion.signature == "list(t)"
      assert suggestion.arity == 1
      assert suggestion.doc == "Proper list ([]-terminated)"
      assert suggestion.origin == ""
    end

    test "erlang types" do
      buffer = "defmodule My, do: @type my_type :: :erlang.tim"

      suggestions = suggestions_by_type(:type_spec, buffer)

      assert [
               %{
                 arity: 0,
                 doc: "",
                 name: :time_unit,
                 origin: ":erlang",
                 signature: "time_unit()",
                 spec:
                   "@type time_unit() ::\n  pos_integer()\n  | :second\n  | :millisecond\n  | :microsecond\n  | :nanosecond\n  | :native\n  | :perf_counter\n  | deprecated_time_unit()",
                 type: :type_spec,
                 metadata: %{}
               },
               %{
                 arity: 0,
                 doc: "",
                 name: :timestamp,
                 origin: ":erlang",
                 signature: "timestamp()",
                 spec:
                   "@type timestamp() ::\n  {megaSecs ::\n     non_neg_integer(),\n   secs :: non_neg_integer(),\n   microSecs ::\n     non_neg_integer()}",
                 type: :type_spec,
                 metadata: %{}
               }
             ] == suggestions
    end

    test "erlang types edoc" do
      buffer = "defmodule My, do: @type my_type :: :docsh_edoc_xmerl.xml_element_con"

      suggestions = suggestions_by_type(:type_spec, buffer)

      assert [
               %{
                 arity: 0,
                 doc: "#xmlElement.content as defined by xmerl.hrl.",
                 name: :xml_element_content,
                 origin: ":docsh_edoc_xmerl",
                 signature: "xml_element_content()",
                 spec:
                   "@type xml_element_content() :: [\n  record(:xmlElement)\n  | record(:xmlText)\n  | record(:xmlPI)\n  | record(:xmlComment)\n  | record(:xmlDecl)\n]",
                 type: :type_spec,
                 metadata: %{}
               }
             ] == suggestions
    end

    test "no erlang private types" do
      buffer = "defmodule My, do: @type my_type :: :erlang.cpu_topo"

      suggestions = suggestions_by_type(:type_spec, buffer)

      assert [] == suggestions
    end

    test "type with @typedoc false" do
      buffer =
        "defmodule My, do: @type my_type :: ElixirSenseExample.ModuleWithDocs.some_type_doc_false"

      suggestions = suggestions_by_type(:type_spec, buffer)

      assert [
               %{
                 arity: 0,
                 doc: "",
                 name: :some_type_doc_false,
                 origin: "ElixirSenseExample.ModuleWithDocs",
                 signature: "some_type_doc_false()",
                 spec: "@type some_type_doc_false() ::\n  integer()",
                 type: :type_spec,
                 metadata: %{}
               }
             ] == suggestions
    end

    test "local types from metadata" do
      buffer = """
      defmodule MyModule do
        @typep my_local_t :: integer
        @typep my_local_arg_t(a, b) :: {a, b}
        @type my_type :: my_loc
        #                      ^
      end
      """

      list =
        ElixirSense.suggestions(buffer, 4, 26)
        |> Enum.filter(fn %{type: t} -> t == :type_spec end)

      assert [suggestion1, suggestion2] = list

      assert %{
               arity: 0,
               name: "my_local_t",
               origin: "MyModule",
               type: :type_spec,
               signature: "my_local_t()",
               doc: "",
               spec: "",
               metadata: %{}
             } == suggestion2

      assert %{
               arity: 2,
               name: "my_local_arg_t",
               origin: "MyModule",
               type: :type_spec,
               signature: "my_local_arg_t(a, b)",
               doc: "",
               spec: "",
               metadata: %{}
             } == suggestion1
    end

    test "local types from metadata external call - private types are not suggested" do
      buffer = """
      defmodule MyModule do
        @type my_local_t :: integer
        @typep my_local_arg_t(a, b) :: {a, b}
        @type my_type :: MyModule.my_loc
        #                               ^
      end
      """

      list =
        ElixirSense.suggestions(buffer, 4, 35)
        |> Enum.filter(fn %{type: t} -> t == :type_spec end)

      assert [suggestion1] = list

      assert %{
               arity: 0,
               name: "my_local_t",
               origin: "MyModule",
               type: :type_spec,
               signature: "my_local_t()",
               doc: "",
               spec: "",
               metadata: %{}
             } == suggestion1
    end

    test "remote public and opaque types from metadata" do
      buffer = """
      defmodule SomeModule do
        @typep my_local_priv_t :: integer
        @type my_local_pub_t(a, b) :: {a, b}
        @opaque my_local_op_t() :: my_local_priv_t
      end

      defmodule MyModule do
        alias SomeModule, as: Some
        @type my_type :: Some.my_loc
        #                           ^
      end
      """

      list =
        ElixirSense.suggestions(buffer, 9, 31)
        |> Enum.filter(fn %{type: t} -> t == :type_spec end)

      assert [suggestion1, suggestion2] = list

      assert %{
               arity: 2,
               name: "my_local_pub_t",
               origin: "SomeModule",
               type: :type_spec,
               signature: "my_local_pub_t(a, b)",
               doc: "",
               spec: "",
               metadata: %{}
             } == suggestion2

      assert %{
               arity: 0,
               name: "my_local_op_t",
               origin: "SomeModule",
               type: :type_spec,
               signature: "my_local_op_t()",
               doc: "",
               spec: "",
               metadata: %{}
             } == suggestion1
    end
  end

  test "suggestion understands alias shadowing" do
    # ordinary alias
    buffer = """
    defmodule ElixirSenseExample.OtherModule do
      alias ElixirSenseExample.SameModule
      def some_fun() do
        SameModule.te
      end
    end
    """

    assert [
             %{type: :hint, value: "SameModule.test_fun"},
             %{origin: "ElixirSenseExample.SameModule"}
           ] = ElixirSense.suggestions(buffer, 4, 17)

    # alias shadowing scope/inherited aliases
    buffer = """
    defmodule ElixirSenseExample.Abc.SameModule do
      alias List, as: SameModule
      alias ElixirSenseExample.SameModule
      def some_fun() do
        SameModule.te
      end
    end
    """

    assert [
             %{type: :hint, value: "SameModule.test_fun"},
             %{origin: "ElixirSenseExample.SameModule"}
           ] = ElixirSense.suggestions(buffer, 5, 17)

    buffer = """
    defmodule ElixirSenseExample.Abc.SameModule do
      require Logger, as: ModuleB
      require ElixirSenseExample.SameModule, as: SameModule
      SameModule.so
    end
    """

    assert [
             %{type: :hint, value: "SameModule.some_test_macro"},
             %{origin: "ElixirSenseExample.SameModule"}
           ] = ElixirSense.suggestions(buffer, 4, 15)
  end

  defp suggestions_by_type(type, buffer) do
    {line, column} = get_last_line_and_column(buffer)
    suggestions_by_type(type, buffer, line, column)
  end

  defp suggestions_by_type(type, buffer, line, column) do
    buffer
    |> add_aliases("Local, Remote")
    |> ElixirSense.suggestions(line + 1, column)
    |> Enum.filter(fn %{type: t} -> t == type end)
    |> Enum.sort()
  end

  defp suggestions_by_name(name, buffer) do
    {line, column} = get_last_line_and_column(buffer)
    suggestions_by_name(name, buffer, line, column)
  end

  defp suggestions_by_name(name, buffer, line, column) do
    buffer
    |> add_aliases("Local, Remote")
    |> ElixirSense.suggestions(line + 1, column)
    |> Enum.filter(fn
      %{name: n} -> n == name
      _ -> false
    end)
    |> Enum.sort()
  end

  defp suggestion_by_name(name, buffer) do
    {line, column} = get_last_line_and_column(buffer)
    suggestion_by_name(name, buffer, line, column)
  end

  defp suggestion_by_name(name, buffer, line, column) do
    [suggestion] = suggestions_by_name(name, buffer, line, column)
    suggestion
  end

  defp get_last_line_and_column(buffer) do
    str_lines = String.split(buffer, "\n")
    line = length(str_lines)
    column = (str_lines |> List.last() |> String.length()) + 1
    {line, column}
  end

  defp add_aliases(buffer, aliases) do
    "alias ElixirSenseExample.ModuleWithTypespecs.{#{aliases}}\n" <> buffer
  end
end
