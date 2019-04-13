defmodule ElixirSense.SuggestionsTest do

  use ExUnit.Case

  test "empty hint" do
    buffer = """
    defmodule MyModule do

    end
    """

    list = ElixirSense.suggestions(buffer, 2, 7)

    assert Enum.find(list, fn s -> match?(%{name: "import", arity: 2}, s) end) == %{
      args: "module,opts", arity: 2, name: "import",
      origin: "Kernel.SpecialForms", spec: "",
      summary: "Imports functions and macros from other modules.",
      type: "macro"
    }
    assert Enum.find(list, fn s -> match?(%{name: "quote", arity: 2}, s) end) == %{
      arity: 2, origin: "Kernel.SpecialForms",
      spec: "", type: "macro", args: "opts,block",
      name: "quote",
      summary: "Gets the representation of any expression."
    }
    assert Enum.find(list, fn s -> match?(%{name: "require", arity: 2}, s) end) == %{
      arity: 2, origin: "Kernel.SpecialForms",
      spec: "", type: "macro", args: "module,opts",
      name: "require",
      summary: "Requires a module in order to use its macros."
    }

  end

  test "without empty hint" do

    buffer = """
    defmodule MyModule do
      is_b
    end
    """

    list = ElixirSense.suggestions(buffer, 2, 11)

    assert list == [
      %{type: :hint, value: "is_b"},
      %{args: "term", arity: 1, name: "is_binary", origin: "Kernel",
        spec: "@spec is_binary(term) :: boolean",
        summary: "Returns `true` if `term` is a binary; otherwise returns `false`.",
        type: "function"},
      %{args: "term", arity: 1, name: "is_bitstring", origin: "Kernel",
        spec: "@spec is_bitstring(term) :: boolean",
        summary: "Returns `true` if `term` is a bitstring (including a binary); otherwise returns `false`.",
        type: "function"},
      %{args: "term", arity: 1, name: "is_boolean", origin: "Kernel",
        spec: "@spec is_boolean(term) :: boolean",
        summary: "Returns `true` if `term` is either the atom `true` or the atom `false` (i.e.,\na boolean); otherwise returns `false`.",
        type: "function"}
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

    assert list  == [
      %{type: :hint, value: "MyList.flatten"},
      %{args: "list,tail", arity: 2, name: "flatten", origin: "List",
       spec: "@spec flatten(deep_list, [elem]) :: [elem] when deep_list: [elem | deep_list], elem: var",
       summary: "Flattens the given `list` of nested lists.\nThe list `tail` will be added at the end of\nthe flattened list.",
       type: "function"},
      %{args: "list", arity: 1, name: "flatten", origin: "List",
       spec: "@spec flatten(deep_list) :: list when deep_list: [any | deep_list]",
       summary: "Flattens the given `list` of nested lists.",
       type: "function"}
    ]
  end

  test "with a module hint" do
    buffer = """
    defmodule MyModule do
      Str
    end
    """

    list = ElixirSense.suggestions(buffer, 2, 6)

    assert list == [
      %{type: :hint, value: "Str"},
      %{name: "Stream", subtype: :struct,
       summary: "Functions for creating and composing streams.",
       type: :module},
      %{name: "String", subtype: nil,
       summary: "A String in Elixir is a UTF-8 encoded binary.",
       type: :module},
      %{name: "StringIO", subtype: nil,
       summary: "Controls an IO device process that wraps a string.",
       type: :module}
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
      |> Enum.filter(fn s -> s.type == :callback && s.name == :code_change end)

    assert [%{
      args: "old_vsn,state,extra", arity: 3, name: :code_change,
      origin: "GenServer",
      spec: "@callback code_change(old_vsn, state :: term, extra :: term) ::" <> _,
      summary: "Invoked to change the state of the `GenServer` when a different version of a\nmodule is loaded (hot code swapping) and the state's term structure should be\nchanged.",
      type: :callback
    }] = list
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
      %{description: "{:reply, reply, new_state}",
       snippet: "{:reply, \"${1:reply}$\", \"${2:new_state}$\"}",
       spec: "{:reply, reply, new_state} when reply: term, new_state: term, reason: term",
       type: :return},
      %{description: "{:reply, reply, new_state, timeout | :hibernate | {:continue, term}}",
        snippet: "{:reply, \"${1:reply}$\", \"${2:new_state}$\", \"${3:timeout | :hibernate | {:continue, term}}$\"}",
        spec: "{:reply, reply, new_state, timeout | :hibernate | {:continue, term}} when reply: term, new_state: term, reason: term",
        type: :return},
      %{description: "{:noreply, new_state}",
        snippet: "{:noreply, \"${1:new_state}$\"}",
        spec: "{:noreply, new_state} when reply: term, new_state: term, reason: term",
        type: :return},
      %{description: "{:noreply, new_state, timeout | :hibernate | {:continue, term}}",
        snippet: "{:noreply, \"${1:new_state}$\", \"${2:timeout | :hibernate | {:continue, term}}$\"}",
        spec: "{:noreply, new_state, timeout | :hibernate | {:continue, term}} when reply: term, new_state: term, reason: term",
        type: :return},
      %{description: "{:stop, reason, reply, new_state}",
        snippet: "{:stop, \"${1:reason}$\", \"${2:reply}$\", \"${3:new_state}$\"}",
        spec: "{:stop, reason, reply, new_state} when reply: term, new_state: term, reason: term",
        type: :return},
      %{description: "{:stop, reason, new_state}",
        snippet: "{:stop, \"${1:reason}$\", \"${2:new_state}$\"}",
        spec: "{:stop, reason, new_state} when reply: term, new_state: term, reason: term",
        type: :return}
    ]
  end

  test "lists params and vars" do
    buffer = """
    defmodule MyServer do
      use GenServer

      def handle_call(request, from, state) do
        var1 = true

      end

    end
    """

    list =
      ElixirSense.suggestions(buffer, 6, 5)
      |> Enum.filter(fn s -> s.type == :variable end)

    assert list == [
      %{name: :from, type: :variable},
      %{name: :request, type: :variable},
      %{name: :state, type: :variable},
      %{name: :var1, type: :variable}
    ]
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
      |> Enum.filter(fn s -> s.type == :attribute end)

    assert list == [
      %{name: "@my_attribute1", type: :attribute},
      %{name: "@my_attribute2", type: :attribute}
    ]
  end

  test "Elixir module" do
    buffer = """
    defmodule MyModule do
      El
    end
    """

    list = ElixirSense.suggestions(buffer, 2, 5)

    assert Enum.at(list,0) == %{type: :hint, value: "Elixir"}
    assert Enum.at(list,1) == %{type: :module, name: "Elixir", subtype: nil, summary: ""}
  end

  test "suggestion for aliases modules defined by require clause" do

    buffer =
      """
      defmodule Mod do
        require Integer, as: I
        I.is_o
      end
      """

    list = ElixirSense.suggestions(buffer, 3, 9)
    assert Enum.at(list,1).name == "is_odd"
  end

  test "suggestion for struct fields" do
    buffer =
      """
      defmodule Mod do
        %IO.Stream{
      end
      """

    list = ElixirSense.suggestions(buffer, 2, 14)
    assert list == [
      %{type: :hint, value: ""},
      %{name: :device, origin: "IO.Stream", type: :field},
      %{name: :line_or_bytes, origin: "IO.Stream", type: :field},
      %{name: :raw, origin: "IO.Stream", type: :field}
    ]
  end

  test "suggestion for aliased struct fields" do
    buffer =
      """
      defmodule Mod do
        alias IO.Stream
        %Stream{
      end
      """

    list = ElixirSense.suggestions(buffer, 3, 11)
    assert list == [
      %{type: :hint, value: ""},
      %{name: :device, origin: "IO.Stream", type: :field},
      %{name: :line_or_bytes, origin: "IO.Stream", type: :field},
      %{name: :raw, origin: "IO.Stream", type: :field}
    ]
  end

  test "no suggestion of fields when the module is not a struct" do
    buffer =
      """
      defmodule Mod do
        %Enum{
      end
      """

    list = ElixirSense.suggestions(buffer, 2, 9)
    assert Enum.any?(list, fn %{type: type} -> type == :field end) == false
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
      assert suggestions_by_type(:param_option, buffer1) == suggestions_by_type(:param_option, buffer2)
    end

    test "options as inline list" do
      buffer = "Local.func_with_options_as_inline_list("

      assert %{type_spec: "local_t()", expanded_spec: "@type local_t :: atom"} =
        suggestion_by_name(:local_o, buffer)

      assert %{type_spec: "keyword()", expanded_spec: "@type keyword :: [{atom, any}]"} =
        suggestion_by_name(:builtin_o, buffer)
    end

    test "options vars defined in when" do
      type_spec = "local_t()"
      origin = "ElixirSenseExample.ModuleWithTypespecs.Local"
      spec = "@type local_t :: atom"

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

    test "opaque type" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:opaque_o, buffer)

      assert suggestion.type_spec == "opaque_t()"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Local"
      assert suggestion.expanded_spec == ""
      assert suggestion.doc == "Local opaque type"
    end

    test "private type" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:private_o, buffer)

      assert suggestion.type_spec == "private_t()"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Local"
      assert suggestion.expanded_spec == ""
      assert suggestion.doc == ""
    end

    test "local type" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:local_o, buffer)

      assert suggestion.type_spec == "local_t()"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Local"
      assert suggestion.expanded_spec == "@type local_t :: atom"
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
      assert suggestion.expanded_spec == "@type keyword :: [{atom, any}]"
      assert suggestion.doc == "A keyword list"
    end

    test "built-in type with params" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:builtin_with_params_o, buffer)

      assert suggestion.type_spec == "keyword(term())"
      assert suggestion.origin == ""
      assert suggestion.expanded_spec == "@type keyword(t) :: [{atom, t}]"
      assert suggestion.doc == "A keyword list with values of type `t`"
    end

    test "union type" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:union_o, buffer)

      assert suggestion.type_spec == "union_t()"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Local"
      assert suggestion.expanded_spec == "@type union_t :: atom | integer"
    end

    test "list type" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:list_o, buffer)

      assert suggestion.type_spec == "list_t()"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Local"
      assert suggestion.expanded_spec == "@type list_t :: [:trace | :log]"
    end

    test "remote type" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:remote_o, buffer)

      assert suggestion.type_spec == "ElixirSenseExample.ModuleWithTypespecs.Remote.remote_t()"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Remote"
      assert suggestion.expanded_spec == "@type remote_t :: atom"
      assert suggestion.doc == "Remote type"
    end

    test "remote type with args" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:remote_with_params_o, buffer)

      assert suggestion.type_spec == "ElixirSenseExample.ModuleWithTypespecs.Remote.remote_t(atom(), integer())"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Remote"
      assert suggestion.expanded_spec == "@type remote_t(a, b) :: {a, b}"
      assert suggestion.doc == "Remote type with params"
    end

    test "remote aliased type" do
      buffer = "Local.func_with_options("
      suggestion = suggestion_by_name(:remote_aliased_o, buffer)

      assert suggestion.type_spec == "remote_aliased_t()"
      assert suggestion.origin == "ElixirSenseExample.ModuleWithTypespecs.Local"
      assert suggestion.expanded_spec == """
      @type remote_aliased_t ::
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
      assert suggestion.expanded_spec == "@type remote_t :: atom"
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

      assert suggestion.type_spec == "ElixirSenseExample.ModuleWithTypespecs.Remote.non_existent()"
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
      assert suggestion.expanded_spec == "@type remote_t :: atom"
      assert suggestion.doc == "Remote type"
    end

    test "format type spec" do
      buffer = "Local.func_with_options("

      assert suggestion_by_name(:large_o, buffer).expanded_spec == """
      @type large_t ::
        pid
        | port
        | (registered_name :: atom)
        | {registered_name :: atom,
           node}\
      """
    end
  end

  defp suggestions_by_type(type, buffer) do
    {line, column} = get_last_line_and_column(buffer)
    suggestions_by_type(type, buffer, line, column)
  end

  defp suggestions_by_type(type, buffer, line, column) do
    buffer
    |> add_aliases("Local")
    |> ElixirSense.suggestions(line + 1, column)
    |> Enum.filter(fn %{type: t} -> t == type end)
  end

  defp suggestion_by_name(name, buffer) do
    {line, column} = get_last_line_and_column(buffer)
    suggestion_by_name(name, buffer, line, column)
  end

  defp suggestion_by_name(name, buffer, line, column) do
    [suggestion] =
      buffer
      |> add_aliases("Local")
      |> ElixirSense.suggestions(line + 1, column)
      |> Enum.filter(fn %{name: n} -> n == name; _ -> false end)
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
