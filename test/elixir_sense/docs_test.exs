defmodule ElixirSense.DocsTest do
  use ExUnit.Case, async: true

  describe "docs" do
    test "when no docs do not return Built-in type" do
      buffer = """
      hkjnjknjk
      """

      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 1, 2)

      assert subject == "hkjnjknjk"
      assert actual_subject == "hkjnjknjk"

      assert docs == "No documentation available"
    end

    test "when empty buffer" do
      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs("", 1, 1)

      assert subject == ""
      assert actual_subject == ""

      assert docs == "No documentation available"
    end

    test "retrieve documentation" do
      buffer = """
      defmodule MyModule do

      end
      """

      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 1, 2)

      assert subject == "defmodule"
      assert actual_subject == "Kernel.defmodule"

      assert docs =~ """
             Defines a module given by name with the given contents.
             """
    end

    test "retrieve function documentation" do
      buffer = """
      defmodule MyModule do
        def func(list) do
          List.flatten(list)
        end
      end
      """

      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 3, 12)

      assert subject == "List.flatten"
      assert actual_subject == "List.flatten"

      assert docs =~ """
             > List.flatten(list)

             ### Specs

             `@spec flatten(deep_list) :: list when deep_list: [any | deep_list]`

             Flattens the given `list` of nested lists.
             """
    end

    test "retrieve macro documentation" do
      buffer = """
      defmodule MyModule do
        require ElixirSenseExample.BehaviourWithMacrocallback.Impl, as: Macros
        Macros.some({})
      end
      """

      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 3, 12)

      assert subject == "Macros.some"
      assert actual_subject == "ElixirSenseExample.BehaviourWithMacrocallback.Impl.some"
      
      assert docs =~ """
             > ElixirSenseExample.BehaviourWithMacrocallback.Impl.some(var)

             ### Specs

             `@spec some(integer) :: Macro.t`

             some macro
             """
    end
    test "retrieve function documentation atom module" do
      buffer = """
      defmodule MyModule do
        def func(list) do
          :"Elixir.List".flatten(list)
        end
      end
      """

      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 3, 22)

      assert subject == ":\"Elixir.List\".flatten"
      assert actual_subject == "List.flatten"

      assert docs =~ """
             > List.flatten(list)

             ### Specs

             `@spec flatten(deep_list) :: list when deep_list: [any | deep_list]`

             Flattens the given `list` of nested lists.
             """
    end

    test "retrieve function documentation with __MODULE__" do
      buffer = """
      defmodule Inspect do
        def func(list) do
          __MODULE__.Algebra.string(list)
        end
      end
      """

      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 3, 26)

      assert subject == "__MODULE__.Algebra.string"
      assert actual_subject == "Inspect.Algebra.string"

      assert docs =~ """
             > Inspect.Algebra.string(string)
             """
    end

    test "retrieve function documentation from aliased modules" do
      buffer = """
      defmodule MyModule do
        alias List, as: MyList
        MyList.flatten
      end
      """

      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 3, 12)

      assert subject == "MyList.flatten"
      assert actual_subject == "List.flatten"

      assert docs =~ """
             > List.flatten(list)

             ### Specs

             `@spec flatten(deep_list) :: list when deep_list: [any | deep_list]`

             Flattens the given `list` of nested lists.
             """
    end

    test "retrive function documentation from imported modules" do
      buffer = """
      defmodule MyModule do
        import Mix.Generator
        create_file(
      end
      """

      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 3, 5)

      assert subject == "create_file"
      assert actual_subject == "Mix.Generator.create_file"

      assert docs =~ """
             > Mix.Generator.create_file(path, contents, opts \\\\\\\\ [])
             """
    end

    test "request for defmacro" do
      buffer = """
      defmodule MyModule do
        defmacro my_macro do
        end
      end
      """

      %{subject: subject, docs: %{docs: docs}} = ElixirSense.docs(buffer, 2, 5)

      assert subject == "defmacro"
      assert docs =~ "Kernel.defmacro(call, expr \\\\\\\\ nil)"
      assert docs =~ "Defines a macro with the given name and body."
    end

    test "retrieve documentation from modules" do
      buffer = """
      defmodule MyModule do
        use GenServer
      end
      """

      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 2, 8)

      assert subject == "GenServer"
      assert actual_subject == "GenServer"

      assert docs =~ """
             > GenServer

             A behaviour module for implementing the server of a client-server relation.

             A GenServer is a process like any other Elixir process and it can be used
             to keep state, execute code asynchronously and so on. The advantage of using
             a generic server process (GenServer) implemented using this module is that it
             will have a standard set of interface functions and include functionality for
             tracing and error reporting. It will also fit into a supervision tree.
             """
    end

    test "retrieve type information from modules" do
      buffer = """
      defmodule MyModule do
        use GenServer
      end
      """

      %{subject: subject, docs: %{types: docs}} = ElixirSense.docs(buffer, 2, 8)

      assert subject == "GenServer"

      assert docs =~ """
             `@type from :: {pid, tag :: term}
             `

             Tuple describing the client of a call request.
             """
    end

    test "does not reveal opaque types" do
      buffer = """
      defmodule MyModule do
        @behaviour ElixirSenseExample.CallbackOpaque
      end
      """

      %{subject: subject, docs: %{types: docs}} = ElixirSense.docs(buffer, 2, 40)

      assert subject == "ElixirSenseExample.CallbackOpaque"

      assert docs =~ """
             `@opaque t(x)
             `
             """
    end

    test "retrieve callback information from modules" do
      buffer = """
      defmodule MyModule do
        use Application
      end
      """

      %{subject: subject, docs: %{callbacks: docs}} = ElixirSense.docs(buffer, 2, 8)

      assert subject == "Application"

      assert docs =~ """
             > config_change(changed, new, removed)

             ### Specs

             `@callback config_change(changed, new, removed) :: :ok when changed: keyword, new: keyword, removed: [atom]
             `

             Callback invoked after code upgrade\
             """
    end

    test "retrieve macrocallback information from modules" do
      buffer = """
      defmodule MyModule do
        @behaviour ElixirSenseExample.BehaviourWithMacrocallback
      end
      """

      %{subject: subject, docs: %{callbacks: docs}} = ElixirSense.docs(buffer, 2, 40)

      assert subject == "ElixirSenseExample.BehaviourWithMacrocallback"

      assert docs =~ """
             > optional(atom)

             ### Specs

             `@macrocallback optional(atom) :: Macro.t
             `

             An optional macrocallback\
             """
    end

    test "no docs" do
      buffer = """
      defmodule MyModule do
        raise ArgumentError, "Error"
      end
      """

      %{subject: subject, docs: %{docs: docs}} = ElixirSense.docs(buffer, 2, 11)

      assert subject == "ArgumentError"
      assert docs == "No documentation available"
    end

    test "retrieve type documentation" do
      buffer = """
      defmodule MyModule do
        alias ElixirSenseExample.ModuleWithTypespecs.Remote
        @type my_list :: Remote.remote_t
        #                           ^
      end
      """

      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 3, 31)

      assert subject == "Remote.remote_t"
      assert actual_subject == "ElixirSenseExample.ModuleWithTypespecs.Remote.remote_t"

      assert docs == """
             Remote type

             ```
             @type remote_t() :: atom()
             ```

             ---

             Remote type with params

             ```
             @type remote_t(a, b) :: {a, b}
             ```\
             """
    end

    test "does not reveal opaque type details" do
      buffer = """
      defmodule MyModule do
        alias ElixirSenseExample.CallbackOpaque
        @type my_list :: CallbackOpaque.t(integer)
        #                               ^
      end
      """

      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 3, 35)

      assert subject == "CallbackOpaque.t"
      assert actual_subject == "ElixirSenseExample.CallbackOpaque.t"

      assert docs == """
             Opaque type


             ```
             @opaque t(x)
             ```\
             """
    end

    test "retrieve builtin type documentation" do
      buffer = """
      defmodule MyModule do
        @type options :: keyword
        #                   ^
      end
      """

      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 2, 23)

      assert subject == "keyword"
      assert actual_subject == "keyword"

      assert docs == """
             A keyword list

             ```
             @type keyword() :: [{atom(), any()}]
             ```

             ---

             A keyword list with values of type `t`

             ```
             @type keyword(t) :: [{atom(), t}]
             ```

             ---

             _* Built-in type_\
             """
    end

    test "retrieve basic type documentation" do
      buffer = """
      defmodule MyModule do
        @type num :: integer
        #               ^
      end
      """

      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 2, 19)

      assert subject == "integer"
      assert actual_subject == "integer"

      assert docs == """
             An integer number

             ```
             integer()
             ```

             ---

             _* Built-in type_\
             """
    end

    test "retrieve basic and builtin type documentation" do
      buffer = """
      defmodule MyModule do
        @type num :: list(atom)
        #              ^
      end
      """

      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 2, 18)

      assert subject == "list"
      assert actual_subject == "list"

      assert docs == """
             A list

             ```
             @type list() :: [any()]
             ```

             ---

             Proper list ([]-terminated)

             ```
             list(t)
             ```

             ---

             _* Built-in type_\
             """
    end

    test "issue #34" do
      buffer = """
      use Mix.Config

      config :logger, :console,
        format: "$time $metadata[$level] $message\n"
      """

      %{actual_subject: "Kernel.use"} = ElixirSense.docs(buffer, 1, 2)
    end
  end
end
