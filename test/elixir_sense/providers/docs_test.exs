defmodule ElixirSense.Providers.DocsTest do

  use ExUnit.Case

  describe "docs" do

    test "retrieve documentation" do
      buffer = """
      defmodule MyModule do

      end
      """

      assert ElixirSense.docs("defmodule", buffer, 2).docs =~ """
      Defines a module given by name with the given contents.
      """
    end

    test "retrieve function documentation" do
      buffer = """
      defmodule MyModule do

      end
      """

      docs = ElixirSense.docs("List.flatten", buffer, 2).docs

      assert docs == """
      > List.flatten(list)

      ### Specs

      `@spec flatten(deep_list) :: list when deep_list: [any | deep_list]`

      Flattens the given `list` of nested lists.

      ## Examples

          iex> List.flatten([1, [[2], 3]])
          [1, 2, 3]



      ____

      > List.flatten(list, tail)

      ### Specs

      `@spec flatten(deep_list, [elem]) :: [elem] when deep_list: [elem | deep_list], elem: var`

      Flattens the given `list` of nested lists.
      The list `tail` will be added at the end of
      the flattened list.

      ## Examples

          iex> List.flatten([1, [[2], 3]], [4, 5])
          [1, 2, 3, 4, 5]

      """
    end

    test "retrieve function documentation from aliased modules" do
      buffer = """
      defmodule MyModule do
        alias List, as: MyList

      end
      """

      docs = ElixirSense.docs("MyList.flatten", buffer, 3).docs

      assert docs == """
      > List.flatten(list)

      ### Specs

      `@spec flatten(deep_list) :: list when deep_list: [any | deep_list]`

      Flattens the given `list` of nested lists.

      ## Examples

          iex> List.flatten([1, [[2], 3]])
          [1, 2, 3]



      ____

      > List.flatten(list, tail)

      ### Specs

      `@spec flatten(deep_list, [elem]) :: [elem] when deep_list: [elem | deep_list], elem: var`

      Flattens the given `list` of nested lists.
      The list `tail` will be added at the end of
      the flattened list.

      ## Examples

          iex> List.flatten([1, [[2], 3]], [4, 5])
          [1, 2, 3, 4, 5]

      """
    end

    test "retrive function documentation from imported modules" do
      buffer = """
      defmodule MyModule do
        import Mix.Generator

      end
      """

      docs = ElixirSense.docs("create_file", buffer, 3).docs

      assert docs =~ """
      > Mix.Generator.create_file(path, contents, opts \\\\\\\\ [])

      ### Specs

      `@spec create_file(Path.t, iodata, Keyword.t) :: any`

      Creates a file with the given contents.
      If the file already exists, asks for user confirmation.

      ## Options

        * `:force` - forces installation without a shell prompt.
      """
    end

    test "request for defmacro" do
      buffer = """
      defmodule MyModule do

      end
      """

      docs = ElixirSense.docs("defmacro", buffer, 2).docs

      assert docs =~ """
      > Kernel.defmacro(call, expr \\\\\\\\ nil)

      Defines a macro with the given name and body.

      ## Examples

          defmodule MyLogic do
            defmacro unless(expr, opts) do
              quote do
                if !unquote(expr), unquote(opts)
              end
            end
          end

          require MyLogic
          MyLogic.unless false do
            IO.puts \"It works\"
          end

      """
    end

    test "retrieve documentation from modules" do
      buffer = """
      defmodule MyModule do

      end
      """

      docs = ElixirSense.docs("GenServer", buffer, 2).docs

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

      end
      """

      docs = ElixirSense.docs("GenServer", buffer, 2).types

      assert docs =~ """
        `@type on_start ::
        {:ok, pid} |
        :ignore |
        {:error, {:already_started, pid} | term}
      `

        Return values of `start*` functions


      ____

        `@type name ::
        atom |
        {:global, term} |
        {:via, module, term}
      `

        The GenServer name
      """
    end

    test "retrieve callback information from modules" do
      buffer = """
      defmodule MyModule do

      end
      """

      docs = ElixirSense.docs("Application", buffer, 2).callbacks

      assert docs =~ """
        > start(start_type, start_args)

        ### Specs

        `@callback start(start_type, start_args :: term) ::
        {:ok, pid} |
        {:ok, pid, state} |
        {:error, reason :: term}
      `

        Called when an application is started.
      """
    end

  end
end
