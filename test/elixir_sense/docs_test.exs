defmodule ElixirSense.DocsTest do
  use ExUnit.Case, async: true

  describe "docs" do
    test "when no docs do not return Built-in type" do
      buffer = """
      hkjnjknjk
      """

      refute ElixirSense.docs(buffer, 1, 2)
    end

    test "when empty buffer" do
      assert nil == ElixirSense.docs("", 1, 1)
    end

    test "module with @moduledoc false" do
      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs("ElixirSenseExample.ModuleWithDocFalse", 1, 22)

      assert actual_subject == "ElixirSenseExample.ModuleWithDocFalse"

      assert docs == "> ElixirSenseExample.ModuleWithDocFalse\n\n"
    end

    test "retrieve documentation from Kernel macro" do
      buffer = """
      defmodule MyModule do

      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 1, 2)

      assert actual_subject == "Kernel.defmodule"

      assert docs =~ """
             Defines a module given by name with the given contents.
             """
    end

    test "retrieve documentation from Kernel.SpecialForm macro" do
      buffer = """
      defmodule MyModule do
        import List
         ^
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 2, 4)

      assert actual_subject == "Kernel.SpecialForms.import"

      assert docs =~ """
             Imports functions and macros\
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
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 3, 12)

      assert actual_subject == "List.flatten"

      assert docs =~ """
             > List.flatten(list)

             ### Specs

             ```
             @spec flatten(deep_list) :: list when deep_list: [any | deep_list]
             ```

             Flattens the given `list` of nested lists.
             """
    end

    test "retrieve metadata function documentation" do
      buffer = """
      defmodule MyLocalModule do
        @doc "Sample doc"
        @doc since: "1.2.3"
        @spec flatten(list()) :: list()
        def flatten(list) do
          []
        end
      end

      defmodule MyModule do
        def func(list) do
          MyLocalModule.flatten(list)
        end
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 12, 20)

      assert actual_subject == "MyLocalModule.flatten"

      assert docs =~ """
             > MyLocalModule.flatten(list)

             ### Specs

             ```
             @spec flatten(list) :: list
             ```
             """

      #  TODO docs and metadata
    end

    test "retrieve local private metadata function documentation" do
      buffer = """
      defmodule MyLocalModule do
        @doc "Sample doc"
        @doc since: "1.2.3"
        @spec flatten(list()) :: list()
        defp flatten(list) do
          []
        end

        def func(list) do
          flatten(list)
        end
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 10, 7)

      assert actual_subject == "MyLocalModule.flatten"

      assert docs =~ """
             > MyLocalModule.flatten(list)

             ### Specs

             ```
             @spec flatten(list) :: list
             ```
             """

      #  TODO docs and metadata
    end

    test "retrieve metadata function documentation - fallback to callback in metadata" do
      buffer = """
      defmodule MyBehaviour do
        @doc "Sample doc"
        @doc since: "1.2.3"
        @callback flatten(list()) :: list()
      end

      defmodule MyLocalModule do
        @behaviour MyBehaviour
        
        @impl true
        def flatten(list) do
          []
        end
      end

      defmodule MyModule do
        def func(list) do
          MyLocalModule.flatten(list)
        end
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 18, 20)

      assert actual_subject == "MyLocalModule.flatten"

      assert docs =~ """
             > MyLocalModule.flatten(list)

             **Implementing behaviour**
             MyBehaviour

             ### Specs

             ```
             @callback flatten(list) :: list
             ```
             """

      #  TODO docs and metadata
    end

    test "retrieve metadata function documentation - fallback to protocol function in metadata" do
      buffer = """
      defprotocol BB do
        @doc "asdf"
        @spec go(t) :: integer()
        def go(t)
      end

      defimpl BB, for: String do
        def go(t), do: ""
      end

      defmodule MyModule do
        def func(list) do
          BB.String.go(list)
        end
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 13, 16)

      assert actual_subject == "BB.String.go"

      assert docs =~ """
             > BB.String.go(t)

             **Implementing behaviour**
             BB

             ### Specs

             ```
             @callback go(t) :: integer
             ```
             """

      #  TODO docs and metadata
    end

    test "retrieve documentation of local macro" do
      buffer = """
      defmodule MyModule do
        defmacrop some(var), do: Macro.expand(var, __CALLER__)

        defmacro other do
          some(1)
        end
      end
      """

      assert %{
               actual_subject: _actual_subject,
               docs: _docs
             } = ElixirSense.docs(buffer, 5, 6)
    end

    test "find definition of local macro on definition" do
      buffer = """
      defmodule MyModule do
        defmacrop some(var), do: Macro.expand(var, __CALLER__)

        defmacro other do
          some(1)
        end
      end
      """

      assert %{
               actual_subject: _actual_subject,
               docs: _docs
             } = ElixirSense.docs(buffer, 2, 14)
    end

    test "does not find definition of local macro if it's defined after the cursor" do
      buffer = """
      defmodule MyModule do
        defmacro other do
          some(1)
        end

        defmacrop some(var), do: Macro.expand(var, __CALLER__)
      end
      """

      assert ElixirSense.docs(buffer, 3, 6) == nil
    end

    test "find definition of local function even if it's defined after the cursor" do
      buffer = """
      defmodule MyModule do
        def other do
          some(1)
        end

        defp some(var), do: :ok
      end
      """

      assert %{
               actual_subject: _actual_subject,
               docs: _docs
             } = ElixirSense.docs(buffer, 3, 6)
    end

    test "retrieve metadata macro documentation - fallback to macrocallback in metadata" do
      buffer = """
      defmodule MyBehaviour do
        @doc "Sample doc"
        @doc since: "1.2.3"
        @macrocallback flatten(list()) :: list()
      end

      defmodule MyLocalModule do
        @behaviour MyBehaviour
        
        @impl true
        defmacro flatten(list) do
          []
        end
      end

      defmodule MyModule do
        require MyLocalModule
        def func(list) do
          MyLocalModule.flatten(list)
        end
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 19, 20)

      assert actual_subject == "MyLocalModule.flatten"

      assert docs =~ """
             > MyLocalModule.flatten(list)

             **Implementing behaviour**
             MyBehaviour

             ### Specs

             ```
             @macrocallback flatten(list) :: list
             ```
             """

      #  TODO docs and metadata
    end

    test "retrieve metadata function documentation - fallback to callback" do
      buffer = """
      defmodule MyLocalModule do
        @behaviour ElixirSenseExample.BehaviourWithMeta
        
        @impl true
        def flatten(list) do
          []
        end
      end

      defmodule MyModule do
        def func(list) do
          MyLocalModule.flatten(list)
        end
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 12, 20)

      assert actual_subject == "MyLocalModule.flatten"

      assert docs =~ """
             > MyLocalModule.flatten(list)

             **Implementing behaviour**
             ElixirSenseExample.BehaviourWithMeta
             **Since**
             1.2.3

             ### Specs

             ```
             @callback flatten(list) :: list
             ```
             """
    end

    test "retrieve metadata function documentation - fallback to erlang callback" do
      buffer = """
      defmodule MyLocalModule do
        @behaviour :gen_statem
        
        @impl true
        def init(list) do
          []
        end
      end

      defmodule MyModule do
        def func(list) do
          MyLocalModule.init(list)
        end
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 12, 20)

      assert actual_subject == "MyLocalModule.init"

      if ExUnitConfig.erlang_eep48_supported() do
        assert docs =~ """
               > MyLocalModule.init(list)

               **Implementing behaviour**
               :gen_statem
               **Since**
               OTP 19.0

               ### Specs

               ```
               @callback init(args :: term) ::\
               """

        assert docs =~
                 "this function is called by the new process to initialize the implementation state and server data"
      end
    end

    test "retrieve metadata macro documentation - fallback to macrocallback" do
      buffer = """
      defmodule MyLocalModule do
        @behaviour ElixirSenseExample.BehaviourWithMeta
        
        @impl true
        defmacro bar(list) do
          []
        end
      end

      defmodule MyModule do
        require MyLocalModule
        def func(list) do
          MyLocalModule.bar(list)
        end
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 13, 20)

      assert actual_subject == "MyLocalModule.bar"

      assert docs =~ """
             > MyLocalModule.bar(list)

             **Implementing behaviour**
             ElixirSenseExample.BehaviourWithMeta
             **Since**
             1.2.3

             ### Specs

             ```
             @macrocallback bar(integer) :: Macro.t
             ```

             Docs for bar
             """
    end

    test "retrieve local private metadata function documentation on __MODULE__ call" do
      buffer = """
      defmodule MyLocalModule do
        @doc "Sample doc"
        @doc since: "1.2.3"
        @spec flatten(list()) :: list()
        def flatten(list) do
          []
        end

        def func(list) do
          __MODULE__.flatten(list)
        end
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 10, 17)

      assert actual_subject == "MyLocalModule.flatten"

      assert docs =~ """
             > MyLocalModule.flatten(list)

             ### Specs

             ```
             @spec flatten(list) :: list
             ```
             """

      #  TODO docs and metadata
    end

    @tag requires_elixir_1_14: true
    test "retrieve local private metadata function documentation on __MODULE__ submodule call" do
      buffer = """
      defmodule MyLocalModule do
        defmodule Sub do
          @doc "Sample doc"
          @doc since: "1.2.3"
          @spec flatten(list()) :: list()
          def flatten(list) do
            []
          end
        end

        def func(list) do
          __MODULE__.Sub.flatten(list)
        end
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 12, 20)

      assert actual_subject == "MyLocalModule.Sub.flatten"

      assert docs =~ """
             > MyLocalModule.Sub.flatten(list)

             ### Specs

             ```
             @spec flatten(list) :: list
             ```
             """

      #  TODO docs and metadata
    end

    test "does not retrieve remote private metadata function documentation" do
      buffer = """
      defmodule MyLocalModule do
        @doc "Sample doc"
        @doc since: "1.2.3"
        @spec flatten(list()) :: list()
        defp flatten(list) do
          []
        end
      end

      defmodule MyModule do
        def func(list) do
          MyLocalModule.flatten(list)
        end
      end
      """

      assert nil == ElixirSense.docs(buffer, 12, 20)
    end

    test "retrieve metadata function documentation with @doc false" do
      buffer = """
      defmodule MyLocalModule do
        @doc false
        @spec flatten(list()) :: list()
        def flatten(list) do
          []
        end
      end

      defmodule MyModule do
        def func(list) do
          MyLocalModule.flatten(list)
        end
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 11, 20)

      assert actual_subject == "MyLocalModule.flatten"

      assert docs =~ """
             > MyLocalModule.flatten(list)

             ### Specs

             ```
             @spec flatten(list) :: list
             ```
             """

      #  TODO mark as hidden in metadata
    end

    test "retrieve function documentation on @attr call" do
      buffer = """
      defmodule MyModule do
        @attr List
        @attr.flatten(list)
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 3, 12)

      assert actual_subject == "List.flatten"

      assert docs =~ """
             > List.flatten(list)

             ### Specs

             ```
             @spec flatten(deep_list) :: list when deep_list: [any | deep_list]
             ```

             Flattens the given `list` of nested lists.
             """
    end

    test "retrieve erlang function documentation" do
      buffer = """
      defmodule MyModule do
        def func(list) do
          :lists.flatten(list)
        end
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 3, 12)

      assert actual_subject == ":lists.flatten"

      assert docs =~ """
             > :lists.flatten(deepList)

             ### Specs

             ```
             @spec flatten(deepList) :: list when deepList: [term | deepList], list: [term]
             ```
             """

      if ExUnitConfig.erlang_eep48_supported() do
        assert docs =~ """
               Returns a flattened version of `DeepList`\\.
               """
      end
    end

    @tag requires_otp_23: true
    test "retrieve fallback erlang builtin function documentation" do
      buffer = """
      defmodule MyModule do
        def func(list) do
          :erlang.or(a, b)
          :erlang.orelse(a, b)
        end
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 3, 14)

      assert actual_subject == ":erlang.or"

      assert docs =~ """

             ### Specs

             ```
             @spec boolean or boolean :: boolean
             ```

             """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 4, 14)

      assert actual_subject == ":erlang.orelse"

      assert docs =~ """
             > :erlang.orelse(term, term)

             **Built-in**
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
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 3, 12)

      assert actual_subject == "ElixirSenseExample.BehaviourWithMacrocallback.Impl.some"

      assert docs =~ """
             > ElixirSenseExample.BehaviourWithMacrocallback.Impl.some(var)

             ### Specs

             ```
             @spec some(integer) :: Macro.t
             @spec some(b) :: Macro.t when b: float
             ```

             some macro
             """
    end

    @tag requires_elixir_1_14: true
    test "retrieve function documentation with __MODULE__ submodule call" do
      buffer = """
      defmodule Inspect do
        def func(list) do
          __MODULE__.Algebra.string(list)
        end
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 3, 26)

      assert actual_subject == "Inspect.Algebra.string"

      assert docs =~ """
             > Inspect.Algebra.string(string)
             """
    end

    test "retrieve function documentation from aliased modules" do
      buffer = """
      defmodule MyModule do
        alias List, as: MyList
        MyList.flatten([])
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 3, 12)

      assert actual_subject == "List.flatten"

      assert docs =~ """
             > List.flatten(list)

             ### Specs

             ```
             @spec flatten(deep_list) :: list when deep_list: [any | deep_list]
             ```

             Flattens the given `list` of nested lists.
             """
    end

    test "retrieve function documentation from imported modules" do
      buffer = """
      defmodule MyModule do
        import Mix.Generator
        create_file("a", "b")
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 3, 5)

      assert actual_subject == "Mix.Generator.create_file"

      assert docs =~ """
             > Mix.Generator.create_file(path, contents, opts \\\\\\\\ [])
             """
    end

    test "retrieve documentation from modules" do
      buffer = """
      defmodule MyModule do
        use GenServer
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 2, 8)

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

    test "retrieve documentation from metadata modules" do
      buffer = """
      defmodule MyLocalModule do
        @moduledoc "Some example doc"
        @moduledoc since: "1.2.3"

        @callback some() :: :ok
      end

      defmodule MyModule do
        @behaviour MyLocalModule
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 9, 15)

      assert actual_subject == "MyLocalModule"

      assert docs =~ """
             > MyLocalModule
             """

      # TODO doc and metadata
    end

    test "retrieve documentation from metadata modules on __MODULE__" do
      buffer = """
      defmodule MyLocalModule do
        @moduledoc "Some example doc"
        @moduledoc since: "1.2.3"

        def self() do
          __MODULE__
        end
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 6, 6)

      assert actual_subject == "MyLocalModule"

      assert docs =~ """
             > MyLocalModule
             """

      # TODO doc and metadata
    end

    @tag requires_elixir_1_14: true
    test "retrieve documentation from metadata modules on __MODULE__ submodule" do
      buffer = """
      defmodule MyLocalModule do
        defmodule Sub do
          @moduledoc "Some example doc"
          @moduledoc since: "1.2.3"
        end

        def self() do
          __MODULE__.Sub
        end
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 8, 17)

      assert actual_subject == "MyLocalModule.Sub"

      assert docs =~ """
             > MyLocalModule.Sub
             """

      # TODO doc and metadata
    end

    test "retrieve documentation from metadata modules with @moduledoc false" do
      buffer = """
      defmodule MyLocalModule do
        @moduledoc false

        @callback some() :: :ok
      end

      defmodule MyModule do
        @behaviour MyLocalModule
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 8, 15)

      assert actual_subject == "MyLocalModule"

      assert docs =~ """
             > MyLocalModule
             """

      # TODO mark as hidden
    end

    test "retrieve documentation from erlang modules" do
      buffer = """
      defmodule MyModule do
        alias :erlang, as: Erl
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 2, 13)

      assert actual_subject == ":erlang"

      assert docs =~ """
             > :erlang
             """

      if ExUnitConfig.erlang_eep48_supported() do
        assert docs =~ """
               By convention,\
               """
      end
    end

    test "retrieve documentation from modules in 1.2 alias syntax" do
      buffer = """
      defmodule MyModule do
        alias ElixirSenseExample.ModuleWithDocs
        alias ElixirSenseExample.{Some, ModuleWithDocs}
      end
      """

      %{
        actual_subject: actual_subject_1,
        docs: docs_1
      } = ElixirSense.docs(buffer, 2, 30)

      %{
        actual_subject: actual_subject_2,
        docs: docs_2
      } = ElixirSense.docs(buffer, 2, 38)

      assert actual_subject_1 == actual_subject_2
      assert docs_1 == docs_2
    end

    test "existing module with no docs" do
      buffer = """
      defmodule MyModule do
        raise ArgumentError, "Error"
      end
      """

      %{actual_subject: actual_subject, docs: docs} = ElixirSense.docs(buffer, 2, 11)

      assert actual_subject == "ArgumentError"
      assert docs == "> ArgumentError\n\n"
    end

    test "not existing module docs" do
      buffer = """
      defmodule MyModule do
        raise NotExistingError, "Error"
      end
      """

      refute ElixirSense.docs(buffer, 2, 11)
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
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 3, 31)

      assert actual_subject == "ElixirSenseExample.ModuleWithTypespecs.Remote.remote_t"

      assert docs == """
             > ElixirSenseExample.ModuleWithTypespecs.Remote.remote_t()

             ### Definition

             ```
             @type remote_t() :: atom()
             ```

             Remote type
             """
    end

    test "retrieve metadata type documentation" do
      buffer = """
      defmodule MyLocalModule do
        @typedoc "My example type"
        @typedoc since: "1.2.3"
        @type some(a) :: {a}
      end

      defmodule MyModule do
        @type my_list :: MyLocalModule.some(:a)
        #                               ^
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 8, 35)

      assert actual_subject == "MyLocalModule.some"

      assert docs == """
             > MyLocalModule.some(a)

             ### Definition

             ```
             @type some(a) :: {a}
             ```


             """

      # TODO docs and metadata
    end

    test "retrieve local private metadata type documentation" do
      buffer = """
      defmodule MyLocalModule do
        @typedoc "My example type"
        @typedoc since: "1.2.3"
        @typep some(a) :: {a}

        @type my_list :: some(:a)
        #                  ^
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 6, 22)

      assert actual_subject == "MyLocalModule.some"

      assert docs == """
             > MyLocalModule.some(a)

             ### Definition

             ```
             @typep some(a) :: {a}
             ```


             """

      # TODO docs and metadata
    end

    test "retrieve local metadata type documentation even if it's defined after cursor" do
      buffer = """
      defmodule MyModule do
        @type remote_list_t :: [my_t]
        #                         ^

        @typep my_t :: integer
      end
      """

      assert %{actual_subject: _} =
               ElixirSense.docs(buffer, 2, 29)
    end

    test "does not retrieve remote private metadata type documentation" do
      buffer = """
      defmodule MyLocalModule do
        @typedoc "My example type"
        @typedoc since: "1.2.3"
        @typep some(a) :: {a}
      end

      defmodule MyModule do
        @type my_list :: MyLocalModule.some(:a)
        #                               ^
      end
      """

      assert nil == ElixirSense.docs(buffer, 8, 35)
    end

    test "does not reveal details for opaque metadata type" do
      buffer = """
      defmodule MyLocalModule do
        @typedoc "My example type"
        @typedoc since: "1.2.3"
        @opaque some(a) :: {a}
      end

      defmodule MyModule do
        @type my_list :: MyLocalModule.some(:a)
        #                               ^
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 8, 35)

      assert actual_subject == "MyLocalModule.some"

      assert docs == """
             > MyLocalModule.some(a)

             ### Definition

             ```
             @opaque some(a)
             ```


             """

      # TODO docs and metadata
    end

    test "retrieve metadata type documentation with @typedoc false" do
      buffer = """
      defmodule MyLocalModule do
        @typedoc false
        @type some(a) :: {a}
      end

      defmodule MyModule do
        @type my_list :: MyLocalModule.some(:a)
        #                               ^
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 7, 35)

      assert actual_subject == "MyLocalModule.some"

      assert docs == """
             > MyLocalModule.some(a)

             ### Definition

             ```
             @type some(a) :: {a}
             ```


             """

      # TODO mark as hidden
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
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 3, 35)

      assert actual_subject == "ElixirSenseExample.CallbackOpaque.t"

      assert docs == """
             > ElixirSenseExample.CallbackOpaque.t(x)

             **Opaque**

             ### Definition

             ```
             @opaque t(x)
             ```

             Opaque type

             """
    end

    test "retrieve erlang type documentation" do
      buffer = """
      defmodule MyModule do
        alias ElixirSenseExample.ModuleWithTypespecs.Remote
        @type my_list :: :erlang.time_unit
        #                           ^
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 3, 31)

      assert actual_subject == ":erlang.time_unit"

      assert docs =~ """
             > :erlang.time_unit()

             ### Definition

             ```
             @type time_unit() ::
               pos_integer()
               | :second
               | :millisecond
               | :microsecond
               | :nanosecond
               | :native
               | :perf_counter
               | deprecated_time_unit()
             ```
             """

      if ExUnitConfig.erlang_eep48_supported() do
        assert docs =~ """
               Supported time unit representations:
               """
      end
    end

    test "retrieve builtin type documentation" do
      buffer = """
      defmodule MyModule do
        @type options :: keyword
        #                   ^
        @type options1 :: keyword(integer)
        #                   ^
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 2, 23)

      assert actual_subject == "keyword"

      assert docs == """
             > keyword()

             **Built-in**

             ### Definition

             ```
             @type keyword() :: [{atom(), any()}]
             ```

             A keyword list
             """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 4, 23)

      assert actual_subject == "keyword"

      assert docs == """
             > keyword(t)

             **Built-in**

             ### Definition

             ```
             @type keyword(t) :: [{atom(), t}]
             ```

             A keyword list with values of type `t`
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
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 2, 19)

      assert actual_subject == "integer"

      assert docs == """
             > integer()

             **Built-in**

             ### Definition

             ```
             @type integer()
             ```

             An integer number
             """
    end

    test "retrieve basic and builtin type documentation" do
      buffer = """
      defmodule MyModule do
        @type num :: list()
        #              ^
        @type num1 :: list(atom)
        #              ^
      end
      """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 2, 18)

      assert actual_subject == "list"

      assert docs == """
             > list()

             **Built-in**

             ### Definition

             ```
             @type list() :: [any()]
             ```

             A list
             """

      %{
        actual_subject: actual_subject,
        docs: docs
      } = ElixirSense.docs(buffer, 4, 18)

      assert actual_subject == "list"

      assert docs == """
             > list(t)

             **Built-in**

             ### Definition

             ```
             @type list(t)
             ```

             Proper list ([]-terminated)
             """
    end

    test "find built-in functions" do
      # module_info is defined by default for every elixir and erlang module
      # __info__ is defined for every elixir module
      # behaviour_info is defined for every behaviour and every protocol
      buffer = """
      defmodule MyModule do
        ElixirSenseExample.ModuleWithFunctions.module_info()
        #                                      ^
        ElixirSenseExample.ModuleWithFunctions.module_info(:exports)
        #                                      ^
        ElixirSenseExample.ModuleWithFunctions.__info__(:macros)
        #                                      ^
        ElixirSenseExample.ExampleBehaviour.behaviour_info(:callbacks)
        #                                      ^
      end
      """

      assert %{
               actual_subject: "ElixirSenseExample.ModuleWithFunctions.module_info",
               docs: """
               > ElixirSenseExample.ModuleWithFunctions.module_info()

               **Built-in**

               ### Specs

               ```
               @spec module_info :: [{:module | :attributes | :compile | :exports | :md5 | :native, term}]
               ```


               """
             } = ElixirSense.docs(buffer, 2, 42)

      assert %{
               actual_subject: "ElixirSenseExample.ModuleWithFunctions.module_info",
               docs: """
               > ElixirSenseExample.ModuleWithFunctions.module_info(key)

               **Built-in**

               ### Specs

               ```
               @spec module_info(:module) :: atom
               @spec module_info(:attributes | :compile) :: [{atom, term}]
               @spec module_info(:md5) :: binary
               @spec module_info(:exports | :functions | :nifs) :: [{atom, non_neg_integer}]
               @spec module_info(:native) :: boolean
               ```


               """
             } = ElixirSense.docs(buffer, 4, 42)

      assert %{actual_subject: "ElixirSenseExample.ModuleWithFunctions.__info__"} =
               ElixirSense.docs(buffer, 6, 42)

      assert %{actual_subject: "ElixirSenseExample.ExampleBehaviour.behaviour_info"} =
               ElixirSense.docs(buffer, 8, 42)
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
        #^
        __info__(:macros)
        #^
        behaviour_info(:callbacks)
        #^
      end
      """

      refute ElixirSense.docs(buffer, 4, 5)

      refute ElixirSense.docs(buffer, 6, 5)

      refute ElixirSense.docs(buffer, 8, 5)
    end
  end

  test "retrieve function documentation from behaviour if available" do
    buffer = """
    defmodule MyModule do
      import ElixirSenseExample.ExampleBehaviourWithDocCallbackNoImpl
      foo()
    end
    """

    %{
      actual_subject: actual_subject,
      docs: docs
    } = ElixirSense.docs(buffer, 3, 5)

    assert actual_subject == "ElixirSenseExample.ExampleBehaviourWithDocCallbackNoImpl.foo"

    assert docs =~ """
           > ElixirSenseExample.ExampleBehaviourWithDocCallbackNoImpl.foo()

           **Implementing behaviour**
           ElixirSenseExample.ExampleBehaviourWithDoc

           ### Specs

           ```
           @callback foo :: :ok
           ```

           Docs for foo
           """
  end

  test "retrieve function documentation from behaviour even if @doc is set to false" do
    buffer = """
    defmodule MyModule do
      import ElixirSenseExample.ExampleBehaviourWithDocCallbackImpl
      baz(1)
    end
    """

    %{
      actual_subject: actual_subject,
      docs: docs
    } = ElixirSense.docs(buffer, 3, 5)

    assert actual_subject == "ElixirSenseExample.ExampleBehaviourWithDocCallbackImpl.baz"

    assert docs =~ """
           > ElixirSenseExample.ExampleBehaviourWithDocCallbackImpl.baz(a)

           **Implementing behaviour**
           ElixirSenseExample.ExampleBehaviourWithDoc

           ### Specs

           ```
           @callback baz(integer) :: :ok
           ```

           Docs for baz
           """
  end

  test "retrieve macro documentation from behaviour if available" do
    buffer = """
    defmodule MyModule do
      import ElixirSenseExample.ExampleBehaviourWithDocCallbackNoImpl
      bar(1)
    end
    """

    %{
      actual_subject: actual_subject,
      docs: docs
    } = ElixirSense.docs(buffer, 3, 5)

    assert actual_subject == "ElixirSenseExample.ExampleBehaviourWithDocCallbackNoImpl.bar"

    assert docs =~ """
           > ElixirSenseExample.ExampleBehaviourWithDocCallbackNoImpl.bar(b)

           **Implementing behaviour**
           ElixirSenseExample.ExampleBehaviourWithDoc

           ### Specs

           ```
           @macrocallback bar(integer) :: Macro.t
           ```

           Docs for bar
           """
  end

  @tag requires_otp_25: true
  test "retrieve erlang behaviour implementation " do
    buffer = """
    :file_server.init(a)
    """

    %{
      actual_subject: actual_subject,
      docs: docs
    } = ElixirSense.docs(buffer, 1, 16)

    assert actual_subject == ":file_server.init"

    assert docs =~ """
           > :file_server.init(args)

           **Implementing behaviour**
           :gen_server

           ### Specs

           ```
           @callback init(args :: term) ::
           """

    assert docs =~ "Whenever a `gen_server` process is started"
  end

  test "do not crash for erlang behaviour callbacks" do
    buffer = """
    defmodule MyModule do
      import ElixirSenseExample.ExampleBehaviourWithDocCallbackErlang
      init(:ok)
    end
    """

    %{
      actual_subject: actual_subject,
      docs: docs
    } = ElixirSense.docs(buffer, 3, 5)

    assert actual_subject == "ElixirSenseExample.ExampleBehaviourWithDocCallbackErlang.init"

    if ExUnitConfig.erlang_eep48_supported() do
      assert docs =~ """
             > ElixirSenseExample.ExampleBehaviourWithDocCallbackErlang.init(_)

             **Implementing behaviour**
             :gen_statem
             **Since**
             OTP 19.0

             ### Specs

             ```
             @callback init(args :: term) :: init_result(state)
             ```
             """
    else
      assert docs =~ """
             > ElixirSenseExample.ExampleBehaviourWithDocCallbackErlang.init(_)
             """
    end
  end

  test "retrieve callback documentation from behaviour" do
    buffer = """
    defmodule MyModule do
      def func(list) do
        List.flatten(list)
      end
    end
    """

    %{
      actual_subject: actual_subject,
      docs: docs
    } = ElixirSense.docs(buffer, 3, 12)

    assert actual_subject == "List.flatten"

    assert docs =~ """
           > List.flatten(list)

           ### Specs

           ```
           @spec flatten(deep_list) :: list when deep_list: [any | deep_list]
           ```

           Flattens the given `list` of nested lists.
           """
  end

  test "retrieve reserved module attributes documentation" do
    buffer = """
    defmodule MyModule do
      @on_load :on_load

      def on_load(), do: :ok
    end
    """

    assert %{
             actual_subject: "@on_load",
             docs: docs
           } = ElixirSense.docs(buffer, 2, 6)

    assert docs =~ "A hook that will be invoked whenever the module is loaded."
  end

  test "retrieve unreserved module attributes documentation" do
    buffer = """
    defmodule MyModule do
      @my_attribute nil
    end
    """

    assert %{
             actual_subject: "@my_attribute",
             docs: docs
           } = ElixirSense.docs(buffer, 2, 6)

    assert docs =~ "attribute"
  end

  test "retrieve docs on reserved words" do
    buffer = """
    defmodule MyModule do
    end
    """

    if Version.match?(System.version(), ">= 1.14.0") do
      assert %{
               actual_subject: "do",
               docs: docs
             } = ElixirSense.docs(buffer, 1, 21)

      assert docs =~ "do-end block control keyword"
    else
      assert nil == ElixirSense.docs(buffer, 1, 21)
    end
  end

  test "retrieve docs on variables" do
    buffer = """
    defmodule MyModule do
      def fun(my_var) do
        other_var = 5
        abc(my_var, other_var)
      end
    end
    """

    assert %{
             actual_subject: "my_var",
             docs: docs
           } = ElixirSense.docs(buffer, 2, 12)

    assert docs =~ "variable"

    assert %{
             actual_subject: "other_var",
             docs: docs
           } = ElixirSense.docs(buffer, 3, 6)

    assert docs =~ "variable"
  end

  test "variables shadow builtin functions" do
    buffer = """
    defmodule Vector do
      @spec magnitude(Vec2.t()) :: number()
      def magnitude(%Vec2{} = v), do: :math.sqrt(:math.pow(v.x, 2) + :math.pow(v.y, 2))

      @spec normalize(Vec2.t()) :: Vec2.t()
      def normalize(%Vec2{} = v) do
        length = magnitude(v)
        %{v | x: v.x / length, y: v.y / length}
      end
    end
    """

    assert %{
             actual_subject: "length",
             docs: docs
           } = ElixirSense.docs(buffer, 7, 6)

    assert docs =~ "variable"

    assert %{
             actual_subject: "length",
             docs: docs
           } = ElixirSense.docs(buffer, 8, 21)

    assert docs =~ "variable"
  end

  test "find local type in typespec local def elsewhere" do
    buffer = """
    defmodule ElixirSenseExample.Some do
      @type some_local :: integer

      def some_local(), do: :ok

      @type user :: {some_local, integer}

      def foo do
        some_local
      end
    end
    """

    assert %{actual_subject: "ElixirSenseExample.Some.some_local"} =
             ElixirSense.docs(buffer, 6, 20)

    assert %{actual_subject: "ElixirSenseExample.Some.some_local"} =
             ElixirSense.docs(buffer, 9, 9)
  end

  test "retrieves documentation for correct arity function" do
    buffer = """
    defmodule MyModule do
      alias ElixirSenseExample.FunctionsWithDefaultArgs, as: F
      def main, do: {F.my_func(), F.my_func("a"), F.my_func(1, 2, 3), F.my_func(1, 2, 3, 4)}
    end
    """

    assert %{actual_subject: "ElixirSenseExample.FunctionsWithDefaultArgs.my_func", docs: docs} =
             ElixirSense.docs(buffer, 3, 34)

    assert docs =~ "2 params version"
    assert docs =~ "@spec my_func(1 | 2) :: binary"
    assert docs =~ "@spec my_func(1 | 2, binary) :: binary"

    refute docs =~ "no params version"
    refute docs =~ "@spec my_func :: binary"
    refute docs =~ "3 params version"
    refute docs =~ "@spec my_func(1, 2, 3) :: :ok"
    refute docs =~ "@spec my_func(2, 2, 3) :: :error"

    # too many arguments
    assert nil == ElixirSense.docs(buffer, 3, 70)
  end

  test "retrieves documentation for all matching arities with incomplete code" do
    buffer = """
    defmodule MyModule do
      alias ElixirSenseExample.FunctionsWithDefaultArgs, as: F
      def main, do: F.my_func(
    end
    """

    assert %{actual_subject: "ElixirSenseExample.FunctionsWithDefaultArgs.my_func", docs: docs} =
             ElixirSense.docs(buffer, 3, 20)

    assert docs =~ "no params version"
    assert docs =~ "2 params version"
    assert docs =~ "3 params version"

    buffer = """
    defmodule MyModule do
      alias ElixirSenseExample.FunctionsWithDefaultArgs, as: F
      def main, do: F.my_func(1
    end
    """

    assert %{actual_subject: "ElixirSenseExample.FunctionsWithDefaultArgs.my_func", docs: docs} =
             ElixirSense.docs(buffer, 3, 20)

    refute docs =~ "no params version"
    assert docs =~ "2 params version"
    assert docs =~ "3 params version"

    buffer = """
    defmodule MyModule do
      alias ElixirSenseExample.FunctionsWithDefaultArgs, as: F
      def main, do: F.my_func(1, 2,
    end
    """

    assert %{actual_subject: "ElixirSenseExample.FunctionsWithDefaultArgs.my_func", docs: docs} =
             ElixirSense.docs(buffer, 3, 20)

    refute docs =~ "no params version"
    refute docs =~ "2 params version"
    assert docs =~ "3 params version"

    buffer = """
    defmodule MyModule do
      alias ElixirSenseExample.FunctionsWithDefaultArgs, as: F
      def main, do: F.my_func(1, 2, 3
    end
    """

    assert %{actual_subject: "ElixirSenseExample.FunctionsWithDefaultArgs.my_func", docs: docs} =
             ElixirSense.docs(buffer, 3, 20)

    refute docs =~ "no params version"
    refute docs =~ "2 params version"
    assert docs =~ "3 params version"

    buffer = """
    defmodule MyModule do
      alias ElixirSenseExample.FunctionsWithDefaultArgs, as: F
      def main, do: F.my_func(1, 2, 3,
    end
    """

    # too many arguments
    assert nil == ElixirSense.docs(buffer, 3, 20)

    buffer = """
    defmodule MyModule do
      alias ElixirSenseExample.FunctionsWithDefaultArgs, as: F
      def main, do: 1 |> F.my_func(
    end
    """

    assert %{actual_subject: "ElixirSenseExample.FunctionsWithDefaultArgs.my_func", docs: docs} =
             ElixirSense.docs(buffer, 3, 26)

    refute docs =~ "no params version"
    assert docs =~ "2 params version"
    assert docs =~ "3 params version"

    buffer = """
    defmodule MyModule do
      alias ElixirSenseExample.FunctionsWithDefaultArgs, as: F
      def main, do: 1 |> F.my_func(1,
    end
    """

    assert %{actual_subject: "ElixirSenseExample.FunctionsWithDefaultArgs.my_func", docs: docs} =
             ElixirSense.docs(buffer, 3, 26)

    refute docs =~ "no params version"
    refute docs =~ "2 params version"
    assert docs =~ "3 params version"
  end

  test "retrieves documentation for correct arity function capture" do
    buffer = """
    defmodule MyModule do
      alias ElixirSenseExample.FunctionsWithDefaultArgs, as: F
      def go, do: &F.my_func/1
    end
    """

    assert %{actual_subject: "ElixirSenseExample.FunctionsWithDefaultArgs.my_func", docs: docs} =
             ElixirSense.docs(buffer, 3, 19)

    assert docs =~ "2 params version"
    assert docs =~ "@spec my_func(1 | 2) :: binary"
    assert docs =~ "@spec my_func(1 | 2, binary) :: binary"

    refute docs =~ "no params version"
    refute docs =~ "@spec my_func :: binary"
    refute docs =~ "3 params version"
    refute docs =~ "@spec my_func(1, 2, 3) :: :ok"
    refute docs =~ "@spec my_func(2, 2, 3) :: :error"
  end

  test "retrieves documentation for correct arity type" do
    buffer = """
    defmodule MyModule do
      alias ElixirSenseExample.TypesWithMultipleArity, as: T
      @type some :: {T.my_type, T.my_type(boolean), T.my_type(1, 2), T.my_type(1, 2, 3)}
    end
    """

    assert %{actual_subject: "ElixirSenseExample.TypesWithMultipleArity.my_type", docs: docs} =
             ElixirSense.docs(buffer, 3, 32)

    assert docs =~ "one param version"
    assert docs =~ "@type my_type(a)"

    refute docs =~ "no params version"
    refute docs =~ "@type my_type()"
    refute docs =~ "two params version"
    refute docs =~ "@type my_type(a, b)"

    # too many arguments
    assert nil == ElixirSense.docs(buffer, 3, 68)
  end

  test "retrieves documentation for all matching type arities with incomplete code" do
    buffer = """
    defmodule MyModule do
      alias ElixirSenseExample.TypesWithMultipleArity, as: T
      @type some :: T.my_type(
    end
    """

    assert %{actual_subject: "ElixirSenseExample.TypesWithMultipleArity.my_type", docs: docs} =
             ElixirSense.docs(buffer, 3, 20)

    assert docs =~ "no params version"
    assert docs =~ "one param version"
    assert docs =~ "two params version"

    buffer = """
    defmodule MyModule do
      alias ElixirSenseExample.TypesWithMultipleArity, as: T
      @type some :: T.my_type(integer
    end
    """

    assert %{actual_subject: "ElixirSenseExample.TypesWithMultipleArity.my_type", docs: docs} =
             ElixirSense.docs(buffer, 3, 20)

    refute docs =~ "no params version"
    assert docs =~ "one param version"
    assert docs =~ "two params version"

    buffer = """
    defmodule MyModule do
      alias ElixirSenseExample.TypesWithMultipleArity, as: T
      @type some :: T.my_type(integer, integer
    end
    """

    assert %{actual_subject: "ElixirSenseExample.TypesWithMultipleArity.my_type", docs: docs} =
             ElixirSense.docs(buffer, 3, 20)

    refute docs =~ "no params version"
    refute docs =~ "one param version"
    assert docs =~ "two params version"

    buffer = """
    defmodule MyModule do
      alias ElixirSenseExample.TypesWithMultipleArity, as: T
      @type some :: T.my_type(integer, integer,
    end
    """

    # too many arguments
    assert nil == ElixirSense.docs(buffer, 3, 20)
  end
end
