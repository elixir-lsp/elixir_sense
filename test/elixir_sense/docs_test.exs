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

      assert docs == "No documentation available\n"
    end

    test "when empty buffer" do
      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs("", 1, 1)

      assert subject == ""
      assert actual_subject == ""

      assert docs == "No documentation available\n"
    end

    test "module with @moduledoc false" do
      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs("ElixirSenseExample.ModuleWithDocFalse", 1, 22)

      assert subject == "ElixirSenseExample.ModuleWithDocFalse"
      assert actual_subject == "ElixirSenseExample.ModuleWithDocFalse"

      assert docs == "> ElixirSenseExample.ModuleWithDocFalse\n\nNo documentation available\n"
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

             ```
             @spec flatten(deep_list) :: list when deep_list: [any | deep_list]
             ```

             Flattens the given `list` of nested lists.
             """
    end

    test "retrieve function documentation on @attr call" do
      buffer = """
      defmodule MyModule do
        @attr List
        @attr.flatten(list)
      end
      """

      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 3, 12)

      assert subject == "@attr.flatten"
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
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 3, 12)

      assert subject == ":lists.flatten"
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
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 3, 14)

      assert subject == ":erlang.or"
      assert actual_subject == ":erlang.or"

      assert docs =~ """

             ### Specs

             ```
             @spec boolean or boolean :: boolean
             ```

             """

      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 4, 14)

      assert subject == ":erlang.orelse"
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
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 3, 12)

      assert subject == "Macros.some"
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

             ```
             @spec flatten(deep_list) :: list when deep_list: [any | deep_list]
             ```

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

             ```
             @spec flatten(deep_list) :: list when deep_list: [any | deep_list]
             ```

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
      assert docs =~ "Kernel.defmacro("
      assert docs =~ "macro with the given name and body."
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

    test "retrieve documentation from erlang modules" do
      buffer = """
      defmodule MyModule do
        alias :erlang, as: Erl
      end
      """

      %{
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 2, 13)

      assert subject == ":erlang"
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

    test "retrieve type information from modules" do
      buffer = """
      defmodule MyModule do
        use GenServer
      end
      """

      %{subject: subject, docs: %{types: docs}} = ElixirSense.docs(buffer, 2, 8)

      assert subject == "GenServer"

      assert docs =~ """
             ```
             @type from :: {pid, tag :: term}
             ```

             Tuple describing the client of a call request.
             """
    end

    test "retrieve fallback type information from erlang modules" do
      buffer = """
      defmodule MyModule do
        alias :erlang, as: Erl
      end
      """

      %{subject: subject, docs: %{types: docs}} = ElixirSense.docs(buffer, 2, 11)

      assert subject == ":erlang"

      assert docs =~ """
             ```
             @type time_unit ::\
             """
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
        docs: %{docs: docs_1}
      } = ElixirSense.docs(buffer, 2, 30)

      %{
        actual_subject: actual_subject_2,
        docs: %{docs: docs_2}
      } = ElixirSense.docs(buffer, 2, 38)

      assert actual_subject_1 == actual_subject_2
      assert docs_1 == docs_2
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
             ```
             @opaque t(x)
             ```
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

             **Optional**

             ### Specs

             ```
             @callback config_change(changed, new, removed) :: :ok when changed: keyword, new: keyword, removed: [atom]
             ```

             Callback invoked after code upgrade\
             """
    end

    test "retrieve fallback callback information from erlang modules" do
      buffer = """
      defmodule MyModule do
        use :gen_statem
      end
      """

      %{subject: subject, docs: %{callbacks: docs}} = ElixirSense.docs(buffer, 2, 8)

      assert subject == ":gen_statem"

      assert docs =~ """
             ```
             @callback state_name\
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
             > optional(a)

             **Optional**

             ### Specs

             ```
             @macrocallback optional(a) :: Macro.t when a: atom
             ```

             An optional macrocallback
             """
    end

    test "existing module with no docs" do
      buffer = """
      defmodule MyModule do
        raise ArgumentError, "Error"
      end
      """

      %{subject: subject, docs: %{docs: docs}} = ElixirSense.docs(buffer, 2, 11)

      assert subject == "ArgumentError"
      assert docs == "> ArgumentError\n\nNo documentation available\n"
    end

    test "not existing module docs" do
      buffer = """
      defmodule MyModule do
        raise NotExistingError, "Error"
      end
      """

      %{subject: subject, docs: %{docs: docs}} = ElixirSense.docs(buffer, 2, 11)

      assert subject == "NotExistingError"
      assert docs == "No documentation available\n"
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
             > ElixirSenseExample.ModuleWithTypespecs.Remote.remote_t()

             ### Specs

             ```
             @type remote_t() :: atom()
             ```

             Remote type

             ---

             > ElixirSenseExample.ModuleWithTypespecs.Remote.remote_t(a, b)

             ### Specs

             ```
             @type remote_t(a, b) :: {a, b}
             ```

             Remote type with params
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
             > ElixirSenseExample.CallbackOpaque.t(x)

             **Opaque**

             ### Specs

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
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 3, 31)

      assert subject == ":erlang.time_unit"
      assert actual_subject == ":erlang.time_unit"

      assert docs =~ """
             > :erlang.time_unit()

             ### Specs

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
             > keyword()

             **Built-in**

             ### Specs

             ```
             @type keyword() :: [{atom(), any()}]
             ```

             A keyword list

             ---

             > keyword(t)

             **Built-in**

             ### Specs

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
        subject: subject,
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 2, 19)

      assert subject == "integer"
      assert actual_subject == "integer"

      assert docs == """
             > integer()

             **Built-in**

             ### Specs

             ```
             integer()
             ```

             An integer number
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
             > list()

             **Built-in**

             ### Specs

             ```
             @type list() :: [any()]
             ```

             A list

             ---

             > list(t)

             **Built-in**

             ### Specs

             ```
             list(t)
             ```

             Proper list ([]-terminated)
             """
    end

    test "issue #34" do
      buffer = """
      use Config

      config :logger, :console,
        format: "$time $metadata[$level] $message\n"
      """

      %{actual_subject: "Kernel.use"} = ElixirSense.docs(buffer, 1, 2)
    end

    test "find built-in functions" do
      # module_info is defined by default for every elixir and erlang module
      # __info__ is defined for every elixir module
      # behaviour_info is defined for every behaviour and every protocol
      buffer = """
      defmodule MyModule do
        ElixirSenseExample.ModuleWithFunctions.module_info()
        #                                      ^
        ElixirSenseExample.ModuleWithFunctions.__info__(:macros)
        #                                      ^
        ElixirSenseExample.ExampleBehaviour.behaviour_info(:callbacks)
        #                                      ^
      end
      """

      assert %{
               actual_subject: "ElixirSenseExample.ModuleWithFunctions.module_info",
               docs: %{
                 docs: """
                 > ElixirSenseExample.ModuleWithFunctions.module_info()

                 **Built-in**

                 ### Specs

                 ```
                 @spec module_info :: [{:module | :attributes | :compile | :exports | :md5 | :native, term}]
                 ```

                 No documentation available


                 ---

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

                 No documentation available

                 """
               }
             } = ElixirSense.docs(buffer, 2, 42)

      assert %{actual_subject: "ElixirSenseExample.ModuleWithFunctions.__info__"} =
               ElixirSense.docs(buffer, 4, 42)

      assert %{actual_subject: "ElixirSenseExample.ExampleBehaviour.behaviour_info"} =
               ElixirSense.docs(buffer, 6, 42)
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

      assert %{actual_subject: "module_info", docs: %{docs: "No documentation available\n"}} =
               ElixirSense.docs(buffer, 4, 5)

      assert %{actual_subject: "__info__", docs: %{docs: "No documentation available\n"}} =
               ElixirSense.docs(buffer, 6, 5)

      assert %{actual_subject: "behaviour_info", docs: %{docs: "No documentation available\n"}} =
               ElixirSense.docs(buffer, 8, 5)
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
      subject: subject,
      actual_subject: actual_subject,
      docs: %{docs: docs}
    } = ElixirSense.docs(buffer, 3, 5)

    assert subject == "foo"
    assert actual_subject == "ElixirSenseExample.ExampleBehaviourWithDocCallbackNoImpl.foo"

    assert docs =~ """
           > ElixirSenseExample.ExampleBehaviourWithDocCallbackNoImpl.foo()

           **Implementing behaviour**
           ElixirSenseExample.ExampleBehaviourWithDoc

           ### Specs

           ```
           @spec foo :: :ok
           ```

           Docs for foo
           """
  end

  test "retrieve function documentation from behaviour even if @doc is set to false" do
    buffer = """
    defmodule MyModule do
      import ElixirSenseExample.ExampleBehaviourWithDocCallbackImpl
      baz()
    end
    """

    %{
      subject: subject,
      actual_subject: actual_subject,
      docs: %{docs: docs}
    } = ElixirSense.docs(buffer, 3, 5)

    assert subject == "baz"
    assert actual_subject == "ElixirSenseExample.ExampleBehaviourWithDocCallbackImpl.baz"

    assert docs =~ """
           > ElixirSenseExample.ExampleBehaviourWithDocCallbackImpl.baz(a)

           **Implementing behaviour**
           ElixirSenseExample.ExampleBehaviourWithDoc

           ### Specs

           ```
           @spec baz(integer) :: :ok
           ```

           Docs for baz
           """
  end

  test "retrieve macro documentation from behaviour if available" do
    buffer = """
    defmodule MyModule do
      import ElixirSenseExample.ExampleBehaviourWithDocCallbackNoImpl
      bar()
    end
    """

    %{
      subject: subject,
      actual_subject: actual_subject,
      docs: %{docs: docs}
    } = ElixirSense.docs(buffer, 3, 5)

    assert subject == "bar"
    assert actual_subject == "ElixirSenseExample.ExampleBehaviourWithDocCallbackNoImpl.bar"

    assert docs =~ """
           > ElixirSenseExample.ExampleBehaviourWithDocCallbackNoImpl.bar(b)

           **Implementing behaviour**
           ElixirSenseExample.ExampleBehaviourWithDoc

           ### Specs

           ```
           @spec bar(integer) :: Macro.t
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
      subject: subject,
      actual_subject: actual_subject,
      docs: %{docs: docs}
    } = ElixirSense.docs(buffer, 1, 16)

    assert subject == ":file_server.init"
    assert actual_subject == ":file_server.init"

    assert docs =~ """
           > :file_server.init(term)

           **Implementing behaviour**
           :gen_server

           ### Specs

           ```
           @spec init(args :: term) ::
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
      subject: subject,
      actual_subject: actual_subject,
      docs: %{docs: docs}
    } = ElixirSense.docs(buffer, 3, 5)

    assert subject == "init"
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
             @spec init(args :: term) :: init_result(state)
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
      subject: subject,
      actual_subject: actual_subject,
      docs: %{docs: docs}
    } = ElixirSense.docs(buffer, 3, 12)

    assert subject == "List.flatten"
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
end
