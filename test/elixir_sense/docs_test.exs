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
        docs: %{docs: docs}
      } = ElixirSense.docs("ElixirSenseExample.ModuleWithDocFalse", 1, 22)

      assert actual_subject == "ElixirSenseExample.ModuleWithDocFalse"

      assert docs == "> ElixirSenseExample.ModuleWithDocFalse\n\n"
    end

    test "retrieve documentation" do
      buffer = """
      defmodule MyModule do

      end
      """

      %{
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 1, 2)

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
        actual_subject: actual_subject,
        docs: %{docs: docs}
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

    test "retrieve function documentation on @attr call" do
      buffer = """
      defmodule MyModule do
        @attr List
        @attr.flatten(list)
      end
      """

      %{
        actual_subject: actual_subject,
        docs: %{docs: docs}
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
        docs: %{docs: docs}
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
        docs: %{docs: docs}
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
        docs: %{docs: docs}
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
        docs: %{docs: docs}
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
    test "retrieve function documentation with __MODULE__" do
      buffer = """
      defmodule Inspect do
        def func(list) do
          __MODULE__.Algebra.string(list)
        end
      end
      """

      %{
        actual_subject: actual_subject,
        docs: %{docs: docs}
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
        MyList.flatten
      end
      """

      %{
        actual_subject: actual_subject,
        docs: %{docs: docs}
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

    test "retrive function documentation from imported modules" do
      buffer = """
      defmodule MyModule do
        import Mix.Generator
        create_file(
      end
      """

      %{
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 3, 5)

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

      %{actual_subject: actual_subject, docs: %{docs: docs}} = ElixirSense.docs(buffer, 2, 5)

      assert actual_subject == "Kernel.defmacro"

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
        actual_subject: actual_subject,
        docs: %{docs: docs}
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

    test "retrieve documentation from erlang modules" do
      buffer = """
      defmodule MyModule do
        alias :erlang, as: Erl
      end
      """

      %{
        actual_subject: actual_subject,
        docs: %{docs: docs}
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
        docs: %{docs: docs_1}
      } = ElixirSense.docs(buffer, 2, 30)

      %{
        actual_subject: actual_subject_2,
        docs: %{docs: docs_2}
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

      %{actual_subject: actual_subject, docs: %{docs: docs}} = ElixirSense.docs(buffer, 2, 11)

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
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 3, 31)

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
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 3, 35)

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
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 3, 31)

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
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 2, 23)

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
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 2, 19)

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
        actual_subject: actual_subject,
        docs: %{docs: docs}
      } = ElixirSense.docs(buffer, 2, 18)

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
      docs: %{docs: docs}
    } = ElixirSense.docs(buffer, 3, 5)

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
      actual_subject: actual_subject,
      docs: %{docs: docs}
    } = ElixirSense.docs(buffer, 3, 5)

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
      actual_subject: actual_subject,
      docs: %{docs: docs}
    } = ElixirSense.docs(buffer, 3, 5)

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
      actual_subject: actual_subject,
      docs: %{docs: docs}
    } = ElixirSense.docs(buffer, 1, 16)

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
      actual_subject: actual_subject,
      docs: %{docs: docs}
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
      actual_subject: actual_subject,
      docs: %{docs: docs}
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
             docs: %{docs: docs}
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
             docs: %{docs: docs}
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
               docs: %{docs: docs}
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
             docs: %{docs: docs}
           } = ElixirSense.docs(buffer, 2, 12)

    assert docs =~ "variable"

    assert %{
             actual_subject: "other_var",
             docs: %{docs: docs}
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
             docs: %{docs: docs}
           } = ElixirSense.docs(buffer, 7, 6)

    assert docs =~ "variable"

    assert %{
             actual_subject: "length",
             docs: %{docs: docs}
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

    # TODO assert type
    assert %{actual_subject: "ElixirSenseExample.Some.some_local"} =
             ElixirSense.docs(buffer, 9, 9)

    # TODO assert function
  end
end
