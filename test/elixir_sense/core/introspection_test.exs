defmodule ElixirSense.Core.IntrospectionTest do
  use ExUnit.Case, async: true
  doctest ElixirSense.Core.Introspection
  alias ElixirSense.Core.TypeInfo
  import ElixirSense.Core.Introspection

  test "format_spec_ast with one return option does not aplit the returns" do
    type_ast = TypeInfo.get_type_ast(GenServer, :debug)

    assert format_spec_ast(type_ast) == """
           debug :: [:trace | :log | :statistics | {:log_to_file, Path.t}]\
           """
  end

  test "format_spec_ast with more than one return option aplits the returns" do
    type_ast = TypeInfo.get_type_ast(GenServer, :on_start)

    assert format_spec_ast(type_ast) == """
           on_start ::
             {:ok, pid} |
             :ignore |
             {:error, {:already_started, pid} | term}\
           """
  end

  test "format_spec_ast for callback with opaque" do
    ast = get_callback_ast(ElixirSenseExample.CallbackOpaque, :do_stuff, 2)

    assert format_spec_ast(ast) == """
           do_stuff(t(a), term) :: t(a) when a: any\
           """
  end

  test "format_spec_ast for macrocallback" do
    ast =
      get_callback_ast(ElixirSenseExample.BehaviourWithMacrocallback, :required, 1)
      |> remove_first_macro_arg()

    assert format_spec_ast(ast) == """
           required(atom) :: Macro.t\
           """
  end

  test "format_spec_ast for callback" do
    ast = get_callback_ast(GenServer, :code_change, 3)

    assert format_spec_ast(ast) == """
           code_change(old_vsn, state :: term, extra :: term) ::
             {:ok, new_state :: term} |
             {:error, reason :: term} when old_vsn: term | {:down, term}\
           """
  end

  test "get_callbacks_with_docs for with opaque" do
    assert get_callbacks_with_docs(ElixirSenseExample.CallbackOpaque) == [
             %{
               name: :do_stuff,
               arity: 2,
               callback: """
               @callback do_stuff(t(a), term) :: t(a) when a: any\
               """,
               signature: "do_stuff(t, term)",
               doc: "Does stuff to opaque arg\n",
               metadata: %{optional: false},
               kind: :callback
             }
           ]
  end

  test "get_callbacks_with_docs for erlang behaviours" do
    assert [
             %{
               arity: 0,
               callback: "@callback callback_mode :: callback_mode_result",
               doc: summary,
               kind: :callback,
               metadata: %{optional: false},
               name: :callback_mode,
               signature: "callback_mode()"
             }
           ] = get_callbacks_with_docs(:gen_statem) |> Enum.filter(&(&1.name == :callback_mode))

    if ExUnitConfig.erlang_eep48_supported() do
      assert "- CallbackMode = " <> _ = summary
    end
  end

  test "get_callbacks_with_docs for Elixir behaviours with no docs defined" do
    assert get_callbacks_with_docs(Exception) == [
             %{
               arity: 2,
               name: :blame,
               callback: "@callback blame(t, stacktrace) :: {t, stacktrace}",
               doc:
                 "Called from `Exception.blame/3` to augment the exception struct.\n\nCan be used to collect additional information about the exception\nor do some additional expensive computation.\n",
               signature: "blame(t, stacktrace)",
               metadata: %{optional: true},
               kind: :callback
             },
             %{
               arity: 1,
               name: :exception,
               doc: nil,
               callback: "@callback exception(term) :: t",
               signature: "exception(term)",
               metadata: %{optional: false},
               kind: :callback
             },
             %{
               arity: 1,
               name: :message,
               callback: "@callback message(t) :: String.t",
               doc: nil,
               signature: "message(t)",
               metadata: %{optional: false},
               kind: :callback
             }
           ]
  end

  test "get_callbacks_with_docs for Elixir behaviours with docs defined" do
    info =
      get_callbacks_with_docs(GenServer) |> Enum.find(fn info -> info.name == :code_change end)

    assert info.name == :code_change
    assert info.arity == 3

    assert info.callback =~ """
           @callback code_change(old_vsn, state :: term, extra :: term) ::
             {:ok, new_state :: term} |
           """

    assert info.doc =~ "Invoked to change the state of the `GenServer`"
    assert info.signature == "code_change(old_vsn, state, extra)"
  end

  test "get_returns_from_callback" do
    returns = get_returns_from_callback(GenServer, :code_change, 3)

    assert [
             %{
               description: "{:ok, new_state}",
               snippet: "{:ok, \"${1:new_state}$\"}",
               spec: "{:ok, new_state :: term} when old_vsn: term" <> _
             }
             | _
           ] = returns
  end

  test "get_returns_from_macrocallback" do
    returns =
      get_returns_from_callback(ElixirSenseExample.BehaviourWithMacrocallback, :required, 1)

    assert [%{description: "Macro.t", snippet: "\"${1:Macro.t}$\"", spec: "Macro.t"}] = returns
  end

  test "get_returns_from_callback (all types in 'when')" do
    returns = get_returns_from_callback(ElixirSenseExample.ExampleBehaviour, :handle_call, 3)

    assert [
             %{
               description: "{:reply, reply, new_state}",
               snippet: "{:reply, \"${1:reply}$\", \"${2:new_state}$\"}",
               spec: "{:reply, reply, new_state} when reply: term, new_state: term, reason: term"
             },
             %{
               description:
                 "{:reply, reply, new_state, timeout | :hibernate | {:continue, term}}",
               snippet:
                 "{:reply, \"${1:reply}$\", \"${2:new_state}$\", \"${3:timeout | :hibernate | {:continue, term}}$\"}",
               spec: "{:reply, reply, new_state, timeout | :hibernate | {:continue, term}}" <> _
             },
             %{
               description: "{:noreply, new_state}",
               snippet: "{:noreply, \"${1:new_state}$\"}",
               spec: "{:noreply, new_state} when reply: term, new_state: term, reason: term"
             },
             %{
               description: "{:noreply, new_state, timeout | :hibernate | {:continue, term}}",
               snippet:
                 "{:noreply, \"${1:new_state}$\", \"${2:timeout | :hibernate | {:continue, term}}$\"}",
               spec: "{:noreply, new_state, timeout | :hibernate | {:continue, term}}" <> _
             },
             %{
               description: "{:stop, reason, reply, new_state}",
               snippet: "{:stop, \"${1:reason}$\", \"${2:reply}$\", \"${3:new_state}$\"}",
               spec:
                 "{:stop, reason, reply, new_state} when reply: term, new_state: term, reason: term"
             },
             %{
               description: "{:stop, reason, new_state}",
               snippet: "{:stop, \"${1:reason}$\", \"${2:new_state}$\"}",
               spec: "{:stop, reason, new_state} when reply: term, new_state: term, reason: term"
             }
           ] = returns
  end

  test "get_returns_from_callback (erlang specs)" do
    returns = get_returns_from_callback(:gen_fsm, :handle_event, 3)

    assert returns == [
             %{
               description: "{:next_state, nextStateName, newStateData}",
               snippet: "{:next_state, \"${1:nextStateName}$\", \"${2:newStateData}$\"}",
               spec: "{:next_state, nextStateName :: atom, newStateData :: term}"
             },
             %{
               description: "{:next_state, nextStateName, newStateData, timeout | :hibernate}",
               snippet:
                 "{:next_state, \"${1:nextStateName}$\", \"${2:newStateData}$\", \"${3:timeout | :hibernate}$\"}",
               spec:
                 "{:next_state, nextStateName :: atom, newStateData :: term, timeout | :hibernate}"
             },
             %{
               description: "{:stop, reason, newStateData}",
               snippet: "{:stop, \"${1:reason}$\", \"${2:newStateData}$\"}",
               spec: "{:stop, reason :: term, newStateData :: term}"
             }
           ]
  end

  test "actual_mod_fun Elixir proxy" do
    # Elixir is not a valid module
    assert {Elixir, nil, false} = actual_mod_fun({Elixir, nil}, [], [], [], nil, %{}, %{})

    # But defines some types: Code.Typespec.fetch_types(Elixir) returns keyword, as_boolean and other elixir builtins
    # we do not support that as such types compile fine but are marked as unknown by dialyzer
    # no longer true - on elixir 1.14 Code.Typespec.fetch_types(Elixir) returns :error
    assert {Elixir, :keyword, false} =
             actual_mod_fun({Elixir, :keyword}, [], [], [], nil, %{}, %{})

    # not found
    assert {Elixir, :asdf, false} = actual_mod_fun({Elixir, :asdf}, [], [], [], nil, %{}, %{})
  end

  test "actual_mod_fun :erlang builtings" do
    assert {:erlang, :andalso, true} =
             actual_mod_fun({:erlang, :andalso}, [], [], [], nil, %{}, %{})

    assert {:erlang, :orelse, true} =
             actual_mod_fun({:erlang, :orelse}, [], [], [], nil, %{}, %{})
  end

  describe "actual_mod_fun and requires" do
    test "finds only macros from required modules" do
      assert {Logger, :info, false} = actual_mod_fun({Logger, :info}, [], [], [], nil, %{}, %{})

      assert {Logger, :info, true} =
              actual_mod_fun({Logger, :info}, [], [Logger], [], nil, %{}, %{})
    end

    test "finds only public macros from required metadata modules" do
      for kind <- [:defmacro, :defmacrop, :defguard, :defguardp] do
        macro_info = %ElixirSense.Core.State.ModFunInfo{
          type: kind
        }

        mod_fun = %{
          {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{},
          {MyModule, :info, nil} => macro_info,
          {MyModule, :info, 1} => macro_info
        }

        findable = kind in [:defmacro, :defguard]

        assert {MyModule, :info, false} =
                actual_mod_fun({MyModule, :info}, [], [], [], nil, mod_fun, %{})

        assert {MyModule, :info, ^findable} =
                actual_mod_fun({MyModule, :info}, [], [MyModule], [], nil, mod_fun, %{})
      end
    end
  end

  describe "actual_mod_fun and local calls" do
    test "finds macros from Kernel.SpecialForms" do
      assert {Kernel.SpecialForms, :unquote, true} =
              actual_mod_fun({nil, :unquote}, [], [], [], nil, %{}, %{})
    end

    test "not existing local" do
      assert {nil, :not_existing, false} = actual_mod_fun({nil, :not_existing}, [], [], [], nil, %{}, %{})
    end

    test "module builtin functions cannot be called locally" do
      def_info = %ElixirSense.Core.State.ModFunInfo{
        type: :def
      }
      mod_fun = %{
        {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{},
        {MyModule, :__info__, nil} => def_info,
        {MyModule, :module_info, nil} => def_info,
        {MyModule, :behaviour_info, nil} => def_info
      }
      for fun <- [:module_info, :behaviour_info, :__info__] do
        assert {nil, ^fun, false} = actual_mod_fun({nil, fun}, [], [], [], MyModule, mod_fun, %{})
      end
    end

    test "actual_mod_fun finds functions and macros current module" do
      for kind <- [:def, :defp, :defmacro, :defmacrop, :defguard, :defguardp, :defdelegate] do
        def_info = %ElixirSense.Core.State.ModFunInfo{
          type: kind
        }

        mod_fun = %{
          {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{},
          {MyModule, :info, nil} => def_info,
          {MyModule, :info, 1} => def_info
        }

        assert {nil, :not_existing, false} =
              actual_mod_fun({nil, :not_existing}, [], [], [], MyModule, mod_fun, %{})

        assert {MyModule, :info, true} =
              actual_mod_fun({nil, :info}, [], [], [], MyModule, mod_fun, %{})
      end
    end

    test "finds types from current module" do
      mod_fun = %{
        {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{},
      }

      for kind <- [:type, :typep, :opaque] do
        type_info = %ElixirSense.Core.State.TypeInfo{
          kind: kind
        }
        types = %{
          {MyModule, :info, nil} => type_info,
          {MyModule, :info, 1} => type_info
        }
        assert {MyModule, :info, true} =
              actual_mod_fun({nil, :info}, [], [], [], MyModule, mod_fun, types)
      end
    end

    test "finds builtin types" do
      types = ElixirSense.Core.BuiltinTypes.all()
      |> Enum.map(& &1 |> elem(0) |> String.to_atom)

      for type <- types do
        assert {nil, ^type, true} =
                actual_mod_fun({nil, type}, [], [], [], nil, %{}, %{})
      end
    end
  end

  describe "actual_mod_fun and imports" do
    test "finds functions from imported modules" do
      assert {nil, :at, false} = actual_mod_fun({nil, :at}, [], [], [], nil, %{}, %{})

      assert {Enum, :at, true} =
              actual_mod_fun({nil, :at}, [{Enum, []}], [], [], nil, %{}, %{})
    end

    test "behaviour_info can be from some modules imported" do
      assert {nil, :behaviour_info, false} =
              actual_mod_fun({nil, :behaviour_info}, [{Application, []}], [], [], nil, %{}, %{})
      assert {:gen_server, :behaviour_info, true} =
              actual_mod_fun({nil, :behaviour_info}, [{:gen_server, []}], [], [], nil, %{}, %{})
    end

    test "types are not imported" do
      assert {nil, :t, false} =
              actual_mod_fun({nil, :t}, [{Enum, []}], [], [], nil, %{}, %{})
    end

    test "finds macros from imported modules" do
      assert {nil, :info, false} = actual_mod_fun({nil, :info}, [], [], [], nil, %{}, %{})

      assert {Logger, :info, true} =
              actual_mod_fun({nil, :info}, [{Logger, []}], [], [], nil, %{}, %{})
    end

    test "respects import options" do
      assert {Enum, :at, true} =
              actual_mod_fun({nil, :at}, [{Enum, []}], [], [], nil, %{}, %{})
      assert {nil, :at, false} =
              actual_mod_fun({nil, :at}, [{Enum, [only: [abc: 1]]}], [], [], nil, %{}, %{})
      assert {nil, :at, false} =
              actual_mod_fun({nil, :at}, [{Enum, [except: [at: 2, at: 3]]}], [], [], nil, %{}, %{})
      assert {nil, :at, false} =
              actual_mod_fun({nil, :at}, [{Enum, [only: :macros]}], [], [], nil, %{}, %{})
    end
  end

  describe "actual_mod_fun and remote calls" do
    test "finds functions from remote modules" do
      assert {Enum, :at, true} =
              actual_mod_fun({Enum, :at}, [], [], [], nil, %{}, %{})

      assert {Enum, :not_existing, false} =
              actual_mod_fun({Enum, :not_existing}, [], [], [], nil, %{}, %{})
    end

    test "finds public functions from metadata modules" do
      for kind <- [:def, :defp, :defdelegate] do
        def_info = %ElixirSense.Core.State.ModFunInfo{
          type: kind
        }

        mod_fun = %{
          {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{},
          {MyModule, :info, nil} => def_info,
          {MyModule, :info, 1} => def_info
        }

        findable = kind != :defp

        assert {MyModule, :info, ^findable} =
                actual_mod_fun({MyModule, :info}, [], [], [], nil, mod_fun, %{})
      end

      assert {MyModule, :not_existing, false} =
                actual_mod_fun({MyModule, :not_existing}, [], [], [], nil, %{}, %{})
    end

    test "module builtin functions can be called remotely" do
      def_info = %ElixirSense.Core.State.ModFunInfo{
        type: :def
      }
      mod_fun = %{
        {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{},
        {MyModule, :__info__, nil} => def_info,
        {MyModule, :module_info, nil} => def_info,
        {MyModule, :behaviour_info, nil} => def_info
      }
      for fun <- [:module_info, :__info__, :behaviour_info], module <- [MyModule, Application] do
        assert {^module, ^fun, true} = actual_mod_fun({module, fun}, [], [], [], nil, mod_fun, %{})
      end
    end

    test "finds types from remote modules" do
      assert {Enum, :t, true} =
              actual_mod_fun({Enum, :t}, [], [], [], nil, %{}, %{})
    end

    test "finds public metadata types from remote modules" do
      mod_fun = %{
        {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{},
      }

      for kind <- [:type, :typep, :opaque] do
        type_info = %ElixirSense.Core.State.TypeInfo{
          kind: kind
        }
        types = %{
          {MyModule, :info, nil} => type_info,
          {MyModule, :info, 1} => type_info
        }
        findable = kind != :typep
        assert {MyModule, :info, ^findable} =
              actual_mod_fun({MyModule, :info}, [], [], [], nil, mod_fun, types)
      end
    end
  end

  describe "actual_mod_fun modules" do
    test "finds modules" do
      assert {Enum, nil, true} =
              actual_mod_fun({Enum, nil}, [], [], [], nil, %{}, %{})

      assert {:lists, nil, true} =
              actual_mod_fun({:lists, nil}, [], [], [], nil, %{}, %{})
    end

    test "finds metadata modules" do
      mod_fun = %{
        {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{},
      }

      assert {MyModule, nil, true} =
              actual_mod_fun({MyModule, nil}, [], [], [], nil, mod_fun, %{})

      assert {MyModule, nil, false} =
              actual_mod_fun({MyModule, nil}, [], [], [], nil, %{}, %{})
    end
  end

  describe "get_all_docs" do
    test "returns delageted metadata on functions" do
      assert %{docs: docs} =
               get_all_docs({ElixirSenseExample.ModuleWithDelegates, :delegated_fun}, SomeModule)

      assert docs == """
             > ElixirSenseExample.ModuleWithDelegates.delegated_fun(a, b)

             **Delegates to**
             ElixirSenseExample.ModuleWithDocs.some_fun_no_doc/2

             A delegated function

             """
    end

    test "returns since metadata on functions" do
      assert %{docs: docs} =
               get_all_docs({ElixirSenseExample.ModuleWithDocs, :some_fun}, SomeModule)

      assert docs == """
             > ElixirSenseExample.ModuleWithDocs.some_fun(a, b \\\\\\\\ nil)

             **Since**
             1.1.0

             An example fun

             """
    end

    test "returns deprecated metadata on functions" do
      assert %{docs: docs} =
               get_all_docs({ElixirSenseExample.ModuleWithDocs, :soft_deprecated_fun}, SomeModule)

      assert docs == """
             > ElixirSenseExample.ModuleWithDocs.soft_deprecated_fun(a)

             **Deprecated**
             This function will be removed in a future release

             An example fun

             """
    end

    test "returns since metadata on types" do
      assert %{docs: docs} =
               get_all_docs({ElixirSenseExample.ModuleWithDocs, :some_type}, SomeModule)

      assert docs == """
             > ElixirSenseExample.ModuleWithDocs.some_type()

             **Since**
             1.1.0

             ### Specs

             ```
             @type some_type() :: integer()
             ```

             An example type

             """
    end

    test "returns since metadata on modules" do
      assert %{docs: docs} = get_all_docs({ElixirSenseExample.ModuleWithDocs, nil}, SomeModule)

      assert docs == """
             > ElixirSenseExample.ModuleWithDocs

             **Since**
             1.2.3

             An example module
             """
    end

    test "returns since metadata on callbacks" do
      assert %{callbacks: callbacks} =
               get_all_docs({ElixirSenseExample.ModuleWithDocs, nil}, SomeModule)

      assert callbacks =~ """
             > some_callback(integer)

             **Since**
             1.1.0

             ### Specs

             ```
             @callback some_callback(integer) :: atom
             ```

             An example callback
             """
    end

    test "returns optional metadata on callbacks" do
      assert %{callbacks: callbacks} =
               get_all_docs({ElixirSenseExample.ModuleWithDocs, nil}, SomeModule)

      assert callbacks =~ """
             > soft_deprecated_callback(integer)

             **Deprecated**
             This callback will be removed in a future release
             **Optional**

             ### Specs

             ```
             @callback soft_deprecated_callback(integer) :: atom
             ```

             An example callback



             ---

             > soft_deprecated_macrocallback(integer)

             **Deprecated**
             This callback will be removed in a future release
             **Optional**

             ### Specs

             ```
             @macrocallback soft_deprecated_macrocallback(integer) :: atom
             ```

             An example macrocallback
             """
    end

    test "returns since metadata on types (module)" do
      assert %{types: types} = get_all_docs({ElixirSenseExample.ModuleWithDocs, nil}, SomeModule)

      assert types =~ """
             > ElixirSenseExample.ModuleWithDocs.opaque_type()

             **Opaque**

             ### Specs
             ```
             @opaque opaque_type
             ```

             An example opaque type
             """
    end
  end
end
