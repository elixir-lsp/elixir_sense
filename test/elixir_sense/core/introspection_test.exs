defmodule ElixirSense.Core.IntrospectionTest do
  use ExUnit.Case, async: true
  doctest ElixirSense.Core.Introspection
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.TypeInfo
  import ElixirSense.Core.Introspection
  alias ElixirSense.Core.State

  test "format_spec_ast with one return option does not split the returns" do
    type_ast = TypeInfo.get_type_ast(GenServer, :debug)

    assert format_spec_ast(type_ast) == """
           debug :: [:trace | :log | :statistics | {:log_to_file, Path.t}]\
           """
  end

  test "format_spec_ast with more than one return option splits the returns" do
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
    assert {Elixir, nil, false, nil} =
             actual_mod_fun({Elixir, nil}, [], [], [], nil, Abc, %{}, %{}, {1, 1})

    # But defines some types: Code.Typespec.fetch_types(Elixir) returns keyword, as_boolean and other elixir builtins
    # we do not support that as such types compile fine but are marked as unknown by dialyzer
    # no longer true - on elixir 1.14 Code.Typespec.fetch_types(Elixir) returns :error
    assert {Elixir, :keyword, false, nil} =
             actual_mod_fun({Elixir, :keyword}, [], [], [], nil, Abc, %{}, %{}, {1, 1})

    # not found
    assert {Elixir, :asdf, false, nil} =
             actual_mod_fun({Elixir, :asdf}, [], [], [], nil, Abc, %{}, %{}, {1, 1})
  end

  test "actual_mod_fun :erlang builtings" do
    assert {:erlang, :andalso, true, :mod_fun} =
             actual_mod_fun({:erlang, :andalso}, [], [], [], nil, Abc, %{}, %{}, {1, 1})

    assert {:erlang, :orelse, true, :mod_fun} =
             actual_mod_fun({:erlang, :orelse}, [], [], [], nil, Abc, %{}, %{}, {1, 1})
  end

  describe "actual_mod_fun and requires" do
    test "finds only macros from required modules" do
      assert {Logger, :info, false, nil} =
               actual_mod_fun({Logger, :info}, [], [], [], nil, Abc, %{}, %{}, {1, 1})

      assert {Logger, :info, true, :mod_fun} =
               actual_mod_fun({Logger, :info}, [], [Logger], [], nil, Abc, %{}, %{}, {1, 1})
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

        kind =
          if findable do
            :mod_fun
          end

        assert {MyModule, :info, false, nil} =
                 actual_mod_fun({MyModule, :info}, [], [], [], nil, Abc, mod_fun, %{}, {1, 1})

        assert {MyModule, :info, ^findable, ^kind} =
                 actual_mod_fun(
                   {MyModule, :info},
                   [],
                   [MyModule],
                   [],
                   nil,
                   Abc,
                   mod_fun,
                   %{},
                   {1, 1}
                 )
      end
    end
  end

  describe "actual_mod_fun and local calls" do
    test "finds macros from Kernel.SpecialForms" do
      assert {Kernel.SpecialForms, :unquote, true, :mod_fun} =
               actual_mod_fun({nil, :unquote}, [], [], [], nil, Abc, %{}, %{}, {1, 1})
    end

    test "not existing local" do
      assert {nil, :not_existing, false, nil} =
               actual_mod_fun({nil, :not_existing}, [], [], [], nil, Abc, %{}, %{}, {1, 1})
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
        assert {nil, ^fun, false, nil} =
                 actual_mod_fun({nil, fun}, [], [], [], MyModule, MyModule, mod_fun, %{}, {1, 1})
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

        assert {nil, :not_existing, false, nil} =
                 actual_mod_fun(
                   {nil, :not_existing},
                   [],
                   [],
                   [],
                   MyModule,
                   MyModule,
                   mod_fun,
                   %{},
                   {1, 1}
                 )

        assert {MyModule, :info, true, :mod_fun} =
                 actual_mod_fun(
                   {nil, :info},
                   [],
                   [],
                   [],
                   MyModule,
                   MyModule,
                   mod_fun,
                   %{},
                   {1, 1}
                 )
      end
    end

    test "finds types from current module" do
      mod_fun = %{
        {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{}
      }

      for kind <- [:type, :typep, :opaque] do
        type_info = %ElixirSense.Core.State.TypeInfo{
          kind: kind
        }

        types = %{
          {MyModule, :info, nil} => type_info,
          {MyModule, :info, 1} => type_info
        }

        assert {MyModule, :info, true, :type} =
                 actual_mod_fun(
                   {nil, :info},
                   [],
                   [],
                   [],
                   MyModule,
                   {:typespec, :a, 1},
                   mod_fun,
                   types,
                   {1, 1}
                 )
      end
    end

    test "finds builtin types" do
      types =
        ElixirSense.Core.BuiltinTypes.all()
        |> Enum.map(&(&1 |> elem(0) |> String.to_atom()))

      for type <- types do
        assert {nil, ^type, true, :type} =
                 actual_mod_fun(
                   {nil, type},
                   [],
                   [],
                   [],
                   MyModule,
                   {:typespec, :a, 1},
                   %{},
                   %{},
                   {1, 1}
                 )
      end
    end
  end

  describe "actual_mod_fun and imports" do
    test "finds functions from imported modules" do
      assert {nil, :at, false, nil} =
               actual_mod_fun({nil, :at}, [], [], [], nil, Elixir, %{}, %{}, {1, 1})

      assert {Enum, :at, true, :mod_fun} =
               actual_mod_fun({nil, :at}, [{Enum, []}], [], [], nil, Elixir, %{}, %{}, {1, 1})
    end

    test "finds public functions and macros from imported metadata modules" do
      for kind <- [:def, :defp, :defmacro, :defmacrop, :defguard, :defguardp, :defdelegate] do
        def_info = %ElixirSense.Core.State.ModFunInfo{
          type: kind,
          params: [
            [
              {:a, [line: 2, column: 11], nil}
            ]
          ]
        }

        mod_fun = %{
          {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{},
          {MyModule, :info, nil} => def_info,
          {MyModule, :info, 1} => def_info
        }

        findable = kind not in [:defp, :defmacrop, :defguardp]

        expected_module =
          if findable do
            MyModule
          end

        kind =
          if findable do
            :mod_fun
          end

        assert {^expected_module, :info, ^findable, ^kind} =
                 actual_mod_fun(
                   {nil, :info},
                   [{MyModule, []}],
                   [],
                   [],
                   nil,
                   Elixir,
                   mod_fun,
                   %{},
                   {1, 1}
                 )
      end
    end

    test "respects import options on metadata functions and macros" do
      def_info = %ElixirSense.Core.State.ModFunInfo{
        type: :def,
        params: [
          [
            {:a, [line: 2, column: 11], nil}
          ]
        ]
      }

      defmacro_info = %ElixirSense.Core.State.ModFunInfo{
        type: :defmacro,
        params: [
          [
            {:a, [line: 2, column: 11], nil}
          ]
        ]
      }

      default_args = %ElixirSense.Core.State.ModFunInfo{
        type: :def,
        params: [
          [
            {:a, [line: 2, column: 11], nil},
            {:\\, [line: 2, column: 16], [{:b, [line: 2, column: 14], nil}, nil]},
            {:c, [line: 2, column: 24], nil},
            {:\\, [line: 2, column: 29], [{:d, [line: 2, column: 27], nil}, [1]]}
          ]
        ]
      }

      mod_fun = %{
        {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{},
        {MyModule, :def_info, nil} => def_info,
        {MyModule, :def_info, 1} => def_info,
        {MyModule, :defmacro_info, nil} => defmacro_info,
        {MyModule, :defmacro_info, 1} => defmacro_info,
        {MyModule, :default_args, nil} => default_args,
        {MyModule, :default_args, 4} => default_args
      }

      assert {MyModule, :def_info, true, :mod_fun} =
               actual_mod_fun(
                 {nil, :def_info},
                 [{MyModule, []}],
                 [],
                 [],
                 nil,
                 Elixir,
                 mod_fun,
                 %{},
                 {1, 1}
               )

      assert {nil, :def_info, false, nil} =
               actual_mod_fun(
                 {nil, :def_info},
                 [{MyModule, [only: :macros]}],
                 [],
                 [],
                 nil,
                 Elixir,
                 mod_fun,
                 %{},
                 {1, 1}
               )

      assert {nil, :def_info, false, nil} =
               actual_mod_fun(
                 {nil, :def_info},
                 [{MyModule, [except: [{:def_info, 1}]]}],
                 [],
                 [],
                 nil,
                 Elixir,
                 mod_fun,
                 %{},
                 {1, 1}
               )

      assert {MyModule, :defmacro_info, true, :mod_fun} =
               actual_mod_fun(
                 {nil, :defmacro_info},
                 [{MyModule, [only: :macros]}],
                 [],
                 [],
                 nil,
                 Elixir,
                 mod_fun,
                 %{},
                 {1, 1}
               )

      assert {MyModule, :default_args, true, :mod_fun} =
               actual_mod_fun(
                 {nil, :default_args},
                 [{MyModule, [only: [{:default_args, 2}]]}],
                 [],
                 [],
                 nil,
                 Elixir,
                 mod_fun,
                 %{},
                 {1, 1}
               )

      assert {MyModule, :default_args, true, :mod_fun} =
               actual_mod_fun(
                 {nil, :default_args},
                 [{MyModule, [except: [{:default_args, 4}]]}],
                 [],
                 [],
                 nil,
                 Elixir,
                 mod_fun,
                 %{},
                 {1, 1}
               )
    end

    test "behaviour_info can be imported from from erlang behaviours on elixir < 1.15" do
      assert {nil, :behaviour_info, false, nil} =
               actual_mod_fun(
                 {nil, :behaviour_info},
                 [{Application, []}],
                 [],
                 [],
                 nil,
                 Elixir,
                 %{},
                 %{},
                 {1, 1}
               )

      erl_behaviour_result =
        actual_mod_fun(
          {nil, :behaviour_info},
          [{:gen_server, []}],
          [],
          [],
          nil,
          Elixir,
          %{},
          %{},
          {1, 1}
        )

      if Version.match?(System.version(), ">= 1.15.0") do
        assert {nil, :behaviour_info, false, nil} = erl_behaviour_result
      else
        assert {:gen_server, :behaviour_info, true, :mod_fun} = erl_behaviour_result
      end
    end

    test "types are not imported" do
      assert {nil, :t, false, nil} =
               actual_mod_fun(
                 {nil, :t},
                 [{Enum, []}],
                 [],
                 [],
                 MyModule,
                 {:typespec, :a, 1},
                 %{},
                 %{},
                 {1, 1}
               )
    end

    test "finds macros from imported modules" do
      assert {nil, :info, false, nil} =
               actual_mod_fun({nil, :info}, [], [], [], nil, Elixir, %{}, %{}, {1, 1})

      assert {Logger, :info, true, :mod_fun} =
               actual_mod_fun({nil, :info}, [{Logger, []}], [], [], nil, Elixir, %{}, %{}, {1, 1})
    end

    test "respects import options" do
      assert {Enum, :at, true, :mod_fun} =
               actual_mod_fun({nil, :at}, [{Enum, []}], [], [], nil, Elixir, %{}, %{}, {1, 1})

      assert {nil, :at, false, nil} =
               actual_mod_fun(
                 {nil, :at},
                 [{Enum, [only: [abc: 1]]}],
                 [],
                 [],
                 nil,
                 Elixir,
                 %{},
                 %{},
                 {1, 1}
               )

      assert {nil, :at, false, nil} =
               actual_mod_fun(
                 {nil, :at},
                 [{Enum, [except: [at: 2, at: 3]]}],
                 [],
                 [],
                 nil,
                 Elixir,
                 %{},
                 %{},
                 {1, 1}
               )

      assert {nil, :at, false, nil} =
               actual_mod_fun(
                 {nil, :at},
                 [{Enum, [only: :macros]}],
                 [],
                 [],
                 nil,
                 Elixir,
                 %{},
                 %{},
                 {1, 1}
               )
    end
  end

  describe "actual_mod_fun and remote calls" do
    test "finds functions from remote modules" do
      assert {Enum, :at, true, :mod_fun} =
               actual_mod_fun({Enum, :at}, [], [], [], nil, Elixir, %{}, %{}, {1, 1})

      assert {Enum, :not_existing, false, nil} =
               actual_mod_fun({Enum, :not_existing}, [], [], [], nil, Elixir, %{}, %{}, {1, 1})
    end

    test "does not find functions in typespec" do
      assert {Enum, :at, false, nil} =
               actual_mod_fun(
                 {Enum, :at},
                 [],
                 [],
                 [],
                 MyModule,
                 {:typespec, :a, 1},
                 %{},
                 %{},
                 {1, 1}
               )
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
        kind = if(findable, do: :mod_fun)

        assert {MyModule, :info, ^findable, ^kind} =
                 actual_mod_fun({MyModule, :info}, [], [], [], nil, Elixir, mod_fun, %{}, {1, 1})
      end

      assert {MyModule, :not_existing, false, nil} =
               actual_mod_fun(
                 {MyModule, :not_existing},
                 [],
                 [],
                 [],
                 nil,
                 Elixir,
                 %{},
                 %{},
                 {1, 1}
               )
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
        assert {^module, ^fun, true, :mod_fun} =
                 actual_mod_fun({module, fun}, [], [], [], nil, Elixir, mod_fun, %{}, {1, 1})
      end
    end

    test "finds types from remote modules" do
      assert {Enum, :t, true, :type} =
               actual_mod_fun(
                 {Enum, :t},
                 [],
                 [],
                 [],
                 MyModule,
                 {:typespec, :a, 1},
                 %{},
                 %{},
                 {1, 1}
               )
    end

    test "does not find types outside typespec scope" do
      assert {Enum, :t, false, nil} =
               actual_mod_fun({Enum, :t}, [], [], [], MyModule, {:a, 1}, %{}, %{}, {1, 1})

      assert {Enum, :t, false, nil} =
               actual_mod_fun({Enum, :t}, [], [], [], MyModule, MyModule, %{}, %{}, {1, 1})

      assert {Enum, :t, false, nil} =
               actual_mod_fun({Enum, :t}, [], [], [], nil, Elixir, %{}, %{}, {1, 1})
    end

    test "finds public metadata types from remote modules" do
      mod_fun = %{
        {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{}
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

        kind =
          if findable do
            :type
          end

        assert {MyModule, :info, ^findable, ^kind} =
                 actual_mod_fun(
                   {MyModule, :info},
                   [],
                   [],
                   [],
                   MyModule,
                   {:typespec, :a, 1},
                   mod_fun,
                   types,
                   {1, 1}
                 )
      end
    end
  end

  describe "actual_mod_fun modules" do
    test "finds modules" do
      assert {Enum, nil, true, :mod_fun} =
               actual_mod_fun({Enum, nil}, [], [], [], nil, Elixir, %{}, %{}, {1, 1})

      assert {:lists, nil, true, :mod_fun} =
               actual_mod_fun({:lists, nil}, [], [], [], nil, Elixir, %{}, %{}, {1, 1})
    end

    test "finds modules in typespec scope" do
      assert {Enum, nil, true, :mod_fun} =
               actual_mod_fun(
                 {Enum, nil},
                 [],
                 [],
                 [],
                 MyModule,
                 {:typespec, :a, 1},
                 %{},
                 %{},
                 {1, 1}
               )
    end

    test "finds metadata modules" do
      mod_fun = %{
        {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{}
      }

      assert {MyModule, nil, true, :mod_fun} =
               actual_mod_fun({MyModule, nil}, [], [], [], nil, Elixir, mod_fun, %{}, {1, 1})

      assert {MyModule, nil, false, nil} =
               actual_mod_fun({MyModule, nil}, [], [], [], nil, Elixir, %{}, %{}, {1, 1})
    end
  end

  describe "get_all_docs" do
    test "returns delegated metadata on functions" do
      assert docs =
               get_all_docs(
                 {ElixirSenseExample.ModuleWithDelegates, :delegated_fun, 2},
                 %Metadata{},
                 %State.Env{},
                 :mod_fun
               )

      assert docs == """
             > ElixirSenseExample.ModuleWithDelegates.delegated_fun(a, b)

             **Delegates to**
             ElixirSenseExample.ModuleWithDocs.some_fun_no_doc/2

             A delegated function

             """
    end

    test "returns since metadata on functions" do
      assert docs =
               get_all_docs(
                 {ElixirSenseExample.ModuleWithDocs, :some_fun, 2},
                 %Metadata{},
                 %State.Env{},
                 :mod_fun
               )

      assert docs == """
             > ElixirSenseExample.ModuleWithDocs.some_fun(a, b \\\\\\\\ nil)

             **Since**
             1.1.0

             An example fun

             """
    end

    test "returns deprecated metadata on functions" do
      assert docs =
               get_all_docs(
                 {ElixirSenseExample.ModuleWithDocs, :soft_deprecated_fun, 1},
                 %Metadata{},
                 %State.Env{},
                 :mod_fun
               )

      assert docs == """
             > ElixirSenseExample.ModuleWithDocs.soft_deprecated_fun(a)

             **Deprecated**
             This function will be removed in a future release

             An example fun

             """
    end

    test "returns since metadata on types" do
      assert docs =
               get_all_docs(
                 {ElixirSenseExample.ModuleWithDocs, :some_type, 0},
                 %Metadata{},
                 %State.Env{},
                 :type
               )

      assert docs == """
             > ElixirSenseExample.ModuleWithDocs.some_type()

             **Since**
             1.1.0

             ### Definition

             ```
             @type some_type() :: integer()
             ```

             An example type

             """
    end

    test "returns since metadata on modules" do
      assert docs =
               get_all_docs(
                 {ElixirSenseExample.ModuleWithDocs, nil, :any},
                 %Metadata{},
                 %State.Env{},
                 :mod_fun
               )

      assert docs == """
             > ElixirSenseExample.ModuleWithDocs

             **Since**
             1.2.3

             An example module
             """
    end
  end
end
