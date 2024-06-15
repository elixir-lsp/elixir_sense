defmodule ElixirSense.Core.IntrospectionTest do
  use ExUnit.Case, async: true
  doctest ElixirSense.Core.Introspection
  alias ElixirSense.Core.TypeInfo
  import ElixirSense.Core.Introspection
  alias ElixirSense.Core.State.Env

  test "format_spec_ast with one return option does not split the returns" do
    type_ast = TypeInfo.get_type_ast(GenServer, :debug)

    assert format_spec_ast(type_ast) == """
           debug() :: [:trace | :log | :statistics | {:log_to_file, Path.t()}]\
           """
  end

  test "format_spec_ast with more than one return option splits the returns" do
    type_ast = TypeInfo.get_type_ast(GenServer, :on_start)

    assert format_spec_ast(type_ast) == """
           on_start() ::
             {:ok, pid()} |
             :ignore |
             {:error, {:already_started, pid()} | term()}\
           """
  end

  test "get_callbacks_with_docs for with opaque" do
    assert get_callbacks_with_docs(ElixirSenseExample.CallbackOpaque) == [
             %{
               name: :do_stuff,
               arity: 2,
               callback: """
               @callback do_stuff(t(a), term()) :: t(a) when a: any()\
               """,
               signature: "do_stuff(t, term)",
               doc: "Does stuff to opaque arg\n",
               metadata: %{optional: false, app: :elixir_sense},
               kind: :callback
             }
           ]
  end

  test "get_callbacks_with_docs for erlang behaviours" do
    assert [
             %{
               arity: 0,
               callback: "@callback callback_mode() :: callback_mode_result()",
               doc: summary,
               kind: :callback,
               metadata: %{optional: false},
               name: :callback_mode,
               signature: "callback_mode()"
             }
           ] = get_callbacks_with_docs(:gen_statem) |> Enum.filter(&(&1.name == :callback_mode))

    if System.otp_release() |> String.to_integer() >= 23 do
      if System.otp_release() |> String.to_integer() >= 27 do
        assert "This function is" <> _ = summary
      else
        assert "- CallbackMode = " <> _ = summary
      end
    end
  end

  test "get_callbacks_with_docs for Elixir behaviours with docs defined" do
    info =
      get_callbacks_with_docs(GenServer) |> Enum.find(fn info -> info.name == :code_change end)

    assert info.name == :code_change
    assert info.arity == 3

    assert info.callback =~ """
           @callback code_change(old_vsn, state :: term(), extra :: term()) ::
             {:ok, new_state :: term()} |
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
               spec: "{:ok, new_state :: term()} when old_vsn: term()" <> _
             }
             | _
           ] = returns
  end

  test "get_returns_from_macrocallback" do
    returns =
      get_returns_from_callback(ElixirSenseExample.BehaviourWithMacrocallback, :required, 1)

    assert [%{description: "Macro.t()", snippet: "\"${1:Macro.t()}$\"", spec: "Macro.t()"}] =
             returns
  end

  test "get_returns_from_callback (all types in 'when')" do
    returns = get_returns_from_callback(ElixirSenseExample.ExampleBehaviour, :handle_call, 3)

    assert [
             %{
               description: "{:reply, reply, new_state}",
               snippet: "{:reply, \"${1:reply}$\", \"${2:new_state}$\"}",
               spec:
                 "{:reply, reply, new_state} when reply: term(), new_state: term(), reason: term()"
             },
             %{
               description:
                 "{:reply, reply, new_state, timeout() | :hibernate | {:continue, term()}}",
               snippet:
                 "{:reply, \"${1:reply}$\", \"${2:new_state}$\", \"${3:timeout() | :hibernate | {:continue, term()}}$\"}",
               spec:
                 "{:reply, reply, new_state, timeout() | :hibernate | {:continue, term()}}" <> _
             },
             %{
               description: "{:noreply, new_state}",
               snippet: "{:noreply, \"${1:new_state}$\"}",
               spec: "{:noreply, new_state} when reply: term(), new_state: term(), reason: term()"
             },
             %{
               description: "{:noreply, new_state, timeout() | :hibernate | {:continue, term()}}",
               snippet:
                 "{:noreply, \"${1:new_state}$\", \"${2:timeout() | :hibernate | {:continue, term()}}$\"}",
               spec: "{:noreply, new_state, timeout() | :hibernate | {:continue, term()}}" <> _
             },
             %{
               description: "{:stop, reason, reply, new_state}",
               snippet: "{:stop, \"${1:reason}$\", \"${2:reply}$\", \"${3:new_state}$\"}",
               spec:
                 "{:stop, reason, reply, new_state} when reply: term(), new_state: term(), reason: term()"
             },
             %{
               description: "{:stop, reason, new_state}",
               snippet: "{:stop, \"${1:reason}$\", \"${2:new_state}$\"}",
               spec:
                 "{:stop, reason, new_state} when reply: term(), new_state: term(), reason: term()"
             }
           ] = returns
  end

  test "get_returns_from_callback (erlang specs)" do
    returns = get_returns_from_callback(:gen_fsm, :handle_event, 3)

    assert returns == [
             %{
               description: "{:next_state, nextStateName, newStateData}",
               snippet: "{:next_state, \"${1:nextStateName}$\", \"${2:newStateData}$\"}",
               spec: "{:next_state, nextStateName :: atom(), newStateData :: term()}"
             },
             %{
               description: "{:next_state, nextStateName, newStateData, timeout() | :hibernate}",
               snippet:
                 "{:next_state, \"${1:nextStateName}$\", \"${2:newStateData}$\", \"${3:timeout() | :hibernate}$\"}",
               spec:
                 "{:next_state, nextStateName :: atom(), newStateData :: term(), timeout() | :hibernate}"
             },
             %{
               description: "{:stop, reason, newStateData}",
               snippet: "{:stop, \"${1:reason}$\", \"${2:newStateData}$\"}",
               spec: "{:stop, reason :: term(), newStateData :: term()}"
             }
           ]
  end

  test "actual_mod_fun Elixir proxy" do
    # Elixir is not a valid module
    assert {Elixir, nil, false, nil} =
             actual_mod_fun({Elixir, nil}, %Env{}, %{}, %{}, {1, 1}, false)

    # But defines some types: Code.Typespec.fetch_types(Elixir) returns keyword, as_boolean and other elixir builtins
    # we do not support that as such types compile fine but are marked as unknown by dialyzer
    # no longer true - on elixir 1.14 Code.Typespec.fetch_types(Elixir) returns :error
    assert {Elixir, :keyword, false, nil} =
             actual_mod_fun({Elixir, :keyword}, %Env{}, %{}, %{}, {1, 1}, false)

    # not found
    assert {Elixir, :asdf, false, nil} =
             actual_mod_fun({Elixir, :asdf}, %Env{}, %{}, %{}, {1, 1}, false)
  end

  test "actual_mod_fun :erlang builtings" do
    assert {:erlang, :andalso, true, :mod_fun} =
             actual_mod_fun({:erlang, :andalso}, %Env{}, %{}, %{}, {1, 1}, false)

    assert {:erlang, :orelse, true, :mod_fun} =
             actual_mod_fun({:erlang, :orelse}, %Env{}, %{}, %{}, {1, 1}, false)
  end

  describe "actual_mod_fun and requires" do
    test "finds only macros from required modules" do
      assert {Logger, :info, false, nil} =
               actual_mod_fun({Logger, :info}, %Env{}, %{}, %{}, {1, 1}, false)

      assert {Logger, :info, true, :mod_fun} =
               actual_mod_fun({Logger, :info}, %Env{requires: [Logger]}, %{}, %{}, {1, 1}, false)
    end

    test "finds only public macros from required metadata modules" do
      for kind <- [:defmacro, :defmacrop, :defguard, :defguardp] do
        macro_info = %ElixirSense.Core.State.ModFunInfo{
          type: kind
        }

        mod_fun = %{
          {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{},
          {MyModule, :info, 1} => macro_info
        }

        findable = kind in [:defmacro, :defguard]

        kind =
          if findable do
            :mod_fun
          end

        assert {MyModule, :info, false, nil} =
                 actual_mod_fun({MyModule, :info}, %Env{}, mod_fun, %{}, {1, 1}, false)

        assert {MyModule, :info, ^findable, ^kind} =
                 actual_mod_fun(
                   {MyModule, :info},
                   %Env{requires: [MyModule]},
                   mod_fun,
                   %{},
                   {1, 1},
                   false
                 )
      end
    end
  end

  describe "actual_mod_fun and local calls" do
    test "finds macros from Kernel.SpecialForms" do
      assert {Kernel.SpecialForms, :unquote, true, :mod_fun} =
               actual_mod_fun({nil, :unquote}, %Env{}, %{}, %{}, {1, 1}, false)
    end

    test "not existing local" do
      assert {nil, :not_existing, false, nil} =
               actual_mod_fun({nil, :not_existing}, %Env{module: Some}, %{}, %{}, {1, 1}, false)
    end

    test "module builtin functions cannot be called locally" do
      def_info = %ElixirSense.Core.State.ModFunInfo{
        type: :def
      }

      mod_fun = %{
        {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{},
        {MyModule, :__info__, 1} => def_info,
        {MyModule, :module_info, 1} => def_info,
        {MyModule, :behaviour_info, 1} => def_info
      }

      for fun <- [:module_info, :behaviour_info, :__info__] do
        assert {nil, ^fun, false, nil} =
                 actual_mod_fun(
                   {nil, fun},
                   %Env{module: MyModule},
                   mod_fun,
                   %{},
                   {1, 1},
                   false
                 )
      end
    end

    test "actual_mod_fun finds functions and macros current module" do
      for kind <- [:def, :defp, :defmacro, :defmacrop, :defguard, :defguardp, :defdelegate] do
        def_info = %ElixirSense.Core.State.ModFunInfo{
          type: kind
        }

        mod_fun = %{
          {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{},
          {MyModule, :info, 1} => def_info
        }

        assert {nil, :not_existing, false, nil} =
                 actual_mod_fun(
                   {nil, :not_existing},
                   %Env{module: MyModule},
                   mod_fun,
                   %{},
                   {1, 1},
                   false
                 )

        assert {MyModule, :info, true, :mod_fun} =
                 actual_mod_fun(
                   {nil, :info},
                   %Env{module: MyModule},
                   mod_fun,
                   %{},
                   {1, 1},
                   false
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
          {MyModule, :info, 1} => type_info
        }

        assert {MyModule, :info, true, :type} =
                 actual_mod_fun(
                   {nil, :info},
                   %Env{module: MyModule, typespec: {:a, 1}},
                   mod_fun,
                   types,
                   {1, 1},
                   false
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
                   %Env{module: MyModule, typespec: {:a, 1}},
                   %{},
                   %{},
                   {1, 1},
                   false
                 )
      end
    end
  end

  describe "actual_mod_fun and imports" do
    test "finds functions from imported modules" do
      assert {nil, :at, false, nil} =
               actual_mod_fun({nil, :at}, %Env{}, %{}, %{}, {1, 1}, false)

      assert {Enum, :at, true, :mod_fun} =
               actual_mod_fun(
                 {nil, :at},
                 %Env{functions: [{Enum, [{:at, 2}]}]},
                 %{},
                 %{},
                 {1, 1},
                 false
               )
    end

    test "finds public functions and macros from imported metadata modules" do
      for kind <- [:def, :defmacro, :defguard, :defdelegate] do
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
          {MyModule, :info, 1} => def_info
        }

        assert {MyModule, :info, true, :mod_fun} =
                 actual_mod_fun(
                   {nil, :info},
                   %Env{functions: [{MyModule, [{:info, 1}]}]},
                   mod_fun,
                   %{},
                   {1, 1},
                   false
                 )
      end
    end

    test "finds macros from imported modules" do
      assert {nil, :info, false, nil} =
               actual_mod_fun({nil, :info}, %Env{}, %{}, %{}, {1, 1}, false)

      assert {Logger, :info, true, :mod_fun} =
               actual_mod_fun(
                 {nil, :info},
                 %Env{macros: [{Logger, [{:info, 1}]}]},
                 %{},
                 %{},
                 {1, 1},
                 false
               )
    end
  end

  describe "actual_mod_fun and remote calls" do
    test "finds functions from remote modules" do
      assert {Enum, :at, true, :mod_fun} =
               actual_mod_fun({Enum, :at}, %Env{}, %{}, %{}, {1, 1}, false)

      assert {Enum, :not_existing, false, nil} =
               actual_mod_fun(
                 {Enum, :not_existing},
                 %Env{},
                 %{},
                 %{},
                 {1, 1},
                 false
               )
    end

    test "does not find functions in typespec" do
      assert {Enum, :at, false, nil} =
               actual_mod_fun(
                 {Enum, :at},
                 %Env{typespec: {:a, 1}, module: Some},
                 %{},
                 %{},
                 {1, 1},
                 false
               )
    end

    test "finds public functions from metadata modules" do
      for kind <- [:def, :defp, :defdelegate] do
        def_info = %ElixirSense.Core.State.ModFunInfo{
          type: kind
        }

        mod_fun = %{
          {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{},
          {MyModule, :info, 1} => def_info
        }

        findable = kind != :defp
        kind = if(findable, do: :mod_fun)

        assert {MyModule, :info, ^findable, ^kind} =
                 actual_mod_fun(
                   {MyModule, :info},
                   %Env{},
                   mod_fun,
                   %{},
                   {1, 1},
                   false
                 )
      end

      assert {MyModule, :not_existing, false, nil} =
               actual_mod_fun(
                 {MyModule, :not_existing},
                 %Env{},
                 %{},
                 %{},
                 {1, 1},
                 false
               )
    end

    test "module builtin functions can be called remotely" do
      def_info = %ElixirSense.Core.State.ModFunInfo{
        type: :def
      }

      mod_fun = %{
        {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{},
        {MyModule, :__info__, 1} => def_info,
        {MyModule, :module_info, 1} => def_info,
        {MyModule, :behaviour_info, 1} => def_info
      }

      for fun <- [:module_info, :__info__, :behaviour_info], module <- [MyModule, Application] do
        assert {^module, ^fun, true, :mod_fun} =
                 actual_mod_fun({module, fun}, %Env{}, mod_fun, %{}, {1, 1}, false)
      end
    end

    test "finds types from remote modules" do
      assert {Enum, :t, true, :type} =
               actual_mod_fun(
                 {Enum, :t},
                 %Env{typespec: {:a, 1}, module: Some},
                 %{},
                 %{},
                 {1, 1},
                 false
               )
    end

    test "does not find types outside typespec scope" do
      assert {Enum, :t, false, nil} =
               actual_mod_fun(
                 {Enum, :t},
                 %Env{module: MyModule, function: {:a, 1}},
                 %{},
                 %{},
                 {1, 1},
                 false
               )

      assert {Enum, :t, false, nil} =
               actual_mod_fun({Enum, :t}, %Env{module: Some}, %{}, %{}, {1, 1}, false)

      assert {Enum, :t, false, nil} =
               actual_mod_fun({Enum, :t}, %Env{}, %{}, %{}, {1, 1}, false)
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
                   %Env{module: MyModule, typespec: {:a, 1}},
                   mod_fun,
                   types,
                   {1, 1},
                   false
                 )
      end
    end
  end

  describe "actual_mod_fun modules" do
    test "finds modules" do
      assert {Enum, nil, true, :mod_fun} =
               actual_mod_fun({Enum, nil}, %Env{}, %{}, %{}, {1, 1}, false)

      assert {:lists, nil, true, :mod_fun} =
               actual_mod_fun({:lists, nil}, %Env{}, %{}, %{}, {1, 1}, false)
    end

    test "finds modules in typespec scope" do
      assert {Enum, nil, true, :mod_fun} =
               actual_mod_fun(
                 {Enum, nil},
                 %Env{module: MyModule, typespec: {:a, 1}},
                 %{},
                 %{},
                 {1, 1},
                 false
               )
    end

    test "finds metadata modules" do
      mod_fun = %{
        {MyModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{}
      }

      assert {MyModule, nil, true, :mod_fun} =
               actual_mod_fun({MyModule, nil}, %Env{}, mod_fun, %{}, {1, 1}, false)

      assert {MyModule, nil, false, nil} =
               actual_mod_fun({MyModule, nil}, %Env{}, %{}, %{}, {1, 1}, false)
    end
  end
end
