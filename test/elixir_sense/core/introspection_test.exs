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
               doc: "Does stuff to opaque arg\n"
             }
           ]
  end

  test "get_callbacks_with_docs for erlang behaviours" do
    assert get_callbacks_with_docs(:supervisor) == [
             %{
               name: :init,
               arity: 1,
               callback: """
               @callback init(args :: term) ::
                 {:ok, {supFlags :: sup_flags, [childSpec :: child_spec]}} |
                 :ignore\
               """,
               signature: "init(args)",
               doc: nil
             }
           ]
  end

  test "get_callbacks_with_docs for Elixir behaviours with no docs defined" do
    assert get_callbacks_with_docs(Exception) == [
             %{
               arity: 2,
               name: :blame,
               callback: "@callback blame(t, stacktrace) :: {t, stacktrace}",
               doc:
                 "Called from `Exception.blame/3` to augment the exception struct.\n\nCan be used to collect additional information about the exception\nor do some additional expensive computation.\n",
               signature: "blame(t, stacktrace)"
             },
             %{
               arity: 1,
               name: :exception,
               doc: nil,
               callback: "@callback exception(term) :: t",
               signature: "exception(term)"
             },
             %{
               arity: 1,
               name: :message,
               callback: "@callback message(t) :: String.t",
               doc: nil,
               signature: "message(t)"
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

    expected_returns = [
      %{
        description: "{:reply, reply, new_state}",
        snippet: "{:reply, \"${1:reply}$\", \"${2:new_state}$\"}",
        spec: "{:reply, reply, new_state} when reply: term, new_state: term, reason: term"
      },
      %{
        description: "{:reply, reply, new_state, timeout | :hibernate | {:continue, term}}",
        snippet:
          "{:reply, \"${1:reply}$\", \"${2:new_state}$\", \"${3:timeout | :hibernate | {:continue, term}}$\"}",
        spec:
          "{:reply, reply, new_state, timeout | :hibernate | {:continue, term}} when reply: term, new_state: term, reason: term"
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
        spec:
          "{:noreply, new_state, timeout | :hibernate | {:continue, term}} when reply: term, new_state: term, reason: term"
      },
      %{
        description: "{:stop, reason, reply, new_state}",
        snippet: "{:stop, \"${1:reason}$\", \"${2:reply}$\", \"${3:new_state}$\"}",
        spec: "{:stop, reason, reply, new_state} when reply: term, new_state: term, reason: term"
      },
      %{
        description: "{:stop, reason, new_state}",
        snippet: "{:stop, \"${1:reason}$\", \"${2:new_state}$\"}",
        spec: "{:stop, reason, new_state} when reply: term, new_state: term, reason: term"
      }
    ]

    assert returns == expected_returns
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

  test "actual_mod_fun when nil current module" do
    assert {nil, :some_fun, false} = actual_mod_fun({nil, :some_fun}, [], [], nil, %{}, %{})
  end

  test "actual_mod_fun Elixir module" do
    # Elixir is not a valid module
    assert {Elixir, nil, false} = actual_mod_fun({Elixir, nil}, [], [], nil, %{}, %{})

    # But defines some types: Code.Typespec.fetch_types(Elixir) returns keyword, as_boolean and other elixir builtins
    # we do not support that as such types compile fine but are marked as unknown by dialyzer
    assert {Elixir, :keyword, false} = actual_mod_fun({Elixir, :keyword}, [], [], nil, %{}, %{})
    # not found
    assert {Elixir, :asdf, false} = actual_mod_fun({Elixir, :asdf}, [], [], nil, %{}, %{})
  end

  test "actual_mod_fun :erlang builtings" do
    assert {:erlang, :andalso, true} = actual_mod_fun({:erlang, :andalso}, [], [], nil, %{}, %{})
    assert {:erlang, :orelse, true} = actual_mod_fun({:erlang, :orelse}, [], [], nil, %{}, %{})
  end
end
