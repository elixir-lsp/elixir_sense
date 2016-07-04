defmodule Alchemist.API.CompTest do

  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  alias Alchemist.API.Comp

  defp fixture(file) do
    Path.expand("../../fixtures/#{file}", __DIR__)
  end

  test "COMP request with empty hint" do
    assert capture_io(fn ->
      Comp.request(~s({"", "#{fixture("my_module.ex")}", 1}))
    end) =~ """

    import/2;macro;module,opts;Kernel.SpecialForms;Imports functions and macros from other modules.;
    quote/2;macro;opts,block;Kernel.SpecialForms;Gets the representation of any expression.;
    require/2;macro;module,opts;Kernel.SpecialForms;Requires a given module to be compiled and loaded.;
    END-OF-COMP
    """
  end

  test "COMP request without empty hint" do
    assert capture_io(fn ->
      Comp.request(~s({"is_b", "#{fixture("my_module.ex")}", 1}))
    end) =~ """
    is_b;hint
    is_binary/1;function;term;Kernel;Returns `true` if `term` is a binary\\; otherwise returns `false`.;@spec is_binary(term) :: boolean
    is_bitstring/1;function;term;Kernel;Returns `true` if `term` is a bitstring (including a binary)\\; otherwise returns `false`.;@spec is_bitstring(term) :: boolean
    is_boolean/1;function;term;Kernel;Returns `true` if `term` is either the atom `true` or the atom `false` (i.e.,\\na boolean)\\; otherwise returns `false`.;@spec is_boolean(term) :: boolean
    END-OF-COMP
    """
  end

  test "COMP request with an alias" do
    assert capture_io(fn ->
      Comp.request(~s({"MyList.flat", "#{fixture("my_module.ex")}", 3}))
    end) =~ """
    MyList.flatten;hint
    flatten/2;function;list,tail;List;Flattens the given `list` of nested lists.\\nThe list `tail` will be added at the end of\\nthe flattened list.;@spec flatten(deep_list, [elem]) :: [elem] when deep_list: [elem | deep_list], elem: var
    flatten/1;function;list;List;Flattens the given `list` of nested lists.;@spec flatten(deep_list) :: list when deep_list: [any | deep_list]
    END-OF-COMP
    """
  end

  test "COMP request with a module hint" do
    assert capture_io(fn ->
      Comp.request(~s({"Str", "#{fixture("my_module.ex")}", 1}))
    end) =~ """
    Str;hint
    Stream;module;struct;Module for creating and composing streams.
    String;module;;A String in Elixir is a UTF-8 encoded binary.
    StringIO;module;;Controls an IO device process that wraps a string.
    END-OF-COMP
    """
  end

  test "COMP request lists callbacks" do
    assert capture_io(fn ->
      Comp.request(~s({"", "#{fixture("my_server.ex")}", 3}))
    end) =~ """
    code_change/3;callback;old_vsn,state,extra;GenServer;Invoked to change the state of the `GenServer` when a different version of a\\nmodule is loaded (hot code swapping) and the state's term structure should be\\nchanged.;@callback code_change(old_vsn, state :: term, extra :: term) ::\\n  {:ok, new_state :: term} |\\n  {:error, reason :: term} when old_vsn: term | {:down, term}\\n
    format_status/2;callback;reason,pdict_and_state;GenServer;Invoked in some cases to retrieve a formatted version of the `GenServer` status.;@callback format_status(reason, pdict_and_state :: list) :: term when reason: :normal | :terminate\\n
    """
  end

  test "COMP request lists returns" do
    assert capture_io(fn ->
      Comp.request(~s({"", "#{fixture("my_server.ex")}", 5}))
    end) =~ """
    {:reply, reply, new_state};return;{:reply, reply, new_state} when reply: term, new_state: term, reason: term;{:reply, \"${1:reply}$\", \"${2:new_state}$\"}
    {:reply, reply, new_state, timeout | :hibernate};return;{:reply, reply, new_state, timeout | :hibernate} when reply: term, new_state: term, reason: term;{:reply, \"${1:reply}$\", \"${2:new_state}$\", \"${3:timeout | :hibernate}$\"}
    {:noreply, new_state};return;{:noreply, new_state} when reply: term, new_state: term, reason: term;{:noreply, \"${1:new_state}$\"}
    """
  end

  test "COMP request lists params and vars" do
    assert capture_io(fn ->
      Comp.request(~s({"", "#{fixture("my_server.ex")}", 5}))
    end) =~ """
    from;var
    request;var
    state;var
    var1;var
    """
  end

  test "COMP request lists attributes" do
    assert capture_io(fn ->
      Comp.request(~s({"", "#{fixture("my_module.ex")}", 5}))
    end) =~ """
    @my_attribute;attribute
    """
  end
end
