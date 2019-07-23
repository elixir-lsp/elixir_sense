defmodule Alchemist.Helpers.CompleteTest do
  use ExUnit.Case, async: true

  def expand(expr) do
    Alchemist.Helpers.Complete.expand(Enum.reverse expr)
  end

  test "erlang module completion" do
    assert expand(':zl') == {:yes, 'ib.', [%{name: "zlib", subtype: nil, summary: "", type: :module}]}
  end

  test "erlang module no completion" do
    assert expand(':unknown') == {:no, '', []}
    assert expand('Enum:') == {:no, '', []}
  end

  test "erlang module multiple values completion" do
    {:yes, '', list} = expand(':user')
    assert list |> Enum.find(& &1.name == "user")
    assert list |> Enum.find(& &1.name == "user_drv")
  end

  test "erlang root completion" do
    {:yes, '', list} = expand(':')
    assert is_list(list)
    assert list |> Enum.find(& &1.name == "lists")
  end

  test "elixir proxy" do
    {:yes, '', list} = expand('E')
    assert list |> Enum.find(& &1.name == "Elixir")
  end

  test "elixir completion" do
    assert expand('En') == {:yes, 'um', []}
    assert {:yes, 'ble.', [%{name: "Enumerable", subtype: :protocol, type: :module}]} = expand('Enumera')
  end

  test "elixir completion with self" do
    assert {:yes, '.', [%{name: "Enumerable", subtype: :protocol}]} = expand('Enumerable')
  end

  test "elixir completion on modules from load path" do
    assert {:yes, [], [
      %{name: "Stream", subtype: :struct, type: :module},
      %{name: "String", subtype: nil, type: :module},
      %{name: "StringIO", subtype: nil, type: :module}]} = expand('Str')
    assert {:yes, '', [
      %{name: "Macro"},
      %{name: "Map"},
      %{name: "MapSet"},
      %{name: "MatchError"},
    ]} = expand('Ma')
    assert {:yes, 't.', [%{name: "Dict"}]} = expand('Dic')
    assert {:yes, [], [
      %{name: "ExUnit"},
      %{name: "Exception"}]} = expand('Ex')
  end

  test "elixir no completion" do
    assert expand('.') == {:no, '', []}
    assert expand('Xyz') == {:no, '', []}
    assert expand('x.Foo') == {:no, '', []}
  end

  test "elixir root submodule completion" do
    assert {:yes, 'ss.', [%{name: "Access"}]} = expand('Elixir.Acce')
  end

  test "elixir submodule completion" do
    assert {:yes, 'rs.', [%{name: "Chars", subtype: :protocol}]} = expand('String.Cha')
  end

  test "elixir submodule no completion" do
    assert expand('IEx.Xyz') == {:no, '', []}
  end

  test "function completion" do
    assert {:yes, 'rsion', [%{name: "version", origin: "System"}]} = expand('System.ve')
    assert {:yes, 'ms', [%{name: "fun2ms", origin: ":ets"}]} = expand(':ets.fun2')
  end

  test "function completion with arity" do
    assert {:yes, '', [%{name: "printable?", origin: "String"}]} = expand('String.printable?')
    assert {:yes, '', [%{name: "printable?"}]} = expand('String.printable?/')
  end

  test "macro completion" do
    {:yes, '', list} = expand('Kernel.is_')
    assert is_list(list)
  end

  test "imports completion" do
    {:yes, '', list} = expand('')
    assert is_list(list)
    # IEx.Helpers import
    # assert list |> Enum.find(& &1.name == "h")
    assert list |> Enum.find(& &1.name == "unquote")
    # IEx.Helpers import
    # assert list |> Enum.find(& &1.name == "pwd")
  end

  test "kernel import completion" do
    assert {:yes, 'ct', [%{args: "fields", arity: 1, name: "defstruct", origin: "Kernel", spec: "", summary: "Defines a struct.", type: "macro"}]} = expand('defstru')
    assert {:yes, '', [
      %{arity: 2, name: "put_in"},
      %{arity: 3, name: "put_in"},
      %{arity: 3, name: "put_elem"},
    ]} = expand('put_')
  end

  test "kernel special form completion" do
    assert {:yes, 'icing', [%{name: "unquote_splicing", origin: "Kernel.SpecialForms"}]} = expand('unquote_spl')
  end

  test "completion inside expression" do
    assert expand('1 En') == {:yes, 'um', []}
    assert expand('Test(En') == {:yes, 'um', []}
    assert {:yes, 'ib.', [_]} = expand('Test :zl')
    assert {:yes, 'ib.', [_]} = expand('[:zl')
    assert {:yes, 'ib.', [_]} = expand('{:zl')
  end

  test "ampersand completion" do
    assert expand('&Enu') == {:yes, 'm', []}
    assert {:yes, [], [%{name: "all?"}, %{name: "any?"}, %{name: "at"}]} = expand('&Enum.a')
    assert {:yes, [], [%{name: "all?"}, %{name: "any?"}, %{name: "at"}]} = expand('f = &Enum.a')
  end

  defmodule SublevelTest.LevelA.LevelB do
  end

  test "elixir completion sublevel" do
    assert {:yes, 'LevelA.', [%{name: "LevelA"}]} = expand('Alchemist.Helpers.CompleteTest.SublevelTest.')
  end

  defmodule MyServer do
    def current_env do
      %Macro.Env{aliases: [{MyList, List}, {EList, :lists}]}
    end
  end

  test "complete aliases of elixir modules" do
    Application.put_env(:"alchemist.el", :aliases, [{MyList, List}])

    assert {:yes, 'ist.', [%{name: "MyList"}]} = expand('MyL')
    assert {:yes, '.', [%{name: "MyList"}]} = expand('MyList')
    assert {:yes, [], [%{arity: 2, name: "to_integer"}, %{arity: 1, name: "to_integer"}]} = expand('MyList.to_integer')
  end

  test "complete aliases of erlang modules" do
    Application.put_env(:"alchemist.el", :aliases, [{EList, :lists}])

    assert {:yes, 'ist.', [%{name: "EList"}]} = expand('EL')
    assert {:yes, '.', [%{name: "EList"}]} = expand('EList')
    assert {:yes, [], [
      %{arity: 2, name: "map"},
      %{arity: 3, name: "mapfoldl"},
      %{arity: 3, name: "mapfoldr"}]} = expand('EList.map')
  end

end
