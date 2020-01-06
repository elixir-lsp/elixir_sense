defmodule Alchemist.Helpers.CompleteTest do
  use ExUnit.Case, async: true

  alias Alchemist.Helpers.Complete.Env
  alias ElixirSense.Core.State.ModFunInfo

  def expand(expr, env \\ %Env{}) do
    Alchemist.Helpers.Complete.expand(Enum.reverse(expr), env)
  end

  test "erlang module completion" do
    assert expand(':zl') ==
             {:yes, 'ib', [%{name: "zlib", subtype: nil, summary: "", type: :module}]}
  end

  test "erlang module no completion" do
    assert expand(':unknown') == {:no, '', []}
    assert expand('Enum:') == {:no, '', []}
  end

  test "erlang module multiple values completion" do
    {:yes, '', list} = expand(':user')
    assert list |> Enum.find(&(&1.name == "user"))
    assert list |> Enum.find(&(&1.name == "user_drv"))
  end

  test "erlang root completion" do
    {:yes, '', list} = expand(':')
    assert is_list(list)
    assert list |> Enum.find(&(&1.name == "lists"))
  end

  test "elixir proxy" do
    {:yes, '', list} = expand('E')
    assert list |> Enum.find(&(&1.name == "Elixir"))
  end

  test "elixir completion" do
    assert expand('En') == {:yes, 'um', []}

    assert {:yes, 'ble', [%{name: "Enumerable", subtype: :protocol, type: :module}]} =
             expand('Enumera')
  end

  test "elixir completion with self" do
    assert {:yes, '.', [%{name: "Enumerable", subtype: :protocol}]} = expand('Enumerable')
  end

  test "elixir completion macro with default args" do
    assert {:yes, 'h_default',
            [
              %{
                args: "a \\\\ :asdf,b,var \\\\ 0",
                arity: 1,
                name: "with_default",
                origin: "ElixirSenseExample.BehaviourWithMacrocallback.Impl",
                spec: "@spec with_default(atom, list, integer) :: Macro.t",
                summary: "some macro with default arg\n",
                type: :macro
              },
              %{
                args: "a \\\\ :asdf,b,var \\\\ 0",
                arity: 2,
                name: "with_default",
                origin: "ElixirSenseExample.BehaviourWithMacrocallback.Impl",
                spec: "@spec with_default(atom, list, integer) :: Macro.t",
                summary: "some macro with default arg\n",
                type: :macro
              },
              %{
                args: "a \\\\ :asdf,b,var \\\\ 0",
                arity: 3,
                name: "with_default",
                origin: "ElixirSenseExample.BehaviourWithMacrocallback.Impl",
                spec: "@spec with_default(atom, list, integer) :: Macro.t",
                summary: "some macro with default arg\n",
                type: :macro
              }
            ]} = expand('ElixirSenseExample.BehaviourWithMacrocallback.Impl.wit')
  end

  test "elixir completion on modules from load path" do
    assert {:yes, [],
            [
              %{name: "Stream", subtype: :struct, type: :module},
              %{name: "String", subtype: nil, type: :module},
              %{name: "StringIO", subtype: nil, type: :module}
            ]} = expand('Str')

    assert {:yes, '',
            [
              %{name: "Macro"},
              %{name: "Map"},
              %{name: "MapSet"},
              %{name: "MatchError"}
            ]} = expand('Ma')

    assert {:yes, 't', [%{name: "Dict"}]} = expand('Dic')
    assert {:yes, [], suggestions} = expand('Ex')
    assert Enum.any?(suggestions, &(&1.name == "ExUnit"))
    assert Enum.any?(suggestions, &(&1.name == "Exception"))
  end

  test "Elixir no completion for underscored functions with no doc" do
    {:module, _, bytecode, _} =
      defmodule Elixir.Sample do
        def __foo__(), do: 0
        @doc "Bar doc"
        def __bar__(), do: 1
      end

    File.write!("Elixir.Sample.beam", bytecode)
    assert {:docs_v1, _, _, _, _, _, _} = Code.fetch_docs(Sample)

    # IEx version asserts expansion on Sample._ but we also include :__info__ and there is more than 1 match
    assert {:yes, 'ar__', [%{name: "__bar__"}]} = expand('Sample.__b')
  after
    File.rm("Elixir.Sample.beam")
    :code.purge(Sample)
    :code.delete(Sample)
  end

  test "completion for functions added when compiled module is reloaded" do
    {:module, _, bytecode, _} =
      defmodule Sample do
        def foo(), do: 0
      end

    File.write!("Alchemist.Helpers.CompleteTest.Sample.beam", bytecode)
    assert {:yes, '', [%{name: "foo"}]} = expand('Alchemist.Helpers.CompleteTest.Sample.foo')

    Code.compiler_options(ignore_module_conflict: true)

    defmodule Sample do
      def foo(), do: 0
      def foobar(), do: 0
    end

    assert {:yes, '', [%{name: "foo"}, %{name: "foobar"}]} =
             expand('Alchemist.Helpers.CompleteTest.Sample.foo')
  after
    File.rm("Alchemist.Helpers.CompleteTest.Sample.beam")
    Code.compiler_options(ignore_module_conflict: false)
    :code.purge(Sample)
    :code.delete(Sample)
  end

  test "Elixir no completion for default argument functions with doc set to false" do
    {:yes, '', available} = expand('String.')
    refute Enum.any?(available, &(&1.name == "rjust" and &1.arity == 2))
    assert Enum.any?(available, &(&1.name == "replace" and &1.arity == 3))

    assert expand('String.r') == {:yes, 'e', []}

    {:module, _, bytecode, _} =
      defmodule Elixir.DefaultArgumentFunctions do
        def afoo(a \\ :a, b, c \\ :c),
          do: {a, b, c}

        def _do_fizz(a \\ :a, b, c \\ :c),
          do: {a, b, c}

        @doc false
        def __fizz__(a \\ :a, b, c \\ :c),
          do: {a, b, c}

        @doc "bar/0 doc"
        def abar(),
          do: :bar

        @doc false
        def abar(a \\ :a, b, c \\ :c, d \\ :d),
          do: {a, b, c, d}

        @doc false
        def abar(a, b, c, d, e),
          do: {a, b, c, d, e}

        @doc false
        def abaz(a \\ :a),
          do: {a}

        @doc "biz/3 doc"
        def abiz(a, b, c \\ :c),
          do: {a, b, c}
      end

    File.write!("Elixir.DefaultArgumentFunctions.beam", bytecode)

    assert {:yes, '',
            [
              %{name: "abar", arity: 0},
              %{name: "afoo", arity: 1},
              %{name: "afoo", arity: 2},
              %{name: "afoo", arity: 3},
              %{name: "abiz", arity: 2},
              %{name: "abiz", arity: 3}
            ]} = expand('DefaultArgumentFunctions.a')

    assert {:yes, 'z',
            [
              %{name: "abiz", arity: 2},
              %{name: "abiz", arity: 3}
            ]} = expand('DefaultArgumentFunctions.abi')

    assert {:yes, '',
            [
              %{name: "afoo", arity: 1},
              %{name: "afoo", arity: 2},
              %{name: "afoo", arity: 3}
            ]} = expand('DefaultArgumentFunctions.afoo')
  after
    File.rm("Elixir.DefaultArgumentFunctions.beam")
    :code.purge(DefaultArgumentFunctions)
    :code.delete(DefaultArgumentFunctions)
  end

  test "elixir no completion" do
    assert expand('.') == {:no, '', []}
    assert expand('Xyz') == {:no, '', []}
    assert expand('x.Foo') == {:no, '', []}
    assert expand('x.Foo.get_by') == {:no, '', []}
  end

  test "elixir root submodule completion" do
    assert {:yes, 'ss', [%{name: "Access"}]} = expand('Elixir.Acce')
    assert {:yes, '', _} = expand('Elixir.')
  end

  test "elixir submodule completion" do
    assert {:yes, 'rs', [%{name: "Chars", subtype: :protocol}]} = expand('String.Cha')
  end

  test "elixir submodule no completion" do
    assert expand('IEx.Xyz') == {:no, '', []}
  end

  test "function completion" do
    assert {:yes, 'rsion', [%{name: "version", origin: "System"}]} = expand('System.ve')
    assert {:yes, 'ms', [%{name: "fun2ms", origin: ":ets"}]} = expand(':ets.fun2')
  end

  test "function completion with arity" do
    assert {:yes, '',
            [
              %{
                name: "printable?",
                arity: 1,
                spec:
                  "@spec printable?(t, 0) :: true\n@spec printable?(t, pos_integer | :infinity) :: boolean",
                summary:
                  "Checks if a string contains only printable characters up to `character_limit`."
              },
              %{
                name: "printable?",
                arity: 2,
                spec:
                  "@spec printable?(t, 0) :: true\n@spec printable?(t, pos_integer | :infinity) :: boolean",
                summary:
                  "Checks if a string contains only printable characters up to `character_limit`."
              }
            ]} = expand('String.printable?')

    assert {:yes, '', [%{name: "printable?", arity: 1}, %{name: "printable?", arity: 2}]} =
             expand('String.printable?/')
  end

  test "macro completion" do
    {:yes, '', list} = expand('Kernel.is_')
    assert is_list(list)
  end

  test "imports completion" do
    {:yes, '', list} = expand('')
    assert is_list(list)

    assert list |> Enum.find(&(&1.name == "unquote"))
    # IEX version asserts IEx.Helpers are imported
    # assert list |> Enum.find(& &1.name == "h")
    # assert list |> Enum.find(& &1.name == "pwd")
  end

  test "kernel import completion" do
    assert {:yes, 'ct',
            [
              %{
                args: "fields",
                arity: 1,
                name: "defstruct",
                origin: "Kernel",
                spec: "",
                summary: "Defines a struct.",
                type: :macro
              }
            ]} = expand('defstru')

    assert {:yes, '',
            [
              %{arity: 2, name: "put_in"},
              %{arity: 3, name: "put_in"},
              %{arity: 3, name: "put_elem"}
            ]} = expand('put_')
  end

  test "kernel special form completion" do
    assert {:yes, 'icing', [%{name: "unquote_splicing", origin: "Kernel.SpecialForms"}]} =
             expand('unquote_spl')
  end

  test "completion inside expression" do
    assert expand('1 En') == {:yes, 'um', []}
    assert expand('Test(En') == {:yes, 'um', []}
    assert {:yes, 'ib', [_]} = expand('Test :zl')
    assert {:yes, 'ib', [_]} = expand('[:zl')
    assert {:yes, 'ib', [_]} = expand('{:zl')
  end

  test "ampersand completion" do
    assert expand('&Enu') == {:yes, 'm', []}

    assert {:yes, [],
            [
              %{name: "all?", arity: 1},
              %{name: "all?", arity: 2},
              %{name: "any?", arity: 1},
              %{name: "any?", arity: 2},
              %{name: "at", arity: 2},
              %{name: "at", arity: 3}
            ]} = expand('&Enum.a')

    assert {:yes, [],
            [
              %{name: "all?", arity: 1},
              %{name: "all?", arity: 2},
              %{name: "any?", arity: 1},
              %{name: "any?", arity: 2},
              %{name: "at", arity: 2},
              %{name: "at", arity: 3}
            ]} = expand('f = &Enum.a')
  end

  defmodule SublevelTest.LevelA.LevelB do
  end

  test "elixir completion sublevel" do
    assert {:yes, 'LevelA', [%{name: "LevelA"}]} =
             expand('Alchemist.Helpers.CompleteTest.SublevelTest.')
  end

  defmodule MyServer do
    def current_env do
      %Macro.Env{aliases: [{MyList, List}, {EList, :lists}]}
    end
  end

  test "complete aliases of elixir modules" do
    env = %Env{
      aliases: [{MyList, List}]
    }

    assert {:yes, 'ist', [%{name: "MyList"}]} = expand('MyL', env)
    assert {:yes, '.', [%{name: "MyList"}]} = expand('MyList', env)

    assert {:yes, [], [%{arity: 1, name: "to_integer"}, %{arity: 2, name: "to_integer"}]} =
             expand('MyList.to_integer', env)
  end

  test "complete aliases of erlang modules" do
    env = %Env{
      aliases: [{EList, :lists}]
    }

    assert {:yes, 'ist', [%{name: "EList"}]} = expand('EL', env)
    assert {:yes, '.', [%{name: "EList"}]} = expand('EList', env)

    assert {:yes, [],
            [
              %{arity: 2, name: "map"},
              %{arity: 3, name: "mapfoldl"},
              %{arity: 3, name: "mapfoldr"}
            ]} = expand('EList.map', env)
  end

  test "complete local funs from scope module" do
    env = %Env{
      scope_module: MyModule,
      mods_and_funs: %{
        {MyModule, nil, nil} => %ModFunInfo{type: :defmodule},
        {MyModule, :my_fun_priv, nil} => %ModFunInfo{type: :defp},
        {MyModule, :my_fun_priv, 1} => %ModFunInfo{type: :defp},
        {MyModule, :my_fun_pub, nil} => %ModFunInfo{type: :def},
        {MyModule, :my_fun_pub, 1} => %ModFunInfo{type: :def},
        {MyModule, :my_macro_priv, nil} => %ModFunInfo{type: :defmacrop},
        {MyModule, :my_macro_priv, 1} => %ModFunInfo{type: :defmacrop},
        {MyModule, :my_macro_pub, nil} => %ModFunInfo{type: :defmacro},
        {MyModule, :my_macro_pub, 1} => %ModFunInfo{type: :defmacro},
        {MyModule, :my_guard_priv, nil} => %ModFunInfo{type: :defguardp},
        {MyModule, :my_guard_priv, 1} => %ModFunInfo{type: :defguardp},
        {MyModule, :my_guard_pub, nil} => %ModFunInfo{type: :defguard},
        {MyModule, :my_guard_pub, 1} => %ModFunInfo{type: :defguard},
        {MyModule, :my_delegated, nil} => %ModFunInfo{type: :defdelegate},
        {MyModule, :my_delegated, 1} => %ModFunInfo{type: :defdelegate},
        {OtherModule, nil, nil} => %ModFunInfo{},
        {OtherModule, :my_fun_pub_other, nil} => %ModFunInfo{type: :def},
        {OtherModule, :my_fun_pub_other, 1} => %ModFunInfo{type: :def}
      }
    }

    assert {:yes, 'un_p', []} = expand('my_f', env)

    assert {:yes, 'iv',
            [
              %{name: "my_fun_priv", origin: "MyModule", type: :function}
            ]} = expand('my_fun_pr', env)

    assert {:yes, 'b',
            [
              %{name: "my_fun_pub", origin: "MyModule", type: :function}
            ]} = expand('my_fun_pu', env)

    assert {:yes, 'iv',
            [
              %{name: "my_macro_priv", origin: "MyModule", type: :macro}
            ]} = expand('my_macro_pr', env)

    assert {:yes, 'b',
            [
              %{name: "my_macro_pub", origin: "MyModule", type: :macro}
            ]} = expand('my_macro_pu', env)

    assert {:yes, 'iv',
            [
              %{name: "my_guard_priv", origin: "MyModule", type: :macro}
            ]} = expand('my_guard_pr', env)

    assert {:yes, 'b',
            [
              %{name: "my_guard_pub", origin: "MyModule", type: :macro}
            ]} = expand('my_guard_pu', env)

    assert {:yes, 'legated',
            [
              %{name: "my_delegated", origin: "MyModule", type: :function}
            ]} = expand('my_de', env)
  end

  test "complete remote funs from imported module" do
    env = %Env{
      scope_module: MyModule,
      imports: [OtherModule],
      mods_and_funs: %{
        {OtherModule, nil, nil} => %ModFunInfo{type: :defmodule},
        {OtherModule, :my_fun_other_pub, nil} => %ModFunInfo{type: :def},
        {OtherModule, :my_fun_other_pub, 1} => %ModFunInfo{type: :def},
        {OtherModule, :my_fun_other_priv, nil} => %ModFunInfo{type: :defp},
        {OtherModule, :my_fun_other_priv, 1} => %ModFunInfo{type: :defp}
      }
    }

    assert {:yes, 'un_other_pub',
            [
              %{name: "my_fun_other_pub", origin: "OtherModule"}
            ]} = expand('my_f', env)
  end

  test "complete remote funs" do
    env = %Env{
      scope_module: MyModule,
      mods_and_funs: %{
        {Some.OtherModule, nil, nil} => %ModFunInfo{type: :defmodule},
        {Some.OtherModule, :my_fun_other_pub, nil} => %ModFunInfo{type: :def},
        {Some.OtherModule, :my_fun_other_pub, 1} => %ModFunInfo{type: :def},
        {Some.OtherModule, :my_fun_other_priv, nil} => %ModFunInfo{type: :defp},
        {Some.OtherModule, :my_fun_other_priv, 1} => %ModFunInfo{type: :defp}
      }
    }

    assert {:yes, 'un_other_pub',
            [
              %{name: "my_fun_other_pub", origin: "Some.OtherModule"}
            ]} = expand('Some.OtherModule.my_f', env)
  end

  test "complete remote funs from aliased module" do
    env = %Env{
      scope_module: MyModule,
      aliases: [{S, Some.OtherModule}],
      mods_and_funs: %{
        {Some.OtherModule, nil, nil} => %ModFunInfo{type: :defmodule},
        {Some.OtherModule, :my_fun_other_pub, nil} => %ModFunInfo{type: :def},
        {Some.OtherModule, :my_fun_other_pub, 1} => %ModFunInfo{type: :def},
        {Some.OtherModule, :my_fun_other_priv, nil} => %ModFunInfo{type: :defp},
        {Some.OtherModule, :my_fun_other_priv, 1} => %ModFunInfo{type: :defp}
      }
    }

    assert {:yes, 'un_other_pub',
            [
              %{name: "my_fun_other_pub", origin: "Some.OtherModule"}
            ]} = expand('S.my_f', env)
  end

  test "complete modules" do
    env = %Env{
      scope_module: MyModule,
      aliases: [{MyAlias, Some.OtherModule.Nested}],
      mods_and_funs: %{
        {Some.OtherModule, nil, nil} => %ModFunInfo{type: :defmodule}
      }
    }

    assert {:yes, 'me', [%{name: "Some", type: :module}]} = expand('So', env)
    assert {:yes, 'OtherModule', [%{name: "OtherModule", type: :module}]} = expand('Some.', env)
    assert {:yes, 'lias', [%{name: "MyAlias", type: :module}]} = expand('MyA', env)
  end

  defmodule MyStruct do
    defstruct [:my_val]
  end

  test "completion for structs" do
    assert {:yes, 'uct', [%{name: "MyStruct"}]} = expand('%Alchemist.Helpers.CompleteTest.MyStr')
  end

  test "ignore invalid Elixir module literals" do
    defmodule :"Alchemist.Helpers.CompleteTest.Unicodé", do: nil
    assert expand('Alchemist.Helpers.CompleteTest.Unicod') == {:no, '', []}
  after
    :code.purge(:"Alchemist.Helpers.CompleteTest.Unicodé")
    :code.delete(:"Alchemist.Helpers.CompleteTest.Unicodé")
  end

  defmodule MyMacro do
    defmacro test(do: expr) do
      expr
    end

    def fun, do: :ok
    defguard guard(value) when is_integer(value) and rem(value, 2) == 0
    defdelegate delegated(par), to: OtherModule
  end

  test "complete macros and functions from not loaded modules" do
    assert {:yes, 'st', [%{name: "test", type: :macro}]} =
             expand('Alchemist.Helpers.CompleteTest.MyMacro.te')

    assert {:yes, 'un', [%{name: "fun", type: :function}]} =
             expand('Alchemist.Helpers.CompleteTest.MyMacro.f')

    assert {:yes, 'uard', [%{name: "guard", type: :macro}]} =
             expand('Alchemist.Helpers.CompleteTest.MyMacro.g')

    assert {:yes, 'legated', [%{name: "delegated", type: :function}]} =
             expand('Alchemist.Helpers.CompleteTest.MyMacro.de')
  end

  test "complete build in functions on non local calls" do
    assert {:no, _, _} = expand('mo')
    assert {:no, _, _} = expand('__in')

    assert {:no, _, _} = expand('Elixir.mo')
    assert {:no, _, _} = expand('Elixir.__in')

    assert {:yes, 'dule_info',
            [
              %{name: "module_info", type: :function, arity: 0},
              %{name: "module_info", type: :function, arity: 1}
            ]} = expand('Alchemist.Helpers.CompleteTest.MyMacro.mo')

    assert {:yes, 'fo__', [%{name: "__info__", type: :function}]} =
             expand('Alchemist.Helpers.CompleteTest.MyMacro.__in')

    assert {:yes, 'dule_info',
            [
              %{name: "module_info", type: :function, arity: 0},
              %{name: "module_info", type: :function, arity: 1}
            ]} = expand(':ets.mo')

    assert {:no, _, _} = expand(':ets.__in')

    env = %Env{
      scope_module: MyModule,
      aliases: [{MyAlias, Some.OtherModule.Nested}],
      mods_and_funs: %{
        {MyModule, nil, nil} => %ModFunInfo{type: :defmodule}
      }
    }

    assert {:no, _, _} = expand('mo', env)
    assert {:no, _, _} = expand('__in', env)

    assert {:yes, 'dule_info',
            [
              %{name: "module_info", type: :function, arity: 0},
              %{name: "module_info", type: :function, arity: 1}
            ]} = expand('MyModule.mo', env)

    assert {:yes, 'fo__', [%{name: "__info__", type: :function}]} = expand('MyModule.__in', env)
  end

  test "complete build in behaviour functions" do
    assert {:no, _, _} = expand('Elixir.beh')

    assert {:yes, 'aviour_info',
            [
              %{name: "behaviour_info", type: :function, arity: 1}
            ]} = expand(':gen_server.beh')

    assert {:yes, 'aviour_info',
            [
              %{name: "behaviour_info", type: :function, arity: 1}
            ]} = expand('GenServer.beh')
  end
end
