defmodule ElixirSense.Core.Compiler.TypespecTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.Compiler.Typespec
  alias ElixirSense.Core.Compiler
  alias ElixirSense.Core.Compiler.State
  alias ElixirSense.Core.Normalized.Macro.Env, as: NormalizedMacroEnv

  defp default_state,
    do: %State{
      prematch:
        if Version.match?(System.version(), ">= 1.15.0") do
          Code.get_compiler_option(:on_undefined_variable)
        else
          :warn
        end
    }

  defp expand_typespec(ast, vars \\ [], state \\ default_state(), env \\ Compiler.env()) do
    Typespec.expand_typespec(ast, vars, state, env)
  end

  describe "expand_typespec" do
    test "literal" do
      assert {:foo, _state} = expand_typespec(:foo)
      assert {1, _state} = expand_typespec(1)
      assert {[], _state} = expand_typespec([])
      assert {{:nonempty_list, [], [{:any, [], []}]}, _state} = expand_typespec([{:..., [], []}])
      assert {{:nonempty_list, [], [:foo]}, _state} = expand_typespec([:foo, {:..., [], []}])
      assert {{:list, [], [:foo]}, _state} = expand_typespec([:foo])

      assert {{:list, [], [{:|, [], [foo: 1, bar: :baz]}]}, _state} =
               expand_typespec(foo: 1, bar: :baz)

      # invalid
      assert {{:list, [], [{:|, [], [foo: 1, bar: :baz]}]}, _state} =
               expand_typespec([0, {:foo, 1}, {:bar, :baz}])
    end

    test "local type" do
      assert {{:local_type, [], []}, _state} = expand_typespec({:local_type, [], []})

      assert {{:local_type, [], [:foo, 1]}, _state} =
               expand_typespec({:local_type, [], [:foo, 1]})
    end

    test "local type no parens" do
      assert {{:foo, [], []}, _state} = expand_typespec({:foo, [], nil})
    end

    test "var" do
      assert {{:foo, [], nil}, _state} = expand_typespec({:foo, [], nil}, [:foo])
    end

    test "named ..." do
      assert {{:..., [], []}, _state} = expand_typespec({:..., [], []})
    end

    test "fun" do
      assert {{:fun, [], [:foo, 1]}, _state} = expand_typespec({:fun, [], [:foo, 1]})
      assert {{:fun, [], []}, _state} = expand_typespec({:fun, [], []})

      assert {[{:->, [], [[:foo], 1]}], _state} = expand_typespec([{:->, [], [[:foo], 1]}])

      assert {[{:->, [], [[:foo, :bar], 1]}], _state} =
               expand_typespec([{:->, [], [[:foo, :bar], 1]}])

      assert {[{:->, [], [[{:..., [], []}], 1]}], _state} =
               expand_typespec([{:->, [], [[{:..., [], []}], 1]}])

      assert {[{:->, [], [[{:..., [], []}], {:any, [], []}]}], _state} =
               expand_typespec([{:->, [], [[{:..., [], []}], {:any, [], []}]}])
    end

    test "charlist" do
      assert {{:charlist, [], []}, _state} =
               expand_typespec({:charlist, [], []})

      assert {{:char_list, [], []}, _state} =
               expand_typespec({:char_list, [], []})

      assert {{:nonempty_charlist, [], []}, _state} =
               expand_typespec({:nonempty_charlist, [], []})
    end

    test "struct" do
      assert {{:struct, [], []}, _state} = expand_typespec({:struct, [], []})
    end

    test "as_boolean" do
      assert {{:as_boolean, [], [:foo]}, _state} =
               expand_typespec({:as_boolean, [], [:foo]})
    end

    test "keyword" do
      assert {{:keyword, [], []}, _state} =
               expand_typespec({:keyword, [], []})

      assert {{:keyword, [], [:foo]}, _state} =
               expand_typespec({:keyword, [], [:foo]})
    end

    test "string" do
      assert {{:string, [], []}, _state} = expand_typespec({:string, [], []})
      assert {{:nonempty_string, [], []}, _state} = expand_typespec({:nonempty_string, [], []})
    end

    test "__block__" do
      assert {:foo, _state} = expand_typespec({:__block__, [], [:foo]})
    end

    test "tuple" do
      assert {{:{}, [], [:foo]}, _state} = expand_typespec({:{}, [], [:foo]})
      assert {{:foo, :bar}, _state} = expand_typespec({:foo, :bar})
      assert {{:tuple, [], []}, _state} = expand_typespec({:tuple, [], []})
    end

    test "remote" do
      assert {{{:., [], [:some, :remote]}, [], [:foo]}, _state} =
               expand_typespec({{:., [], [:some, :remote]}, [], [:foo]})

      assert {{{:., [], [Foo.Bar, :remote]}, [], [:foo]}, _state} =
               expand_typespec(
                 {{:., [], [{:__aliases__, [], [:Foo, :Bar]}, :remote]}, [], [:foo]}
               )

      env = %{Compiler.env() | module: Foo.Bar}

      assert {{:remote, [], [:foo]}, _state} =
               expand_typespec(
                 {{:., [], [{:__aliases__, [], [:Foo, :Bar]}, :remote]}, [], [:foo]},
                 [],
                 default_state(),
                 env
               )

      env = %{Compiler.env() | aliases: [{Foo, Foo.Bar}]}

      assert {{{:., [], [Foo.Bar, :remote]}, [], [:foo]}, _state} =
               expand_typespec(
                 {{:., [], [{:__aliases__, [], [:Foo]}, :remote]}, [], [:foo]},
                 [],
                 default_state(),
                 env
               )

      env = %{Compiler.env() | module: Foo.Bar}

      assert {{:remote, [], [:foo]}, _state} =
               expand_typespec(
                 {{:., [], [{:__MODULE__, [], nil}, :remote]}, [], [:foo]},
                 [],
                 default_state(),
                 env
               )

      env = %{Compiler.env() | module: Foo.Bar}
      state = %{default_state() | attribute_store: %{{Foo.Bar, :baz} => :some}}

      assert {{{:., [], [:some, :remote]}, [], [:foo]}, _state} =
               expand_typespec(
                 {{:., [], [{:@, [], [{:baz, [], nil}]}, :remote]}, [], [:foo]},
                 [],
                 state,
                 env
               )

      assert {{{:., [], [nil, :remote]}, [], [:foo]}, _state} =
               expand_typespec(
                 {{:., [], [{:@, [], [{:baz, [], nil}]}, :remote]}, [], [:foo]},
                 [],
                 default_state(),
                 env
               )

      # invalid
      assert {{{:., [], [1, :remote]}, [], [:foo]}, _state} =
               expand_typespec({{:., [], [1, :remote]}, [], [:foo]})
    end

    test "unary op" do
      assert {{:+, [], [1]}, _state} = expand_typespec({:+, [], [1]})
      assert {{:-, [], [1]}, _state} = expand_typespec({:-, [], [1]})
    end

    test "special forms" do
      env = %{Compiler.env() | module: Foo.Bar}
      assert {Foo.Bar, _state} = expand_typespec({:__MODULE__, [], nil}, [], default_state(), env)

      env = %{Compiler.env() | aliases: [{Foo, Foo.Bar}]}

      assert {Foo.Bar, _state} =
               expand_typespec({:__aliases__, [], [:Foo]}, [], default_state(), env)
    end

    test "annotated type" do
      assert {{:"::", [], [{:some, [], nil}, {:any, [], []}]}, _state} =
               expand_typespec({:"::", [], [{:some, [], nil}, {:any, [], nil}]})

      # invalid
      assert {{:"::", [], [1, {:any, [], []}]}, _state} =
               expand_typespec({:"::", [], [1, {:any, [], nil}]})

      # invalid nested
      assert {{
                :"::",
                [],
                [{:some, [], nil}, {:"::", [], [{:other, [], nil}, {:any, [], []}]}]
              },
              _state} =
               expand_typespec(
                 {:"::", [],
                  [{:some, [], nil}, {:"::", [], [{:other, [], nil}, {:any, [], nil}]}]}
               )
    end

    test "range" do
      assert {{:.., [], [1, 10]}, _state} = expand_typespec({:.., [], [1, 10]})
    end
  end

  test "union" do
    assert {{:|, [], [{:some, [], []}, {:any, [], []}]}, _state} =
             expand_typespec({:|, [], [{:some, [], nil}, {:any, [], nil}]})

    assert {{
              :|,
              [],
              [{:some, [], []}, {:|, [], [{:other, [], []}, {:any, [], []}]}]
            },
            _state} =
             expand_typespec(
               {:|, [], [{:some, [], nil}, {:|, [], [{:other, [], nil}, {:any, [], nil}]}]}
             )
  end

  test "map" do
    assert {{:map, [], []}, _state} = expand_typespec({:map, [], []})
    assert {{:map, [], []}, _state} = expand_typespec({:map, [], nil})
    assert {{:%{}, [], []}, _state} = expand_typespec({:%{}, [], []})

    assert {{:%{}, [], [foo: :bar]}, _state} = expand_typespec({:%{}, [], [foo: :bar]})

    assert {{:%{}, [], [{{:optional, [], [:foo]}, :bar}]}, _state} =
             expand_typespec({:%{}, [], [{{:optional, [], [:foo]}, :bar}]})

    assert {{:%{}, [], [foo: :bar]}, _state} =
             expand_typespec({:%{}, [], [{{:required, [], [:foo]}, :bar}]})

    # illegal update
    assert {{:%{}, [], []}, _state} =
             expand_typespec({:%{}, [], [{:|, [], [{:s, [], nil}, [asd: 324]]}]})
  end

  test "struct" do
    assert {{
              :%,
              [],
              [
                Date,
                {:%{}, [],
                 [
                   calendar: {:term, [], []},
                   day: {:term, [], []},
                   month: {:term, [], []},
                   year: {:term, [], []}
                 ]}
              ]
            }, _state} = expand_typespec({:%, [], [{:__aliases__, [], [:Date]}, {:%{}, [], []}]})

    assert {{
              :%,
              [],
              [
                Date,
                {:%{}, [],
                 [
                   calendar: {:term, [], []},
                   day: :foo,
                   month: {:term, [], []},
                   year: {:term, [], []}
                 ]}
              ]
            },
            _state} =
             expand_typespec({:%, [], [{:__aliases__, [], [:Date]}, {:%{}, [], [day: :foo]}]})

    # non atom key
    assert {{
              :%,
              [],
              [
                Date,
                {:%{}, [],
                 [
                   calendar: {:term, [], []},
                   day: {:term, [], []},
                   month: {:term, [], []},
                   year: {:term, [], []}
                 ]}
              ]
            },
            _state} =
             expand_typespec({:%, [], [{:__aliases__, [], [:Date]}, {:%{}, [], [{"day", :foo}]}]})

    # invalid key
    assert {{
              :%,
              [],
              [
                Date,
                {:%{}, [],
                 [
                   calendar: {:term, [], []},
                   day: {:term, [], []},
                   month: {:term, [], []},
                   year: {:term, [], []}
                 ]}
              ]
            },
            _state} =
             expand_typespec({:%, [], [{:__aliases__, [], [:Date]}, {:%{}, [], [{:baz, :foo}]}]})

    # non atom
    assert {{:%, [], [1, {:%{}, [], []}]}, _} = expand_typespec({:%, [], [1, {:%{}, [], []}]})

    # unknown
    assert {{:%, [], [UnknownStruct, {:%{}, [], []}]}, _} =
             expand_typespec({:%, [], [{:__aliases__, [], [:UnknownStruct]}, {:%{}, [], []}]})
  end

  test "binaries" do
    type = {:<<>>, [], []}
    assert {^type, _state} = expand_typespec(type)
    type = {:<<>>, [], [{:"::", [], [{:_, [], nil}, {:*, [], [{:_, [], nil}, 8]}]}]}
    assert {^type, _state} = expand_typespec(type)
    type = {:<<>>, [], [{:"::", [], [{:_, [], nil}, 8]}]}
    assert {^type, _state} = expand_typespec(type)

    type = {
      :<<>>,
      [],
      [
        {:"::", [], [{:_, [], nil}, 32]},
        {:"::", [], [{:_, [], nil}, {:*, [], [{:_, [], nil}, 8]}]}
      ]
    }

    assert {^type, _state} = expand_typespec(type)
  end

  test "records" do
    {:ok, env} =
      Compiler.env()
      |> NormalizedMacroEnv.define_import([], ElixirSenseExample.ModuleWithRecord, trace: false)

    assert {{:record, [], [:user, []]}, _state} =
             expand_typespec({:record, [], [:user]}, [], default_state(), env)

    assert {{:record, [], [:user, [age: :foo]]}, _state} =
             expand_typespec({:record, [], [:user, [age: :foo]]}, [], default_state(), env)

    # invalid record
    assert {{:record, [], [1, []]}, _} = expand_typespec({:record, [], [1]})

    # invalid field
    assert {{:record, [], [:user, [invalid: :foo]]}, _state} =
             expand_typespec({:record, [], [:user, [invalid: :foo]]}, [], default_state(), env)

    # unknown record
    assert {{:record, [], [:foo, []]}, _} = expand_typespec({:record, [], [:foo]})
  end
end
