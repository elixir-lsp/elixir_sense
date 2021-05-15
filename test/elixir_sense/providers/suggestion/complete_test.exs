# This file includes modified code extracted from the elixir project. Namely:
#
# https://github.com/elixir-lang/elixir/blob/v1.9/lib/iex/test/iex/autocomplete_test.exs
#
# The original code is licensed as follows:
#
# Copyright 2012 Plataformatec
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

defmodule ElixirSense.Providers.Suggestion.CompleteTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Providers.Suggestion.Complete.Env
  alias ElixirSense.Core.State.{ModFunInfo, SpecInfo, VarInfo, AttributeInfo}

  def expand(expr, env \\ %Env{}) do
    ElixirSense.Providers.Suggestion.Complete.expand(Enum.reverse(expr), env)
  end

  test "erlang module completion" do
    assert [
             %{
               name: ":zlib",
               subtype: nil,
               summary:
                 "This module provides an API for the zlib library \\([www\\.zlib\\.net]\\)\\. It is used to compress and decompress data\\. The data format is described by [RFC 1950], [RFC 1951], and [RFC 1952]\\.",
               type: :module,
               metadata: %{otp_doc_vsn: {1, 0, 0}}
             }
           ] = expand(':zl')
  end

  # Code.fetch_docs(:edoc_wiki) on OTP 23 returns
  # {:error, :chunk_not_found} yet doc_sh is able to retrieve docs 
  test "erlang module completion edoc" do
    assert expand(':edoc_wi') ==
             [
               %{
                 name: ":edoc_wiki",
                 subtype: nil,
                 summary:
                   "EDoc wiki expansion, parsing and postprocessing of XML text.\nUses XMerL.",
                 type: :module,
                 metadata: %{}
               }
             ]
  end

  test "erlang module no completion" do
    assert expand(':unknown') == []
    assert expand('Enum:') == []
  end

  test "erlang module multiple values completion" do
    list = expand(':user')
    assert list |> Enum.find(&(&1.name == ":user"))
    assert list |> Enum.find(&(&1.name == ":user_drv"))
  end

  test "erlang root completion" do
    list = expand(':')
    assert is_list(list)
    assert list |> Enum.find(&(&1.name == ":lists"))
  end

  test "elixir proxy" do
    list = expand('E')
    assert list |> Enum.find(&(&1.name == "Elixir"))
  end

  test "elixir completion" do
    assert [_ | _] = expand('En')

    assert [%{name: "Enumerable", subtype: :protocol, type: :module}] = expand('Enumera')
  end

  test "elixir module completion with @moduledoc false" do
    assert [%{name: "ModuleWithDocFalse", summary: ""}] =
             expand('ElixirSenseExample.ModuleWithDocFals')
  end

  test "elixir function completion with @doc false" do
    assert [
             %{
               name: "some_fun_doc_false",
               summary: "",
               args: "a, b \\\\ nil",
               arity: 1,
               origin: "ElixirSenseExample.ModuleWithDocs",
               spec: "",
               type: :function
             },
             %{
               args: "a, b \\\\ nil",
               arity: 2,
               name: "some_fun_doc_false",
               origin: "ElixirSenseExample.ModuleWithDocs",
               spec: "",
               summary: "",
               type: :function
             },
             %{
               args: "a, b \\\\ nil",
               arity: 1,
               name: "some_fun_no_doc",
               origin: "ElixirSenseExample.ModuleWithDocs",
               spec: "",
               summary: "",
               type: :function
             },
             %{
               args: "a, b \\\\ nil",
               arity: 2,
               name: "some_fun_no_doc",
               origin: "ElixirSenseExample.ModuleWithDocs",
               spec: "",
               summary: "",
               type: :function
             }
           ] = expand('ElixirSenseExample.ModuleWithDocs.some_fun_')
  end

  test "elixir completion with self" do
    assert [%{name: "Enumerable", subtype: :protocol}] = expand('Enumerable')
  end

  test "elixir completion macro with default args" do
    assert [
             %{
               args: "a \\\\ :asdf, b, var \\\\ 0",
               arity: 1,
               name: "with_default",
               origin: "ElixirSenseExample.BehaviourWithMacrocallback.Impl",
               spec: "@spec with_default(atom, list, integer) :: Macro.t",
               summary: "some macro with default arg\n",
               type: :macro
             },
             %{
               args: "a \\\\ :asdf, b, var \\\\ 0",
               arity: 2,
               name: "with_default",
               origin: "ElixirSenseExample.BehaviourWithMacrocallback.Impl",
               spec: "@spec with_default(atom, list, integer) :: Macro.t",
               summary: "some macro with default arg\n",
               type: :macro
             },
             %{
               args: "a \\\\ :asdf, b, var \\\\ 0",
               arity: 3,
               name: "with_default",
               origin: "ElixirSenseExample.BehaviourWithMacrocallback.Impl",
               spec: "@spec with_default(atom, list, integer) :: Macro.t",
               summary: "some macro with default arg\n",
               type: :macro
             }
           ] = expand('ElixirSenseExample.BehaviourWithMacrocallback.Impl.wit')
  end

  test "elixir completion on modules from load path" do
    assert [
             %{name: "Stream", subtype: :struct, type: :module},
             %{name: "String", subtype: nil, type: :module},
             %{name: "StringIO", subtype: nil, type: :module}
           ] = expand('Str')

    assert [
             %{name: "Macro"},
             %{name: "Map"},
             %{name: "MapSet"},
             %{name: "MatchError"}
           ] = expand('Ma')

    assert [%{name: "Dict"}] = expand('Dic')
    assert suggestions = expand('Ex')
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
    assert [%{name: "__bar__"}] = expand('Sample.__b')
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

    File.write!("ElixirSense.Providers.Suggestion.CompleteTest.Sample.beam", bytecode)

    assert [%{name: "foo"}] = expand('ElixirSense.Providers.Suggestion.CompleteTest.Sample.foo')

    Code.compiler_options(ignore_module_conflict: true)

    defmodule Sample do
      def foo(), do: 0
      def foobar(), do: 0
    end

    assert [%{name: "foo"}, %{name: "foobar"}] =
             expand('ElixirSense.Providers.Suggestion.CompleteTest.Sample.foo')
  after
    File.rm("ElixirSense.Providers.Suggestion.CompleteTest.Sample.beam")
    Code.compiler_options(ignore_module_conflict: false)
    :code.purge(Sample)
    :code.delete(Sample)
  end

  test "elixir no completion" do
    assert expand('.') == []
    assert expand('Xyz') == []
    assert expand('x.Foo') == []
    assert expand('x.Foo.get_by') == []
  end

  test "elixir root submodule completion" do
    assert [%{name: "Access", summary: "Key-based access to data structures."}] =
             expand('Elixir.Acce')

    assert [_ | _] = expand('Elixir.')
  end

  test "elixir submodule completion" do
    assert [
             %{
               name: "Chars",
               subtype: :protocol,
               summary:
                 "The `String.Chars` protocol is responsible for\nconverting a structure to a binary (only if applicable)."
             }
           ] = expand('String.Cha')
  end

  test "elixir submodule no completion" do
    assert expand('IEx.Xyz') == []
  end

  test "function completion" do
    assert [%{name: "version", origin: "System"}] = expand('System.ve')
    assert [%{name: "fun2ms", origin: ":ets"}] = expand(':ets.fun2')
  end

  test "function completion with arity" do
    assert [
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
           ] = expand('String.printable?')

    assert [%{name: "printable?", arity: 1}, %{name: "printable?", arity: 2}] =
             expand('String.printable?/')
  end

  test "function completion using a variable bound to a module" do
    env = %Env{
      vars: [
        %VarInfo{
          name: :mod,
          type: {:atom, String}
        }
      ]
    }

    assert [%{name: "printable?", arity: 1}, %{name: "printable?", arity: 2}] =
             expand('mod.print', env)
  end

  test "map atom key completion is supported" do
    env = %Env{
      vars: [
        %VarInfo{
          name: :map,
          type: {:map, [foo: 1, bar_1: 23, bar_2: 14], nil}
        }
      ]
    }

    assert expand('map.f', env) ==
             [%{name: "foo", subtype: :map_key, type: :field, origin: nil, call?: true}]

    assert [_ | _] = expand('map.b', env)

    assert expand('map.bar_', env) ==
             [
               %{name: "bar_1", subtype: :map_key, type: :field, origin: nil, call?: true},
               %{name: "bar_2", subtype: :map_key, type: :field, origin: nil, call?: true}
             ]

    assert expand('map.c', env) == []

    assert expand('map.', env) ==
             [
               %{name: "bar_1", subtype: :map_key, type: :field, origin: nil, call?: true},
               %{name: "bar_2", subtype: :map_key, type: :field, origin: nil, call?: true},
               %{name: "foo", subtype: :map_key, type: :field, origin: nil, call?: true}
             ]

    assert expand('map.foo', env) == []
  end

  test "map atom key completion is supported on attributes" do
    env = %Env{
      attributes: [
        %AttributeInfo{
          name: :map,
          type: {:map, [foo: 1, bar_1: 23, bar_2: 14], nil}
        }
      ]
    }

    assert expand('@map.f', env) ==
             [%{name: "foo", subtype: :map_key, type: :field, origin: nil, call?: true}]

    assert [_ | _] = expand('@map.b', env)

    assert expand('@map.bar_', env) ==
             [
               %{name: "bar_1", subtype: :map_key, type: :field, origin: nil, call?: true},
               %{name: "bar_2", subtype: :map_key, type: :field, origin: nil, call?: true}
             ]

    assert expand('@map.c', env) == []

    assert expand('@map.', env) ==
             [
               %{name: "bar_1", subtype: :map_key, type: :field, origin: nil, call?: true},
               %{name: "bar_2", subtype: :map_key, type: :field, origin: nil, call?: true},
               %{name: "foo", subtype: :map_key, type: :field, origin: nil, call?: true}
             ]

    assert expand('@map.foo', env) == []
  end

  test "nested map atom key completion is supported" do
    env = %Env{
      vars: [
        %VarInfo{
          name: :map,
          type:
            {:map,
             [
               nested:
                 {:map,
                  [
                    deeply:
                      {:map,
                       [
                         foo: 1,
                         bar_1: 23,
                         bar_2: 14,
                         mod: {:atom, String},
                         num: 1
                       ], nil}
                  ], nil}
             ], nil}
        }
      ]
    }

    assert expand('map.nested.deeply.f', env) ==
             [%{name: "foo", subtype: :map_key, type: :field, origin: nil, call?: true}]

    assert [_ | _] = expand('map.nested.deeply.b', env)

    assert expand('map.nested.deeply.bar_', env) ==
             [
               %{name: "bar_1", subtype: :map_key, type: :field, origin: nil, call?: true},
               %{name: "bar_2", subtype: :map_key, type: :field, origin: nil, call?: true}
             ]

    assert expand('map.nested.deeply.', env) ==
             [
               %{name: "bar_1", subtype: :map_key, type: :field, origin: nil, call?: true},
               %{name: "bar_2", subtype: :map_key, type: :field, origin: nil, call?: true},
               %{name: "foo", subtype: :map_key, type: :field, origin: nil, call?: true},
               %{name: "mod", subtype: :map_key, type: :field, origin: nil, call?: true},
               %{name: "num", subtype: :map_key, type: :field, origin: nil, call?: true}
             ]

    assert [_ | _] = expand('map.nested.deeply.mod.print', env)

    assert expand('map.nested', env) ==
             [%{name: "nested", subtype: :map_key, type: :field, origin: nil, call?: true}]

    assert expand('map.nested.deeply', env) ==
             [%{name: "deeply", subtype: :map_key, type: :field, origin: nil, call?: true}]

    assert expand('map.nested.deeply.foo', env) == []

    assert expand('map.nested.deeply.c', env) == []
    assert expand('map.a.b.c.f', env) == []
  end

  test "map string key completion is not supported" do
    env = %Env{
      vars: [
        %VarInfo{
          name: :map,
          type: {:map, [{"foo", 124}], nil}
        }
      ]
    }

    assert expand('map.f', env) == []
  end

  test "autocompletion off a bound variable only works for modules and maps" do
    env = %Env{
      vars: [
        %VarInfo{
          name: :map,
          type: {:map, [nested: {:map, [num: 23], nil}], nil}
        }
      ]
    }

    assert expand('num.print', env) == []
    assert expand('map.nested.num.f', env) == []
    assert expand('map.nested.num.key.f', env) == []
  end

  test "autocomplete map fields from call binding" do
    env = %Env{
      vars: [
        %VarInfo{
          name: :map,
          type: {:map, [{:foo, {:atom, String}}], nil}
        },
        %VarInfo{
          name: :call,
          type: {:call, {:variable, :map}, :foo, []}
        }
      ]
    }

    assert [_ | _] = expand('call.print', env)
  end

  test "autocomplete call return binding" do
    env = %Env{
      vars: [
        %VarInfo{
          name: :call,
          type: {:call, {:atom, DateTime}, :utc_now, []}
        }
      ]
    }

    assert [_ | _] = expand('call.ho', env)
    assert [_ | _] = expand('DateTime.utc_now.ho', env)
    # FIXME Complete.reduce(expr) breaks things...
    # assert {:yes, 'ur', _} = expand('DateTime.utc_now().', env)
    # assert {:yes, 'ur', _} = expand('DateTime.utc_now().ho', env)
  end

  test "autocompletion off of unbound variables is not supported" do
    assert expand('other_var.f') == []
    assert expand('a.b.c.d') == []
  end

  test "macro completion" do
    assert [_ | _] = expand('Kernel.is_')
  end

  test "imports completion" do
    list = expand('')
    assert is_list(list)

    assert list |> Enum.find(&(&1.name == "unquote"))
    # IEX version asserts IEx.Helpers are imported
    # assert list |> Enum.find(& &1.name == "h")
    # assert list |> Enum.find(& &1.name == "pwd")
  end

  test "kernel import completion" do
    assert [
             %{
               args: "fields",
               arity: 1,
               name: "defstruct",
               origin: "Kernel",
               spec: "",
               summary: "Defines a struct.",
               type: :macro
             }
           ] = expand('defstru')

    assert [
             %{arity: 3, name: "put_elem"},
             %{arity: 2, name: "put_in"},
             %{arity: 3, name: "put_in"}
           ] = expand('put_')
  end

  test "variable name completion" do
    env = %Env{
      vars: [
        %VarInfo{
          name: :numeral
        },
        %VarInfo{
          name: :number
        },
        %VarInfo{
          name: :nothing
        }
      ]
    }

    assert expand('numb', env) == [%{type: :variable, name: "number"}]

    assert expand('num', env) ==
             [%{type: :variable, name: "number"}, %{type: :variable, name: "numeral"}]

    assert [%{type: :variable, name: "nothing"} | _] = expand('no', env)
  end

  test "variable name completion after pin" do
    env = %Env{
      vars: [
        %VarInfo{
          name: :number
        }
      ]
    }

    assert expand('^numb', env) == [%{type: :variable, name: "number"}]
    assert expand('^', env) == [%{type: :variable, name: "number"}]
  end

  test "attribute name completion" do
    env = %Env{
      attributes: [
        %AttributeInfo{
          name: :numeral
        },
        %AttributeInfo{
          name: :number
        },
        %AttributeInfo{
          name: :nothing
        }
      ],
      scope: {:some, 0}
    }

    assert expand('@numb', env) == [%{type: :attribute, name: "@number"}]

    assert expand('@num', env) ==
             [%{type: :attribute, name: "@number"}, %{type: :attribute, name: "@numeral"}]

    assert expand('@', env) ==
             [
               %{name: "@nothing", type: :attribute},
               %{type: :attribute, name: "@number"},
               %{type: :attribute, name: "@numeral"}
             ]
  end

  test "builtin attribute name completion" do
    env_function = %Env{
      attributes: [],
      scope: {:some, 0}
    }

    env_module = %Env{
      attributes: [],
      scope: Some.Module
    }

    env_outside_module = %Env{
      attributes: [],
      scope: Elixir
    }

    assert expand('@befo', env_function) == []
    assert expand('@befo', env_outside_module) == []

    assert expand('@befo', env_module) ==
             [%{type: :attribute, name: "@before_compile"}]
  end

  test "kernel special form completion" do
    assert [%{name: "unquote_splicing", origin: "Kernel.SpecialForms"}] = expand('unquote_spl')
  end

  test "completion inside expression" do
    assert [_ | _] = expand('1 En')
    assert [_ | _] = expand('Test(En')
    assert [_] = expand('Test :zl')
    assert [_] = expand('[:zl')
    assert [_] = expand('{:zl')
  end

  test "ampersand completion" do
    assert [_ | _] = expand('&Enu')

    assert [
             %{name: "all?", arity: 1},
             %{name: "all?", arity: 2},
             %{name: "any?", arity: 1},
             %{name: "any?", arity: 2},
             %{name: "at", arity: 2},
             %{name: "at", arity: 3}
           ] = expand('&Enum.a')

    assert [
             %{name: "all?", arity: 1},
             %{name: "all?", arity: 2},
             %{name: "any?", arity: 1},
             %{name: "any?", arity: 2},
             %{name: "at", arity: 2},
             %{name: "at", arity: 3}
           ] = expand('f = &Enum.a')
  end

  defmodule SublevelTest.LevelA.LevelB do
  end

  test "elixir completion sublevel" do
    assert [%{name: "LevelA"}] =
             expand('ElixirSense.Providers.Suggestion.CompleteTest.SublevelTest.')
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

    assert [%{name: "MyList"}] = expand('MyL', env)
    assert [%{name: "MyList"}] = expand('MyList', env)

    assert [%{arity: 1, name: "to_integer"}, %{arity: 2, name: "to_integer"}] =
             expand('MyList.to_integer', env)
  end

  test "complete aliases of erlang modules" do
    env = %Env{
      aliases: [{EList, :lists}]
    }

    assert [%{name: "EList"}] = expand('EL', env)
    assert [%{name: "EList"}] = expand('EList', env)

    assert [
             %{arity: 2, name: "map"},
             %{arity: 3, name: "mapfoldl"},
             %{arity: 3, name: "mapfoldr"}
           ] = expand('EList.map', env)
  end

  test "complete local funs from scope module" do
    env = %Env{
      scope_module: MyModule,
      mods_and_funs: %{
        {MyModule, nil, nil} => %ModFunInfo{type: :defmodule},
        {MyModule, :my_fun_priv, nil} => %ModFunInfo{type: :defp},
        {MyModule, :my_fun_priv, 2} => %ModFunInfo{
          type: :defp,
          params: [[{:some, [], nil}, {:other, [], nil}]]
        },
        {MyModule, :my_fun_pub, nil} => %ModFunInfo{type: :def},
        {MyModule, :my_fun_pub, 1} => %ModFunInfo{type: :def, params: [[{:some, [], nil}]]},
        {MyModule, :my_macro_priv, nil} => %ModFunInfo{type: :defmacrop},
        {MyModule, :my_macro_priv, 1} => %ModFunInfo{
          type: :defmacrop,
          params: [[{:some, [], nil}]]
        },
        {MyModule, :my_macro_pub, nil} => %ModFunInfo{type: :defmacro},
        {MyModule, :my_macro_pub, 1} => %ModFunInfo{type: :defmacro, params: [[{:some, [], nil}]]},
        {MyModule, :my_guard_priv, nil} => %ModFunInfo{type: :defguardp},
        {MyModule, :my_guard_priv, 1} => %ModFunInfo{
          type: :defguardp,
          params: [[{:some, [], nil}]]
        },
        {MyModule, :my_guard_pub, nil} => %ModFunInfo{type: :defguard},
        {MyModule, :my_guard_pub, 1} => %ModFunInfo{type: :defguard, params: [[{:some, [], nil}]]},
        {MyModule, :my_delegated, nil} => %ModFunInfo{type: :defdelegate},
        {MyModule, :my_delegated, 1} => %ModFunInfo{
          type: :defdelegate,
          params: [[{:some, [], nil}]]
        },
        {OtherModule, nil, nil} => %ModFunInfo{},
        {OtherModule, :my_fun_pub_other, nil} => %ModFunInfo{type: :def},
        {OtherModule, :my_fun_pub_other, 1} => %ModFunInfo{
          type: :def,
          params: [[{:some, [], nil}]]
        }
      },
      specs: %{
        {MyModule, :my_fun_priv, 2} => %SpecInfo{
          kind: :spec,
          specs: ["@spec my_fun_priv(atom, integer) :: boolean"]
        }
      }
    }

    assert [_ | _] = expand('my_f', env)

    assert [
             %{
               name: "my_fun_priv",
               origin: "MyModule",
               args: "some, other",
               type: :function,
               spec: "@spec my_fun_priv(atom, integer) :: boolean"
             }
           ] = expand('my_fun_pr', env)

    assert [
             %{name: "my_fun_pub", origin: "MyModule", type: :function}
           ] = expand('my_fun_pu', env)

    assert [
             %{name: "my_macro_priv", origin: "MyModule", type: :macro}
           ] = expand('my_macro_pr', env)

    assert [
             %{name: "my_macro_pub", origin: "MyModule", type: :macro}
           ] = expand('my_macro_pu', env)

    assert [
             %{name: "my_guard_priv", origin: "MyModule", type: :macro}
           ] = expand('my_guard_pr', env)

    assert [
             %{name: "my_guard_pub", origin: "MyModule", type: :macro}
           ] = expand('my_guard_pu', env)

    assert [
             %{name: "my_delegated", origin: "MyModule", type: :function}
           ] = expand('my_de', env)
  end

  test "complete remote funs from imported module" do
    env = %Env{
      scope_module: MyModule,
      imports: [OtherModule],
      mods_and_funs: %{
        {OtherModule, nil, nil} => %ModFunInfo{type: :defmodule},
        {OtherModule, :my_fun_other_pub, nil} => %ModFunInfo{type: :def},
        {OtherModule, :my_fun_other_pub, 1} => %ModFunInfo{
          type: :def,
          params: [[{:some, [], nil}]]
        },
        {OtherModule, :my_fun_other_priv, nil} => %ModFunInfo{type: :defp},
        {OtherModule, :my_fun_other_priv, 1} => %ModFunInfo{
          type: :defp,
          params: [[{:some, [], nil}]]
        }
      }
    }

    assert [
             %{name: "my_fun_other_pub", origin: "OtherModule"}
           ] = expand('my_f', env)
  end

  test "complete remote funs" do
    env = %Env{
      scope_module: MyModule,
      mods_and_funs: %{
        {Some.OtherModule, nil, nil} => %ModFunInfo{type: :defmodule},
        {Some.OtherModule, :my_fun_other_pub, nil} => %ModFunInfo{type: :def},
        {Some.OtherModule, :my_fun_other_pub, 1} => %ModFunInfo{
          type: :def,
          params: [[{:some, [], nil}]]
        },
        {Some.OtherModule, :my_fun_other_priv, nil} => %ModFunInfo{type: :defp},
        {Some.OtherModule, :my_fun_other_priv, 1} => %ModFunInfo{
          type: :defp,
          params: [[{:some, [], nil}]]
        }
      }
    }

    assert [
             %{name: "my_fun_other_pub", origin: "Some.OtherModule"}
           ] = expand('Some.OtherModule.my_f', env)
  end

  test "complete remote funs from aliased module" do
    env = %Env{
      scope_module: MyModule,
      aliases: [{S, Some.OtherModule}],
      mods_and_funs: %{
        {Some.OtherModule, nil, nil} => %ModFunInfo{type: :defmodule},
        {Some.OtherModule, :my_fun_other_pub, nil} => %ModFunInfo{type: :def},
        {Some.OtherModule, :my_fun_other_pub, 1} => %ModFunInfo{
          type: :def,
          params: [[{:some, [], nil}]]
        },
        {Some.OtherModule, :my_fun_other_priv, nil} => %ModFunInfo{type: :defp},
        {Some.OtherModule, :my_fun_other_priv, 1} => %ModFunInfo{
          type: :defp,
          params: [[{:some, [], nil}]]
        }
      }
    }

    assert [
             %{name: "my_fun_other_pub", origin: "Some.OtherModule"}
           ] = expand('S.my_f', env)
  end

  test "complete modules" do
    env = %Env{
      scope_module: MyModule,
      aliases: [{MyAlias, Some.OtherModule.Nested}],
      mods_and_funs: %{
        {Some.OtherModule, nil, nil} => %ModFunInfo{type: :defmodule}
      }
    }

    assert [%{name: "Some", type: :module}] = expand('So', env)
    assert [%{name: "OtherModule", type: :module}] = expand('Some.', env)
    assert [%{name: "MyAlias", type: :module}] = expand('MyA', env)
  end

  test "alias rules" do
    env = %Env{
      scope_module: MyModule,
      aliases: [{Keyword, MyKeyword}],
      mods_and_funs: %{
        {MyKeyword, nil, nil} => %ModFunInfo{type: :defmodule},
        {MyKeyword, :values1, 0} => %ModFunInfo{type: :def, params: [[]]},
        {MyKeyword, :values1, nil} => %ModFunInfo{type: :def}
      }
    }

    assert [
             %{
               name: "values1",
               type: :function,
               args: "",
               arity: 0,
               origin: "MyKeyword",
               spec: "",
               summary: ""
             }
           ] = expand('Keyword.valu', env)

    assert [%{name: "values", type: :function, arity: 1, origin: "Keyword"}] =
             expand('Elixir.Keyword.valu', env)
  end

  defmodule MyStruct do
    defstruct [:my_val, :some_map, :a_mod, :str, :unknown_str]
  end

  test "completion for structs" do
    assert [%{name: "MyStruct"}] = expand('%ElixirSense.Providers.Suggestion.CompleteTest.MyStr')
  end

  test "completion for struct keys" do
    env = %Env{
      vars: [
        %VarInfo{
          name: :struct,
          type:
            {:struct,
             [
               a_mod: {:atom, String},
               some_map: {:map, [asdf: 1], nil},
               str: {:struct, [], {:atom, MyStruct}, nil},
               unknown_str: {:struct, [abc: nil], nil, nil}
             ], {:atom, MyStruct}, nil}
        }
      ]
    }

    assert expand('struct.my', env) ==
             [
               %{
                 name: "my_val",
                 subtype: :struct_field,
                 type: :field,
                 origin: "ElixirSense.Providers.Suggestion.CompleteTest.MyStruct",
                 call?: true
               }
             ]

    assert expand('struct.some_m', env) ==
             [
               %{
                 name: "some_map",
                 subtype: :struct_field,
                 type: :field,
                 origin: "ElixirSense.Providers.Suggestion.CompleteTest.MyStruct",
                 call?: true
               }
             ]

    assert expand('struct.some_map.', env) ==
             [%{name: "asdf", subtype: :map_key, type: :field, origin: nil, call?: true}]

    assert expand('struct.str.', env) ==
             [
               %{
                 name: "__struct__",
                 subtype: :struct_field,
                 type: :field,
                 origin: "ElixirSense.Providers.Suggestion.CompleteTest.MyStruct",
                 call?: true
               },
               %{
                 name: "a_mod",
                 subtype: :struct_field,
                 type: :field,
                 origin: "ElixirSense.Providers.Suggestion.CompleteTest.MyStruct",
                 call?: true
               },
               %{
                 name: "my_val",
                 subtype: :struct_field,
                 type: :field,
                 origin: "ElixirSense.Providers.Suggestion.CompleteTest.MyStruct",
                 call?: true
               },
               %{
                 name: "some_map",
                 subtype: :struct_field,
                 type: :field,
                 origin: "ElixirSense.Providers.Suggestion.CompleteTest.MyStruct",
                 call?: true
               },
               %{
                 name: "str",
                 subtype: :struct_field,
                 type: :field,
                 origin: "ElixirSense.Providers.Suggestion.CompleteTest.MyStruct",
                 call?: true
               },
               %{
                 name: "unknown_str",
                 subtype: :struct_field,
                 type: :field,
                 origin: "ElixirSense.Providers.Suggestion.CompleteTest.MyStruct",
                 call?: true
               }
             ]

    assert expand('struct.str', env) ==
             [
               %{
                 name: "str",
                 subtype: :struct_field,
                 type: :field,
                 origin: "ElixirSense.Providers.Suggestion.CompleteTest.MyStruct",
                 call?: true
               }
             ]

    assert expand('struct.unknown_str.', env) ==
             [
               %{
                 call?: true,
                 name: "__struct__",
                 origin: nil,
                 subtype: :struct_field,
                 type: :field
               },
               %{call?: true, name: "abc", origin: nil, subtype: :struct_field, type: :field}
             ]
  end

  test "ignore invalid Elixir module literals" do
    defmodule :"ElixirSense.Providers.Suggestion.CompleteTest.Unicodé", do: nil
    assert expand('ElixirSense.Providers.Suggestion.CompleteTest.Unicod') == []
  after
    :code.purge(:"ElixirSense.Providers.Suggestion.CompleteTest.Unicodé")
    :code.delete(:"ElixirSense.Providers.Suggestion.CompleteTest.Unicodé")
  end

  defmodule MyMacro do
    @compile {:no_warn_undefined, OtherModule}
    defmacro test(do: expr) do
      expr
    end

    def fun, do: :ok
    defguard guard(value) when is_integer(value) and rem(value, 2) == 0
    defdelegate delegated(par), to: OtherModule
  end

  test "complete macros and functions from not loaded modules" do
    assert [%{name: "test", type: :macro}] =
             expand('ElixirSense.Providers.Suggestion.CompleteTest.MyMacro.te')

    assert [%{name: "fun", type: :function}] =
             expand('ElixirSense.Providers.Suggestion.CompleteTest.MyMacro.f')

    assert [%{name: "guard", type: :macro}] =
             expand('ElixirSense.Providers.Suggestion.CompleteTest.MyMacro.g')

    assert [%{name: "delegated", type: :function}] =
             expand('ElixirSense.Providers.Suggestion.CompleteTest.MyMacro.de')
  end

  test "complete build in functions on non local calls" do
    assert [] = expand('module_')
    assert [] = expand('__in')

    assert [] = expand('Elixir.mo')
    assert [] = expand('Elixir.__in')

    assert [
             %{
               name: "module_info",
               type: :function,
               arity: 0,
               spec:
                 "@spec module_info :: [{:module | :attributes | :compile | :exports | :md5 | :native, term}]"
             },
             %{
               name: "module_info",
               type: :function,
               arity: 1,
               spec:
                 "@spec module_info(:module) :: atom\n@spec module_info(:attributes | :compile) :: [{atom, term}]\n@spec module_info(:md5) :: binary\n@spec module_info(:exports | :functions | :nifs) :: [{atom, non_neg_integer}]\n@spec module_info(:native) :: boolean"
             }
           ] = expand('ElixirSense.Providers.Suggestion.CompleteTest.MyMacro.mo')

    assert [
             %{
               name: "__info__",
               type: :function,
               spec:
                 "@spec __info__(:attributes) :: keyword()\n@spec __info__(:compile) :: [term()]\n@spec __info__(:functions) :: [{atom, non_neg_integer}]\n@spec __info__(:macros) :: [{atom, non_neg_integer}]\n@spec __info__(:md5) :: binary()\n@spec __info__(:module) :: module()"
             }
           ] = expand('ElixirSense.Providers.Suggestion.CompleteTest.MyMacro.__in')

    assert [
             %{
               name: "module_info",
               type: :function,
               arity: 0,
               spec:
                 "@spec module_info :: [{:module | :attributes | :compile | :exports | :md5 | :native, term}]"
             },
             %{
               name: "module_info",
               type: :function,
               arity: 1,
               spec:
                 "@spec module_info(:module) :: atom\n@spec module_info(:attributes | :compile) :: [{atom, term}]\n@spec module_info(:md5) :: binary\n@spec module_info(:exports | :functions | :nifs) :: [{atom, non_neg_integer}]\n@spec module_info(:native) :: boolean"
             }
           ] = expand(':ets.module_')

    assert [] = expand(':ets.__in')

    env = %Env{
      scope_module: MyModule,
      aliases: [{MyAlias, Some.OtherModule.Nested}],
      mods_and_funs: %{
        {MyModule, nil, nil} => %ModFunInfo{type: :defmodule},
        {MyModule, :module_info, nil} => %ModFunInfo{type: :def},
        {MyModule, :module_info, 0} => %ModFunInfo{type: :def, params: [[]]},
        {MyModule, :module_info, 1} => %ModFunInfo{type: :def, params: [[{:atom, [], nil}]]},
        {MyModule, :__info__, nil} => %ModFunInfo{type: :def},
        {MyModule, :__info__, 1} => %ModFunInfo{type: :def, params: [[{:atom, [], nil}]]}
      }
    }

    assert [] = expand('module_', env)
    assert [] = expand('__in', env)

    assert [
             %{
               name: "module_info",
               type: :function,
               arity: 0,
               spec:
                 "@spec module_info :: [{:module | :attributes | :compile | :exports | :md5 | :native, term}]"
             },
             %{
               name: "module_info",
               type: :function,
               arity: 1,
               spec:
                 "@spec module_info(:module) :: atom\n@spec module_info(:attributes | :compile) :: [{atom, term}]\n@spec module_info(:md5) :: binary\n@spec module_info(:exports | :functions | :nifs) :: [{atom, non_neg_integer}]\n@spec module_info(:native) :: boolean"
             }
           ] = expand('MyModule.mo', env)

    assert [
             %{
               name: "__info__",
               type: :function,
               spec:
                 "@spec __info__(:attributes) :: keyword()\n@spec __info__(:compile) :: [term()]\n@spec __info__(:functions) :: [{atom, non_neg_integer}]\n@spec __info__(:macros) :: [{atom, non_neg_integer}]\n@spec __info__(:md5) :: binary()\n@spec __info__(:module) :: module()"
             }
           ] = expand('MyModule.__in', env)
  end

  test "complete build in behaviour functions" do
    assert [] = expand('Elixir.beh')

    assert [
             %{
               name: "behaviour_info",
               type: :function,
               arity: 1,
               spec:
                 "@spec behaviour_info(:callbacks | :optional_callbacks) :: [{atom, non_neg_integer}]"
             }
           ] = expand(':gen_server.beh')

    assert [
             %{
               name: "behaviour_info",
               type: :function,
               arity: 1,
               spec:
                 "@spec behaviour_info(:callbacks | :optional_callbacks) :: [{atom, non_neg_integer}]"
             }
           ] = expand('GenServer.beh')
  end

  test "complete build in protocol functions" do
    assert [] = expand('Elixir.__pr')

    assert [
             %{
               name: "__protocol__",
               type: :function,
               arity: 1,
               spec:
                 "@spec __protocol__(:module) :: module\n@spec __protocol__(:functions) :: [{atom, non_neg_integer}]\n@spec __protocol__(:consolidated?) :: boolean\n@spec __protocol__(:impls) :: :not_consolidated | {:consolidated, [module]}"
             }
           ] = expand('Enumerable.__pro')

    assert [_, _] = expand('Enumerable.imp')

    assert [
             %{
               name: "impl_for!",
               type: :function,
               arity: 1,
               spec: "@spec impl_for!(term) :: atom"
             }
           ] = expand('Enumerable.impl_for!')
  end

  test "complete build in protocol implementation functions" do
    assert [] = expand('Elixir.__im')

    assert [
             %{
               name: "__impl__",
               type: :function,
               arity: 1,
               spec: "@spec __impl__(:for | :target | :protocol) :: module"
             }
           ] = expand('Enumerable.List.__im')
  end

  test "complete build in struct functions" do
    assert [] = expand('Elixir.__str')

    assert [
             %{
               name: "__struct__",
               type: :function,
               arity: 0,
               spec:
                 "@spec __struct__() :: %{required(:__struct__) => module, optional(any) => any}"
             },
             %{
               name: "__struct__",
               type: :function,
               arity: 1,
               spec:
                 "@spec __struct__(keyword) :: %{required(:__struct__) => module, optional(any) => any}"
             }
           ] = expand('ElixirSenseExample.ModuleWithStruct.__str')
  end

  test "complete build in exception functions" do
    assert [] = expand('Elixir.mes')

    assert [
             %{
               name: "message",
               type: :function,
               arity: 1,
               spec: "@spec message(Exception.t()) :: String.t()"
             }
           ] = expand('ArgumentError.mes')

    assert [] = expand('Elixir.exce')

    assert [
             %{
               name: "exception",
               type: :function,
               arity: 1,
               spec: "@spec exception(term) :: Exception.t()"
             }
           ] = expand('ArgumentError.exce')

    assert [] = expand('Elixir.bla')

    assert [
             %{name: "blame", type: :function, arity: 2}
           ] = expand('ArgumentError.bla')
  end

  @tag requires_otp_23: true
  test "complete build in :erlang functions" do
    assert [
             %{arity: 2, name: "open_port", origin: ":erlang"},
             %{
               arity: 2,
               name: "or",
               spec: "@spec boolean or boolean :: boolean",
               type: :function,
               args: "boolean(), boolean()",
               origin: ":erlang",
               summary: ""
             },
             %{
               args: "term, term",
               arity: 2,
               name: "orelse",
               origin: ":erlang",
               spec: "",
               summary: "",
               type: :function
             }
           ] = expand(':erlang.or')

    assert [
             %{
               arity: 2,
               name: "and",
               spec: "@spec boolean and boolean :: boolean",
               type: :function,
               args: "boolean(), boolean()",
               origin: ":erlang",
               summary: ""
             },
             %{
               args: "term, term",
               arity: 2,
               name: "andalso",
               origin: ":erlang",
               spec: "",
               summary: "",
               type: :function
             },
             %{arity: 2, name: "append", origin: ":erlang"},
             %{arity: 2, name: "append_element", origin: ":erlang"}
           ] = expand(':erlang.and')
  end

  test "provide doc and specs for erlang functions" do
    assert [
             %{
               arity: 1,
               name: "whereis",
               origin: ":erlang",
               spec: "@spec whereis(regName) :: pid | port | :undefined when regName: atom",
               type: :function
             }
           ] = expand(':erlang.where')

    assert [
             %{
               arity: 1,
               name: "cancel_timer",
               spec:
                 "@spec cancel_timer(timerRef) :: result when timerRef: reference, time: non_neg_integer, result: time | false",
               type: :function,
               args: "timerRef",
               origin: ":erlang",
               summary:
                 "Cancels a timer\\. The same as calling [`erlang:cancel_timer(TimerRef, [])`]\\."
             },
             %{
               arity: 2,
               name: "cancel_timer",
               spec:
                 "@spec cancel_timer(timerRef, options) :: result | :ok when timerRef: reference, async: boolean, info: boolean, option: {:async, async} | {:info, info}, options: [option], time: non_neg_integer, result: time | false",
               type: :function,
               args: "timerRef, options",
               origin: ":erlang",
               summary:
                 "Cancels a timer that has been created by [`erlang:start_timer`] or [`erlang:send_after`]\\. `TimerRef` identifies the timer, and was returned by the BIF that created the timer\\."
             }
           ] = expand(':erlang.cancel_time')
  end

  @tag :edoc_fallback
  test "profide specs for erlang functions edoc" do
    assert [
             %{
               args: "term",
               arity: 1,
               name: "files",
               origin: ":edoc",
               spec: "",
               summary: "",
               type: :function
             },
             %{
               args: "term, term",
               arity: 2,
               name: "files",
               origin: ":edoc",
               spec: "",
               summary:
                 "Runs EDoc on a given set of source files. See run/2 for\ndetails, including options.",
               type: :function
             }
           ] = expand(':edoc.files')
  end

  test "complete after ! operator" do
    assert [%{name: "is_binary"}] = expand('!is_bina')
  end

  test "correctly find subtype and doc for modules that have submodule" do
    assert [
             %{
               name: "File",
               type: :module,
               metadata: %{},
               subtype: nil,
               summary: "This module contains functions to manipulate files."
             }
           ] = expand('Fi')
  end

  test "complete only struct modules after %" do
    assert list = expand('%')
    refute Enum.any?(list, &(&1.type != :module))
    assert Enum.any?(list, &(&1.name == "ArithmeticError"))
    assert Enum.any?(list, &(&1.name == "URI"))
    refute Enum.any?(list, &(&1.name == "File"))
    refute Enum.any?(list, &(&1.subtype not in [:struct, :exception]))

    assert [_ | _] = expand('%Fi')
    assert list = expand('%File.')
    assert Enum.any?(list, &(&1.name == "CopyError"))
    refute Enum.any?(list, &(&1.type != :module))
    refute Enum.any?(list, &(&1.subtype not in [:struct, :exception]))
  end

  test "complete modules and local funs after &" do
    assert list = expand('&')
    assert Enum.any?(list, &(&1.type == :module))
    assert Enum.any?(list, &(&1.type == :function))
    refute Enum.any?(list, &(&1.type not in [:function, :module, :macro]))
  end

  test "complete Kernel.SpecialForms macros with fixed argument list" do
    assert [%{args_list: ["term"]}] = expand('Kernel.SpecialForms.fn')
  end
end
