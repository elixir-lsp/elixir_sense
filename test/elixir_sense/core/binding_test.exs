defmodule ElixirSense.Core.BindingTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.State.AttributeInfo
  alias ElixirSense.Core.State.VarInfo
  alias ElixirSense.Core.State.ModFunInfo
  alias ElixirSense.Core.State.SpecInfo
  alias ElixirSense.Core.State.StructInfo
  alias ElixirSense.Core.State.TypeInfo

  @env %Binding{}

  describe "expand" do
    test "map" do
      assert {:map, [abc: nil, cde: {:variable, :a}], nil} ==
               Binding.expand(@env, {:map, [abc: nil, cde: {:variable, :a}], nil})
    end

    test "map update" do
      assert {:map, [{:efg, {:atom, :a}}, {:abc, nil}, {:cde, {:variable, :a}}], nil} ==
               Binding.expand(
                 @env,
                 {:map, [abc: nil, cde: {:variable, :a}],
                  {:map, [abc: nil, cde: nil, efg: {:atom, :a}], nil}}
               )
    end

    test "introspection struct" do
      assert {:struct,
              [
                __struct__: {:atom, ElixirSenseExample.ModuleWithTypedStruct},
                other: nil,
                typed_field: nil
              ], ElixirSenseExample.ModuleWithTypedStruct,
              nil} ==
               Binding.expand(
                 @env,
                 {:struct, [], {:atom, ElixirSenseExample.ModuleWithTypedStruct}, nil}
               )
    end

    test "introspection module not a stuct" do
      assert nil ==
               Binding.expand(@env, {:struct, [], {:atom, ElixirSenseExample.EmptyModule}, nil})
    end

    test "introspection struct update" do
      assert {:struct,
              [
                __struct__: {:atom, ElixirSenseExample.ModuleWithTypedStruct},
                other: {:atom, :a},
                typed_field: {:atom, :b}
              ], ElixirSenseExample.ModuleWithTypedStruct,
              nil} ==
               Binding.expand(
                 @env,
                 {:struct, [typed_field: {:atom, :b}],
                  {:atom, ElixirSenseExample.ModuleWithTypedStruct},
                  {:struct, [other: {:atom, :a}],
                   {:atom, ElixirSenseExample.ModuleWithTypedStruct}, nil}}
               )
    end

    test "introspection struct update as map" do
      assert {:struct,
              [
                __struct__: {:atom, ElixirSenseExample.ModuleWithTypedStruct},
                other: {:atom, :a},
                typed_field: {:atom, :b}
              ], ElixirSenseExample.ModuleWithTypedStruct,
              nil} ==
               Binding.expand(
                 @env,
                 {:map, [typed_field: {:atom, :b}],
                  {:struct, [other: {:atom, :a}],
                   {:atom, ElixirSenseExample.ModuleWithTypedStruct}, nil}}
               )
    end

    test "introspection struct from attribute" do
      assert {:struct,
              [
                __struct__: {:atom, ElixirSenseExample.ModuleWithTypedStruct},
                other: nil,
                typed_field: nil
              ], ElixirSenseExample.ModuleWithTypedStruct,
              nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:attributes, [
                   %AttributeInfo{
                     name: :v,
                     type: {:atom, ElixirSenseExample.ModuleWithTypedStruct}
                   }
                 ]),
                 {:struct, [], {:attribute, :v}, nil}
               )
    end

    test "introspection struct from variable" do
      assert {:struct, [__struct__: nil], nil, nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{name: :v, type: {:atom, ElixirSenseExample.ModuleWithTypedStruct}}
                 ]),
                 {:struct, [], {:variable, :v}, nil}
               )
    end

    test "metadata struct" do
      assert {:struct, [__struct__: {:atom, MyMod}, abc: nil], MyMod, nil} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   structs: %{
                     MyMod => %StructInfo{
                       fields: [abc: nil, __struct__: MyMod]
                     }
                   }
                 }),
                 {:struct, [], {:atom, MyMod}, nil}
               )
    end

    test "atom" do
      assert {:atom, :abc} == Binding.expand(@env, {:atom, :abc})
    end

    test "nil" do
      assert nil == Binding.expand(@env, nil)
    end

    test "other" do
      assert nil == Binding.expand(@env, 123)
    end

    test "known variable" do
      assert {:atom, :abc} ==
               Binding.expand(
                 @env |> Map.put(:variables, [%VarInfo{name: :v, type: {:atom, :abc}}]),
                 {:variable, :v}
               )
    end

    test "anonymous variable" do
      assert nil ==
               Binding.expand(
                 @env |> Map.put(:variables, [%VarInfo{name: :_, type: {:atom, :abc}}]),
                 {:variable, :_}
               )
    end

    test "unknown variable" do
      assert nil == Binding.expand(@env, {:variable, :v})
    end

    test "known attribute" do
      assert {:atom, :abc} ==
               Binding.expand(
                 @env |> Map.put(:attributes, [%AttributeInfo{name: :v, type: {:atom, :abc}}]),
                 {:attribute, :v}
               )
    end

    test "unknown attribute" do
      assert nil == Binding.expand(@env, {:attribute, :v})
    end

    test "call existing map field access" do
      assert {:atom, :a} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{name: :map, type: {:map, [field: {:atom, :a}], nil}},
                   %VarInfo{name: :ref, type: {:call, {:variable, :map}, :field, 0}}
                 ]),
                 {:variable, :ref}
               )
    end

    test "call existing map field access invalid arity" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{name: :map, type: {:map, [field: {:atom, :a}], nil}},
                   %VarInfo{name: :ref, type: {:call, {:variable, :map}, :field, 1}}
                 ]),
                 {:variable, :ref}
               )
    end

    test "call not existing map field access" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{name: :map, type: {:map, [field: {:atom, :a}], nil}},
                   %VarInfo{name: :ref, type: {:call, {:variable, :map}, :not_existing, 0}}
                 ]),
                 {:variable, :ref}
               )
    end

    test "call existing struct field access" do
      assert {:atom, :abc} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :map,
                     type:
                       {:struct, [typed_field: {:atom, :abc}],
                        {:atom, ElixirSenseExample.ModuleWithTypedStruct}, nil}
                   },
                   %VarInfo{name: :ref, type: {:call, {:variable, :map}, :typed_field, 0}}
                 ]),
                 {:variable, :ref}
               )
    end

    test "call not existing struct field access" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :map,
                     type:
                       {:struct, [typed_field: {:atom, :abc}],
                        {:atom, ElixirSenseExample.ModuleWithTypedStruct}, nil}
                   },
                   %VarInfo{name: :ref, type: {:call, {:variable, :map}, :not_existing, 0}}
                 ]),
                 {:variable, :ref}
               )
    end

    test "call existing struct field access invalid arity" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :map,
                     type:
                       {:struct, [typed_field: {:atom, :abc}],
                        {:atom, ElixirSenseExample.ModuleWithTypedStruct}, nil}
                   },
                   %VarInfo{name: :ref, type: {:call, {:variable, :map}, :typed_field, 1}}
                 ]),
                 {:variable, :ref}
               )
    end

    test "call on nil" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{name: :ref, type: {:call, nil, :not_existing, 0}}
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call not existing fun" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :not_existing,
                        0}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call existing fun invalid arity" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f1, 1}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with spec t undefined" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f01, 0}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with spec t expanding to atom" do
      assert {:atom, :asd} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f02, 0}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with spec t expanding to number" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f03, 0}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with spec local t expanding to struct" do
      assert {:struct,
              [
                __struct__: {:atom, ElixirSenseExample.FunctionsWithReturnSpec},
                abc: {:map, [key: {:atom, nil}], nil}
              ], ElixirSenseExample.FunctionsWithReturnSpec,
              nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f1, 0}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with spec remote t expanding to struct" do
      assert {:struct,
              [__struct__: {:atom, ElixirSenseExample.FunctionsWithReturnSpec.Remote}, abc: nil],
              ElixirSenseExample.FunctionsWithReturnSpec.Remote,
              nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f3, 0}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with spec struct" do
      assert {:struct,
              [__struct__: {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, abc: nil],
              ElixirSenseExample.FunctionsWithReturnSpec,
              nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f5, 0}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with spec local t expanding to map" do
      assert {:map, [{:abc, {:atom, :asd}}, {:cde, {:atom, :asd}}], nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f2, 0}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with spec remote t expanding to map" do
      assert {:map, [abc: nil], nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f4, 0}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with spec map" do
      assert {:map, [abc: nil], nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f6, 0}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with spec intersection different returns" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f7, 0}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with spec intersection same returns" do
      assert {:map, [abc: nil], nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f71, 1}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with spec union" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f8, 0}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with spec parametrized map" do
      assert {:map, [abc: nil], nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f91, 0}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "local call metadata fun returning struct" do
      assert {:struct, [{:__struct__, {:atom, MyMod}}, {:abc, nil}], MyMod, nil} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   variables: [
                     %VarInfo{name: :ref, type: {:local_call, :fun, 0}}
                   ],
                   current_module: MyMod,
                   specs: %{
                     {MyMod, :fun, nil} => %SpecInfo{
                       specs: ["@spec fun() :: %MyMod{}"]
                     },
                     {MyMod, :fun, 0} => %SpecInfo{
                       specs: ["@spec fun() :: %MyMod{}"]
                     }
                   },
                   mods_and_funs: %{
                     {MyMod, :fun, nil} => %ModFunInfo{
                       params: [[]],
                       type: :defp
                     }
                   },
                   structs: %{
                     MyMod => %StructInfo{
                       fields: [abc: nil, __struct__: MyMod]
                     }
                   }
                 }),
                 {:variable, :ref}
               )
    end

    test "local call metadata fun returning local type expanding to struct" do
      assert {:struct, [{:__struct__, {:atom, MyMod}}, {:abc, nil}], MyMod, nil} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   variables: [
                     %VarInfo{name: :ref, type: {:local_call, :fun, 0}}
                   ],
                   current_module: MyMod,
                   specs: %{
                     {MyMod, :fun, nil} => %SpecInfo{
                       specs: ["@spec fun() :: t()"]
                     },
                     {MyMod, :fun, 0} => %SpecInfo{
                       specs: ["@spec fun() :: t()"]
                     }
                   },
                   mods_and_funs: %{
                     {MyMod, :fun, nil} => %ModFunInfo{
                       params: [[]],
                       type: :def
                     }
                   },
                   structs: %{
                     MyMod => %StructInfo{
                       fields: [abc: nil, __struct__: MyMod]
                     }
                   },
                   types: %{
                     {MyMod, :t, nil} => %TypeInfo{
                       specs: ["@type t() :: %MyMod{}"]
                     },
                     {MyMod, :t, 0} => %TypeInfo{
                       specs: ["@type t() :: %MyMod{}"]
                     }
                   }
                 }),
                 {:variable, :ref}
               )
    end

    test "local call metadata fun returning local type expanding to private type" do
      assert {:struct, [{:__struct__, {:atom, MyMod}}, {:abc, nil}], MyMod, nil} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   variables: [
                     %VarInfo{name: :ref, type: {:local_call, :fun, 0}}
                   ],
                   current_module: MyMod,
                   specs: %{
                     {MyMod, :fun, nil} => %SpecInfo{
                       specs: ["@spec fun() :: t()"]
                     },
                     {MyMod, :fun, 0} => %SpecInfo{
                       specs: ["@spec fun() :: t()"]
                     }
                   },
                   mods_and_funs: %{
                     {MyMod, :fun, nil} => %ModFunInfo{
                       params: [[]],
                       type: :def
                     }
                   },
                   structs: %{
                     MyMod => %StructInfo{
                       fields: [abc: nil, __struct__: MyMod]
                     }
                   },
                   types: %{
                     {MyMod, :t, nil} => %TypeInfo{
                       kind: :typep,
                       specs: ["@type t() :: %MyMod{}"]
                     },
                     {MyMod, :t, 0} => %TypeInfo{
                       kind: :typep,
                       specs: ["@type t() :: %MyMod{}"]
                     }
                   }
                 }),
                 {:variable, :ref}
               )
    end

    test "remote call metadata public fun returning local type expanding to struct" do
      assert {:struct, [{:__struct__, {:atom, MyMod}}, {:abc, nil}], MyMod, nil} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   variables: [
                     %VarInfo{name: :ref, type: {:call, {:atom, MyMod}, :fun, 0}}
                   ],
                   current_module: SomeMod,
                   specs: %{
                     {MyMod, :fun, nil} => %SpecInfo{
                       specs: ["@spec fun() :: t()"]
                     },
                     {MyMod, :fun, 0} => %SpecInfo{
                       specs: ["@spec fun() :: t()"]
                     }
                   },
                   mods_and_funs: %{
                     {MyMod, :fun, nil} => %ModFunInfo{
                       params: [[]],
                       type: :def
                     }
                   },
                   structs: %{
                     MyMod => %StructInfo{
                       fields: [abc: nil, __struct__: MyMod]
                     }
                   },
                   types: %{
                     {MyMod, :t, nil} => %TypeInfo{
                       specs: ["@type t() :: %MyMod{}"]
                     },
                     {MyMod, :t, 0} => %TypeInfo{
                       specs: ["@type t() :: %MyMod{}"]
                     }
                   }
                 }),
                 {:variable, :ref}
               )
    end

    test "remote call metadata public fun returning local type expanding to opaque" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   variables: [
                     %VarInfo{name: :ref, type: {:call, {:atom, MyMod}, :fun, 0}}
                   ],
                   current_module: SomeMod,
                   specs: %{
                     {MyMod, :fun, nil} => %SpecInfo{
                       specs: ["@spec fun() :: t()"]
                     },
                     {MyMod, :fun, 0} => %SpecInfo{
                       specs: ["@spec fun() :: t()"]
                     }
                   },
                   mods_and_funs: %{
                     {MyMod, :fun, nil} => %ModFunInfo{
                       params: [[]],
                       type: :def
                     }
                   },
                   structs: %{
                     MyMod => %StructInfo{
                       fields: [abc: nil, __struct__: MyMod]
                     }
                   },
                   types: %{
                     {MyMod, :t, nil} => %TypeInfo{
                       kind: :opaque,
                       specs: ["@type t() :: %MyMod{}"]
                     },
                     {MyMod, :t, 0} => %TypeInfo{
                       kind: :opaque,
                       specs: ["@type t() :: %MyMod{}"]
                     }
                   }
                 }),
                 {:variable, :ref}
               )
    end

    test "remote call metadata private fun returning local type expanding to struct" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   variables: [
                     %VarInfo{name: :ref, type: {:call, {:atom, MyMod}, :fun, 0}}
                   ],
                   current_module: SomeMod,
                   specs: %{
                     {MyMod, :fun, nil} => %SpecInfo{
                       specs: ["@spec fun() :: t()"]
                     },
                     {MyMod, :fun, 0} => %SpecInfo{
                       specs: ["@spec fun() :: t()"]
                     }
                   },
                   mods_and_funs: %{
                     {MyMod, :fun, nil} => %ModFunInfo{
                       params: [[]],
                       type: :defp
                     }
                   },
                   structs: %{
                     MyMod => %StructInfo{
                       fields: [abc: nil, __struct__: MyMod]
                     }
                   },
                   types: %{
                     {MyMod, :t, nil} => %TypeInfo{
                       specs: ["@type t() :: %MyMod{}"]
                     },
                     {MyMod, :t, 0} => %TypeInfo{
                       specs: ["@type t() :: %MyMod{}"]
                     }
                   }
                 }),
                 {:variable, :ref}
               )
    end

    test "local call metadata fun with default args returning struct" do
      env =
        @env
        |> Map.merge(%{
          current_module: MyMod,
          specs: %{
            {MyMod, :fun, nil} => %SpecInfo{
              specs: ["@spec fun(integer(), integer(), any()) :: %MyMod{}"]
            },
            {MyMod, :fun, 3} => %SpecInfo{
              specs: ["@spec fun(integer(), integer(), any()) :: %MyMod{}"]
            }
          },
          mods_and_funs: %{
            {MyMod, :fun, nil} => %ModFunInfo{
              params: [
                [
                  {:\\, [], []},
                  {:\\, [], []},
                  {:x, [], []}
                ]
              ],
              type: :def
            }
          },
          structs: %{
            MyMod => %StructInfo{
              fields: [abc: nil, __struct__: MyMod]
            }
          }
        })

      assert nil ==
               Binding.expand(
                 env
                 |> Map.put(:variables, [
                   %VarInfo{name: :ref, type: {:local_call, :fun, 0}}
                 ]),
                 {:variable, :ref}
               )

      assert {:struct, [{:__struct__, {:atom, MyMod}}, {:abc, nil}], MyMod, nil} ==
               Binding.expand(
                 env
                 |> Map.put(:variables, [
                   %VarInfo{name: :ref, type: {:local_call, :fun, 1}}
                 ]),
                 {:variable, :ref}
               )

      assert {:struct, [{:__struct__, {:atom, MyMod}}, {:abc, nil}], MyMod, nil} ==
               Binding.expand(
                 env
                 |> Map.put(:variables, [
                   %VarInfo{name: :ref, type: {:local_call, :fun, 2}}
                 ]),
                 {:variable, :ref}
               )

      assert {:struct, [{:__struct__, {:atom, MyMod}}, {:abc, nil}], MyMod, nil} ==
               Binding.expand(
                 env
                 |> Map.put(:variables, [
                   %VarInfo{name: :ref, type: {:local_call, :fun, 3}}
                 ]),
                 {:variable, :ref}
               )

      assert nil ==
               Binding.expand(
                 env
                 |> Map.put(:variables, [
                   %VarInfo{name: :ref, type: {:local_call, :fun, 4}}
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with default args" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f10, 0}
                   }
                 ]),
                 {:variable, :ref}
               )

      assert {:atom, String} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f10, 1}
                   }
                 ]),
                 {:variable, :ref}
               )

      assert {:atom, String} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f10, 2}
                   }
                 ]),
                 {:variable, :ref}
               )

      assert {:atom, String} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f10, 3}
                   }
                 ]),
                 {:variable, :ref}
               )

      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f10, 4}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "local call imported fun with spec t expanding to atom" do
      assert {:atom, :asd} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   variables: [
                     %VarInfo{name: :ref, type: {:local_call, :f02, 0}}
                   ],
                   imports: [ElixirSenseExample.FunctionsWithReturnSpec]
                 }),
                 {:variable, :ref}
               )
    end

    test "local call no parens imported fun with spec t expanding to atom" do
      assert {:atom, :asd} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   variables: [
                     %VarInfo{name: :ref, type: {:variable, :f02}}
                   ],
                   imports: [ElixirSenseExample.FunctionsWithReturnSpec]
                 }),
                 {:variable, :ref}
               )
    end

    test "extract struct key type from typespec" do
      assert {:map, [key: {:atom, nil}], nil} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   variables: [
                     %VarInfo{
                       name: :ref,
                       type:
                         {:call,
                          {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f1, 0},
                          :abc, 0}
                     }
                   ],
                   imports: [ElixirSenseExample.FunctionsWithReturnSpec]
                 }),
                 {:variable, :ref}
               )
    end

    test "extract required map key type from typespec" do
      assert {:atom, :asd} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   variables: [
                     %VarInfo{
                       name: :ref,
                       type:
                         {:call,
                          {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f2, 0},
                          :abc, 0}
                     }
                   ],
                   imports: [ElixirSenseExample.FunctionsWithReturnSpec]
                 }),
                 {:variable, :ref}
               )
    end

    test "optimisticallyb extract optional map key type from typespec" do
      assert {:atom, :asd} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   variables: [
                     %VarInfo{
                       name: :ref,
                       type:
                         {:call,
                          {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f2, 0},
                          :cde, 0}
                     }
                   ],
                   imports: [ElixirSenseExample.FunctionsWithReturnSpec]
                 }),
                 {:variable, :ref}
               )
    end
  end
end
