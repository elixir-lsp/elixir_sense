defmodule ElixirSense.Core.BindingTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.State.AttributeInfo
  alias ElixirSense.Core.State.VarInfo
  alias ElixirSense.Core.State.ModFunInfo
  alias ElixirSense.Core.State.SpecInfo
  alias ElixirSense.Core.State.StructInfo
  alias ElixirSense.Core.State.TypeInfo

  @env %Binding{
    functions: __ENV__.functions,
    macros: __ENV__.macros
  }

  describe "expand" do
    def build_dependency_injection_binding(fetcher \\ :get_env, default_value \\ nil)
        when is_atom(default_value) do
      arguments = [
        atom: :elixir_sense,
        atom: :some_attribute
      ]

      arguments = if(default_value, do: arguments ++ [{:atom, default_value}], else: arguments)

      attribute_info = %AttributeInfo{
        name: :some_module,
        type: {:call, {:atom, Application}, fetcher, arguments}
      }

      Map.put(@env, :attributes, [
        attribute_info
      ])
    end

    test "misconfigured dependency injection" do
      Application.delete_env(:elixir_sense, :some_attribute)

      unsafe_fetchers = [:fetch_env!, :compile_env!]
      safe_fetchers = [:get_env, :compile_env]

      Enum.each(unsafe_fetchers, fn fetcher ->
        assert :none ==
                 Binding.expand(
                   build_dependency_injection_binding(fetcher),
                   {:attribute, :some_module}
                 )
      end)

      Enum.each(safe_fetchers, fn fetcher ->
        assert {:atom, nil} ==
                 Binding.expand(
                   build_dependency_injection_binding(fetcher),
                   {:attribute, :some_module}
                 )
      end)
    end

    test "dependency injection without default value" do
      Application.put_env(:elixir_sense, :some_attribute, ElixirSenseExample.SameModule)

      fetchers = [:get_env, :fetch_env!, :compile_env, :compile_env!]

      Enum.each(fetchers, fn fetcher ->
        assert {:atom, ElixirSenseExample.SameModule} ==
                 Binding.expand(
                   build_dependency_injection_binding(fetcher),
                   {:attribute, :some_module}
                 )
      end)
    end

    test "dependency injection with default value" do
      assert {:atom, ElixirSenseExample.SameModule} ==
               Binding.expand(
                 build_dependency_injection_binding(:get_env, ElixirSenseExample.SameModule),
                 {:attribute, :some_module}
               )
    end

    test "map" do
      assert {:map, [abc: nil, cde: {:variable, :a, 1}], nil} ==
               Binding.expand(@env, {:map, [abc: nil, cde: {:variable, :a, 1}], nil})
    end

    test "map update" do
      assert {:map, [{:efg, {:atom, :a}}, {:abc, nil}, {:cde, {:variable, :a, 1}}], nil} ==
               Binding.expand(
                 @env,
                 {:map, [abc: nil, cde: {:variable, :a, 1}],
                  {:map, [abc: nil, cde: nil, efg: {:atom, :a}], nil}}
               )
    end

    test "introspection struct" do
      assert {:struct,
              [
                __struct__: {:atom, ElixirSenseExample.ModuleWithTypedStruct},
                other: nil,
                typed_field: nil
              ], {:atom, ElixirSenseExample.ModuleWithTypedStruct},
              nil} ==
               Binding.expand(
                 @env,
                 {:struct, [], {:atom, ElixirSenseExample.ModuleWithTypedStruct}, nil}
               )
    end

    test "introspection struct from guard" do
      assert {:struct, [__struct__: nil], nil, nil} ==
               Binding.expand(
                 @env,
                 {
                   :intersection,
                   [
                     {:intersection, [{:map, [], nil}, {:struct, [], nil, nil}]},
                     {:struct, [], nil, nil}
                   ]
                 }
               )

      assert {
               :struct,
               [
                 {:__struct__, {:atom, URI}} | _
               ],
               {:atom, URI},
               nil
             } =
               Binding.expand(
                 @env,
                 {
                   :intersection,
                   [
                     {:intersection, [{:map, [], nil}, {:struct, [], nil, nil}]},
                     {:struct, [], {:atom, URI}, nil}
                   ]
                 }
               )

      assert {:struct, [__struct__: nil, __exception__: {:atom, true}], nil, nil} ==
               Binding.expand(
                 @env,
                 {
                   :intersection,
                   [
                     {
                       :intersection,
                       [
                         {
                           :intersection,
                           [
                             {:intersection, [{:map, [], nil}, {:struct, [], nil, nil}]},
                             {:struct, [], nil, nil}
                           ]
                         },
                         {:map, [{:__exception__, nil}], nil}
                       ]
                     },
                     {:map, [{:__exception__, {:atom, true}}], nil}
                   ]
                 }
               )

      assert {
               :struct,
               [
                 {:__struct__, {:atom, ArgumentError}} | _
               ],
               {:atom, ArgumentError},
               nil
             } =
               Binding.expand(
                 @env,
                 {
                   :intersection,
                   [
                     {
                       :intersection,
                       [
                         {
                           :intersection,
                           [
                             {:intersection, [{:map, [], nil}, {:struct, [], nil, nil}]},
                             {:struct, [], {:atom, ArgumentError}, nil}
                           ]
                         },
                         {:map, [{:__exception__, nil}], nil}
                       ]
                     },
                     {:map, [{:__exception__, {:atom, true}}], nil}
                   ]
                 }
               )
    end

    test "introspection module not a struct" do
      assert :none ==
               Binding.expand(@env, {:struct, [], {:atom, ElixirSenseExample.EmptyModule}, nil})
    end

    test "introspection struct update" do
      assert {:struct,
              [
                __struct__: {:atom, ElixirSenseExample.ModuleWithTypedStruct},
                other: {:atom, :a},
                typed_field: {:atom, :b}
              ], {:atom, ElixirSenseExample.ModuleWithTypedStruct},
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
              ], {:atom, ElixirSenseExample.ModuleWithTypedStruct},
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
              ], {:atom, ElixirSenseExample.ModuleWithTypedStruct},
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
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :v,
                     type: {:atom, ElixirSenseExample.ModuleWithTypedStruct}
                   }
                 ]),
                 {:struct, [], {:variable, :v, 1}, nil}
               )
    end

    test "metadata struct" do
      assert {:struct, [__struct__: {:atom, MyMod}, abc: nil], {:atom, MyMod}, nil} ==
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

    test "integer" do
      assert {:integer, 77} == Binding.expand(@env, {:integer, 77})
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
                 @env
                 |> Map.put(:vars, [%VarInfo{version: 1, name: :v, type: {:atom, :abc}}]),
                 {:variable, :v, 1}
               )
    end

    test "known variable any version chooses max" do
      assert {:atom, :abc} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :v, type: {:atom, :foo}},
                   %VarInfo{version: 3, name: :v, type: {:atom, :abc}},
                   %VarInfo{version: 2, name: :v, type: {:atom, :bar}}
                 ]),
                 {:variable, :v, :any}
               )
    end

    test "known variable self referencing" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [%VarInfo{version: 1, name: :v, type: {:variable, :v, 1}}]),
                 {:variable, :v, 1}
               )
    end

    test "unknown variable" do
      assert :none == Binding.expand(@env, {:variable, :v, 1})

      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [%VarInfo{version: 1, name: :v, type: {:integer, 1}}]),
                 {:variable, :v, 2}
               )
    end

    test "known attribute" do
      assert {:atom, :abc} ==
               Binding.expand(
                 @env |> Map.put(:attributes, [%AttributeInfo{name: :v, type: {:atom, :abc}}]),
                 {:attribute, :v}
               )
    end

    test "known attribute self referencing" do
      assert nil ==
               Binding.expand(
                 @env |> Map.put(:attributes, [%AttributeInfo{name: :v, type: {:attribute, :v}}]),
                 {:attribute, :v}
               )
    end

    test "unknown attribute" do
      assert :none == Binding.expand(@env, {:attribute, :v})
    end

    test "tuple" do
      assert {:tuple, 2, [nil, {:atom, :abc}]} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :tuple,
                     type: {:tuple, 2, [nil, {:variable, :a, 1}]}
                   },
                   %VarInfo{version: 1, name: :a, type: {:atom, :abc}}
                 ]),
                 {:variable, :tuple, 1}
               )
    end

    test "tuple nth" do
      assert {:atom, :a} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :tuple, type: {:tuple, 2, [nil, {:atom, :a}]}},
                   %VarInfo{version: 1, name: :ref, type: {:tuple_nth, {:variable, :tuple, 1}, 1}}
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "list" do
      assert {:list, {:atom, :abc}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :list, type: {:list, {:variable, :a, 1}}},
                   %VarInfo{version: 1, name: :a, type: {:atom, :abc}}
                 ]),
                 {:variable, :list, 1}
               )
    end

    test "map key" do
      assert {:atom, :abc} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :map,
                     type: {:map_key, {:variable, :a, 1}, {:atom, :x}}
                   },
                   %VarInfo{version: 1, name: :a, type: {:map, [x: {:atom, :abc}], nil}}
                 ]),
                 {:variable, :map, 1}
               )

      assert {:atom, :abc} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :struct,
                     type: {:map_key, {:variable, :a, 1}, {:atom, :typed_field}}
                   },
                   %VarInfo{
                     version: 1,
                     name: :a,
                     type:
                       {:struct, [typed_field: {:atom, :abc}],
                        {:atom, ElixirSenseExample.ModuleWithTypedStruct}, nil}
                   }
                 ]),
                 {:variable, :struct, 1}
               )

      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :map,
                     type: {:map_key, {:variable, :a, 1}, {:atom, :y}}
                   },
                   %VarInfo{version: 1, name: :a, type: {:map, [x: {:atom, :abc}], nil}}
                 ]),
                 {:variable, :map, 1}
               )

      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :map, type: {:map_key, {:variable, :a, 1}, nil}},
                   %VarInfo{version: 1, name: :a, type: {:map, [x: {:atom, :abc}], nil}}
                 ]),
                 {:variable, :map, 1}
               )
    end

    test "for expression" do
      assert {:atom, :abc} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :list, type: {:for_expression, {:variable, :a, 1}}},
                   %VarInfo{version: 1, name: :a, type: {:list, {:atom, :abc}}}
                 ]),
                 {:variable, :list, 1}
               )

      assert {:tuple, 2, [nil, {:atom, :abc}]} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :map, type: {:for_expression, {:variable, :a, 1}}},
                   %VarInfo{version: 1, name: :a, type: {:map, [x: {:atom, :abc}], nil}}
                 ]),
                 {:variable, :map, 1}
               )

      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :list, type: {:list_head, {:variable, :a, 1}}},
                   %VarInfo{version: 1, name: :a, type: {:list, :empty}}
                 ]),
                 {:variable, :list, 1}
               )
    end

    test "list head" do
      assert {:atom, :abc} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :list, type: {:list_head, {:variable, :a, 1}}},
                   %VarInfo{version: 1, name: :a, type: {:list, {:atom, :abc}}}
                 ]),
                 {:variable, :list, 1}
               )

      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :list, type: {:list_head, {:variable, :a, 1}}},
                   %VarInfo{version: 1, name: :a, type: {:list, :empty}}
                 ]),
                 {:variable, :list, 1}
               )
    end

    test "list tail" do
      assert {:list, {:atom, :abc}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :list, type: {:list_tail, {:variable, :a, 1}}},
                   %VarInfo{version: 1, name: :a, type: {:list, {:atom, :abc}}}
                 ]),
                 {:variable, :list, 1}
               )

      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :list, type: {:list_tail, {:variable, :a, 1}}},
                   %VarInfo{version: 1, name: :a, type: {:list, :empty}}
                 ]),
                 {:variable, :list, 1}
               )
    end

    test "call existing map field access" do
      assert {:atom, :a} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :map, type: {:map, [field: {:atom, :a}], nil}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:variable, :map, 1}, :field, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "call existing map field access invalid arity" do
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :map, type: {:map, [field: {:atom, :a}], nil}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:variable, :map, 1}, :field, [nil]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "call not existing map field access" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :map, type: {:map, [field: {:atom, :a}], nil}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:variable, :map, 1}, :not_existing, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "call existing struct field access" do
      assert {:atom, :abc} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :map,
                     type:
                       {:struct, [typed_field: {:atom, :abc}],
                        {:atom, ElixirSenseExample.ModuleWithTypedStruct}, nil}
                   },
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:variable, :map, 1}, :typed_field, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "call not existing struct field access" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :map,
                     type:
                       {:struct, [typed_field: {:atom, :abc}],
                        {:atom, ElixirSenseExample.ModuleWithTypedStruct}, nil}
                   },
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:variable, :map, 1}, :not_existing, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "call existing struct field access invalid arity" do
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :map,
                     type:
                       {:struct, [typed_field: {:atom, :abc}],
                        {:atom, ElixirSenseExample.ModuleWithTypedStruct}, nil}
                   },
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:variable, :map, 1}, :typed_field, [nil]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "call on nil, true, false, none" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :ref, type: {:call, nil, :not_existing, []}}
                 ]),
                 {:variable, :ref, 1}
               )

      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :ref, type: {:call, :none, :not_existing, []}}
                 ]),
                 {:variable, :ref, 1}
               )

      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, nil}, :not_existing, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, true}, :not_existing, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call macro" do
      # required
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.SameModuleWithSecMacro},
                        :some_test_macro, []}
                   }
                 ])
                 |> Map.put(:requires, [ElixirSenseExample.SameModuleWithSecMacro]),
                 {:variable, :ref, 1}
               )

      # not required
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.SameModuleWithSecMacro},
                        :some_test_macro, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call not existing fun" do
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :not_existing,
                        []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call existing fun invalid arity" do
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f1, [nil]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call existing none arg" do
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f1x, [:none]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with spec t undefined" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f01, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with spec t expanding to atom" do
      assert {:atom, :asd} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f02, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with spec t expanding to tuple" do
      assert {:tuple, 2, [atom: :ok, atom: :abc]} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f04, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with spec t expanding to list" do
      assert {:list, :empty} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list1, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:list, nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list2, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:list, nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list3, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:list, {:atom, :ok}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list4, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:list, {:atom, :ok}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list5, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:list, {:atom, :ok}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list6, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:list, {:atom, :ok}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list7, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:list, {:atom, :ok}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list8, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:list, {:atom, :ok}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list9, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:list, {:atom, :ok}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list10, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:list, {:tuple, 2, [nil, nil]}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list11, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:list, {:tuple, 2, [nil, {:atom, :ok}]}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list12, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:list, {:tuple, 2, [{:atom, :some}, {:atom, :ok}]}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list13, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with spec t expanding to number" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f03, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with spec t expanding to integer" do
      assert {:integer, 44} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f05, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with spec local t expanding to struct" do
      assert {:struct,
              [
                __struct__: {:atom, ElixirSenseExample.FunctionsWithReturnSpec},
                abc: {:map, [key: {:atom, nil}], nil}
              ], {:atom, ElixirSenseExample.FunctionsWithReturnSpec},
              nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f1, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with spec remote t expanding to struct" do
      assert {:struct,
              [__struct__: {:atom, ElixirSenseExample.FunctionsWithReturnSpec.Remote}, abc: nil],
              {:atom, ElixirSenseExample.FunctionsWithReturnSpec.Remote},
              nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f3, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with spec struct" do
      assert {:struct,
              [__struct__: {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, abc: nil],
              {:atom, ElixirSenseExample.FunctionsWithReturnSpec},
              nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f5, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with spec local t expanding to map" do
      assert {:map, [{:abc, {:atom, :asd}}, {:cde, {:atom, :asd}}], nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f2, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with spec remote t expanding to map" do
      assert {:map, [abc: nil], nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f4, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with spec map" do
      assert {:map, [abc: nil], nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f6, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with spec intersection different returns" do
      assert {:union, [{:map, [abc: {:atom, String}], nil}, {:atom, nil}]} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f7, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with spec intersection different returns nested" do
      assert {:atom, String} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call,
                        {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f7, []},
                        :abc, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with spec intersection same returns" do
      assert {:map, [abc: nil], nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f71, [nil]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with spec union" do
      assert {:union, [{:map, [abc: nil], nil}, {:atom, nil}]} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f8, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with no_return" do
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f_no_return,
                        []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with any" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f_any, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f_term, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with spec parametrized map" do
      assert {:map, [abc: nil], nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f91, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "local call metadata fun returning struct" do
      assert {:struct, [{:__struct__, {:atom, MyMod}}, {:abc, nil}], {:atom, MyMod}, nil} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{version: 1, name: :ref, type: {:local_call, :fun, {1, 1}, []}}
                   ],
                   module: MyMod,
                   function: {:some, 0},
                   specs: %{
                     {MyMod, :fun, 0} => %SpecInfo{
                       specs: ["@spec fun() :: %MyMod{}"]
                     }
                   },
                   mods_funs_to_positions: %{
                     {MyMod, :fun, 0} => %ModFunInfo{
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
                 {:variable, :ref, 1}
               )

      assert :none ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{version: 1, name: :ref, type: {:local_call, :fun, {1, 1}, []}}
                   ],
                   module: MyMod,
                   function: nil,
                   specs: %{
                     {MyMod, :fun, 0} => %SpecInfo{
                       specs: ["@spec fun() :: %MyMod{}"]
                     }
                   },
                   mods_funs_to_positions: %{
                     {MyMod, :fun, 0} => %ModFunInfo{
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
                 {:variable, :ref, 1}
               )
    end

    test "local call metadata macro returning struct" do
      # before definition
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{version: 1, name: :ref, type: {:local_call, :fun, {1, 1}, []}}
                   ],
                   module: MyMod,
                   function: {:some, 0},
                   specs: %{
                     {MyMod, :fun, 0} => %SpecInfo{
                       specs: ["@spec fun() :: Macro.t()"]
                     }
                   },
                   mods_funs_to_positions: %{
                     {MyMod, :fun, 0} => %ModFunInfo{
                       params: [[]],
                       positions: [{10, 1}],
                       type: :defmacrop
                     }
                   },
                   structs: %{
                     MyMod => %StructInfo{
                       fields: [abc: nil, __struct__: MyMod]
                     }
                   }
                 }),
                 {:variable, :ref, 1}
               )

      # after definition - we do not return Macro as AST return type is generally dynamic
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{version: 1, name: :ref, type: {:local_call, :fun, {20, 1}, []}}
                   ],
                   module: MyMod,
                   function: {:some, 0},
                   specs: %{
                     {MyMod, :fun, 0} => %SpecInfo{
                       specs: ["@spec fun() :: Macro.t()"]
                     }
                   },
                   mods_funs_to_positions: %{
                     {MyMod, :fun, 0} => %ModFunInfo{
                       params: [[]],
                       positions: [{10, 1}],
                       type: :defmacrop
                     }
                   },
                   structs: %{
                     MyMod => %StructInfo{
                       fields: [abc: nil, __struct__: MyMod]
                     }
                   }
                 }),
                 {:variable, :ref, 1}
               )

      # in module body
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{version: 1, name: :ref, type: {:local_call, :fun, {20, 1}, []}}
                   ],
                   module: MyMod,
                   function: nil,
                   specs: %{
                     {MyMod, :fun, 0} => %SpecInfo{
                       specs: ["@spec fun() :: %MyMod{}"]
                     }
                   },
                   mods_funs_to_positions: %{
                     {MyMod, :fun, 0} => %ModFunInfo{
                       params: [[]],
                       positions: [{10, 1}],
                       type: :defp
                     }
                   },
                   structs: %{
                     MyMod => %StructInfo{
                       fields: [abc: nil, __struct__: MyMod]
                     }
                   }
                 }),
                 {:variable, :ref, 1}
               )
    end

    test "local call metadata fun returning local type expanding to struct" do
      assert {:struct, [{:__struct__, {:atom, MyMod}}, {:abc, nil}], {:atom, MyMod}, nil} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{version: 1, name: :ref, type: {:local_call, :fun, {1, 1}, []}}
                   ],
                   module: MyMod,
                   function: {:some, 0},
                   specs: %{
                     {MyMod, :fun, 0} => %SpecInfo{
                       specs: ["@spec fun() :: t()"]
                     }
                   },
                   mods_funs_to_positions: %{
                     {MyMod, :fun, 0} => %ModFunInfo{
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
                     {MyMod, :t, 0} => %TypeInfo{
                       specs: ["@type t() :: %MyMod{}"]
                     }
                   }
                 }),
                 {:variable, :ref, 1}
               )
    end

    test "local call metadata fun returning local type expanding to private type" do
      assert {:struct, [{:__struct__, {:atom, MyMod}}, {:abc, nil}], {:atom, MyMod}, nil} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{version: 1, name: :ref, type: {:local_call, :fun, {1, 1}, []}}
                   ],
                   module: MyMod,
                   function: {:some, 0},
                   specs: %{
                     {MyMod, :fun, 0} => %SpecInfo{
                       specs: ["@spec fun() :: t()"]
                     }
                   },
                   mods_funs_to_positions: %{
                     {MyMod, :fun, 0} => %ModFunInfo{
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
                     {MyMod, :t, 0} => %TypeInfo{
                       kind: :typep,
                       specs: ["@type t() :: %MyMod{}"]
                     }
                   }
                 }),
                 {:variable, :ref, 1}
               )
    end

    test "remote call metadata public fun returning local type expanding to struct" do
      assert {:struct, [{:__struct__, {:atom, MyMod}}, {:abc, nil}], {:atom, MyMod}, nil} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{version: 1, name: :ref, type: {:call, {:atom, MyMod}, :fun, []}}
                   ],
                   module: SomeMod,
                   specs: %{
                     {MyMod, :fun, 0} => %SpecInfo{
                       specs: ["@spec fun() :: t()"]
                     }
                   },
                   mods_funs_to_positions: %{
                     {MyMod, :fun, 0} => %ModFunInfo{
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
                     {MyMod, :t, 0} => %TypeInfo{
                       specs: ["@type t() :: %MyMod{}"]
                     }
                   }
                 }),
                 {:variable, :ref, 1}
               )
    end

    test "remote call metadata public macro" do
      # required
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{version: 1, name: :ref, type: {:call, {:atom, MyMod}, :fun, []}}
                   ],
                   module: SomeMod,
                   requires: [MyMod],
                   specs: %{
                     {MyMod, :fun, 0} => %SpecInfo{
                       specs: ["@spec fun() :: Macro.t()"]
                     }
                   },
                   mods_funs_to_positions: %{
                     {MyMod, :fun, 0} => %ModFunInfo{
                       params: [[]],
                       type: :defmacro
                     }
                   }
                 }),
                 {:variable, :ref, 1}
               )

      # not required
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{version: 1, name: :ref, type: {:call, {:atom, MyMod}, :fun, []}}
                   ],
                   module: SomeMod,
                   requires: [],
                   specs: %{
                     {MyMod, :fun, 0} => %SpecInfo{
                       specs: ["@spec fun() :: Macro.t()"]
                     }
                   },
                   mods_funs_to_positions: %{
                     {MyMod, :fun, 0} => %ModFunInfo{
                       params: [[]],
                       type: :defmacro
                     }
                   }
                 }),
                 {:variable, :ref, 1}
               )
    end

    test "remote call metadata public fun returning local type expanding to opaque" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{version: 1, name: :ref, type: {:call, {:atom, MyMod}, :fun, []}}
                   ],
                   module: SomeMod,
                   specs: %{
                     {MyMod, :fun, 0} => %SpecInfo{
                       specs: ["@spec fun() :: t()"]
                     }
                   },
                   mods_funs_to_positions: %{
                     {MyMod, :fun, 0} => %ModFunInfo{
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
                     {MyMod, :t, 0} => %TypeInfo{
                       kind: :opaque,
                       specs: ["@type t() :: %MyMod{}"]
                     }
                   }
                 }),
                 {:variable, :ref, 1}
               )
    end

    test "remote call metadata private fun returning local type expanding to struct" do
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{version: 1, name: :ref, type: {:call, {:atom, MyMod}, :fun, []}}
                   ],
                   module: SomeMod,
                   specs: %{
                     {MyMod, :fun, 0} => %SpecInfo{
                       specs: ["@spec fun() :: t()"]
                     }
                   },
                   mods_funs_to_positions: %{
                     {MyMod, :fun, 0} => %ModFunInfo{
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
                     {MyMod, :t, 0} => %TypeInfo{
                       specs: ["@type t() :: %MyMod{}"]
                     }
                   }
                 }),
                 {:variable, :ref, 1}
               )
    end

    test "local call metadata fun with default args returning struct" do
      env =
        @env
        |> Map.merge(%{
          module: MyMod,
          function: {:some, 0},
          specs: %{
            {MyMod, :fun, 3} => %SpecInfo{
              specs: ["@spec fun(integer(), integer(), any()) :: %MyMod{}"]
            }
          },
          mods_funs_to_positions: %{
            {MyMod, :fun, 3} => %ModFunInfo{
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

      assert :none ==
               Binding.expand(
                 env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :ref, type: {:local_call, :fun, {1, 1}, []}}
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:struct, [{:__struct__, {:atom, MyMod}}, {:abc, nil}], {:atom, MyMod}, nil} ==
               Binding.expand(
                 env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :ref, type: {:local_call, :fun, {1, 1}, [nil]}}
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:struct, [{:__struct__, {:atom, MyMod}}, {:abc, nil}], {:atom, MyMod}, nil} ==
               Binding.expand(
                 env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :ref, type: {:local_call, :fun, {1, 1}, [nil, nil]}}
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:struct, [{:__struct__, {:atom, MyMod}}, {:abc, nil}], {:atom, MyMod}, nil} ==
               Binding.expand(
                 env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:local_call, :fun, {1, 1}, [nil, nil, nil]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert :none ==
               Binding.expand(
                 env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:local_call, :fun, {1, 1}, [nil, nil, nil, nil]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "remote call fun with default args" do
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f10, []}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:atom, String} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f10, [nil]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:atom, String} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f10,
                        [nil, nil]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:atom, String} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f10,
                        [nil, nil, nil]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f10,
                        [nil, nil, nil, nil]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "local call imported fun with spec t expanding to atom" do
      assert {:atom, :asd} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{version: 1, name: :ref, type: {:local_call, :f02, {1, 1}, []}}
                   ],
                   functions: [{ElixirSenseExample.FunctionsWithReturnSpec, [{:f02, 0}]}]
                 }),
                 {:variable, :ref, 1}
               )
    end

    test "local call not imported" do
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{version: 1, name: :ref, type: {:local_call, :f02, {1, 1}, []}}
                   ]
                 }),
                 {:variable, :ref, 1}
               )
    end

    test "local call no parens imported fun with spec t expanding to atom" do
      assert {:atom, :asd} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{version: 1, name: :ref, type: {:variable, :f02, 1}}
                   ],
                   functions: [{ElixirSenseExample.FunctionsWithReturnSpec, [{:f02, 0}]}]
                 }),
                 {:variable, :ref, 1}
               )
    end

    test "extract struct key type from typespec" do
      assert {:map, [key: {:atom, nil}], nil} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{
                       version: 1,
                       name: :ref,
                       type:
                         {:call,
                          {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f1, []},
                          :abc, []}
                     }
                   ]
                 }),
                 {:variable, :ref, 1}
               )
    end

    test "extract required map key type from typespec" do
      assert {:atom, :asd} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{
                       version: 1,
                       name: :ref,
                       type:
                         {:call,
                          {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f2, []},
                          :abc, []}
                     }
                   ]
                 }),
                 {:variable, :ref, 1}
               )
    end

    test "optimistically extract optional map key type from typespec" do
      assert {:atom, :asd} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{
                       version: 1,
                       name: :ref,
                       type:
                         {:call,
                          {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f2, []},
                          :cde, []}
                     }
                   ]
                 }),
                 {:variable, :ref, 1}
               )
    end

    test "bonds on tuple element" do
      assert {:atom, :some} =
               Binding.expand(
                 @env,
                 {:tuple_nth,
                  {:intersection,
                   [
                     {:tuple, 2, [{:atom, :ok}, nil]},
                     {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f11, []}
                   ]}, 1}
               )

      assert {:atom, :some_error} =
               Binding.expand(
                 @env,
                 {:tuple_nth,
                  {:intersection,
                   [
                     {:tuple, 2, [{:atom, :error}, nil]},
                     {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f11, []}
                   ]}, 1}
               )
    end
  end

  describe "Kernel functions" do
    test "++" do
      assert {:list, {:integer, 1}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :a, type: {:integer, 1}},
                   %VarInfo{version: 1, name: :b, type: {:integer, 2}}
                 ]),
                 {:local_call, :++, {1, 1}, [list: {:variable, :a, 1}, list: {:variable, :b, 1}]}
               )

      assert {:list, {:integer, 1}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :a, type: {:integer, 1}},
                   %VarInfo{version: 1, name: :b, type: {:integer, 2}}
                 ]),
                 {:call, {:atom, :erlang}, :++,
                  [list: {:variable, :a, 1}, list: {:variable, :b, 1}]}
               )
    end

    test "tuple elem" do
      assert {:atom, :a} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :tuple, type: {:tuple, 2, [nil, {:atom, :a}]}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:local_call, :elem, {1, 1}, [{:variable, :tuple, 1}, {:integer, 1}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:atom, :a} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :tuple, type: {:tuple, 2, [nil, {:atom, :a}]}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, :erlang}, :element,
                        [{:integer, 2}, {:variable, :tuple, 1}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "tuple put_elem" do
      assert {:tuple, 2, [{:atom, :b}, {:atom, :a}]} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :tuple, type: {:tuple, 2, [nil, {:atom, :a}]}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:local_call, :put_elem, {1, 1},
                        [{:variable, :tuple, 1}, {:integer, 0}, {:atom, :b}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:tuple, 2, [{:atom, :b}, {:atom, :a}]} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :tuple, type: {:tuple, 2, [nil, {:atom, :a}]}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, :erlang}, :setelement,
                        [{:integer, 1}, {:variable, :tuple, 1}, {:atom, :b}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "tuple_size" do
      assert {:integer, 2} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :tuple, type: {:tuple, 2, [nil, {:atom, :a}]}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:local_call, :tuple_size, {1, 1}, [{:variable, :tuple, 1}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:integer, 2} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :tuple, type: {:tuple, 2, [nil, {:atom, :a}]}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, :erlang}, :tuple_size, [{:variable, :tuple, 1}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "list hd" do
      assert {:atom, :a} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :list, type: {:list, {:atom, :a}}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:local_call, :hd, {1, 1}, [{:variable, :list, 1}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:atom, :a} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :list, type: {:list, {:atom, :a}}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, :erlang}, :hd, [{:variable, :list, 1}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "list tl" do
      assert {:list, {:atom, :a}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :list, type: {:list, {:atom, :a}}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:local_call, :tl, {1, 1}, [{:variable, :list, 1}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:list, {:atom, :a}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :list, type: {:list, {:atom, :a}}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, :erlang}, :tl, [{:variable, :list, 1}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end
  end

  describe "Tuple functions" do
    test "append" do
      assert {:tuple, 3, [nil, {:atom, :a}, {:atom, :b}]} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :tuple, type: {:tuple, 2, [nil, {:atom, :a}]}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, Tuple}, :append, [{:variable, :tuple, 1}, {:atom, :b}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:tuple, 3, [nil, {:atom, :a}, {:atom, :b}]} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :tuple, type: {:tuple, 2, [nil, {:atom, :a}]}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, :erlang}, :append_element,
                        [{:variable, :tuple, 1}, {:atom, :b}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "delete_at" do
      assert {:tuple, 1, [{:atom, :a}]} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :tuple, type: {:tuple, 2, [nil, {:atom, :a}]}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, Tuple}, :delete_at,
                        [{:variable, :tuple, 1}, {:integer, 0}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:tuple, 1, [{:atom, :a}]} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :tuple, type: {:tuple, 2, [nil, {:atom, :a}]}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, :erlang}, :delete_element,
                        [{:integer, 1}, {:variable, :tuple, 1}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "insert_at" do
      assert {:tuple, 3, [{:atom, :b}, nil, {:atom, :a}]} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :tuple, type: {:tuple, 2, [nil, {:atom, :a}]}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, Tuple}, :insert_at,
                        [{:variable, :tuple, 1}, {:integer, 0}, {:atom, :b}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:tuple, 3, [{:atom, :b}, nil, {:atom, :a}]} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :tuple, type: {:tuple, 2, [nil, {:atom, :a}]}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, :erlang}, :insert_element,
                        [{:integer, 1}, {:variable, :tuple, 1}, {:atom, :b}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "to_list" do
      assert {:list, {:atom, :a}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :tuple, type: {:tuple, 1, [{:atom, :a}]}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, Tuple}, :to_list, [{:variable, :tuple, 1}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:list, :empty} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :tuple, type: {:tuple, 0, []}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, Tuple}, :to_list, [{:variable, :tuple, 1}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:list, {:atom, :a}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :tuple, type: {:tuple, 1, [{:atom, :a}]}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type: {:call, {:atom, :erlang}, :tuple_to_list, [{:variable, :tuple, 1}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end

    test "duplicate" do
      assert {:tuple, 2, [{:atom, :a}, {:atom, :a}]} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :tuple, type: {:atom, :a}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, Tuple}, :duplicate,
                        [{:variable, :tuple, 1}, {:integer, 2}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )

      assert {:tuple, 2, [{:atom, :a}, {:atom, :a}]} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :tuple, type: {:atom, :a}},
                   %VarInfo{
                     version: 1,
                     name: :ref,
                     type:
                       {:call, {:atom, :erlang}, :make_tuple,
                        [{:integer, 2}, {:variable, :tuple, 1}]}
                   }
                 ]),
                 {:variable, :ref, 1}
               )
    end
  end

  describe "Map functions" do
    test "put" do
      assert {:map, [cde: {:atom, :b}, abc: {:atom, :a}], nil} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :put,
                  [{:map, [abc: {:atom, :a}], nil}, {:atom, :cde}, {:atom, :b}]}
               )

      assert {:map, [cde: {:atom, :b}, abc: {:atom, :a}], nil} =
               Binding.expand(
                 @env,
                 {:call, {:atom, :maps}, :put,
                  [{:atom, :cde}, {:atom, :b}, {:map, [abc: {:atom, :a}], nil}]}
               )
    end

    test "put not a map" do
      assert {:map, [cde: {:atom, :b}], nil} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :put, [nil, {:atom, :cde}, {:atom, :b}]}
               )
    end

    test "put not an atom key" do
      assert {:map, [], nil} =
               Binding.expand(@env, {:call, {:atom, Map}, :put, [nil, nil, {:atom, :b}]})
    end

    test "delete" do
      assert {:map, [abc: {:atom, :a}], nil} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :delete,
                  [{:map, [abc: {:atom, :a}, cde: nil], nil}, {:atom, :cde}]}
               )

      assert {:map, [abc: {:atom, :a}], nil} =
               Binding.expand(
                 @env,
                 {:call, {:atom, :maps}, :remove,
                  [{:atom, :cde}, {:map, [abc: {:atom, :a}, cde: nil], nil}]}
               )
    end

    test "merge" do
      assert {:map, [abc: {:atom, :a}, cde: {:atom, :b}], nil} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :merge,
                  [{:map, [abc: {:atom, :a}], nil}, {:map, [cde: {:atom, :b}], nil}]}
               )

      assert {:map, [abc: {:atom, :a}, cde: {:atom, :b}], nil} =
               Binding.expand(
                 @env,
                 {:call, {:atom, :maps}, :merge,
                  [{:map, [abc: {:atom, :a}], nil}, {:map, [cde: {:atom, :b}], nil}]}
               )
    end

    test "merge/3" do
      assert {:map, [abc: {:atom, :a}, cde: {:atom, :b}], nil} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :merge,
                  [{:map, [abc: {:atom, :a}], nil}, {:map, [cde: {:atom, :b}], nil}, nil]}
               )
    end

    test "from_struct" do
      assert {:map, [other: nil, typed_field: nil], nil} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :from_struct,
                  [{:struct, [], {:atom, ElixirSenseExample.ModuleWithTypedStruct}, nil}]}
               )
    end

    test "from_struct atom arg" do
      assert {:map, [other: nil, typed_field: nil], nil} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :from_struct,
                  [{:atom, ElixirSenseExample.ModuleWithTypedStruct}]}
               )
    end

    test "update" do
      assert {:map, [abc: nil], nil} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :update,
                  [{:map, [abc: {:atom, :a}], nil}, {:atom, :abc}, nil, {:atom, :b}]}
               )
    end

    test "update!" do
      assert {:map, [abc: nil], nil} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :update!,
                  [{:map, [abc: {:atom, :a}], nil}, {:atom, :abc}, {:atom, :b}]}
               )
    end

    test "replace!" do
      assert {:map, [abc: {:atom, :b}], nil} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :replace!,
                  [{:map, [abc: {:atom, :a}], nil}, {:atom, :abc}, {:atom, :b}]}
               )

      assert {:map, [abc: {:atom, :b}], nil} =
               Binding.expand(
                 @env,
                 {:call, {:atom, :maps}, :update,
                  [{:atom, :abc}, {:atom, :b}, {:map, [abc: {:atom, :a}], nil}]}
               )
    end

    test "put_new" do
      assert {:map, [cde: {:atom, :b}, abc: {:atom, :a}], nil} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :put_new,
                  [{:map, [abc: {:atom, :a}], nil}, {:atom, :cde}, {:atom, :b}]}
               )
    end

    test "put_new_lazy" do
      assert {:map, [cde: nil, abc: {:atom, :a}], nil} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :put_new_lazy,
                  [{:map, [abc: {:atom, :a}], nil}, {:atom, :cde}, {:atom, :b}]}
               )
    end

    test "fetch!" do
      assert {:atom, :a} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :fetch!, [{:map, [abc: {:atom, :a}], nil}, {:atom, :abc}]}
               )

      assert {:atom, :a} =
               Binding.expand(
                 @env,
                 {:call, {:atom, :maps}, :get, [{:atom, :abc}, {:map, [abc: {:atom, :a}], nil}]}
               )
    end

    test "fetch" do
      assert {:tuple, 2, [atom: :ok, atom: :a]} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :fetch, [{:map, [abc: {:atom, :a}], nil}, {:atom, :abc}]}
               )

      assert {:tuple, 2, [atom: :ok, atom: :a]} =
               Binding.expand(
                 @env,
                 {:call, {:atom, :maps}, :find, [{:atom, :abc}, {:map, [abc: {:atom, :a}], nil}]}
               )
    end

    test "get" do
      assert {:atom, :a} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :get, [{:map, [abc: {:atom, :a}], nil}, {:atom, :abc}]}
               )
    end

    test "get default" do
      assert {:atom, :x} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :get,
                  [{:map, [abc: {:atom, :a}], nil}, {:atom, :cde}, {:atom, :x}]}
               )
    end

    test "get_lazy" do
      assert nil ==
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :get_lazy,
                  [{:map, [abc: {:atom, :a}], nil}, {:atom, :cde}, {:atom, :x}]}
               )
    end

    test "new" do
      assert {:map, [], nil} = Binding.expand(@env, {:call, {:atom, Map}, :new, []})
    end
  end

  describe "Enum functions" do
    test "at" do
      assert {:atom, :a} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Enum}, :at, [{:list, {:atom, :a}}, nil]}
               )

      assert {:atom, :a} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Enum}, :at, [{:list, {:atom, :a}}, nil, nil]}
               )
    end

    test "fetch" do
      assert {:tuple, 2, [{:atom, :ok}, {:atom, :a}]} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Enum}, :fetch, [{:list, {:atom, :a}}, nil]}
               )
    end

    test "filter" do
      assert {:list, {:atom, :a}} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Enum}, :filter, [{:list, {:atom, :a}}, nil]}
               )
    end

    test "concat" do
      assert {:list, {:atom, :a}} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Enum}, :concat, [{:list, {:list, {:atom, :a}}}]}
               )
    end

    test "split" do
      assert {:tuple, 2, [list: {:atom, :a}, list: {:atom, :a}]} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Enum}, :split, [{:list, {:atom, :a}}, nil]}
               )
    end

    test "min_max" do
      assert {:tuple, 2, [atom: :a, atom: :a]} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Enum}, :min_max, [{:list, {:atom, :a}}, nil]}
               )
    end

    test "chunk_by" do
      assert {:list, {:list, {:atom, :a}}} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Enum}, :chunk_by, [{:list, {:atom, :a}}, nil]}
               )
    end
  end

  describe "List functions" do
    test "first" do
      assert {:atom, :a} =
               Binding.expand(
                 @env,
                 {:call, {:atom, List}, :first, [{:list, {:atom, :a}}, nil]}
               )
    end

    test "update_at" do
      assert {:list, {:atom, :a}} =
               Binding.expand(
                 @env,
                 {:call, {:atom, List}, :update_at, [{:list, {:atom, :a}}, nil]}
               )
    end

    test "wrap" do
      assert {:list, {:atom, :a}} =
               Binding.expand(
                 @env,
                 {:call, {:atom, List}, :wrap, [{:list, {:atom, :a}}]}
               )

      assert {:list, {:atom, :a}} =
               Binding.expand(
                 @env,
                 {:call, {:atom, List}, :wrap, [{:atom, :a}]}
               )

      assert {:list, :empty} =
               Binding.expand(
                 @env,
                 {:call, {:atom, List}, :wrap, [{:atom, nil}]}
               )
    end
  end

  describe "intersection" do
    test "intersection" do
      assert {:struct,
              [
                {:__struct__, {:atom, State}},
                {:abc, nil},
                {:formatted, {:variable, :formatted, 1}}
              ], {:atom, State},
              nil} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   structs: %{
                     State => %StructInfo{
                       fields: [abc: nil, formatted: nil, __struct__: State]
                     }
                   },
                   vars: [
                     %VarInfo{version: 1, name: :socket, type: nil}
                   ]
                 }),
                 {:intersection,
                  [
                    {:call, {:call, {:variable, :socket, 1}, :assigns, []}, :state, []},
                    {:struct, [formatted: {:variable, :formatted, 1}], {:atom, State}, nil}
                  ]}
               )
    end

    test "none intersection" do
      assert :none == Binding.expand(@env, {:intersection, [{:atom, A}, :none]})
      assert :none == Binding.expand(@env, {:intersection, [:none, {:atom, A}]})
      assert :none == Binding.expand(@env, {:intersection, [:none, :none]})
    end

    test "none union" do
      assert {:atom, A} == Binding.expand(@env, {:union, [{:atom, A}, :none]})
      assert {:atom, A} == Binding.expand(@env, {:union, [:none, {:atom, A}]})
      assert :none == Binding.expand(@env, {:union, [:none, :none]})
    end

    test "unknown intersection" do
      assert {:atom, A} == Binding.expand(@env, {:intersection, [{:atom, A}, nil]})
      assert {:atom, A} == Binding.expand(@env, {:intersection, [nil, {:atom, A}]})
      assert nil == Binding.expand(@env, {:intersection, [nil, nil]})
    end

    test "unknown union" do
      assert nil == Binding.expand(@env, {:union, [{:atom, A}, nil]})
      assert nil == Binding.expand(@env, {:union, [nil, {:atom, A}]})
      assert nil == Binding.expand(@env, {:union, [nil, nil]})
    end

    test "equal" do
      assert {:atom, A} == Binding.expand(@env, {:intersection, [{:atom, A}, {:atom, A}]})
      assert :none == Binding.expand(@env, {:intersection, [{:atom, A}, {:atom, B}]})

      assert {:atom, A} == Binding.expand(@env, {:union, [{:atom, A}, {:atom, A}]})

      assert {:union, [{:atom, A}, {:atom, B}]} ==
               Binding.expand(@env, {:union, [{:atom, A}, {:atom, B}]})
    end

    test "tuple" do
      assert {:tuple, 2, [{:atom, B}, {:atom, A}]} ==
               Binding.expand(
                 @env,
                 {:intersection, [{:tuple, 2, [nil, {:atom, A}]}, {:tuple, 2, [{:atom, B}, nil]}]}
               )

      assert :none ==
               Binding.expand(
                 @env,
                 {:intersection, [{:tuple, 2, [nil, {:atom, A}]}, {:tuple, 1, [{:atom, B}]}]}
               )
    end

    test "map" do
      assert {:map, [], nil} ==
               Binding.expand(@env, {:intersection, [{:map, [], nil}, {:map, [], nil}]})

      # NOTE intersection is not strict and does an union on map keys
      assert {:map, [{:a, nil}, {:b, {:tuple, 2, [atom: Z, atom: X]}}, {:c, {:atom, C}}], nil} ==
               Binding.expand(
                 @env,
                 {:intersection,
                  [
                    {:map, [a: nil, b: {:tuple, 2, [{:atom, Z}, nil]}], nil},
                    {:map, [c: {:atom, C}, b: {:tuple, 2, [nil, {:atom, X}]}], nil}
                  ]}
               )

      assert :none ==
               Binding.expand(
                 @env,
                 {:intersection,
                  [
                    {:map, [c: {:atom, A}, b: {:tuple, 2, [{:atom, Z}, nil]}], nil},
                    {:map, [c: {:atom, C}, b: {:tuple, 2, [nil, {:atom, X}]}], nil}
                  ]}
               )
    end

    test "struct and map" do
      assert {:struct,
              [
                {:__struct__, {:atom, State}},
                {:abc, {:atom, X}},
                {:formatted, {:variable, :formatted, 1}}
              ], {:atom, State},
              nil} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   structs: %{
                     State => %StructInfo{
                       fields: [abc: nil, formatted: nil, __struct__: State]
                     }
                   }
                 }),
                 {:intersection,
                  [
                    {:struct, [formatted: {:variable, :formatted, 1}], {:atom, State}, nil},
                    {:map, [not_existing: nil, abc: {:atom, X}], nil}
                  ]}
               )

      assert {:struct,
              [
                {:__struct__, {:atom, State}},
                {:abc, {:atom, X}},
                {:formatted, {:variable, :formatted, 1}}
              ], {:atom, State},
              nil} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   structs: %{
                     State => %StructInfo{
                       fields: [abc: nil, formatted: nil, __struct__: State]
                     }
                   }
                 }),
                 {:intersection,
                  [
                    {:map, [not_existing: nil, abc: {:atom, X}], nil},
                    {:struct, [formatted: {:variable, :formatted, 1}], {:atom, State}, nil}
                  ]}
               )
    end

    test "unknown struct and map" do
      assert {:struct,
              [
                {:__struct__, nil},
                {:formatted, {:variable, :formatted, 1}},
                {:not_existing, nil},
                {:abc, {:atom, X}}
              ], nil,
              nil} ==
               Binding.expand(
                 @env,
                 {:intersection,
                  [
                    {:struct, [formatted: {:variable, :formatted, 1}], nil, nil},
                    {:map, [not_existing: nil, abc: {:atom, X}], nil}
                  ]}
               )
    end

    test "unknown struct and unknown struct" do
      assert {:struct,
              [
                {:__struct__, nil},
                {:formatted, {:variable, :formatted, 1}},
                {:not_existing, nil},
                {:abc, {:atom, X}}
              ], nil,
              nil} ==
               Binding.expand(
                 @env,
                 {:intersection,
                  [
                    {:struct, [formatted: {:variable, :formatted, 1}], nil, nil},
                    {:struct, [not_existing: nil, abc: {:atom, X}], nil, nil}
                  ]}
               )
    end

    test "struct and unknown struct" do
      assert {:struct,
              [
                {:__struct__, {:atom, State}},
                {:abc, {:atom, X}},
                {:formatted, {:variable, :formatted, 1}}
              ], {:atom, State},
              nil} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   structs: %{
                     State => %StructInfo{
                       fields: [abc: nil, formatted: nil, __struct__: State]
                     }
                   }
                 }),
                 {:intersection,
                  [
                    {:struct, [formatted: {:variable, :formatted, 1}], {:atom, State}, nil},
                    {:struct, [not_existing: nil, abc: {:atom, X}], nil, nil}
                  ]}
               )

      assert {:struct,
              [
                {:__struct__, {:atom, State}},
                {:abc, {:atom, X}},
                {:formatted, {:variable, :formatted, 1}}
              ], {:atom, State},
              nil} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   structs: %{
                     State => %StructInfo{
                       fields: [abc: nil, formatted: nil, __struct__: State]
                     }
                   }
                 }),
                 {:intersection,
                  [
                    {:struct, [not_existing: nil, abc: {:atom, X}], nil, nil},
                    {:struct, [formatted: {:variable, :formatted, 1}], {:atom, State}, nil}
                  ]}
               )
    end

    test "struct" do
      assert {:struct,
              [
                {:__struct__, {:atom, State}},
                {:abc, {:atom, X}},
                {:formatted, {:variable, :formatted, 1}}
              ], {:atom, State},
              nil} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   structs: %{
                     State => %StructInfo{
                       fields: [abc: nil, formatted: nil, __struct__: State]
                     }
                   }
                 }),
                 {:intersection,
                  [
                    {:struct, [formatted: {:variable, :formatted, 1}], {:atom, State}, nil},
                    {:struct, [not_existing: nil, abc: {:atom, X}], {:atom, State}, nil}
                  ]}
               )

      assert :none ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   structs: %{
                     State => %StructInfo{
                       fields: [abc: nil, formatted: nil, __struct__: State]
                     },
                     Other => %StructInfo{
                       fields: [abc: nil, formatted: nil, __struct__: Other]
                     }
                   }
                 }),
                 {:intersection,
                  [
                    {:struct, [not_existing: nil, abc: {:atom, X}], {:atom, Other}, nil},
                    {:struct, [formatted: {:variable, :formatted, 1}], {:atom, State}, nil}
                  ]}
               )
    end
  end

  describe "from_var" do
    defmodule Elixir.BindingTest.Some do
      defstruct [:asd]
    end

    defp assert_is_stable(expansion, expected) do
      assert expansion == expected
      assert Binding.expand(@env, expansion) == expected
    end

    test "integer" do
      assert_is_stable(Binding.from_var(3), {:integer, 3})
    end

    test "atom" do
      assert_is_stable(Binding.from_var(:asd), {:atom, :asd})
      assert_is_stable(Binding.from_var(nil), {:atom, nil})
      assert_is_stable(Binding.from_var(true), {:atom, true})
      assert_is_stable(Binding.from_var(false), {:atom, false})
    end

    test "tuple" do
      assert_is_stable(Binding.from_var({:asd, 123}), {:tuple, 2, [atom: :asd, integer: 123]})
    end

    test "map" do
      assert_is_stable(Binding.from_var(%{asd: 123}), {:map, [asd: {:integer, 123}], nil})
    end

    test "struct" do
      assert_is_stable(
        Binding.from_var(%{__struct__: BindingTest.Some, asd: 123}),
        {:struct, [{:__struct__, {:atom, BindingTest.Some}}, {:asd, {:integer, 123}}],
         {:atom, BindingTest.Some}, nil}
      )
    end
  end
end
