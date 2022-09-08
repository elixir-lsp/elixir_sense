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
      assert :none ==
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
      assert :none ==
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
                 @env |> Map.put(:variables, [%VarInfo{name: :v, type: {:atom, :abc}}]),
                 {:variable, :v}
               )
    end

    test "known variable self referencing" do
      assert nil ==
               Binding.expand(
                 @env |> Map.put(:variables, [%VarInfo{name: :v, type: {:variable, :v}}]),
                 {:variable, :v}
               )
    end

    test "anonymous variable" do
      assert :none ==
               Binding.expand(
                 @env |> Map.put(:variables, [%VarInfo{name: :_, type: {:atom, :abc}}]),
                 {:variable, :_}
               )
    end

    test "unknown variable" do
      assert :none == Binding.expand(@env, {:variable, :v})
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
                 |> Map.put(:variables, [
                   %VarInfo{name: :tuple, type: {:tuple, 2, [nil, {:variable, :a}]}},
                   %VarInfo{name: :a, type: {:atom, :abc}}
                 ]),
                 {:variable, :tuple}
               )
    end

    test "tuple nth" do
      assert {:atom, :a} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{name: :tuple, type: {:tuple, 2, [nil, {:atom, :a}]}},
                   %VarInfo{name: :ref, type: {:tuple_nth, {:variable, :tuple}, 1}}
                 ]),
                 {:variable, :ref}
               )
    end

    test "tuple elem" do
      assert {:atom, :a} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{name: :tuple, type: {:tuple, 2, [nil, {:atom, :a}]}},
                   %VarInfo{
                     name: :ref,
                     type: {:local_call, :elem, [{:variable, :tuple}, {:integer, 1}]}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "list" do
      assert {:list, {:atom, :abc}} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{name: :list, type: {:list, {:variable, :a}}},
                   %VarInfo{name: :a, type: {:atom, :abc}}
                 ]),
                 {:variable, :list}
               )
    end

    test "list head" do
      assert {:atom, :abc} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{name: :list, type: {:list_head, {:variable, :a}}},
                   %VarInfo{name: :a, type: {:list, {:atom, :abc}}}
                 ]),
                 {:variable, :list}
               )

      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{name: :list, type: {:list_head, {:variable, :a}}},
                   %VarInfo{name: :a, type: {:list, :empty}}
                 ]),
                 {:variable, :list}
               )
    end

    test "list tail" do
      assert {:list, {:atom, :abc}} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{name: :list, type: {:list_tail, {:variable, :a}}},
                   %VarInfo{name: :a, type: {:list, {:atom, :abc}}}
                 ]),
                 {:variable, :list}
               )

      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{name: :list, type: {:list_tail, {:variable, :a}}},
                   %VarInfo{name: :a, type: {:list, :empty}}
                 ]),
                 {:variable, :list}
               )
    end

    test "call existing map field access" do
      assert {:atom, :a} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{name: :map, type: {:map, [field: {:atom, :a}], nil}},
                   %VarInfo{name: :ref, type: {:call, {:variable, :map}, :field, []}}
                 ]),
                 {:variable, :ref}
               )
    end

    test "call existing map field access invalid arity" do
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{name: :map, type: {:map, [field: {:atom, :a}], nil}},
                   %VarInfo{name: :ref, type: {:call, {:variable, :map}, :field, [nil]}}
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
                   %VarInfo{name: :ref, type: {:call, {:variable, :map}, :not_existing, []}}
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
                   %VarInfo{name: :ref, type: {:call, {:variable, :map}, :typed_field, []}}
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
                   %VarInfo{name: :ref, type: {:call, {:variable, :map}, :not_existing, []}}
                 ]),
                 {:variable, :ref}
               )
    end

    test "call existing struct field access invalid arity" do
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :map,
                     type:
                       {:struct, [typed_field: {:atom, :abc}],
                        {:atom, ElixirSenseExample.ModuleWithTypedStruct}, nil}
                   },
                   %VarInfo{name: :ref, type: {:call, {:variable, :map}, :typed_field, [nil]}}
                 ]),
                 {:variable, :ref}
               )
    end

    test "call on nil, true, false, none" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{name: :ref, type: {:call, nil, :not_existing, []}}
                 ]),
                 {:variable, :ref}
               )

      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{name: :ref, type: {:call, :none, :not_existing, []}}
                 ]),
                 {:variable, :ref}
               )

      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{name: :ref, type: {:call, {:atom, nil}, :not_existing, []}}
                 ]),
                 {:variable, :ref}
               )

      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{name: :ref, type: {:call, {:atom, true}, :not_existing, []}}
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call not existing fun" do
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :not_existing,
                        []}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call existing fun invalid arity" do
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f1, [nil]}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call existing none arg" do
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f1x, [:none]}
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
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f01, []}
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
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f02, []}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with spec t expanding to tuple" do
      assert {:tuple, 2, [atom: :ok, atom: :abc]} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f04, []}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with spec t expanding to list" do
      assert {:list, :empty} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list1, []}
                   }
                 ]),
                 {:variable, :ref}
               )

      assert {:list, nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list2, []}
                   }
                 ]),
                 {:variable, :ref}
               )

      assert {:list, nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list3, []}
                   }
                 ]),
                 {:variable, :ref}
               )

      assert {:list, {:atom, :ok}} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list4, []}
                   }
                 ]),
                 {:variable, :ref}
               )

      assert {:list, {:atom, :ok}} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list5, []}
                   }
                 ]),
                 {:variable, :ref}
               )

      assert {:list, {:atom, :ok}} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list6, []}
                   }
                 ]),
                 {:variable, :ref}
               )

      assert {:list, {:atom, :ok}} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list7, []}
                   }
                 ]),
                 {:variable, :ref}
               )

      assert {:list, {:atom, :ok}} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list8, []}
                   }
                 ]),
                 {:variable, :ref}
               )

      assert {:list, {:atom, :ok}} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list9, []}
                   }
                 ]),
                 {:variable, :ref}
               )

      assert {:list, {:atom, :ok}} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list10, []}
                   }
                 ]),
                 {:variable, :ref}
               )

      assert {:list, {:tuple, 2, [nil, nil]}} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list11, []}
                   }
                 ]),
                 {:variable, :ref}
               )

      assert {:list, {:tuple, 2, [nil, {:atom, :ok}]}} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list12, []}
                   }
                 ]),
                 {:variable, :ref}
               )

      assert {:list, {:tuple, 2, [{:atom, :some}, {:atom, :ok}]}} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :list13, []}
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
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f03, []}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with spec t expanding to integer" do
      assert {:integer, 44} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f05, []}
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
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f1, []}
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
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f3, []}
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
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f5, []}
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
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f2, []}
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
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f4, []}
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
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f6, []}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with spec intersection different returns" do
      assert {:union, [{:map, [abc: nil], nil}, {:atom, nil}]} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f7, []}
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
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f71, [nil]}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with spec union" do
      assert {:union, [{:map, [abc: nil], nil}, {:atom, nil}]} ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f8, []}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with no_return" do
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f_no_return,
                        []}
                   }
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with any" do
      assert nil ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f_any, []}
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
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f_term, []}
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
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f91, []}
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
                     %VarInfo{name: :ref, type: {:local_call, :fun, []}}
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
                     %VarInfo{name: :ref, type: {:local_call, :fun, []}}
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
                     %VarInfo{name: :ref, type: {:local_call, :fun, []}}
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
                     %VarInfo{name: :ref, type: {:call, {:atom, MyMod}, :fun, []}}
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
                     %VarInfo{name: :ref, type: {:call, {:atom, MyMod}, :fun, []}}
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
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   variables: [
                     %VarInfo{name: :ref, type: {:call, {:atom, MyMod}, :fun, []}}
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
                   %VarInfo{name: :ref, type: {:local_call, :fun, []}}
                 ]),
                 {:variable, :ref}
               )

      assert {:struct, [{:__struct__, {:atom, MyMod}}, {:abc, nil}], MyMod, nil} ==
               Binding.expand(
                 env
                 |> Map.put(:variables, [
                   %VarInfo{name: :ref, type: {:local_call, :fun, [nil]}}
                 ]),
                 {:variable, :ref}
               )

      assert {:struct, [{:__struct__, {:atom, MyMod}}, {:abc, nil}], MyMod, nil} ==
               Binding.expand(
                 env
                 |> Map.put(:variables, [
                   %VarInfo{name: :ref, type: {:local_call, :fun, [nil, nil]}}
                 ]),
                 {:variable, :ref}
               )

      assert {:struct, [{:__struct__, {:atom, MyMod}}, {:abc, nil}], MyMod, nil} ==
               Binding.expand(
                 env
                 |> Map.put(:variables, [
                   %VarInfo{name: :ref, type: {:local_call, :fun, [nil, nil, nil]}}
                 ]),
                 {:variable, :ref}
               )

      assert nil ==
               Binding.expand(
                 env
                 |> Map.put(:variables, [
                   %VarInfo{name: :ref, type: {:local_call, :fun, [nil, nil, nil, nil]}}
                 ]),
                 {:variable, :ref}
               )
    end

    test "remote call fun with default args" do
      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f10, []}
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
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f10, [nil]}
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
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f10,
                        [nil, nil]}
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
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f10,
                        [nil, nil, nil]}
                   }
                 ]),
                 {:variable, :ref}
               )

      assert :none ==
               Binding.expand(
                 @env
                 |> Map.put(:variables, [
                   %VarInfo{
                     name: :ref,
                     type:
                       {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f10,
                        [nil, nil, nil, nil]}
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
                     %VarInfo{name: :ref, type: {:local_call, :f02, []}}
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
                          {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f1, []},
                          :abc, []}
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
                          {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f2, []},
                          :abc, []}
                     }
                   ],
                   imports: [ElixirSenseExample.FunctionsWithReturnSpec]
                 }),
                 {:variable, :ref}
               )
    end

    test "optimistically extract optional map key type from typespec" do
      assert {:atom, :asd} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   variables: [
                     %VarInfo{
                       name: :ref,
                       type:
                         {:call,
                          {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f2, []},
                          :cde, []}
                     }
                   ],
                   imports: [ElixirSenseExample.FunctionsWithReturnSpec]
                 }),
                 {:variable, :ref}
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

  describe "Map functions" do
    test "put" do
      assert {:map, [cde: {:atom, :b}, abc: {:atom, :a}], nil} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :put,
                  [{:map, [abc: {:atom, :a}], nil}, {:atom, :cde}, {:atom, :b}]}
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
    end

    test "merge" do
      assert {:map, [abc: {:atom, :a}, cde: {:atom, :b}], nil} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :merge,
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
    end

    test "fetch" do
      assert {:atom, :a} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :fetch, [{:map, [abc: {:atom, :a}], nil}, {:atom, :abc}]}
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

  describe "intersection" do
    test "intersection" do
      assert {:struct,
              [{:__struct__, {:atom, State}}, {:abc, nil}, {:formatted, {:variable, :formatted}}],
              State,
              nil} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   structs: %{
                     State => %StructInfo{
                       fields: [abc: nil, formatted: nil, __struct__: State]
                     }
                   },
                   variables: [
                     %VarInfo{
                       name: :socket,
                       type: nil
                     }
                   ]
                 }),
                 {:intersection,
                  [
                    {:call, {:call, {:variable, :socket}, :assigns, []}, :state, []},
                    {:struct, [formatted: {:variable, :formatted}], {:atom, State}, nil}
                  ]}
               )
    end

    test "none" do
      assert :none == Binding.expand(@env, {:intersection, [{:atom, A}, :none]})
      assert :none == Binding.expand(@env, {:intersection, [:none, {:atom, A}]})
      assert :none == Binding.expand(@env, {:intersection, [:none, :none]})
    end

    test "unknown" do
      assert {:atom, A} == Binding.expand(@env, {:intersection, [{:atom, A}, nil]})
      assert {:atom, A} == Binding.expand(@env, {:intersection, [nil, {:atom, A}]})
      assert nil == Binding.expand(@env, {:intersection, [nil, nil]})
    end

    test "equal" do
      assert {:atom, A} == Binding.expand(@env, {:intersection, [{:atom, A}, {:atom, A}]})
      assert :none == Binding.expand(@env, {:intersection, [{:atom, A}, {:atom, B}]})
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
                {:formatted, {:variable, :formatted}}
              ], State,
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
                    {:struct, [formatted: {:variable, :formatted}], {:atom, State}, nil},
                    {:map, [not_existing: nil, abc: {:atom, X}], nil}
                  ]}
               )

      assert {:struct,
              [
                {:__struct__, {:atom, State}},
                {:abc, {:atom, X}},
                {:formatted, {:variable, :formatted}}
              ], State,
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
                    {:struct, [formatted: {:variable, :formatted}], {:atom, State}, nil}
                  ]}
               )
    end

    test "unknown struct and map" do
      assert {:struct,
              [
                {:__struct__, nil},
                {:formatted, {:variable, :formatted}},
                {:not_existing, nil},
                {:abc, {:atom, X}}
              ], nil,
              nil} ==
               Binding.expand(
                 @env,
                 {:intersection,
                  [
                    {:struct, [formatted: {:variable, :formatted}], nil, nil},
                    {:map, [not_existing: nil, abc: {:atom, X}], nil}
                  ]}
               )
    end

    test "unknown struct and unknown struct" do
      assert {:struct,
              [
                {:__struct__, nil},
                {:formatted, {:variable, :formatted}},
                {:not_existing, nil},
                {:abc, {:atom, X}}
              ], nil,
              nil} ==
               Binding.expand(
                 @env,
                 {:intersection,
                  [
                    {:struct, [formatted: {:variable, :formatted}], nil, nil},
                    {:struct, [not_existing: nil, abc: {:atom, X}], nil, nil}
                  ]}
               )
    end

    test "struct and unknown struct" do
      assert {:struct,
              [
                {:__struct__, {:atom, State}},
                {:abc, {:atom, X}},
                {:formatted, {:variable, :formatted}}
              ], State,
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
                    {:struct, [formatted: {:variable, :formatted}], {:atom, State}, nil},
                    {:struct, [not_existing: nil, abc: {:atom, X}], nil, nil}
                  ]}
               )

      assert {:struct,
              [
                {:__struct__, {:atom, State}},
                {:abc, {:atom, X}},
                {:formatted, {:variable, :formatted}}
              ], State,
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
                    {:struct, [formatted: {:variable, :formatted}], {:atom, State}, nil}
                  ]}
               )
    end

    test "struct" do
      assert {:struct,
              [
                {:__struct__, {:atom, State}},
                {:abc, {:atom, X}},
                {:formatted, {:variable, :formatted}}
              ], State,
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
                    {:struct, [formatted: {:variable, :formatted}], {:atom, State}, nil},
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
                    {:struct, [formatted: {:variable, :formatted}], {:atom, State}, nil}
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
         BindingTest.Some, nil}
      )
    end
  end
end
