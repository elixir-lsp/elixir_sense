defmodule ElixirSense.Core.BindingTest do
  use ExUnit.Case, async: false
  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.ElixirTypes
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
        # `fetch_env!`/`compile_env!` raise when the env is unset; the failure is
        # now rescued to nil (unknown) rather than :none (bottom). :none is dropped
        # from unions and over-claims; an unresolvable config value is simply unknown.
        assert nil ==
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

    test "Map.put expands value" do
      assert {:map, [abc: {:atom, :value}], nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [%VarInfo{version: 1, name: :v, type: {:atom, :value}}]),
                 {:call, {:atom, Map}, :put,
                  [{:map, [abc: nil], nil}, {:atom, :abc}, {:variable, :v, 1}]}
               )
    end

    test "Map.put_new expands value" do
      assert {:map, [abc: {:atom, :value}], nil} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [%VarInfo{version: 1, name: :v, type: {:atom, :value}}]),
                 {:call, {:atom, Map}, :put_new,
                  [{:map, [], nil}, {:atom, :abc}, {:variable, :v, 1}]}
               )
    end

    test "Enum.intersperse keeps list type" do
      assert {:list, {:atom, :item}} ==
               Binding.expand(
                 @env,
                 {:call, {:atom, Enum}, :intersperse,
                  [{:list, {:atom, :item}}, {:atom, :separator}]}
               )
    end

    test "map update" do
      # Updated keys keep the base map's declaration order; new keys append.
      assert {:map, [{:abc, nil}, {:cde, {:variable, :a, 1}}, {:efg, {:atom, :a}}], nil} ==
               Binding.expand(
                 @env,
                 {:map, [abc: nil, cde: {:variable, :a, 1}],
                  {:map, [abc: nil, cde: nil, efg: {:atom, :a}], nil}}
               )
    end

    test "map update with an UNKNOWN base yields an OPEN map (P1 soundness)" do
      # `def f(m), do: %{m | a: 1}` — base does not resolve. Map update preserves
      # the full base type, so the result has the updated key PLUS unknown keys.
      # It must be marked `:open`, never a closed `{:map, fields, nil}` literal.
      assert {:map, [a: {:integer, 1}], :open} ==
               Binding.expand(
                 @env,
                 {:map, [a: {:integer, 1}], {:variable, :m, 1}}
               )
    end

    test "map update with a KNOWN partial base stays partial (nil tail preserved)" do
      assert {:map, [a: {:integer, 9}, b: {:integer, 2}], nil} ==
               Binding.expand(
                 @env,
                 {:map, [a: {:integer, 9}], {:map, [a: {:integer, 1}, b: {:integer, 2}], nil}}
               )
    end

    test "map update on a :closed base stays :closed" do
      assert {:map, [a: {:integer, 9}, b: {:integer, 2}], :closed} ==
               Binding.expand(
                 @env,
                 {:map, [a: {:integer, 9}], {:map, [a: {:integer, 1}, b: {:integer, 2}], :closed}}
               )
    end

    test "nil-tail (partial) map literal is preserved as-is" do
      assert {:map, [a: {:integer, 1}], nil} ==
               Binding.expand(@env, {:map, [a: {:integer, 1}], nil})
    end

    test ":closed map literal is preserved as-is" do
      assert {:map, [a: {:integer, 1}], :closed} ==
               Binding.expand(@env, {:map, [a: {:integer, 1}], :closed})
    end

    test "map update with a non-atom (domain) key does not crash" do
      assert {:map, fields, nil} =
               Binding.expand(
                 @env,
                 {:map, [{{:domain, {:binary, "k"}}, {:integer, 1}}], {:map, [abc: nil], nil}}
               )

      assert {{:domain, {:binary, "k"}}, {:integer, 1}} in fields
      assert {:abc, nil} in fields
    end

    test "tuple_nth out-of-bounds index resolves to :none, not nil" do
      tuple = {:tuple, 2, [{:atom, :a}, {:integer, 1}]}
      assert Binding.expand(@env, {:tuple_nth, tuple, 1}) == {:integer, 1}
      # index == size is out of bounds (0-based) -> :none, not a silent nil
      assert Binding.expand(@env, {:tuple_nth, tuple, 2}) == :none
    end

    test "intersection of two unions collects all overlaps" do
      a = {:union, [{:atom, :a}, {:atom, :b}, {:atom, :c}]}
      b = {:union, [{:atom, :b}, {:atom, :c}]}

      assert Binding.expand(@env, {:intersection, [a, b]}) ==
               {:union, [{:atom, :b}, {:atom, :c}]}

      # disjoint unions intersect to :none (not nil)
      assert Binding.expand(@env, {:intersection, [{:atom, :a}, {:union, [{:atom, :b}]}]}) ==
               :none
    end

    test "union subsumption: list/nonempty/:list, bitstring/binary, map top" do
      int = {:integer, nil}
      assert Binding.expand(@env, {:union, [{:list, int}, {:nonempty_list, int}]}) == {:list, int}
      assert Binding.expand(@env, {:union, [:list, {:list, int}]}) == :list
      assert Binding.expand(@env, {:union, [:bitstring, {:binary, nil}]}) == :bitstring

      assert Binding.expand(@env, {:union, [{:map, [], nil}, {:map, [a: {:integer, 1}], nil}]}) ==
               {:map, [], nil}

      # Map top (`%{}`) also subsumes an OPEN map.
      assert Binding.expand(@env, {:union, [{:map, [], nil}, {:map, [a: {:integer, 1}], :open}]}) ==
               {:map, [], nil}
    end

    test "open vs closed map: union keeps both (conservative covers?)" do
      # A closed map is NOT subsumed by a non-empty open map and vice versa, so
      # neither member is dropped from the union. (covers?/2 stays conservative.)
      open = {:map, [a: {:integer, 1}], :open}
      closed = {:map, [a: {:integer, 1}], nil}

      result = Binding.expand(@env, {:union, [open, closed]})
      assert match?({:union, members} when length(members) == 2, result)
    end

    test "difference output is normalized" do
      assert Binding.expand(
               @env,
               {:difference, {:union, [{:integer, 5}, {:integer, nil}]}, {:integer, 5}}
             ) == {:integer, nil}
    end

    test "case_result drops clauses whose pattern can't match the scrutinee" do
      # scrutinee is a map; the tuple clause is unreachable -> :none (the case
      # raises rather than returning that body).
      assert :none ==
               Binding.expand(@env, {
                 :case_result,
                 {:map, [a: {:integer, 1}], nil},
                 [{{:tuple, 2, [{:atom, :ok}, nil]}, {:atom, Enum}}]
               })

      # an unknown/unresolvable scrutinee keeps every clause (can't rule any out)
      assert {:union, [{:integer, 1}, {:integer, 2}]} ==
               Binding.expand(@env, {
                 :case_result,
                 nil,
                 [{{:atom, :ok}, {:integer, 1}}, {{:atom, :error}, {:integer, 2}}]
               })

      # a matching clause survives
      assert {:atom, :result} ==
               Binding.expand(@env, {
                 :case_result,
                 {:map, [a: {:integer, 1}], nil},
                 [{{:map, [a: nil], nil}, {:atom, :result}}]
               })
    end

    test "map_key projects a non-atom (domain) key" do
      m = {:map, [{{:domain, {:binary, "a"}}, {:integer, 1}}], nil}
      assert Binding.expand(@env, {:map_key, m, {:binary, "a"}}) == {:integer, 1}
      # a key shape that doesn't match any domain key -> unknown
      assert Binding.expand(@env, {:map_key, m, {:binary, "z"}}) == nil
    end

    test "introspection struct" do
      assert {:struct,
              [
                __struct__: {:atom, ElixirSenseExample.ModuleWithTypedStruct},
                other: nil,
                typed_field: nil
              ], {:atom, ElixirSenseExample.ModuleWithTypedStruct}, nil} ==
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
              ], {:atom, ElixirSenseExample.ModuleWithTypedStruct}, nil} ==
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
              ], {:atom, ElixirSenseExample.ModuleWithTypedStruct}, nil} ==
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
              ], {:atom, ElixirSenseExample.ModuleWithTypedStruct}, nil} ==
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
      # A *versioned* var not in scope is unknown (nil), not :none. Previously it
      # fell back to a same-named 0-arity local call which resolved to :none here;
      # that fallback now only applies to the `version == :any` (Code.Fragment
      # misclassification) case.
      assert nil == Binding.expand(@env, {:variable, :v, 1})

      assert nil ==
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
              ], {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, nil} ==
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
              {:atom, ElixirSenseExample.FunctionsWithReturnSpec.Remote}, nil} ==
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
              {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, nil} ==
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

    @tag :requires_native_types
    test "shape conversion recognizes pid/port/reference subtype-compatible descriptors" do
      assert ElixirSense.Core.ElixirTypes.to_shape(Module.Types.Descr.pid()) == :pid
      assert ElixirSense.Core.ElixirTypes.to_shape(Module.Types.Descr.port()) == :port
      assert ElixirSense.Core.ElixirTypes.to_shape(Module.Types.Descr.reference()) == :reference
    end

    test "remote type expansion resolves __MODULE__ and module attributes" do
      env =
        @env
        |> Map.merge(%{
          module: ElixirSenseExample.FunctionsWithReturnSpec,
          attributes: [
            %AttributeInfo{
              name: :remote_mod,
              type: {:atom, ElixirSenseExample.FunctionsWithReturnSpec.Remote}
            }
          ],
          vars: [
            %VarInfo{
              version: 1,
              name: :from_module,
              type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f3, []}
            },
            %VarInfo{
              version: 2,
              name: :from_attr,
              type: {:call, {:atom, ElixirSenseExample.FunctionsWithReturnSpec}, :f4, []}
            }
          ]
        })

      assert {:struct,
              [
                {:__struct__, {:atom, ElixirSenseExample.FunctionsWithReturnSpec.Remote}},
                {:abc, nil}
              ], {:atom, ElixirSenseExample.FunctionsWithReturnSpec.Remote}, nil} =
               Binding.expand(env, {:variable, :from_module, 1})

      assert {:map, [abc: nil], nil} = Binding.expand(env, {:variable, :from_attr, 2})
    end

    test "remote type expansion resolves module-valued variables" do
      env =
        @env
        |> Map.merge(%{
          module: ElixirSenseExample.FunctionsWithReturnSpec,
          vars: [
            %VarInfo{
              version: 1,
              name: :remote_module,
              type: {:atom, ElixirSenseExample.FunctionsWithReturnSpec.Remote}
            }
          ]
        })

      assert {:struct,
              [
                abc: nil
              ], {:atom, ElixirSenseExample.FunctionsWithReturnSpec.Remote}, nil} ==
               Binding.expand_type(
                 env,
                 {:., [], [{:remote_module, [], nil}, :t]},
                 [],
                 false,
                 []
               )
    end

    test "remote type expansion resolves nested module aliases" do
      env = %Binding{
        module: ElixirSenseExample.ModuleWithTypespecs,
        aliases: [{Elixir.Remote, ElixirSenseExample.ModuleWithTypespecs.Remote}],
        vars: [],
        attributes: []
      }

      # remote_t :: atom — atom is a built-in type so expands to nil (unknown),
      # but the alias resolution path is still exercised
      assert nil ==
               Binding.expand_type(
                 env,
                 {{:., [], [{:__aliases__, [], [:Remote]}, :remote_t]}, [], []},
                 [],
                 false,
                 []
               )

      # __MODULE__.Remote resolves to ElixirSenseExample.ModuleWithTypespecs.Remote
      # remote_list_t :: [remote_t] — remote_t resolves to nil, so list element is nil
      assert {:list, nil} ==
               Binding.expand_type(
                 env,
                 {{:., [],
                   [{{:., [], [{:__MODULE__, [], nil}, :Remote]}, [], []}, :remote_list_t]}, [],
                  []},
                 [],
                 false,
                 []
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
              specs: ["@spec fun(integer(), integer(), any()) :: %MyMod{}"],
              elixir_types_sig: {
                :strong,
                nil,
                [
                  {[
                     Module.Types.Descr.integer(),
                     Module.Types.Descr.integer(),
                     Module.Types.Descr.dynamic()
                   ],
                   Module.Types.Descr.closed_map([
                     {:__struct__, Module.Types.Descr.atom([MyMod])},
                     {:abc, Module.Types.Descr.dynamic()}
                   ])}
                ]
              }
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

      expected =
        {:struct, [__struct__: {:atom, MyMod}, abc: nil], {:atom, MyMod}, nil}

      assert expected ==
               Binding.expand(
                 env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :ref, type: {:local_call, :fun, {1, 1}, [nil]}}
                 ]),
                 {:variable, :ref, 1}
               )

      assert expected ==
               Binding.expand(
                 env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :ref, type: {:local_call, :fun, {1, 1}, [nil, nil]}}
                 ]),
                 {:variable, :ref, 1}
               )

      assert expected ==
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

    if ElixirSense.Core.ElixirTypes.available?() do
      test "local call metadata multi-clause overload selection by arg type" do
        env =
          @env
          |> Map.merge(%{
            module: MyMod,
            function: {:some, 0},
            specs: %{
              {MyMod, :classify, 1} => %SpecInfo{
                specs: [
                  "@spec classify(integer()) :: :int",
                  "@spec classify(atom()) :: :atom_type"
                ],
                elixir_types_sig: {
                  :strong,
                  nil,
                  [
                    {[Module.Types.Descr.integer()], Module.Types.Descr.atom([:int])},
                    {[Module.Types.Descr.atom()], Module.Types.Descr.atom([:atom_type])}
                  ]
                }
              }
            },
            mods_funs_to_positions: %{
              {MyMod, :classify, 1} => %ModFunInfo{
                params: [[{:x, [], []}]],
                type: :def
              }
            }
          })

        # Call with integer arg → should return :int (not union)
        assert {:atom, :int} ==
                 Binding.expand(
                   env
                   |> Map.put(:vars, [
                     %VarInfo{
                       version: 1,
                       name: :ref,
                       type: {:local_call, :classify, {1, 1}, [{:integer, 42}]}
                     }
                   ]),
                   {:variable, :ref, 1}
                 )

        # Call with atom arg → should return :atom_type (not union)
        assert {:atom, :atom_type} ==
                 Binding.expand(
                   env
                   |> Map.put(:vars, [
                     %VarInfo{
                       version: 1,
                       name: :ref,
                       type: {:local_call, :classify, {1, 1}, [{:atom, :foo}]}
                     }
                   ]),
                   {:variable, :ref, 1}
                 )

        # Call with nil arg (unknown type) → should return union of both
        result =
          Binding.expand(
            env
            |> Map.put(:vars, [
              %VarInfo{
                version: 1,
                name: :ref,
                type: {:local_call, :classify, {1, 1}, [nil]}
              }
            ]),
            {:variable, :ref, 1}
          )

        assert result in [
                 {:union, [{:atom, :int}, {:atom, :atom_type}]},
                 {:union, [{:atom, :atom_type}, {:atom, :int}]}
               ]
      end
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

    @tag :requires_native_types
    test "metadata spec signatures are preferred for remote call return typing" do
      env =
        @env
        |> Map.put(:specs, %{
          {String, :split, 2} => %SpecInfo{
            specs: ["@spec split(binary(), binary()) :: [binary()]"],
            elixir_types_sig: {
              :strong,
              nil,
              [
                {[
                   Module.Types.Descr.binary(),
                   Module.Types.Descr.binary()
                 ], Module.Types.Descr.list(Module.Types.Descr.binary())}
              ]
            }
          }
        })

      assert {:ok, sig} =
               ElixirSense.Core.ElixirTypes.spec_signature_from_metadata(env, String, :split, 2)

      assert is_map(ElixirSense.Core.ElixirTypes.extract_return_type_from_sig(sig))
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
      # A no-parens call misclassified as a variable carries `version == :any`
      # (Code.Fragment / surround_context spelling). Only that case falls back to a
      # 0-arity local/imported call. A *versioned* out-of-scope var would be nil.
      assert {:atom, :asd} ==
               Binding.expand(
                 @env
                 |> Map.merge(%{
                   vars: [
                     %VarInfo{version: 1, name: :ref, type: {:variable, :f02, :any}}
                   ],
                   functions: [{ElixirSenseExample.FunctionsWithReturnSpec, [{:f02, 0}]}]
                 }),
                 {:variable, :ref, 1}
               )
    end

    test "versioned out-of-scope var does not resolve to a 0-arity local function" do
      # Regression for task #6: a versioned var not in `env.vars` must be unknown
      # (nil), never a same-named 0-arity function, even when such a function is
      # imported. Otherwise the function's return type leaks into thunks.
      assert nil ==
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
    defp expand_call_type(call) do
      Binding.expand(
        %{
          @env
          | vars: [
              %VarInfo{version: 1, name: :a, type: nil},
              %VarInfo{version: 1, name: :b, type: nil}
            ]
        },
        call
      )
    end

    test "arithmetic / bitwise / boolean operator result types" do
      a = {:variable, :a, 1}
      b = {:variable, :b, 1}
      erl = {:atom, :erlang}

      # integer-only: bitwise + div/rem
      for fun <- [:band, :bor, :bxor, :bsl, :bsr, :div, :rem] do
        assert {:integer, nil} == expand_call_type({:call, erl, fun, [a, b]})
      end

      assert {:integer, nil} == expand_call_type({:call, erl, :bnot, [a]})

      # float division
      assert {:float, nil} == expand_call_type({:call, erl, :/, [a, b]})

      # number tower for +,-,*
      assert {:integer, nil} ==
               expand_call_type({:call, erl, :+, [{:integer, 1}, {:integer, 2}]})

      assert {:float, nil} == expand_call_type({:call, erl, :*, [{:float, 1.0}, {:integer, 2}]})
      assert :number == expand_call_type({:call, erl, :-, [a, b]})

      # comparisons / boolean ops / type guards -> boolean()
      for fun <- [:==, :"/=", :<, :>=, :"=:=", :not, :is_integer, :is_map] do
        assert :boolean == expand_call_type({:call, erl, fun, [a]})
      end

      # raising functions -> :none (drop out of result unions)
      assert :none == expand_call_type({:call, erl, :error, [a]})
      assert :none == expand_call_type({:call, erl, :throw, [a]})
    end

    test "++" do
      # `++` unions both operands' element types.
      assert {:list, {:union, [{:integer, 1}, {:integer, 2}]}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :a, type: {:integer, 1}},
                   %VarInfo{version: 1, name: :b, type: {:integer, 2}}
                 ]),
                 {:local_call, :++, {1, 1}, [list: {:variable, :a, 1}, list: {:variable, :b, 1}]}
               )

      assert {:list, {:union, [{:integer, 1}, {:integer, 2}]}} ==
               Binding.expand(
                 @env
                 |> Map.put(:vars, [
                   %VarInfo{version: 1, name: :a, type: {:integer, 1}},
                   %VarInfo{version: 1, name: :b, type: {:integer, 2}}
                 ]),
                 {:call, {:atom, :erlang}, :++,
                  [list: {:variable, :a, 1}, list: {:variable, :b, 1}]}
               )

      # `a ++ b` is a list even when the left element type is unknown.
      assert {:list, nil} ==
               Binding.expand(
                 @env |> Map.put(:vars, [%VarInfo{version: 1, name: :a, type: nil}]),
                 {:call, {:atom, :erlang}, :++, [{:variable, :a, 1}, {:list, {:integer, 0}}]}
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

    test "put on an unknown base yields an open map" do
      # Unknown base (`nil`): the result must stay OPEN — `Map.put` only adds
      # the one key we can see; other keys may already exist on the base.
      assert {:map, [cde: {:atom, :b}], :open} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :put, [nil, {:atom, :cde}, {:atom, :b}]}
               )
    end

    test "put with an unknown key on an unknown base yields an open map" do
      assert {:map, [], :open} =
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
      # A resolved struct is literal-complete, so the resulting map is `:closed`.
      assert {:map, [other: nil, typed_field: nil], :closed} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :from_struct,
                  [{:struct, [], {:atom, ElixirSenseExample.ModuleWithTypedStruct}, nil}]}
               )
    end

    test "from_struct atom arg" do
      assert {:map, [other: nil, typed_field: nil], :closed} =
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
      # `Map.new/0` constructs the empty, literal-complete (`:closed`) map.
      assert {:map, [], :closed} = Binding.expand(@env, {:call, {:atom, Map}, :new, []})
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
              ], {:atom, State}, nil} ==
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
              ], {:atom, State}, nil} ==
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
              ], {:atom, State}, nil} ==
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
              ], nil, nil} ==
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
              ], nil, nil} ==
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
              ], {:atom, State}, nil} ==
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
              ], {:atom, State}, nil} ==
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
              ], {:atom, State}, nil} ==
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

  describe "descr-backed intersection (native on, exact shapes)" do
    # Consolidated backlog P2 2.2 / GPT P1: when native typing is enabled AND both
    # operands are `descr_exact?` shape kinds, intersection delegates to the real
    # `Module.Types.Descr.intersection/2`, which is strictly more faithful than the
    # hand-written structural approximation. These assertions encode the
    # Descr-faithful results.
    setup do
      prev = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, true)

      on_exit(fn ->
        Application.put_env(:elixir_sense, :use_elixir_types, prev)
      end)

      # The gate only fires when the native backend is available; assert it is so
      # these descr-faithful expectations are actually exercised.
      assert ElixirTypes.enabled?()

      :ok
    end

    test "boolean ∩ {:atom, true} narrows to true" do
      assert {:atom, true} ==
               Binding.expand(@env, {:intersection, [:boolean, {:atom, true}]})

      assert {:atom, true} ==
               Binding.expand(@env, {:intersection, [{:atom, true}, :boolean]})
    end

    test "overlapping atom unions intersect to the shared member" do
      assert {:atom, :b} ==
               Binding.expand(
                 @env,
                 {:intersection, [{:union, [atom: :a, atom: :b]}, {:union, [atom: :b, atom: :c]}]}
               )
    end

    test "disjoint exact atoms intersect to :none" do
      assert :none == Binding.expand(@env, {:intersection, [{:atom, :a}, {:atom, :b}]})
    end

    test "integer ∩ number narrows to integer" do
      # `to_shape` renders the `integer()` descr as the abstract literal shape
      # `{:integer, nil}` (nil payload = any integer).
      assert {:integer, nil} == Binding.expand(@env, {:intersection, [:integer, :number]})
    end

    test "tuple intersects elementwise via Descr" do
      assert {:tuple, 2, [{:integer, nil}, {:atom, true}]} ==
               Binding.expand(
                 @env,
                 {:intersection,
                  [{:tuple, 2, [:integer, :boolean]}, {:tuple, 2, [:integer, {:atom, true}]}]}
               )
    end

    test "list element intersection narrows the element type" do
      assert {:list, {:atom, true}} ==
               Binding.expand(
                 @env,
                 {:intersection, [{:list, :boolean}, {:list, {:atom, true}}]}
               )
    end

    test "nonempty_list element intersection narrows; disjoint elements -> :none" do
      assert {:nonempty_list, {:atom, true}} ==
               Binding.expand(
                 @env,
                 {:intersection, [{:nonempty_list, :boolean}, {:nonempty_list, {:atom, true}}]}
               )

      assert :none ==
               Binding.expand(
                 @env,
                 {:intersection, [{:nonempty_list, {:atom, :a}}, {:nonempty_list, {:atom, :b}}]}
               )
    end

    test "non-exact kinds (maps) still take the custom path" do
      # Maps are excluded from descr_exact? (coercion makes them open), so this
      # must produce the SAME result as the native-off custom path: a key union.
      assert {:map, [{:a, nil}, {:b, {:atom, B}}, {:c, {:atom, C}}], nil} ==
               Binding.expand(
                 @env,
                 {:intersection,
                  [
                    {:map, [a: nil, b: {:atom, B}], nil},
                    {:map, [c: {:atom, C}], nil}
                  ]}
               )
    end

    test "literal scalars (excluded from whitelist) keep custom disjointness" do
      # {:integer, _} widens on coercion, so it is excluded — custom path applies.
      assert :none == Binding.expand(@env, {:intersection, [{:integer, 1}, {:integer, 2}]})
    end
  end

  describe "descr-backed intersection (native off, custom path preserved)" do
    setup do
      prev = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, false)
      on_exit(fn -> Application.put_env(:elixir_sense, :use_elixir_types, prev) end)
      :ok
    end

    test "boolean ∩ {:atom, true} via custom subsumption still narrows to true" do
      assert {:atom, true} ==
               Binding.expand(@env, {:intersection, [:boolean, {:atom, true}]})
    end

    test "disjoint atoms intersect to :none on the custom path" do
      assert :none == Binding.expand(@env, {:intersection, [{:atom, A}, {:atom, B}]})
    end

    test "tuple elementwise intersection unchanged on the custom path" do
      assert {:tuple, 2, [{:atom, B}, {:atom, A}]} ==
               Binding.expand(
                 @env,
                 {:intersection, [{:tuple, 2, [nil, {:atom, A}]}, {:tuple, 2, [{:atom, B}, nil]}]}
               )
    end
  end

  describe "intersection overlap matrix (soundness regressions)" do
    # Task #3: the fallback used `covers?/2` (subsumption) to decide disjointness,
    # so any non-subsuming pair collapsed to :none even when the value sets overlap.

    test "nonempty_list ∩ list intersects elements and stays nonempty" do
      # Both contain [] for the list side, but a nonempty side forces nonempty;
      # element types intersect.
      assert {:nonempty_list, {:integer, nil}} ==
               Binding.expand(
                 @env,
                 {:intersection, [{:nonempty_list, nil}, {:list, {:integer, nil}}]}
               )

      assert {:nonempty_list, {:integer, nil}} ==
               Binding.expand(
                 @env,
                 {:intersection, [{:list, {:integer, nil}}, {:nonempty_list, nil}]}
               )
    end

    test "list ∩ list with overlapping element types" do
      assert {:list, {:integer, 1}} ==
               Binding.expand(
                 @env,
                 {:intersection, [{:list, :integer}, {:list, {:integer, 1}}]}
               )
    end

    test "list(:a) ∩ list(:b) is the empty list, not :none (both contain [])" do
      # Disjoint element types still leave `[]` reachable -> {:list, :empty}.
      assert {:list, :empty} ==
               Binding.expand(
                 @env,
                 {:intersection, [{:list, {:atom, :a}}, {:list, {:atom, :b}}]}
               )
    end

    test "generic :list ∩ concrete list yields the concrete list" do
      assert {:list, {:integer, nil}} ==
               Binding.expand(@env, {:intersection, [:list, {:list, {:integer, nil}}]})

      assert {:nonempty_list, {:integer, nil}} ==
               Binding.expand(@env, {:intersection, [{:nonempty_list, {:integer, nil}}, :list]})
    end

    test "provably-disjoint top-level kinds are :none" do
      # atom vs list, integer vs binary, tuple vs map, etc.
      assert :none ==
               Binding.expand(@env, {:intersection, [{:atom, :a}, {:list, {:integer, nil}}]})

      assert :none ==
               Binding.expand(@env, {:intersection, [{:integer, 1}, {:binary, nil}]})

      assert :none ==
               Binding.expand(@env, {:intersection, [:tuple, {:map, [], nil}]})

      assert :none == Binding.expand(@env, {:intersection, [:list, {:integer, nil}]})
    end

    test "different-arity tuples are disjoint" do
      assert :none ==
               Binding.expand(
                 @env,
                 {:intersection, [{:tuple, 2, [nil, nil]}, {:tuple, 1, [nil]}]}
               )
    end

    test "distinct concrete literals of the same kind are disjoint" do
      assert :none == Binding.expand(@env, {:intersection, [{:atom, :a}, {:atom, :b}]})
      assert :none == Binding.expand(@env, {:intersection, [{:integer, 1}, {:integer, 2}]})
      assert :none == Binding.expand(@env, {:intersection, [{:binary, "a"}, {:binary, "b"}]})
    end

    test "structs of different concrete modules are disjoint" do
      assert :none ==
               Binding.expand(
                 @env,
                 {:intersection,
                  [{:struct, [], {:atom, Foo}, nil}, {:struct, [], {:atom, Bar}, nil}]}
               )
    end

    test "same-kind, non-subsuming, non-literal pair is unknown (nil), never :none" do
      # integer() ∩ float() both live in the number tower: we do not assert these
      # disjoint (conservative). Result is unknown rather than a false :none.
      assert nil == Binding.expand(@env, {:intersection, [{:integer, nil}, {:float, nil}]})
    end
  end

  describe "++ operator (improper / non-list RHS soundness)" do
    # Task #5: `++` returned {:list, _} unconditionally. Ground truth
    # (:erlang.++): `[] ++ x` is `x`; `[1|_] ++ x` may be improper.

    test "proper-list RHS yields a proper list of unioned elements" do
      assert {:list, {:integer, nil}} ==
               Binding.expand(
                 @env,
                 {:call, {:atom, Kernel}, :++,
                  [{:list, {:integer, nil}}, {:list, {:integer, nil}}]}
               )
    end

    test "non-empty LHS with unknown RHS is unknown (may be improper)" do
      assert nil ==
               Binding.expand(
                 @env,
                 {:call, {:atom, Kernel}, :++, [{:nonempty_list, {:integer, nil}}, nil]}
               )
    end

    test "non-empty proper LHS with known non-list RHS is an improper non-empty list" do
      # `[1, ...] ++ :a` is a non-empty IMPROPER list: proper prefix of the LHS
      # element type, final tail = the RHS shape. Modeled by the 3-tuple (was
      # conservatively `nil` before the improper-list shape existed).
      assert {:nonempty_list, {:integer, nil}, {:atom, :a}} ==
               Binding.expand(
                 @env,
                 {:call, {:atom, Kernel}, :++, [{:nonempty_list, {:integer, nil}}, {:atom, :a}]}
               )
    end

    test "possibly-empty LHS with non-list RHS stays unknown (could be the bare RHS)" do
      # `list ++ :a` could be exactly `:a` when the LHS is `[]`, which the
      # prefix-bearing 3-tuple can't model, so we stay conservative (`nil`).
      assert nil ==
               Binding.expand(
                 @env,
                 {:call, {:atom, Kernel}, :++, [{:list, {:integer, nil}}, {:atom, :a}]}
               )
    end

    test "empty-list LHS yields exactly the RHS" do
      assert {:atom, :a} ==
               Binding.expand(
                 @env,
                 {:call, {:atom, Kernel}, :++, [{:list, :empty}, {:atom, :a}]}
               )
    end

    test "-- stays a proper list of the left element type" do
      assert {:list, {:integer, nil}} ==
               Binding.expand(
                 @env,
                 {:call, {:atom, Kernel}, :--, [{:list, {:integer, nil}}, {:atom, :a}]}
               )
    end
  end

  describe "improper non-empty list shape ({:nonempty_list, elem, tail})" do
    # The 3-tuple = non-empty, possibly-improper list: proper prefix of `elem`
    # elements terminated by a non-list `tail`. It is its OWN kind variant —
    # neither subsumed by nor subsuming proper list shapes.

    test "do_expand expands both components" do
      assert {:nonempty_list, {:integer, nil}, {:atom, :a}} ==
               Binding.expand(@env, {:nonempty_list, :integer, {:atom, :a}})
    end

    test "is NOT covered by a proper list union member (improper not subsumed)" do
      # A union of a proper list and the improper 3-tuple does NOT collapse —
      # `covers?` keeps both (the proper shape does not subsume the improper one).
      improper = {:nonempty_list, {:integer, nil}, {:atom, :a}}

      assert {:union, members} =
               Binding.expand(@env, {:union, [{:list, {:integer, nil}}, improper]})

      assert improper in members
    end

    test "two equal-shaped 3-tuples in a union dedupe via covers?" do
      improper = {:nonempty_list, {:integer, nil}, {:atom, :a}}
      assert improper == Binding.expand(@env, {:union, [improper, improper]})
    end

    test "intersection of two 3-tuples is elementwise" do
      # prefix: {:integer, nil} ∩ nil = {:integer, nil}; tail: boolean ∩ true = true
      assert {:nonempty_list, {:integer, nil}, {:atom, true}} ==
               Binding.expand(
                 @env,
                 {:intersection,
                  [
                    {:nonempty_list, {:integer, nil}, :boolean},
                    {:nonempty_list, nil, {:atom, true}}
                  ]}
               )
    end

    test "intersection bottoms out when the tails are disjoint" do
      assert :none ==
               Binding.expand(
                 @env,
                 {:intersection,
                  [
                    {:nonempty_list, {:integer, nil}, {:atom, :a}},
                    {:nonempty_list, {:integer, nil}, {:atom, :b}}
                  ]}
               )
    end

    test "intersection with a proper list stays conservative (nil), not :none" do
      # An improper list and a proper list are not provably disjoint in the
      # coarse `shape_kind` classifier (both `:list`), so the intersection is
      # `nil` (unknown) rather than an unsound `:none`.
      assert nil ==
               Binding.expand(
                 @env,
                 {:intersection,
                  [{:nonempty_list, {:integer, nil}, {:atom, :a}}, {:list, {:integer, nil}}]}
               )
    end
  end

  describe "numeric operators on known non-numeric operands" do
    # Task #27: numeric_result over-claimed :number for non-numeric operands.

    test "+ with a known non-numeric operand is unknown, not :number" do
      assert nil ==
               Binding.expand(@env, {:call, {:atom, Kernel}, :+, [{:integer, 1}, {:atom, :a}]})

      assert nil ==
               Binding.expand(@env, {:call, {:atom, Kernel}, :*, [{:binary, "x"}, {:integer, 2}]})
    end

    test "+ with an unknown operand may still be numeric (number())" do
      assert :number ==
               Binding.expand(@env, {:call, {:atom, Kernel}, :+, [{:integer, 1}, nil]})
    end

    test "+ of two integers is integer()" do
      assert {:integer, nil} ==
               Binding.expand(
                 @env,
                 {:call, {:atom, Kernel}, :+, [{:integer, 1}, {:integer, 2}]}
               )
    end
  end

  describe "andalso/orelse are not boolean-typed (task #27)" do
    test "andalso/orelse do not resolve to :boolean" do
      refute :boolean ==
               Binding.expand(
                 @env,
                 {:call, {:atom, :erlang}, :andalso, [:boolean, {:integer, 5}]}
               )

      refute :boolean ==
               Binding.expand(@env, {:call, {:atom, :erlang}, :orelse, [:boolean, {:integer, 5}]})
    end

    test "strict and/or still resolve to :boolean" do
      assert :boolean ==
               Binding.expand(@env, {:call, {:atom, :erlang}, :and, [:boolean, :boolean]})
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

  describe "terminal shape expansion" do
    test "primitive type atoms are preserved through expand" do
      assert Binding.expand(@env, :atom) == :atom
      assert Binding.expand(@env, :integer) == {:integer, nil}
      assert Binding.expand(@env, :binary) == {:binary, nil}
      assert Binding.expand(@env, :float) == {:float, nil}
      assert Binding.expand(@env, :number) == :number
      assert Binding.expand(@env, :pid) == :pid
      assert Binding.expand(@env, :port) == :port
      assert Binding.expand(@env, :reference) == :reference
      assert Binding.expand(@env, :fun) == :fun
      assert Binding.expand(@env, :tuple) == :tuple
    end

    test ":none is preserved through expand" do
      assert Binding.expand(@env, :none) == :none
    end

    test "tagged terminal shapes are preserved through expand" do
      assert Binding.expand(@env, {:binary, nil}) == {:binary, nil}
      assert Binding.expand(@env, {:float, nil}) == {:float, nil}
      assert Binding.expand(@env, {:fun, 2}) == {:fun, 2}
    end

    test "fun with args and return is expanded recursively" do
      assert Binding.expand(@env, {:fun, [nil, nil], nil}) == {:fun, [nil, nil], nil}

      assert Binding.expand(@env, {:fun, [{:atom, :ok}], {:integer, nil}}) ==
               {:fun, [{:atom, :ok}], {:integer, nil}}
    end

    test "fun_clauses are expanded recursively" do
      assert Binding.expand(@env, {:fun_clauses, [{[nil], {:atom, :ok}}]}) ==
               {:fun_clauses, [{[nil], {:atom, :ok}}]}
    end
  end

  describe "difference (cross-clause subtraction)" do
    defp union_env(type) do
      %Binding{
        functions: __ENV__.functions,
        macros: __ENV__.macros,
        vars: [%VarInfo{version: 1, name: :x, type: type}]
      }
    end

    test "drops the subtracted member from a union base" do
      env = union_env({:union, [{:atom, :a}, {:atom, :b}, {:atom, :c}]})

      assert Binding.expand(env, {:difference, {:variable, :x, 1}, {:atom, :a}}) ==
               {:union, [{:atom, :b}, {:atom, :c}]}
    end

    test "collapses to the single remaining member" do
      env = union_env({:union, [{:binary, nil}, {:atom, nil}]})

      # the flagship: `binary() | nil` minus `nil` is `binary()`
      assert Binding.expand(env, {:difference, {:variable, :x, 1}, {:atom, nil}}) ==
               {:binary, nil}
    end

    test "subtracts a tagged-tuple family (element-wise, _ as wildcard)" do
      ok = {:tuple, 2, [{:atom, :ok}, {:integer, nil}]}
      error = {:tuple, 2, [{:atom, :error}, {:atom, :reason}]}
      env = union_env({:union, [ok, error]})

      # `{:ok, _}` subtracts the whole :ok branch, leaving the :error branch
      assert Binding.expand(
               env,
               {:difference, {:variable, :x, 1}, {:tuple, 2, [{:atom, :ok}, nil]}}
             ) == error
    end

    test "a generic subtracted type removes all matching literals" do
      env = union_env({:union, [{:atom, :a}, {:integer, 1}, {:integer, 2}]})

      assert Binding.expand(env, {:difference, {:variable, :x, 1}, :integer}) ==
               {:atom, :a}
    end

    test "subtracting everything yields :none" do
      env = union_env({:union, [{:atom, :a}, {:atom, :b}]})

      assert Binding.expand(
               env,
               {:difference, {:variable, :x, 1}, {:union, [{:atom, :a}, {:atom, :b}]}}
             ) == :none
    end

    test "is conservative for a non-union base (returns it unchanged)" do
      env = union_env({:atom, :a})

      assert Binding.expand(env, {:difference, {:variable, :x, 1}, {:atom, :b}}) ==
               {:atom, :a}
    end

    test "a versioned out-of-scope base var is unknown (nil), not a 0-arity local call" do
      # Previously a versioned var not in `env.vars` fell back to a same-named
      # 0-arity local call (yielding :none here). That injected unrelated function
      # return types into difference/feasibility thunks; a versioned var that is
      # not in scope is simply unknown, so the difference is unknown (nil).
      assert Binding.expand(@env, {:difference, {:variable, :missing, 9}, {:atom, :a}}) == nil
    end
  end

  describe "union / intersection normalization" do
    test "drops union members subsumed by a more general sibling" do
      # `:integer` expands to the generic `{:integer, nil}`, which subsumes `5`
      assert Binding.expand(@env, {:union, [{:integer, 5}, :integer]}) == {:integer, nil}
      assert Binding.expand(@env, {:union, [{:atom, :ok}, :atom]}) == :atom
    end

    test "flattens nested unions and drops :none" do
      assert Binding.expand(@env, {:union, [{:atom, :a}, {:union, [{:atom, :b}, :none]}]}) ==
               {:union, [{:atom, :a}, {:atom, :b}]}
    end

    test "keeps distinct literals" do
      assert Binding.expand(@env, {:union, [{:integer, 1}, {:integer, 2}]}) ==
               {:union, [{:integer, 1}, {:integer, 2}]}
    end

    test "intersection of a generic and its literal is the literal" do
      assert Binding.expand(@env, {:intersection, [:integer, {:integer, 5}]}) == {:integer, 5}
      assert Binding.expand(@env, {:intersection, [:atom, {:atom, :ok}]}) == {:atom, :ok}
    end

    test "intersection of disjoint scalars is :none" do
      assert Binding.expand(@env, {:intersection, [{:atom, :a}, {:integer, 5}]}) == :none
    end

    test "merges same-key maps field-wise" do
      m1 = {:map, [a: {:integer, 1}], nil}
      m2 = {:map, [a: {:integer, 2}], nil}

      assert Binding.expand(@env, {:union, [m1, m2]}) ==
               {:map, [a: {:union, [{:integer, 1}, {:integer, 2}]}], nil}
    end

    test "keeps maps with different key sets as a union" do
      m1 = {:map, [a: {:integer, 1}], nil}
      m2 = {:map, [b: {:integer, 2}], nil}

      assert Binding.expand(@env, {:union, [m1, m2]}) == {:union, [m1, m2]}
    end

    test "merges same-module structs field-wise" do
      # expansion fills all URI fields; only the differing `host` becomes a union
      s1 = {:struct, [__struct__: {:atom, URI}, host: {:binary, "a"}], {:atom, URI}, nil}
      s2 = {:struct, [__struct__: {:atom, URI}, host: {:binary, "b"}], {:atom, URI}, nil}

      assert {:struct, fields, {:atom, URI}, nil} = Binding.expand(@env, {:union, [s1, s2]})
      assert Keyword.fetch!(fields, :host) == {:union, [{:binary, "a"}, {:binary, "b"}]}
      assert Keyword.fetch!(fields, :port) == nil
    end

    test "keeps different-module structs as a union" do
      s1 = {:struct, [__struct__: {:atom, URI}, host: {:binary, "a"}], {:atom, URI}, nil}
      s2 = {:struct, [__struct__: {:atom, Date}, year: {:integer, 2024}], {:atom, Date}, nil}

      assert {:union, [{:struct, _, {:atom, URI}, nil}, {:struct, _, {:atom, Date}, nil}]} =
               Binding.expand(@env, {:union, [s1, s2]})
    end

    test "merges list members, unioning element types" do
      assert Binding.expand(@env, {:union, [{:list, {:integer, 1}}, {:list, {:integer, 2}}]}) ==
               {:list, {:union, [{:integer, 1}, {:integer, 2}]}}
    end

    test "merges an empty list with a non-empty list" do
      assert Binding.expand(@env, {:union, [{:list, :empty}, {:list, {:integer, 1}}]}) ==
               {:list, {:integer, 1}}
    end

    test "nonempty_list resolves and supports head/tail projections" do
      env = union_env({:nonempty_list, {:integer, 5}})
      assert Binding.expand(env, {:variable, :x, 1}) == {:nonempty_list, {:integer, 5}}
      assert Binding.expand(env, {:list_head, {:variable, :x, 1}}) == {:integer, 5}
      assert Binding.expand(env, {:list_tail, {:variable, :x, 1}}) == {:list, {:integer, 5}}
    end

    test "number() subsumes integer()/float() in a union" do
      assert Binding.expand(@env, {:union, [:number, {:integer, 5}]}) == :number
      assert Binding.expand(@env, {:union, [:number, {:float, 1.0}]}) == :number
      # without number() present, integer()|float() stays precise (no widening)
      assert Binding.expand(@env, {:union, [{:integer, nil}, {:float, nil}]}) ==
               {:union, [{:integer, nil}, {:float, nil}]}
    end

    test "intersection narrows number() to integer()/float()" do
      assert Binding.expand(@env, {:intersection, [:number, {:integer, 5}]}) == {:integer, 5}
      assert Binding.expand(@env, {:intersection, [:number, :integer]}) == {:integer, nil}
    end

    test "generic tuple()/fun() subsume their concrete instances" do
      assert Binding.expand(@env, {:union, [:tuple, {:tuple, 2, [{:atom, :ok}, nil]}]}) == :tuple

      assert Binding.expand(@env, {:intersection, [:tuple, {:tuple, 1, [{:atom, :ok}]}]}) ==
               {:tuple, 1, [{:atom, :ok}]}

      assert Binding.expand(@env, {:union, [:fun, {:fun, 1}]}) == :fun
    end
  end

  # ---------------------------------------------------------------------------
  # Shape vocabulary CONTRACT tests.
  # These pin the documented algebra behaviours so future changes to the engine
  # break loudly rather than silently.  Do NOT change expected values without
  # a matching update to the ## Shape vocabulary section in Binding's moduledoc.
  # ---------------------------------------------------------------------------
  describe "shape vocabulary contract" do
    # --- nil (unknown / top) ---

    # nil member in a union poisons the whole union to nil (absorbing element).
    test "nil member in union poisons union to nil" do
      assert nil ==
               Binding.expand(@env, {:union, [{:atom, :ok}, nil, {:integer, 1}]})
    end

    # nil as an intersection operand is the TOP (identity): T ∩ nil = T.
    test "nil as intersection identity: T ∩ nil = T" do
      result = Binding.expand(@env, {:intersection, [{:atom, :ok}, nil]})
      # nil is the identity in intersection; the result should be the other operand,
      # i.e. {:atom, :ok}, not :none and not nil.
      assert result == {:atom, :ok}
    end

    # --- :none (bottom) ---

    # :none members are dropped from unions — they vanish, leaving the rest.
    test ":none member is dropped from union" do
      assert {:atom, :ok} ==
               Binding.expand(@env, {:union, [{:atom, :ok}, :none]})
    end

    # An all-:none union collapses to :none.
    test "all-:none union collapses to :none" do
      assert :none ==
               Binding.expand(@env, {:union, [:none, :none]})
    end

    # :none is the annihilator in intersection: :none ∩ T = :none.
    test ":none annihilates intersection" do
      assert :none ==
               Binding.expand(@env, {:intersection, [:none, {:atom, :ok}]})
    end

    # --- expand of terminal shapes ---

    test "expand :atom returns :atom" do
      assert :atom == Binding.expand(@env, :atom)
    end

    test "expand :none returns :none" do
      assert :none == Binding.expand(@env, :none)
    end

    test "expand {:atom, :ok} returns {:atom, :ok}" do
      assert {:atom, :ok} == Binding.expand(@env, {:atom, :ok})
    end

    test "expand :empty_list returns {:list, :empty}" do
      # :empty_list is the alias produced by to_shape; expand normalises it.
      assert {:list, :empty} == Binding.expand(@env, :empty_list)
    end

    test "expand {:list, :empty} is idempotent" do
      assert {:list, :empty} == Binding.expand(@env, {:list, :empty})
    end

    # --- covers? top cases ---

    # The generic :list atom subsumes any {:list, _} shape.
    test "covers?: :list subsumes {:list, _}" do
      # Use difference/2 indirectly: `[:a] | :list` should reduce to :list
      # because {:atom, :a} is subsumed by... wait, let's directly test via
      # normalize_union which calls drop_subsumed which uses covers?.
      # {:list, {:atom, :ok}} should be dropped when :list is also present.
      result = Binding.expand(@env, {:union, [:list, {:list, {:atom, :ok}}]})
      assert result == :list
    end

    # nil (as covers? left-side) means "any element matches" — used only
    # inside tuple element coverage; indirectly tested through difference.
    test "covers?: nil left-side covers anything (wildcard)" do
      # A {:tuple, 1, [nil]} pattern covers {:tuple, 1, [{:atom, :ok}]}
      # which means the latter is dropped from the union when the former is present.
      result = Binding.expand(@env, {:union, [{:tuple, 1, [nil]}, {:tuple, 1, [{:atom, :ok}]}]})
      # {:tuple, 1, [nil]} subsumes {:tuple, 1, [{:atom, :ok}]}, so only the
      # more general member remains.
      assert result == {:tuple, 1, [nil]}
    end

    # --- map tail three-marker model: :closed / nil (partial) / :open ---

    # Rendering compromise: both `:closed` (literal-complete) and `nil` (partial)
    # render WITHOUT the open marker; only `:open` carries `...`.
    test "closed map tail :closed renders with no open marker" do
      alias ElixirSense.Core.TypePresentation
      {:ok, rendered} = TypePresentation.render({:map, [a: {:integer, 1}], :closed})
      refute String.contains?(rendered, "...")
    end

    test "partial map tail nil renders with no open marker (display compromise)" do
      alias ElixirSense.Core.TypePresentation
      {:ok, rendered} = TypePresentation.render({:map, [a: {:integer, 1}], nil})
      refute String.contains?(rendered, "...")
    end

    test "open map tail :open renders with open marker" do
      alias ElixirSense.Core.TypePresentation
      shape = {:map, [a: {:integer, 1}], :open}
      {:ok, rendered} = TypePresentation.render(shape)
      # open map — must contain the "..." open marker
      assert String.contains?(rendered, "...")
    end

    test "empty open map renders as map()" do
      alias ElixirSense.Core.TypePresentation
      {:ok, rendered} = TypePresentation.render({:map, [], :open})
      assert rendered == "map()"
    end

    # map_key access: a missing key on a `:closed` map is PROVABLY absent
    # (`:not_set`); on a `nil`-partial or `:open` map it is merely unknown (`nil`).
    test "map_key on :closed map: missing key is :not_set" do
      closed = {:map, [a: {:integer, 1}], :closed}
      assert :not_set == Binding.expand(@env, {:map_key, closed, {:atom, :b}})
      assert {:integer, 1} == Binding.expand(@env, {:map_key, closed, {:atom, :a}})
    end

    test "map_key on nil-partial map: missing key is nil (unknown), never :not_set" do
      partial = {:map, [a: {:integer, 1}], nil}
      assert nil == Binding.expand(@env, {:map_key, partial, {:atom, :b}})
    end

    test "map_key on :open map: missing key is nil (unknown)" do
      open = {:map, [a: {:integer, 1}], :open}
      assert nil == Binding.expand(@env, {:map_key, open, {:atom, :b}})
    end

    # merge_tails: closed only if BOTH closed; partial otherwise; open is sticky.
    test "Map.merge of two :closed maps stays :closed" do
      a = {:map, [a: {:integer, 1}], :closed}
      b = {:map, [b: {:integer, 2}], :closed}
      assert {:map, _, :closed} = Binding.expand(@env, {:call, {:atom, Map}, :merge, [a, b]})
    end

    test "Map.merge of :closed and nil-partial maps is partial (nil)" do
      a = {:map, [a: {:integer, 1}], :closed}
      b = {:map, [b: {:integer, 2}], nil}
      assert {:map, _, nil} = Binding.expand(@env, {:call, {:atom, Map}, :merge, [a, b]})
    end

    # Map.put on a :closed base keeps the result :closed (key set still known).
    test "Map.put on a :closed base stays :closed" do
      base = {:map, [a: {:integer, 1}], :closed}

      assert {:map, _, :closed} =
               Binding.expand(
                 @env,
                 {:call, {:atom, Map}, :put, [base, {:atom, :b}, {:integer, 2}]}
               )
    end

    # --- :not_set filtering in fields_for_receiver ---

    test ":not_set field values are excluded from fields_for_receiver" do
      alias ElixirSense.Core.TypePresentation
      env_binding = %Binding{functions: __ENV__.functions, macros: __ENV__.macros}

      # Build a map shape with one normal field and one :not_set field.
      # :not_set means the key is provably absent — should not appear in completion.
      shape = {:map, [present: {:atom, :ok}, absent: :not_set], nil}
      fields = TypePresentation.fields_for_receiver(env_binding, shape)

      assert Map.has_key?(fields, :present)
      refute Map.has_key?(fields, :absent)
    end

    # --- {:optional, _} wrapper preserved through expand ---

    test "{:optional, inner} is preserved through expand" do
      assert {:optional, {:atom, :ok}} ==
               Binding.expand(@env, {:optional, {:atom, :ok}})
    end

    test "{:optional, inner} with unresolvable inner collapses to nil" do
      assert nil ==
               Binding.expand(@env, {:optional, {:variable, :missing_var_xyz, 999}})
    end

    # optional field IS informative (must not be dropped by uninformative_field?)
    test "{:optional, inner} renders as if_set(...) not term()" do
      alias ElixirSense.Core.TypePresentation
      {:ok, rendered} = TypePresentation.render({:optional, {:atom, :ok}})
      assert rendered == "if_set(:ok)"
    end
  end

  describe "expand preserves optional (if_set) map-field wrappers" do
    test "a top-level {:optional, inner} expands the inner (variable) and re-wraps" do
      env = %Binding{
        functions: __ENV__.functions,
        macros: __ENV__.macros,
        vars: [%VarInfo{version: 1, name: :v, type: {:integer, 5}}]
      }

      assert Binding.expand(env, {:optional, {:variable, :v, 1}}) ==
               {:optional, {:integer, 5}}
    end

    test "a top-level {:optional, inner} expands the inner and re-wraps" do
      assert Binding.expand(@env, {:optional, {:atom, :ok}}) == {:optional, {:atom, :ok}}
    end

    test "{:optional, inner} whose inner resolves to nil drops the wrapper" do
      # An unresolvable variable expands to nil — no wrapper to keep.
      assert Binding.expand(@env, {:optional, {:variable, :missing, 9}}) == nil
    end

    test "intersection with an optional member is conservative (no crash, no false :none)" do
      # {:optional, _} is unknown to combine_intersection's structured clauses;
      # it must fall through to nil (unknown) rather than crash or claim :none.
      assert Binding.expand(@env, {:intersection, [{:optional, {:integer, 1}}, {:tuple, 0, []}]}) !=
               :none

      # An optional wrapping a covered scalar intersects to the narrower side,
      # not to :none.
      assert Binding.expand(@env, {:intersection, [{:optional, {:integer, 5}}, :number]}) !=
               :none
    end
  end
end
