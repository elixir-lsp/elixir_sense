defmodule ElixirSense.Core.TypeInferenceTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.TypeInference

  describe "find_vars" do
    defp find_vars_in(code, match_context \\ nil, context \\ nil) do
      ast =
        Code.string_to_quoted!(code)
        |> Macro.prewalk(fn
          {:__aliases__, _, list} ->
            Module.concat(list)

          {atom, _meta, var_context} = node when is_atom(atom) and is_atom(var_context) ->
            Macro.update_meta(node, &Keyword.put(&1, :version, 1))

          node ->
            node
        end)

      TypeInference.find_vars(ast, match_context, context)
    end

    test "finds simple variable" do
      assert find_vars_in("a", nil, :match) == [{{:a, 1}, nil}]
      assert find_vars_in("a", nil) == []
    end

    test "finds simple variable with match context" do
      assert find_vars_in("a", {:integer, 1}, :match) == [{{:a, 1}, {:integer, 1}}]
      assert find_vars_in("a", {:integer, 1}) == []
    end

    test "does not find special variables" do
      assert find_vars_in("__MODULE__") == []
      assert find_vars_in("__MODULE__", nil, :match) == []
    end

    test "does not find _" do
      assert find_vars_in("_") == []
      assert find_vars_in("_", nil, :match) == []
    end

    test "does not find other primitives" do
      assert find_vars_in("1") == []
      assert find_vars_in("1.3") == []
      assert find_vars_in("\"as\"") == []
    end

    test "does not find pinned variables" do
      assert find_vars_in("^a") == []
      assert find_vars_in("^a", nil, :match) == []
    end

    test "finds variables in tuple" do
      assert find_vars_in("{}", nil, :match) == []
      assert find_vars_in("{a}", nil, :match) == [{{:a, 1}, nil}]
      assert find_vars_in("{a}") == []

      assert find_vars_in("{a, b}", nil, :match) == [
               {{:a, 1}, nil},
               {{:b, 1}, nil}
             ]

      assert find_vars_in("{a, b}") == []
    end

    test "finds variables in tuple with match context" do
      assert find_vars_in("{a}", {:integer, 1}, :match) == [
               {{:a, 1}, {:tuple_nth, {:integer, 1}, 0}}
             ]

      assert find_vars_in("{a, b}", {:integer, 1}, :match) == [
               {
                 {:a, 1},
                 {:tuple_nth, {:integer, 1}, 0}
               },
               {
                 {:b, 1},
                 {:tuple_nth, {:integer, 1}, 1}
               }
             ]
    end

    test "finds variables in list" do
      assert find_vars_in("[]", nil, :match) == []
      assert find_vars_in("[a]", nil, :match) == [{{:a, 1}, nil}]
      assert find_vars_in("[a]", nil) == []

      assert find_vars_in("[a, b]", nil, :match) == [
               {{:a, 1}, nil},
               {{:b, 1}, nil}
             ]

      assert find_vars_in("[a | b]", nil, :match) == [
               {{:a, 1}, nil},
               {{:b, 1}, nil}
             ]
    end

    test "finds variables in list with match context" do
      assert find_vars_in("[a]", {:integer, 1}, :match) == [
               {{:a, 1}, {:list_head, {:integer, 1}}}
             ]

      assert find_vars_in("[a, b]", {:integer, 1}, :match) == [
               {{:a, 1}, {:list_head, {:integer, 1}}},
               {{:b, 1}, {:list_head, {:list_tail, {:integer, 1}}}}
             ]

      assert find_vars_in("[a | b]", {:integer, 1}, :match) == [
               {{:a, 1}, {:list_head, {:integer, 1}}},
               {{:b, 1}, {:list_tail, {:integer, 1}}}
             ]
    end

    test "finds variables in map" do
      assert find_vars_in("%{}", nil, :match) == []
      assert find_vars_in("%{a: a}", nil, :match) == [{{:a, 1}, nil}]
      assert find_vars_in("%{a: a}", nil) == []
      assert find_vars_in("%{\"a\" => a}", nil, :match) == [{{:a, 1}, nil}]
      # NOTE variable keys are forbidden in match
      assert find_vars_in("%{a => 1}", nil, :match) == []
      assert find_vars_in("%{a => 1}", nil) == []
      # NOTE map update is forbidden in match
      assert find_vars_in("%{a | b: b}", nil, :match) == []
      assert find_vars_in("%{a | b: b}", nil) == []
    end

    test "finds variables in map with match context" do
      assert find_vars_in("%{a: a}", {:integer, 1}, :match) == [
               {{:a, 1}, {:map_key, {:integer, 1}, {:atom, :a}}}
             ]
    end

    test "finds variables in struct" do
      assert find_vars_in("%Foo{}", nil, :match) == []
      assert find_vars_in("%Foo{a: a}", nil, :match) == [{{:a, 1}, nil}]
      assert find_vars_in("%Foo{a: a}", nil) == []
      assert find_vars_in("%bar{a: a}", nil) == []
      assert find_vars_in("%bar{a: a}", nil, :match) == [{{:a, 1}, nil}, {{:bar, 1}, nil}]
      assert find_vars_in("%_{a: a}", nil, :match) == [{{:a, 1}, nil}]
      assert find_vars_in("%Foo{a | b: b}", nil) == []
      assert find_vars_in("%Foo{a | b: b}", nil, :match) == []
    end

    test "finds variables in struct with match context" do
      assert find_vars_in("%Foo{a: a}", {:integer, 1}, :match) == [
               {{:a, 1}, {:map_key, {:integer, 1}, {:atom, :a}}}
             ]

      assert find_vars_in("%bar{a: a}", {:integer, 1}, :match) == [
               {{:a, 1}, {:map_key, {:integer, 1}, {:atom, :a}}},
               {{:bar, 1}, {:map_key, {:integer, 1}, {:atom, :__struct__}}}
             ]
    end

    test "finds variables in match" do
      assert find_vars_in("a = b", nil, :match) == [{{:b, 1}, nil}, {{:a, 1}, nil}]
      assert find_vars_in("a = b", nil) == [{{:a, 1}, {:variable, :b}}]
      assert find_vars_in("^a = b", nil) == []

      assert find_vars_in("a = a", nil, :match) == [{{:a, 1}, nil}]
      assert find_vars_in("a = a", nil) == [{{:a, 1}, {:variable, :a}}]

      assert find_vars_in("a = b = c", nil, :match) == [
               {{:c, 1}, nil},
               {{:b, 1}, nil},
               {{:a, 1}, nil}
             ]

      assert find_vars_in("[a] = b", nil) == [{{:a, 1}, {:list_head, {:variable, :b}}}]
      assert find_vars_in("[a] = b", nil, :match) == [{{:b, 1}, {:list, nil}}, {{:a, 1}, nil}]

      assert find_vars_in("[a] = b", {:variable, :x}, :match) == [
               {{:b, 1}, {:intersection, [variable: :x, list: nil]}},
               {{:a, 1}, {:list_head, {:variable, :x}}}
             ]

      assert find_vars_in("{a} = b", nil) == [{{:a, 1}, {:tuple_nth, {:variable, :b}, 0}}]

      assert find_vars_in("{a} = b", nil, :match) == [
               {{:b, 1}, {:tuple, 1, [nil]}},
               {{:a, 1}, nil}
             ]

      assert find_vars_in("{a} = b", {:variable, :x}, :match) == [
               {{:b, 1}, {:intersection, [{:variable, :x}, {:tuple, 1, [nil]}]}},
               {{:a, 1}, {:tuple_nth, {:variable, :x}, 0}}
             ]

      assert find_vars_in("%{foo: a} = b", nil) == [
               {{:a, 1}, {:map_key, {:variable, :b}, {:atom, :foo}}}
             ]

      assert find_vars_in("%{foo: a} = b", nil, :match) == [
               {{:b, 1}, {:map, [foo: nil], nil}},
               {{:a, 1}, nil}
             ]

      assert find_vars_in("%{foo: a} = b", {:variable, :x}, :match) == [
               {{:b, 1}, {:intersection, [{:variable, :x}, {:map, [foo: nil], nil}]}},
               {{:a, 1}, {:map_key, {:variable, :x}, {:atom, :foo}}}
             ]

      assert find_vars_in("%Foo{foo: a} = b", nil) == [
               {{:a, 1}, {:map_key, {:variable, :b}, {:atom, :foo}}}
             ]

      assert find_vars_in("%Foo{foo: a} = b", nil, :match) == [
               {{:b, 1}, {:struct, [foo: nil], {:atom, Foo}, nil}},
               {{:a, 1}, nil}
             ]

      assert find_vars_in("%Foo{foo: a} = b", {:variable, :x}, :match) == [
               {{:b, 1},
                {:intersection, [{:variable, :x}, {:struct, [foo: nil], {:atom, Foo}, nil}]}},
               {{:a, 1}, {:map_key, {:variable, :x}, {:atom, :foo}}}
             ]

      assert find_vars_in("%{foo: a} = %{bar: b} = c", nil) == [
               {
                 {:a, 1},
                 {
                   :map_key,
                   {:intersection, [{:map, [bar: nil], nil}, {:variable, :c}]},
                   {:atom, :foo}
                 }
               },
               {{:b, 1},
                {:map_key, {:intersection, [{:map, [foo: nil], nil}, {:variable, :c}]},
                 {:atom, :bar}}}
             ]

      # TODO check how Binding module handles this case
      assert find_vars_in("%{foo: a} = %{bar: b} = c", nil, :match) == [
               {
                 {:c, 1},
                 {:intersection, [{:map, [foo: nil], nil}, {:map, [bar: nil], nil}]}
               },
               {{:a, 1}, {:map_key, {:map, [bar: nil], nil}, {:atom, :foo}}},
               {{:b, 1}, {:map_key, {:map, [foo: nil], nil}, {:atom, :bar}}}
             ]

      assert find_vars_in("%{foo: a} = %{bar: b} = c", {:variable, :x}, :match) == [
               {
                 {:c, 1},
                 {
                   :intersection,
                   [{:map, [bar: nil], nil}, {:variable, :x}, {:map, [foo: nil], nil}]
                 }
               },
               {
                 {:a, 1},
                 {
                   :map_key,
                   {:intersection, [{:variable, :x}, {:map, [bar: nil], nil}]},
                   {:atom, :foo}
                 }
               },
               {
                 {:b, 1},
                 {
                   :map_key,
                   {:intersection, [{:variable, :x}, {:map, [foo: nil], nil}]},
                   {:atom, :bar}
                 }
               }
             ]
    end
  end

  describe "get_binding_type" do
    defp binding_type_in(code, context \\ nil) do
      # NOTE binding_type_in works on expanded AST so it expects aliases expanded to atoms
      ast =
        Code.string_to_quoted!(code)
        |> Macro.prewalk(fn
          {:__aliases__, _, list} ->
            Module.concat(list)

          {atom, _meta, var_context} = node when is_atom(atom) and is_atom(var_context) ->
            Macro.update_meta(node, &Keyword.put(&1, :version, 1))

          node ->
            node
        end)

      TypeInference.get_binding_type(ast, context)
    end

    test "atom" do
      assert binding_type_in(":a") == {:atom, :a}
      assert binding_type_in("My.Module") == {:atom, My.Module}
      assert binding_type_in("nil") == {:atom, nil}
      assert binding_type_in("true") == {:atom, true}
      assert binding_type_in("false") == {:atom, false}
    end

    test "variable" do
      assert binding_type_in("a") == {:variable, :a}
      assert binding_type_in("a", :match) == nil
      assert binding_type_in("^a", :match) == {:variable, :a}
      assert binding_type_in("^a") == :none
      assert binding_type_in("_", :match) == nil
      assert binding_type_in("_") == :none
    end

    test "attribute" do
      assert binding_type_in("@a") == {:attribute, :a}
    end

    test "integer" do
      assert binding_type_in("1") == {:integer, 1}
    end

    test "list" do
      assert binding_type_in("[]") == {:list, :empty}
      assert binding_type_in("[a]") == {:list, {:variable, :a}}
      assert binding_type_in("[a]", :match) == {:list, nil}
      assert binding_type_in("[^a]", :match) == {:list, {:variable, :a}}
      assert binding_type_in("[[1]]") == {:list, {:list, {:integer, 1}}}
      # TODO intersection a | b?
      assert binding_type_in("[a, b]") == {:list, {:variable, :a}}
      assert binding_type_in("[a | b]") == {:list, {:variable, :a}}
    end

    test "tuple" do
      assert binding_type_in("{}") == {:tuple, 0, []}
      assert binding_type_in("{a}") == {:tuple, 1, [{:variable, :a}]}
      assert binding_type_in("{a, b}") == {:tuple, 2, [{:variable, :a}, {:variable, :b}]}
    end

    test "map" do
      assert binding_type_in("%{}") == {:map, [], nil}
      assert binding_type_in("%{asd: a}") == {:map, [{:asd, {:variable, :a}}], nil}
      # NOTE non atom keys are not supported
      assert binding_type_in("%{\"asd\" => a}") == {:map, [], nil}

      assert binding_type_in("%{b | asd: a}") ==
               {:map, [{:asd, {:variable, :a}}], {:variable, :b}}

      assert binding_type_in("%{b | asd: a}", :match) == :none
    end

    test "map with __struct__ key" do
      assert binding_type_in("%{__struct__: Foo}") == {:struct, [], {:atom, Foo}, nil}

      assert binding_type_in("%{__struct__: Foo, asd: a}") ==
               {:struct, [{:asd, {:variable, :a}}], {:atom, Foo}, nil}

      assert binding_type_in("%{b | __struct__: Foo, asd: a}") ==
               {:struct, [{:asd, {:variable, :a}}], {:atom, Foo}, {:variable, :b}}
    end

    test "struct" do
      assert binding_type_in("%Foo{}") == {:struct, [], {:atom, Foo}, nil}
      assert binding_type_in("%a{}") == {:struct, [], {:variable, :a}, nil}
      assert binding_type_in("%@a{}") == {:struct, [], {:attribute, :a}, nil}

      assert binding_type_in("%Foo{asd: a}") ==
               {:struct, [{:asd, {:variable, :a}}], {:atom, Foo}, nil}

      assert binding_type_in("%Foo{b | asd: a}") ==
               {:struct, [{:asd, {:variable, :a}}], {:atom, Foo}, {:variable, :b}}

      assert binding_type_in("%Foo{b | asd: a}", :match) == :none
    end

    test "range" do
      assert binding_type_in("a..b") ==
               {:struct,
                [{:first, {:variable, :a}}, {:last, {:variable, :b}}, {:step, {:integer, 1}}],
                {:atom, Range}, nil}

      assert binding_type_in("a..b//2") ==
               {:struct,
                [{:first, {:variable, :a}}, {:last, {:variable, :b}}, {:step, {:integer, 2}}],
                {:atom, Range}, nil}
    end

    test "sigil" do
      # NOTE we do not attempt to parse sigils
      assert binding_type_in("~r//") == {:struct, [], {:atom, Regex}, nil}
      assert binding_type_in("~R//") == {:struct, [], {:atom, Regex}, nil}
      assert binding_type_in("~N//") == {:struct, [], {:atom, NaiveDateTime}, nil}
      assert binding_type_in("~U//") == {:struct, [], {:atom, DateTime}, nil}
      assert binding_type_in("~T//") == {:struct, [], {:atom, Time}, nil}
      assert binding_type_in("~D//") == {:struct, [], {:atom, Date}, nil}
    end

    test "local call" do
      assert binding_type_in("foo(a)") == {:local_call, :foo, [{:variable, :a}]}
    end

    test "remote call" do
      assert binding_type_in(":foo.bar(a)") == {:call, {:atom, :foo}, :bar, [variable: :a]}
    end

    test "match" do
      assert binding_type_in("a = 5") == {:integer, 5}
      assert binding_type_in("5 = a") == {:intersection, [integer: 5, variable: :a]}
      assert binding_type_in("b = 5 = a") == {:intersection, [{:integer, 5}, {:variable, :a}]}
      assert binding_type_in("5 = 5") == {:integer, 5}

      assert binding_type_in("%{foo: a} = %{bar: b}") ==
               {:intersection, [{:map, [foo: nil], nil}, {:map, [bar: {:variable, :b}], nil}]}

      assert binding_type_in("%{foo: a} = %{bar: b}", :match) ==
               {:intersection, [{:map, [foo: nil], nil}, {:map, [bar: nil], nil}]}
    end

    test "other" do
      assert binding_type_in("\"asd\"") == nil
      assert binding_type_in("1.23") == nil
    end
  end
end
