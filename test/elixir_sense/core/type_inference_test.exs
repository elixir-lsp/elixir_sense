defmodule ElixirSense.Core.TypeInferenceTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.TypeInference

  describe "find_typed_vars" do
    defp find_typed_vars_in(code, match_context \\ nil, context \\ nil) do
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

      TypeInference.find_typed_vars(ast, match_context, context)
    end

    test "finds simple variable" do
      assert find_typed_vars_in("a", nil, :match) == [{{:a, 1}, nil}]
      assert find_typed_vars_in("a", nil) == []
    end

    test "finds simple variable with match context" do
      assert find_typed_vars_in("a", {:integer, 1}, :match) == [{{:a, 1}, {:integer, 1}}]
      assert find_typed_vars_in("a", {:integer, 1}) == []
    end

    test "does not find special variables" do
      assert find_typed_vars_in("__MODULE__") == []
      assert find_typed_vars_in("__MODULE__", nil, :match) == []
    end

    test "does not find _" do
      assert find_typed_vars_in("_") == []
      assert find_typed_vars_in("_", nil, :match) == []
    end

    test "does not find other primitives" do
      assert find_typed_vars_in("1") == []
      assert find_typed_vars_in("1.3") == []
      assert find_typed_vars_in("\"as\"") == []
    end

    test "does not find pinned variables" do
      assert find_typed_vars_in("^a") == []
      assert find_typed_vars_in("^a", nil, :match) == []
    end

    test "finds variables in tuple" do
      assert find_typed_vars_in("{}", nil, :match) == []
      assert find_typed_vars_in("{a}", nil, :match) == [{{:a, 1}, nil}]
      assert find_typed_vars_in("{a}", :none, :match) == [{{:a, 1}, :none}]
      assert find_typed_vars_in("{a}") == []

      assert find_typed_vars_in("{a, b}", nil, :match) == [
               {{:a, 1}, nil},
               {{:b, 1}, nil}
             ]

      assert find_typed_vars_in("{a, b}") == []
    end

    test "finds variables in tuple with match context" do
      assert find_typed_vars_in("{a}", {:integer, 1}, :match) == [
               {{:a, 1}, {:tuple_nth, {:integer, 1}, 0}}
             ]

      assert find_typed_vars_in("{a, b}", {:integer, 1}, :match) == [
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
      assert find_typed_vars_in("[]", nil, :match) == []
      assert find_typed_vars_in("[a]", nil, :match) == [{{:a, 1}, nil}]
      assert find_typed_vars_in("[a]", :none, :match) == [{{:a, 1}, :none}]
      assert find_typed_vars_in("[a]", nil) == []

      assert find_typed_vars_in("[a, b]", nil, :match) == [
               {{:a, 1}, nil},
               {{:b, 1}, nil}
             ]

      assert find_typed_vars_in("[a | b]", nil, :match) == [
               {{:a, 1}, nil},
               {{:b, 1}, nil}
             ]
    end

    test "finds variables in list with match context" do
      assert find_typed_vars_in("[a]", {:integer, 1}, :match) == [
               {{:a, 1}, {:list_head, {:integer, 1}}}
             ]

      assert find_typed_vars_in("[a, b]", {:integer, 1}, :match) == [
               {{:a, 1}, {:list_head, {:integer, 1}}},
               {{:b, 1}, {:list_head, {:list_tail, {:integer, 1}}}}
             ]

      assert find_typed_vars_in("[a | b]", {:integer, 1}, :match) == [
               {{:a, 1}, {:list_head, {:integer, 1}}},
               {{:b, 1}, {:list_tail, {:integer, 1}}}
             ]

      assert find_typed_vars_in("[1, a | b]", {:integer, 1}, :match) == [
               {{:a, 1}, {:list_head, {:list_tail, {:integer, 1}}}},
               {{:b, 1}, {:list_tail, {:list_tail, {:integer, 1}}}}
             ]

      assert find_typed_vars_in("[a | 1]", {:integer, 1}, :match) == [
               {{:a, 1}, {:list_head, {:integer, 1}}}
             ]
    end

    test "finds variables in list operator" do
      assert find_typed_vars_in(":erlang.++([a], [5])", {:integer, 1}, :match) == [
               {{:a, 1}, {:list_head, {:integer, 1}}}
             ]

      assert find_typed_vars_in(":erlang.++([a], 5)", {:integer, 1}, :match) == [
               {{:a, 1}, {:list_head, {:integer, 1}}}
             ]

      assert find_typed_vars_in(":erlang.++([2, a], [5])", {:integer, 1}, :match) == [
               {{:a, 1}, {:list_head, {:list_tail, {:integer, 1}}}}
             ]

      assert find_typed_vars_in(":erlang.++([5], [a])", {:integer, 1}, :match) == [
               {{:a, 1}, {:list_head, {:list_tail, {:integer, 1}}}}
             ]

      assert find_typed_vars_in(":erlang.++([5], [2, a])", {:integer, 1}, :match) == [
               {{:a, 1}, {:list_head, {:list_tail, {:list_tail, {:integer, 1}}}}}
             ]

      assert find_typed_vars_in(":erlang.++([5], a)", {:integer, 1}, :match) == [
               {{:a, 1}, {:list_tail, {:integer, 1}}}
             ]

      assert find_typed_vars_in(":erlang.++([5, 6], a)", {:integer, 1}, :match) == [
               {{:a, 1}, {:list_tail, {:list_tail, {:integer, 1}}}}
             ]
    end

    test "finds variables in map" do
      assert find_typed_vars_in("%{}", nil, :match) == []
      assert find_typed_vars_in("%{a: a}", nil, :match) == [{{:a, 1}, nil}]
      assert find_typed_vars_in("%{a: a}", :none, :match) == [{{:a, 1}, :none}]
      assert find_typed_vars_in("%{a: a}", nil) == []
      assert find_typed_vars_in("%{\"a\" => a}", nil, :match) == [{{:a, 1}, nil}]
      # NOTE variable keys are forbidden in match
      assert find_typed_vars_in("%{a => 1}", nil, :match) == []
      assert find_typed_vars_in("%{a => 1}", nil) == []
      # NOTE map update is forbidden in match
      assert find_typed_vars_in("%{a | b: b}", nil, :match) == []
      assert find_typed_vars_in("%{a | b: b}", nil) == []
    end

    test "finds variables in map with match context" do
      assert find_typed_vars_in("%{a: a}", {:integer, 1}, :match) == [
               {{:a, 1}, {:map_key, {:integer, 1}, {:atom, :a}}}
             ]
    end

    test "finds variables in struct" do
      assert find_typed_vars_in("%Foo{}", nil, :match) == []
      assert find_typed_vars_in("%Foo{a: a}", nil, :match) == [{{:a, 1}, nil}]
      assert find_typed_vars_in("%Foo{a: a}", :none, :match) == [{{:a, 1}, :none}]
      assert find_typed_vars_in("%Foo{a: a}", nil) == []
      assert find_typed_vars_in("%bar{a: a}", nil) == []
      assert find_typed_vars_in("%bar{a: a}", nil, :match) == [{{:a, 1}, nil}, {{:bar, 1}, nil}]
      assert find_typed_vars_in("%_{a: a}", nil, :match) == [{{:a, 1}, nil}]
      assert find_typed_vars_in("%Foo{a | b: b}", nil) == []
      assert find_typed_vars_in("%Foo{a | b: b}", nil, :match) == []
    end

    test "finds variables in struct with match context" do
      assert find_typed_vars_in("%Foo{a: a}", {:integer, 1}, :match) == [
               {{:a, 1}, {:map_key, {:integer, 1}, {:atom, :a}}}
             ]

      assert find_typed_vars_in("%bar{a: a}", {:integer, 1}, :match) == [
               {{:a, 1}, {:map_key, {:integer, 1}, {:atom, :a}}},
               {{:bar, 1}, {:map_key, {:integer, 1}, {:atom, :__struct__}}}
             ]
    end

    test "finds variables in match" do
      assert find_typed_vars_in("a = b", nil, :match) == [{{:b, 1}, nil}, {{:a, 1}, nil}]
      assert find_typed_vars_in("a = b", nil) == [{{:a, 1}, {:variable, :b, 1}}]
      assert find_typed_vars_in("^a = b", nil) == []

      assert find_typed_vars_in("a = a", nil, :match) == [{{:a, 1}, nil}]
      assert find_typed_vars_in("a = a", nil) == [{{:a, 1}, {:variable, :a, 1}}]

      assert find_typed_vars_in("a = b = c", nil, :match) == [
               {{:c, 1}, nil},
               {{:b, 1}, nil},
               {{:a, 1}, nil}
             ]

      assert find_typed_vars_in("[a] = b", nil) == [{{:a, 1}, {:list_head, {:variable, :b, 1}}}]

      assert find_typed_vars_in("[a] = b", nil, :match) == [
               {{:b, 1}, {:list, nil}},
               {{:a, 1}, nil}
             ]

      assert find_typed_vars_in("[a] = b", {:variable, :x}, :match) == [
               {{:b, 1}, {:intersection, [variable: :x, list: nil]}},
               {{:a, 1}, {:list_head, {:variable, :x}}}
             ]

      assert find_typed_vars_in("{a} = b", nil) == [
               {{:a, 1}, {:tuple_nth, {:variable, :b, 1}, 0}}
             ]

      assert find_typed_vars_in("{a} = b", nil, :match) == [
               {{:b, 1}, {:tuple, 1, [nil]}},
               {{:a, 1}, nil}
             ]

      assert find_typed_vars_in("{a} = b", {:variable, :x}, :match) == [
               {{:b, 1}, {:intersection, [{:variable, :x}, {:tuple, 1, [nil]}]}},
               {{:a, 1}, {:tuple_nth, {:variable, :x}, 0}}
             ]

      assert find_typed_vars_in("%{foo: a} = b", nil) == [
               {{:a, 1}, {:map_key, {:variable, :b, 1}, {:atom, :foo}}}
             ]

      assert find_typed_vars_in("%{foo: a} = b", nil, :match) == [
               {{:b, 1}, {:map, [foo: nil], nil}},
               {{:a, 1}, nil}
             ]

      assert find_typed_vars_in("%{foo: a} = b", {:variable, :x}, :match) == [
               {{:b, 1}, {:intersection, [{:variable, :x}, {:map, [foo: nil], nil}]}},
               {{:a, 1}, {:map_key, {:variable, :x}, {:atom, :foo}}}
             ]

      assert find_typed_vars_in("%Foo{foo: a} = b", nil) == [
               {{:a, 1}, {:map_key, {:variable, :b, 1}, {:atom, :foo}}}
             ]

      assert find_typed_vars_in("%Foo{foo: a} = b", nil, :match) == [
               {{:b, 1}, {:struct, [foo: nil], {:atom, Foo}, nil}},
               {{:a, 1}, nil}
             ]

      assert find_typed_vars_in("%Foo{foo: a} = b", {:variable, :x}, :match) == [
               {{:b, 1},
                {:intersection, [{:variable, :x}, {:struct, [foo: nil], {:atom, Foo}, nil}]}},
               {{:a, 1}, {:map_key, {:variable, :x}, {:atom, :foo}}}
             ]

      assert find_typed_vars_in("%{foo: a} = %{bar: b} = c", nil) == [
               {
                 {:a, 1},
                 {
                   :map_key,
                   {:intersection, [{:map, [bar: nil], nil}, {:variable, :c, 1}]},
                   {:atom, :foo}
                 }
               },
               {{:b, 1},
                {:map_key, {:intersection, [{:map, [foo: nil], nil}, {:variable, :c, 1}]},
                 {:atom, :bar}}}
             ]

      assert find_typed_vars_in("%{foo: a} = %{bar: b} = c", nil, :match) == [
               {
                 {:c, 1},
                 {:intersection, [{:map, [foo: nil], nil}, {:map, [bar: nil], nil}]}
               },
               {{:a, 1}, {:map_key, {:map, [bar: nil], nil}, {:atom, :foo}}},
               {{:b, 1}, {:map_key, {:map, [foo: nil], nil}, {:atom, :bar}}}
             ]

      assert find_typed_vars_in("%{foo: a} = %{bar: b} = c", {:variable, :x}, :match) == [
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

  describe "type_of" do
    defp type_of(code, context \\ nil) do
      # NOTE type_of works on expanded AST so it expects aliases expanded to atoms
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

      TypeInference.type_of(ast, context)
    end

    test "atom" do
      assert type_of(":a") == {:atom, :a}
      assert type_of("My.Module") == {:atom, My.Module}
      assert type_of("nil") == {:atom, nil}
      assert type_of("true") == {:atom, true}
      assert type_of("false") == {:atom, false}
    end

    test "variable" do
      assert type_of("a") == {:variable, :a, 1}
      assert type_of("a", :match) == nil
      assert type_of("^a", :match) == {:variable, :a, 1}
      assert type_of("^a") == :none
      assert type_of("_", :match) == nil
      assert type_of("_") == :none
    end

    test "attribute" do
      assert type_of("@a") == {:attribute, :a}
    end

    test "integer" do
      assert type_of("1") == {:integer, 1}
    end

    test "list" do
      assert type_of("[]") == {:list, :empty}
      assert type_of("[a]") == {:list, {:variable, :a, 1}}
      assert type_of("[a | 1]") == {:list, {:variable, :a, 1}}
      assert type_of("[a]", :match) == {:list, nil}
      assert type_of("[a | 1]", :match) == {:list, nil}
      assert type_of("[^a]", :match) == {:list, {:variable, :a, 1}}
      assert type_of("[[1]]") == {:list, {:list, {:integer, 1}}}
      # TODO union a | b?
      assert type_of("[a, b]") == {:list, {:variable, :a, 1}}
      assert type_of("[a | b]") == {:list, {:variable, :a, 1}}
      assert type_of("[a, b | c]") == {:list, {:variable, :a, 1}}
    end

    test "list operators" do
      assert type_of(":erlang.++([a], [b])") ==
               {:call, {:atom, :erlang}, :++,
                [list: {:variable, :a, 1}, list: {:variable, :b, 1}]}

      assert type_of(":erlang.--([a], [b])") ==
               {:call, {:atom, :erlang}, :--,
                [list: {:variable, :a, 1}, list: {:variable, :b, 1}]}
    end

    test "tuple" do
      assert type_of("{}") == {:tuple, 0, []}
      assert type_of("{a}") == {:tuple, 1, [{:variable, :a, 1}]}
      assert type_of("{a, b}") == {:tuple, 2, [{:variable, :a, 1}, {:variable, :b, 1}]}
    end

    test "map" do
      assert type_of("%{}") == {:map, [], nil}
      assert type_of("%{asd: a}") == {:map, [{:asd, {:variable, :a, 1}}], nil}
      # NOTE non atom keys are not supported
      assert type_of("%{\"asd\" => a}") == {:map, [], nil}

      assert type_of("%{b | asd: a}") ==
               {:map, [{:asd, {:variable, :a, 1}}], {:variable, :b, 1}}

      assert type_of("%{b | asd: a}", :match) == :none
    end

    test "map with __struct__ key" do
      assert type_of("%{__struct__: Foo}") == {:struct, [], {:atom, Foo}, nil}

      assert type_of("%{__struct__: Foo, asd: a}") ==
               {:struct, [{:asd, {:variable, :a, 1}}], {:atom, Foo}, nil}

      assert type_of("%{b | __struct__: Foo, asd: a}") ==
               {:struct, [{:asd, {:variable, :a, 1}}], {:atom, Foo}, {:variable, :b, 1}}
    end

    test "struct" do
      assert type_of("%Foo{}") == {:struct, [], {:atom, Foo}, nil}
      assert type_of("%a{}") == {:struct, [], {:variable, :a, 1}, nil}
      assert type_of("%@a{}") == {:struct, [], {:attribute, :a}, nil}

      assert type_of("%Foo{asd: a}") ==
               {:struct, [{:asd, {:variable, :a, 1}}], {:atom, Foo}, nil}

      assert type_of("%Foo{b | asd: a}") ==
               {:struct, [{:asd, {:variable, :a, 1}}], {:atom, Foo}, {:variable, :b, 1}}

      assert type_of("%Foo{b | asd: a}", :match) == :none
    end

    test "range" do
      assert type_of("a..b") ==
               {:struct,
                [
                  {:first, {:variable, :a, 1}},
                  {:last, {:variable, :b, 1}},
                  {:step, {:integer, 1}}
                ], {:atom, Range}, nil}

      assert type_of("a..b//2") ==
               {:struct,
                [
                  {:first, {:variable, :a, 1}},
                  {:last, {:variable, :b, 1}},
                  {:step, {:integer, 2}}
                ], {:atom, Range}, nil}
    end

    test "sigil" do
      # NOTE we do not attempt to parse sigils
      assert type_of("~r//") == {:struct, [], {:atom, Regex}, nil}
      assert type_of("~R//") == {:struct, [], {:atom, Regex}, nil}
      assert type_of("~N//") == {:struct, [], {:atom, NaiveDateTime}, nil}
      assert type_of("~U//") == {:struct, [], {:atom, DateTime}, nil}
      assert type_of("~T//") == {:struct, [], {:atom, Time}, nil}
      assert type_of("~D//") == {:struct, [], {:atom, Date}, nil}
    end

    test "local call" do
      assert type_of("foo(a)") == {:local_call, :foo, [{:variable, :a, 1}]}
    end

    test "remote call" do
      assert type_of(":foo.bar(a)") == {:call, {:atom, :foo}, :bar, [{:variable, :a, 1}]}
    end

    test "match" do
      assert type_of("a = 5") == {:integer, 5}
      assert type_of("5 = a") == {:intersection, [{:integer, 5}, {:variable, :a, 1}]}
      assert type_of("b = 5 = a") == {:intersection, [{:integer, 5}, {:variable, :a, 1}]}
      assert type_of("5 = 5") == {:integer, 5}

      assert type_of("%{foo: a} = %{bar: b}") ==
               {:intersection, [{:map, [foo: nil], nil}, {:map, [bar: {:variable, :b, 1}], nil}]}

      assert type_of("%{foo: a} = %{bar: b}", :match) ==
               {:intersection, [{:map, [foo: nil], nil}, {:map, [bar: nil], nil}]}
    end

    test "other" do
      assert type_of("\"asd\"") == nil
      assert type_of("1.23") == nil
    end
  end
end
