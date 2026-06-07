defmodule ElixirSense.Core.TypeInferenceTest do
  use ExUnit.Case, async: false
  alias ElixirSense.Core.{ElixirTypes, TypeInference}

  setup do
    original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)

    on_exit(fn ->
      Application.put_env(:elixir_sense, :use_elixir_types, original_value)
    end)

    :ok
  end

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

    test "does not find variables in guard" do
      assert find_typed_vars_in("_ when is_integer(a)", nil, :match) == []
    end

    # TODO should it find variables in bitstring size specifiers guard?

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

    # TODO should it find vars in bitstring?

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

    test "finds variables in binary pattern" do
      assert find_typed_vars_in("<<a::binary, b::integer>>", nil, :match) == [
               {{:a, 1}, {:binary, nil}},
               {{:b, 1}, {:integer, nil}}
             ]

      assert find_typed_vars_in("<<x::utf8, rest::binary>>", nil, :match) == [
               {{:x, 1}, {:integer, nil}},
               {{:rest, 1}, {:binary, nil}}
             ]

      assert find_typed_vars_in("<<f::float>>", nil, :match) == [
               {{:f, 1}, {:float, nil}}
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

    defp with_elixir_types(enabled, fun) do
      original_value = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, enabled)

      try do
        fun.()
      after
        Application.put_env(:elixir_sense, :use_elixir_types, original_value)
      end
    end

    defp assert_old_and_native(code, old_expected, native_expected, context \\ nil) do
      assert with_elixir_types(false, fn -> type_of(code, context) end) == old_expected

      # `native_expected` describes the expected-type native backend (1.19+).
      # On 1.18 expression typing stays on the custom engine, so only the
      # disabled-mode (`old_expected`) assertion above applies.
      if ElixirTypes.available?(:expr) do
        assert with_elixir_types(true, fn -> type_of(code, context) end) == native_expected
      end
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
      # The element type is the union of every element's type.
      assert type_of("[a, b]") ==
               {:list, {:union, [{:variable, :a, 1}, {:variable, :b, 1}]}}

      # A pure cons `[a | b]` keeps only the head's type (the tail is a var).
      assert type_of("[a | b]") == {:list, {:variable, :a, 1}}

      # A trailing cons `[a, b | c]` contributes its head `b`; tail `c` is unknown.
      assert type_of("[a, b | c]") ==
               {:list, {:union, [{:variable, :a, 1}, {:variable, :b, 1}]}}
    end

    test "list operators" do
      assert_old_and_native(
        ":erlang.++([a], [b])",
        {:call, {:atom, :erlang}, :++, [list: {:variable, :a, 1}, list: {:variable, :b, 1}]},
        {:list, nil}
      )

      assert_old_and_native(
        ":erlang.--([a], [b])",
        {:call, {:atom, :erlang}, :--, [list: {:variable, :a, 1}, list: {:variable, :b, 1}]},
        {:list, nil}
      )
    end

    test "tuple" do
      assert type_of("{}") == {:tuple, 0, []}
      assert type_of("{a}") == {:tuple, 1, [{:variable, :a, 1}]}
      assert type_of("{a, b}") == {:tuple, 2, [{:variable, :a, 1}, {:variable, :b, 1}]}
    end

    test "map" do
      assert type_of("%{}") == {:map, [], nil}
      assert type_of("%{asd: a}") == {:map, [{:asd, {:variable, :a, 1}}], nil}
      # Non-atom keys are preserved as domain keys (`{:domain, key_type}`).
      assert type_of("%{\"asd\" => a}") ==
               {:map, [{{:domain, {:binary, "asd"}}, {:variable, :a, 1}}], nil}

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
      # Native typing returns nil for this unsupported local call, so the engine
      # falls back to the {:local_call, ...} shape Binding relies on.
      assert_old_and_native(
        "foo(a)",
        {:local_call, :foo, {1, 1}, [{:variable, :a, 1}]},
        {:local_call, :foo, {1, 1}, [{:variable, :a, 1}]}
      )
    end

    test "remote call" do
      # Likewise the {:call, ...} shape is preserved when native typing has no
      # signature for the remote call.
      assert_old_and_native(
        ":foo.bar(a)",
        {:call, {:atom, :foo}, :bar, [{:variable, :a, 1}]},
        {:call, {:atom, :foo}, :bar, [{:variable, :a, 1}]}
      )
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

    test "string literal" do
      assert_old_and_native("\"asd\"", {:binary, "asd"}, {:binary, "asd"})
    end

    test "float literal" do
      assert_old_and_native("1.23", {:float, 1.23}, {:float, 1.23})
    end

    test "binary expression" do
      assert_old_and_native("<<1, 2, 3>>", {:binary, nil}, {:binary, nil})
      assert_old_and_native("<<a::utf8>>", {:binary, nil}, {:binary, nil})
    end

    test "__DIR__ returns binary" do
      assert type_of("__DIR__", nil) == {:binary, nil}
    end

    test "__STACKTRACE__ returns list" do
      assert type_of("__STACKTRACE__", nil) == {:list, nil}
    end

    test "__CALLER__ returns Macro.Env struct" do
      assert type_of("__CALLER__", nil) == {:struct, [], {:atom, Macro.Env}, nil}
    end

    test "for comprehension without into" do
      assert_old_and_native("for x <- [1, 2], do: x", {:list, nil}, {:list, nil})
    end

    test "for comprehension with into map" do
      assert_old_and_native(
        "for x <- [1, 2], into: %{}, do: {x, x}",
        {:map, [], nil},
        {:map, [], nil}
      )
    end

    test "for comprehension with into string" do
      assert_old_and_native(
        ~s(for x <- ["a", "b"], into: "", do: x),
        {:binary, nil},
        {:binary, nil}
      )
    end

    test "__STACKTRACE__ returns {:list, nil}" do
      assert type_of("__STACKTRACE__") == {:list, nil}
    end

    test "anonymous function" do
      # Both modes now extract fn arity/arg info
      assert_old_and_native("fn -> a end", {:fun, 0}, {:fun, 0})
      assert_old_and_native("fn x -> x + 1 end", {:fun, [nil], nil}, {:fun, [nil], nil})

      assert_old_and_native(
        "fn x, y -> x * y end",
        {:fun, [nil, nil], nil},
        {:fun, [nil, nil], nil}
      )
    end
  end

  describe "block expressions" do
    test "non-empty block returns type of last expression" do
      assert type_of("(a = 1; b = 2; c = 3)") == {:integer, 3}

      assert type_of("""
               (
                 a = 1
                 b = 2
                 c = 3
               )
             """) == {:integer, 3}
    end

    test "empty block returns nil" do
      assert type_of("( )") == nil
    end

    test "__CALLER__ returns {:struct, [], {:atom, Macro.Env}, nil}" do
      assert type_of("__CALLER__") == {:struct, [], {:atom, Macro.Env}, nil}
    end

    test "if/unless result is the union of both branches (native-off)" do
      assert type_of("if a, do: :yes, else: :no") ==
               {:union, [{:atom, :yes}, {:atom, :no}]}

      assert type_of("unless a, do: :no, else: :yes") ==
               {:union, [{:atom, :no}, {:atom, :yes}]}

      # An `if` without `else` can return nil.
      assert type_of("if a, do: :yes") == {:union, [{:atom, :yes}, {:atom, nil}]}
    end

    test "anonymous functions render by arity (native-off fallback)" do
      assert type_of("fn x -> x + 1 end") == {:fun, [nil], nil}
      assert type_of("fn -> :ok end") == {:fun, 0}
    end

    test "with without else unions the do body and each <- failure value" do
      assert type_of("with {:ok, v} <- a, do: :done") ==
               {:union, [{:atom, :done}, {:variable, :a, 1}]}
    end

    test "a clause body that is just a head-bound var widens to nil (no leak)" do
      # `v` is out of scope at the case result site, so it must not leak.
      assert type_of("case a do\n  {:ok, v} -> v\n  _ -> :err\nend") == nil
    end

    test "a structured clause body keeps its shape" do
      assert type_of("case a do\n  1 -> {:wrapped, :a}\n  2 -> {:wrapped, :b}\nend") ==
               {:union,
                [
                  {:tuple, 2, [{:atom, :wrapped}, {:atom, :a}]},
                  {:tuple, 2, [{:atom, :wrapped}, {:atom, :b}]}
                ]}
    end

    test "bitstring vs binary: sub-byte segments are bitstrings" do
      assert type_of("<<1::1>>") == :bitstring
      assert type_of("<<x::bitstring>>") == :bitstring
      assert type_of("<<x::integer-size(4)>>") == :bitstring
      assert type_of("<<1, 2, 3>>") == {:binary, nil}
      assert type_of("<<x::binary-size(4)>>") == {:binary, nil}
      assert type_of("<<x::utf8>>") == {:binary, nil}
    end
  end

  describe "special forms" do
    special_forms = [
      "case a do\n  :ok -> 1\n  :error -> 2\nend",
      "cond do\n  a -> 1\n  b -> 2\nend",
      "try do\n  risky_operation()\nrescue\n  e -> handle(e)\nend",
      "receive do\n  {:msg, msg} -> process(msg)\nend",
      "for x <- list, do: x * 2",
      "with {:ok, a} <- fetch_a(), {:ok, b} <- fetch_b(a), do: a + b",
      "quote do: a + b",
      "unquote(expr)",
      "unquote_splicing(expr)",
      "import Module",
      "alias Module.SubModule",
      "require Module"
    ]

    # Native (1.19+) types `case`/`cond` precisely; for `try`/`receive`/`with`
    # over undefined calls native returns nil, so the conservative branch-result
    # union (same as native-off) applies.
    native_expectations = %{
      "case a do\n  :ok -> 1\n  :error -> 2\nend" => {:integer, nil},
      "cond do\n  a -> 1\n  b -> 2\nend" => {:integer, nil},
      "try do\n  risky_operation()\nrescue\n  e -> handle(e)\nend" =>
        {:union,
         [
           {:local_call, :risky_operation, {2, 1}, []},
           {:local_call, :handle, {4, 1}, [{:variable, :e, 1}]}
         ]},
      "receive do\n  {:msg, msg} -> process(msg)\nend" =>
        {:local_call, :process, {2, 1}, [{:variable, :msg, 1}]},
      "for x <- list, do: x * 2" => {:list, nil},
      # no `else`: result is the `do` body unioned with each `<-` failure value
      "with {:ok, a} <- fetch_a(), {:ok, b} <- fetch_b(a), do: a + b" =>
        {:union,
         [
           {:local_call, :+, {1, 1}, [{:variable, :a, 1}, {:variable, :b, 1}]},
           {:local_call, :fetch_a, {1, 1}, []},
           {:local_call, :fetch_b, {1, 1}, [{:variable, :a, 1}]}
         ]},
      "quote do: a + b" => nil,
      "unquote(expr)" => nil,
      "unquote_splicing(expr)" => nil,
      "import Module" => nil,
      "alias Module.SubModule" => nil,
      "require Module" => nil
    }

    # Native-off (legacy / 1.18): clause constructs now return a conservative
    # union of their branch result types instead of nil.
    old_expectations = %{
      "case a do\n  :ok -> 1\n  :error -> 2\nend" => {:union, [{:integer, 1}, {:integer, 2}]},
      "cond do\n  a -> 1\n  b -> 2\nend" => {:union, [{:integer, 1}, {:integer, 2}]},
      "try do\n  risky_operation()\nrescue\n  e -> handle(e)\nend" =>
        {:union,
         [
           {:local_call, :risky_operation, {2, 1}, []},
           {:local_call, :handle, {4, 1}, [{:variable, :e, 1}]}
         ]},
      "receive do\n  {:msg, msg} -> process(msg)\nend" =>
        {:local_call, :process, {2, 1}, [{:variable, :msg, 1}]},
      # no `else`: result is the `do` body unioned with each `<-` failure value
      "with {:ok, a} <- fetch_a(), {:ok, b} <- fetch_b(a), do: a + b" =>
        {:union,
         [
           {:local_call, :+, {1, 1}, [{:variable, :a, 1}, {:variable, :b, 1}]},
           {:local_call, :fetch_a, {1, 1}, []},
           {:local_call, :fetch_b, {1, 1}, [{:variable, :a, 1}]}
         ]},
      "for x <- list, do: x * 2" => {:list, nil}
    }

    for form <- special_forms do
      test "special form: #{inspect(form)} matches legacy and native mode" do
        old_expected = Map.get(unquote(Macro.escape(old_expectations)), unquote(form))

        assert_old_and_native(
          unquote(form),
          old_expected,
          Map.fetch!(unquote(Macro.escape(native_expectations)), unquote(form))
        )
      end
    end
  end

  describe "type_of/4 with compiler context" do
    alias ElixirSense.Core.State.VarInfo

    defp type_of_with_context(code, vars_info, env \\ %{}) do
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

      state = %{vars_info: [vars_info]}

      env =
        Map.merge(%{module: TestModule, function: {:test, 0}, file: "nofile", context: nil}, env)

      TypeInference.type_of(ast, env[:context], state, env)
    end

    test "type_of/4 falls back to type_of/2 when native typing disabled" do
      original = Application.get_env(:elixir_sense, :use_elixir_types, false)
      Application.put_env(:elixir_sense, :use_elixir_types, false)

      try do
        result =
          type_of_with_context("x", %{
            {:x, 1} => %VarInfo{name: :x, version: 1, type: {:integer, 1}}
          })

        assert result == {:variable, :x, 1}
      after
        Application.put_env(:elixir_sense, :use_elixir_types, original)
      end
    end

    test "type_of/4 with variable context produces native result when enabled" do
      # Native expression typing requires the expected-type backend (1.19+); on
      # 1.18 type_of/4 stays on the custom engine.
      if ElixirTypes.available?(:expr) do
        original = Application.get_env(:elixir_sense, :use_elixir_types, false)
        Application.put_env(:elixir_sense, :use_elixir_types, true)

        try do
          # With an integer variable in scope, native typing should type "x + 1" as integer
          vars = %{
            {:x, 1} => %VarInfo{
              name: :x,
              version: 1,
              type: {:integer, nil},
              elixir_types_descr: Module.Types.Descr.integer()
            }
          }

          result = type_of_with_context(":erlang.+(x, 1)", vars)
          assert result in [{:integer, nil}, :number]
        after
          Application.put_env(:elixir_sense, :use_elixir_types, original)
        end
      end
    end

    test "type_of/4 handles empty vars_info gracefully" do
      result = type_of_with_context(":ok", %{})
      assert result == {:atom, :ok}
    end
  end
end
