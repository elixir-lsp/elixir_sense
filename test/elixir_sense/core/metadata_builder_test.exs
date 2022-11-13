defmodule ElixirSense.Core.MetadataBuilderTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Core.MetadataBuilder
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State
  alias ElixirSense.Core.State.{VarInfo, CallInfo, StructInfo, ModFunInfo, AttributeInfo}

  @tag requires_source: true
  test "build metadata from kernel.ex" do
    assert get_subject_definition_line(Kernel, :defmodule, nil) =~
             "defmacro defmodule(alias, do_block)"
  end

  @tag requires_source: true
  test "build metadata from kernel/special_forms.ex" do
    assert get_subject_definition_line(Kernel.SpecialForms, :alias, nil) =~
             "defmacro alias(module, opts)"
  end

  test "build_metadata from a module" do
    assert get_subject_definition_line(
             ElixirSenseExample.ModuleWithFunctions,
             :function_arity_zero,
             nil
           ) =~ "def function_arity_zero"
  end

  test "moduledoc heredoc version" do
    state =
      """
      defmodule Outer do
        @moduledoc \"\"\"
        This is the here doc version
        \"\"\"
        defmodule Inner do
          @moduledoc \"\"\"
          This is the Inner modules moduledoc
          \"\"\"
        end
      end
      """
      |> string_to_state

    assert %{Outer => {5, 3}, Outer.Inner => {9, 5}} = state.moduledoc_positions
  end

  test "moduledoc boolean version" do
    state =
      """
      defmodule Outer do
        @moduledoc false
      end
      """
      |> string_to_state

    assert %{Outer => {3, 3}} = state.moduledoc_positions
  end

  test "module attributes" do
    state =
      """
      defmodule MyModule do
        @myattribute String
        IO.puts @myattribute
        defmodule InnerModule do
          @inner_attr %{abc: nil}
          @inner_attr_1 __MODULE__
          IO.puts @inner_attr
        end
        IO.puts ""
        @otherattribute Application.get_env(:elixir_sense, :some_attribute, InnerModule)
      end
      """
      |> string_to_state

    assert get_line_attributes(state, 10) == [
             %ElixirSense.Core.State.AttributeInfo{
               name: :myattribute,
               positions: [{2, 3}, {3, 11}],
               type: {:atom, String}
             },
             %AttributeInfo{
               name: :otherattribute,
               positions: [{10, 3}],
               type:
                 {:call, {:atom, Application}, :get_env,
                  [atom: :elixir_sense, atom: :some_attribute, atom: MyModule.InnerModule]}
             }
           ]

    assert get_line_attributes(state, 3) == [
             %AttributeInfo{
               name: :myattribute,
               positions: [{2, 3}, {3, 11}],
               type: {:atom, String}
             }
           ]

    assert get_line_attributes(state, 7) == [
             %AttributeInfo{
               name: :inner_attr,
               positions: [{5, 5}, {7, 13}],
               type: {:map, [abc: {:atom, nil}], nil}
             },
             %AttributeInfo{
               name: :inner_attr_1,
               positions: [{6, 5}],
               type: {:atom, MyModule.InnerModule}
             }
           ]

    assert get_line_attributes(state, 9) == [
             %AttributeInfo{
               name: :myattribute,
               positions: [{2, 3}, {3, 11}],
               type: {:atom, String}
             }
           ]
  end

  test "module attributes rebinding" do
    state =
      """
      defmodule MyModule do
        @myattribute String
        @myattribute List
        @myattribute
        IO.puts ""
        def a do
          @myattribute
        end
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_attributes(state, 5) == [
             %AttributeInfo{
               name: :myattribute,
               positions: [{2, 3}, {3, 3}, {4, 3}],
               type: {:atom, List}
             }
           ]

    assert get_line_attributes(state, 9) == [
             %AttributeInfo{
               name: :myattribute,
               positions: [{2, 3}, {3, 3}, {4, 3}, {7, 5}],
               type: {:atom, List}
             }
           ]
  end

  test "module attributes value binding" do
    state =
      """
      defmodule MyModule do
        @myattribute %{abc: String}
        @some_attr @myattribute
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_attributes(state, 4) == [
             %AttributeInfo{
               name: :myattribute,
               positions: [{2, 3}, {3, 14}],
               type: {:map, [abc: {:atom, String}], nil}
             },
             %AttributeInfo{
               name: :some_attr,
               positions: [{3, 3}],
               type: {:attribute, :myattribute}
             }
           ]
  end

  test "module attributes value binding to and from variables" do
    state =
      """
      defmodule MyModule do
        @myattribute %{abc: String}
        var = @myattribute
        @other var
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_attributes(state, 5) == [
             %AttributeInfo{
               name: :myattribute,
               positions: [{2, 3}, {3, 9}],
               type: {:map, [abc: {:atom, String}], nil}
             },
             %AttributeInfo{
               name: :other,
               positions: [{4, 3}],
               type: {:variable, :var}
             }
           ]

    assert [
             %VarInfo{name: :var, type: {:attribute, :myattribute}}
           ] = state |> get_line_vars(5)
  end

  test "tuple destructuring" do
    state =
      """
      defmodule MyModule do
        @myattribute {:ok, %{abc: nil}}
        {:ok, var} = @myattribute
        other = elem(@myattribute, 0)
        IO.puts
        q = {:a, :b, :c}
        {_, _, q1} = q
        IO.puts
      end
      """
      |> string_to_state

    assert get_line_attributes(state, 4) == [
             %AttributeInfo{
               name: :myattribute,
               positions: [{2, 3}, {3, 16}, {4, 16}],
               type: {:tuple, 2, [{:atom, :ok}, {:map, [abc: {:atom, nil}], nil}]}
             }
           ]

    assert [
             %VarInfo{
               name: :other,
               type: {:local_call, :elem, [{:attribute, :myattribute}, {:integer, 0}]}
             },
             %VarInfo{
               name: :var,
               type:
                 {:tuple_nth,
                  {:intersection, [{:attribute, :myattribute}, {:tuple, 2, [{:atom, :ok}, nil]}]},
                  1}
             }
           ] = state |> get_line_vars(4)

    assert [
             %VarInfo{
               name: :q,
               type: {:tuple, 3, [{:atom, :a}, {:atom, :b}, {:atom, :c}]}
             },
             %VarInfo{
               name: :q1,
               type:
                 {:tuple_nth,
                  {:intersection,
                   [{:variable, :q}, {:tuple, 3, [{:variable, :_}, {:variable, :_}, nil]}]}, 2}
             }
           ] =
             state
             |> get_line_vars(8)
             |> Enum.filter(&(&1.name |> Atom.to_string() |> String.starts_with?("q")))
  end

  test "list destructuring" do
    state =
      """
      defmodule MyModule do
        @a []
        @myattribute [:ok, :error, :other]
        @other1 [:some, :error | @myattribute]
        @other2 [:some | @myattribute]
        [var, _var1, _var2] = @myattribute
        [other | rest] = @myattribute
        [a] = @other
        [b] = []
        IO.puts
      end
      """
      |> string_to_state

    assert get_line_attributes(state, 5) == [
             %AttributeInfo{
               name: :a,
               positions: [{2, 3}],
               type: {:list, :empty}
             },
             %AttributeInfo{
               name: :myattribute,
               positions: [{3, 3}, {4, 28}, {5, 20}],
               type: {:list, {:atom, :ok}}
             },
             %AttributeInfo{name: :other1, positions: [{4, 3}], type: {:list, {:atom, :some}}},
             %AttributeInfo{name: :other2, positions: [{5, 3}], type: {:list, {:atom, :some}}}
           ]

    assert [
             %VarInfo{
               name: :a,
               type: {:list_head, {:attribute, :other}}
             },
             %VarInfo{
               name: :b,
               type: {:list_head, {:list, :empty}}
             },
             %VarInfo{name: :other, type: {:list_head, {:attribute, :myattribute}}},
             %VarInfo{name: :rest, type: {:list_tail, {:attribute, :myattribute}}},
             %VarInfo{name: :var, type: {:list_head, {:attribute, :myattribute}}}
           ] = state |> get_line_vars(10)
  end

  test "list destructuring for" do
    state =
      """
      defmodule MyModule do
        @myattribute [:ok, :error, :other]
        for a <- @myattribute do
          b = a
          IO.puts
        end

        for a <- @myattribute, a1 = @myattribute, a2 <- a1 do
          b = a
          IO.puts
        end
      end
      """
      |> string_to_state

    assert [
             %VarInfo{name: :a, type: {:list_head, {:attribute, :myattribute}}},
             %VarInfo{name: :b, type: {:variable, :a}}
           ] = state |> get_line_vars(5)

    assert [
             %VarInfo{name: :a, type: {:list_head, {:attribute, :myattribute}}},
             %VarInfo{name: :a1, type: {:attribute, :myattribute}},
             %VarInfo{name: :a2, type: {:list_head, {:variable, :a1}}},
             %VarInfo{name: :b, type: {:variable, :a}}
           ] = state |> get_line_vars(10)
  end

  test "binding in with expression" do
    state =
      """
      defmodule MyModule do
        @myattribute [:ok, :error, :other]
        with a <- @myattribute do
          b = a
          IO.puts
        end
      end
      """
      |> string_to_state

    assert [
             %VarInfo{name: :a, type: {:attribute, :myattribute}},
             %VarInfo{name: :b, type: {:variable, :a}}
           ] = state |> get_line_vars(5)
  end

  test "vars defined inside a function without params" do
    state =
      """
      defmodule MyModule do
        var_out1 = 1
        def func do
          var_in1 = 1
          var_in2 = 1
          IO.puts ""
        end
        var_out2 = 1
        IO.puts ""
      end
      """
      |> string_to_state

    assert [
             %VarInfo{name: :var_in1, positions: [{4, 5}], scope_id: 3},
             %VarInfo{name: :var_in2, positions: [{5, 5}], scope_id: 3}
           ] = state |> get_line_vars(6)
  end

  test "vars binding" do
    state =
      """
      defmodule MyModule do
        def func do
          var = String
          IO.puts ""
          var = Map
          IO.puts ""
          if abc do
            IO.puts ""
            var = List
            IO.puts ""
            var = Enum
            IO.puts ""
          end
          IO.puts ""
          var = Atom
          IO.puts ""
          other = var
          IO.puts ""
        end
      end
      """
      |> string_to_state

    assert [%VarInfo{type: {:atom, String}}] = state |> get_line_vars(4)
    assert [%VarInfo{type: {:atom, Map}}] = state |> get_line_vars(6)
    assert [%VarInfo{type: {:atom, Map}}] = state |> get_line_vars(8)
    assert [%VarInfo{type: {:atom, List}}] = state |> get_line_vars(10)
    assert [%VarInfo{type: {:atom, Enum}}] = state |> get_line_vars(12)
    assert [%VarInfo{type: {:atom, Map}}] = state |> get_line_vars(14)
    assert [%VarInfo{type: {:atom, Atom}}] = state |> get_line_vars(16)

    assert [
             %VarInfo{name: :other, type: {:variable, :var}},
             %VarInfo{name: :var, type: {:atom, Atom}}
           ] = state |> get_line_vars(18)
  end

  test "variables are added to environment" do
    state =
      """
      defmodule MyModule do
        def func do
          var = :my_var
        end
      end
      """
      |> string_to_state

    assert [%VarInfo{type: {:atom, :my_var}}] = state |> get_line_vars(3)
  end

  test "call binding" do
    state =
      """
      defmodule MyModule do
        def remote_calls do
          var1 = DateTime.now
          var2 = :erlang.now()
          var3 = __MODULE__.now(:abc)
          var4 = "Etc/UTC" |> DateTime.now
          IO.puts ""
        end

        def local_calls do
          var1 = now
          var2 = now()
          var3 = now(:abc)
          var4 = :abc |> now
          var5 = :abc |> now(5)
          IO.puts ""
        end

        @attr %{qwe: String}
        def map_field(var1) do
          var1 = var1.abc
          var2 = @attr.qwe(0)
          var3 = abc.cde.efg
          IO.puts ""
        end
      end
      """
      |> string_to_state

    assert [
             %VarInfo{name: :var1, type: {:call, {:atom, DateTime}, :now, []}},
             %VarInfo{name: :var2, type: {:call, {:atom, :erlang}, :now, []}},
             %VarInfo{name: :var3, type: {:call, {:atom, MyModule}, :now, [{:atom, :abc}]}},
             %VarInfo{name: :var4, type: {:call, {:atom, DateTime}, :now, [nil]}}
           ] = state |> get_line_vars(7)

    assert [
             %VarInfo{name: :var1, type: {:variable, :now}},
             %VarInfo{name: :var2, type: {:local_call, :now, []}},
             %VarInfo{name: :var3, type: {:local_call, :now, [{:atom, :abc}]}},
             %VarInfo{name: :var4, type: {:local_call, :now, [{:atom, :abc}]}},
             %VarInfo{name: :var5, type: {:local_call, :now, [{:atom, :abc}, {:integer, 5}]}}
           ] = state |> get_line_vars(16)

    assert [
             %VarInfo{name: :var1, type: {:call, {:variable, :var1}, :abc, []}},
             %VarInfo{name: :var2, type: {:call, {:attribute, :attr}, :qwe, [{:integer, 0}]}},
             %VarInfo{name: :var3, type: {:call, {:call, {:variable, :abc}, :cde, []}, :efg, []}}
           ] = state |> get_line_vars(24)
  end

  test "map binding" do
    state =
      """
      defmodule MyModule do
        def func do
          var = %{asd: 5}
          IO.puts ""
          var = %{asd: 5, nested: %{wer: "asd"}}
          IO.puts ""
          var = %{"asd" => "dsds"}
          IO.puts ""
          var = %{asd: 5, zxc: String}
          IO.puts ""
          qwe = %{var | asd: 2, zxc: 5}
          IO.puts ""
          qwe = %{var | asd: 2}
          IO.puts ""

        end
      end
      """
      |> string_to_state

    assert [%VarInfo{type: {:map, [asd: {:integer, 5}], nil}}] = state |> get_line_vars(4)

    assert [%VarInfo{type: {:map, [asd: {:integer, 5}, nested: {:map, [wer: nil], nil}], nil}}] =
             state |> get_line_vars(6)

    assert [%VarInfo{type: {:map, [], nil}}] = state |> get_line_vars(8)

    assert [%VarInfo{type: {:map, [asd: {:integer, 5}, zxc: {:atom, String}], nil}}] =
             state |> get_line_vars(10)

    assert %VarInfo{type: {:map, [asd: {:integer, 2}, zxc: {:integer, 5}], {:variable, :var}}} =
             state |> get_line_vars(12) |> Enum.find(&(&1.name == :qwe))

    assert %VarInfo{type: {:map, [{:asd, {:integer, 2}}], {:variable, :var}}} =
             state |> get_line_vars(14) |> Enum.find(&(&1.name == :qwe))
  end

  test "struct binding" do
    state =
      """
      defmodule MyModule do
        def func(%MyStruct{} = var1, var2 = %:other_struct{}, var3 = %__MODULE__{},
          var4 = %__MODULE__.Sub{}, var7 = %_{}) do
          IO.puts ""
        end

        def some(a) do
          asd = %Some{sub: Atom}
          IO.puts ""
          asd = %Other{a | sub: Atom}
          IO.puts ""
          asd = %{asd | other: 123}
          IO.puts ""
          z = x = asd
          IO.puts ""
        end
      end
      """
      |> string_to_state

    assert [
             %VarInfo{name: :var1, type: {:struct, [], {:atom, MyStruct}, nil}},
             %VarInfo{name: :var2, type: {:struct, [], {:atom, :other_struct}, nil}},
             %VarInfo{name: :var3, type: {:struct, [], {:atom, MyModule}, nil}},
             %VarInfo{name: :var4, type: {:struct, [], {:atom, MyModule.Sub}, nil}},
             %VarInfo{name: :var7, type: {:struct, [], nil, nil}}
           ] = state |> get_line_vars(4)

    assert %VarInfo{name: :asd, type: {:struct, [{:sub, {:atom, Atom}}], {:atom, Some}, nil}} =
             state |> get_line_vars(9) |> Enum.find(&(&1.name == :asd))

    assert %VarInfo{
             name: :asd,
             type: {:struct, [{:sub, {:atom, Atom}}], {:atom, Other}, {:variable, :a}}
           } = state |> get_line_vars(11) |> Enum.find(&(&1.name == :asd))

    assert %VarInfo{name: :asd, type: {:map, [{:other, {:integer, 123}}], {:variable, :asd}}} =
             state |> get_line_vars(13) |> Enum.find(&(&1.name == :asd))

    assert [
             %VarInfo{name: :x, type: {:intersection, [{:variable, :z}, {:variable, :asd}]}},
             %VarInfo{name: :z, type: {:variable, :asd}}
           ] = state |> get_line_vars(15) |> Enum.filter(&(&1.name in [:x, :z]))
  end

  test "struct binding understands builtin sigils and ranges" do
    state =
      """
      defmodule MyModule do
        def some() do
          var1 = ~D[2000-01-01]
          var2 = ~T[13:00:07]
          var3 = ~U[2015-01-13 13:00:07Z]
          var4 = ~N[2000-01-01 23:00:07]
          var5 = ~r/foo/iu
          var6 = ~R(f\#{1,3}o)
          var7 = 12..34
          IO.puts ""
        end
      end
      """
      |> string_to_state

    assert [
             %VarInfo{name: :var1, type: {:struct, [], {:atom, Date}}},
             %VarInfo{name: :var2, type: {:struct, [], {:atom, Time}}},
             %VarInfo{name: :var3, type: {:struct, [], {:atom, DateTime}}},
             %VarInfo{name: :var4, type: {:struct, [], {:atom, NaiveDateTime}}},
             %VarInfo{name: :var5, type: {:struct, [], {:atom, Regex}}},
             %VarInfo{name: :var6, type: {:struct, [], {:atom, Regex}}},
             %VarInfo{name: :var7, type: {:struct, [], {:atom, Range}}}
           ] = state |> get_line_vars(10)
  end

  @tag requires_elixir_1_12: true
  test "struct binding understands stepped ranges" do
    state =
      """
      defmodule MyModule do
        def some() do
          var1 = 12..34//2
          IO.puts ""
        end
      end
      """
      |> string_to_state

    assert [
             %VarInfo{name: :var1, type: {:struct, [], {:atom, Range}}}
           ] = state |> get_line_vars(4)
  end

  test "nested `=` binding" do
    state =
      """
      defmodule MyModule do
        def some() do
          %State{formatted: formatted} = state = socket.assigns.state
          IO.puts ""
        end
      end
      """
      |> string_to_state

    # FIXME formatted type should be {:call, {:call, {:call, {:variable, :socket}, :assigns, []}, :state, []}, :formatted, []}
    # needs support for map/struct destructuring
    assert [
             %VarInfo{
               name: :formatted,
               type:
                 {:tuple_nth,
                  {:intersection,
                   [
                     {:call, {:call, {:variable, :socket}, :assigns, []}, :state, []},
                     {:tuple, 2, [{:atom, :formatted}, nil]}
                   ]}, 1}
             },
             %VarInfo{
               name: :state,
               type:
                 {:intersection,
                  [
                    {:struct, [formatted: {:variable, :formatted}], {:atom, Elixir.State}, nil},
                    {:call, {:call, {:variable, :socket}, :assigns, []}, :state, []}
                  ]}
             }
           ] = state |> get_line_vars(4)
  end

  test "case binding" do
    state =
      """
      defmodule MyModule do
        def some() do
          case Some.call() do
            {:ok, x} ->
              IO.puts ""
          end
        end
      end
      """
      |> string_to_state

    assert [
             %VarInfo{
               name: :x,
               type:
                 {:tuple_nth,
                  {:intersection,
                   [{:call, {:atom, Some}, :call, []}, {:tuple, 2, [{:atom, :ok}, nil]}]}, 1}
             }
           ] = state |> get_line_vars(5)
  end

  test "rescue binding" do
    state =
      """
      defmodule MyModule do
        def some() do
          try do
            Some.call()
          rescue
            e0 in ArgumentError ->
              :ok
            e1 in [ArgumentError] ->
              :ok
            e2 in [RuntimeError, Enum.EmptyError] ->
              :ok
            e3 ->
              :ok
          else
            a ->
              :ok
          end
        end
      end
      """
      |> string_to_state

    assert [
             %VarInfo{
               name: :e0,
               type: {:struct, [], {:atom, ArgumentError}, nil}
             }
           ] = state |> get_line_vars(6)

    assert [
             %VarInfo{
               name: :e1,
               type: {:struct, [], {:atom, ArgumentError}, nil}
             }
           ] = state |> get_line_vars(8)

    assert [
             %VarInfo{
               name: :e2,
               type: {:struct, [], {:atom, Exception}, nil}
             }
           ] = state |> get_line_vars(10)

    assert [
             %VarInfo{
               name: :e3,
               type: {:struct, [], {:atom, Exception}, nil}
             }
           ] = state |> get_line_vars(12)

    assert [
             %VarInfo{
               name: :a,
               type: nil
             }
           ] = state |> get_line_vars(15)
  end

  test "vars defined inside a function `after`/`rescue`/`catch`" do
    state =
      """
      defmodule MyModule do
        var_out1 = 1
        def func(var_arg) do
          var_in1 = 1
          var_in2 = 1
          IO.puts ""
        after
          var_after = 1
          IO.puts ""
        end
      end
      """
      |> string_to_state

    assert [
             %VarInfo{name: :var_arg, positions: [{3, 12}], scope_id: 2},
             %VarInfo{name: :var_in1, positions: [{4, 5}], scope_id: 3},
             %VarInfo{name: :var_in2, positions: [{5, 5}], scope_id: 3}
           ] = state |> get_line_vars(6)

    assert [
             %VarInfo{name: :var_after, positions: [{8, 5}], scope_id: 4},
             %VarInfo{name: :var_arg, positions: [{3, 12}], scope_id: 2}
           ] = state |> get_line_vars(9)
  end

  test "vars defined inside a function with params" do
    state =
      """
      defmodule MyModule do
        var_out1 = 1
        def func(%{key1: par1, key2: [par2|[par3, _]]}, par4, _par5) do
          var_in1 = 1
          var_in2 = 1
          IO.puts ""
        end
        defp func1(arg), do: arg + 1
        var_out2 = 1
      end
      """
      |> string_to_state

    assert [
             %VarInfo{name: :par1, positions: [{3, 20}], scope_id: 2},
             %VarInfo{name: :par2, positions: [{3, 33}], scope_id: 2},
             %VarInfo{name: :par3, positions: [{3, 39}], scope_id: 2},
             %VarInfo{name: :par4, positions: [{3, 51}], scope_id: 2},
             %VarInfo{name: :var_in1, positions: [{4, 5}], scope_id: 3},
             %VarInfo{name: :var_in2, positions: [{5, 5}], scope_id: 3}
           ] = state |> get_line_vars(6)

    assert [
             %VarInfo{name: :arg, positions: [{8, 14}], scope_id: 2}
           ] = state |> get_line_vars(8)
  end

  test "guards do not define vars" do
    state =
      """
      defmodule MyModule do
        def func1(a) when is_integer(b) do
          IO.puts("")
        end
        def func2(a) when is_integer(b) or is_list(c) do
          IO.puts("")
        end
        def func3(a) when is_integer(b) when is_list(c) do
          IO.puts("")
        end

        case x do
          y when is_integer(z) ->
            IO.puts("")
        end

        with x when is_integer(y) <- z do
          IO.puts("")
        end

        def func3(a) when is_integer(b)
      end
      """
      |> string_to_state

    assert [%VarInfo{name: :a, positions: [{2, 13}], scope_id: 2}] = state |> get_line_vars(3)
    assert [%VarInfo{name: :a, positions: [{5, 13}], scope_id: 2}] = state |> get_line_vars(6)
    assert [%VarInfo{name: :a, positions: [{8, 13}], scope_id: 2}] = state |> get_line_vars(9)
    assert [%VarInfo{name: :y, positions: [{13, 5}], scope_id: 7}] = state |> get_line_vars(14)
    assert [%VarInfo{name: :x, positions: [{17, 8}], scope_id: 8}] = state |> get_line_vars(18)
    assert [%VarInfo{name: :a, positions: [{21, 13}], scope_id: 2}] = state |> get_line_vars(21)
  end

  test "rebinding vars" do
    state =
      """
      defmodule MyModule do
        var1 = 1
        def func(%{var: var1, key: [_|[_, var1]]}) do
          var1 = 1
          var1 = 2
          IO.puts ""
        end
        var1 = 1
      end
      """
      |> string_to_state

    vars = state |> get_line_vars(6)

    assert [
             %VarInfo{name: :var1, positions: [{3, 19}, {3, 37}, {4, 5}, {5, 5}], scope_id: 3}
           ] = vars
  end

  test "vars defined inside a module" do
    state =
      """
      defmodule MyModule do
        var_out1 = 1
        def func do
          var_in = 1
        end
        var_out2 = 1
        IO.puts ""
      end
      """
      |> string_to_state

    assert [
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
             %VarInfo{name: :var_out2, positions: [{6, 3}], scope_id: 2}
           ] = state |> get_line_vars(7)
  end

  test "vars defined in a `for` comprehension" do
    state =
      """
      defmodule MyModule do
        var_out1 = 1
        IO.puts ""
        for var_on <- [1,2], var_on != 2, var_on1 = var_on + 1 do
          var_in = 1
          IO.puts ""
        end
        var_out2 = 1
        IO.puts ""
      end
      """
      |> string_to_state

    assert [
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 3)

    assert [
             %VarInfo{is_definition: true, name: :var_in, positions: [{5, 5}], scope_id: 4},
             %VarInfo{
               is_definition: true,
               name: :var_on,
               positions: [{4, 7}, {4, 24}, {4, 47}],
               scope_id: 3
             },
             %VarInfo{is_definition: true, name: :var_on1, positions: [{4, 37}], scope_id: 3},
             %VarInfo{is_definition: true, name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 6)

    assert [
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
             %VarInfo{name: :var_out2, positions: [{8, 3}], scope_id: 2}
           ] = get_line_vars(state, 9)
  end

  test "vars defined in a `with` expression" do
    state =
      """
      defmodule MyModule do
        var_out1 = 1
        IO.puts ""
        with var_on <- [1,2], var_on != 2, var_on1 = var_on + 1 do
          var_in = 1
          IO.puts ""
        end
        var_out2 = 1
        IO.puts ""
      end
      """
      |> string_to_state

    assert [
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 3)

    assert [
             %VarInfo{is_definition: true, name: :var_in, positions: [{5, 5}], scope_id: 4},
             %VarInfo{
               is_definition: true,
               name: :var_on,
               positions: [{4, 8}, {4, 25}, {4, 48}],
               scope_id: 3
             },
             %VarInfo{is_definition: true, name: :var_on1, positions: [{4, 38}], scope_id: 3},
             %VarInfo{is_definition: true, name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 6)

    assert [
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
             %VarInfo{name: :var_out2, positions: [{8, 3}], scope_id: 2}
           ] = get_line_vars(state, 9)
  end

  test "vars defined in a `if/else` expression" do
    state =
      """
      defmodule MyModule do
        var_out1 = 1
        if var_on = true do
          var_in_if = 1
          IO.puts ""
        else
          var_in_else = 1
          IO.puts ""
        end
        var_out2 = 1
        IO.puts ""
      end
      """
      |> string_to_state

    assert [
             %VarInfo{name: :var_in_if, positions: [{4, 5}], scope_id: 3},
             %VarInfo{name: :var_on, positions: [{3, 6}], scope_id: 2},
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 5)

    assert [
             %VarInfo{name: :var_in_else, positions: [{7, 5}], scope_id: 4},
             %VarInfo{name: :var_on, positions: [{3, 6}], scope_id: 2},
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 8)

    assert [
             %VarInfo{name: :var_on, positions: [{3, 6}], scope_id: 2},
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
             %VarInfo{name: :var_out2, positions: [{10, 3}], scope_id: 2}
           ] = get_line_vars(state, 11)
  end

  test "vars defined inside a `fn`" do
    state =
      """
      defmodule MyModule do
        var_out1 = 1
        fn var_on ->
          var_in = 1
          IO.puts ""
        end
        var_out2 = 1
        IO.puts ""
      end
      """
      |> string_to_state

    assert [
             %VarInfo{is_definition: true, name: :var_in, positions: [{4, 5}], scope_id: 4},
             %VarInfo{is_definition: true, name: :var_on, positions: [{3, 6}], scope_id: 4},
             %VarInfo{is_definition: true, name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 5)

    assert [
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
             %VarInfo{name: :var_out2, positions: [{7, 3}], scope_id: 2}
           ] = get_line_vars(state, 8)
  end

  test "vars defined inside a `case`" do
    state =
      """
      defmodule MyModule do
        var_out1 = 1
        case var_on0 = var_out1 do
          {var_on1} ->
            var_in1 = 1
            IO.puts ""
          {var_on2} ->
            var_in2 = 2
            IO.puts ""
          var_on3 -> IO.puts ""
        end
        var_out2 = 1
        IO.puts ""
      end
      """
      |> string_to_state

    assert [
             %VarInfo{is_definition: true, name: :var_in1, positions: [{5, 7}], scope_id: 4},
             %VarInfo{is_definition: true, name: :var_on0, positions: [{3, 8}], scope_id: 2},
             %VarInfo{is_definition: true, name: :var_on1, positions: [{4, 6}], scope_id: 4},
             %VarInfo{
               is_definition: true,
               name: :var_out1,
               positions: [{2, 3}, {3, 18}],
               scope_id: 2
             }
           ] = get_line_vars(state, 6)

    assert [
             %VarInfo{name: :var_in2, positions: [{8, 7}], scope_id: 5},
             %VarInfo{name: :var_on0, positions: [{3, 8}], scope_id: 2},
             %VarInfo{name: :var_on2, positions: [{7, 6}], scope_id: 5},
             %VarInfo{name: :var_out1, positions: [{2, 3}, {3, 18}], scope_id: 2}
           ] = get_line_vars(state, 9)

    assert [
             %VarInfo{name: :var_on0, positions: [{3, 8}], scope_id: 2},
             %VarInfo{name: :var_on3, positions: [{10, 5}], scope_id: 6},
             %VarInfo{name: :var_out1, positions: [{2, 3}, {3, 18}], scope_id: 2}
           ] = get_line_vars(state, 10)

    assert [
             %VarInfo{name: :var_on0, positions: [{3, 8}], scope_id: 2},
             %VarInfo{name: :var_out1, positions: [{2, 3}, {3, 18}], scope_id: 2},
             %VarInfo{name: :var_out2, positions: [{12, 3}], scope_id: 2}
           ] = get_line_vars(state, 13)
  end

  test "vars defined inside a `cond`" do
    state =
      """
      defmodule MyModule do
        var_out1 = 1
        cond do
          1 == 1 ->
            var_in = 1
            IO.puts ""
          var_in1 = Enum.find([], 1) ->
            IO.puts ""
          var_in2 = Enum.find([], 2) -> IO.puts ""
        end
        var_out2 = 1
        IO.puts ""
      end
      """
      |> string_to_state

    assert [
             %VarInfo{name: :var_in, positions: [{5, 7}], scope_id: 4},
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 6)

    assert [
             %VarInfo{name: :var_in1, positions: [{7, 5}], scope_id: 5},
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 8)

    assert [
             %VarInfo{name: :var_in2, positions: [{9, 5}], scope_id: 6},
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 9)

    assert [
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
             %VarInfo{name: :var_out2, positions: [{11, 3}], scope_id: 2}
           ] = get_line_vars(state, 12)
  end

  test "vars defined inside a `try`" do
    state =
      """
      defmodule MyModule do
        var_out1 = 1
        try do
          var_in_try = 1
          IO.puts ""
        rescue
          e1 in ArgumentError -> IO.puts ""
          e2 in KeyError ->
            var_in_rescue = 0
            IO.puts ""
        catch
          :exit, reason1 -> IO.puts ""
          reason2 ->
            var_in_catch = 0
            IO.puts ""
        else
          {:atom, var_on_else} -> IO.puts ""
          :atom ->
            var_in_else = 0
            IO.puts ""
        after
          var_in_after = 0
          IO.puts ""
        end
        var_out2 = 1
        IO.puts ""
      end
      """
      |> string_to_state

    assert [
             %VarInfo{name: :var_in_try, positions: [{4, 5}], scope_id: 3},
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 5)

    assert [
             %VarInfo{name: :e1, positions: [{7, 5}], scope_id: 5},
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 7)

    assert [
             %VarInfo{name: :e2, positions: [{8, 5}], scope_id: 6},
             %VarInfo{name: :var_in_rescue, positions: [{9, 7}], scope_id: 6},
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 10)

    assert [
             %VarInfo{name: :reason1, positions: [{12, 12}], scope_id: 8},
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 12)

    assert [
             %VarInfo{name: :reason2, positions: [{13, 5}], scope_id: 9},
             %VarInfo{name: :var_in_catch, positions: [{14, 7}], scope_id: 9},
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 15)

    assert [
             %VarInfo{name: :var_on_else, positions: [{17, 13}], scope_id: 11},
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 17)

    assert [
             %VarInfo{name: :var_in_else, positions: [{19, 7}], scope_id: 12},
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 20)

    assert [
             %VarInfo{name: :var_in_after, positions: [{22, 5}], scope_id: 13},
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 23)

    assert [
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
             %VarInfo{name: :var_out2, positions: [{25, 3}], scope_id: 2}
           ] = get_line_vars(state, 26)
  end

  test "vars defined inside a `receive`" do
    state =
      """
      defmodule MyModule do
        var_out1 = 1
        receive do
          {:atom, msg1} -> IO.puts ""
          :msg ->
            var_in = 0
            IO.puts ""
        after
          300 ->
            var_in_after = 0
            IO.puts ""
        end
        var_out2 = 1
        IO.puts ""
      end
      """
      |> string_to_state

    assert [
             %VarInfo{name: :msg1, positions: [{4, 13}], scope_id: 4},
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 4)

    assert [
             %VarInfo{name: :var_in, positions: [{6, 7}], scope_id: 5},
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 7)

    assert [
             %VarInfo{name: :var_in_after, positions: [{10, 7}], scope_id: 7},
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 11)

    assert [
             %VarInfo{name: :var_out1, positions: [{2, 3}], scope_id: 2},
             %VarInfo{name: :var_out2, positions: [{13, 3}], scope_id: 2}
           ] = get_line_vars(state, 14)
  end

  test "functions of arity 0 should not be in the vars list" do
    state =
      """
      defmodule MyModule do
        myself = self
        mynode = node()
        IO.puts ""
      end
      """
      |> string_to_state

    assert [
             %VarInfo{name: :mynode, positions: [{3, 3}], scope_id: 2},
             %VarInfo{name: :myself, positions: [{2, 3}], scope_id: 2}
           ] = get_line_vars(state, 3)
  end

  test "inherited vars" do
    state =
      """
      top_level_var = 1
      IO.puts ""
      defmodule OuterModule do
        outer_module_var = 1
        IO.puts ""
        defmodule InnerModule do
          inner_module_var = 1
          IO.puts ""
          def func do
            func_var = 1
            IO.puts ""
          end
          IO.puts ""
        end
        IO.puts ""
      end
      IO.puts ""
      """
      |> string_to_state

    assert [
             %VarInfo{name: :top_level_var, positions: [{1, 1}], scope_id: 0}
           ] = get_line_vars(state, 2)

    assert [
             %VarInfo{name: :outer_module_var, positions: [{4, 3}], scope_id: 2},
             %VarInfo{name: :top_level_var, positions: [{1, 1}], scope_id: 0}
           ] = get_line_vars(state, 5)

    assert [
             %VarInfo{name: :inner_module_var, positions: [{7, 5}], scope_id: 4},
             %VarInfo{name: :outer_module_var, positions: [{4, 3}], scope_id: 2},
             %VarInfo{name: :top_level_var, positions: [{1, 1}], scope_id: 0}
           ] = get_line_vars(state, 8)

    assert [
             %VarInfo{name: :func_var, positions: [{10, 7}], scope_id: 5}
           ] = get_line_vars(state, 11)

    assert [
             %VarInfo{name: :inner_module_var, positions: [{7, 5}], scope_id: 4},
             %VarInfo{name: :outer_module_var, positions: [{4, 3}], scope_id: 2},
             %VarInfo{name: :top_level_var, positions: [{1, 1}], scope_id: 0}
           ] = get_line_vars(state, 13)

    assert [
             %VarInfo{name: :outer_module_var, positions: [{4, 3}], scope_id: 2},
             %VarInfo{name: :top_level_var, positions: [{1, 1}], scope_id: 0}
           ] = get_line_vars(state, 15)

    assert [
             %VarInfo{name: :top_level_var, positions: [{1, 1}], scope_id: 0}
           ] = get_line_vars(state, 17)
  end

  test "vars as a struct type" do
    state =
      """
      defmodule MyModule do
        def func(%my_var{}, %_my_other{}, %_{}, x) do
          %abc{} = x
          IO.puts ""
        end
      end
      """
      |> string_to_state

    assert [
             %VarInfo{is_definition: true, name: :abc, positions: [{3, 6}], scope_id: 3},
             %VarInfo{is_definition: true, name: :my_var, positions: [{2, 13}], scope_id: 2},
             %VarInfo{is_definition: true, name: :x, positions: [{2, 43}, {3, 14}], scope_id: 2}
           ] = state |> get_line_vars(4)
  end

  test "aliases" do
    state =
      """
      defmodule OuterModule do
        alias List, as: MyList
        IO.puts ""
        defmodule InnerModule do
          alias Enum, as: MyEnum
          IO.puts ""
          def func do
            alias String, as: MyString
            IO.puts ""
            if true do
              alias Macro, as: MyMacro
              IO.puts ""
            end
            IO.puts ""
          end
          IO.puts ""
        end
        IO.puts ""
        alias Code, as: MyCode
        IO.puts ""
        defmodule AnotherInnerModule do
          IO.puts ""
        end
        IO.puts ""
        defmodule SomeInnerModule.Nested do
          IO.puts ""
        end
        IO.puts ""
      end
      IO.puts ""
      """
      |> string_to_state

    assert get_line_aliases(state, 3) == [{MyList, List}]
    assert get_line_aliases(state, 6) == [{MyList, List}, {MyEnum, Enum}]
    assert get_line_aliases(state, 9) == [{MyList, List}, {MyEnum, Enum}, {MyString, String}]

    assert get_line_aliases(state, 12) == [
             {MyList, List},
             {MyEnum, Enum},
             {MyString, String},
             {MyMacro, Macro}
           ]

    assert get_line_aliases(state, 14) == [{MyList, List}, {MyEnum, Enum}, {MyString, String}]
    assert get_line_aliases(state, 16) == [{MyList, List}, {MyEnum, Enum}]
    # submodule defines an alias in parent module
    assert get_line_aliases(state, 18) == [{MyList, List}, {InnerModule, OuterModule.InnerModule}]

    assert get_line_aliases(state, 20) == [
             {MyList, List},
             {InnerModule, OuterModule.InnerModule},
             {MyCode, Code}
           ]

    assert get_line_aliases(state, 22) == [
             {MyList, List},
             {InnerModule, OuterModule.InnerModule},
             {MyCode, Code}
           ]

    assert get_line_aliases(state, 24) == [
             {MyList, List},
             {InnerModule, OuterModule.InnerModule},
             {MyCode, Code},
             {AnotherInnerModule, OuterModule.AnotherInnerModule}
           ]

    # submodule aliases are inherited to sibling submodules
    assert get_line_aliases(state, 26) == [
             {MyList, List},
             {InnerModule, OuterModule.InnerModule},
             {MyCode, Code},
             {AnotherInnerModule, OuterModule.AnotherInnerModule}
           ]

    # submodule Sub0.Sub1.Sub2 is equivalent to alias Sub0
    assert get_line_aliases(state, 28) == [
             {MyList, List},
             {InnerModule, OuterModule.InnerModule},
             {MyCode, Code},
             {AnotherInnerModule, OuterModule.AnotherInnerModule},
             {SomeInnerModule, OuterModule.SomeInnerModule}
           ]

    assert get_line_aliases(state, 30) == []
  end

  test "aliases with `fn`" do
    state =
      """
      defmodule MyModule do
        alias Enum, as: MyEnum
        IO.puts ""
        fn var_on ->
          alias List, as: MyList
          IO.puts ""
        end
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_aliases(state, 3) == [{MyEnum, Enum}]
    assert get_line_aliases(state, 6) == [{MyEnum, Enum}, {MyList, List}]
    assert get_line_aliases(state, 8) == [{MyEnum, Enum}]
  end

  test "aliases defined with v1.2 notation" do
    state =
      """
      defmodule MyModule do
        alias Foo.{User, Email, Elixir.Test}
        alias Elixir.User.{Data.Other, Address}
        alias Elixir.{String.Stream}
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_aliases(state, 5) == [
             {User, Foo.User},
             {Email, Foo.Email},
             {Test, Foo.Elixir.Test},
             {Other, User.Data.Other},
             {Address, User.Address},
             {Stream, String.Stream}
           ]
  end

  test "aliases defined with v1.2 notation __MODULE__" do
    state =
      """
      defmodule MyModule do
        alias __MODULE__.{User, Email}
        alias __MODULE__.Sub.{A, B.C}
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_aliases(state, 4) == [
             {User, MyModule.User},
             {Email, MyModule.Email},
             {A, MyModule.Sub.A},
             {C, MyModule.Sub.B.C}
           ]
  end

  test "aliases with __MODULE__" do
    state =
      """
      defmodule MyModule do
        alias __MODULE__.Sub
        alias __MODULE__
        alias __MODULE__.A.B, as: C
        alias __MODULE__, as: Some
        alias Some.Private
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_aliases(state, 7) == [
             {Sub, MyModule.Sub},
             {C, MyModule.A.B},
             {Some, MyModule},
             {Private, MyModule.Private}
           ]
  end

  test "aliases of aliases" do
    ElixirSense.Core.Source.split_module_and_func("Elixir.Keyword", CurrentMod, [
      {Keyword, My.Mod}
    ])

    state =
      """
      defmodule MyModule do
        alias Foo.Bar, as: Fb
        alias Fb.Sub, as: S
        alias Elixir.Fb.Oth
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_aliases(state, 5) == [{Fb, Foo.Bar}, {S, Foo.Bar.Sub}, {Oth, Fb.Oth}]
  end

  test "aliases erlang module" do
    state =
      """
      defmodule MyModule do
        alias :ets, as: Ets
        alias :erlang_module
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_aliases(state, 4) == [{Ets, :ets}, {:"Elixir.erlang_module", :erlang_module}]
  end

  test "aliases atom module" do
    state =
      """
      defmodule MyModule do
        alias :"Elixir.A.B"
        alias :"Elixir.A.C", as: S
        alias :"Elixir.A.D", as: :"Elixir.X"
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_aliases(state, 5) == [{B, A.B}, {S, A.C}, {X, A.D}]
  end

  test "aliases defined with v1.2 notation (multiline)" do
    state =
      """
      defmodule A do
        alias A.{
          B
        }
      end
      """
      |> string_to_state

    assert get_line_aliases(state, 3) == [{B, A.B}]
  end

  test "aliases defined with v1.2 notation nested" do
    state =
      """
      defmodule A do
        alias Components.{Dialog, Dialog.Footer, Button, :"Elixir.Other"}
        alias Some.{}
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_aliases(state, 4) == [
             {Dialog, Components.Dialog},
             {Footer, Components.Dialog.Footer},
             {Button, Components.Button},
             {Other, Components.Other}
           ]
  end

  test "aliases defined with v1.2 notation with atom module" do
    state =
      """
      defmodule A do
        alias :"Elixir.Components".{Dialog, Dialog.Footer, Button, :"Elixir.Other"}
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_aliases(state, 3) == [
             {Dialog, Components.Dialog},
             {Footer, Components.Dialog.Footer},
             {Button, Components.Button},
             {Other, Components.Other}
           ]
  end

  test "aliases without options" do
    state =
      """
      defmodule MyModule do
        alias Foo.User
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_aliases(state, 3) == [{User, Foo.User}]
  end

  test "aliases single level without options" do
    state =
      """
      defmodule MyModule do
        alias Foo
        alias :erlang_module
        IO.puts ""
        alias Enum, as: Foo
        alias Elixir.Foo
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_aliases(state, 4) == [{:"Elixir.erlang_module", :erlang_module}]
    assert get_line_aliases(state, 7) == [{:"Elixir.erlang_module", :erlang_module}]
  end

  test "aliases duplicated" do
    state =
      """
      defmodule MyModule do
        alias Foo.User
        alias Foo.User
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_aliases(state, 4) == [{User, Foo.User}]
  end

  test "import with options" do
    state =
      """
      defmodule MyModule do
        import Enum, only: []
        import Elixir.{List}, only: []
        import :lists, only: []
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_imports(state, 5) == [:lists, List, Enum]
  end

  test "imports defined with v1.2 notation" do
    state =
      """
      defmodule MyModule do
        import Foo.Bar.{User, Email, :"Elixir.Other"}
        import Bar.{}
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_imports(state, 4) == [Foo.Bar.Other, Foo.Bar.Email, Foo.Bar.User]
  end

  test "imports defined with v1.2 notation with atom module" do
    state =
      """
      defmodule MyModule do
        import :"Elixir.Foo.Bar".{User, Email, :"Elixir.Other"}
        import Bar.{}
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_imports(state, 4) == [Foo.Bar.Other, Foo.Bar.Email, Foo.Bar.User]
  end

  test "imports" do
    state =
      """
      defmodule OuterModule do
        import List
        IO.puts ""
        defmodule InnerModule do
          import Enum.List
          IO.puts ""
          def func do
            import String
            IO.puts ""
            if true do
              import Macro
              IO.puts ""
            end
            IO.puts ""
          end
          IO.puts ""
        end
        import Code
        IO.puts ""
      end
      IO.puts ""
      """
      |> string_to_state

    assert get_line_imports(state, 3) == [List]

    # note that `import` causes `require` module's macros available
    assert get_line_requires(state, 3) == [List]

    assert get_line_imports(state, 6) == [List, Enum.List]
    assert get_line_imports(state, 9) == [List, Enum.List, String]
    assert get_line_imports(state, 12) == [List, Enum.List, String, Macro]
    assert get_line_imports(state, 14) == [List, Enum.List, String]
    assert get_line_imports(state, 16) == [List, Enum.List]
    assert get_line_imports(state, 19) == [Code, List]
    assert get_line_imports(state, 21) == []
  end

  test "imports duplicated" do
    state =
      """
      defmodule OuterModule do
        import List
        import List
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_imports(state, 4) == [List]
  end

  test "imports with __MODULE__" do
    state =
      """
      defmodule OuterModule do
        import __MODULE__.Sub
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_imports(state, 3) == [OuterModule.Sub]
  end

  test "imports aliased module" do
    state =
      """
      defmodule OuterModule do
        alias Some.Other.Module, as: S
        import S
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_imports(state, 4) == [Some.Other.Module]
  end

  test "requires" do
    state =
      """
      defmodule MyModule do
        require Mod
        IO.puts ""
        defmodule Inner do
          require OtherMod
          IO.puts ""
        end
        IO.puts ""
      end
      IO.puts ""
      """
      |> string_to_state

    assert get_line_requires(state, 3) == [Mod]
    assert get_line_requires(state, 6) == [Mod, OtherMod]
    assert get_line_requires(state, 8) == [Mod]
    assert get_line_requires(state, 10) == []
  end

  test "requires single level" do
    state =
      """
      defmodule MyModule do
        require Mod
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_requires(state, 3) == [Mod]
  end

  test "requires with __MODULE__" do
    state =
      """
      defmodule MyModule do
        require __MODULE__.Sub
        require __MODULE__.A, as: B
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_requires(state, 4) == [MyModule.A, MyModule.Sub]
    assert get_line_aliases(state, 4) == [{B, MyModule.A}]
  end

  test "requires with 1.2 notation" do
    state =
      """
      defmodule MyModule do
        require Mod.{Mo1, Mod2, :"Elixir.Mod3"}
        require Foo.{}
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_requires(state, 4) == [Mod.Mod3, Mod.Mod2, Mod.Mo1]
  end

  test "requires with 1.2 notation with atom module" do
    state =
      """
      defmodule MyModule do
        require :"Elixir.Mod".{Mo1, Mod2, :"Elixir.Mod3"}
        require Foo.{}
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_requires(state, 4) == [Mod.Mod3, Mod.Mod2, Mod.Mo1]
  end

  test "requires duplicated" do
    state =
      """
      defmodule MyModule do
        require Mod.Mo1
        require Mod.Mo1
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_requires(state, 4) == [Mod.Mo1]
  end

  test "requires with :as option" do
    state =
      """
      defmodule MyModule do
        require Integer, as: I
        require :ets, as: E
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_requires(state, 4) == [:ets, Integer]
    assert get_line_aliases(state, 4) == [{I, Integer}, {E, :ets}]
  end

  test "requires atom module" do
    state =
      """
      defmodule MyModule do
        require :my_mod
        require :"Elixir.MyMod.Some"
        require :"Elixir.MyMod.Other", as: A
        require :"Elixir.MyMod.Other1", as: :"Elixir.A1"
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_requires(state, 6) == [MyMod.Other1, MyMod.Other, MyMod.Some, :my_mod]
    assert get_line_aliases(state, 6) == [{A, MyMod.Other}, {A1, MyMod.Other1}]
  end

  test "requires aliased module" do
    state =
      """
      defmodule MyModule do
        alias Some.Other.Module, as: S
        require S
        require S.Sub
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_requires(state, 5) == [Some.Other.Module.Sub, Some.Other.Module]
  end

  test "current module" do
    state =
      """
      IO.puts ""
      defmodule OuterModule do
        IO.puts ""
        defmodule InnerModule do
          def func do
            if true do
              IO.puts ""
            end
          end
        end
        IO.puts ""
      end

      defmodule Some.Nested do
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_module(state, 1) == Elixir
    assert get_line_protocol(state, 1) == nil
    assert get_line_module(state, 3) == OuterModule
    assert get_line_protocol(state, 3) == nil
    assert get_line_module(state, 7) == OuterModule.InnerModule
    assert get_line_protocol(state, 7) == nil
    assert get_line_module(state, 11) == OuterModule
    assert get_line_protocol(state, 11) == nil

    assert get_line_module(state, 15) == Some.Nested
    assert get_line_protocol(state, 15) == nil
  end

  test "current module with __MODULE__" do
    state =
      """
      IO.puts ""
      defmodule OuterModule do
        IO.puts ""
        defmodule __MODULE__.InnerModule do
          def func do
            if true do
              IO.puts ""
            end
          end
        end
        IO.puts ""
      end

      defmodule Some.Nested do
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_module(state, 1) == Elixir
    assert get_line_protocol(state, 1) == nil
    assert get_line_module(state, 3) == OuterModule
    assert get_line_protocol(state, 3) == nil
    assert get_line_module(state, 7) == OuterModule.InnerModule
    assert get_line_protocol(state, 7) == nil
    assert get_line_module(state, 11) == OuterModule
    assert get_line_protocol(state, 11) == nil

    assert get_line_module(state, 15) == Some.Nested
    assert get_line_protocol(state, 15) == nil
  end

  test "current module with `Elixir` prefix" do
    state =
      """
      IO.puts ""
      defmodule Elixir.OuterModule do
        IO.puts ""
        defmodule InnerModule do
          IO.puts ""
        end

        defmodule Elixir.ExternalModule do
          IO.puts ""
        end

        defprotocol Elixir.Reversible do
          def reverse(term)
          IO.puts ""
        end
        IO.puts ""
      end

      defmodule Elixir.Some.Nested do
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_module(state, 1) == Elixir
    assert get_line_module(state, 3) == OuterModule
    assert get_line_module(state, 5) == OuterModule.InnerModule
    assert get_line_module(state, 9) == ExternalModule
    assert get_line_module(state, 14) == Reversible
    assert get_line_module(state, 16) == OuterModule
    # external submodule does not create an alias
    assert get_line_aliases(state, 16) == [{InnerModule, OuterModule.InnerModule}]

    assert get_line_module(state, 20) == Some.Nested
  end

  test "current module with `Elixir` submodule" do
    state =
      """
      IO.puts ""
      defmodule OuterModule do
        IO.puts ""

        defmodule Elixir do
          IO.puts ""
        end

        IO.puts ""
      end

      defmodule Some.Nested do
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_module(state, 1) == Elixir
    assert get_line_module(state, 3) == OuterModule
    assert get_line_module(state, 6) == OuterModule.Elixir
    assert get_line_module(state, 9) == OuterModule

    assert get_line_module(state, 13) == Some.Nested
  end

  test "current module atom" do
    state =
      """
      IO.puts ""
      defmodule :outer_module do
        IO.puts ""
        defmodule :inner_module do
          def func do
            if true do
              IO.puts ""
            end
          end
        end
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_module(state, 1) == Elixir
    assert get_line_protocol(state, 1) == nil
    assert get_line_module(state, 3) == :outer_module
    assert get_line_protocol(state, 3) == nil
    assert get_line_module(state, 7) == :inner_module
    assert get_line_protocol(state, 7) == nil
    assert get_line_module(state, 11) == :outer_module
    assert get_line_protocol(state, 11) == nil
  end

  test "current module as atom" do
    state =
      """
      IO.puts ""
      defmodule :"Elixir.OuterModule" do
        IO.puts ""
        defmodule :"Elixir.InnerModule" do
          def func do
            if true do
              IO.puts ""
            end
          end
        end
        IO.puts ""

        defmodule :"Elixir.OuterModule.InnerModule1" do
          def func do
            if true do
              IO.puts ""
            end
          end
        end
      end
      """
      |> string_to_state

    assert get_line_module(state, 1) == Elixir
    assert get_line_protocol(state, 1) == nil
    assert get_line_module(state, 3) == OuterModule
    assert get_line_protocol(state, 3) == nil
    assert get_line_module(state, 7) == InnerModule
    assert get_line_protocol(state, 7) == nil
    assert get_line_module(state, 11) == OuterModule
    assert get_line_protocol(state, 11) == nil
    assert get_line_module(state, 16) == OuterModule.InnerModule1
    assert get_line_protocol(state, 16) == nil
  end

  test "current module and protocol implementation" do
    state =
      """
      defprotocol My.Reversible do
        def reverse(term)
        IO.puts ""
      end

      defimpl My.Reversible, for: String do
        def reverse(term), do: String.reverse(term)
        IO.puts ""
      end

      defimpl My.Reversible, for: [Map, My.List] do
        def reverse(term), do: Enum.reverse(term)
        IO.puts ""

        defmodule OuterModule do
          IO.puts ""
        end

        defprotocol Other do
          def other(term)
          IO.puts ""
        end

        defimpl Other, for: [Map, My.Map] do
          def other(term), do: nil
          IO.puts ""
        end
      end
      """
      |> string_to_state

    # protocol and implementations create modules
    assert get_line_module(state, 3) == My.Reversible
    assert get_line_protocol(state, 3) == nil
    assert get_line_module(state, 8) == My.Reversible.String
    assert get_line_protocol(state, 8) == {My.Reversible, [String]}
    assert get_line_module(state, 13) == [My.Reversible.Map, My.Reversible.My.List]
    assert get_line_protocol(state, 13) == {My.Reversible, [Map, My.List]}

    # implementation has behaviour
    assert get_line_behaviours(state, 8) == [My.Reversible]

    # multiple implementations create multiple modules
    assert get_line_module(state, 16) == [
             My.Reversible.Map.OuterModule,
             My.Reversible.My.List.OuterModule
           ]

    assert get_line_protocol(state, 16) == nil

    # protocol and implementations inside protocol implementation creates a cross product
    assert get_line_module(state, 21) == [My.Reversible.Map.Other, My.Reversible.My.List.Other]
    assert get_line_protocol(state, 21) == nil

    assert get_line_module(state, 26) == [
             My.Reversible.Map.Other.Map,
             My.Reversible.Map.Other.My.Map,
             My.Reversible.My.List.Other.Map,
             My.Reversible.My.List.Other.My.Map
           ]

    assert get_line_protocol(state, 26) == [
             {My.Reversible.Map.Other, [Map, My.Map]},
             {My.Reversible.My.List.Other, [Map, My.Map]}
           ]
  end

  test "protocol implementation for atom modules" do
    state =
      """
      defprotocol :my_reversible do
        def reverse(term)
        IO.puts ""
      end

      defimpl :my_reversible, for: [String, :my_str, :"Elixir.MyStr"] do
        def reverse(term), do: String.reverse(term)
        IO.puts ""
      end

      defprotocol :"Elixir.My.Reversible" do
        def reverse(term)
        IO.puts ""
      end

      defimpl :"Elixir.My.Reversible", for: [String, :my_str, :"Elixir.MyStr"] do
        def reverse(term), do: String.reverse(term)
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_module(state, 3) == :my_reversible
    assert get_line_protocol(state, 3) == nil

    assert get_line_module(state, 8) == [
             :"Elixir.my_reversible.String",
             :"Elixir.my_reversible.my_str",
             :"Elixir.my_reversible.MyStr"
           ]

    assert get_line_protocol(state, 8) == {:my_reversible, [String, :my_str, MyStr]}

    assert get_line_module(state, 13) == My.Reversible
    assert get_line_protocol(state, 13) == nil

    assert get_line_module(state, 18) == [
             My.Reversible.String,
             :"Elixir.My.Reversible.my_str",
             My.Reversible.MyStr
           ]

    assert get_line_protocol(state, 18) == {My.Reversible, [String, :my_str, MyStr]}
  end

  test "protocol implementation module naming rules" do
    state =
      """
      defprotocol NiceProto do
        def reverse(term)
      end

      defmodule NiceProtoImplementations do
        defimpl NiceProto, for: String do
          def reverse(term), do: String.reverse(term)
          IO.puts ""
        end

        defmodule Some do
          defstruct [a: nil]
        end

        defimpl NiceProto, for: Some do
          def reverse(term), do: String.reverse(term)
          IO.puts ""
        end

        alias Enumerable.Date.Range, as: R
        alias NiceProto, as: N

        defimpl N, for: R do
          def reverse(term), do: String.reverse(term)
          IO.puts ""
        end
      end
      """
      |> string_to_state

    # protocol implementation module name does not inherit enclosing module, only protocol
    assert get_line_module(state, 8) == NiceProto.String
    assert get_line_protocol(state, 8) == {NiceProto, [String]}

    # properly gets implementation name inherited from enclosing module
    assert get_line_module(state, 16) == NiceProto.NiceProtoImplementations.Some
    assert get_line_protocol(state, 16) == {NiceProto, [NiceProtoImplementations.Some]}

    # aliases are expanded on protocol and implementation
    assert get_line_module(state, 24) == NiceProto.Enumerable.Date.Range
    assert get_line_protocol(state, 24) == {NiceProto, [Enumerable.Date.Range]}
  end

  test "protocol implementation using __MODULE__" do
    state =
      """
      defprotocol NiceProto do
        def reverse(term)
      end

      defmodule MyStruct do
        defstruct [a: nil]

        defimpl NiceProto, for: __MODULE__ do
          def reverse(term), do: String.reverse(term)
        end
      end
      """
      |> string_to_state

    # protocol implementation module name does not inherit enclosing module, only protocol
    assert get_line_module(state, 8) == NiceProto.MyStruct
    assert get_line_protocol(state, 8) == {NiceProto, [MyStruct]}
  end

  test "protocol implementation using __MODULE__ 2" do
    state =
      """
      defmodule Nice do
        defprotocol Proto do
          def reverse(term)
        end

        defimpl __MODULE__.Proto, for: String do
          def reverse(term), do: String.reverse(term)
        end
      end
      """
      |> string_to_state

    assert get_line_module(state, 7) == Nice.Proto.String
    assert get_line_protocol(state, 7) == {Nice.Proto, [String]}
  end

  test "protocol implementation for structs does not require for" do
    state =
      """
      defprotocol Proto do
        def reverse(term)
      end

      defmodule MyStruct do
        defstruct [:field]

        defimpl Proto do
          def reverse(term), do: String.reverse(term)
        end
      end
      """
      |> string_to_state

    assert get_line_module(state, 9) == Proto.MyStruct
    assert get_line_protocol(state, 9) == {Proto, [MyStruct]}
  end

  test "protocol implementation by deriving" do
    state =
      """
      defprotocol Proto do
        def reverse(term)
      end

      defimpl Proto, for: Any do
        def reverse(term), do: term
      end

      defmodule MyStruct do
        @derive Proto
        defstruct [:field]
        IO.puts ""
      end
      IO.puts ""

      defmodule MyOtherStruct do
        @derive [{Proto, opt: 1}, Enumerable]
        defstruct [:field]
      end
      """
      |> string_to_state

    assert %{
             {Enumerable.MyOtherStruct, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [[line: 17, column: 3]],
               type: :defmodule
             },
             {MyOtherStruct, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{16, 11}],
               type: :defmodule
             },
             {MyStruct, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{9, 11}],
               type: :defmodule
             },
             {Proto, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{1, 13}],
               type: :defmodule
             },
             {Proto, :reverse, 1} => %ModFunInfo{
               params: [[{:term, [line: 2, column: 15], nil}]],
               positions: [{2, 7}],
               type: :def
             },
             {Proto, :reverse, nil} => %ModFunInfo{
               params: [[{:term, [line: 2, column: 15], nil}]],
               positions: [{2, 7}],
               type: :def
             },
             {Proto.Any, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{5, 9}],
               type: :defmodule
             },
             {Proto.Any, :reverse, 1} => %ModFunInfo{
               params: [[{:term, [line: 6, column: 15], nil}]],
               positions: [{6, 7}],
               type: :def
             },
             {Proto.Any, :reverse, nil} => %ModFunInfo{
               params: [[{:term, [line: 6, column: 15], nil}]],
               positions: [{6, 7}],
               type: :def
             },
             {Proto.MyOtherStruct, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{5, 9}],
               type: :defmodule
             },
             {Proto.MyOtherStruct, :reverse, 1} => %ModFunInfo{
               params: [[{:term, [line: 6, column: 15], nil}]],
               positions: [{6, 7}],
               type: :def
             },
             {Proto.MyOtherStruct, :reverse, nil} => %ModFunInfo{
               params: [[{:term, [line: 6, column: 15], nil}]],
               positions: [{6, 7}],
               type: :def
             },
             {Proto.MyStruct, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{5, 9}],
               type: :defmodule
             },
             {Proto.MyStruct, :reverse, 1} => %ModFunInfo{
               params: [[{:term, [line: 6, column: 15], nil}]],
               positions: [{6, 7}],
               type: :def
             },
             {Proto.MyStruct, :reverse, nil} => %ModFunInfo{
               params: [[{:term, [line: 6, column: 15], nil}]],
               positions: [{6, 7}],
               type: :def
             },
             {MyOtherStruct, :__struct__, 0} => %ModFunInfo{
               params: [[]],
               positions: [{18, 3}],
               type: :def
             },
             {MyOtherStruct, :__struct__, 1} => %ModFunInfo{
               params: [[{:kv, [line: 18, column: 3], nil}]],
               positions: [{18, 3}],
               type: :def
             },
             {MyOtherStruct, :__struct__, nil} => %ModFunInfo{
               params: [[{:kv, [line: 18, column: 3], nil}], []],
               positions: [{18, 3}, {18, 3}],
               type: :def
             },
             {MyStruct, :__struct__, 0} => %ModFunInfo{
               params: [[]],
               positions: [{11, 3}],
               type: :def
             },
             {MyStruct, :__struct__, 1} => %ModFunInfo{
               params: [[{:kv, [line: 11, column: 3], nil}]],
               positions: [{11, 3}],
               type: :def
             },
             {MyStruct, :__struct__, nil} => %ModFunInfo{
               params: [[{:kv, [line: 11, column: 3], nil}], []],
               positions: [{11, 3}, {11, 3}],
               type: :def
             },
             {Proto, :__protocol__, 1} => %ModFunInfo{
               params: [[{:atom, [line: 1, column: 13], nil}]],
               positions: [{1, 13}],
               type: :def
             },
             {Proto, :__protocol__, nil} => %ModFunInfo{
               params: [[{:atom, [line: 1, column: 13], nil}]],
               positions: [{1, 13}],
               type: :def
             },
             {Proto, :impl_for, 1} => %ModFunInfo{
               params: [[{:data, [line: 1, column: 13], nil}]],
               positions: [{1, 13}],
               type: :def
             },
             {Proto, :impl_for, nil} => %ModFunInfo{
               params: [[{:data, [line: 1, column: 13], nil}]],
               positions: [{1, 13}],
               type: :def
             },
             {Proto, :impl_for!, 1} => %ModFunInfo{
               params: [[{:data, [line: 1, column: 13], nil}]],
               positions: [{1, 13}],
               type: :def
             },
             {Proto, :impl_for!, nil} => %ModFunInfo{
               params: [[{:data, [line: 1, column: 13], nil}]],
               positions: [{1, 13}],
               type: :def
             },
             {Proto.MyStruct, :__impl__, nil} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 5, column: 9], nil}]],
               positions: [{5, 9}],
               type: :def
             },
             {Proto.Any, :__impl__, nil} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 5, column: 9], nil}]],
               positions: [{5, 9}],
               type: :def
             },
             {Proto.MyOtherStruct, :__impl__, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 5, column: 9], nil}]],
               positions: [{5, 9}],
               type: :def
             },
             {Proto.MyOtherStruct, :__impl__, nil} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 5, column: 9], nil}]],
               positions: [{5, 9}],
               type: :def
             },
             {Proto.MyStruct, :__impl__, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 5, column: 9], nil}]],
               positions: [{5, 9}],
               type: :def
             },
             {Proto.Any, :__impl__, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 5, column: 9], nil}]],
               positions: [{5, 9}],
               type: :def
             },
             {Proto, :behaviour_info, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 1, column: 13], nil}]],
               positions: [{1, 13}],
               type: :def
             },
             {Proto, :behaviour_info, nil} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 1, column: 13], nil}]],
               positions: [{1, 13}],
               type: :def
             }
           } = state.mods_funs_to_positions
  end

  test "protocol registers callbacks from specs or generate dummy callbacks" do
    state =
      """
      defprotocol Proto do
        @spec with_spec(t, integer) :: String.t
        @spec with_spec(t, boolean) :: number
        def with_spec(t, integer)

        def without_spec(t, integer)
      end
      """
      |> string_to_state

    assert state.specs == %{
             {Proto, :with_spec, 2} => %ElixirSense.Core.State.SpecInfo{
               args: [["t", "boolean"]],
               kind: :callback,
               name: :with_spec,
               positions: [{3, 3}],
               specs: [
                 "@callback with_spec(t, boolean) :: number",
                 "@spec with_spec(t, boolean) :: number"
               ]
             },
             {Proto, :with_spec, nil} => %ElixirSense.Core.State.SpecInfo{
               args: [["t", "boolean"], ["t", "integer"]],
               kind: :callback,
               name: :with_spec,
               positions: [{3, 3}, {2, 3}],
               specs: [
                 "@callback with_spec(t, boolean) :: number",
                 "@callback with_spec(t, integer) :: String.t",
                 "@spec with_spec(t, boolean) :: number",
                 "@spec with_spec(t, integer) :: String.t"
               ]
             },
             {Proto, :without_spec, nil} => %ElixirSense.Core.State.SpecInfo{
               args: [["t", "integer"]],
               kind: :callback,
               name: :without_spec,
               positions: [{6, 7}],
               specs: ["@callback without_spec(t, integer) :: term"]
             },
             {Proto, :without_spec, 2} => %ElixirSense.Core.State.SpecInfo{
               args: [["t", "integer"]],
               kind: :callback,
               name: :without_spec,
               positions: [{6, 7}],
               specs: ["@callback without_spec(t, integer) :: term"]
             }
           }
  end

  test "registers positions" do
    state =
      """
      IO.puts ""
      defmodule OuterModule do
        IO.puts ""
        defmodule InnerModule do
          def func do
            if true do
              IO.puts ""
            end
          end
        end
        IO.puts ""
      end

      defmodule Some.Nested do
        IO.puts ""
      end

      defprotocol Reversible do
        def reverse(term)
        IO.puts ""
      end

      defimpl Reversible, for: String do
        def reverse(term), do: String.reverse(term)
        IO.puts ""
      end

      defmodule Impls do
        alias Reversible, as: R
        alias My.List, as: Ml
        defimpl R, for: [Map, Ml] do
          def reverse(term), do: Enum.reverse(term)
          IO.puts ""
        end
      end
      """
      |> string_to_state

    assert %{
             {OuterModule, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{2, 11}],
               type: :defmodule
             },
             {OuterModule.InnerModule, :func, 0} => %ModFunInfo{
               params: [[]],
               positions: [{5, 9}],
               type: :def
             },
             {OuterModule.InnerModule, :func, nil} => %ModFunInfo{
               params: [[]],
               positions: [{5, 9}],
               type: :def
             },
             {OuterModule.InnerModule, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{4, 13}],
               type: :defmodule
             },
             {Impls, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{28, 11}],
               type: :defmodule
             },
             {Reversible, :reverse, 1} => %ModFunInfo{
               params: [[{:term, [line: 19, column: 15], nil}]],
               positions: [{19, 7}],
               type: :def
             },
             {Reversible, :reverse, nil} => %ModFunInfo{
               params: [[{:term, [line: 19, column: 15], nil}]],
               positions: [{19, 7}],
               type: :def
             },
             {Reversible, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{18, 13}],
               type: :defmodule
             },
             {Reversible.Map, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{31, 11}],
               type: :defmodule
             },
             {Reversible.Map, :reverse, 1} => %ModFunInfo{
               params: [[{:term, [line: 32, column: 17], nil}]],
               positions: [{32, 9}],
               type: :def
             },
             {Reversible.Map, :reverse, nil} => %ModFunInfo{
               params: [[{:term, [line: 32, column: 17], nil}]],
               positions: [{32, 9}],
               type: :def
             },
             {Reversible.My.List, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{31, 11}],
               type: :defmodule
             },
             {Reversible.My.List, :reverse, 1} => %ModFunInfo{
               params: [[{:term, [line: 32, column: 17], nil}]],
               positions: [{32, 9}],
               type: :def
             },
             {Reversible.My.List, :reverse, nil} => %ModFunInfo{
               params: [[{:term, [line: 32, column: 17], nil}]],
               positions: [{32, 9}],
               type: :def
             },
             {Reversible.String, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{23, 9}],
               type: :defmodule
             },
             {Reversible.String, :reverse, 1} => %ModFunInfo{
               params: [[{:term, [line: 24, column: 15], nil}]],
               positions: [{24, 7}],
               type: :def
             },
             {Reversible.String, :reverse, nil} => %ModFunInfo{
               params: [[{:term, [line: 24, column: 15], nil}]],
               positions: [{24, 7}],
               type: :def
             },
             {Some.Nested, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{14, 11}],
               type: :defmodule
             },
             {Reversible, :__protocol__, 1} => %ModFunInfo{
               params: [[{:atom, [line: 18, column: 13], nil}]],
               positions: [{18, 13}],
               type: :def
             },
             {Reversible, :__protocol__, nil} => %ModFunInfo{
               params: [[{:atom, [line: 18, column: 13], nil}]],
               positions: [{18, 13}],
               type: :def
             },
             {Reversible, :impl_for, 1} => %ModFunInfo{
               params: [[{:data, [line: 18, column: 13], nil}]],
               positions: [{18, 13}],
               type: :def
             },
             {Reversible, :impl_for, nil} => %ModFunInfo{
               params: [[{:data, [line: 18, column: 13], nil}]],
               positions: [{18, 13}],
               type: :def
             },
             {Reversible, :impl_for!, 1} => %ModFunInfo{
               params: [[{:data, [line: 18, column: 13], nil}]],
               positions: [{18, 13}],
               type: :def
             },
             {Reversible, :impl_for!, nil} => %ModFunInfo{
               params: [[{:data, [line: 18, column: 13], nil}]],
               positions: [{18, 13}],
               type: :def
             },
             {Reversible.Map, :__impl__, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 31, column: 11], nil}]],
               positions: [{31, 11}],
               type: :def
             },
             {Reversible.Map, :__impl__, nil} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 31, column: 11], nil}]],
               positions: [{31, 11}],
               type: :def
             },
             {Reversible.My.List, :__impl__, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 31, column: 11], nil}]],
               positions: [{31, 11}],
               type: :def
             },
             {Reversible.My.List, :__impl__, nil} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 31, column: 11], nil}]],
               positions: [{31, 11}],
               type: :def
             },
             {Reversible.String, :__impl__, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 23, column: 9], nil}]],
               positions: [{23, 9}],
               type: :def
             },
             {Reversible.String, :__impl__, nil} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 23, column: 9], nil}]],
               positions: [{23, 9}],
               type: :def
             },
             {Reversible, :behaviour_info, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 18, column: 13], nil}]],
               positions: [{18, 13}],
               type: :def
             },
             {Reversible, :behaviour_info, nil} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 18, column: 13], nil}]],
               positions: [{18, 13}],
               type: :def
             }
           } = state.mods_funs_to_positions
  end

  test "behaviours" do
    state =
      """
      IO.puts ""
      defmodule OuterModule do
        use Application
        @behaviour SomeModule.SomeBehaviour
        IO.puts ""
        defmodule InnerModuleWithUse do
          use GenServer
          IO.puts ""
        end
        defmodule InnerModuleWithBh do
          @behaviour SomeOtherBehaviour
          IO.puts ""
        end
        defmodule InnerModuleWithoutBh do
          IO.puts ""
        end
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_behaviours(state, 1) == []
    assert get_line_behaviours(state, 5) == [Application, SomeModule.SomeBehaviour]
    assert get_line_behaviours(state, 8) == [GenServer]
    assert get_line_behaviours(state, 12) == [SomeOtherBehaviour]
    assert get_line_behaviours(state, 15) == []
    assert get_line_behaviours(state, 17) == [Application, SomeModule.SomeBehaviour]
  end

  test "behaviour from erlang module" do
    state =
      """
      defmodule OuterModule do
        @behaviour :gen_server
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_behaviours(state, 3) == [:gen_server]
  end

  test "behaviour from __MODULE__" do
    state =
      """
      defmodule OuterModule do
        @behaviour __MODULE__.Sub
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_behaviours(state, 3) == [OuterModule.Sub]
  end

  test "behaviour from atom module" do
    state =
      """
      defmodule OuterModule do
        @behaviour :"Elixir.My.Behavior"
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_behaviours(state, 3) == [My.Behavior]
  end

  test "behaviour duplicated" do
    state =
      """
      defmodule OuterModule do
        @behaviour :gen_server
        @behaviour :gen_server
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_behaviours(state, 4) == [:gen_server]
  end

  test "behaviour from aliased module" do
    state =
      """
      defmodule OuterModule do
        alias Some.Module, as: S
        @behaviour S
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_behaviours(state, 4) == [Some.Module]
  end

  test "current scope" do
    state =
      """
      defmodule MyModule do
        def func do
          IO.puts ""
        end
        IO.puts ""
        def func_with_when(par) when is_list(par) do
          IO.puts ""
        end
        IO.puts ""
        defmacro macro1(ast) do
          IO.puts ""
        end
        IO.puts ""
        defmacro import(module, opts)
        IO.puts ""
        defdelegate func_delegated(par), to: OtherModule
        IO.puts ""
        defguard is_even(value) when is_integer(value) and rem(value, 2) == 0
        IO.puts ""
        defmodule Nester.Module1 do
          IO.puts ""
        end
      end

      defmodule AnotherNester.Module2 do
        IO.puts ""
      end

      defprotocol Reversible do
        def reverse(term)
        IO.puts ""
      end

      defimpl Reversible, for: [Map, My.List] do
        IO.puts ""
        def reverse(term) do
          IO.puts ""
        end
      end
      """
      |> string_to_state

    assert State.get_scope_name(state, 3) == {:func, 0}
    assert State.get_scope_name(state, 5) == :MyModule
    assert State.get_scope_name(state, 7) == {:func_with_when, 1}
    assert State.get_scope_name(state, 9) == :MyModule
    assert State.get_scope_name(state, 11) == {:macro1, 1}
    assert State.get_scope_name(state, 13) == :MyModule
    assert State.get_scope_name(state, 15) == :MyModule
    assert State.get_scope_name(state, 16) == {:func_delegated, 1}
    assert State.get_scope_name(state, 18) == {:is_even, 1}
    assert State.get_scope_name(state, 21) == :Module1
    assert State.get_scope_name(state, 26) == :Module2
    assert State.get_scope_name(state, 31) == :Reversible
    assert State.get_scope_name(state, 35) == :"Map(__or__)My(__dot__)List"
    assert State.get_scope_name(state, 37) == {:reverse, 1}
  end

  test "finds positions for guards" do
    state =
      """
      defmodule MyModule do
        defguard is_even(value) when is_integer(value) and rem(value, 2) == 0
        defguardp is_odd(value) when is_integer(value) and rem(value, 2) == 1
        defguardp useless when 1 == 1
        IO.puts ""
      end
      """
      |> string_to_state

    assert %{
             mods_funs_to_positions: %{
               {MyModule, :is_even, 1} => %{
                 params: [[{:value, [line: 2, column: 20], nil}]],
                 positions: [{2, 12}]
               },
               {MyModule, :is_even, nil} => %{
                 params: [[{:value, [line: 2, column: 20], nil}]],
                 positions: [{2, 12}]
               },
               {MyModule, :is_odd, 1} => %{
                 params: [[{:value, [line: 3, column: 20], nil}]],
                 positions: [{3, 13}]
               },
               {MyModule, :is_odd, nil} => %{
                 params: [[{:value, [line: 3, column: 20], nil}]],
                 positions: [{3, 13}]
               },
               {MyModule, :useless, 0} => %{
                 params: [[]],
                 positions: [{4, 13}]
               },
               {MyModule, :useless, nil} => %{
                 params: [[]],
                 positions: [{4, 13}]
               }
             }
           } = state
  end

  test "registers mods and func" do
    state =
      """
      defmodule MyModuleWithoutFuns do
      end
      defmodule MyModuleWithFuns do
        def func do
          IO.puts ""
        end
        defp funcp do
          IO.puts ""
        end
        defmacro macro1(ast) do
          IO.puts ""
        end
        defmacrop macro1p(ast) do
          IO.puts ""
        end
        defguard is_even(value) when is_integer(value) and rem(value, 2) == 0
        defguardp is_evenp(value) when is_integer(value) and rem(value, 2) == 0

        defmodule Nested do
        end
      end
      """
      |> string_to_state

    assert %{
             {MyModuleWithFuns, :func, 0} => %ModFunInfo{
               params: [[]],
               positions: [{4, 7}],
               type: :def
             },
             {MyModuleWithFuns, :func, nil} => %ModFunInfo{
               params: [[]],
               positions: [{4, 7}],
               type: :def
             },
             {MyModuleWithFuns, :funcp, 0} => %ModFunInfo{
               params: [[]],
               positions: [{7, 8}],
               type: :defp
             },
             {MyModuleWithFuns, :funcp, nil} => %ModFunInfo{
               params: [[]],
               positions: [{7, 8}],
               type: :defp
             },
             {MyModuleWithFuns, :is_even, 1} => %ModFunInfo{
               params: [[{:value, [line: 16, column: 20], nil}]],
               positions: [{16, 12}],
               type: :defguard
             },
             {MyModuleWithFuns, :is_even, nil} => %ModFunInfo{
               params: [[{:value, [line: 16, column: 20], nil}]],
               positions: [{16, 12}],
               type: :defguard
             },
             {MyModuleWithFuns, :is_evenp, 1} => %ModFunInfo{
               params: [[{:value, [line: 17, column: 22], nil}]],
               positions: [{17, 13}],
               type: :defguardp
             },
             {MyModuleWithFuns, :is_evenp, nil} => %ModFunInfo{
               params: [[{:value, [line: 17, column: 22], nil}]],
               positions: [{17, 13}],
               type: :defguardp
             },
             {MyModuleWithFuns, :macro1, 1} => %ModFunInfo{
               params: [[{:ast, [line: 10, column: 19], nil}]],
               positions: [{10, 12}],
               type: :defmacro
             },
             {MyModuleWithFuns, :macro1, nil} => %ModFunInfo{
               params: [[{:ast, [line: 10, column: 19], nil}]],
               positions: [{10, 12}],
               type: :defmacro
             },
             {MyModuleWithFuns, :macro1p, 1} => %ModFunInfo{
               params: [[{:ast, [line: 13, column: 21], nil}]],
               positions: [{13, 13}],
               type: :defmacrop
             },
             {MyModuleWithFuns, :macro1p, nil} => %ModFunInfo{
               params: [[{:ast, [line: 13, column: 21], nil}]],
               positions: [{13, 13}],
               type: :defmacrop
             },
             {MyModuleWithFuns, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{3, 11}],
               type: :defmodule
             },
             {MyModuleWithFuns.Nested, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{19, 13}],
               type: :defmodule
             },
             {MyModuleWithoutFuns, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{1, 11}],
               type: :defmodule
             },
             {MyModuleWithFuns, :__info__, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 3, column: 11], nil}]],
               positions: [{3, 11}],
               type: :def
             },
             {MyModuleWithFuns, :__info__, nil} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 3, column: 11], nil}]],
               positions: [{3, 11}],
               type: :def
             },
             {MyModuleWithFuns, :module_info, 0} => %ElixirSense.Core.State.ModFunInfo{
               params: [[]],
               positions: [{3, 11}],
               type: :def
             },
             {MyModuleWithFuns, :module_info, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 3, column: 11], nil}]],
               positions: [{3, 11}],
               type: :def
             },
             {MyModuleWithFuns, :module_info, nil} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 3, column: 11], nil}], []],
               positions: [{3, 11}, {3, 11}],
               type: :def
             },
             {MyModuleWithFuns.Nested, :__info__, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 19, column: 13], nil}]],
               positions: [{19, 13}],
               type: :def
             },
             {MyModuleWithFuns.Nested, :__info__, nil} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 19, column: 13], nil}]],
               positions: [{19, 13}],
               type: :def
             },
             {MyModuleWithFuns.Nested, :module_info, 0} => %ElixirSense.Core.State.ModFunInfo{
               params: [[]],
               positions: [{19, 13}],
               type: :def
             },
             {MyModuleWithFuns.Nested, :module_info, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 19, column: 13], nil}]],
               positions: [{19, 13}],
               type: :def
             },
             {MyModuleWithFuns.Nested, :module_info, nil} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 19, column: 13], nil}], []],
               positions: [{19, 13}, {19, 13}],
               type: :def
             },
             {MyModuleWithoutFuns, :__info__, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 1, column: 11], nil}]],
               positions: [{1, 11}],
               type: :def
             },
             {MyModuleWithoutFuns, :__info__, nil} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 1, column: 11], nil}]],
               positions: [{1, 11}],
               type: :def
             },
             {MyModuleWithoutFuns, :module_info, 0} => %ElixirSense.Core.State.ModFunInfo{
               params: [[]],
               positions: [{1, 11}],
               type: :def
             },
             {MyModuleWithoutFuns, :module_info, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 1, column: 11], nil}]],
               positions: [{1, 11}],
               type: :def
             },
             {MyModuleWithoutFuns, :module_info, nil} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 1, column: 11], nil}], []],
               positions: [{1, 11}, {1, 11}],
               type: :def
             }
           } == state.mods_funs_to_positions
  end

  test "registers delegated func" do
    state =
      """
      defmodule MyModuleWithFuns do
        alias Enum, as: E
        defdelegate func_delegated(par), to: OtherModule
        defdelegate func_delegated_erlang(par), to: :erlang_module
        defdelegate func_delegated_as(par), to: __MODULE__.Sub, as: :my_func
        defdelegate func_delegated_alias(par), to: E
      end
      """
      |> string_to_state

    assert %{
             {MyModuleWithFuns, :func_delegated, 1} => %ModFunInfo{
               params: [[{:par, [line: 3, column: 30], nil}]],
               positions: [{3, 15}],
               target: {OtherModule, :func_delegated},
               type: :defdelegate
             },
             {MyModuleWithFuns, :func_delegated, nil} => %ModFunInfo{
               params: [[{:par, [line: 3, column: 30], nil}]],
               positions: [{3, 15}],
               target: {OtherModule, :func_delegated},
               type: :defdelegate
             },
             {MyModuleWithFuns, :func_delegated_alias, 1} => %ModFunInfo{
               params: [[{:par, [line: 6, column: 36], nil}]],
               positions: [{6, 15}],
               target: {Enum, :func_delegated_alias},
               type: :defdelegate
             },
             {MyModuleWithFuns, :func_delegated_alias, nil} => %ModFunInfo{
               params: [[{:par, [line: 6, column: 36], nil}]],
               positions: [{6, 15}],
               target: {Enum, :func_delegated_alias},
               type: :defdelegate
             },
             {MyModuleWithFuns, :func_delegated_as, 1} => %ModFunInfo{
               params: [[{:par, [line: 5, column: 33], nil}]],
               positions: [{5, 15}],
               target: {MyModuleWithFuns.Sub, :my_func},
               type: :defdelegate
             },
             {MyModuleWithFuns, :func_delegated_as, nil} => %ModFunInfo{
               params: [[{:par, [line: 5, column: 33], nil}]],
               positions: [{5, 15}],
               target: {MyModuleWithFuns.Sub, :my_func},
               type: :defdelegate
             },
             {MyModuleWithFuns, :func_delegated_erlang, 1} => %ModFunInfo{
               params: [[{:par, [line: 4, column: 37], nil}]],
               positions: [{4, 15}],
               target: {:erlang_module, :func_delegated_erlang},
               type: :defdelegate
             },
             {MyModuleWithFuns, :func_delegated_erlang, nil} => %ModFunInfo{
               params: [[{:par, [line: 4, column: 37], nil}]],
               positions: [{4, 15}],
               target: {:erlang_module, :func_delegated_erlang},
               type: :defdelegate
             }
           } = state.mods_funs_to_positions
  end

  test "registers mods and func for protocols" do
    state =
      """
      defmodule MyModuleWithoutFuns do
      end
      defmodule MyModuleWithFuns do
        def func do
          IO.puts ""
        end
        defp funcp do
          IO.puts ""
        end
        defmacro macro1(ast) do
          IO.puts ""
        end
        defmacrop macro1p(ast) do
          IO.puts ""
        end
        defguard is_even(value) when is_integer(value) and rem(value, 2) == 0
        defguardp is_evenp(value) when is_integer(value) and rem(value, 2) == 0
        defdelegate func_delegated(par), to: OtherModule
        defmodule Nested do
        end
      end

      defprotocol Reversible do
        def reverse(term)
        IO.puts ""
      end

      defimpl Reversible, for: String do
        def reverse(term), do: String.reverse(term)
        IO.puts ""
      end

      defmodule Impls do
        alias Reversible, as: R
        alias My.List, as: Ml
        defimpl R, for: [Map, Ml] do
          def reverse(term), do: Enum.reverse(term)
          IO.puts ""
        end
      end
      """
      |> string_to_state

    assert %{
             {Impls, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{33, 11}],
               type: :defmodule
             },
             {MyModuleWithFuns, :func, 0} => %ModFunInfo{
               params: [[]],
               positions: [{4, 7}],
               type: :def
             },
             {MyModuleWithFuns, :func, nil} => %ModFunInfo{
               params: [[]],
               positions: [{4, 7}],
               type: :def
             },
             {MyModuleWithFuns, :func_delegated, 1} => %ModFunInfo{
               params: [[{:par, [line: 18, column: 30], nil}]],
               positions: [{18, 15}],
               type: :defdelegate
             },
             {MyModuleWithFuns, :func_delegated, nil} => %ModFunInfo{
               params: [[{:par, [line: 18, column: 30], nil}]],
               positions: [{18, 15}],
               type: :defdelegate
             },
             {MyModuleWithFuns, :funcp, 0} => %ModFunInfo{
               params: [[]],
               positions: [{7, 8}],
               type: :defp
             },
             {MyModuleWithFuns, :funcp, nil} => %ModFunInfo{
               params: [[]],
               positions: [{7, 8}],
               type: :defp
             },
             {MyModuleWithFuns, :is_even, 1} => %ModFunInfo{
               params: [[{:value, [line: 16, column: 20], nil}]],
               positions: [{16, 12}],
               type: :defguard
             },
             {MyModuleWithFuns, :is_even, nil} => %ModFunInfo{
               params: [[{:value, [line: 16, column: 20], nil}]],
               positions: [{16, 12}],
               type: :defguard
             },
             {MyModuleWithFuns, :is_evenp, 1} => %ModFunInfo{
               params: [[{:value, [line: 17, column: 22], nil}]],
               positions: [{17, 13}],
               type: :defguardp
             },
             {MyModuleWithFuns, :is_evenp, nil} => %ModFunInfo{
               params: [[{:value, [line: 17, column: 22], nil}]],
               positions: [{17, 13}],
               type: :defguardp
             },
             {MyModuleWithFuns, :macro1, 1} => %ModFunInfo{
               params: [[{:ast, [line: 10, column: 19], nil}]],
               positions: [{10, 12}],
               type: :defmacro
             },
             {MyModuleWithFuns, :macro1, nil} => %ModFunInfo{
               params: [[{:ast, [line: 10, column: 19], nil}]],
               positions: [{10, 12}],
               type: :defmacro
             },
             {MyModuleWithFuns, :macro1p, 1} => %ModFunInfo{
               params: [[{:ast, [line: 13, column: 21], nil}]],
               positions: [{13, 13}],
               type: :defmacrop
             },
             {MyModuleWithFuns, :macro1p, nil} => %ModFunInfo{
               params: [[{:ast, [line: 13, column: 21], nil}]],
               positions: [{13, 13}],
               type: :defmacrop
             },
             {MyModuleWithFuns, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{3, 11}],
               type: :defmodule
             },
             {MyModuleWithFuns.Nested, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{19, 13}],
               type: :defmodule
             },
             {MyModuleWithoutFuns, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{1, 11}],
               type: :defmodule
             },
             {Reversible, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{23, 13}],
               type: :defmodule
             },
             {Reversible, :reverse, 1} => %ModFunInfo{
               params: [[{:term, [line: 24, column: 15], nil}]],
               positions: [{24, 7}],
               type: :def
             },
             {Reversible, :reverse, nil} => %ModFunInfo{
               params: [[{:term, [line: 24, column: 15], nil}]],
               positions: [{24, 7}],
               type: :def
             },
             {Reversible.Map, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{36, 11}],
               type: :defmodule
             },
             {Reversible.Map, :reverse, 1} => %ModFunInfo{
               params: [[{:term, [line: 37, column: 17], nil}]],
               positions: [{37, 9}],
               type: :def
             },
             {Reversible.Map, :reverse, nil} => %ModFunInfo{
               params: [[{:term, [line: 37, column: 17], nil}]],
               positions: [{37, 9}],
               type: :def
             },
             {Reversible.My.List, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{36, 11}],
               type: :defmodule
             },
             {Reversible.My.List, :reverse, 1} => %ModFunInfo{
               params: [[{:term, [line: 37, column: 17], nil}]],
               positions: [{37, 9}],
               type: :def
             },
             {Reversible.My.List, :reverse, nil} => %ModFunInfo{
               params: [[{:term, [line: 37, column: 17], nil}]],
               positions: [{37, 9}],
               type: :def
             },
             {Reversible.String, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{28, 9}],
               type: :defmodule
             },
             {Reversible.String, :reverse, 1} => %ModFunInfo{
               params: [[{:term, [line: 29, column: 15], nil}]],
               positions: [{29, 7}],
               type: :def
             },
             {Reversible.String, :reverse, nil} => %ModFunInfo{
               params: [[{:term, [line: 29, column: 15], nil}]],
               positions: [{29, 7}],
               type: :def
             },
             {Reversible, :impl_for!, nil} => %ModFunInfo{
               params: [[{:data, [line: 23, column: 13], nil}]],
               positions: [{23, 13}],
               type: :def
             },
             {Reversible, :__protocol__, nil} => %ModFunInfo{
               params: [[{:atom, [line: 23, column: 13], nil}]],
               positions: [{23, 13}],
               type: :def
             },
             {Reversible, :impl_for!, 1} => %ModFunInfo{
               params: [[{:data, [line: 23, column: 13], nil}]],
               positions: [{23, 13}],
               type: :def
             },
             {Reversible, :impl_for, nil} => %ModFunInfo{
               params: [[{:data, [line: 23, column: 13], nil}]],
               positions: [{23, 13}],
               type: :def
             },
             {Reversible, :__protocol__, 1} => %ModFunInfo{
               params: [[{:atom, [line: 23, column: 13], nil}]],
               positions: [{23, 13}],
               type: :def
             },
             {Reversible, :impl_for, 1} => %ModFunInfo{
               params: [[{:data, [line: 23, column: 13], nil}]],
               positions: [{23, 13}],
               type: :def
             },
             {Reversible.Map, :__impl__, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 36, column: 11], nil}]],
               positions: [{36, 11}],
               type: :def
             },
             {Reversible.String, :__impl__, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 28, column: 9], nil}]],
               positions: [{28, 9}],
               type: :def
             },
             {Reversible.Map, :__impl__, nil} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 36, column: 11], nil}]],
               positions: [{36, 11}],
               type: :def
             },
             {Reversible.String, :__impl__, nil} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 28, column: 9], nil}]],
               positions: [{28, 9}],
               type: :def
             },
             {Reversible.My.List, :__impl__, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 36, column: 11], nil}]],
               positions: [{36, 11}],
               type: :def
             },
             {Reversible.My.List, :__impl__, nil} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 36, column: 11], nil}]],
               positions: [{36, 11}],
               type: :def
             },
             {Reversible, :behaviour_info, 1} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 23, column: 13], nil}]],
               positions: [{23, 13}],
               type: :def
             },
             {Reversible, :behaviour_info, nil} => %ElixirSense.Core.State.ModFunInfo{
               params: [[{:atom, [line: 23, column: 13], nil}]],
               positions: [{23, 13}],
               type: :def
             }
           } = state.mods_funs_to_positions
  end

  test "first_alias_positions" do
    state =
      """
      defmodule OuterMod do
        alias Foo.Bar
        alias Foo1.Bar1
        defmodule InnerMod do
          alias Baz.Quz
        end
      end
      """
      |> string_to_state

    assert %{OuterMod => {2, 3}, OuterMod.InnerMod => {5, 5}} = state.first_alias_positions
  end

  test "use" do
    state =
      """
      defmodule InheritMod do
        use ElixirSenseExample.ExampleBehaviour

        IO.puts("")
      end
      """
      |> string_to_state

    assert get_line_behaviours(state, 4) == [ElixirSenseExample.ExampleBehaviour]

    # note that `use` causes `require` to be able to execute `__using__/1` macro
    assert get_line_requires(state, 4) == [
             MyMacros.Two.Three,
             MyMacros.One,
             :ets,
             MyMacros.Nested,
             MyMacros,
             ElixirSenseExample.ExampleBehaviour
           ]

    assert get_line_imports(state, 4) == [
             :lists,
             MyImports.Two.ThreeImports,
             MyImports.OneImports,
             MyImports.NestedImports,
             MyImports,
             Some.List
           ]

    assert get_line_aliases(state, 4) == [
             {Utils, MyModule.Some.Nested},
             {Ets, :ets},
             {One, MyModule.One},
             {Three, MyModule.Two.Three},
             {Four, MyModule.Four},
             {:"Elixir.lists", :lists},
             {OutsideOfMyModule, Three.OutsideOfMyModule},
             {NestedMacros, MyMacros.Nested},
             {ErlangMacros, :ets},
             {Nested, InheritMod.Nested},
             {Deeply, InheritMod.Deeply},
             {ProtocolEmbedded, InheritMod.ProtocolEmbedded}
           ]

    assert get_line_attributes(state, 4) == [
             %AttributeInfo{name: :my_attribute, positions: [{2, 3}]}
           ]

    # FIXME `defdelegate` inside `__using__/1` macro is not supported
    # FIXME only submodules defined at top level are supported in `__using__/1`
    # FIXME submofule func and macro extraction is not supported in `__using__/1`

    assert %{
             {InheritMod, :handle_call, 3} => %ModFunInfo{
               params: [
                 [
                   {:msg, _, ElixirSenseExample.ExampleBehaviour},
                   {:_from, _, ElixirSenseExample.ExampleBehaviour},
                   {:state, _, ElixirSenseExample.ExampleBehaviour}
                 ]
               ],
               positions: [{2, 3}],
               type: :def
             },
             {InheritMod, :handle_call, nil} => %ModFunInfo{},
             {InheritMod, nil, nil} => %ModFunInfo{
               params: [nil, nil],
               positions: [{2, 3}, {1, 11}],
               type: :defmodule
             },
             {InheritMod, :private_func, 0} => %ModFunInfo{
               params: [[]],
               positions: [{2, 3}],
               type: :defp
             },
             {InheritMod, :private_func_arg, 1} => %ModFunInfo{
               params: [
                 [{:\\, _, [{:a, _, ElixirSenseExample.ExampleBehaviour}, nil]}],
                 [{:a, _, ElixirSenseExample.ExampleBehaviour}]
               ],
               positions: [{2, 3}, {2, 3}],
               type: :defp
             },
             {InheritMod, :private_guard, 0} => %ModFunInfo{
               params: [[]],
               positions: [{2, 3}],
               type: :defguardp
             },
             {InheritMod, :private_guard_arg, 1} => %ModFunInfo{
               params: [
                 [
                   {:a, _, ElixirSenseExample.ExampleBehaviour}
                 ]
               ],
               positions: [{2, 3}],
               type: :defguardp
             },
             {InheritMod, :private_macro, 0} => %ModFunInfo{
               params: [[]],
               positions: [{2, 3}],
               type: :defmacrop
             },
             {InheritMod, :private_macro_arg, 1} => %ModFunInfo{
               params: [
                 [
                   {:a, _, ElixirSenseExample.ExampleBehaviour}
                 ]
               ],
               positions: [{2, 3}],
               type: :defmacrop
             },
             {InheritMod, :public_func, 0} => %ModFunInfo{
               params: [[]],
               positions: [{2, 3}],
               type: :def,
               overridable: {true, ElixirSenseExample.ExampleBehaviour}
             },
             {InheritMod, :public_func_arg, 2} => %ModFunInfo{
               params: [
                 [
                   {:b, _, ElixirSenseExample.ExampleBehaviour},
                   {:\\, [keep: {"test/support/example_behaviour.ex", 121}],
                    [
                      {:a, _, ElixirSenseExample.ExampleBehaviour},
                      "def"
                    ]}
                 ]
               ],
               positions: [{2, 3}],
               type: :def
             },
             {InheritMod, :public_guard, 0} => %ModFunInfo{
               params: [[]],
               positions: [{2, 3}],
               type: :defguard
             },
             {InheritMod, :public_guard_arg, 1} => %ModFunInfo{
               params: [
                 [
                   {:a, _, ElixirSenseExample.ExampleBehaviour}
                 ]
               ],
               positions: [{2, 3}],
               type: :defguard
             },
             {InheritMod, :public_macro, 0} => %ModFunInfo{
               params: [[]],
               positions: [{2, 3}],
               type: :defmacro
             },
             {InheritMod, :public_macro_arg, 1} => %ModFunInfo{
               params: [
                 [
                   {:a, _, ElixirSenseExample.ExampleBehaviour}
                 ]
               ],
               positions: [{2, 3}],
               type: :defmacro
             },
             {InheritMod.Deeply.Nested, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{2, 3}],
               type: :defmodule
             },
             {InheritMod.Nested, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{2, 3}],
               type: :defmodule
             },
             {InheritMod.ProtocolEmbedded, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{2, 3}],
               type: :defmodule
             },
             {InheritMod, :behaviour_info, 1} => %ModFunInfo{
               params: [[{:atom, [line: 2, column: 3], nil}]],
               positions: [{2, 3}],
               target: nil,
               type: :def
             },
             {InheritMod.ProtocolEmbedded, :module_info, 1} => %ModFunInfo{}
           } = state.mods_funs_to_positions

    assert %{
             {InheritMod, :my_opaque_type, 0} => %State.TypeInfo{
               args: [[]],
               kind: :opaque,
               name: :my_opaque_type,
               positions: [{2, 3}],
               specs: ["@opaque my_opaque_type :: any"]
             },
             {InheritMod, :my_opaque_type, nil} => %State.TypeInfo{},
             {InheritMod, :my_priv_type, 0} => %State.TypeInfo{
               args: [[]],
               kind: :typep,
               name: :my_priv_type,
               positions: [{2, 3}],
               specs: ["@typep my_priv_type :: any"]
             },
             {InheritMod, :my_pub_type, 0} => %State.TypeInfo{
               args: [[]],
               kind: :type,
               name: :my_pub_type,
               positions: [{2, 3}],
               specs: ["@type my_pub_type :: any"]
             },
             {InheritMod, :my_pub_type_arg, 2} => %State.TypeInfo{
               args: [["a", "b"]],
               kind: :type,
               name: :my_pub_type_arg,
               positions: [{2, 3}],
               specs: ["@type my_pub_type_arg(a, b) :: {b, a}"]
             }
           } = state.types

    assert %{
             {InheritMod, :private_func, 0} => %State.SpecInfo{
               args: [[]],
               kind: :spec,
               name: :private_func,
               positions: [{2, 3}],
               specs: ["@spec private_func :: String.t"]
             },
             {InheritMod, :private_func, nil} => %State.SpecInfo{},
             {InheritMod, :some_callback, 1} => %State.SpecInfo{
               args: [["abc"]],
               kind: :callback,
               name: :some_callback,
               positions: [{2, 3}],
               specs: ["@callback some_callback(abc) :: :ok when abc: integer"]
             }
           } = state.specs
  end

  test "use defining struct" do
    state =
      """
      defmodule InheritMod do
        use ElixirSenseExample.ExampleBehaviourWithStruct

        IO.puts("")
      end
      """
      |> string_to_state

    assert %{
             InheritMod => %State.StructInfo{fields: [__struct__: InheritMod], type: :defstruct}
           } = state.structs

    assert %{
             {InheritMod, :__struct__, 0} => %State.ModFunInfo{},
             {InheritMod, :__struct__, 1} => %State.ModFunInfo{}
           } = state.mods_funs_to_positions
  end

  test "use defining exception" do
    state =
      """
      defmodule MyError do
        use ElixirSenseExample.ExampleBehaviourWithException

        IO.puts("")
      end
      """
      |> string_to_state

    assert %{
             MyError => %State.StructInfo{
               fields: [__exception__: true, __struct__: MyError],
               type: :defexception
             }
           } = state.structs

    assert %{
             {MyError, :__struct__, 0} => %State.ModFunInfo{},
             {MyError, :__struct__, 1} => %State.ModFunInfo{},
             {MyError, :exception, 1} => %State.ModFunInfo{}
           } = state.mods_funs_to_positions
  end

  test "use v1.2 notation" do
    state =
      """
      defmodule InheritMod do
        use ElixirSenseExample.{ExampleBehaviour}
        use Foo.{}

        IO.puts("")
      end
      """
      |> string_to_state

    assert get_line_behaviours(state, 5) == [ElixirSenseExample.ExampleBehaviour]
  end

  test "use v1.2 notation with atom module" do
    state =
      """
      defmodule InheritMod do
        use :"Elixir.ElixirSenseExample".{:"Elixir.ExampleBehaviour"}

        IO.puts("")
      end
      """
      |> string_to_state

    assert get_line_behaviours(state, 4) == [ElixirSenseExample.ExampleBehaviour]
  end

  test "use with __MODULE__" do
    state =
      """
      defmodule ElixirSenseExample do
        use __MODULE__.ExampleBehaviour

        IO.puts("")
      end
      """
      |> string_to_state

    assert get_line_behaviours(state, 4) == [ElixirSenseExample.ExampleBehaviour]
  end

  test "use aliased" do
    state =
      """
      defmodule InheritMod do
        alias ElixirSenseExample.ExampleBehaviour, as: S
        use S

        IO.puts("")
      end
      """
      |> string_to_state

    assert get_line_behaviours(state, 5) == [ElixirSenseExample.ExampleBehaviour]
  end

  test "use atom module" do
    state =
      """
      defmodule InheritMod do
        use :"Elixir.ElixirSenseExample.ExampleBehaviour"

        IO.puts("")
      end
      """
      |> string_to_state

    assert get_line_behaviours(state, 4) == [ElixirSenseExample.ExampleBehaviour]
  end

  test "find struct" do
    state =
      """
      defmodule MyStruct do
        defstruct [:some_field, a_field: 1]
        IO.puts ""
      end
      """
      |> string_to_state

    assert state.structs == %{
             MyStruct => %StructInfo{
               type: :defstruct,
               fields: [some_field: nil, a_field: 1, __struct__: MyStruct]
             }
           }

    # defstruct adds struct/0 and struct/1 functions
    assert %{
             {MyStruct, :__struct__, 0} => %ModFunInfo{
               params: [[]],
               positions: [{2, 3}],
               type: :def
             },
             {MyStruct, :__struct__, 1} => %ModFunInfo{
               params: [[{:kv, [line: 2, column: 3], nil}]],
               positions: [{2, 3}],
               type: :def
             },
             {MyStruct, :__struct__, nil} => %ModFunInfo{
               params: [[{:kv, [line: 2, column: 3], nil}], []],
               positions: [{2, 3}, {2, 3}],
               type: :def
             },
             {MyStruct, nil, nil} => %ModFunInfo{
               params: [nil],
               positions: [{1, 11}],
               type: :defmodule
             }
           } = state.mods_funs_to_positions
  end

  test "find struct fields from expression" do
    state =
      """
      defmodule MyStruct do
        @fields_1 [a: nil]
        defstruct [a_field: nil] ++ @fields_1
      end
      """
      |> string_to_state

    # TODO expression is not supported
    assert state.structs == %{
             MyStruct => %StructInfo{type: :defstruct, fields: [__struct__: MyStruct]}
           }
  end

  test "find exception" do
    state =
      """
      defmodule MyError do
        defexception [message: nil]

        IO.puts("")
      end
      """
      |> string_to_state

    assert state.structs == %{
             MyError => %StructInfo{
               type: :defexception,
               fields: [message: nil, __exception__: true, __struct__: MyError]
             }
           }

    # defexception adds Exception behaviour
    assert get_line_behaviours(state, 4) == [Exception]
    # and message/1 and exception/1 callbacks
    assert %{
             {MyError, :__struct__, 0} => %ModFunInfo{
               params: [[]],
               positions: [{2, 3}],
               type: :def
             },
             {MyError, :__struct__, 1} => %ModFunInfo{
               params: [[{:kv, [line: 2, column: 3], nil}]],
               positions: [{2, 3}],
               type: :def
             },
             {MyError, :exception, 1} => %ModFunInfo{
               params: [[{:args, [line: 2, column: 3], nil}], [{:msg, [line: 2, column: 3], nil}]],
               positions: [{2, 3}, {2, 3}],
               type: :def
             },
             {MyError, :message, 1} => %ModFunInfo{
               params: [[{:exception, [line: 2, column: 3], nil}]],
               positions: [{2, 3}],
               type: :def
             }
           } = state.mods_funs_to_positions
  end

  test "find exception without message key" do
    state =
      """
      defmodule MyError do
        defexception []

        IO.puts("")
      end
      """
      |> string_to_state

    assert state.structs == %{
             MyError => %StructInfo{
               type: :defexception,
               fields: [__exception__: true, __struct__: MyError]
             }
           }

    # defexception adds Exception behaviour
    assert get_line_behaviours(state, 4) == [Exception]
    # and message/1 and exception/1 callbacks
    assert %{
             {MyError, :__struct__, 0} => %ModFunInfo{
               params: [[]],
               positions: [{2, 3}],
               type: :def
             },
             {MyError, :__struct__, 1} => %ModFunInfo{
               params: [[{:kv, [line: 2, column: 3], nil}]],
               positions: [{2, 3}],
               type: :def
             },
             {MyError, :exception, 1} => %ModFunInfo{
               params: [[{:args, [line: 2, column: 3], nil}]],
               positions: [{2, 3}],
               type: :def
             }
           } = state.mods_funs_to_positions
  end

  test "registers calls with __MODULE__" do
    state =
      """
      defmodule NyModule do
        def func1, do: :ok
        def func2(a), do: :ok
        def func do
          __MODULE__.func1
          __MODULE__.func1()
          __MODULE__.func2(2)
          __MODULE__.Sub.func2(2)
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             5 => [%CallInfo{arity: 0, func: :func1, position: {5, 16}, mod: NyModule}],
             6 => [%CallInfo{arity: 0, func: :func1, position: {6, 16}, mod: NyModule}],
             7 => [%CallInfo{arity: 1, func: :func2, position: {7, 16}, mod: NyModule}],
             8 => [%CallInfo{arity: 1, func: :func2, position: {8, 20}, mod: NyModule.Sub}]
           }
  end

  test "registers calls with erlang module" do
    state =
      """
      defmodule NyModule do
        def func do
          :erl_mod.func1
          :erl_mod.func1()
          :erl_mod.func2(2)
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [%CallInfo{arity: 0, func: :func1, position: {3, 14}, mod: :erl_mod}],
             4 => [%CallInfo{arity: 0, func: :func1, position: {4, 14}, mod: :erl_mod}],
             5 => [%CallInfo{arity: 1, func: :func2, position: {5, 14}, mod: :erl_mod}]
           }
  end

  test "registers calls with atom module" do
    state =
      """
      defmodule NyModule do
        def func do
          :"Elixir.MyMod".func1
          :"Elixir.MyMod".func1()
          :"Elixir.MyMod".func2(2)
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [%CallInfo{arity: 0, func: :func1, position: {3, 21}, mod: MyMod}],
             4 => [%CallInfo{arity: 0, func: :func1, position: {4, 21}, mod: MyMod}],
             5 => [%CallInfo{arity: 1, func: :func2, position: {5, 21}, mod: MyMod}]
           }
  end

  test "registers calls no arg no parens" do
    state =
      """
      defmodule NyModule do
        def func do
          MyMod.func
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [%CallInfo{arity: 0, func: :func, position: {3, 11}, mod: MyMod}]
           }
  end

  test "registers calls no arg" do
    state =
      """
      defmodule NyModule do
        def func do
          MyMod.func()
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [%CallInfo{arity: 0, func: :func, position: {3, 11}, mod: MyMod}]
           }
  end

  test "registers calls local no arg no parens" do
    state =
      """
      defmodule NyModule do
        def func do
          func_1
        end
      end
      """
      |> string_to_state

    assert state.calls == %{3 => [%CallInfo{arity: 0, func: :func_1, position: {3, 5}, mod: nil}]}
  end

  test "registers calls local no arg" do
    state =
      """
      defmodule NyModule do
        def func do
          func_1()
        end
      end
      """
      |> string_to_state

    assert state.calls == %{3 => [%CallInfo{arity: 0, func: :func_1, position: {3, 5}, mod: nil}]}
  end

  test "registers calls local arg" do
    state =
      """
      defmodule NyModule do
        def func do
          func_1("a")
        end
      end
      """
      |> string_to_state

    assert state.calls == %{3 => [%CallInfo{arity: 1, func: :func_1, position: {3, 5}, mod: nil}]}
  end

  test "registers calls arg" do
    state =
      """
      defmodule NyModule do
        def func do
          MyMod.func("test")
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [%CallInfo{arity: 1, func: :func, position: {3, 11}, mod: MyMod}]
           }
  end

  test "registers calls on attribute with args" do
    state =
      """
      defmodule NyModule do
        def func do
          @attr.func("test")
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [%CallInfo{arity: 1, func: :func, position: {3, 11}, mod: {:attribute, :attr}}]
           }
  end

  test "registers calls on attribute without args" do
    state =
      """
      defmodule NyModule do
        def func do
          @attr.func
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [%CallInfo{arity: 0, func: :func, position: {3, 11}, mod: {:attribute, :attr}}]
           }
  end

  test "registers calls pipe with __MODULE__ operator no parens" do
    state =
      """
      defmodule NyModule do
        def func do
          "test" |> __MODULE__.func
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [%CallInfo{arity: 1, func: :func, position: {3, 26}, mod: NyModule}]
           }
  end

  test "registers calls pipe operator no parens" do
    state =
      """
      defmodule NyModule do
        def func do
          "test" |> MyMod.func
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [%CallInfo{arity: 1, func: :func, position: {3, 21}, mod: MyMod}]
           }
  end

  test "registers calls pipe operator" do
    state =
      """
      defmodule NyModule do
        def func do
          "test" |> MyMod.func()
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [%CallInfo{arity: 1, func: :func, position: {3, 21}, mod: MyMod}]
           }
  end

  test "registers calls pipe operator with arg" do
    state =
      """
      defmodule NyModule do
        def func do
          "test" |> MyMod.func("arg")
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [%CallInfo{arity: 2, func: :func, position: {3, 21}, mod: MyMod}]
           }
  end

  test "registers calls pipe operator erlang module" do
    state =
      """
      defmodule NyModule do
        def func do
          "test" |> :my_mod.func("arg")
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [%CallInfo{arity: 2, func: :func, position: {3, 23}, mod: :my_mod}]
           }
  end

  test "registers calls pipe operator atom module" do
    state =
      """
      defmodule NyModule do
        def func do
          "test" |> :"Elixir.MyMod".func("arg")
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [%CallInfo{arity: 2, func: :func, position: {3, 31}, mod: MyMod}]
           }
  end

  test "registers calls pipe operator local" do
    state =
      """
      defmodule NyModule do
        def func do
          "test" |> func("arg")
        end
      end
      """
      |> string_to_state

    assert state.calls == %{3 => [%CallInfo{arity: 2, func: :func, position: {3, 15}, mod: nil}]}
  end

  test "registers calls pipe operator nested external into local" do
    state =
      """
      defmodule NyModule do
        def func do
          "test" |> MyMod.func() |> other
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [
               %CallInfo{arity: 1, position: {3, 21}, func: :func, mod: MyMod},
               %CallInfo{arity: 1, position: {3, 31}, func: :other, mod: nil}
             ]
           }
  end

  test "registers calls pipe operator nested external into external" do
    state =
      """
      defmodule NyModule do
        def func do
          "test" |> MyMod.func() |> Other.other
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [
               %CallInfo{arity: 1, position: {3, 21}, func: :func, mod: MyMod},
               %CallInfo{arity: 1, position: {3, 37}, func: :other, mod: Other}
             ]
           }
  end

  test "registers calls pipe operator nested local into external" do
    state =
      """
      defmodule NyModule do
        def func do
          "test" |> func_1() |> Some.other
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [
               %CallInfo{arity: 1, position: {3, 15}, func: :func_1, mod: nil},
               %CallInfo{arity: 1, position: {3, 32}, func: :other, mod: Some}
             ]
           }
  end

  test "registers calls pipe operator nested local into local" do
    state =
      """
      defmodule NyModule do
        def func do
          "test" |> func_1() |> other
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [
               %CallInfo{arity: 1, position: {3, 15}, func: :func_1, mod: nil},
               %CallInfo{arity: 1, position: {3, 27}, func: :other, mod: nil}
             ]
           }
  end

  test "registers calls capture operator __MODULE__" do
    state =
      """
      defmodule NyModule do
        def func do
          &__MODULE__.func/1
          &__MODULE__.Sub.func/1
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [%CallInfo{arity: 1, position: {3, 17}, func: :func, mod: NyModule}],
             4 => [%CallInfo{arity: 1, position: {4, 21}, func: :func, mod: NyModule.Sub}]
           }
  end

  test "registers calls capture operator external" do
    state =
      """
      defmodule NyModule do
        def func do
          &MyMod.func/1
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [%CallInfo{arity: 1, position: {3, 12}, func: :func, mod: MyMod}]
           }
  end

  test "registers calls capture operator external erlang module" do
    state =
      """
      defmodule NyModule do
        def func do
          &:erl_mod.func/1
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [%CallInfo{arity: 1, func: :func, position: {3, 15}, mod: :erl_mod}]
           }
  end

  test "registers calls capture operator external atom module" do
    state =
      """
      defmodule NyModule do
        def func do
          &:"Elixir.MyMod".func/1
        end
      end
      """
      |> string_to_state

    assert state.calls == %{
             3 => [%CallInfo{arity: 1, func: :func, position: {3, 22}, mod: MyMod}]
           }
  end

  test "registers calls capture operator local" do
    state =
      """
      defmodule NyModule do
        def func do
          &func/1
        end
      end
      """
      |> string_to_state

    assert state.calls == %{3 => [%CallInfo{arity: 1, func: :func, position: {3, 6}, mod: nil}]}
  end

  test "registers types" do
    state =
      """
      defmodule My do
        @type no_arg_no_parens :: integer
        @typep no_args() :: integer
        @opaque with_args(a, b) :: {a, b}
        @type overloaded :: {}
        @type overloaded(a) :: {a}
      end
      IO.puts("")
      """
      |> string_to_state

    assert state.types == %{
             {My, :no_arg_no_parens, 0} => %ElixirSense.Core.State.TypeInfo{
               args: [[]],
               kind: :type,
               name: :no_arg_no_parens,
               positions: [{2, 3}],
               specs: ["@type no_arg_no_parens :: integer"]
             },
             {My, :no_arg_no_parens, nil} => %ElixirSense.Core.State.TypeInfo{
               args: [[]],
               kind: :type,
               name: :no_arg_no_parens,
               positions: [{2, 3}],
               specs: ["@type no_arg_no_parens :: integer"]
             },
             {My, :no_args, 0} => %ElixirSense.Core.State.TypeInfo{
               args: [[]],
               kind: :typep,
               name: :no_args,
               positions: [{3, 3}],
               specs: ["@typep no_args :: integer"]
             },
             {My, :no_args, nil} => %ElixirSense.Core.State.TypeInfo{
               args: [[]],
               kind: :typep,
               name: :no_args,
               positions: [{3, 3}],
               specs: ["@typep no_args :: integer"]
             },
             {My, :overloaded, 0} => %ElixirSense.Core.State.TypeInfo{
               args: [[]],
               kind: :type,
               name: :overloaded,
               positions: [{5, 3}],
               specs: ["@type overloaded :: {}"]
             },
             {My, :overloaded, 1} => %ElixirSense.Core.State.TypeInfo{
               kind: :type,
               name: :overloaded,
               positions: [{6, 3}],
               args: [["a"]],
               specs: ["@type overloaded(a) :: {a}"]
             },
             {My, :overloaded, nil} => %ElixirSense.Core.State.TypeInfo{
               kind: :type,
               name: :overloaded,
               positions: [{6, 3}, {5, 3}],
               args: [["a"], []],
               specs: ["@type overloaded(a) :: {a}", "@type overloaded :: {}"]
             },
             {My, :with_args, 2} => %ElixirSense.Core.State.TypeInfo{
               kind: :opaque,
               name: :with_args,
               positions: [{4, 3}],
               args: [["a", "b"]],
               specs: ["@opaque with_args(a, b) :: {a, b}"]
             },
             {My, :with_args, nil} => %ElixirSense.Core.State.TypeInfo{
               kind: :opaque,
               name: :with_args,
               positions: [{4, 3}],
               args: [["a", "b"]],
               specs: ["@opaque with_args(a, b) :: {a, b}"]
             }
           }
  end

  test "protocol exports type t" do
    state =
      """
      defprotocol Proto do
        def reverse(term)
      end
      """
      |> string_to_state

    assert state.types == %{
             {Proto, :t, nil} => %ElixirSense.Core.State.TypeInfo{
               args: [[]],
               kind: :type,
               name: :t,
               positions: [{1, 13}],
               specs: ["@type t :: term"]
             },
             {Proto, :t, 0} => %ElixirSense.Core.State.TypeInfo{
               args: [[]],
               kind: :type,
               name: :t,
               positions: [{1, 13}],
               specs: ["@type t :: term"]
             }
           }
  end

  test "specs and callbacks" do
    state =
      """
      defmodule Proto do
        @spec abc :: atom | integer
        @spec abc :: reference
        @callback my(a :: integer) :: atom
        @macrocallback other(x) :: Macro.t when x: integer
      end
      """
      |> string_to_state

    # if there are callbacks behaviour_info/1 is defined
    assert state.mods_funs_to_positions[{Proto, :behaviour_info, 1}] != nil

    assert state.specs == %{
             {Proto, :abc, 0} => %ElixirSense.Core.State.SpecInfo{
               args: [[]],
               kind: :spec,
               name: :abc,
               positions: [{3, 3}],
               specs: ["@spec abc :: reference"]
             },
             {Proto, :abc, nil} => %ElixirSense.Core.State.SpecInfo{
               kind: :spec,
               name: :abc,
               args: [[], []],
               positions: [{3, 3}, {2, 3}],
               specs: ["@spec abc :: reference", "@spec abc :: atom | integer"]
             },
             {Proto, :my, 1} => %ElixirSense.Core.State.SpecInfo{
               kind: :callback,
               name: :my,
               args: [["a :: integer"]],
               positions: [{4, 3}],
               specs: ["@callback my(a :: integer) :: atom"]
             },
             {Proto, :my, nil} => %ElixirSense.Core.State.SpecInfo{
               kind: :callback,
               name: :my,
               args: [["a :: integer"]],
               positions: [{4, 3}],
               specs: ["@callback my(a :: integer) :: atom"]
             },
             {Proto, :other, 1} => %ElixirSense.Core.State.SpecInfo{
               kind: :macrocallback,
               name: :other,
               args: [["x"]],
               positions: [{5, 3}],
               specs: ["@macrocallback other(x) :: Macro.t when x: integer"]
             },
             {Proto, :other, nil} => %ElixirSense.Core.State.SpecInfo{
               kind: :macrocallback,
               name: :other,
               args: [["x"]],
               positions: [{5, 3}],
               specs: ["@macrocallback other(x) :: Macro.t when x: integer"]
             }
           }
  end

  test "specs and types expand aliases" do
    state =
      """
      defmodule Proto do
        alias Model.User
        alias Model.Order
        alias Model.UserOrder
        @type local_type() :: User.t
        @spec abc({%User{}}) :: [%UserOrder{order: Order.t}, local_type()]
      end
      """
      |> string_to_state

    assert %{
             {Proto, :abc, 1} => %State.SpecInfo{
               args: [["{%Model.User{}}"]],
               specs: [
                 "@spec abc({%Model.User{}}) :: [%Model.UserOrder{order: Model.Order.t}, local_type]"
               ]
             },
             {Proto, :abc, nil} => %State.SpecInfo{
               args: [["{%Model.User{}}"]],
               specs: [
                 "@spec abc({%Model.User{}}) :: [%Model.UserOrder{order: Model.Order.t}, local_type]"
               ]
             }
           } = state.specs

    assert %{
             {Proto, :local_type, 0} => %State.TypeInfo{
               specs: ["@type local_type :: Model.User.t"]
             },
             {Proto, :local_type, nil} => %State.TypeInfo{
               specs: ["@type local_type :: Model.User.t"]
             }
           } = state.types
  end

  test "defrecord defines record macros" do
    state =
      """
      defmodule MyRecords do
        require Record
        Record.defrecord(:user, name: "meg", age: "25")
        @type user :: record(:user, name: String.t(), age: integer)
        Record.defrecordp(:userp, name: "meg", age: "25")
        Record.defrecord(:my_rec, Record.extract(:file_info, from_lib: "kernel/include/file.hrl")
          |> Keyword.merge(fun_field: &__MODULE__.foo/2))
        def foo(bar, baz), do: IO.inspect({bar, baz})
      end
      """
      |> string_to_state

    assert %{
             {MyRecords, :user, 1} => %ModFunInfo{
               params: [[{:\\, :args, []}]],
               positions: [{3, 9}],
               type: :defmacro
             },
             {MyRecords, :user, 2} => %ModFunInfo{
               params: [[:record, :args]],
               positions: [{3, 9}],
               type: :defmacro
             },
             {MyRecords, :userp, 1} => %ModFunInfo{type: :defmacrop},
             {MyRecords, :my_rec, 1} => %ModFunInfo{type: :defmacro}
           } = state.mods_funs_to_positions

    assert %{
             {MyRecords, :user, 0} => %State.TypeInfo{
               name: :user,
               specs: ["@type user :: record(:user, name: String.t, age: integer)"]
             }
           } = state.types
  end

  test "gets ExUnit imports from `use ExUnit.Case`" do
    state =
      """
      defmodule MyTest do
        use ExUnit.Case
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_imports(state, 3) == [
             ExUnit.DocTest,
             ExUnit.Case,
             ExUnit.Assertions,
             ExUnit.Callbacks
           ]
  end

  test "gets ExUnit imports from case template" do
    state =
      """
      defmodule MyTest do
        use ElixirSenseExample.CaseTemplateExample
        IO.puts ""
      end
      """
      |> string_to_state

    assert get_line_imports(state, 3) == [
             ExUnit.DocTest,
             ExUnit.Case,
             ExUnit.Assertions,
             ExUnit.Callbacks
           ]
  end

  defp string_to_state(string) do
    string
    |> Code.string_to_quoted(columns: true)
    |> (fn {:ok, ast} -> ast end).()
    |> MetadataBuilder.build()
  end

  defp get_line_vars(state, line) do
    case state.lines_to_env[line] do
      nil -> []
      env -> env.vars
    end
    |> Enum.sort()
  end

  defp get_line_aliases(state, line) do
    case state.lines_to_env[line] do
      nil -> []
      env -> env.aliases
    end
  end

  defp get_line_imports(state, line) do
    case state.lines_to_env[line] do
      nil -> []
      env -> env.imports
    end
  end

  defp get_line_requires(state, line) do
    case state.lines_to_env[line] do
      nil -> []
      env -> env.requires
    end
  end

  defp get_line_attributes(state, line) do
    case state.lines_to_env[line] do
      nil -> []
      env -> env.attributes
    end
    |> Enum.sort()
  end

  defp get_line_behaviours(state, line) do
    case state.lines_to_env[line] do
      nil -> []
      env -> env.behaviours
    end
    |> Enum.sort()
  end

  defp get_line_module(state, line) do
    if env = state.lines_to_env[line] do
      case env.module_variants do
        [single] -> single
        other -> other
      end
    end
  end

  defp get_line_protocol(state, line) do
    if env = state.lines_to_env[line] do
      case env.protocol_variants do
        [] -> nil
        [single] -> single
        other -> other
      end
    end
  end

  defp get_subject_definition_line(module, func, arity) do
    file = module.module_info(:compile)[:source]

    {:ok, ast} =
      File.read!(file)
      |> Code.string_to_quoted(columns: true)

    acc = MetadataBuilder.build(ast)

    %{positions: positions} = Map.get(acc.mods_funs_to_positions, {module, func, arity})
    {line_number, _col} = List.last(positions)

    File.read!(file) |> Source.split_lines() |> Enum.at(line_number - 1)
  end

  @tag requires_source: true
  test "all elixir modules" do
    elixir_sense_src_path =
      MetadataBuilder.module_info(:compile)[:source]
      |> Path.join("../../../..")
      |> Path.expand()
      |> ls_r

    elixir_src_path =
      Enum.module_info(:compile)[:source]
      |> Path.join("../../..")
      |> Path.expand()
      |> ls_r

    for path <- elixir_sense_src_path ++ elixir_src_path do
      case File.read!(path)
           |> Code.string_to_quoted(columns: true) do
        {:ok, ast} -> MetadataBuilder.build(ast)
        _ -> :ok
      end
    end
  end

  def ls_r(path \\ ".") do
    cond do
      File.regular?(path) ->
        [path]

      File.dir?(path) ->
        File.ls!(path)
        |> Enum.map(&Path.join(path, &1))
        |> Enum.map(&ls_r/1)
        |> Enum.concat()

      true ->
        []
    end
    # .eex?
    |> Enum.filter(&(String.ends_with?(&1, ".ex") or String.ends_with?(&1, ".exs")))
  end
end
