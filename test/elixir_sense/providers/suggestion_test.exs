defmodule ElixirSense.Providers.SuggestionTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Providers.Suggestion

  doctest Suggestion

  defmodule MyModule do
    def say_hi, do: true
  end

  test "find definition of built-in functions" do
    result =
      Suggestion.find(
        "ElixirSenseExample.EmptyModule.",
        [],
        [],
        SomeModule,
        [],
        [],
        [],
        SomeModule,
        nil,
        %{},
        %{},
        ""
      )

    assert result |> Enum.at(0) == %{type: :hint, value: "ElixirSenseExample.EmptyModule."}

    assert result |> Enum.at(1) == %{
             args: "",
             arity: 0,
             name: "module_info",
             origin: "ElixirSenseExample.EmptyModule",
             spec: nil,
             summary: "Built-in function",
             type: :function
           }

    assert result |> Enum.at(2) == %{
             args: "",
             arity: 1,
             name: "module_info",
             origin: "ElixirSenseExample.EmptyModule",
             spec: nil,
             summary: "Built-in function",
             type: :function
           }

    assert result |> Enum.at(3) == %{
             args: "",
             arity: 1,
             name: "__info__",
             origin: "ElixirSenseExample.EmptyModule",
             spec: nil,
             summary: "Built-in function",
             type: :function
           }
  end

  test "return completion candidates for 'Str'" do
    assert Suggestion.find("Str", [], [], SomeModule, [], [], [], SomeModule, nil, %{}, %{}, "") ==
             [
               %{type: :hint, value: "Str"},
               %{
                 name: "Stream",
                 subtype: :struct,
                 summary: "Functions for creating and composing streams.",
                 type: :module
               },
               %{
                 name: "String",
                 subtype: nil,
                 summary: "A String in Elixir is a UTF-8 encoded binary.",
                 type: :module
               },
               %{
                 name: "StringIO",
                 subtype: nil,
                 summary: "Controls an IO device process that wraps a string.",
                 type: :module
               }
             ]
  end

  test "return completion candidates for 'List.del'" do
    assert [
             %{type: :hint, value: "List.delete"},
             %{
               args: "list," <> _,
               arity: 2,
               name: "delete",
               origin: "List",
               spec: "@spec delete(" <> _,
               summary: "Deletes the given" <> _,
               type: :function
             },
             %{
               args: "list,index",
               arity: 2,
               name: "delete_at",
               origin: "List",
               spec: "@spec delete_at(list, integer) :: list",
               summary: "Produces a new list by " <> _,
               type: :function
             }
           ] =
             Suggestion.find(
               "List.del",
               [],
               [],
               SomeModule,
               [],
               [],
               [],
               SomeModule,
               nil,
               %{},
               %{},
               ""
             )
  end

  test "return completion candidates for module with alias" do
    assert [
             %{type: :hint, value: "MyList.delete"},
             %{
               args: "list," <> _,
               arity: 2,
               name: "delete",
               origin: "List",
               spec: "@spec delete(" <> _,
               summary: "Deletes the given " <> _,
               type: :function
             },
             %{
               args: "list,index",
               arity: 2,
               name: "delete_at",
               origin: "List",
               spec: "@spec delete_at(list, integer) :: list",
               summary: "Produces a new list " <> _,
               type: :function
             }
           ] =
             Suggestion.find(
               "MyList.del",
               [],
               [{MyList, List}],
               SomeModule,
               [],
               [],
               [],
               SomeModule,
               nil,
               %{},
               %{},
               ""
             )
  end

  test "return completion candidates for functions from import" do
    assert Suggestion.find(
             "say",
             [MyModule],
             [],
             SomeModule,
             [],
             [],
             [],
             SomeModule,
             nil,
             %{},
             %{},
             ""
           ) == [
             %{type: :hint, value: "say_hi"},
             %{
               args: "",
               arity: 0,
               name: "say_hi",
               origin: "ElixirSense.Providers.SuggestionTest.MyModule",
               spec: nil,
               summary: "",
               type: :function
             }
           ]
  end

  test "local calls should not return built-in functions" do
    list =
      Suggestion.find("mo", [MyModule], [], SomeModule, [], [], [], SomeModule, nil, %{}, %{}, "")
      |> Enum.filter(fn item -> item.type in [:hint, :function, :function] end)

    assert list == [%{type: :hint, value: "mo"}]
  end

  test "empty hint should not return built-in functions" do
    suggestions_names =
      Suggestion.find("", [MyModule], [], SomeModule, [], [], [], SomeModule, nil, %{}, %{}, "")
      |> Enum.filter(&Map.has_key?(&1, :name))
      |> Enum.map(& &1.name)

    refute "module_info" in suggestions_names
  end

  defmodule MyStruct do
    defstruct [:my_val]
  end

  test "return completion candidates for struct starting with %" do
    assert [%{type: :hint, value: "%ElixirSense.Providers.SuggestionTest.MyStruct"} | _] =
             Suggestion.find(
               "%ElixirSense.Providers.SuggestionTest.MyStr",
               [MyModule],
               [],
               SomeModule,
               [],
               [],
               [],
               {:func, 0},
               nil,
               %{},
               %{},
               ""
             )
  end

  test "return completion candidates for &func" do
    assert [%{type: :hint, value: "f = &Enum.all?"} | _] =
             Suggestion.find(
               "f = &Enum.al",
               [MyModule],
               [],
               SomeModule,
               [],
               [],
               [],
               {:func, 0},
               nil,
               %{},
               %{},
               ""
             )
  end

  test "do not return completion candidates for unknown erlang modules" do
    assert [%{type: :hint, value: "Enum:"}] =
             Suggestion.find(
               "Enum:",
               [MyModule],
               [],
               SomeModule,
               [],
               [],
               [],
               {:func, 0},
               nil,
               %{},
               %{},
               ""
             )
  end

  test "do not return completion candidates for unknown modules" do
    assert [%{type: :hint, value: "x.Foo.get_by"}] =
             Suggestion.find(
               "x.Foo.get_by",
               [MyModule],
               [],
               SomeModule,
               [],
               [],
               [],
               {:func, 0},
               nil,
               %{},
               %{},
               ""
             )
  end

  test "return completion candidates for metadata modules" do
    assert [%{type: :hint, value: "my_func"} | _] =
             Suggestion.find(
               "my_f",
               [MyModule],
               [],
               SomeModule,
               [],
               [],
               [],
               {:func, 0},
               nil,
               %{
                 SomeModule => %{
                   {:my_func, 1} => %ElixirSense.Core.State.ModFunInfo{type: :defp}
                 }
               },
               %{},
               ""
             )

    assert [%{type: :hint, value: "SomeModule"} | _] =
             Suggestion.find(
               "So",
               [MyModule],
               [],
               SomeModule,
               [],
               [],
               [],
               {:func, 0},
               nil,
               %{
                 SomeModule => %{}
               },
               %{},
               ""
             )
  end

  test "return completion candidates for metadata structs" do
    assert [
             %{type: :hint, value: "str_"},
             %{name: "str_field", origin: "SomeModule", type: :field}
           ] =
             Suggestion.find(
               "str_",
               [MyModule],
               [],
               SomeModule,
               [],
               [],
               [],
               {:func, 0},
               nil,
               %{
                 SomeModule => %{}
               },
               %{SomeModule => {:defstruct, [str_field: 1]}},
               "%SomeModule{st"
             )
  end
end
