defmodule ElixirSense.Providers.SuggestionTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Providers.Suggestion
  alias ElixirSense.Core.State.StructInfo
  alias ElixirSense.Core.Metadata

  doctest Suggestion

  defmodule MyModule do
    def say_hi, do: true
  end

  @env %ElixirSense.Core.State.Env{
    module: SomeModule,
    scope: SomeModule
  }

  @env_func %ElixirSense.Core.State.Env{
    module: SomeModule,
    scope: {:func, 0}
  }

  test "find definition of built-in functions" do
    result = Suggestion.find("ElixirSenseExample.EmptyModule.", "", @env, %Metadata{})

    assert result |> Enum.at(0) == %{
             args: "atom",
             arity: 1,
             name: "__info__",
             origin: "ElixirSenseExample.EmptyModule",
             spec:
               "@spec __info__(:attributes) :: keyword()\n@spec __info__(:compile) :: [term()]\n@spec __info__(:functions) :: [{atom, non_neg_integer}]\n@spec __info__(:macros) :: [{atom, non_neg_integer}]\n@spec __info__(:md5) :: binary()\n@spec __info__(:module) :: module()",
             summary: "Built-in function",
             type: :function,
             metadata: %{builtin: true}
           }

    assert result |> Enum.at(1) == %{
             args: "",
             arity: 0,
             name: "module_info",
             origin: "ElixirSenseExample.EmptyModule",
             spec:
               "@spec module_info :: [{:module | :attributes | :compile | :exports | :md5 | :native, term}]",
             summary: "Built-in function",
             type: :function,
             metadata: %{builtin: true}
           }

    assert result |> Enum.at(2) == %{
             args: "key",
             arity: 1,
             name: "module_info",
             origin: "ElixirSenseExample.EmptyModule",
             spec:
               "@spec module_info(:module) :: atom\n@spec module_info(:attributes | :compile) :: [{atom, term}]\n@spec module_info(:md5) :: binary\n@spec module_info(:exports | :functions | :nifs) :: [{atom, non_neg_integer}]\n@spec module_info(:native) :: boolean",
             summary: "Built-in function",
             type: :function,
             metadata: %{builtin: true}
           }
  end

  test "return completion candidates for 'Str'" do
    assert Suggestion.find("ElixirSenseExample.ModuleWithDo", "", @env, %Metadata{}) ==
             [
               %{
                 name: "ModuleWithDocFalse",
                 subtype: nil,
                 summary: "",
                 type: :module,
                 metadata: %{}
               },
               %{
                 name: "ModuleWithDocs",
                 subtype: :behaviour,
                 summary: "An example module\n",
                 type: :module,
                 metadata: %{since: "1.2.3"}
               }
             ]
  end

  test "return completion candidates for 'List.del'" do
    assert [
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
               args: "list, index",
               arity: 2,
               name: "delete_at",
               origin: "List",
               spec: "@spec delete_at(list, integer) :: list",
               summary: "Produces a new list by " <> _,
               type: :function
             }
           ] = Suggestion.find("List.del", "", @env, %Metadata{})
  end

  test "return completion candidates for module with alias" do
    assert [
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
               args: "list, index",
               arity: 2,
               name: "delete_at",
               origin: "List",
               spec: "@spec delete_at(list, integer) :: list",
               summary: "Produces a new list " <> _,
               type: :function
             }
           ] = Suggestion.find("MyList.del", "", %{@env | aliases: [{MyList, List}]}, %Metadata{})
  end

  test "return completion candidates for functions from import" do
    assert Suggestion.find("say", "", %{@env | imports: [MyModule]}, %Metadata{}) ==
             [
               %{
                 args: "",
                 arity: 0,
                 name: "say_hi",
                 origin: "ElixirSense.Providers.SuggestionTest.MyModule",
                 spec: "",
                 summary: "",
                 type: :function,
                 metadata: %{}
               }
             ]
  end

  test "local calls should not return built-in functions" do
    list =
      Suggestion.find("mo", "", @env, %Metadata{})
      |> Enum.filter(fn item -> item.type in [:function] end)

    assert list == []
  end

  test "empty hint should not return built-in functions" do
    suggestions_names =
      Suggestion.find("", "", @env, %Metadata{})
      |> Enum.filter(&Map.has_key?(&1, :name))
      |> Enum.map(& &1.name)

    refute "module_info" in suggestions_names
  end

  defmodule MyStruct do
    defstruct [:my_val]
  end

  test "return completion candidates for struct starting with %" do
    assert [%{type: :module, name: "MyStruct"} | _] =
             Suggestion.find(
               "%ElixirSense.Providers.SuggestionTest.MyStr",
               "",
               @env_func,
               %Metadata{}
             )
  end

  test "return completion candidates for &func" do
    assert [%{type: :function, name: "all?", origin: "Enum"} | _] =
             Suggestion.find(
               "f = &Enum.al",
               "",
               @env_func,
               %Metadata{}
             )
  end

  test "do not return completion candidates for unknown erlang modules" do
    assert [] =
             Suggestion.find(
               "Enum:",
               "",
               @env_func,
               %Metadata{}
             )
  end

  test "do not return completion candidates for unknown modules" do
    assert [] =
             Suggestion.find(
               "x.Foo.get_by",
               "",
               @env_func,
               %Metadata{}
             )
  end

  test "return completion candidates for metadata modules" do
    assert [%{type: :function, name: "my_func"} | _] =
             Suggestion.find(
               "my_f",
               "",
               @env_func,
               %Metadata{
                 mods_funs_to_positions: %{
                   {SomeModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{type: :defmodule},
                   {SomeModule, :my_func, nil} => %ElixirSense.Core.State.ModFunInfo{type: :defp},
                   {SomeModule, :my_func, 1} => %ElixirSense.Core.State.ModFunInfo{
                     type: :defp,
                     params: [[[:a, [], nil]]]
                   }
                 }
               }
             )

    assert [%{type: :module, name: "SomeModule"} | _] =
             Suggestion.find(
               "So",
               "",
               @env_func,
               %Metadata{
                 mods_funs_to_positions: %{
                   {SomeModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{type: :defmodule}
                 }
               }
             )
  end

  test "return completion candidates for metadata structs" do
    assert [
             %{name: "str_field", origin: "SomeModule", type: :field}
           ] =
             Suggestion.find(
               "str_",
               "%SomeModule{st",
               @env_func,
               %Metadata{
                 structs: %{SomeModule => %StructInfo{type: :defstruct, fields: [str_field: 1]}},
                 mods_funs_to_positions: %{
                   {SomeModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{type: :defmodule}
                 }
               }
             )
  end
end
