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

  @cursor_context %{text_before: "", text_after: ""}
  @module_store %ElixirSense.Core.ModuleStore{}

  test "find definition of built-in functions" do
    result =
      Suggestion.find(
        "ElixirSenseExample.EmptyModule.",
        @env,
        %Metadata{},
        @cursor_context,
        @module_store
      )

    assert %{
             args: "atom",
             args_list: ["atom"],
             arity: 1,
             def_arity: 1,
             name: "__info__",
             origin: "ElixirSenseExample.EmptyModule",
             spec:
               "@spec __info__(:attributes) :: keyword()\n@spec __info__(:compile) :: [term()]\n@spec __info__(:functions) :: [{atom, non_neg_integer}]\n@spec __info__(:macros) :: [{atom, non_neg_integer}]\n@spec __info__(:md5) :: binary()\n@spec __info__(:module) :: module()",
             summary: "Built-in function",
             type: :function,
             metadata: %{builtin: true},
             snippet: nil,
             visibility: :public
           } = Enum.at(result, 0)

    assert %{
             args: "",
             args_list: [],
             arity: 0,
             def_arity: 0,
             name: "module_info",
             origin: "ElixirSenseExample.EmptyModule",
             spec:
               "@spec module_info :: [{:module | :attributes | :compile | :exports | :md5 | :native, term}]",
             summary: "Built-in function",
             type: :function,
             metadata: %{builtin: true},
             snippet: nil,
             visibility: :public
           } = Enum.at(result, 1)

    assert %{
             args: "key",
             args_list: ["key"],
             arity: 1,
             def_arity: 1,
             name: "module_info",
             origin: "ElixirSenseExample.EmptyModule",
             spec:
               "@spec module_info(:module) :: atom\n@spec module_info(:attributes | :compile) :: [{atom, term}]\n@spec module_info(:md5) :: binary\n@spec module_info(:exports | :functions | :nifs) :: [{atom, non_neg_integer}]\n@spec module_info(:native) :: boolean",
             summary: "Built-in function",
             type: :function,
             metadata: %{builtin: true},
             snippet: nil,
             visibility: :public
           } = Enum.at(result, 2)
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
           ] = Suggestion.find("List.del", @env, %Metadata{}, @cursor_context, @module_store)
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
           ] =
             Suggestion.find(
               "MyList.del",
               %{@env | aliases: [{MyList, List}]},
               %Metadata{},
               @cursor_context,
               @module_store
             )
  end

  test "return completion candidates for functions from import" do
    assert [
             %{
               args: "",
               args_list: [],
               arity: 0,
               def_arity: 0,
               name: "say_hi",
               origin: "ElixirSense.Providers.SuggestionTest.MyModule",
               spec: "",
               summary: "",
               type: :function,
               metadata: %{},
               snippet: nil,
               visibility: :public
             }
           ] =
             Suggestion.find(
               "say",
               %{@env | imports: [MyModule]},
               %Metadata{},
               @cursor_context,
               @module_store
             )
  end

  test "local calls should not return built-in functions" do
    list =
      Suggestion.find(
        # Trying to find module_info
        "module_",
        @env,
        %Metadata{},
        @cursor_context,
        @module_store
      )
      |> Enum.filter(fn item -> item.type in [:function] end)

    assert list == []
  end

  test "empty hint should not return built-in functions" do
    suggestions_names =
      Suggestion.find("", @env, %Metadata{}, @cursor_context, @module_store)
      |> Enum.filter(&Map.has_key?(&1, :name))
      |> Enum.map(& &1.name)

    refute "module_info" in suggestions_names
  end

  test "a function with default args generate multiple derived entries with same info, except arity" do
    assert [
             %{
               arity: 1,
               def_arity: 2,
               name: "all?",
               summary: "all?/2 docs",
               type: :function
             },
             %{
               arity: 2,
               def_arity: 2,
               name: "all?",
               summary: "all?/2 docs",
               type: :function
             }
           ] =
             Suggestion.find(
               "ElixirSenseExample.FunctionsWithTheSameName.all",
               @env,
               %Metadata{},
               @cursor_context,
               @module_store
             )
  end

  test "functions with the same name but different arities generates independent entries" do
    assert [
             %{
               arity: 1,
               def_arity: 1,
               name: "concat",
               summary: "concat/1 docs",
               type: :function
             },
             %{
               arity: 2,
               def_arity: 2,
               name: "concat",
               summary: "concat/2 docs",
               type: :function
             }
           ] =
             Suggestion.find(
               "ElixirSenseExample.FunctionsWithTheSameName.conca",
               @env,
               %Metadata{},
               @cursor_context,
               @module_store
             )
  end

  defmodule MyStruct do
    defstruct [:my_val]
  end

  test "return completion candidates for struct starting with %" do
    assert [%{type: :module, name: "MyStruct"} | _] =
             Suggestion.find(
               "%ElixirSense.Providers.SuggestionTest.MyStr",
               @env_func,
               %Metadata{},
               @cursor_context,
               @module_store
             )
  end

  test "return completion candidates for &func" do
    assert [%{type: :function, name: "all?", origin: "Enum"} | _] =
             Suggestion.find(
               "f = &Enum.al",
               @env_func,
               %Metadata{},
               @cursor_context,
               @module_store
             )
  end

  test "do not return completion candidates for unknown modules" do
    assert [] =
             Suggestion.find(
               "x.Foo.get_by",
               @env_func,
               %Metadata{},
               @cursor_context,
               @module_store
             )
  end

  test "return completion candidates for metadata modules" do
    assert [%{type: :function, name: "my_func"} | _] =
             Suggestion.find(
               "my_f",
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
               },
               @cursor_context,
               @module_store
             )

    assert [%{type: :module, name: "SomeModule"} | _] =
             Suggestion.find(
               "So",
               @env_func,
               %Metadata{
                 mods_funs_to_positions: %{
                   {SomeModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{type: :defmodule}
                 }
               },
               @cursor_context,
               @module_store
             )
  end

  test "return completion candidates for metadata structs" do
    assert [
             %{name: "str_field", origin: "SomeModule", type: :field}
           ] =
             Suggestion.find(
               "str_",
               @env_func,
               %Metadata{
                 structs: %{SomeModule => %StructInfo{type: :defstruct, fields: [str_field: 1]}},
                 mods_funs_to_positions: %{
                   {SomeModule, nil, nil} => %ElixirSense.Core.State.ModFunInfo{type: :defmodule}
                 }
               },
               %{text_before: "%SomeModule{st", text_after: ""},
               @module_store
             )
  end
end
