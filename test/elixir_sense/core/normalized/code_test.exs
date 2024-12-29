defmodule ElixirSense.Core.Normalized.CodeTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.Normalized.Code

  test "gets function docs" do
    assert [
             {{:hard_deprecated_fun, 1}, 71, :function, [{:a, [line: 1], nil}],
              "An example fun\n",
              %{
                deprecated: "This function will be removed in a future release",
                app: :elixir_sense
              }},
             {{:soft_deprecated_fun, 1}, 57, :function, [{:a, [line: 1], nil}],
              "An example fun\n",
              %{
                deprecated: "This function will be removed in a future release",
                app: :elixir_sense
              }},
             {{:some_fun, 2}, 21, :function,
              [{:a, [line: 1], nil}, {:\\, [line: 1], [{:b, [line: 1], nil}, nil]}],
              "An example fun\n", %{defaults: 1, since: "1.1.0", app: :elixir_sense}},
             {{:some_fun_doc_false, 2}, 26, :function,
              [{:a, [line: 1], nil}, {:\\, [line: 1], [{:b, [line: 1], nil}, nil]}], false,
              %{defaults: 1, hidden: true, app: :elixir_sense}},
             {{:some_fun_no_doc, 2}, 28, :function,
              [{:a, [line: 1], nil}, {:\\, [line: 1], [{:b, [line: 1], nil}, nil]}], nil,
              %{defaults: 1, app: :elixir_sense}},
             {{:hard_deprecated_macro, 1}, 77, :macro, [{:a, [line: 1], nil}],
              "An example macro\n",
              %{deprecated: "This macro will be removed in a future release", app: :elixir_sense}},
             {{:soft_deprecated_macro, 1}, 63, :macro, [{:a, [line: 1], nil}],
              "An example macro\n",
              %{deprecated: "This macro will be removed in a future release", app: :elixir_sense}},
             {{:some_macro, 2}, 30, :macro,
              [{:a, [line: 1], nil}, {:\\, [line: 1], [{:b, [line: 1], nil}, nil]}],
              "An example macro\n", %{defaults: 1, since: "1.1.0", app: :elixir_sense}},
             {{:some_macro_doc_false, 2}, 35, :macro,
              [{:a, [line: 1], nil}, {:\\, [line: 1], [{:b, [line: 1], nil}, nil]}], false,
              %{defaults: 1, hidden: true, app: :elixir_sense}},
             {{:some_macro_no_doc, 2}, 37, :macro,
              [{:a, [line: 1], nil}, {:\\, [line: 1], [{:b, [line: 1], nil}, nil]}], nil,
              %{defaults: 1, app: :elixir_sense}}
           ] = Code.get_docs(ElixirSenseExample.ModuleWithDocs, :docs)
  end

  test "gets type docs" do
    assert [
             {{:some_type, 0}, 7, :type, "An example type\n",
              %{since: "1.1.0", app: :elixir_sense}},
             {{:some_type_doc_false, 0}, 12, :type, false, %{hidden: true, app: :elixir_sense}},
             {{:some_type_no_doc, 0}, 14, :type, nil, %{app: :elixir_sense}},
             {{:soft_deprecated_type, 0}, 95, :type, "An example type\n",
              %{deprecated: "This type will be removed in a future release", app: :elixir_sense}},
             {{:opaque_type, 0}, 16, :type, "An example opaque type\n",
              %{opaque: true, app: :elixir_sense}}
           ] = Code.get_docs(ElixirSenseExample.ModuleWithDocs, :type_docs)
  end

  test "gets callback docs" do
    assert [
             {{:soft_deprecated_callback, 1}, 83, :callback, "An example callback\n",
              %{
                deprecated: "This callback will be removed in a future release",
                app: :elixir_sense
              }},
             {{:some_callback, 1}, 39, :callback, "An example callback\n",
              %{since: "1.1.0", app: :elixir_sense}},
             {{:some_callback_doc_false, 1}, 44, :callback, false,
              %{hidden: true, app: :elixir_sense}},
             {{:some_callback_no_doc, 1}, 46, :callback, nil, %{app: :elixir_sense}},
             {{:soft_deprecated_macrocallback, 1}, 89, :macrocallback,
              "An example macrocallback\n",
              %{
                deprecated: "This callback will be removed in a future release",
                app: :elixir_sense
              }},
             {{:some_macrocallback, 1}, 48, :macrocallback, "An example callback\n",
              %{since: "1.1.0", app: :elixir_sense}},
             {{:some_macrocallback_doc_false, 1}, 53, :macrocallback, false,
              %{hidden: true, app: :elixir_sense}},
             {{:some_macrocallback_no_doc, 1}, 55, :macrocallback, nil, %{app: :elixir_sense}}
           ] = Code.get_docs(ElixirSenseExample.ModuleWithDocs, :callback_docs)
  end

  test "gets module docs" do
    assert {2, "An example module\n", %{since: "1.2.3", app: :elixir_sense}} =
             Code.get_docs(ElixirSenseExample.ModuleWithDocs, :moduledoc)

    assert {105, false, %{hidden: true, app: :elixir_sense}} =
             Code.get_docs(ElixirSenseExample.ModuleWithDocFalse, :moduledoc)

    assert {108, nil, %{app: :elixir_sense}} =
             Code.get_docs(ElixirSenseExample.ModuleWithNoDocs, :moduledoc)

    assert {112, "An example module\n",
            %{deprecated: "This module will be removed in a future release", app: :elixir_sense}} =
             Code.get_docs(ElixirSenseExample.SoftDeprecatedModule, :moduledoc)
  end

  test "not existing module" do
    assert nil == Code.get_docs(ElixirSenseExample.NotExistingModule, :docs)

    assert nil == Code.get_docs(ElixirSenseExample.NotExistingModule, :type_docs)

    assert nil == Code.get_docs(ElixirSenseExample.NotExistingModule, :callback_docs)

    assert nil == Code.get_docs(ElixirSenseExample.NotExistingModule, :moduledoc)
  end

  test "erlang module" do
    if ExUnitConfig.erlang_eep48_supported() do
      assert is_list(Code.get_docs(:lists, :docs))

      assert is_list(Code.get_docs(:erlang, :type_docs))

      assert is_list(Code.get_docs(:gen_server, :callback_docs))

      assert is_tuple(Code.get_docs(:lists, :moduledoc))
    else
      assert is_nil(Code.get_docs(:lists, :docs))

      assert is_nil(Code.get_docs(:erlang, :type_docs))

      assert is_nil(Code.get_docs(:gen_server, :callback_docs))

      assert is_nil(Code.get_docs(:lists, :moduledoc))
    end
  end
end
