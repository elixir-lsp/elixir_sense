defmodule ElixirSense.Core.Normalized.CodeTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.Normalized.Code

  test "gets function docs" do
    assert [
             {{:hard_deprecated_fun, 1}, 71, :function, [{:a, [line: 1], nil}],
              "An example fun\n",
              %{deprecated: "This function will be removed in a future release"}},
             {{:soft_deprecated_fun, 1}, 57, :function, [{:a, [line: 1], nil}],
              "An example fun\n",
              %{deprecated: "This function will be removed in a future release"}},
             {{:some_fun, 2}, 21, :function,
              [{:a, [line: 1], nil}, {:\\, [line: 1], [{:b, [line: 1], nil}, nil]}],
              "An example fun\n", %{defaults: 1, since: "1.1.0"}},
             {{:some_fun_doc_false, 2}, 26, :function,
              [{:a, [line: 1], nil}, {:\\, [line: 1], [{:b, [line: 1], nil}, nil]}], false,
              %{defaults: 1}},
             {{:some_fun_no_doc, 2}, 28, :function,
              [{:a, [line: 1], nil}, {:\\, [line: 1], [{:b, [line: 1], nil}, nil]}], nil,
              %{defaults: 1}},
             {{:hard_deprecated_macro, 1}, 77, :macro, [{:a, [line: 1], nil}],
              "An example macro\n",
              %{deprecated: "This macro will be removed in a future release"}},
             {{:soft_deprecated_macro, 1}, 63, :macro, [{:a, [line: 1], nil}],
              "An example macro\n",
              %{deprecated: "This macro will be removed in a future release"}},
             {{:some_macro, 2}, 30, :macro,
              [{:a, [line: 1], nil}, {:\\, [line: 1], [{:b, [line: 1], nil}, nil]}],
              "An example macro\n", %{defaults: 1, since: "1.1.0"}},
             {{:some_macro_doc_false, 2}, 35, :macro,
              [{:a, [line: 1], nil}, {:\\, [line: 1], [{:b, [line: 1], nil}, nil]}], false,
              %{defaults: 1}},
             {{:some_macro_no_doc, 2}, 37, :macro,
              [{:a, [line: 1], nil}, {:\\, [line: 1], [{:b, [line: 1], nil}, nil]}], nil,
              %{defaults: 1}}
           ] == Code.get_docs(ElixirSenseExample.ModuleWithDocs, :docs)
  end

  test "gets type docs" do
    assert [
             {{:some_type, 0}, 7, :type, "An example type\n", %{since: "1.1.0"}},
             {{:some_type_doc_false, 0}, 12, :type, false, %{}},
             {{:some_type_no_doc, 0}, 14, :type, nil, %{}},
             {{:soft_deprecated_type, 0}, 95, :type, "An example type\n",
              %{deprecated: "This type will be removed in a future release"}},
             {{:opaque_type, 0}, 16, :type, "An example opaque type\n", %{opaque: true}}
           ] == Code.get_docs(ElixirSenseExample.ModuleWithDocs, :type_docs)
  end

  test "gets callback docs" do
    assert [
             {{:soft_deprecated_callback, 1}, 83, :callback, "An example callback\n",
              %{deprecated: "This callback will be removed in a future release"}},
             {{:some_callback, 1}, 39, :callback, "An example callback\n", %{since: "1.1.0"}},
             {{:some_callback_doc_false, 1}, 44, :callback, false, %{}},
             {{:some_callback_no_doc, 1}, 46, :callback, nil, %{}},
             {{:soft_deprecated_macrocallback, 1}, 89, :macrocallback,
              "An example macrocallback\n",
              %{deprecated: "This callback will be removed in a future release"}},
             {{:some_macrocallback, 1}, 48, :macrocallback, "An example callback\n",
              %{since: "1.1.0"}},
             {{:some_macrocallback_doc_false, 1}, 53, :macrocallback, false, %{}},
             {{:some_macrocallback_no_doc, 1}, 55, :macrocallback, nil, %{}}
           ] == Code.get_docs(ElixirSenseExample.ModuleWithDocs, :callback_docs)
  end

  test "gets module docs" do
    assert {2, "An example module\n", %{since: "1.2.3"}} ==
             Code.get_docs(ElixirSenseExample.ModuleWithDocs, :moduledoc)

    assert {105, false, %{}} ==
             Code.get_docs(ElixirSenseExample.ModuleWithDocFalse, :moduledoc)

    assert {108, nil, %{}} ==
             Code.get_docs(ElixirSenseExample.ModuleWithNoDocs, :moduledoc)

    assert {112, "An example module\n",
            %{deprecated: "This module will be removed in a future release"}} ==
             Code.get_docs(ElixirSenseExample.SoftDeprecatedModule, :moduledoc)
  end

  test "not existing module" do
    assert nil == Code.get_docs(ElixirSenseExample.NotExistingModule, :docs)

    assert nil == Code.get_docs(ElixirSenseExample.NotExistingModule, :type_docs)

    assert nil == Code.get_docs(ElixirSenseExample.NotExistingModule, :callback_docs)

    assert nil == Code.get_docs(ElixirSenseExample.NotExistingModule, :moduledoc)

    assert nil == Code.get_docs(ElixirSenseExample.NotExistingModule, :all)
  end

  test "erlang module" do
    assert nil == Code.get_docs(:lists, :docs)

    assert nil == Code.get_docs(:lists, :type_docs)

    assert nil == Code.get_docs(:lists, :callback_docs)

    assert nil == Code.get_docs(:lists, :moduledoc)

    assert nil == Code.get_docs(:lists, :all)
  end
end
