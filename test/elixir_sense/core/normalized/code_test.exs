defmodule ElixirSense.Core.Normalized.CodeTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.Normalized.Code

  test "gets function docs" do
    assert [
             {{:some_fun, 2}, 15, :def,
              [{:a, [line: 1], nil}, {:\\, [line: 1], [{:b, [line: 1], nil}, nil]}],
              "An example fun\n", %{defaults: 1, since: "1.1.0"}},
             {{:some_fun_doc_false, 2}, 20, :def,
              [{:a, [line: 1], nil}, {:\\, [line: 1], [{:b, [line: 1], nil}, nil]}], false,
              %{defaults: 1}},
             {{:some_fun_no_doc, 2}, 22, :def,
              [{:a, [line: 1], nil}, {:\\, [line: 1], [{:b, [line: 1], nil}, nil]}], nil,
              %{defaults: 1}},
             {{:some_macro, 2}, 24, :def,
              [{:a, [line: 1], nil}, {:\\, [line: 1], [{:b, [line: 1], nil}, nil]}],
              "An example macro\n", %{defaults: 1, since: "1.1.0"}},
             {{:some_macro_doc_false, 2}, 29, :def,
              [{:a, [line: 1], nil}, {:\\, [line: 1], [{:b, [line: 1], nil}, nil]}], false,
              %{defaults: 1}},
             {{:some_macro_no_doc, 2}, 31, :def,
              [{:a, [line: 1], nil}, {:\\, [line: 1], [{:b, [line: 1], nil}, nil]}], nil,
              %{defaults: 1}}
           ] == Code.get_docs(ElixirSenseExample.ModuleWithDocs, :docs)
  end

  test "gets type docs" do
    assert [
             {{:some_type, 0}, 6, :type, "An example type\n", %{since: "1.1.0"}},
             {{:some_type_doc_false, 0}, 11, :type, false, %{}},
             {{:some_type_no_doc, 0}, 13, :type, nil, %{}}
           ] == Code.get_docs(ElixirSenseExample.ModuleWithDocs, :type_docs)
  end

  test "gets callback docs" do
    assert [
             {{:some_callback, 1}, 33, :callback, "An example callback\n", %{since: "1.1.0"}},
             {{:some_callback_doc_false, 1}, 38, :callback, false, %{}},
             {{:some_callback_no_doc, 1}, 40, :callback, nil, %{}},
             {{:some_macrocallback, 1}, 42, :macrocallback, "An example callback\n",
              %{since: "1.1.0"}},
             {{:some_macrocallback_doc_false, 1}, 47, :macrocallback, false, %{}},
             {{:some_macrocallback_no_doc, 1}, 49, :macrocallback, nil, %{}}
           ] == Code.get_docs(ElixirSenseExample.ModuleWithDocs, :callback_docs)
  end

  test "gets module docs" do
    assert {2, "An example module\n", %{}} ==
             Code.get_docs(ElixirSenseExample.ModuleWithDocs, :moduledoc)

    assert {53, false, %{}} ==
             Code.get_docs(ElixirSenseExample.ModuleWithDocFalse, :moduledoc)

    assert {56, nil, %{}} ==
             Code.get_docs(ElixirSenseExample.ModuleWithNoDocs, :moduledoc)
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
