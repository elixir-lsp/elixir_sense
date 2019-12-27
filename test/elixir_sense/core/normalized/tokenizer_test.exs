defmodule ElixirSense.Core.Normalized.TokenizerTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.Normalized.Tokenizer

  test "tokenizes valid elixir source" do
    buffer = """
    defmodule Abc do
        def fun(a), do: :ok
    end
    """

    assert [
             {:eol, {3, 4, 1}},
             {:end, {3, 1, nil}},
             {:eol, {2, 24, 1}},
             {:atom, {2, 21, nil}, :ok},
             {:kw_identifier, {2, 17, nil}, :do},
             {:",", {2, 15, 0}},
             {:")", {2, 14, nil}},
             {:identifier, {2, 13, nil}, :a},
             {:"(", {2, 12, nil}},
             {:paren_identifier, {2, 9, nil}, :fun},
             {:identifier, {2, 5, nil}, :def},
             {:eol, {1, 17, 1}},
             {:do, {1, 15, nil}},
             {:alias, {1, 11, nil}, :Abc},
             {:identifier, {1, 1, nil}, :defmodule}
           ] == Tokenizer.tokenize(buffer)
  end

  test "tokenizes invalidvalid elixir source" do
    buffer = """
    defmodule Abc do
        def jsndc(}.)
    """

    assert [
             {:"(", {2, 14, nil}},
             {:paren_identifier, {2, 9, nil}, :jsndc},
             {:identifier, {2, 5, nil}, :def},
             {:eol, {1, 17, 1}},
             {:do, {1, 15, nil}},
             {:alias, {1, 11, nil}, :Abc},
             {:identifier, {1, 1, nil}, :defmodule}
           ] == Tokenizer.tokenize(buffer)
  end
end
