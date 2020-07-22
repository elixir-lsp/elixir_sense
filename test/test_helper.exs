ExUnit.configure(exclude: [requires_source: true])
ExUnit.start()

defmodule TestHelper do
  def cursors(text) do
    {_, cursors} =
      ElixirSense.Core.Source.walk_text(text, {false, []}, fn
        "#", rest, _, _, {_comment?, cursors} ->
          {rest, {true, cursors}}

        "\n", rest, _, _, {_comment?, cursors} ->
          {rest, {false, cursors}}

        "^", rest, line, col, {true, cursors} ->
          {rest, {true, [%{line: line - 1, col: col} | cursors]}}

        _, rest, _, _, acc ->
          {rest, acc}
      end)

    Enum.reverse(cursors)
  end

  def suggestions(buffer, cursor) do
    ElixirSense.suggestions(buffer, cursor.line, cursor.col)
  end

  def suggestions(buffer, cursor, type) do
    suggestions(buffer, cursor)
    |> Enum.filter(fn s -> s.type == type end)
  end

  def suggestions_by_kind(buffer, cursor, kind) do
    suggestions(buffer, cursor)
    |> Enum.filter(fn s -> s[:kind] == kind end)
  end
end
