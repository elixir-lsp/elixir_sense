defmodule ExUnitConfig do
  defp otp_related do
    {otp_major_version, ""} = Integer.parse(System.build_info()[:otp_release])
    if otp_major_version < 23 do
      [requires_otp_23: true]
    else
      []
    end
  end

  def excludes do
    [requires_source: true] ++ otp_related()
  end
end

ExUnit.configure(exclude: ExUnitConfig.excludes())
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
