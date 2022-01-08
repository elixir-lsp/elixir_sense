defmodule ExUnitConfig do
  defp otp_older_than(major) do
    {otp_major_version, ""} = Integer.parse(System.build_info()[:otp_release])
    otp_major_version < major
  end

  defp otp_related do
    for major <- 23..23, otp_older_than(major) do
      {:"requires_otp_#{major}", true}
    end
  end

  defp elixir_older_than(minor) do
    !Version.match?(System.build_info().version, ">= 1.#{minor}.0")
  end

  defp elixir_related do
    for minor_version <- 10..13, elixir_older_than(minor_version) do
      {:"requires_elixir_1_#{minor_version}", true}
    end
  end

  def erlang_eep48_supported do
    otp_release = System.otp_release() |> String.to_integer()
    otp_release >= 23 and Version.match?(System.version(), ">= 1.11.0")
  end

  defp edoc_fallback do
    [{:edoc_fallback, erlang_eep48_supported()}]
  end

  def excludes do
    [requires_source: true] ++ otp_related() ++ elixir_related() ++ edoc_fallback()
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
