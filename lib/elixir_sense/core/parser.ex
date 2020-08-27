defmodule ElixirSense.Core.Parser do
  @moduledoc """
  Core Parser
  """

  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.MetadataBuilder
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State

  @spec parse_file(String.t(), boolean, boolean, pos_integer | nil) :: Metadata.t()
  def parse_file(file, try_to_fix_parse_error, try_to_fix_line_not_found, cursor_line_number) do
    case File.read(file) do
      {:ok, source} ->
        parse_string(
          source,
          try_to_fix_parse_error,
          try_to_fix_line_not_found,
          cursor_line_number
        )

      error ->
        create_metadata("", error)
    end
  end

  @spec parse_string(String.t(), boolean, boolean, pos_integer | nil) :: Metadata.t()
  def parse_string(source, try_to_fix_parse_error, try_to_fix_line_not_found, cursor_line_number) do
    case string_to_ast(source, if(try_to_fix_parse_error, do: 6, else: 0), cursor_line_number) do
      {:ok, ast, modified_source} ->
        acc = MetadataBuilder.build(ast)

        if cursor_line_number == nil or Map.has_key?(acc.lines_to_env, cursor_line_number) or
             !try_to_fix_line_not_found do
          create_metadata(source, {:ok, acc})
        else
          # IO.puts :stderr, "LINE NOT FOUND"
          result =
            case try_fix_line_not_found_by_inserting_marker(modified_source, cursor_line_number) do
              {:ok, acc} -> acc
              _ -> fix_line_not_found_by_taking_previous_line(acc, cursor_line_number)
            end

          create_metadata(source, {:ok, result})
        end

      {:error, _reason} = error ->
        # IO.puts :stderr, "CAN'T FIX IT"
        # IO.inspect :stderr, reason, []
        create_metadata(source, error)
    end
  end

  defp fix_line_not_found_by_taking_previous_line(acc, cursor_line_number)
       when is_integer(cursor_line_number) do
    previous_env_or_default = State.get_closest_previous_env(acc, cursor_line_number)

    fixed_lines_to_env = acc.lines_to_env |> Map.put(cursor_line_number, previous_env_or_default)
    %State{acc | lines_to_env: fixed_lines_to_env}
  end

  defp try_fix_line_not_found_by_inserting_marker(modified_source, cursor_line_number)
       when is_integer(cursor_line_number) do
    with {:ok, ast, _modified_source} <-
           modified_source
           |> fix_line_not_found(cursor_line_number)
           |> string_to_ast(0, cursor_line_number) do
      acc = MetadataBuilder.build(ast)

      if Map.has_key?(acc.lines_to_env, cursor_line_number) do
        {:ok, acc}
      else
        :not_found
      end
    end
  end

  defp create_metadata(source, {:error, error}) do
    %Metadata{
      source: source,
      error: error
    }
  end

  defp create_metadata(source, {:ok, acc}) do
    %Metadata{
      source: source,
      types: acc.types,
      specs: acc.specs,
      structs: acc.structs,
      mods_funs_to_positions: acc.mods_funs_to_positions,
      lines_to_env: acc.lines_to_env,
      vars_info_per_scope_id: acc.vars_info_per_scope_id,
      calls: acc.calls
    }
  end

  def string_to_ast(
        source,
        errors_threshold,
        cursor_line_number,
        original_error \\ nil,
        opts \\ []
      ) do
    case Code.string_to_quoted(source, opts |> Keyword.put(:columns, true)) do
      {:ok, ast} ->
        {:ok, ast, source}

      error ->
        # IO.puts :stderr, "PARSE ERROR"
        # IO.inspect :stderr, error, []

        if errors_threshold > 0 do
          source
          |> fix_parse_error(cursor_line_number, normalize_error(error))
          |> string_to_ast(errors_threshold - 1, cursor_line_number, original_error, opts)
        else
          original_error || error
        end
    end
  end

  # elixir < 1.11
  defp normalize_error({:error, {line, msg, detail}}) when is_integer(line),
    do: {:error, {line, msg, detail}}

  # elixir >= 1.11
  defp normalize_error({:error, {[line: line, column: _column], msg, detail}}),
    do: {:error, {line, msg, detail}}

  defp fix_parse_error(
         source,
         _cursor_line_number,
         {:error, {line, {"\"" <> <<_::bytes-size(1)>> <> "\" is missing terminator" <> _, _}, _}}
       )
       when is_integer(line) do
    source
    |> replace_line_with_marker(line)
  end

  defp fix_parse_error(
         source,
         _cursor_line_number,
         {:error, {line_number, {"unexpected token: ", _text}, "do"}}
       ) do
    source
    |> Source.split_lines()
    |> List.update_at(line_number - 1, fn line ->
      # try to replace token do with do: marker
      line
      |> String.replace("do", "do: " <> marker(line_number), global: false)
    end)
    |> Enum.join("\n")
  end

  # since elixir 1.11 error is {"unexpected reserved word: ", ". The \"{\" at line 3 is missing terminator \"}\""}, "end"}

  defp fix_parse_error(
         source,
         cursor_line_number,
         {:error, {line_number, {message, text}, token}}
       )
       when is_integer(cursor_line_number) and
              message in ["unexpected token: ", "unexpected reserved word: "] do
    terminator =
      case Regex.run(Regex.recompile!(~r/terminator\s\"([^\s\"]+)/), text) do
        [_, terminator] -> terminator
        nil -> nil
      end

    if terminator != nil do
      source
      |> Source.split_lines()
      |> List.update_at(cursor_line_number - 1, fn line ->
        if cursor_line_number != line_number do
          # try to close the line with with missing terminator
          line <> " " <> terminator
        else
          # try to prepend first occurence of unexpected token with missing terminator
          line
          |> String.replace(token, terminator <> " " <> token, global: false)
        end
      end)
      |> Enum.join("\n")
    else
      source
      |> Source.split_lines()
      |> List.update_at(line_number - 1, fn line ->
        # drop unexpected token
        line
        |> String.replace(token, "", global: false)
      end)
      |> Enum.join("\n")
    end
  end

  defp fix_parse_error(
         source,
         _cursor_line_number,
         {:error, {line_number, "syntax" <> _, terminator_quoted}}
       )
       when is_integer(line_number) and terminator_quoted in ["'end'", "')'", "']'"] do
    terminator = Regex.replace(~r/[\"\']/, terminator_quoted, "")

    source
    |> Source.split_lines()
    |> List.update_at(line_number - 1, fn line ->
      # try to prepend unexpected terminator with marker
      line
      |> String.replace(terminator, marker(line_number) <> " " <> terminator, global: false)
    end)
    |> Enum.join("\n")
  end

  defp fix_parse_error(source, _cursor_line_number, {:error, {line, "syntax" <> _, token}})
       when is_integer(line) do
    if Regex.match?(~r/^[a-zA-Z_][a-zA-Z0-9_]*$/, token) do
      remove_line(source, line)
    else
      replace_line_with_marker(source, line)
    end
  end

  defp fix_parse_error(_, nil, error) do
    error
  end

  defp fix_parse_error(
         source,
         cursor_line_number,
         {:error, {line_end, text = "missing terminator: " <> _, _}}
       )
       when is_integer(cursor_line_number) do
    terminator =
      case Regex.run(Regex.recompile!(~r/terminator:\s([^\s]+)/), text) do
        [_, terminator] -> terminator
      end

    line_start =
      case Regex.run(Regex.recompile!(~r/line\s(\d+)/), text) do
        [_, line] -> line |> String.to_integer()
      end

    if terminator in ["\"", "\'"] do
      source
      |> Source.split_lines()
      |> List.update_at(max(cursor_line_number, line_start) - 1, fn line ->
        # try to close line with terminator
        line <> terminator
      end)
      |> Enum.join("\n")
    else
      compare_mode =
        case terminator do
          "\"\"\"" -> :lt
          _ -> :eq
        end

      line_intendations =
        source
        |> Source.split_lines()
        |> Enum.map(fn line ->
          line = normalize_intendation(line)
          {line, get_intendation_level(line)}
        end)

      line_intendation_at_start = line_intendations |> Enum.at(line_start - 1) |> elem(1)

      {source, _, missing_end} =
        line_intendations
        |> Enum.reduce({[], 1, true}, fn {line, intendation},
                                         {source_acc, current_line, missing_end} ->
          {modified_lines, missing_end} =
            cond do
              source_acc == [] ->
                {[line | source_acc], true}

              current_line <= line_start ->
                {[line | source_acc], true}

              missing_end and line != "" and
                compare_intendation(compare_mode, intendation, line_intendation_at_start) and
                  current_line < line_end ->
                [previous | rest] = source_acc

                replaced_line =
                  case terminator do
                    "end" -> previous <> "; " <> marker(length(rest)) <> "; end"
                    _ -> previous <> " " <> terminator
                  end

                {[line, replaced_line | rest], false}

              true ->
                {[line | source_acc], missing_end}
            end

          {modified_lines, current_line + 1, missing_end}
        end)

      if missing_end do
        [last | rest] = source
        [last <> " " <> terminator | rest]
      else
        source
      end
      |> Enum.reverse()
      |> Enum.join("\n")
    end
  end

  defp fix_parse_error(
         source,
         cursor_line_number,
         {:error, {_line_end, text = "missing interpolation terminator: \"}\"" <> _, _}}
       )
       when is_integer(cursor_line_number) do
    line_start =
      case Regex.run(Regex.recompile!(~r/line\s(\d+)/), text) do
        [_, line] -> line |> String.to_integer()
      end

    source
    |> Source.split_lines()
    |> List.update_at(max(cursor_line_number, line_start) - 1, fn line ->
      # try to close line with terminator
      line <> "}"
    end)
    |> Enum.join("\n")
  end

  defp fix_parse_error(source, cursor_line_number, _error) when is_integer(cursor_line_number) do
    source
    |> replace_line_with_marker(cursor_line_number)
  end

  defp compare_intendation(:eq, left, right), do: left == right
  defp compare_intendation(:lt, left, right), do: left < right

  defp normalize_intendation(line) do
    line
    |> String.replace_leading("\t", "  ")
  end

  def get_intendation_level(line) do
    trimmed_line = String.trim_leading(line)
    String.length(line) - String.length(trimmed_line)
  end

  defp fix_line_not_found(source, line_number) when is_integer(line_number) do
    source
    |> Source.split_lines()
    # by replacing a line here we risk introducing a syntax error
    # instead we append marker to the existing line
    |> List.update_at(line_number - 1, &(&1 <> "; " <> marker(line_number)))
    |> Enum.join("\n")
  end

  defp replace_line_with_marker(source, line_number) when is_integer(line_number) do
    # IO.puts :stderr, "REPLACING LINE: #{line}"
    source
    |> Source.split_lines()
    |> List.replace_at(line_number - 1, marker(line_number))
    |> Enum.join("\n")
  end

  defp remove_line(source, line_number) when is_integer(line_number) do
    # IO.puts :stderr, "REMOVING LINE: #{line}"
    source
    |> Source.split_lines()
    |> List.delete_at(line_number - 1)
    |> Enum.join("\n")
  end

  defp marker(line_number), do: "(__atom_elixir_marker_#{line_number}__())"
end
