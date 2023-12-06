defmodule ElixirSense.Core.Parser do
  @moduledoc """
  Core Parser
  """

  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.MetadataBuilder
  alias ElixirSense.Core.Normalized.Tokenizer
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode
  require Logger

  @spec parse_file(String.t(), boolean, boolean, {pos_integer, pos_integer} | nil) :: Metadata.t()
  def parse_file(file, try_to_fix_parse_error, try_to_fix_line_not_found, cursor_position) do
    case File.read(file) do
      {:ok, source} ->
        if String.valid?(source) do
          parse_string(
            source,
            try_to_fix_parse_error,
            try_to_fix_line_not_found,
            cursor_position
          )
        else
          Logger.warning("Invalid encoding in #{file}")
          create_metadata("", {:error, :invalid_encoding})
        end

      error ->
        Logger.warning("Unable to read file #{file}: #{inspect(error)}")
        create_metadata("", {:error, :io_error})
    end
  end

  @spec parse_string(String.t(), boolean, boolean, {pos_integer, pos_integer} | nil) ::
          Metadata.t()
  def parse_string(source, try_to_fix_parse_error, try_to_fix_line_not_found, cursor_position) do
    unless String.valid?(source) do
      raise ArgumentError, message: "invalid string passed to parse_string"
    end

    string_to_ast_options = [
      errors_threshold: if(try_to_fix_parse_error, do: 6, else: 0),
      cursor_position: cursor_position,
      fallback_to_container_cursor_to_quoted: try_to_fix_parse_error
    ]

    case string_to_ast(source, string_to_ast_options) do
      {:ok, ast, modified_source, error} ->
        acc = MetadataBuilder.build(ast)

        if cursor_position == nil or Map.has_key?(acc.lines_to_env, elem(cursor_position, 0)) or
             !try_to_fix_line_not_found do
          create_metadata(source, {:ok, acc, error})
        else
          case try_fix_line_not_found_by_inserting_marker(modified_source, cursor_position) do
            {:ok, acc} ->
              create_metadata(source, {:ok, acc, error || {:error, :env_not_found}})

            _ ->
              create_metadata(source, {:ok, acc, error || {:error, :env_not_found}})
          end
        end

      {:error, _reason} = error ->
        create_metadata(source, error)
    end
  end

  @default_parser_options [columns: true, token_metadata: true]

  def string_to_ast(source, options \\ []) when is_binary(source) do
    errors_threshold = Keyword.get(options, :errors_threshold, 6)
    cursor_position = Keyword.get(options, :cursor_position)

    parser_options =
      Keyword.get(options, :parser_options, [])
      |> Keyword.merge(@default_parser_options)

    fallback_to_container_cursor_to_quoted =
      Keyword.get(options, :fallback_to_container_cursor_to_quoted, true)

    do_string_to_ast(
      source,
      errors_threshold,
      fallback_to_container_cursor_to_quoted,
      cursor_position,
      source,
      nil,
      parser_options
    )
  end

  defp do_string_to_ast(
         source,
         errors_threshold,
         fallback_to_container_cursor_to_quoted,
         cursor_position,
         original_source,
         original_error,
         parser_options
       ) when is_binary(source) do
    case Code.string_to_quoted(source, parser_options) do
      {:ok, ast} ->
        {:ok, ast, source, original_error}

      error ->
        error_to_report = original_error || {:error, :parse_error}
        # dbg(error)

        modified_source = if(errors_threshold > 0, do: fix_parse_error(source, cursor_position, error), else: error)

        if is_binary(modified_source) do
          do_string_to_ast(
            modified_source,
            errors_threshold - 1,
            fallback_to_container_cursor_to_quoted,
            cursor_position,
            original_source,
            error_to_report,
            parser_options
          )
        else
          case {fallback_to_container_cursor_to_quoted, cursor_position} do
            {true, {cursor_line, cursor_column}} ->
              prefix = Source.text_before(original_source, cursor_line, cursor_column)

              case NormalizedCode.Fragment.container_cursor_to_quoted(prefix, parser_options) do
                {:ok, ast} ->
                  {:ok, ast, prefix, error_to_report}

                _ ->
                  error_to_report
              end

            _ ->
              error_to_report
          end
        end
    end
  end

  def try_fix_line_not_found_by_inserting_marker(
         modified_source,
         {cursor_line_number, _} = cursor_position
       )
       when is_integer(cursor_line_number) do
    with {:ok, ast, _modified_source, _error} <-
           modified_source
           |> fix_line_not_found(cursor_line_number)
           |> do_string_to_ast(
             0,
             false,
             cursor_position,
             modified_source,
             nil,
             @default_parser_options
           ) do
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

  defp create_metadata(source, {:ok, acc, error}) do
    %Metadata{
      source: source,
      error: error,
      types: acc.types,
      specs: acc.specs,
      structs: acc.structs,
      mods_funs_to_positions: acc.mods_funs_to_positions,
      lines_to_env: acc.lines_to_env,
      vars_info_per_scope_id: acc.vars_info_per_scope_id,
      calls: acc.calls,
      first_alias_positions: acc.first_alias_positions,
      moduledoc_positions: acc.moduledoc_positions
    }
  end

  defp fix_parse_error(
         source,
         _cursor_position,
         {:error, {meta, {"\"" <> <<_::bytes-size(1)>> <> "\" is missing terminator" <> _, _}, _}}
       ) do
    line = get_line_from_meta(meta)

    source
    |> replace_line_with_marker(line)
  end

  defp fix_parse_error(
         source,
         _cursor_position,
         {:error, {meta, {message, _}, "do"}}
       )
       when message in ["unexpected reserved word: "] do
    line_number = get_line_from_meta(meta)

    source
    |> Source.split_lines()
    |> List.update_at(line_number - 1, fn line ->
      # try to replace token do with do: marker
      line
      |> String.replace("do", "do: " <> marker(line_number), global: false)
    end)
    |> Enum.join("\n")
  end

  defp fix_parse_error(
         source,
         {cursor_line_number, _},
         {:error, {meta, "unexpected reserved word: ", "end"}}
       )
       when is_integer(cursor_line_number) do
    # try to insert closing before end
    end_line = Keyword.fetch!(meta, :end_line)

    closing_delimiter =
      case Keyword.fetch!(meta, :opening_delimiter) do
        :"<<" -> ">>"
        :"{" -> "}"
        :"[" -> "]"
        :"(" -> ")"
      end

    source
    |> Source.split_lines()
    |> List.insert_at(end_line - 1, closing_delimiter)
    |> Enum.join("\n")
  end

  defp fix_parse_error(
         source,
         {cursor_line_number, _},
         {:error, {meta, {message, text}, token}}
       )
       when is_integer(cursor_line_number) and
              message in ["unexpected token: ", "unexpected reserved word: "] do
    line_number = get_line_from_meta(meta)

    terminator =
      case Regex.run(~r/terminator\s\"([^\s\"]+)/u, text) do
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
          # try to prepend first occurrence of unexpected token with missing terminator
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
         _cursor_position,
         {:error, {meta, "unexpected token: ", terminator}}
       )
       when terminator in [")", "]", "}", ">>"] do
    case Keyword.get(meta, :opening_delimiter) do
      :fn ->
        end_line = Keyword.fetch!(meta, :end_line)

        source
        |> Source.split_lines()
        |> List.update_at(end_line - 1, fn line ->
          # try to prepend unexpected terminator with end
          line
          |> String.replace(terminator, " end" <> terminator, global: false)
        end)
        |> Enum.join("\n")

      _ ->
        source
    end
  end

  defp fix_parse_error(
         source,
         _cursor_position,
         {:error, {meta, "syntax" <> _, terminator_quoted}}
       )
       when terminator_quoted in ["'end'", "')'", "']'", "'}'", "'>>'"] do
    line_number = get_line_from_meta(meta)
    terminator = Regex.replace(~r/[\"\']/u, terminator_quoted, "")

    source
    |> Source.split_lines()
    |> List.update_at(line_number - 1, fn line ->
      # try to prepend unexpected terminator with marker
      line
      |> String.replace(terminator, marker(line_number) <> " " <> terminator, global: false)
    end)
    |> Enum.join("\n")
  end

  defp fix_parse_error(
         source,
         _cursor_position,
         {:error, {meta, "unexpected expression after keyword list" <> _, token}}
       ) do
    line_number = get_line_from_meta(meta)
    token = Regex.replace(~r/[\"\']/u, token, "")

    source
    |> Source.split_lines()
    |> List.update_at(line_number - 1, fn line ->
      # drop unexpected token
      line
      |> String.replace(token, "", global: false)
    end)
    |> Enum.join("\n")
  end

  defp fix_parse_error(source, _cursor_position, {:error, {meta, "syntax" <> _, token}}) do
    line = get_line_from_meta(meta)

    case source
         |> Source.split_lines()
         |> Enum.at(line - 1)
         |> Tokenizer.tokenize()
         |> Enum.at(-1) do
      {:identifier, _, ident} when ident in [:with, :for] ->
        # strip_before(source, line, token |> String.replace("'", ""))
        source
        |> Source.split_lines()
        |> List.replace_at(line - 1, "#{ident} \\")
        |> Enum.join("\n")

      _ ->
        if Regex.match?(~r/^[\p{L}_][\p{L}\p{N}_@]*[?!]?$/u, token) do
          remove_line(source, line)
        else
          replace_line_with_marker(source, line)
        end
    end
  end

  defp fix_parse_error(
         source,
         _cursor_position,
         {:error, {meta, "unexpected operator" <> _, _token}}
       ) do
    line = get_line_from_meta(meta)
    remove_line(source, line)
  end

  defp fix_parse_error(_, nil, error) do
    error
  end

  defp fix_parse_error(
         source,
         {cursor_line_number, _},
         {:error, {meta, text = "missing terminator: " <> _, _}}
       )
       when is_integer(cursor_line_number) do
    line_end = get_line_from_meta(meta)

    terminator =
      case Regex.run(~r/terminator:\s([^\s]+)/u, text) do
        [_, terminator] -> terminator
      end

    line_start =
      case Regex.run(~r/line\s(\d+)/u, text) do
        [_, line] -> line |> String.to_integer()
      end

    if terminator in ["\"", "'", ")", "]", "}", ">>"] do
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
        [terminator | source]
      else
        source
      end
      |> Enum.reverse()
      |> Enum.join("\n")
    end
  end

  defp fix_parse_error(
         source,
         {cursor_line_number, _},
         {:error, {_meta, text = "missing interpolation terminator: \"}\"" <> _, _}}
       )
       when is_integer(cursor_line_number) do
    line_start =
      case Regex.run(~r/line\s(\d+)/u, text) do
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

  defp fix_parse_error(source, {cursor_line_number, _}, _error)
       when is_integer(cursor_line_number) do
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

  defp get_line_from_meta(meta) when is_integer(meta), do: meta
  defp get_line_from_meta(meta), do: Keyword.fetch!(meta, :line)
end
