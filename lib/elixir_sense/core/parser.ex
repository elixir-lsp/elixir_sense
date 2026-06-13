defmodule ElixirSense.Core.Parser do
  @moduledoc """
  Core Parser

  Parsing is delegated to the error tolerant toxic2 parser. It always returns a
  best effort AST (with `{:__error__, meta, %{}}` placeholder nodes on
  unparsable parts) plus a diagnostic stream. The cursor position, when given,
  is located via toxic2's `range:` metadata and marked with a `__cursor__` node
  (see `ElixirSense.Core.Parser.Cursor`).
  """

  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.MetadataBuilder
  alias ElixirSense.Core.Parser.Cursor
  alias ElixirSense.Core.Source
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
  def parse_string(source, _try_to_fix_parse_error, try_to_fix_line_not_found, cursor_position) do
    unless String.valid?(source) do
      raise ArgumentError, message: "invalid string passed to parse_string"
    end

    # parse a CLEAN tree (no `__cursor__` marker) so the metadata - calls, vars,
    # references, lines_to_env - reflects the real code. The precise cursor
    # environment is derived separately in `Metadata.get_cursor_env/3` (a marked
    # parse), exactly so the marker never corrupts this metadata.
    {ast, error} = tolerant_parse(source)
    acc = MetadataBuilder.build(ast, cursor_position)

    if cursor_position == nil or acc.cursor_env != nil or
         Map.has_key?(acc.lines_to_env, elem(cursor_position, 0)) or
         !try_to_fix_line_not_found do
      create_metadata(source, {:ok, acc, error})
    else
      case try_fix_line_not_found_by_inserting_marker(source, cursor_position) do
        {:ok, acc} ->
          create_metadata(source, {:ok, acc, error || {:error, :env_not_found}})

        _ ->
          create_metadata(source, {:ok, acc, error || {:error, :env_not_found}})
      end
    end
  end

  @doc """
  Error tolerant parse via toxic2. Returns `{:ok, ast, source, error}` where
  `error` is `nil` or `{:error, :parse_error}`. With `errors_threshold: 0`
  (strict) a parse error is returned as `{:error, :parse_error}` instead.

  The returned AST is the clean recovered tree - no `__cursor__` marker is
  injected even when a `:cursor_position` is given (the marker is only relevant
  to environment building, see `parse_string/4`). This keeps the AST safe for
  general consumers such as code transforms / `Macro.to_string/1`.
  """
  def string_to_ast(source, options \\ []) when is_binary(source) do
    errors_threshold = Keyword.get(options, :errors_threshold, 6)

    case tolerant_parse(source) do
      {ast, nil} -> {:ok, ast, source, nil}
      {ast, error} when errors_threshold > 0 -> {:ok, ast, source, error}
      {_ast, error} -> error
    end
  end

  @doc false
  # Parse `source` with the cursor marked at `cursor_position` and return the
  # `{meta, env}` cursor environment (or `nil`). Used by
  # `ElixirSense.Core.Metadata.get_cursor_env/3`.
  #
  # `preserve_token: true` keeps the identifier under the cursor (for
  # navigation - definition/hover/references), otherwise a half-typed token is
  # dropped (for completion).
  def cursor_env(source, {_line, _column} = cursor_position, opts \\ []) do
    {ast, _error} = tolerant_parse(source, cursor_position, opts)
    MetadataBuilder.build(ast, cursor_position).cursor_env
  end

  @doc """
  Public toxic2 parse for consumers that want the raw best-effort AST plus the
  diagnostic stream, with `{:__error__, ...}` placeholder nodes neutralized so
  `MetadataBuilder`/`Macro` traversal never crash.

  `keep_range: true` preserves the `range:` metadata toxic2 attaches to every
  source node (needed by range-aware consumers such as the language server
  parser and the document-symbol / folding / selection-range providers); the
  default strips it to keep the classic `line:`/`column:` shape used by metadata
  building. No `__cursor__` marker is injected - this is a clean recovered tree.

  Returns `{ast, diagnostics}` (the toxic2 diagnostic stream, unmodified).
  """
  def parse_to_neutralized_ast(source, opts \\ []) when is_binary(source) do
    keep_range = Keyword.get(opts, :keep_range, false)

    parser_options =
      opts
      |> Keyword.take([:token_metadata, :range, :existing_atoms_only, :literal_encoder])
      |> Keyword.put_new(:token_metadata, true)

    {ast, diagnostics} = Toxic2.parse_to_ast(source, parser_options)
    {neutralize_errors(ast, diagnostics, keep_range), diagnostics}
  end

  @doc false
  # Parse with toxic2 and place the cursor marker. Returns `{ast, error}`.
  def tolerant_parse(source, cursor_position \\ nil, opts \\ []) do
    cursor =
      case cursor_position do
        {line, column} when is_integer(line) and is_integer(column) -> cursor_position
        _ -> nil
      end

    parser_options =
      if cursor, do: [token_metadata: true, range: true], else: [token_metadata: true]

    {ast, diagnostics} = Toxic2.parse_to_ast(source, parser_options)

    ast =
      if cursor do
        ast |> position_error_nodes(diagnostics) |> Cursor.mark(cursor, opts)
      else
        ast
      end

    ast = neutralize_errors(ast, diagnostics)

    error = if Enum.any?(diagnostics, &(elem(&1, 2) == :error)), do: {:error, :parse_error}

    {ast, error}
  end

  # Attach a source range to each `{:__error__, meta, %{diag_ids: ids}}` node
  # from its diagnostic span (toxic2 error nodes carry no position of their own).
  # This lets the cursor walk locate a hole even when it is nested where the
  # surrounding node's range does not reach (e.g. `@type x :: [`).
  defp position_error_nodes(ast, diagnostics) do
    by_id = Map.new(diagnostics, fn diagnostic -> {elem(diagnostic, 0), diagnostic} end)
    position_error_nodes(ast, by_id, :walk)
  end

  defp position_error_nodes({:__error__, meta, %{diag_ids: diag_ids} = args}, by_id, :walk) do
    case Map.get(by_id, List.first(diag_ids || [])) do
      {_id, _phase, _severity, _code, sl, sc, el, ec, _details} ->
        {:__error__, [range: {{sl, sc}, {el, ec}}, line: sl, column: sc] ++ meta, args}

      _ ->
        {:__error__, meta, args}
    end
  end

  defp position_error_nodes({form, meta, args}, by_id, :walk),
    do: {position_error_nodes(form, by_id, :walk), meta, position_error_nodes(args, by_id, :walk)}

  defp position_error_nodes({left, right}, by_id, :walk),
    do: {position_error_nodes(left, by_id, :walk), position_error_nodes(right, by_id, :walk)}

  defp position_error_nodes(list, by_id, :walk) when is_list(list),
    do: Enum.map(list, &position_error_nodes(&1, by_id, :walk))

  defp position_error_nodes(other, _by_id, :walk), do: other

  # toxic2 `{:__error__, meta, %{diag_ids: ids}}` nodes are placeholders for
  # "expected more input here". They are recovery artifacts, never real AST, so
  # we drop them from any list (argument lists, containers, statement blocks) -
  # keeping them would inflate call/definition arities. A cursor that needs to
  # live in such a hole has already been substituted by `Cursor.mark/3` before
  # this runs. Any stray error node not in a list is rewritten to a harmless
  # `{:__error__, meta, []}` (its map args crash `Macro` traversal and the
  # compiler). Range metadata, only needed transiently for cursor marking, is
  # stripped here too so the AST keeps the usual `line:`/`column:` shape.
  # `keep_range` (default false) controls whether the `range:` meta survives -
  # metadata building wants it stripped (classic shape), range-aware consumers
  # want it kept (see `parse_to_neutralized_ast/2`).
  def neutralize_errors(ast, diagnostics, keep_range \\ false) do
    by_id = Map.new(diagnostics, fn diagnostic -> {elem(diagnostic, 0), diagnostic} end)
    neutralize(ast, by_id, keep_range)
  end

  defp neutralize({:__error__, meta, args}, _by_id, keep_range) when not is_list(args),
    do: {:__error__, strip_range(meta, keep_range), []}

  defp neutralize({form, meta, args}, by_id, keep_range) when is_list(args) do
    args = if call_form?(form), do: clean_call_args(args, by_id), else: args

    {neutralize(form, by_id, keep_range), strip_range(meta, keep_range),
     Enum.map(args, &neutralize(&1, by_id, keep_range))}
  end

  defp neutralize({form, meta, args}, by_id, keep_range),
    do:
      {neutralize(form, by_id, keep_range), strip_range(meta, keep_range),
       neutralize(args, by_id, keep_range)}

  defp neutralize({left, right}, by_id, keep_range),
    do: {neutralize(left, by_id, keep_range), neutralize(right, by_id, keep_range)}

  defp neutralize(list, by_id, keep_range) when is_list(list),
    do: Enum.map(list, &neutralize(&1, by_id, keep_range))

  defp neutralize(other, _by_id, _keep_range), do: other

  # Clean a call's argument list of error artifacts so its arity matches user
  # intent: always drop the "missing closing delimiter" sentinel; if the only
  # remaining args are error placeholders (an empty `foo(`) drop them too so the
  # arity is 0. A nascent argument after real ones (a trailing comma, `foo(a,`)
  # is kept so the arity reflects the slot being typed.
  defp clean_call_args(args, by_id) do
    args = Enum.reject(args, &missing_close_error?(&1, by_id))
    if args != [] and Enum.all?(args, &error_node?/1), do: [], else: args
  end

  defp missing_close_error?({:__error__, _meta, %{diag_ids: diag_ids}}, by_id) do
    case Map.get(by_id, List.first(diag_ids || [])) do
      {_id, _phase, _severity, :expected_comma_or_close, _sl, _sc, _el, _ec, _details} -> true
      _ -> false
    end
  end

  defp missing_close_error?(_node, _by_id), do: false

  defp error_node?({:__error__, _meta, _args}), do: true
  defp error_node?(_node), do: false

  defp call_form?({:., _meta, _args}), do: true

  defp call_form?(form) when is_atom(form),
    do:
      form not in [:__block__, :__aliases__, :__cursor__, :%{}, :{}, :<<>>, :%, :^] and
        not Macro.operator?(form, 1) and not Macro.operator?(form, 2)

  defp call_form?(_form), do: false

  defp strip_range(meta, true), do: meta
  defp strip_range(meta, _keep_range) when is_list(meta), do: Keyword.delete(meta, :range)
  defp strip_range(meta, _keep_range), do: meta

  def try_fix_line_not_found_by_inserting_marker(
        source,
        {cursor_line_number, _} = cursor_position
      )
      when is_integer(cursor_line_number) do
    # last resort when the cursor lands on a line with no environment: append a
    # textual marker to that line so a `__cursor__` node is produced there
    modified_source = fix_line_not_found(source, cursor_line_number)
    {ast, _error} = tolerant_parse(modified_source)
    acc = MetadataBuilder.build(ast, cursor_position)

    if acc.cursor_env != nil or Map.has_key?(acc.lines_to_env, cursor_line_number) do
      {:ok, acc}
    else
      :not_found
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
      records: acc.records,
      uses: acc.uses,
      mods_funs_to_positions: acc.mods_funs_to_positions,
      cursor_env: acc.cursor_env,
      closest_env: acc.closest_env,
      lines_to_env: acc.lines_to_env,
      vars_info_per_scope_id: acc.vars_info_per_scope_id,
      calls: acc.calls,
      first_alias_positions: acc.first_alias_positions,
      moduledoc_positions: acc.moduledoc_positions
    }
  end

  defp fix_line_not_found(source, line_number) when is_integer(line_number) do
    source
    |> Source.split_lines()
    # by replacing a line here we risk introducing a syntax error
    # instead we append marker to the existing line
    |> List.update_at(line_number - 1, &(&1 <> "; " <> marker()))
    |> Enum.join("\n")
  end

  defp marker, do: "(__cursor__())"
end
