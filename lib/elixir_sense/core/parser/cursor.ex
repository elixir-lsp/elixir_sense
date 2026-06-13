defmodule ElixirSense.Core.Parser.Cursor do
  @moduledoc """
  Places a `{:__cursor__, meta, args}` marker into a toxic2 AST based on the
  cursor position, using the `range:` metadata toxic2 attaches to every node.

  This replaces the previous approach of splicing a `__cursor__()` token into
  the source text and re-parsing. We find the deepest node whose source range
  contains the cursor and then either:

    * wrap an expression node — `{:__cursor__, meta, [node]}` — when the cursor
      sits on the node itself (a partial identifier, a call, an operator), or
    * insert an empty `{:__cursor__, meta, []}` marker into a *hole* — the body
      of a `do` block or the argument list of a call — when the cursor is in
      whitespace inside a container. Wrapping the container instead would
      capture the environment *outside* it (e.g. `module: nil` rather than the
      enclosing module), so holes must be entered, not wrapped.

  `ElixirSense.Core.Compiler` already understands both `__cursor__` shapes in
  every relevant position (function head, `@`, `defmodule`, `with`, macro
  args), so no compiler changes are needed here.
  """

  @doc """
  Returns `ast` with a single `__cursor__` marker placed at `cursor`, or
  unchanged when the cursor falls outside every ranged node.

  Options:

    * `:preserve_token` (default `false`) - when the cursor lands on a bare
      identifier/alias, keep it (wrap as `__cursor__(token)`) instead of
      dropping it. Navigation (definition/hover/references) sets this so the
      symbol under the cursor stays in the tree; completion leaves it false so
      a half-typed token is dropped.
  """
  @spec mark(Macro.t(), {pos_integer, pos_integer}, keyword()) :: Macro.t()
  def mark(ast, {_line, _column} = cursor, opts \\ []) do
    preserve? = Keyword.get(opts, :preserve_token, false)

    case walk(ast, cursor, preserve?) do
      {ast, true} -> ast
      # the cursor is past every ranged node (typically trailing whitespace or a
      # new line after a complete expression) - it is a new statement at the top
      # level
      {ast, false} -> append_to_root(ast, cursor)
    end
  end

  defp append_to_root({:__block__, meta, statements}, cursor),
    do: {:__block__, meta, insert_statement(statements, cursor)}

  defp append_to_root(nil, cursor), do: {:__block__, [], [marker(cursor)]}

  defp append_to_root(expression, cursor),
    do: {:__block__, [], insert_statement([expression], cursor)}

  # Depth-first, deepest match wins. Returns {new_node, found?}; only one node
  # is ever marked (the first/deepest found short-circuits the rest).

  # An error placeholder (positioned from its diagnostic by the caller) is
  # exactly a "more input expected here" hole - if the cursor sits in it, that
  # is where the cursor belongs.
  defp walk({:__error__, meta, _args} = node, cursor, _preserve?) do
    if contains?(meta, cursor), do: {marker(cursor), true}, else: {node, false}
  end

  defp walk({form, meta, args} = node, cursor, preserve?) when is_list(meta) do
    case walk(form, cursor, preserve?) do
      {form, true} ->
        {{form, meta, args}, true}

      {_form, false} ->
        case walk(args, cursor, preserve?) do
          {args, true} ->
            {{form, meta, args}, true}

          {_args, false} ->
            if contains?(meta, cursor) do
              {place(node, cursor, preserve?), true}
            else
              {node, false}
            end
        end
    end
  end

  defp walk({left, right}, cursor, preserve?) do
    case walk(left, cursor, preserve?) do
      {left, true} ->
        {{left, right}, true}

      {left, false} ->
        with {right, found} <- walk(right, cursor, preserve?), do: {{left, right}, found}
    end
  end

  defp walk(list, cursor, preserve?) when is_list(list), do: walk_list(list, cursor, preserve?)

  defp walk(other, _cursor, _preserve?), do: {other, false}

  defp walk_list([], _cursor, _preserve?), do: {[], false}

  defp walk_list([head | tail], cursor, preserve?) do
    case walk(head, cursor, preserve?) do
      {head, true} ->
        {[head | tail], true}

      {head, false} ->
        with {tail, found} <- walk_list(tail, cursor, preserve?), do: {[head | tail], found}
    end
  end

  # Decide how to place the marker at the deepest containing node. Error-node
  # holes are already handled in `walk/3`; here the cursor is in a structural
  # hole (an empty do/clause body) or on the node's own token.
  defp place({_form, _meta, _args} = node, cursor, preserve?) do
    cond do
      do_body?(node, cursor) -> insert_into_do(node, cursor, preserve?)
      # the keyword form `foo(...), do:` (no `do`/`end`) with the cursor in the
      # (empty) body value
      has_do_keyword?(node) -> insert_into_do(node, cursor, preserve?)
      stab_body?(node, cursor) -> insert_into_stab(node, cursor)
      true -> wrap(node, cursor, preserve?)
    end
  end

  # only the keyword form (`foo(...), do:`) - a `do`/`end` block carries `:do` in
  # its meta and is handled by `do_body?/2` (which respects the cursor position
  # relative to `do`)
  defp has_do_keyword?({_form, meta, args}) when is_list(args) do
    not Keyword.has_key?(meta, :do) and
      case List.last(args) do
        keyword when is_list(keyword) ->
          Keyword.keyword?(keyword) and Keyword.has_key?(keyword, :do)

        _ ->
          false
      end
  end

  defp has_do_keyword?(_node), do: false

  # cursor sits at or after the `->` of a stab clause - it belongs in the clause
  # body, where the clause's pattern variables are in scope
  defp stab_body?({:->, meta, [_args, _body]}, cursor) do
    case meta_point(meta) do
      {line, column} -> before_or_at?({line, column}, cursor)
      _ -> false
    end
  end

  defp stab_body?(_node, _cursor), do: false

  defp insert_into_stab({:->, meta, [args, body]}, cursor),
    do: {:->, meta, [args, insert_into_block(body, cursor)]}

  # A bare identifier / atom / alias at the cursor is a partial token being
  # typed. Like `Code.Fragment.container_cursor_to_quoted/2` we drop it and put
  # the cursor in its place (so e.g. `def foo|` yields `{:__unknown__, 0}` rather
  # than `{:foo, 0}`). Larger expressions are wrapped so their calls/structure
  # are still recorded.
  defp wrap(node, cursor, preserve?) do
    if partial_token?(node) and not preserve? do
      marker(cursor)
    else
      {:__cursor__, cursor_meta(cursor), [node]}
    end
  end

  defp partial_token?({name, _meta, nil}) when is_atom(name), do: true
  defp partial_token?({:__aliases__, _meta, _segments}), do: true
  defp partial_token?(_node), do: false

  # cursor sits at or after the `do` keyword of a do-block bearing node
  defp do_body?({_form, meta, _args}, cursor) do
    case Keyword.get(meta, :do) do
      [line: line, column: column] -> before_or_at?({line, column}, cursor)
      _ -> false
    end
  end

  defp insert_into_do({form, meta, args} = node, cursor, preserve?) do
    case List.pop_at(args, -1) do
      {keyword, rest} when is_list(keyword) ->
        if Keyword.has_key?(keyword, :do) do
          keyword = Keyword.update!(keyword, :do, &insert_into_block(&1, cursor))
          {form, meta, rest ++ [keyword]}
        else
          wrap(node, cursor, preserve?)
        end

      _ ->
        wrap(node, cursor, preserve?)
    end
  end

  defp insert_into_block({:__block__, meta, statements}, cursor),
    do: {:__block__, meta, insert_statement(statements, cursor)}

  defp insert_into_block(nil, cursor), do: {:__block__, [], [marker(cursor)]}

  defp insert_into_block(single, cursor), do: {:__block__, [], insert_statement([single], cursor)}

  @def_kinds [:def, :defp, :defmacro, :defmacrop, :defguard, :defguardp]

  defp insert_statement(statements, cursor) do
    {before, rest} = Enum.split_while(statements, &before_cursor?(&1, cursor))

    case List.last(before) do
      # the cursor directly follows a bodyless `def`/`defp`/... head - it is the
      # body being started, so attach it there (yields the enclosing function
      # scope rather than the module scope)
      {kind, meta, [head]} when kind in @def_kinds and is_tuple(head) ->
        List.replace_at(before, -1, {kind, meta, [head, [do: marker(cursor)]]}) ++ rest

      _ ->
        before ++ [marker(cursor)] ++ rest
    end
  end

  defp marker(cursor), do: {:__cursor__, cursor_meta(cursor), []}

  defp cursor_meta({line, column}), do: [line: line, column: column]

  # a statement/argument lies before the cursor when its source ends at or
  # before the cursor position
  defp before_cursor?(node, cursor) do
    case node_end(node) do
      nil -> true
      end_position -> before_or_at?(end_position, cursor)
    end
  end

  defp node_end({_form, meta, _args}) when is_list(meta) do
    case Keyword.get(meta, :range) do
      {_start, end_position} -> end_position
      _ -> meta_point(meta)
    end
  end

  defp node_end(_node), do: nil

  defp meta_point(meta) do
    case {Keyword.get(meta, :line), Keyword.get(meta, :column)} do
      {line, column} when is_integer(line) and is_integer(column) -> {line, column}
      _ -> nil
    end
  end

  defp contains?(meta, cursor) do
    cond do
      range_contains?(meta, cursor) -> true
      # an unterminated do-block (a `do` with no matching `end`) extends to the
      # end of the input, so it owns any cursor at or after its `do`
      unterminated_do?(meta) and after_do?(meta, cursor) -> true
      true -> false
    end
  end

  defp range_contains?(meta, cursor) do
    case Keyword.get(meta, :range) do
      {start_position, end_position} ->
        if before_or_at?(end_position, start_position) do
          # toxic2 gives incomplete nodes an inverted/zero-width range (the end
          # token is missing) - treat them as extending to the cursor
          before_or_at?(start_position, cursor)
        else
          before_or_at?(start_position, cursor) and before_or_at?(cursor, end_position)
        end

      _ ->
        false
    end
  end

  defp unterminated_do?(meta),
    do: Keyword.has_key?(meta, :do) and not Keyword.has_key?(meta, :end)

  defp after_do?(meta, cursor) do
    case Keyword.get(meta, :do) do
      [line: line, column: column] -> before_or_at?({line, column}, cursor)
      _ -> false
    end
  end

  defp before_or_at?({line1, column1}, {line2, column2}),
    do: line1 < line2 or (line1 == line2 and column1 <= column2)
end
