defmodule ElixirSense.Core.SurroundContext.Toxic do
  @moduledoc false

  # Toxic2 range-based "symbol under cursor" classifier for the NAVIGATION providers
  # (definition / references / implementation / declaration / hover / call_hierarchy).
  #
  # It returns the SAME shape as `Code.Fragment.surround_context/2`
  # (`:none | %{begin: {line, col}, end: {line, col}, context: context}`) so that
  # `ElixirSense.Core.SurroundContext.to_binding/2`, `Metadata.get_cursor_env/3` and the
  # locators themselves stay unchanged - only the call site flips to this module.
  #
  # The classification is derived from toxic2's `range:` node metadata (1-based, end-EXCLUSIVE,
  # the same coordinate system as `Code.Fragment`). A handful of shapes carry no range/meta in the
  # AST (bare `:atom` literals, `key:`/keyword keys), cannot be disambiguated from the tree (cursor
  # left of a meta-less dot operand such as `:erlang.foo`), or diverge from `Code.Fragment`
  # (uppercase multi-letter sigils, which it reports as `:alias`); for those we fall back to
  # `Code.Fragment.surround_context/2`. The whole function is wrapped so it is total and never
  # worse than the previous behavior.
  #
  # NOTE: completion (`Code.Fragment.cursor_context` / `container_cursor_to_quoted`) is out of
  # scope and stays on `Code.Fragment`.

  @spec surround_context(String.t(), {pos_integer, pos_integer}) :: :none | map()
  def surround_context(source, {line, column} = position) when is_binary(source) do
    {ast, _diagnostics} = Toxic2.parse_to_ast(source, token_metadata: true, range: true)

    case deepest_at(ast, {line, column}) do
      {node, parent} ->
        case classify(node, parent, {line, column}) do
          {context, begin_pos, end_pos} -> %{context: context, begin: begin_pos, end: end_pos}
          :fallback -> Code.Fragment.surround_context(source, position)
        end

      nil ->
        Code.Fragment.surround_context(source, position)
    end
  rescue
    _ -> Code.Fragment.surround_context(source, position)
  catch
    _, _ -> Code.Fragment.surround_context(source, position)
  end

  # --- deepest ranged node containing the cursor (with its structural parent) --------------

  # Returns `{node, parent}` for the deepest AST node whose `range:` contains the cursor
  # (end-exclusive), or `nil`. `parent` is the immediately enclosing AST node (or `nil`), used to
  # disambiguate name/alias leaves (`@attr`, `foo/1`, `%Struct{}`).
  defp deepest_at(ast, cursor), do: descend(ast, cursor, nil)

  # An `__aliases__` is a single alias unit (`Foo.Bar`, `__MODULE__.Foo`, `@attr.Foo`). Code.Fragment
  # classifies the whole thing, so do NOT descend into its segments - otherwise a ranged head
  # segment (`__MODULE__`/`@attr`) would be mistaken for a standalone var/attribute and lose the
  # post-dot qualifier. (All-atom segments carry no range, so only exotic heads would be descended
  # into; stopping here makes the alias clause - which falls back for exotic heads - reachable.)
  defp descend({:__aliases__, meta, _segs} = node, cursor, parent) do
    case node_range(meta) do
      nil -> nil
      range -> if contains?(range, cursor), do: {node, parent}, else: nil
    end
  end

  defp descend({_form, meta, _args} = node, cursor, parent) do
    case node_range(meta) do
      nil ->
        # No range of its own (e.g. the `{:., _, _}` dot operator node) - still descend so we can
        # reach ranged children.
        descend_children(node, cursor, node)

      range ->
        if contains?(range, cursor) do
          descend_children(node, cursor, node) || {node, parent}
        else
          nil
        end
    end
  end

  defp descend({_left, _right} = node, cursor, _parent) do
    # 2-tuple (keyword pair, `{key, value}`) - structural, no range of its own.
    descend_children(node, cursor, node)
  end

  defp descend(list, cursor, parent) when is_list(list) do
    # a list node (a `do:`/keyword-list argument) - descend into its elements so the cursor
    # inside a `do` block or keyword arg is still located.
    descend_first(list, cursor, parent)
  end

  defp descend(_literal, _cursor, _parent), do: nil

  defp descend_children({form, _meta, args}, cursor, parent) do
    descend_first([form | as_list(args)], cursor, parent)
  end

  defp descend_children({left, right}, cursor, parent) do
    descend_first([left, right], cursor, parent)
  end

  defp descend_first(items, cursor, parent) do
    Enum.find_value(items, fn item -> descend(item, cursor, parent) end)
  end

  defp as_list(args) when is_list(args), do: args
  defp as_list(_), do: []

  # --- classification --------------------------------------------------------------------------

  # Module attribute - the cursor is on the `@` node itself. Handles both `@attr` and forms with
  # an argument (`@type t :: ...`, `@spec f(...)`, `@moduledoc "..."`). The reported span is
  # `@`..(attribute-name end), matching Code.Fragment.
  defp classify({:@, ameta, [{attr, _, _}]}, _parent, cursor) when is_atom(attr) do
    attr_context(attr, ameta, cursor)
  end

  # Module attribute - the cursor is on the attribute NAME (its parent is the `@` node). This is
  # the deepest node for `@attr`, and also for `@type`/`@spec`/`@doc` whose name carries args.
  # The attribute-name node is the only direct child of an `@` node, so matching the `@` parent is
  # sufficient (an alias/value deeper inside the attribute has the name node as its parent, not @).
  defp classify({name, _, _}, {:@, ameta, _}, cursor) when is_atom(name) do
    attr_context(name, ameta, cursor)
  end

  # Leaf var/name with a disambiguating parent.
  defp classify({name, meta, nil} = node, parent, _cursor) when is_atom(name) do
    range = node_range(meta)

    cond do
      # `%var{}` / `%__MODULE__{}` - a var as a struct type. Code.Fragment reports `:struct` only at
      # a statement-leading `%`, but `:local_or_var` for a mid-expression one (`x = %var{}`); that
      # distinction is lexical (the AST is identical), so defer to Code.Fragment.
      struct_type?(parent, node) ->
        :fallback

      # `foo/1` - the name is the left operand of `{:/, _, [name, int]}`; report the NAME span only
      # (Code.Fragment returns :none when the cursor is on `/` or the arity, which we never reach
      # here because the name node's range does not cover those columns).
      arity_left?(parent, node) ->
        {{:local_arity, atom_charlist(name)}, range_begin(meta), range_end(meta)}

      range != nil ->
        {var_context(name), range_begin(meta), range_end(meta)}

      true ->
        :fallback
    end
  end

  # Alias - standalone or as a struct type.
  defp classify({:__aliases__, meta, segments} = node, parent, _cursor) do
    cond do
      not all_atoms?(segments) ->
        # exotic alias head (e.g. `__MODULE__.Foo`, `@attr.Foo`) - let Code.Fragment handle it
        :fallback

      struct_type?(parent, node) ->
        {:%, smeta, [^node, _map]} = parent
        {{:struct, dotted_charlist(segments)}, range_begin(smeta), range_end(meta)}

      true ->
        {{:alias, dotted_charlist(segments)}, range_begin(meta), range_end(meta)}
    end
  end

  # Struct when the cursor is on the `%` / between type and `{`.
  defp classify({:%, meta, [type, _map]}, _parent, _cursor) do
    case struct_type_context(type) do
      nil -> :fallback
      {context, type_end} -> {context, range_begin(meta), type_end}
    end
  end

  # Capture argument `&1`.
  defp classify({:&, meta, [int]}, _parent, _cursor) when is_integer(int) do
    {{:capture_arg, ~c"&" ++ Integer.to_charlist(int)}, range_begin(meta), range_end(meta)}
  end

  # Dot call `left.fun` (and remote calls). Classify only when the cursor is on/after the `.`;
  # a cursor left of the dot belongs to the left operand - if that operand had a range we would
  # have descended into it, so reaching here with cursor-left means a meta-less operand (`:erlang`)
  # and we fall back.
  defp classify({{:., dmeta, [left, sym]}, cmeta, _args}, _parent, cursor)
       when is_atom(sym) do
    dot_line = Keyword.get(dmeta, :line)
    dot_col = Keyword.get(dmeta, :column)
    {cl, cc} = cursor

    cond do
      # Synthetic dot calls toxic2 produces while lowering sugar - there is no real `.` in the
      # source, so there is no remote-call symbol to navigate to. Let Code.Fragment classify these
      # lexically. The markers: `from_brackets` (`x[y]` -> `Access.get`), `delimiter` (interpolated
      # atom `:"a#{x}"` -> `:erlang.binary_to_atom`), `from_interpolation` (string interpolation
      # `"a#{x}"` -> `Kernel.to_string`).
      synthetic_dot?(cmeta) or synthetic_dot?(dmeta) ->
        :fallback

      # multialias `Foo.{Bar, Baz}` - the `.{` is not a remote call symbol. The cursor on an inner
      # alias lands on that alias node directly; on the `.`/`{` Code.Fragment reports :none.
      sym == :{} ->
        :fallback

      dot_line == nil or dot_col == nil ->
        :fallback

      cl < dot_line or (cl == dot_line and cc < dot_col) ->
        :fallback

      true ->
        case inside_dot(left) do
          nil ->
            :fallback

          inside ->
            sym_str = Atom.to_string(sym)
            begin_pos = range_begin(cmeta)
            fin = {dot_line, dot_col + 1 + String.length(sym_str)}

            if contains?({begin_pos, fin}, cursor) do
              {{:dot, inside, String.to_charlist(sym_str)}, begin_pos, fin}
            else
              :fallback
            end
        end
    end
  end

  # Sigils, operators and local calls (all share the `{atom, meta, list}` shape).
  defp classify({form, meta, args}, _parent, cursor)
       when is_atom(form) and is_list(args) do
    arity = length(args)

    cond do
      # special forms (map/tuple/bitstring/block/anon-fn/stab) and the capture prefix `&` are not
      # callable/navigable names; their interior (keys, `->`, captured operator) is classified
      # lexically by Code.Fragment. (`&1`/`&2` capture args are handled by an earlier clause.)
      form in [:%{}, :{}, :<<>>, :__block__, :fn, :->, :&] ->
        :fallback

      # sigil `~r/.../` - Code.Fragment reports an uppercase multi-letter sigil name as an :alias,
      # so fall back for those; single-char (any case) and lowercase names map cleanly to :sigil.
      (letters = sigil_letters(form)) != nil ->
        begin_pos = range_begin(meta)

        cond do
          uppercase_multiletter?(letters) ->
            :fallback

          begin_pos == nil ->
            :fallback

          true ->
            {bl, bc} = begin_pos
            fin = {bl, bc + 1 + length(letters)}

            if contains?({begin_pos, fin}, cursor),
              do: {{:sigil, letters}, begin_pos, fin},
              else: :fallback
        end

      # arity notation `name/2` - cursor on the `/` or the arity is :none in Code.Fragment;
      # the name itself is handled by the leaf-with-/-parent clause. So fall back here.
      form == :/ and arity == 2 and arity_notation?(args) ->
        :fallback

      # Word operators (`and`/`or`/`in`/`not`/`when`...) read like identifiers; Code.Fragment
      # classifies them as :local_call or :operator depending on surrounding lexemes (e.g. `or (`).
      # Both map to the same binding, but to match exactly we let Code.Fragment decide.
      Macro.operator?(form, arity) and word_operator?(form) ->
        :fallback

      Macro.operator?(form, arity) ->
        op_line = Keyword.get(meta, :line)
        op_col = Keyword.get(meta, :column)
        op_str = Atom.to_string(form)

        if op_line && op_col do
          begin_pos = {op_line, op_col}
          fin = {op_line, op_col + String.length(op_str)}

          if contains?({begin_pos, fin}, cursor),
            do: {{:operator, String.to_charlist(op_str)}, begin_pos, fin},
            else: :fallback
        else
          :fallback
        end

      # a parenthesized local call `foo(...)` - report the NAME span only. Toxic marks the parens
      # with a `:closing` meta key. Without `:closing` the form is ambiguous: a complete `def foo`
      # is a `:local_or_var`, but an incomplete `some(` (unclosed paren) is a `:local_call`. Toxic
      # cannot tell those apart from meta, so we fall back to Code.Fragment's lexical analysis.
      Keyword.has_key?(meta, :closing) ->
        begin_pos = range_begin(meta)

        if begin_pos do
          {bl, bc} = begin_pos
          name_str = Atom.to_string(form)
          fin = {bl, bc + String.length(name_str)}

          if contains?({begin_pos, fin}, cursor),
            do: {{:local_call, String.to_charlist(name_str)}, begin_pos, fin},
            else: :fallback
        else
          :fallback
        end

      true ->
        :fallback
    end
  end

  defp classify(_node, _parent, _cursor), do: :fallback

  # --- inside_dot (left operand of a remote call) ----------------------------------------------

  defp inside_dot({:__aliases__, _, segs}) do
    if all_atoms?(segs), do: {:alias, dotted_charlist(segs)}, else: nil
  end

  defp inside_dot({:@, _, [{attr, _, nil}]}) when is_atom(attr),
    do: {:module_attribute, atom_charlist(attr)}

  defp inside_dot({name, _, nil}) when is_atom(name) do
    if name == :__MODULE__, do: {:var, ~c"__MODULE__"}, else: {:var, atom_charlist(name)}
  end

  defp inside_dot(atom) when is_atom(atom), do: {:unquoted_atom, atom_charlist(atom)}

  # A pure dot PATH `a.b.c` (no call args) - recurse. A dot CALL with args (`build(x).y`) makes the
  # receiver an expression, which Code.Fragment reports as `:expr`; we return nil so the caller
  # falls back.
  defp inside_dot({{:., _, [left, sym]}, _, []}) when is_atom(sym) do
    case inside_dot(left) do
      nil -> nil
      inside -> {:dot, inside, atom_charlist(sym)}
    end
  end

  defp inside_dot(_other), do: nil

  # --- struct type ----------------------------------------------------------------------------

  defp struct_type_context({:__aliases__, meta, segs}) do
    if all_atoms?(segs), do: {{:struct, dotted_charlist(segs)}, range_end(meta)}, else: nil
  end

  defp struct_type_context({:@, meta, [{attr, _, nil}]}) when is_atom(attr),
    do: {{:struct, {:module_attribute, atom_charlist(attr)}}, range_end(meta)}

  # var-type struct (`%var{}`) is left to Code.Fragment (see the leaf-clause note).
  defp struct_type_context(_other), do: nil

  # --- small helpers --------------------------------------------------------------------------

  defp var_context(:__MODULE__), do: {:local_or_var, ~c"__MODULE__"}
  defp var_context(name), do: {:local_or_var, atom_charlist(name)}

  # Module-attribute span: `@`..(attribute-name end), i.e. {@line, @col + 1 + len(name)}.
  # Reject non-identifier "names" (`@@` nests `@` nodes; recovery yields `:__error__`) - those are
  # not real attributes, so let Code.Fragment decide (it returns :none).
  defp attr_context(attr, ameta, cursor) do
    cond do
      not identifier_atom?(attr) ->
        :fallback

      true ->
        attr_context_span(attr, ameta, cursor)
    end
  end

  defp attr_context_span(attr, ameta, cursor) do
    case range_begin(ameta) do
      {al, ac} ->
        name_str = Atom.to_string(attr)
        begin_pos = {al, ac}
        fin = {al, ac + 1 + String.length(name_str)}

        if contains?({begin_pos, fin}, cursor),
          do: {{:module_attribute, String.to_charlist(name_str)}, begin_pos, fin},
          else: :fallback

      _ ->
        :fallback
    end
  end

  defp arity_left?({:/, _, [left, right]}, node), do: left == node and is_integer(right)
  defp arity_left?(_parent, _node), do: false

  defp arity_notation?([{name, _, nil}, int]) when is_atom(name) and is_integer(int), do: true
  defp arity_notation?(_args), do: false

  # A dot node toxic2 synthesized while lowering sugar (no real `.` in the source).
  defp synthetic_dot?(meta) when is_list(meta) do
    Keyword.has_key?(meta, :from_brackets) or Keyword.has_key?(meta, :delimiter) or
      Keyword.has_key?(meta, :from_interpolation)
  end

  defp synthetic_dot?(_meta), do: false

  # An atom that reads like a normal identifier (attribute / variable name).
  defp identifier_atom?(atom) do
    case Atom.to_string(atom) do
      "__error__" -> false
      <<first, _::binary>> -> first in ?a..?z or first == ?_
      _ -> false
    end
  end

  # A "word operator" - one whose name reads like an identifier (`and`, `or`, `in`, `not`, `when`).
  defp word_operator?(form) do
    case Atom.to_string(form) do
      <<first, _::binary>> -> first in ?a..?z
      _ -> false
    end
  end

  defp struct_type?({:%, _, [type, _map]}, node), do: type == node
  defp struct_type?(_parent, _node), do: false

  defp sigil_letters(atom) do
    case Atom.to_string(atom) do
      "sigil_" <> letters when letters != "" -> String.to_charlist(letters)
      _ -> nil
    end
  end

  defp uppercase_multiletter?([first | rest]) when rest != [], do: first in ?A..?Z
  defp uppercase_multiletter?(_letters), do: false

  defp all_atoms?(segs), do: is_list(segs) and Enum.all?(segs, &is_atom/1)

  defp dotted_charlist(segs) do
    segs |> Enum.map_join(".", &Atom.to_string/1) |> String.to_charlist()
  end

  defp atom_charlist(atom) when is_atom(atom),
    do: atom |> Atom.to_string() |> String.to_charlist()

  defp node_range(meta) when is_list(meta), do: Keyword.get(meta, :range)
  defp node_range(_meta), do: nil

  defp range_begin(meta) do
    case node_range(meta) do
      {begin_pos, _end_pos} -> begin_pos
      _ -> nil
    end
  end

  defp range_end(meta) do
    case node_range(meta) do
      {_begin_pos, end_pos} -> end_pos
      _ -> nil
    end
  end

  # cursor in [start, end) : start inclusive, end EXCLUSIVE (matches Code.Fragment :none at end).
  defp contains?({{sl, sc}, {el, ec}}, {cl, cc}) do
    after_or_at_start = cl > sl or (cl == sl and cc >= sc)
    strictly_before_end = cl < el or (cl == el and cc < ec)
    after_or_at_start and strictly_before_end
  end

  defp contains?(_range, _cursor), do: false
end
