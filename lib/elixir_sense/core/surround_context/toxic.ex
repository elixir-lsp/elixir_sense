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
  # A few exotic shapes are classified MORE precisely than `Code.Fragment` (which is purely
  # lexical) and so intentionally diverge from it: operator-name arity captures `&>=/2`/`&+/2`
  # resolve to a navigable `:local_arity` (Code.Fragment reports a stray `:operator "/"`),
  # `&(-&1)` keeps the unary `-`/`&1` classifications (Code.Fragment reports `:none`), and a bare
  # var in an unspaced step range `a..b//c` stays a `:local_or_var`. These are kept because they
  # drive better navigation than matching the lexical oracle would.
  #
  # NOTE: completion (`Code.Fragment.cursor_context` / `container_cursor_to_quoted`) is out of
  # scope and stays on `Code.Fragment`.

  @spec surround_context(String.t(), {pos_integer, pos_integer}) :: :none | map()
  def surround_context(source, {line, column} = position) when is_binary(source) do
    # `literal_encoder` wraps literals (notably bare `:atom`s) in `{:__block__, meta, [literal]}` so
    # they carry a `range:` and can be classified from the parse tree instead of falling back to the
    # lexical Code.Fragment. (Alias segments, struct types and attribute names are NOT literals and
    # stay unwrapped.)
    {ast, _diagnostics} =
      Toxic2.parse_to_ast(source,
        token_metadata: true,
        range: true,
        literal_encoder: fn literal, meta -> {:ok, {:__block__, meta, [literal]}} end
      )

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
  defp classify({:@, ameta, [{attr, attr_meta, _}]}, _parent, cursor) when is_atom(attr) do
    attr_context(attr, attr_meta, ameta, cursor)
  end

  # Module attribute - the cursor is on the attribute NAME (its parent is the `@` node). This is
  # the deepest node for `@attr`, and also for `@type`/`@spec`/`@doc` whose name carries args.
  # The attribute-name node is the only direct child of an `@` node, so matching the `@` parent is
  # sufficient (an alias/value deeper inside the attribute has the name node as its parent, not @).
  defp classify({name, nmeta, _}, {:@, ameta, _}, cursor) when is_atom(name) do
    attr_context(name, nmeta, ameta, cursor)
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
            # the end is the end of the function NAME. `cmeta` line/column point at the name start
            # (not the dot), so this stays correct when the name is on a different line than the dot
            # (`A.\n  bar`); deriving it from `dmeta` would synthesize an impossible same-line end and
            # break get_call_arity.
            call_line = Keyword.get(cmeta, :line)
            call_col = Keyword.get(cmeta, :column)

            if begin_pos && call_line && call_col do
              fin = {call_line, call_col + String.length(sym_str)}

              if contains?({begin_pos, fin}, cursor),
                do: {{:dot, inside, String.to_charlist(sym_str)}, begin_pos, fin},
                else: :fallback
            else
              :fallback
            end
        end
    end
  end

  # A literal wrapped by the literal_encoder. Three atom shapes are classified from the parse tree;
  # everything else (quoted/operator atoms, multi-line) defers to Code.Fragment. The kinds are told
  # apart by their single-line source width (no source re-read needed):
  #   * keyword key `key:`  - `format: :keyword`; navigable span EXCLUDES the trailing colon
  #     (Code.Fragment returns :none when the cursor is on the colon), so width-of-name == len(atom)
  #   * bare `:atom`        - range spans the leading colon, width == len(atom) + 1  -> :unquoted_atom
  #   * bare nil/true/false - no colon, width == len(atom)                            -> :keyword
  defp classify({:__block__, meta, [atom]}, _parent, cursor) when is_atom(atom) do
    name_len = String.length(Atom.to_string(atom))

    case node_range(meta) do
      {{sl, sc}, {el, ec}} when sl == el ->
        cond do
          # keyword key (range covers `name:`); the key itself is `name` (drop the colon)
          Keyword.get(meta, :format) == :keyword and ec - 1 - sc == name_len ->
            span = {{sl, sc}, {el, ec - 1}}

            if contains?(span, cursor),
              do: {{:key, atom_charlist(atom)}, {sl, sc}, {el, ec - 1}},
              else: :fallback

          # `:atom` (leading colon)
          identifier_first_char?(atom) and ec - sc == name_len + 1 ->
            {{:unquoted_atom, atom_charlist(atom)}, {sl, sc}, {el, ec}}

          # bare nil / true / false reserved words (no colon)
          atom in [nil, true, false] and ec - sc == name_len ->
            {{:keyword, atom_charlist(atom)}, {sl, sc}, {el, ec}}

          true ->
            :fallback
        end

      _ ->
        :fallback
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
        # The step-range operator `a..b//c` lowers to a single ternary `:..//` node whose meta
        # points at the `..`. Code.Fragment classifies it lexically as two separate operators -
        # `..` (over the `..`) and `//` (over the `//`). Report `..` for the `..` columns and let
        # the `//` columns fall back to Code.Fragment (the `..//` node's range does not reach them).
        op_str = if form == :..//, do: "..", else: Atom.to_string(form)

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

  # the literal_encoder wraps a bare-atom dot operand (`:erlang.foo`) - unwrap it (identifier atoms
  # only; operator atoms like `:%{}` are not navigable)
  defp inside_dot({:__block__, _, [atom]}) when is_atom(atom) do
    if identifier_first_char?(atom), do: {:unquoted_atom, atom_charlist(atom)}
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
  # not real attributes, so let Code.Fragment decide (it returns :none). Also require the name to be
  # contiguous with the `@` (starting exactly one column after it): a spaced `@ attr` is the unary
  # `@` operator applied to a local var, which Code.Fragment classifies as `:local_or_var`.
  defp attr_context(attr, name_meta, ameta, cursor) do
    cond do
      not identifier_atom?(attr) ->
        :fallback

      not attr_contiguous?(name_meta, ameta) ->
        :fallback

      true ->
        attr_context_span(attr, ameta, cursor)
    end
  end

  defp attr_contiguous?(name_meta, ameta) do
    case {range_begin(ameta), range_begin(name_meta)} do
      {{al, ac}, {nl, nc}} -> nl == al and nc == ac + 1
      _ -> false
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

  defp arity_left?({:/, _, [left, right]}, node), do: left == node and int_literal?(right)
  defp arity_left?(_parent, _node), do: false

  # an atom Code.Fragment would treat as a navigable unquoted atom (`:foo`, `:Foo`, `:_x`) - i.e. it
  # reads as an identifier; operator/special atoms (`:+`, `:%{}`, `:.`) are NOT.
  defp identifier_first_char?(atom) do
    case Atom.to_string(atom) do
      <<c, _::binary>> -> c in ?a..?z or c in ?A..?Z or c == ?_
      _ -> false
    end
  end

  # an integer operand, raw or wrapped by the literal_encoder (`{:__block__, _, [int]}`)
  defp int_literal?(int) when is_integer(int), do: true
  defp int_literal?({:__block__, _, [int]}), do: is_integer(int)
  defp int_literal?(_), do: false

  # `<ref>/<int>` is ambiguous from the AST between arity notation (`foo/1`, `A.bar/1`, `&f/1`) and
  # integer division (`x / 1`); both lower to `{:/, _, [left, int]}`. The distinction is lexical, so
  # whenever the right operand is an integer we defer the `/` to Code.Fragment (which returns :none
  # on an arity slash and `:operator` on a division slash). Covers local AND remote arity.
  defp arity_notation?([_left, right]), do: int_literal?(right)
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
