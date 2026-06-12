defmodule ElixirSense.Core.TypeInference do
  @moduledoc """
  Converts Elixir AST nodes into *shape* terms understood by
  `ElixirSense.Core.Binding`.

  ## Relationship to the shape vocabulary

  Every value produced by `type_of/2` is a shape as defined in
  `ElixirSense.Core.Binding`'s `## Shape vocabulary` moduledoc section.
  Key invariants:

  - `nil` means "unknown / no information" — callers must treat it as the
    top type and not assume the absence of the value.
  - `:none` means "bottom / this branch never returns" — safe to drop from
    unions.
  - `{:map, fields, nil}` is a *closed* map (all keys known).
  - `{:map, fields, :open}` is an *open* map (additional unknown keys exist).
  - `{:list, :empty}` is the empty-list literal `[]`.
  - `{:optional, inner}` wraps a possibly-absent map field value; it should
    only appear as a map field value, never as a top-level expression type.
  - `:not_set` marks a map key known to be absent (from `not is_map_key/2`
    guards); it is only meaningful as a field value and is filtered out by
    field-listing consumers.

  See `ElixirSense.Core.Binding` for the full vocabulary, algebra rules,
  and the improper-list degradation policy.
  """
  alias ElixirSense.Core.ElixirTypes
  alias ElixirSense.Core.State.VarInfo
  alias Module.Types.Descr

  def type_of(
        {:%, _struct_meta,
         [
           _struct_ast,
           {:%{}, _map_meta, [{:|, _, _} | _]}
         ]},
        :match
      ),
      do: :none

  def type_of(
        {:%, _meta,
         [
           struct_ast,
           {:%{}, _, _} = ast
         ]},
        context
      ) do
    {fields, updated_struct} =
      case type_of(ast, context) do
        # Struct closedness derives from the loaded defstruct, not the map tail, so
        # normalise a `:closed` tail back to `nil` (no base).
        {:map, fields, :closed} -> {fields, nil}
        {:map, fields, updated_map} -> {fields, updated_map}
        {:struct, fields, _, updated_struct} -> {fields, updated_struct}
        _ -> {[], nil}
      end

    type = type_of(struct_ast, context) |> known_struct_type()

    {:struct, fields, type, updated_struct}
  end

  def type_of({{:., _, [target, fun]}, _meta, args} = ast, context)
      when is_atom(fun) and is_list(args) do
    # When native typing returns nil keep the `{:call, ...}` shape Binding relies on.
    with true <- ElixirTypes.expr_typing_enabled?(),
         native when not is_nil(native) <- type_of_with_elixir_types(ast, context) do
      native
    else
      _ ->
        target = type_of(target, context)
        {:call, target, fun, Enum.map(args, &type_of(&1, context))}
    end
  end

  # pinned variable
  def type_of({:^, _, [pinned]}, :match), do: type_of(pinned, nil)
  def type_of({:^, _, [_pinned]}, _context), do: :none

  # underscore: in :match it is `nil` (neutral in intersect, so `_ = expr` keeps
  # expr's type); elsewhere `:none` (not a valid standalone value). Handled in
  # every context so it never reaches native of_expr (which raises on bare `_`).
  def type_of({:_, _meta, var_context}, :match) when is_atom(var_context), do: nil
  def type_of({:_, _meta, var_context}, _context) when is_atom(var_context), do: :none

  def type_of({var, meta, var_context}, context)
      when is_atom(var) and is_atom(var_context) and
             var not in [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__, :_] and
             context != :match do
    case Keyword.fetch(meta, :version) do
      {:ok, version} ->
        {:variable, var, version}

      _ ->
        nil
    end
  end

  # attribute
  # expanded attribute reference has nil arg
  def type_of({:@, _, [{attribute, _, nil}]}, _context)
      when is_atom(attribute) do
    {:attribute, attribute}
  end

  # module or atom
  def type_of(atom, _context) when is_atom(atom) do
    {:atom, atom}
  end

  # map
  def type_of({:%{}, _meta, [{:|, _, _} | _]}, :match), do: :none

  def type_of({:%{}, _meta, ast}, context) do
    {updated_map, fields} =
      case ast do
        [{:|, _, [left, right]}] ->
          {type_of(left, context), right}

        list ->
          # No update base. The tail records map-literal completeness: `:closed` in
          # expression context (`%{a: 1}` constructs a map with exactly key `:a`),
          # `nil` (partial) in `:match` (a pattern matches any map having `:a`).
          tail = if context == :match, do: nil, else: :closed
          {tail, list}
      end

    field_types = get_fields_type(fields, context)

    case field_types |> Keyword.fetch(:__struct__) do
      {:ok, type} ->
        # Struct closedness derives from the loaded defstruct, so a `:closed` tail
        # collapses back to `nil` (no update base) in the struct's 4th slot.
        updated_struct = if updated_map == :closed, do: nil, else: updated_map

        {:struct, field_types |> Keyword.delete(:__struct__), type |> known_struct_type(),
         updated_struct}

      _ ->
        {:map, field_types, updated_map}
    end
  end

  # match
  def type_of({:=, _, [left, right]}, context) do
    intersect(type_of(left, :match), type_of(right, context))
  end

  # stepped range struct
  def type_of({:..//, _, [first, last, step]}, context) do
    {:struct,
     [
       first: type_of(first, context),
       last: type_of(last, context),
       step: type_of(step, context)
     ], {:atom, Range}, nil}
  end

  # range struct
  def type_of({:.., _, [first, last]}, context) do
    {:struct,
     [
       first: type_of(first, context),
       last: type_of(last, context),
       step: type_of(1, context)
     ], {:atom, Range}, nil}
  end

  @builtin_sigils %{
    sigil_D: Date,
    sigil_T: Time,
    sigil_U: DateTime,
    sigil_N: NaiveDateTime,
    sigil_R: Regex,
    sigil_r: Regex
  }

  # builtin sigil struct
  def type_of({sigil, _, _}, _context) when is_map_key(@builtin_sigils, sigil) do
    # TODO support custom sigils?
    {:struct, [], {:atom, @builtin_sigils |> Map.fetch!(sigil)}, nil}
  end

  # tuple
  # regular tuples use {:{}, [], [field_1, field_2]} ast
  # two element use {field_1, field_2} ast (probably as an optimization)
  # detect and convert to regular
  def type_of(ast, context) when is_tuple(ast) and tuple_size(ast) == 2 do
    type_of({:{}, [], Tuple.to_list(ast)}, context)
  end

  def type_of({:{}, _, list} = ast, context) do
    native = type_of_with_elixir_types(ast, context)

    if native do
      native
    else
      {:tuple, length(list), list |> Enum.map(&type_of(&1, context))}
    end
  end

  def type_of(list, context) when is_list(list) do
    native = type_of_with_elixir_types(list, context)

    if native do
      native
    else
      case list do
        [] ->
          {:list, :empty}

        [{:|, _, [head, tail]}] ->
          # `[a | b]`: a list tail folds its element type in (proper list); a known
          # non-list tail makes an improper `{:nonempty_list, elem, tail}` 3-tuple;
          # an unknown (`nil`) tail stays a conservative proper list.
          head_type = type_of(head, context)

          case type_of(tail, context) do
            {:list, :empty} ->
              {:list, head_type}

            {:list, tail_elem} ->
              {:list, union_element([head_type, tail_elem])}

            {:nonempty_list, tail_elem} ->
              {:list, union_element([head_type, tail_elem])}

            # Cons onto an already-improper tail stays improper with the same
            # final tail; the proper prefix gains `head_type`.
            {:nonempty_list, tail_elem, tail_tail} ->
              {:nonempty_list, union_element([head_type, tail_elem]), tail_tail}

            tail_shape ->
              # A concrete resolved non-list tail proves the cons improper; emit the
              # 3-tuple only in expression context. `:match` keeps the conservative
              # proper-list under-approximation because `precise_pattern_type`'s
              # cross-clause subtraction must not see an improper 3-tuple. Unknown
              # or deferred tails also stay the conservative proper list.
              if context != :match and improper_tail?(tail_shape) do
                {:nonempty_list, head_type, tail_shape}
              else
                {:list, head_type}
              end
          end

        elements ->
          # Element type is the union of every element's type (a trailing cons
          # `[a, b | t]` contributes its head `b`; the tail var is unknown).
          type =
            elements
            |> Enum.map(fn
              {:|, _, [head, _tail]} -> type_of(head, context)
              element -> type_of(element, context)
            end)
            |> union_element()

          {:list, type}
      end
    end
  end

  def type_of({:fn, _meta, clauses} = ast, context) do
    if ElixirTypes.expr_typing_enabled?() do
      type_of_with_elixir_types(ast, context)
    else
      # Without native expression typing, extract arity from the first clause.
      case clauses do
        [{:->, _, [args, _body]} | _] when is_list(args) ->
          arity = length(args)

          if arity == 0 do
            {:fun, 0}
          else
            {:fun, List.duplicate(nil, arity), nil}
          end

        _ ->
          nil
      end
    end
  end

  # block expressions
  def type_of({:__block__, _meta, exprs}, context) do
    case List.last(exprs) do
      nil -> nil
      last_expr -> type_of(last_expr, context)
    end
  end

  # `case` over a *call* scrutinee: a `{:case_result, …}` thunk so `Binding` can
  # drop clauses whose pattern can't match the scrutinee's resolved return type.
  # Scoped to call scrutinees (return type known from inferred signatures);
  # var/literal scrutinees keep the plain branch-union. Resolving the scrutinee
  # needs the binding env, hence the deferral.
  def type_of({:case, _meta, [scrutinee, [{:do, clauses} | _]]} = ast, context)
      when is_list(clauses) do
    scrutinee_type = type_of(scrutinee, context)

    if call_shape?(scrutinee_type) do
      specs =
        for {:->, _, [head, body]} <- clauses do
          pattern =
            case head do
              [p | _] -> p
              _ -> nil
            end

          {type_of(strip_when_guard(pattern), :match), branch_result_type(head, body, context)}
        end

      {:case_result, scrutinee_type, specs}
    else
      type_of_with_elixir_types(ast, context) || clause_result_type(ast, context)
    end
  end

  def type_of({form, _meta, _clauses} = ast, context)
      when form in [:cond, :try, :receive, :with, :if, :unless] do
    # Prefer native precision; otherwise fall back to a conservative union of the
    # construct's branch result types (an untypable branch widens it to `nil`).
    type_of_with_elixir_types(ast, context) || clause_result_type(ast, context)
  end

  # A `for` produces a list, the `:into` target, or — with `:reduce` — the union
  # of the seed value and every reduce-clause body. The opts keyword list is the
  # last clause; look options up by key rather than matching the head.
  def type_of({:for, _meta, clauses} = ast, context) when is_list(clauses) do
    type_of_with_elixir_types(ast, context) || for_result_type(List.last(clauses), context)
  end

  # A `<<>>` constructor is a `binary()` when every segment is byte-aligned,
  # otherwise a `bitstring()` (`<<1::1>>`). We only downgrade to bitstring when a
  # segment is explicitly sub-byte (`::bitstring`/`::bits`, or a literal bit size
  # not divisible by 8); everything else stays `binary()`.
  def type_of({:<<>>, _meta, segments} = ast, context) do
    type_of_with_elixir_types(ast, context) ||
      if Enum.any?(List.wrap(segments), &sub_byte_segment?/1),
        do: :bitstring,
        else: {:binary, nil}
  end

  # special forms
  # quote/unquote/import/alias/require etc. don't produce meaningful types
  def type_of({form, _meta, _clauses}, _context)
      when form in [
             :quote,
             :unquote,
             :unquote_splicing,
             :import,
             :alias,
             :require,
             :__aliases__,
             :__cursor__,
             :super,
             :"::"
           ],
      do: nil

  # __DIR__ always returns a binary (the current file's directory)
  def type_of({:__DIR__, _meta, _clauses}, _context), do: {:binary, nil}

  # __ENV__ is already expanded to map
  def type_of({form, _meta, _clauses}, _context) when form in [:__CALLER__] do
    {:struct, [], {:atom, Macro.Env}, nil}
  end

  def type_of({:__STACKTRACE__, _meta, _clauses}, _context) do
    {:list, nil}
  end

  def type_of({var, meta, args} = ast, context) when is_atom(var) and is_list(args) do
    # As with remote calls, fall back to the `{:local_call, ...}` shape Binding
    # relies on when native typing returns nil for this local call.
    with true <- ElixirTypes.expr_typing_enabled?(),
         native when not is_nil(native) <- type_of_with_elixir_types(ast, context) do
      native
    else
      _ ->
        line = Keyword.get(meta, :line, 1)
        column = Keyword.get(meta, :column, 1)
        {:local_call, var, {line, column}, Enum.map(args, &type_of(&1, context))}
    end
  end

  # integer
  def type_of(integer, _context) when is_integer(integer) do
    {:integer, integer}
  end

  # float
  def type_of(float, _context) when is_float(float) do
    {:float, float}
  end

  # binary/string literal
  def type_of(binary, _context) when is_binary(binary) do
    {:binary, binary}
  end

  # other
  def type_of(ast, context), do: type_of_with_elixir_types(ast, context)

  # Conservative result type of a clause construct (`case`/`cond`/`with`/`try`/
  # `receive`/`if`/`unless`): the union of each branch's body type, used when the
  # native engine doesn't compute these.
  #
  # Branches are `{head_or_nil, body}`. A body whose type is a variable bound by
  # its own `head` (`{:ok, v} -> v`) is widened to `nil`: that var is out of scope
  # at the result site, so storing it would leak a dangling thunk. Any `nil`
  # branch collapses the whole result to `nil`.
  defp clause_result_type(ast, context) do
    case clause_result_branches(ast) do
      [] ->
        nil

      branches ->
        types = Enum.map(branches, &branch_type(&1, context))

        if Enum.any?(types, &is_nil/1) do
          nil
        else
          case Enum.uniq(types) do
            [single] -> single
            members -> {:union, members}
          end
        end
    end
  end

  defp strip_when_guard({:when, _, [pattern | _]}), do: pattern
  defp strip_when_guard(other), do: other

  # A branch is normally `{head, body}`. A `with` failure branch is
  # `{:failure, pattern, rhs}`: the value reaching the implicit `else` is the RHS
  # minus the matched pattern, but only when the pattern is precise; otherwise the
  # whole RHS may fail to match. Mirrors `expr.ex:883`.
  defp branch_type({:failure, pattern, rhs}, context) do
    rhs_type = type_of(rhs, context)

    case precise_pattern_type(pattern) do
      {:ok, nil} -> rhs_type
      {:ok, pattern_type} -> {:difference, rhs_type, pattern_type}
      :imprecise -> rhs_type
    end
  end

  defp branch_type({head, body}, context), do: branch_result_type(head, body, context)

  @doc """
  Under-approximated type of a clause head pattern, for cross-clause subtraction.

  Subtracting an earlier clause's pattern from a later clause's scrutinee is only
  sound when the contributed type is an *under*-approximation of the value set the
  pattern actually matches. `type_of/2` over-approximates several patterns
  (`[h|t]` and `[x]` both become `{:list, _}`; `<<c, rest>>` becomes
  `{:binary, nil}`), so it cannot be used directly.

  Mirrors the compiler's `pattern_precise? and guard_precise?` gate
  (`Module.Types.Pattern`): a clause contributes only when its pattern is exact.

  Returns `{:ok, type}` when the clause may safely contribute `type`, or
  `:imprecise` when it must be skipped (the catch-all keeps the wider type).

  Rules (mirroring the compiler):

    * any non-trivial `when` guard ⇒ `:imprecise` (compiler `guard_precise?` is
      false for guards)
    * atom literals ⇒ precise
    * bare vars / `_` ⇒ precise (a first-binding var is `precise?` in the
      compiler; its type is `nil`, a subtraction no-op)
    * other literals (integers, floats, binaries) ⇒ `:imprecise` (the compiler
      types them as the whole `integer()`/`float()`/`binary()` domain, not the
      singleton, so they are not precise)
    * tuples (incl. 2-tuples) whose every element is exact-or-wildcard ⇒ precise
      (tagged tuples like `{:ok, _}` are the key case)
    * structs whose every field is exact-or-wildcard ⇒ precise
    * empty list `[]` ⇒ precise
    * cons patterns `[h | t]` ⇒ contribute at most a non-empty-list shape
      (`{:nonempty_list, nil}`), never the element types
    * fixed-length list patterns (`[x]`, `[a, b]`) ⇒ `:imprecise`
    * binary patterns with segments ⇒ `:imprecise`
    * maps ⇒ `:imprecise` (open in patterns; subtracting an open shape is unsound)
  """
  def precise_pattern_type({:when, _, _}), do: :imprecise

  # bare var / underscore — matches everything; contributes nil (subtraction no-op)
  def precise_pattern_type({name, _meta, ctx}) when is_atom(name) and is_atom(ctx),
    do: {:ok, nil}

  # pinned var: matches a single (unknown at this layer) value — not precise
  def precise_pattern_type({:^, _, [_pinned]}), do: :imprecise

  # atom literal
  def precise_pattern_type(atom) when is_atom(atom), do: {:ok, {:atom, atom}}

  # other literals: type_of widens to the whole domain — not precise
  def precise_pattern_type(literal)
      when is_integer(literal) or is_float(literal) or is_binary(literal),
      do: :imprecise

  # empty list
  def precise_pattern_type([]), do: {:ok, {:list, :empty}}

  # cons `[h | t]`: contribute at most a non-empty-list shape (element types
  # over-approximate, so drop them); fixed-length lists contribute nothing.
  def precise_pattern_type([{:|, _, [_head, _tail]}]), do: {:ok, {:nonempty_list, nil}}
  def precise_pattern_type(list) when is_list(list), do: :imprecise

  # 2-tuple
  def precise_pattern_type(tuple) when is_tuple(tuple) and tuple_size(tuple) == 2 do
    precise_pattern_type({:{}, [], Tuple.to_list(tuple)})
  end

  # n-tuple
  def precise_pattern_type({:{}, _, elements}) when is_list(elements) do
    case precise_elements(elements) do
      {:ok, types} -> {:ok, {:tuple, length(types), types}}
      :imprecise -> :imprecise
    end
  end

  # struct `%S{...}`: precise iff every field is exact-or-wildcard
  def precise_pattern_type({:%, _, [_struct_ast, {:%{}, _, [{:|, _, _} | _]}]}), do: :imprecise

  def precise_pattern_type({:%, _, [struct_ast, {:%{}, _, fields}]}) when is_list(fields) do
    with {:ok, field_types} <- precise_fields(fields) do
      struct_type = type_of(struct_ast, :match)

      module =
        case struct_type do
          {:atom, mod} -> {:atom, mod}
          _ -> nil
        end

      {:ok, {:struct, Keyword.delete(field_types, :__struct__), module, nil}}
    end
  end

  # maps and binaries with segments: imprecise
  def precise_pattern_type({:%{}, _, _}), do: :imprecise
  def precise_pattern_type({:<<>>, _, _}), do: :imprecise

  # `=` match: both sides must match; precise iff both sides are precise. Use the
  # narrower (whichever is concrete); if either is imprecise, the whole is.
  def precise_pattern_type({:=, _, [left, right]}) do
    with {:ok, left_type} <- precise_pattern_type(left),
         {:ok, right_type} <- precise_pattern_type(right) do
      {:ok, intersect(left_type, right_type)}
    end
  end

  # anything else (calls, etc.): not a safe-to-subtract pattern
  def precise_pattern_type(_other), do: :imprecise

  defp precise_elements(elements) do
    Enum.reduce_while(elements, {:ok, []}, fn element, {:ok, acc} ->
      case precise_pattern_type(element) do
        {:ok, type} -> {:cont, {:ok, [type | acc]}}
        :imprecise -> {:halt, :imprecise}
      end
    end)
    |> case do
      {:ok, acc} -> {:ok, Enum.reverse(acc)}
      :imprecise -> :imprecise
    end
  end

  defp precise_fields(fields) do
    Enum.reduce_while(fields, {:ok, []}, fn
      {key, value}, {:ok, acc} when is_atom(key) ->
        case precise_pattern_type(value) do
          {:ok, type} -> {:cont, {:ok, [{key, type} | acc]}}
          :imprecise -> {:halt, :imprecise}
        end

      _other, _acc ->
        {:halt, :imprecise}
    end)
    |> case do
      {:ok, acc} -> {:ok, Enum.reverse(acc)}
      :imprecise -> :imprecise
    end
  end

  defp branch_result_type(nil, body, context), do: type_of(body, context)

  defp branch_result_type(head, body, context) do
    case type_of(body, context) do
      {:variable, name, _} = var ->
        if MapSet.member?(head_var_names(head), name), do: nil, else: var

      other ->
        other
    end
  end

  defp clause_result_branches({:case, _, [_subject, blocks]}) when is_list(blocks),
    do: clause_branches(blocks[:do])

  defp clause_result_branches({:cond, _, [blocks]}) when is_list(blocks),
    do: clause_branches(blocks[:do])

  defp clause_result_branches({:receive, _, [blocks]}) when is_list(blocks),
    do: clause_branches(blocks[:do]) ++ clause_branches(blocks[:after])

  defp clause_result_branches({:try, _, [blocks]}) when is_list(blocks) do
    # On success the value is the `do` body, or an `else` clause body when `else`
    # is present. `rescue`/`catch` bodies also contribute; `after` does not.
    main = if blocks[:else], do: clause_branches(blocks[:else]), else: [{nil, blocks[:do]}]
    main ++ clause_branches(blocks[:rescue]) ++ clause_branches(blocks[:catch])
  end

  defp clause_result_branches({form, _, [_cond, blocks]})
       when form in [:if, :unless] and is_list(blocks),
       do: [{nil, blocks[:do]}, {nil, blocks[:else]}]

  defp clause_result_branches({:with, _, args}) when is_list(args) do
    {generators, opts} = with_split(args)

    cond do
      not (is_list(opts) and Keyword.has_key?(opts, :do)) ->
        []

      Keyword.has_key?(opts, :else) ->
        # `else` clauses handle the failure space; the `do` body is typed against
        # the generators' bound vars.
        [{with_patterns(generators), opts[:do]} | clause_branches(opts[:else])]

      true ->
        # Without `else`, a failed `<-` returns its unmatched RHS, so the result is
        # `do_body | each <- (RHS minus the matched pattern)`; the pattern is
        # subtracted only when precise (`{:failure, ...}` gates this).
        bound = with_patterns(generators)

        [
          {bound, opts[:do]}
          | for({:<-, _, [pattern, rhs]} <- generators, do: {:failure, pattern, rhs})
        ]
    end
  end

  defp clause_result_branches(_other), do: []

  defp with_split(args) do
    case List.last(args) do
      opts when is_list(opts) -> {Enum.drop(args, -1), opts}
      _ -> {args, nil}
    end
  end

  # The patterns a `with`'s generators bind (`<-` and `=`); used as the "head" of
  # the `do`/failure branches so bodies referencing those vars widen out.
  defp with_patterns(generators) do
    for {op, _, [pattern, _rhs]} <- generators, op in [:<-, :=], do: pattern
  end

  defp clause_branches(clauses) when is_list(clauses),
    do: for({:->, _, [heads, body]} <- clauses, do: {heads, body})

  defp clause_branches(_other), do: []

  # Variable names introduced by a clause head. Pinned variables (`^x`) reference
  # outer scope, so the pin subtree is replaced with an atom so prewalk skips it.
  defp head_var_names(head) do
    {_ast, names} =
      Macro.prewalk(head, MapSet.new(), fn
        {:^, _, [_pinned]}, acc ->
          {:__pinned__, acc}

        {name, _meta, ctx} = node, acc when is_atom(name) and is_atom(ctx) and name != :_ ->
          {node, MapSet.put(acc, name)}

        node, acc ->
          {node, acc}
      end)

    names
  end

  defp clause_bodies(clauses) when is_list(clauses),
    do: for({:->, _, [_heads, body]} <- clauses, do: body)

  # Collapse a list of element types into a single element type: one type stays
  # itself, several become a `{:union, …}` (normalized later by `Binding`).
  defp union_element(types) do
    case Enum.uniq(types) do
      [single] -> single
      members -> {:union, members}
    end
  end

  # Is this shape a concrete, resolved non-list value that, as a cons tail, makes
  # the list improper? Only scalars and structural shapes count — not list shapes,
  # `nil`/`:none`, or deferred reference forms which may still resolve to a list.
  defp improper_tail?(:atom), do: true
  defp improper_tail?(:boolean), do: true
  defp improper_tail?({:atom, _}), do: true
  defp improper_tail?(:integer), do: true
  defp improper_tail?({:integer, _}), do: true
  defp improper_tail?(:float), do: true
  defp improper_tail?({:float, _}), do: true
  defp improper_tail?(:number), do: true
  defp improper_tail?(:binary), do: true
  defp improper_tail?({:binary, _}), do: true
  defp improper_tail?(:bitstring), do: true
  defp improper_tail?(:pid), do: true
  defp improper_tail?(:port), do: true
  defp improper_tail?(:reference), do: true
  defp improper_tail?(:tuple), do: true
  defp improper_tail?({:tuple, _, _}), do: true
  defp improper_tail?({:tuple_open, _}), do: true
  defp improper_tail?({:map, _, _}), do: true
  defp improper_tail?({:struct, _, _, _}), do: true
  defp improper_tail?(:fun), do: true
  defp improper_tail?({:fun, _}), do: true
  defp improper_tail?({:fun, _, _}), do: true
  defp improper_tail?(_other), do: false

  # A binary segment is sub-byte (makes the whole `<<>>` a bitstring) only when it
  # is an explicit `::bitstring`/`::bits`, or an integer/bit segment with a size
  # not divisible by 8. `binary`/`bytes`/`utf*`/`float` parts stay byte-aligned.
  defp sub_byte_segment?({:"::", _, [_value, spec]}) do
    parts = flatten_segment_spec(spec)

    cond do
      Enum.any?(parts, &(&1 in [:binary, :bytes, :utf8, :utf16, :utf32, :float])) -> false
      Enum.any?(parts, &(&1 in [:bitstring, :bits])) -> true
      true -> Enum.any?(parts, &(is_integer(&1) and rem(&1, 8) != 0))
    end
  end

  # A segment without `::` is the default (integer, 8 bits — byte-aligned).
  defp sub_byte_segment?(_other), do: false

  defp flatten_segment_spec({:-, _, [left, right]}),
    do: flatten_segment_spec(left) ++ flatten_segment_spec(right)

  defp flatten_segment_spec({:*, _, [left, right]}),
    do: flatten_segment_spec(left) ++ flatten_segment_spec(right)

  defp flatten_segment_spec({:size, _, [n]}) when is_integer(n), do: [n]
  defp flatten_segment_spec({name, _, _}) when is_atom(name), do: [name]
  defp flatten_segment_spec(n) when is_integer(n), do: [n]
  defp flatten_segment_spec(_other), do: []

  # The result type of a `for` from its (last-clause) opts. `:reduce` wins over
  # `:into`/list because its opts keyword list also carries `:do`.
  defp for_result_type(opts, context) when is_list(opts) do
    cond do
      not Keyword.keyword?(opts) -> nil
      Keyword.has_key?(opts, :reduce) -> reduce_result_type(opts, context)
      Keyword.has_key?(opts, :into) -> type_of_into_target(opts[:into])
      Keyword.has_key?(opts, :do) -> {:list, nil}
      true -> nil
    end
  end

  defp for_result_type(_opts, _context), do: nil

  # `for ..., reduce: seed do acc -> body end` returns the seed value unioned
  # with every reduce-clause body's type (matching upstream).
  defp reduce_result_type(opts, context) do
    seed_type = type_of(Keyword.fetch!(opts, :reduce), context)

    body_types =
      case Keyword.get(opts, :do) do
        clauses when is_list(clauses) ->
          clauses |> clause_bodies() |> Enum.map(&type_of(&1, context))

        _ ->
          []
      end

    case Enum.uniq([seed_type | body_types]) do
      [single] -> single
      members -> {:union, members}
    end
  end

  @doc """
  Type an expression with compiler state and env context.

  When native typing is enabled, seeds variable types and module/function/file
  context from the compiler state into the native inference engine. Falls back to
  `type_of/2` when native typing is disabled or returns nil.
  """
  # A `case` over a *call* scrutinee is typed by our feasibility-aware
  # `{:case_result, …}` thunk rather than native (native types the result as the
  # clause body even when a clause can't match). Everything else prefers native.
  def type_of({:case, _, [scrutinee | _]} = ast, context, state, env) do
    if call_shape?(type_of(scrutinee, context)) do
      type_of(ast, context)
    else
      type_of_with_state(ast, context, state, env)
    end
  end

  def type_of(ast, context, state, env), do: type_of_with_state(ast, context, state, env)

  defp type_of_with_state(ast, context, %{vars_info: [current_scope | _]} = _state, env)
       when is_map(current_scope) do
    if ElixirTypes.expr_typing_enabled?() do
      env_context = build_env_context(current_scope, env)
      native_result = type_of_with_elixir_types(ast, context, nil, nil, env_context)
      native_result || type_of(ast, context)
    else
      type_of(ast, context)
    end
  end

  defp type_of_with_state(ast, context, _state, _env), do: type_of(ast, context)

  defp call_shape?({:call, _, _, _}), do: true
  defp call_shape?({:local_call, _, _, _}), do: true
  defp call_shape?(_other), do: false

  defp build_env_context(current_scope, env) do
    vars =
      current_scope
      |> Map.values()
      |> Enum.filter(&match?(%VarInfo{}, &1))

    # `env` is a compiler env struct (no Access behaviour), so use Map.get
    # rather than `env[:key]`, which would raise.
    %{
      module: Map.get(env, :module),
      function: Map.get(env, :function),
      file: Map.get(env, :file),
      vars: vars
    }
  end

  # Helper to use ElixirTypes adaptor as fallback
  defp type_of_with_elixir_types(ast, context) do
    type_of_with_elixir_types(ast, context, nil, nil, %{})
  end

  # Helper to use ElixirTypes adaptor with optional local signatures and metadata
  # Used by both the internal type_of/4 path and the external type_of_with_elixir_types/5 API
  def type_of_with_elixir_types(
        ast,
        _context,
        local_sigs_map,
        metadata \\ nil,
        env_context \\ %{}
      ) do
    # General expression typing requires the expected-type API (1.19+); see
    # ElixirTypes.expr_typing_enabled?/0.
    if ElixirTypes.expr_typing_enabled?() do
      case type_expr_with_local_sigs(ast, local_sigs_map, metadata, env_context) do
        {:ok, descr} -> ElixirTypes.to_shape(descr)
        :error -> nil
      end
    else
      nil
    end
  end

  # Helper to type an expression with local signatures and metadata
  defp type_expr_with_local_sigs(ast, local_sigs_map, metadata, env_context) do
    signatures =
      if is_map(local_sigs_map) and not remote_call_ast?(ast) do
        Map.drop(local_sigs_map, [:__module__])
      else
        nil
      end

    module =
      Map.get(env_context, :module) ||
        (is_map(local_sigs_map) && Map.get(local_sigs_map, :__module__)) ||
        extract_module_from_context(ast)

    ElixirTypes.of_expr(ast,
      module: module,
      function: Map.get(env_context, :function),
      file: Map.get(env_context, :file),
      mode: :dynamic,
      local_sigs_map: signatures,
      metadata: metadata,
      variables: variables_from_env_context(env_context)
    )
  end

  # Fallback module for type_of/2 path (when no compiler env is available)
  defp extract_module_from_context(_ast), do: nil

  # Build ElixirTypes variables map from the elixir_sense compiler env_context
  defp variables_from_env_context(env_context) do
    vars = if is_map(env_context), do: Map.get(env_context, :vars), else: nil

    if is_list(vars) do
      Enum.reduce(vars, %{}, fn
        %VarInfo{name: name, version: version, elixir_types_descr: descr}, acc
        when is_atom(name) and is_integer(version) ->
          descr = descr || Descr.dynamic()
          Map.put(acc, {name, version}, descr)

        _, acc ->
          acc
      end)
    else
      %{}
    end
  end

  defp remote_call_ast?({{:., _, [_target, fun]}, _, args}) when is_atom(fun) and is_list(args),
    do: true

  defp remote_call_ast?(_), do: false

  # Infer the result type of a `for` comprehension with `:into`
  defp type_of_into_target(target) when is_binary(target), do: {:binary, nil}
  defp type_of_into_target(target) when is_list(target), do: {:list, nil}
  defp type_of_into_target({:%{}, _, []}), do: {:map, [], nil}
  defp type_of_into_target({:%{}, _, [{:|, _, _} | _]}), do: {:map, [], nil}

  defp type_of_into_target(_), do: nil

  defp get_fields_type(fields, context) do
    for {field, value} <- fields do
      case field do
        atom when is_atom(atom) ->
          {atom, type_of(value, context)}

        # Non-atom keys (`%{"asd" => a}`, `%{n => v}`) are preserved as domain
        # keys `{{:domain, key_type}, value_type}` (rendered `key => value`;
        # property completion skips them as they aren't statically nameable).
        other ->
          {{:domain, type_of(other, context)}, type_of(value, context)}
      end
    end
  end

  # expand struct type - only compile type atoms or attributes are supported
  # variables supported in match context
  defp known_struct_type(type) do
    case type do
      {:atom, atom} -> {:atom, atom}
      {:attribute, attribute} -> {:attribute, attribute}
      {:variable, variable, version} -> {:variable, variable, version}
      _ -> nil
    end
  end

  def find_typed_vars(ast, match_context, context) do
    {_ast, {vars, _match_context, _context}} =
      Macro.prewalk(ast, {[], match_context, context}, &match_var(&1, &2))

    Enum.uniq(vars)
  end

  defp match_var({:when, _, [left, _right]}, match_context) do
    # no variables can be defined in guard context so we skip that subtree
    match_var(left, match_context)
  end

  defp match_var(
         {:=, _meta,
          [
            left,
            right
          ]},
         {vars, match_context, context}
       ) do
    {_ast, {vars, _match_context, _context}} =
      match_var(
        left,
        {vars, intersect(match_context, type_of(right, context)), :match}
      )

    {_ast, {vars, _match_context, _context}} =
      match_var(
        right,
        {vars, intersect(match_context, type_of(left, :match)), context}
      )

    {nil, {vars, nil, context}}
  end

  # pinned variable
  defp match_var(
         {:^, _meta, [{var, _var_meta, var_context}]},
         {vars, match_context, context}
       )
       when is_atom(var) and is_atom(var_context) do
    {nil, {vars, match_context, context}}
  end

  # variable
  defp match_var(
         {var, meta, var_context},
         {vars, match_context, :match}
       )
       when is_atom(var) and is_atom(var_context) and
              var not in [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__, :_] do
    case Keyword.fetch(meta, :version) do
      {:ok, version} ->
        {nil, {[{{var, version}, match_context} | vars], nil, :match}}

      _ ->
        {nil, {vars, match_context, :match}}
    end
  end

  defp match_var({:%, _, [type_ast, {:%{}, _, _ast} = map_ast]}, {vars, match_context, context}) do
    {_ast, {type_vars, _match_context, _context}} =
      match_var(
        type_ast,
        {[],
         propagate_context(
           match_context,
           &{:map_key, &1, type_of(:__struct__, context)}
         ), context}
      )

    {_ast, {map_vars, _match_context, _context}} =
      match_var(map_ast, {[], match_context, context})

    {nil, {vars ++ map_vars ++ type_vars, nil, context}}
  end

  defp match_var({:%{}, _, ast}, {vars, match_context, context}) do
    {updated_vars, list} =
      case ast do
        [{:|, _, [left, right]} | _] ->
          if context == :match do
            # map update is forbidden in match, we're in invalid code
            {[], []}
          else
            {_ast, {updated_vars, _match_context, _context}} = match_var(left, {[], nil, context})
            {updated_vars, right}
          end

        list ->
          {[], list}
      end

    destructured_vars =
      list
      |> Enum.flat_map(fn
        {key, value_ast} ->
          key_type = type_of(key, context)

          {_ast, {new_vars, _match_context, _context}} =
            match_var(
              value_ast,
              {[], propagate_context(match_context, &{:map_key, &1, key_type}), context}
            )

          new_vars
      end)

    {nil, {vars ++ destructured_vars ++ updated_vars, nil, context}}
  end

  # regular tuples use {:{}, [], [field_1, field_2]} ast
  # two element use `{field_1, field_2}` ast (probably as an optimization)
  # detect and convert to regular
  defp match_var(ast, {vars, match_context, context})
       when is_tuple(ast) and tuple_size(ast) == 2 do
    match_var({:{}, [], ast |> Tuple.to_list()}, {vars, match_context, context})
  end

  defp match_var({:{}, _, ast}, {vars, match_context, context}) do
    destructured_vars =
      ast
      |> Enum.with_index()
      |> Enum.flat_map(fn {nth_elem_ast, n} ->
        {_ast, {new_vars, _match_context, _context}} =
          match_var(
            nth_elem_ast,
            {[], propagate_context(match_context, &{:tuple_nth, &1, n}), context}
          )

        new_vars
      end)

    {nil, {vars ++ destructured_vars, nil, context}}
  end

  defp match_var({{:., _, [:erlang, :++]}, _, [left, right]}, {vars, match_context, context})
       when is_list(left) do
    # NOTE this may produce improper lists
    match_var(left ++ right, {vars, match_context, context})
  end

  defp match_var(list, {vars, match_context, context}) when is_list(list) do
    match_var_list = fn head, tail ->
      {_ast, {new_vars_head, _match_context, _context}} =
        match_var(head, {[], propagate_context(match_context, &{:list_head, &1}), context})

      {_ast, {new_vars_tail, _match_context, _context}} =
        match_var(tail, {[], propagate_context(match_context, &{:list_tail, &1}), context})

      {nil, {vars ++ new_vars_head ++ new_vars_tail, nil, context}}
    end

    case list do
      [] ->
        {nil, {vars, nil, context}}

      [{:|, _, [head, tail]}] ->
        match_var_list.(head, tail)

      [head | tail] ->
        match_var_list.(head, tail)
    end
  end

  # Binary/bitstring patterns — variables inside get type based on their segment spec
  defp match_var({:<<>>, _, segments}, {vars, _match_context, context}) do
    destructured_vars =
      segments
      |> Enum.flat_map(fn
        {:"::", _, [var_ast, type_spec]} ->
          # Determine the type from the binary segment type spec
          segment_type = binary_segment_type(type_spec)

          {_ast, {new_vars, _match_context, _context}} =
            match_var(var_ast, {[], segment_type, context})

          new_vars

        _ ->
          []
      end)

    {nil, {vars ++ destructured_vars, nil, context}}
  end

  defp match_var(ast, {vars, _match_context, context}) do
    # traverse literals, not expanded macro calls with nil match_context
    # we cannot assume anything basing on match_context on variables there
    {ast, {vars, nil, context}}
  end

  # Extract type from binary segment type spec (e.g., ::binary, ::integer-8, ::utf8)
  defp binary_segment_type({:-, _, [left, _right]}), do: binary_segment_type(left)
  defp binary_segment_type({:binary, _, _}), do: {:binary, nil}
  defp binary_segment_type({:bytes, _, _}), do: {:binary, nil}
  # `::bitstring`/`::bits` are not byte-aligned, so they are `bitstring()`, a
  # supertype of `binary()`.
  defp binary_segment_type({:bitstring, _, _}), do: :bitstring
  defp binary_segment_type({:bits, _, _}), do: :bitstring
  defp binary_segment_type({:utf8, _, _}), do: {:integer, nil}
  defp binary_segment_type({:utf16, _, _}), do: {:integer, nil}
  defp binary_segment_type({:utf32, _, _}), do: {:integer, nil}
  defp binary_segment_type({:integer, _, _}), do: {:integer, nil}
  defp binary_segment_type({:float, _, _}), do: {:float, nil}
  defp binary_segment_type(_), do: {:integer, nil}

  defp propagate_context(nil, _), do: nil
  defp propagate_context(:none, _), do: :none
  defp propagate_context(match_context, fun), do: fun.(match_context)

  def intersect(nil, new), do: new
  def intersect(old, nil), do: old
  def intersect(:none, _), do: :none
  def intersect(_, :none), do: :none
  def intersect(old, old), do: old

  def intersect({:intersection, old}, {:intersection, new}) do
    {:intersection, Enum.uniq(old ++ new)}
  end

  def intersect({:intersection, old}, new) do
    {:intersection, Enum.uniq([new | old])}
  end

  def intersect(old, {:intersection, new}) do
    {:intersection, Enum.uniq([old | new])}
  end

  def intersect(old, new), do: {:intersection, [old, new]}
end
