defmodule ElixirSense.Core.Binding do
  @moduledoc """
  Binding holds the type environment (variables, attributes, specs, etc.) for
  a single cursor position and drives the structural type-algebra engine used
  for completion, hover, and go-to-definition.

  ## Shape vocabulary

  Every resolved type is represented as a *shape* — a plain Elixir term.
  Unless noted, shapes are produced by `expand/3` / `do_expand/3` and consumed
  by `TypePresentation` (rendering), `covers?/2` (subsumption),
  `combine_intersection/2`, and `normalize_union/1`.

  ### `nil` — unknown / top

  "No information; the value could be anything." Produced by any fallback
  `do_expand` clause, `parse_type` for `:term`/`:any`/`:dynamic`, unresolvable
  variables, and rescued errors.

  Algebra: a `nil` member *poisons* a union to `nil` (absorbing); `nil ∩ T = T`
  (top/identity); `covers?(nil, _b)` is `true` (covers anything — reached only
  inside tuple recursion). Renders as `"term()"` inside structures.

  ### `:none` — bottom

  "This branch never produces a value" (`raise`, dead code, type error).
  Produced by `parse_type` for `no_return()`, unresolvable attribute, non-map
  base in map update, struct with invalid module, `:erlang.error/throw/exit`,
  `fetch_env!` returning `:error`.

  Algebra: `:none` members are *dropped* from unions (an all-`:none` union
  collapses to `:none`); `:none ∩ T = :none` (annihilator); not a valid LHS of
  `covers?` (falls through to `false`). Renders `"none()"`. Note: `expand_call`
  for union targets filters `:none` and `nil` together, so a fully-`:none`
  union of targets returns `nil` (conservative — avoids false dead-code claims).

  ### `:not_set` — known-absent map value

  A map key provably *not present* (`not is_map_key(map, k)`). Produced by
  `ElixirTypes.to_shape_eager/1` for a `not_set()` descr and by
  `type_inference/guard.ex`'s `not_set_map_type/1` for `not is_map_key` guards.
  Consumers (`TypePresentation.fields_for_receiver/2`, `CompletionEngine`)
  filter out `:not_set` fields. Only meaningful as a *field value*; `do_expand`
  has no clause for it (falls through to `nil`). Renders `"not_set()"`.

  ### `{:list, :empty}` / `:empty_list`

  Two spellings for the empty list `[]`. `{:list, :empty}` is the canonical
  internal form (`parse_type` of `[]`, `Tuple.to_list` of `{}`, `List.wrap(nil)`).
  `:empty_list` is the `ElixirTypes.to_shape` alias, mapped back to
  `{:list, :empty}` by `do_expand`. Both are subsumed by `:list` and
  `{:list, nil}`; both render `"empty_list()"`.

  ### List shapes — proper vs improper (`{:nonempty_list, elem, tail}`)

  - `{:list, elem}` — PROPER list INCLUDING `[]` (`elem` `nil` = unknown).
  - `{:nonempty_list, elem}` — PROPER NON-EMPTY list.
  - `{:nonempty_list, elem, tail}` — NON-EMPTY POSSIBLY-IMPROPER list: a proper
    prefix of `elem` terminated by `tail`. Producers only emit the 3-tuple when
    `tail` is a KNOWN NON-LIST shape, so it always denotes a genuinely improper
    list.

    Producers: `ElixirTypes.to_shape/1` of `non_empty_list(a, tail)`; the `++`
    rule (known non-empty proper LHS `++` known non-list RHS); `TypeInference`
    for a cons `[h | t]` with a non-list tail.

    Algebra (conservative — improper is a separate kind): proper lists do NOT
    cover the 3-tuple and vice versa; the 3-tuple covers only an equal-kind
    3-tuple (elementwise). `combine_intersection` intersects two 3-tuples
    elementwise (a `:none` tail bottoms the shape); any proper/improper mix
    falls to `nil`. `shape_kind` keeps the 3-tuple at `:list`-kind so the
    disjointness fallback never claims `:none` against a proper list.

    Rendering: `non_empty_list(elem, tail)`. Coercion probes
    `Descr.non_empty_list/2`; if absent, falls back to `Descr.dynamic()` — NOT
    a widened proper list (improper lists are not in `list(t)`; that would be
    unsound). Left out of the `descr_exact?` whitelist.

  ### Map tail — `:closed` vs `nil` (partial) vs `:open`

  The third element of `{:map, fields, tail}` records whether keys beyond
  `fields` may exist:

  - `:closed` ("literal-complete"): all keys known. Producers (only where
    completeness is CERTAIN): `TypeInference` for a map literal in EXPRESSION
    context (`%{a: 1}` constructs exactly `:a`; as a PATTERN it is `nil`);
    `ElixirTypes.to_shape/1` of a closed descr; `Map.new/0`,
    `Map.put`/`merge`/`delete`/`update` on a `:closed` base, `Map.from_struct/1`
    of a resolved struct.

  - `nil` ("partial"): at least these keys present; closedness UNKNOWN. The
    conservative default. Producers: map literals in `:match` context; guard
    facts; `parse_type` for typespec maps.

  - `:open`: additional unknown keys DO exist. Produced by an unresolvable map
    update (`%{m | a: 1}` → `{:map, [a: nil], :open}`), `expand_map_base` on an
    unknown base, `Map.from_struct` on an unresolvable struct.

  Algebra: `covers?({:map, [], tail}, ...)` with `tail in [nil, :open]` is
  `true` (empty-field partial/open map is the map top; `:closed` `%{}` is the
  empty map, excluded). `merge_tails/2`: `:open` if either is `:open`, `:closed`
  only if BOTH, else `nil`. `intersect_tails/2`: `:closed` if either, else
  `nil`. `map_key` access: a key missing from a `:closed` map is `:not_set`
  (provably absent); missing from `nil`-partial/`:open` is `nil`. SOUNDNESS: a
  `nil`-partial map must NEVER yield `:not_set`. Coercion: `:closed` →
  `closed_map` (EXACT); `nil`/`:open` → `open_map`.

  Rendering (deliberate compromise): `:closed` and `nil`-partial both render
  WITHOUT a marker (`%{key: type}`) — surfacing the distinction would churn
  guard-fact display expectations. `:open` keeps its `...` marker.

  ### `{:dynamic, _}` — display-only, not in Binding algebra

  `{:dynamic, nil}`/`{:dynamic, inner}` are defined only in `TypePresentation`
  (`segment/1`, `widen_literals/1`). No `do_expand` clause handles them, and
  `to_shape/1` strips `{:dynamic, [], []}` → `nil` and `{:dynamic, [], [inner]}`
  → `quoted_to_shape(inner)`, so they never enter the Binding algebra. If
  first-class gradual representation is ever needed, a `{:dynamic, inner}` shape
  with a `do_expand` clause must be introduced explicitly rather than silently
  degrading to `nil`.

  ### `:term`-rendering shapes

  Display aliases carrying no extra information beyond their kind:

  | Shape            | Renders as          | Notes                                 |
  |------------------|---------------------|---------------------------------------|
  | `:atom`          | `"atom()"`          | generic atom                          |
  | `:integer`       | `"integer()"`       | generic integer (also `{:integer,nil}`) |
  | `{:integer, v}`  | `"5"` (literal)     | pinned integer                        |
  | `:float`         | `"float()"`         | generic float                         |
  | `:binary`        | `"binary()"`        | generic binary                        |
  | `:number`        | `"number()"`        | `integer() | float()`                 |
  | `:pid`/`:port`/`:reference` | as named | scalar builtins                |
  | `:tuple`         | `"tuple()"`         | generic tuple (any arity)             |
  | `:fun`           | `"fun()"`           | generic function                      |
  | `:list`          | `"list()"`          | generic list (subsumes proper list shapes) |
  | `:empty_map`     | `"empty_map()"`     | `%{}` from Descr                      |
  | `:non_struct_map`| `"non_struct_map()"` | open non-struct map from Descr       |
  | `:boolean`       | `"boolean()"`       | `false | true`                        |
  | `:bitstring`     | `"bitstring()"`     | superset of `:binary`                 |

  ### `{:optional, _}` — map-field-only wrapper

  A map key that may or may not be present; when present, its value has the
  inner type. Produced by `ElixirTypes.to_shape_eager/1` for an `:optional`
  descr key and `quoted_to_shape` for `{:if_set, [], [inner]}`. `do_expand`
  preserves the wrapper (dropping it if the inner expands to `nil`); `covers?`
  treats it transparently (inner shapes compared, avoiding spurious
  disjointness); `uninformative_field?` and `fields_for_receiver/2` keep
  optional fields (renders `"if_set(inner)"`). Only meaningful as a *field
  value*.

  ---

  ## Improper-list policy

  The shape algebra models non-empty improper lists via the
  `{:nonempty_list, elem, tail}` 3-tuple. Three points:

  1. **`to_shape` boundary**: `{:non_empty_list, [], [elem, tail]}` converts to
     the 3-tuple when both sides convert, else `nil`. A proper `non_empty_list`
     (no tail arg) stays the 2-tuple.

  2. **`++` with non-list RHS**: known NON-EMPTY proper LHS `++` known NON-LIST
     RHS yields the 3-tuple. A possibly-empty LHS stays `nil` (`[] ++ x == x`
     could be the bare RHS); an empty-list LHS yields the RHS; unknown RHS stays
     `nil`.

  3. **Cons patterns**: UNCHANGED — `[head | _]` parses to `{:list, head}`
     (terminator dropped). Typespec `maybe_improper_list`/`nonempty_improper_list`
     remain sound over-approximations; only cons in EXPRESSION context produces
     the 3-tuple.

  Backlog: improper lists from typespecs and the proper/improper intersection
  mix stay coarse.
  """

  require Logger

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.ElixirTypes
  alias ElixirSense.Core.ExCkReader
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.ModuleResolver
  alias ElixirSense.Core.Normalized.Typespec
  alias ElixirSense.Core.State
  alias ElixirSense.Core.Struct
  alias ElixirSense.Core.TypeInfo

  # TODO refactor to use env
  defstruct structs: %{},
            vars: [],
            attributes: [],
            aliases: [],
            module: nil,
            function: nil,
            functions: [],
            macros: [],
            requires: [],
            specs: %{},
            types: %{},
            mods_funs_to_positions: %{},
            cursor_position: {1, 1},
            elixir_types_local_sigs: %{}

  # Tail-algebra predicates (guard-safe). `tail_top?` = a map tail that is the
  # map TOP (`nil` partial or `:open`); used where a `%{}`/`map()` top subsumes
  # anything. `tail_intersectable?` = a tail intersection may proceed under
  # (`nil` partial or `:closed`), but NOT `:open`. The third spelling `tail in
  # [nil, :open, :closed]` ("any resolved map") is left inline at its sites.
  defguardp tail_top?(tail) when tail in [nil, :open]
  defguardp tail_intersectable?(tail) when tail in [nil, :closed]

  def from_env(%State.Env{} = env, %ElixirSense.Core.Metadata{} = metadata, cursor_position) do
    from_env(env, metadata, cursor_position, [])
  end

  @doc """
  Like `from_env/3` but accepts options.

  ## Options

    * `:local_sigs` — a precomputed local sigs map (the result of
      `ElixirTypes.build_local_sigs_map/2`). When given, the rebuild is skipped
      and the supplied map is used verbatim (a per-request memoization hook).
      Passing a stale or wrong-module map only affects native sig lookups, never
      the correctness of the structural engine.
  """
  def from_env(%State.Env{} = env, %ElixirSense.Core.Metadata{} = metadata, cursor_position, opts)
      when is_list(opts) do
    local_sigs =
      case Keyword.fetch(opts, :local_sigs) do
        {:ok, sigs} when is_map(sigs) ->
          sigs

        _ ->
          if ElixirTypes.enabled?() and env.module do
            ElixirTypes.build_local_sigs_map(metadata, env.module)
          else
            %{}
          end
      end

    %Binding{
      vars: env.vars,
      attributes: env.attributes,
      aliases: env.aliases,
      structs: metadata.structs,
      functions: env.functions,
      macros: env.macros,
      requires: env.requires,
      specs: metadata.specs,
      module: env.module,
      function: env.function,
      types: metadata.types,
      mods_funs_to_positions: metadata.mods_funs_to_positions,
      cursor_position: cursor_position,
      elixir_types_local_sigs: local_sigs
    }
  end

  defp get_fields_from({:map, fields, _}), do: fields
  defp get_fields_from({:struct, fields, _, _}), do: fields
  defp get_fields_from(_), do: []

  defp get_struct_fields(%Binding{structs: structs}, fields, module) do
    if Struct.is_struct(module, structs) do
      fields_values =
        for field <- Struct.get_fields(module, structs), field != :__struct__ do
          {field, fields[field]}
        end

      struct =
        case fields[:__struct__] do
          nil -> {:atom, module}
          other -> other
        end

      {Keyword.put(fields_values, :__struct__, struct), module}
    else
      {Keyword.put_new(fields, :__struct__, nil), nil}
    end
  end

  def expand(%Binding{} = env, expanded, stack \\ []) do
    res =
      unless expanded in stack do
        do_expand(env, expanded, [expanded | stack])
      end

    case res do
      {:struct, _, _, _} ->
        do_expand(env, res, [res | stack])

      {:map, _, _} ->
        do_expand(env, res, [res | stack])

      _ ->
        res
    end
  end

  def expand_type(%Binding{} = env, remote_type_ast, args, include_private, stack \\ []) do
    # Handle already-wrapped call forms (e.g., {{:., _, [mod, type]}, _, call_args})
    ast =
      case remote_type_ast do
        {{:., _, _} = dot, _meta, call_args} when is_list(call_args) ->
          {dot, [], args ++ call_args}

        _ ->
          {remote_type_ast, [], args}
      end

    parse_type(env, ast, env.module, include_private, stack)
  end

  def do_expand(%Binding{} = env, {:intersection, variants}, stack) do
    combined =
      variants
      |> Enum.reduce(nil, fn variant, acc ->
        combine_intersection(acc, expand(env, variant, stack))
      end)

    expand(env, combined, stack)
  end

  def do_expand(%Binding{vars: variables} = env, {:variable, variable, version}, stack) do
    sorted_variables = Enum.sort_by(variables, &{&1.name, -&1.version})

    type =
      case Enum.find(sorted_variables, fn %State.VarInfo{} = var ->
             var.name == variable and (var.version == version or version == :any)
           end) do
        nil when version == :any ->
          # No variable found — treat as a local call. Only the Code.Fragment
          # misclassification case (a no-parens call parsed as a variable, which
          # carries `version == :any`) reaches here. A *versioned* var not in
          # scope must NOT resolve to a same-named 0-arity local function — that
          # would inject the function's return type into feasibility/subtraction.
          {:local_call, variable, env.cursor_position, []}

        nil ->
          nil

        %State.VarInfo{type: type} ->
          type
      end

    expand(env, type, stack)
  end

  def do_expand(%Binding{attributes: attributes} = env, {:attribute, attribute}, stack) do
    type =
      case Enum.find(attributes, fn %{name: name} -> name == attribute end) do
        nil -> :none
        %State.AttributeInfo{type: type} -> type
      end

    expand(env, type, stack)
  end

  def do_expand(
        %Binding{structs: structs} = env,
        {:struct, fields, module, updated_struct},
        stack
      ) do
    # struct type must be a compile time atom or attribute
    module =
      case module do
        {:atom, atom} -> {:atom, atom}
        {:attribute, attr} -> {:attribute, attr}
        nil -> nil
        _ -> :none
      end

    module =
      case expand(env, module, stack) do
        {:atom, atom} -> atom
        nil -> nil
        _ -> :none
      end

    if module == nil or (module != :none and Struct.is_struct(module, structs)) do
      expanded = expand(env, updated_struct, stack)

      {fields, module} =
        get_struct_fields(env, put_fields(get_fields_from(expanded), fields), module)

      {:struct, fields, if(module != nil, do: {:atom, module}), nil}
    else
      :none
    end
  end

  # Literal-complete map (`:closed` tail) — all keys known; preserve.
  def do_expand(_env, {:map, fields, :closed}, _stack) do
    {:map, fields, :closed}
  end

  # Partial map (`nil` tail) — at least these keys present, closedness unknown.
  def do_expand(_env, {:map, fields, nil}, _stack) do
    {:map, fields, nil}
  end

  def do_expand(_env, {:map, fields, :open}, _stack) do
    {:map, fields, :open}
  end

  # Map update `%{base | fields}` — `updated_map` is the base shape to merge.
  def do_expand(env, {:map, fields, updated_map}, stack) do
    case expand(env, updated_map, stack) do
      # Update only rebinds existing keys, so the result tail follows the base.
      {:map, expanded_fields, :closed} ->
        {:map, put_fields(expanded_fields, fields), :closed}

      {:map, expanded_fields, nil} ->
        {:map, put_fields(expanded_fields, fields), nil}

      {:map, expanded_fields, :open} ->
        {:map, put_fields(expanded_fields, fields), :open}

      {:struct, expanded_fields, type, nil} ->
        {:struct, put_fields(expanded_fields, fields), type, nil}

      # Unknown base (e.g. `def f(m), do: %{m | a: 1}`): update preserves the
      # full base type with the listed keys refined (see Module.Types.Expr), so
      # the result is an OPEN map — listed fields known, other keys may exist.
      nil ->
        {:map, fields, :open}

      _ ->
        :none
    end
  end

  def do_expand(env, {:map_key, map_candidate, key_candidate}, stack) do
    expanded_key = expand(env, key_candidate, stack)
    {expanded_fields, tail} = expand_map_base(env, map_candidate, stack)

    cond do
      :none in expanded_fields ->
        :none

      match?({:atom, _}, expanded_key) ->
        {:atom, key} = expanded_key
        # On a `:closed` map a missing key is PROVABLY absent (`:not_set`); on a
        # `nil`-partial or `:open` map it is merely unknown (other keys may exist).
        case Keyword.fetch(expanded_fields, key) do
          {:ok, value} -> value
          :error when tail == :closed -> :not_set
          :error -> nil
        end

      true ->
        # Non-atom key: project through an exactly-matching domain key
        # (`%{"a" => v}["a"]`); a fuzzier key yields `nil`.
        Enum.find_value(expanded_fields, fn
          {{:domain, key_shape}, value} -> if key_shape == expanded_key, do: value
          _ -> nil
        end)
    end
  end

  def do_expand(env, {:tuple_nth, tuple_candidate, n}, stack) do
    case expand(env, tuple_candidate, stack) do
      # `n` is a 0-based index, so valid positions are `0..(size - 1)`.
      {:tuple, size, fields} when n >= 0 and n < size ->
        fields |> Enum.at(n)

      nil ->
        nil

      _ ->
        :none
    end
  end

  def do_expand(env, {:for_expression, list_candidate}, stack) do
    case expand(env, list_candidate, stack) do
      {list, type} when list in [:list, :nonempty_list] and type not in [:empty, :none] ->
        type

      {:map, fields, tail} when tail in [nil, :open, :closed] ->
        case fields do
          [{_key, value} | _] ->
            {:tuple, 2, [nil, value]}

          _ ->
            nil
        end

      nil ->
        nil

      _ ->
        :none
    end
  end

  def do_expand(env, {:list_head, list_candidate}, stack) do
    case expand(env, list_candidate, stack) do
      {list, type} when list in [:list, :nonempty_list] and type not in [:empty, :none] ->
        type

      nil ->
        nil

      _ ->
        :none
    end
  end

  def do_expand(env, {:list_tail, list_candidate}, stack) do
    case expand(env, list_candidate, stack) do
      {list, type} when list in [:list, :nonempty_list] and type not in [:empty, :none] ->
        {:list, type}

      nil ->
        nil

      _ ->
        :none
    end
  end

  def do_expand(env, {:nonempty_list, type}, stack),
    do: {:nonempty_list, expand(env, type, stack)}

  # Improper non-empty list: proper prefix of `elem` terminated by non-list `tail`.
  def do_expand(env, {:nonempty_list, elem, tail}, stack),
    do: {:nonempty_list, expand(env, elem, stack), expand(env, tail, stack)}

  # Set difference for cross-clause occurrence typing (a later `case` clause sees
  # the scrutinee minus what earlier clauses matched). Conservative: only narrows
  # when the base resolves to a union (see difference/2).
  def do_expand(env, {:difference, base_candidate, subtracted_candidate}, stack) do
    difference(expand(env, base_candidate, stack), expand(env, subtracted_candidate, stack))
  end

  # `case` result: union the bodies of clauses whose pattern can match the
  # scrutinee; a clause disjoint from the scrutinee can't match (the case raises)
  # so it is dropped. No clause matching → `:none`.
  def do_expand(env, {:case_result, scrutinee_candidate, clause_specs}, stack)
      when is_list(clause_specs) do
    scrutinee = expand(env, scrutinee_candidate, stack)

    bodies =
      for {pattern_candidate, body_candidate} <- clause_specs,
          clause_feasible?(env, scrutinee, pattern_candidate, stack) do
        expand(env, body_candidate, stack)
      end

    normalize_union(bodies)
  end

  # dependency injection
  def do_expand(env, {:call, {:atom, Application}, fun, args}, stack)
      when fun in ~w(compile_env!)a do
    # `Application.compile_env!/2` underneath works like `fetch_env!/2`
    do_expand(env, {:call, {:atom, Application}, :fetch_env!, args}, stack)
  end

  def do_expand(env, {:call, {:atom, Application}, fun, args}, stack)
      when fun in ~w(compile_env)a do
    # `Application.compile_env/3` underneath works like `get_env/3`
    do_expand(env, {:call, {:atom, Application}, :get_env, args}, stack)
  end

  def do_expand(env, {:call, {:atom, Application}, :fetch_env, args}, stack) do
    try do
      expanded_args =
        args
        |> Enum.map(&expand(env, &1, stack))
        |> Enum.map(&elem(&1, 1))

      case apply(Application, :fetch_env, expanded_args) do
        {:ok, value} when is_atom(value) ->
          {:tuple, 2, [{:atom, :ok}, {:atom, value}]}

        {:ok, _value} ->
          {:tuple, 2, [{:atom, :ok}, nil]}

        :error ->
          {:atom, :error}
      end
    rescue
      e ->
        Logger.debug("Application.fetch_env expand failed: #{Exception.message(e)}")
        # Unknown, not bottom: `:none` is dropped from unions and over-claims.
        nil
    end
  end

  def do_expand(env, {:call, {:atom, Application}, fun, args}, stack)
      when fun in ~w(get_env fetch_env!)a do
    try do
      expanded_args =
        args
        |> Enum.map(&expand(env, &1, stack))
        |> Enum.map(&elem(&1, 1))

      result = apply(Application, fun, expanded_args)

      case result do
        :error ->
          :none

        value when is_atom(value) ->
          {:atom, value}

        value when is_integer(value) ->
          {:integer, value}

        value when is_binary(value) ->
          {:binary, value}

        value when is_list(value) ->
          {:list, nil}

        value when is_map(value) ->
          {:map, [], nil}

        _ ->
          nil
      end
    rescue
      e ->
        Logger.debug("Application.#{fun} expand failed: #{Exception.message(e)}")
        # Unknown, not bottom: `:none` is dropped from unions and over-claims.
        nil
    end
  end

  # remote call
  def do_expand(env, {:call, target, function, arguments}, stack) do
    if :none in arguments do
      :none
    else
      expanded_target = expand(env, target, stack)
      # do not include private funs on remote call
      expand_call(env, expanded_target, function, arguments, false, nil, stack)
      |> drop_no_spec
    end
  end

  # local call
  def do_expand(
        %Binding{functions: functions, macros: macros} = env,
        {:local_call, function, position, arguments},
        stack
      ) do
    if :none in arguments do
      :none
    else
      combined_imports =
        {functions, macros}
        |> Introspection.combine_imports()

      candidate_targets =
        if env.module && env.function do
          # locals are available only in defs
          [env.module]
        else
          []
        end ++ combined_imports ++ [Kernel.SpecialForms]

      # take first matching
      Enum.find_value(candidate_targets, fn
        {candidate, imported} ->
          if {function, length(arguments)} in imported do
            expand_call(env, {:atom, candidate}, function, arguments, false, position, stack)
          end

        candidate ->
          # include private from current module
          include_private = candidate == env.module

          expand_call(
            env,
            {:atom, candidate},
            function,
            arguments,
            include_private,
            position,
            stack
          )
      end)
      |> maybe_refine_local_call(env, function, arguments)
      |> drop_no_spec
    end
  end

  def do_expand(env, {:tuple, size, fields}, stack),
    do: {:tuple, size, fields |> Enum.map(&expand(env, &1, stack))}

  def do_expand(_env, {:list, :empty}, _stack),
    do: {:list, :empty}

  def do_expand(env, {:list, type}, stack),
    do: {:list, expand(env, type, stack)}

  def do_expand(_env, {:atom, atom}, _stack), do: {:atom, atom}

  def do_expand(_env, {:integer, integer}, _stack), do: {:integer, integer}

  def do_expand(env, {:union, all}, stack) do
    all |> Enum.map(&expand(env, &1, stack)) |> normalize_union()
  end

  def do_expand(_env, :none, _stack), do: :none

  # Terminal type shapes — return as-is
  def do_expand(_env, :atom, _stack), do: :atom
  def do_expand(_env, :integer, _stack), do: {:integer, nil}
  def do_expand(_env, :binary, _stack), do: {:binary, nil}
  def do_expand(_env, :float, _stack), do: {:float, nil}
  def do_expand(_env, :number, _stack), do: :number
  def do_expand(_env, :pid, _stack), do: :pid
  def do_expand(_env, :port, _stack), do: :port
  def do_expand(_env, :reference, _stack), do: :reference
  def do_expand(_env, :boolean, _stack), do: {:union, [{:atom, false}, {:atom, true}]}
  def do_expand(_env, :bitstring, _stack), do: :bitstring
  def do_expand(_env, :fun, _stack), do: :fun
  def do_expand(_env, :tuple, _stack), do: :tuple
  # `:list` is the generic list shape; keep it so union subsumption (`:list` over
  # `{:list, _}`) and intersection work on it.
  def do_expand(_env, :list, _stack), do: :list
  def do_expand(_env, :empty_map, _stack), do: :empty_map
  def do_expand(_env, :empty_list, _stack), do: {:list, :empty}
  def do_expand(_env, :non_struct_map, _stack), do: :non_struct_map

  def do_expand(env, {:tuple_open, elems}, stack) when is_list(elems),
    do: {:tuple_open, Enum.map(elems, &expand(env, &1, stack))}

  def do_expand(_env, {:binary, _} = shape, _stack), do: shape
  def do_expand(_env, {:float, _} = shape, _stack), do: shape
  def do_expand(_env, {:fun, arity} = shape, _stack) when is_integer(arity), do: shape

  def do_expand(env, {:fun, args, return}, stack) when is_list(args),
    do: {:fun, Enum.map(args, &expand(env, &1, stack)), expand(env, return, stack)}

  def do_expand(env, {:fun_clauses, clauses}, stack) when is_list(clauses) do
    {:fun_clauses,
     Enum.map(clauses, fn {args, return} ->
       {Enum.map(args, &expand(env, &1, stack)), expand(env, return, stack)}
     end)}
  end

  # Optional map field values (`if_set`). Expand the inner shape and re-wrap;
  # drop the wrapper if the inner resolves to `nil`.
  def do_expand(env, {:optional, inner}, stack) do
    case expand(env, inner, stack) do
      nil -> nil
      expanded -> {:optional, expanded}
    end
  end

  def do_expand(_env, _other, _stack), do: nil

  # Conservative set difference over already-expanded shapes. Only representable
  # when X is a union (drop the members Y covers); a single shape collapses to
  # :none if fully covered, else is returned unchanged (no negation invented).
  defp difference(nil, _subtracted), do: nil
  defp difference(base, nil), do: base
  defp difference(:none, _subtracted), do: :none
  defp difference(base, :none), do: base

  defp difference({:union, members}, subtracted) do
    removed = union_to_list(subtracted)

    # Normalize so the remainder is collapsed/subsumed like any other union.
    members
    |> Enum.reject(fn m -> Enum.any?(removed, &covers?(&1, m)) end)
    |> normalize_union()
  end

  defp difference(base, subtracted) do
    removed = union_to_list(subtracted)
    if Enum.any?(removed, &covers?(&1, base)), do: :none, else: base
  end

  defp union_to_list({:union, members}), do: members
  defp union_to_list(other), do: [other]

  # Can a clause whose pattern has type `pattern_candidate` match a scrutinee of
  # type `scrutinee`? Conservative: an unknown scrutinee or pattern is treated as
  # "could match"; otherwise the pattern must overlap the scrutinee (their
  # intersection isn't empty).
  defp clause_feasible?(env, scrutinee, pattern_candidate, stack) do
    pattern = expand(env, pattern_candidate, stack)

    cond do
      # `nil`/`:none` = unknown scrutinee or pattern — can't rule the clause out.
      scrutinee in [nil, :none] -> true
      pattern in [nil, :none] -> true
      true -> combine_intersection(scrutinee, pattern) != :none
    end
  end

  # Normalize an expanded union: flatten nested unions, drop :none, drop members
  # subsumed by a more general sibling (e.g. `5 | integer()` -> `integer()`),
  # dedup, and collapse. A `nil` (unknown) member poisons the whole union to
  # `nil`, since we no longer know it's bounded.
  defp normalize_union(members) do
    members = flatten_unions(members)

    if Enum.any?(members, &is_nil/1) do
      nil
    else
      case members
           |> Enum.reject(&(&1 == :none))
           |> Enum.uniq()
           |> drop_subsumed()
           |> coalesce_union() do
        [] -> :none
        [one] -> one
        many -> {:union, many}
      end
    end
  end

  # Merge structurally-compatible union members field-/element-wise, preserving
  # first-occurrence order: same-shape lists union their element types; maps with
  # the same key set and structs of the same module merge fields key-by-key
  # (`%{a: 1} | %{a: 2}` -> `%{a: 1 | 2}`). Other members are left untouched.
  defp coalesce_union(members) do
    # Accumulate in reverse to avoid O(n) appends, reverse once to restore
    # first-occurrence order.
    members
    |> Enum.reduce([], fn member, acc ->
      case Enum.find_index(acc, &mergeable?(&1, member)) do
        nil -> [member | acc]
        index -> List.update_at(acc, index, &merge_members(&1, member))
      end
    end)
    |> Enum.reverse()
  end

  defp mergeable?({:list, _}, {:list, _}), do: true

  defp mergeable?({:map, fields_1, updated}, {:map, fields_2, updated}),
    do: same_keys?(fields_1, fields_2)

  defp mergeable?({:struct, _, {:atom, mod}, updated}, {:struct, _, {:atom, mod}, updated}),
    do: true

  defp mergeable?(_a, _b), do: false

  defp same_keys?(fields_1, fields_2) do
    Enum.sort(field_keys(fields_1)) == Enum.sort(field_keys(fields_2))
  end

  # Field keys, tolerating non-atom (domain) keys — `Keyword.keys/1` would raise.
  defp field_keys(fields), do: Enum.map(fields, &elem(&1, 0))

  # Field value by key, tolerating non-atom (domain) keys — `Keyword.get/3` and
  # `fields[key]` raise for keys like `{:domain, _}`.
  defp field_get(fields, key, default \\ nil) do
    case List.keyfind(fields, key, 0) do
      {_key, value} -> value
      nil -> default
    end
  end

  # Override `base` fields with `overrides` by key, for any key type (atom or
  # `{:domain, _}`). `Keyword.merge/2` raises on non-atom keys.
  defp put_fields(base, overrides) do
    Enum.reduce(overrides, base, fn {key, _value} = pair, acc ->
      List.keystore(acc, key, 0, pair)
    end)
  end

  defp merge_members({:list, elem_1}, {:list, elem_2}),
    do: {:list, merge_list_elem(elem_1, elem_2)}

  defp merge_members({:map, fields_1, updated}, {:map, fields_2, _}),
    do: {:map, merge_fields(fields_1, fields_2), updated}

  defp merge_members({:struct, fields_1, type, updated}, {:struct, fields_2, _, _}),
    do: {:struct, merge_fields(fields_1, fields_2), type, updated}

  defp merge_list_elem(:empty, elem), do: elem
  defp merge_list_elem(elem, :empty), do: elem
  defp merge_list_elem(elem_1, elem_2), do: normalize_union([elem_1, elem_2])

  # Could this shape be a list? Concrete list shapes yes; `nil` (unknown) is
  # treated as "maybe a list" so list operators stay list-typed; any other
  # concrete shape is not a list.
  defp list_like?({:list, _}), do: true
  defp list_like?({:nonempty_list, _}), do: true
  # An improper non-empty list is still "list-like" for the `++`/`--` argument
  # guard: treating it as non-list would falsely claim `:none` (dead code).
  defp list_like?({:nonempty_list, _, _}), do: true
  defp list_like?(:list), do: true
  defp list_like?(nil), do: true
  defp list_like?(_other), do: false

  # The element type of a (possibly unknown) list shape. For an improper
  # 3-tuple this is the proper-prefix element type (the tail is dropped).
  defp list_element_type({:list, :empty}), do: :empty
  defp list_element_type({:list, elem}), do: elem
  defp list_element_type({:nonempty_list, elem}), do: elem
  defp list_element_type({:nonempty_list, elem, _tail}), do: elem
  defp list_element_type(_other), do: nil

  # The element type of `a ++ b`: the union of both element types. An unknown
  # element on either side widens the result element to `nil` (list of `term()`).
  defp concat_element_type(left, right) do
    merge_list_elem(list_element_type(left), list_element_type(right))
  end

  # The number-tower result of `+`/`-`/`*`: float() if any operand is a float,
  # integer() if all are integers, otherwise number() (covers unknown operands).
  defp numeric_result(env, args, stack) do
    kinds = Enum.map(args, fn arg -> numeric_kind(expand(env, arg, stack)) end)

    cond do
      # A known non-numeric operand (atom, binary, map, tuple, list, ...) is a
      # type error in the compiler (`apply.ex:106-111` has no such clause), so we
      # return nil (unknown) rather than over-claiming `:number`.
      :not_numeric in kinds -> nil
      :float in kinds -> {:float, nil}
      Enum.all?(kinds, &(&1 == :integer)) -> {:integer, nil}
      true -> :number
    end
  end

  defp numeric_kind({:integer, _}), do: :integer
  defp numeric_kind(:integer), do: :integer
  defp numeric_kind({:float, _}), do: :float
  defp numeric_kind(:float), do: :float
  defp numeric_kind(:number), do: :number
  # An unknown operand (`nil`) could still be numeric — result is at best number().
  defp numeric_kind(nil), do: :number

  # Everything else is a KNOWN non-numeric shape -> the call is a type error.
  defp numeric_kind(_other), do: :not_numeric

  # Field values are unioned per key (keys are the same on both sides). A key
  # missing from one side unions with nil, which `normalize_union/1` treats as
  # unknown.
  defp merge_fields(fields_1, fields_2) do
    Enum.map(fields_1, fn {key, value_1} ->
      {key, normalize_union([value_1, field_get(fields_2, key)])}
    end)
  end

  defp flatten_unions(members) do
    Enum.flat_map(members, fn
      {:union, inner} -> inner
      other -> [other]
    end)
  end

  # Keep a member unless another member *strictly* subsumes it (covers it but
  # is not covered by it). Strictness matters because `covers?/2` is symmetric
  # for e.g. two same-module structs — those must be kept (and later merged
  # field-wise by coalesce_union/1), not mutually dropped to nothing.
  defp drop_subsumed(members) do
    Enum.reject(members, fn m ->
      Enum.any?(members, fn n -> n != m and covers?(n, m) and not covers?(m, n) end)
    end)
  end

  # Does shape `a` subsume shape `b` (is `b` a subtype of `a`)? Used to decide
  # which union members a subtracted type removes and to dedup/collapse unions.
  #
  # DESCR-BACKED FAST PATH: when the native typesystem is enabled AND both
  # operands are `descr_exact?` (static coercion loses no information),
  # subsumption delegates to `Descr.subtype?/2`, which is strictly more faithful
  # than the structural clauses below (it sees through unions, the number tower,
  # etc.): `covers?(subsumer, member)` == `subtype?(coerce(member), coerce(subsumer))`.
  #
  # Dynamic-wrapping is safe: `coerce_var_type_public/1` yields
  # `dynamic(static(shape))`, so `subtype?/2` compares the full descrs (the
  # dynamic wrappers cancel: `subtype?(dynamic(a), dynamic(b)) == subtype?(a, b)`)
  # — the same property the descr-backed intersection relies on. Backend drift
  # falls through via try/rescue; non-exact shapes always take the custom path.
  defp covers?(a, b) do
    case descr_backed_covers?(a, b) do
      :__fallthrough__ -> covers_custom?(a, b)
      bool -> bool
    end
  end

  defp descr_backed_covers?(subsumer, member) do
    with_descr_backing(subsumer, member, fn descr_subsumer, descr_member ->
      ElixirTypes.descr_subtype?(descr_member, descr_subsumer)
    end)
  end

  # Shared descr-first gate for the covers?/intersection dispatchers. Runs
  # `descr_fun` on the coercions of both operands only when the native backend is
  # enabled and both shapes are exact. Backend drift or a non-exact/disabled
  # operand yields `:__fallthrough__` so the caller uses its custom algebra;
  # `descr_fun` may itself return `:__fallthrough__` to decline.
  defp with_descr_backing(a, b, descr_fun) do
    if ElixirTypes.enabled?() and descr_exact?(a) and descr_exact?(b) do
      try do
        descr_a = ElixirTypes.coerce_var_type_public(a)
        descr_b = ElixirTypes.coerce_var_type_public(b)
        descr_fun.(descr_a, descr_b)
      rescue
        _ -> :__fallthrough__
      catch
        _, _ -> :__fallthrough__
      end
    else
      :__fallthrough__
    end
  end

  # Structural subsumption fallback. Conservative — exact matches, generic
  # (value-less) types covering their literals, tagged tuples (element-wise, with
  # `nil` as a wildcard element), and structs by module. Recurses through
  # `covers?/2` (the dispatcher) so nested exact subshapes still get the faithful
  # descr treatment.
  defp covers_custom?(same, same), do: true
  # `nil` as a *subtracted* element means "any value here" (the `_` in
  # `{:ok, _}`), so it covers any member. Reached only inside tuple recursion.
  defp covers_custom?(nil, _b), do: true
  defp covers_custom?(:atom, {:atom, _}), do: true
  defp covers_custom?(:boolean, {:atom, bool}) when is_boolean(bool), do: true
  defp covers_custom?(:integer, {:integer, _}), do: true
  defp covers_custom?({:integer, nil}, {:integer, _}), do: true
  defp covers_custom?(:float, {:float, _}), do: true
  defp covers_custom?({:float, nil}, {:float, _}), do: true
  defp covers_custom?(:binary, {:binary, _}), do: true
  defp covers_custom?({:binary, nil}, {:binary, _}), do: true

  # Number tower: number() subsumes integer() and float() (in either spelling).
  defp covers_custom?(:number, {:integer, _}), do: true
  defp covers_custom?(:number, {:float, _}), do: true
  defp covers_custom?(:number, :integer), do: true
  defp covers_custom?(:number, :float), do: true

  # Generic container/callable atoms subsume their concrete instances.
  defp covers_custom?(:tuple, {:tuple, _, _}), do: true
  defp covers_custom?(:fun, {:fun, _}), do: true
  defp covers_custom?(:fun, {:fun, _, _}), do: true
  defp covers_custom?(:fun, {:fun_clauses, _}), do: true

  # `bitstring()` subsumes `binary()` (binaries are byte-aligned bitstrings).
  defp covers_custom?(:bitstring, {:binary, _}), do: true
  defp covers_custom?(:bitstring, :binary), do: true

  # Lists: the generic `:list` atom subsumes any list; a (possibly-empty) list
  # subsumes a non-empty list of the same/covered element; element type is
  # covariant.
  defp covers_custom?(:list, {:list, _}), do: true
  defp covers_custom?(:list, {:nonempty_list, _}), do: true
  defp covers_custom?({:list, :empty}, {:list, :empty}), do: true

  defp covers_custom?({:list, sub_elem}, {:list, mem_elem}),
    do: list_elem_covers?(sub_elem, mem_elem)

  defp covers_custom?({:list, sub_elem}, {:nonempty_list, mem_elem}),
    do: list_elem_covers?(sub_elem, mem_elem)

  defp covers_custom?({:nonempty_list, sub_elem}, {:nonempty_list, mem_elem}),
    do: list_elem_covers?(sub_elem, mem_elem)

  # Improper non-empty list (3-tuple): conservative. Neither subsumed by nor
  # subsuming any proper list; covers only an equal-kind 3-tuple whose prefix
  # element and tail both cover.
  defp covers_custom?({:nonempty_list, sub_elem, sub_tail}, {:nonempty_list, mem_elem, mem_tail}),
    do: list_elem_covers?(sub_elem, mem_elem) and covers?(sub_tail, mem_tail)

  # Map top (`%{}` / `map()`) subsumes any concrete map or struct. An open empty
  # map (`%{...}`) is likewise the map top.
  defp covers_custom?({:map, [], tail}, {:map, _, _}) when tail_top?(tail), do: true
  defp covers_custom?({:map, [], tail}, {:struct, _, _, _}) when tail_top?(tail), do: true

  defp covers_custom?({:tuple, n, sub_elems}, {:tuple, n, mem_elems}) do
    sub_elems
    |> Enum.zip(mem_elems)
    |> Enum.all?(fn {sub, mem} -> covers?(sub, mem) end)
  end

  defp covers_custom?({:struct, _, {:atom, mod}, _}, {:struct, _, {:atom, mod}, _}), do: true
  defp covers_custom?({:struct, _, nil, _}, {:struct, _, _, _}), do: true

  # Optional map-field wrappers (`if_set`): transparent — coverage decided by the
  # inner shapes, so `{:optional, x}` is not spuriously disjoint from `x`.
  defp covers_custom?({:optional, sub}, {:optional, mem}), do: covers?(sub, mem)
  defp covers_custom?({:optional, sub}, mem), do: covers?(sub, mem)
  defp covers_custom?(sub, {:optional, mem}), do: covers?(sub, mem)

  defp covers_custom?(_a, _b), do: false

  # List element coverage: `:empty` (the element of `[]`) is covered by anything,
  # and otherwise defer to `covers?/2`.
  defp list_elem_covers?(_sub, :empty), do: true
  defp list_elem_covers?(sub, mem), do: covers?(sub, mem)

  defp drop_no_spec(:no_spec), do: nil
  defp drop_no_spec(other), do: other

  defp maybe_refine_local_call(result, %Binding{} = env, fun, arguments) do
    with true <- ElixirTypes.enabled?(),
         _module when is_atom(env.module) <- env.module,
         local_sigs when is_map(local_sigs) and local_sigs != %{} <- env.elixir_types_local_sigs,
         entry when not is_nil(entry) <- Map.get(local_sigs, {fun, length(arguments)}),
         {kind, {sig_kind, _domain, _clauses} = sig}
         when kind in [:def, :defp] and sig_kind in [:infer, :strong] <- entry do
      # Expand argument shapes for overload filtering
      expanded_args = Enum.map(arguments, &expand(env, &1, []))
      return_descr = ElixirTypes.extract_return_type_from_sig(sig, expanded_args)
      shape = ElixirTypes.to_shape(return_descr)
      expanded_shape = if shape, do: expand(env, shape, []), else: nil

      if expanded_shape != nil do
        merge_binding_shape(result, expanded_shape)
      else
        result
      end
    else
      _ -> result
    end
  end

  defp merge_binding_shape(:no_spec, shape), do: shape
  defp merge_binding_shape(:none, _shape), do: :none
  defp merge_binding_shape(nil, shape), do: shape
  defp merge_binding_shape({:union, _} = result, _shape), do: result

  defp merge_binding_shape(result, shape) when is_tuple(result) do
    ElixirTypes.merge_shapes(result, shape)
  end

  defp merge_binding_shape(result, _shape), do: result

  # not supported
  defp expand_call(_env, nil, _, _, _, _, _stack), do: nil
  defp expand_call(_env, :none, _, _, _, _, _stack), do: :none

  # map field access
  defp expand_call(env, {:map, fields, _}, field, arity, _, _, stack) do
    # field access is a call with arity 0, other are not allowed
    if arity == [] do
      expand(env, fields[field], stack)
    else
      :none
    end
  end

  # struct field access
  defp expand_call(env, {:struct, fields, _, _}, field, arity, _, _, stack) do
    # field access is a call with arity 0, other are not allowed
    if arity == [] do
      expand(env, fields[field], stack)
    else
      :none
    end
  end

  defp expand_call(
         env,
         {:atom, module},
         name,
         [left_candidate | rest],
         _include_private,
         _,
         stack
       )
       when name in [:++, :--] and module in [Kernel, :erlang] do
    # Ground truth: `apply.ex` `:erlang.++` /`:erlang.--`.
    #   `++`: `[empty_list, term] -> term`, `[non_empty_list(e), term] -> non_empty_list(e, term)`
    #   `--`: `[list(term), list(term)] -> list(term)`
    # So `[] ++ x` is `x` (any term), `[1|_] ++ x` may be improper. `--` always
    # yields a (possibly-empty, proper) sublist of the left, so it stays correct
    # as a list of the left's element type.
    left = expand(env, left_candidate, stack)

    cond do
      left == :none ->
        :none

      not list_like?(left) ->
        :none

      name == :-- ->
        # `a -- b` is a sublist of `a`: same element type.
        {:list, list_element_type(left)}

      true ->
        # `a ++ b` (cases handled per-branch below).
        right = if match?([_ | _], rest), do: expand(env, hd(rest), stack), else: nil

        cond do
          right == :none ->
            :none

          # Known proper-list RHS: result is a proper list of the unioned elements.
          match?({:list, _}, right) or match?({:nonempty_list, _}, right) or right == :list ->
            {:list, concat_element_type(left, right)}

          # Empty-list LHS: `[] ++ x` is exactly `x` (any term).
          left == {:list, :empty} ->
            right

          # Known non-empty proper LHS ++ known non-list RHS: improper non-empty
          # list (prefix from LHS element type, tail from RHS). Possibly-empty or
          # improper LHS fall through to `nil`.
          match?({:nonempty_list, _}, left) and right != nil and not list_like?(right) ->
            {:nonempty_list, list_element_type(left), right}

          # Unknown / non-list / improper RHS with possibly-empty LHS: can't be
          # modeled as a list shape.
          true ->
            nil
        end
    end
  end

  defp expand_call(
         env,
         {:atom, Kernel},
         :elem,
         [tuple_candidate, n_candidate],
         _include_private,
         _,
         stack
       ) do
    case expand(env, n_candidate, stack) do
      {:integer, n} ->
        expand(env, {:tuple_nth, tuple_candidate, n}, stack)

      nil ->
        nil

      _ ->
        :none
    end
  end

  # rewritten elem
  defp expand_call(
         env,
         {:atom, :erlang},
         :element,
         [n_candidate, tuple_candidate],
         _include_private,
         _,
         stack
       ) do
    case expand(env, n_candidate, stack) do
      {:integer, n} ->
        expand(env, {:tuple_nth, tuple_candidate, n - 1}, stack)

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Kernel},
         :put_elem,
         [tuple_candidate, n_candidate, value],
         _include_private,
         _,
         stack
       ) do
    with {:tuple, elems_count, elems} <- expand(env, tuple_candidate, stack),
         {:integer, n} when n >= 0 and n < elems_count <- expand(env, n_candidate, stack),
         expanded_value when expanded_value != :none <- expand(env, value, stack) do
      {:tuple, elems_count, elems |> List.replace_at(n, expanded_value)}
    else
      nil ->
        nil

      _ ->
        :none
    end
  end

  # rewritten put_elem
  defp expand_call(
         env,
         {:atom, :erlang},
         :setelement,
         [n_candidate, tuple_candidate, value],
         _include_private,
         _,
         stack
       ) do
    with {:tuple, elems_count, elems} <- expand(env, tuple_candidate, stack),
         {:integer, n} when n >= 1 and n <= elems_count <- expand(env, n_candidate, stack),
         expanded_value when expanded_value != :none <- expand(env, value, stack) do
      {:tuple, elems_count, elems |> List.replace_at(n - 1, expanded_value)}
    else
      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, module},
         fun,
         [tuple_candidate, value],
         _include_private,
         _,
         stack
       )
       when (module == Tuple and fun == :append) or (module == :erlang and fun == :append_element) do
    with {:tuple, elems_count, elems} <- expand(env, tuple_candidate, stack),
         expanded_value when expanded_value != :none <- expand(env, value, stack) do
      {:tuple, elems_count + 1, elems ++ [expanded_value]}
    else
      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Tuple},
         :delete_at,
         [tuple_candidate, n_candidate],
         _include_private,
         _,
         stack
       ) do
    with {:tuple, elems_count, elems} <- expand(env, tuple_candidate, stack),
         {:integer, n} when n >= 0 and n < elems_count <- expand(env, n_candidate, stack) do
      {:tuple, elems_count - 1, elems |> List.delete_at(n)}
    else
      nil ->
        nil

      _ ->
        :none
    end
  end

  # rewritten Tuple.delete_at
  defp expand_call(
         env,
         {:atom, :erlang},
         :delete_element,
         [n_candidate, tuple_candidate],
         _include_private,
         _,
         stack
       ) do
    with {:tuple, elems_count, elems} <- expand(env, tuple_candidate, stack),
         {:integer, n} when n > 0 and n <= elems_count <- expand(env, n_candidate, stack) do
      {:tuple, elems_count - 1, elems |> List.delete_at(n - 1)}
    else
      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Tuple},
         :insert_at,
         [tuple_candidate, n_candidate, value],
         _include_private,
         _,
         stack
       ) do
    with {:tuple, elems_count, elems} <- expand(env, tuple_candidate, stack),
         {:integer, n} when n >= 0 and n <= elems_count <- expand(env, n_candidate, stack),
         expanded_value when expanded_value != :none <- expand(env, value, stack) do
      {:tuple, elems_count + 1, elems |> List.insert_at(n, expanded_value)}
    else
      nil ->
        nil

      _ ->
        :none
    end
  end

  # rewritten Tuple.insert_at
  defp expand_call(
         env,
         {:atom, :erlang},
         :insert_element,
         [n_candidate, tuple_candidate, value],
         _include_private,
         _,
         stack
       ) do
    with {:tuple, elems_count, elems} <- expand(env, tuple_candidate, stack),
         {:integer, n} when n > 0 and n <= elems_count + 1 <- expand(env, n_candidate, stack),
         expanded_value when expanded_value != :none <- expand(env, value, stack) do
      {:tuple, elems_count + 1, elems |> List.insert_at(n - 1, expanded_value)}
    else
      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, module},
         fun,
         [tuple_candidate],
         _include_private,
         _,
         stack
       )
       when (module == Tuple and fun == :to_list) or (module == :erlang and fun == :tuple_to_list) do
    case expand(env, tuple_candidate, stack) do
      {:tuple, _elems_count, elems} ->
        case elems do
          [] -> {:list, :empty}
          _ -> {:list, normalize_union(elems)}
        end

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, module},
         :tuple_size,
         [tuple_candidate],
         _include_private,
         _,
         stack
       )
       when module in [Kernel, :erlang] do
    case expand(env, tuple_candidate, stack) do
      {:tuple, elems_count, _elems} -> {:integer, elems_count}
      nil -> nil
      _ -> :none
    end
  end

  defp expand_call(
         env,
         {:atom, module},
         fun,
         [value, n_candidate],
         _include_private,
         _,
         stack
       )
       when (module == Tuple and fun == :duplicate) or (module == :erlang and fun == :make_tuple) do
    {value, n_candidate} =
      if module == :erlang do
        {n_candidate, value}
      else
        {value, n_candidate}
      end

    # limit to 5
    with {:integer, n} when n >= 0 <- expand(env, n_candidate, stack),
         expanded_value when expanded_value != :none <- expand(env, value, stack) do
      {:tuple, n, expanded_value |> List.duplicate(n)}
    else
      nil ->
        nil

      {:integer, _n} ->
        nil

      _ ->
        :none
    end
  end

  # hd is inlined
  defp expand_call(
         env,
         {:atom, module},
         :hd,
         [list_candidate],
         _include_private,
         _,
         stack
       )
       when module in [Kernel, :erlang] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        type

      nil ->
        nil

      _ ->
        :none
    end
  end

  # tl is inlined
  defp expand_call(
         env,
         {:atom, module},
         :tl,
         [list_candidate],
         _include_private,
         _,
         stack
       )
       when module in [Kernel, :erlang] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        {:list, type}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Enum},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:at, :fetch, :fetch!, :find, :max, :max_by, :min, :min_by, :random] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        if name == :fetch do
          {:tuple, 2, [{:atom, :ok}, type]}
        else
          type
        end

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Enum},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:split, :split_while] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        {:tuple, 2, [{:list, type}, {:list, type}]}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Enum},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:min_max, :min_max_by] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        {:tuple, 2, [type, type]}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Enum},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:chunk_by, :chunk_every, :chunk_while] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        {:list, {:list, type}}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Enum},
         :concat,
         [list_candidate],
         _include_private,
         _,
         stack
       ) do
    case expand(env, list_candidate, stack) do
      {:list, {:list, type}} ->
        {:list, type}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Enum},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [
              :concat,
              :dedup,
              :dedup_while,
              :drop,
              :drop_every,
              :drop_while,
              :filter,
              :intersperse,
              :reject,
              :reverse,
              :reverse_slice,
              :shuffle,
              :slice,
              :sort,
              :sort_by,
              :take,
              :take_every,
              :take_random,
              :take_while,
              :to_list,
              :uniq,
              :uniq_by
            ] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        {:list, type}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, List},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:delete, :delete_at, :insert_at, :replace_at, :update_at] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        {:list, type}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, List},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:flatten] do
    case expand(env, list_candidate, stack) do
      {:list, {:list, type}} ->
        {:list, type}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, List},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:wrap] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        {:list, type}

      {:atom, nil} ->
        {:list, :empty}

      nil ->
        nil

      :none ->
        :none

      type ->
        {:list, type}
    end
  end

  defp expand_call(
         env,
         {:atom, List},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:pop_at] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        {:tuple, 2, [type, {:list, type}]}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, List},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:first, :last] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        type

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, List},
         name,
         [element | _],
         _include_private,
         _,
         stack
       )
       when name in [:duplicate] do
    case expand(env, element, stack) do
      nil ->
        nil

      :none ->
        :none

      type ->
        {:list, type}
    end
  end

  defp expand_call(env, {:atom, module}, fun, [map, key], _include_private, _, stack)
       when (module == Map and fun in [:fetch, :fetch!, :get]) or
              (module == :maps and fun in [:find, :get]) do
    {map, key} =
      if module == :maps do
        # rewritten versions have different arg order
        {key, map}
      else
        {map, key}
      end

    fields = expand_map_fields(env, map, stack)

    if :none in fields do
      :none
    else
      case expand(env, key, stack) do
        {:atom, atom} ->
          value = fields |> Keyword.get(atom)

          if fun in [:fetch, :find] and value != nil do
            {:tuple, 2, [{:atom, :ok}, value]}
          else
            value
          end

        nil ->
          nil

        _ ->
          :none
      end
    end
  end

  # `Map.new/0` constructs the empty map — literal-complete (no keys at all).
  defp expand_call(_env, {:atom, Map}, :new, [], _include_private, _, _stack) do
    {:map, [], :closed}
  end

  defp expand_call(env, {:atom, Map}, fun, [map, key, default], _include_private, _, stack)
       when fun in [:get, :get_lazy] do
    fields = expand_map_fields(env, map, stack)

    if :none in fields do
      :none
    else
      case expand(env, key, stack) do
        {:atom, atom} ->
          default = if fun == :get, do: expand(env, default, stack)
          fields |> Keyword.get(atom, default)

        nil ->
          nil

        _ ->
          :none
      end
    end
  end

  defp expand_call(env, {:atom, module}, fun, [map, key, value], _include_private, _, stack)
       when (fun == :put and module in [Map, :maps]) or (fun == :update and module == :maps) or
              (fun == :replace! and module == Map) do
    {map, key, value} =
      if module == :maps do
        # rewritten versions have different parameter order
        {value, map, key}
      else
        {map, key, value}
      end

    {fields, tail} = expand_map_base(env, map, stack)

    if :none in fields do
      :none
    else
      case expand(env, key, stack) do
        {:atom, atom} ->
          value = expand(env, value, stack)
          {:map, fields |> Keyword.put(atom, value), tail}

        :none ->
          :none

        _ ->
          {:map, fields, tail}
      end
    end
  end

  defp expand_call(env, {:atom, Map}, fun, [map, key, value], _include_private, _, stack)
       when fun in [:put_new, :put_new_lazy] do
    {fields, tail} = expand_map_base(env, map, stack)

    if :none in fields do
      :none
    else
      case expand(env, key, stack) do
        {:atom, atom} ->
          value = if fun == :put_new, do: expand(env, value, stack)
          {:map, fields |> Keyword.put_new(atom, value), tail}

        :none ->
          :none

        _ ->
          {:map, fields, tail}
      end
    end
  end

  defp expand_call(env, {:atom, module}, fun, [map, key], _include_private, _, stack)
       when (module == Map and fun == :delete) or (module == :maps and fun == :remove) do
    {map, key} =
      if module == :maps do
        # rewritten versions have different arg order
        {key, map}
      else
        {map, key}
      end

    {fields, tail} = expand_map_base(env, map, stack)

    if :none in fields do
      :none
    else
      case expand(env, key, stack) do
        {:atom, atom} ->
          {:map, fields |> Keyword.delete(atom), tail}

        :none ->
          :none

        _ ->
          {:map, fields, tail}
      end
    end
  end

  # Map.merge/2 is inlined
  defp expand_call(env, {:atom, module}, :merge, [map, other_map], _include_private, _, stack)
       when module in [Map, :maps] do
    {fields, tail_a} = expand_map_base(env, map, stack)
    {other_fields, tail_b} = expand_map_base(env, other_map, stack)

    if :none in (fields ++ other_fields) do
      :none
    else
      {:map, put_fields(fields, other_fields), merge_tails(tail_a, tail_b)}
    end
  end

  defp expand_call(env, {:atom, Map}, :merge, [map, other_map, _fun], _include_private, _, stack) do
    {fields, tail_a} = expand_map_base(env, map, stack)
    {other_fields, tail_b} = expand_map_base(env, other_map, stack)

    if :none in (fields ++ other_fields) do
      :none
    else
      conflicts =
        MapSet.new(safe_keys(fields))
        |> MapSet.intersection(MapSet.new(safe_keys(other_fields)))
        |> MapSet.to_list()
        |> Enum.map(&{&1, nil})

      merged = fields |> put_fields(other_fields) |> put_fields(conflicts)

      {:map, merged, merge_tails(tail_a, tail_b)}
    end
  end

  defp expand_call(
         env,
         {:atom, Map},
         :update,
         [map, key, _initial, _fun],
         _include_private,
         _,
         stack
       ) do
    {fields, tail} = expand_map_base(env, map, stack)

    if :none in fields do
      :none
    else
      case expand(env, key, stack) do
        {:atom, atom} ->
          {:map, fields |> Keyword.put(atom, nil), tail}

        :none ->
          :none

        _ ->
          {:map, fields, tail}
      end
    end
  end

  defp expand_call(env, {:atom, Map}, :update!, [map, key, _fun], _include_private, _, stack) do
    {fields, tail} = expand_map_base(env, map, stack)

    if :none in fields do
      :none
    else
      case expand(env, key, stack) do
        {:atom, atom} ->
          {:map, fields |> Keyword.put(atom, nil), tail}

        :none ->
          :none

        _ ->
          {:map, fields, tail}
      end
    end
  end

  defp expand_call(env, {:atom, Map}, :from_struct, [struct], _include_private, _, stack) do
    # `:closed` tail = all fields known (resolved struct → literal-complete map);
    # `:open` = unknown base, so the resulting map may carry fields we never saw.
    {fields, tail} =
      case expand(env, struct, stack) do
        {:struct, fields, _, nil} ->
          {fields, :closed}

        {:atom, atom} ->
          case expand(env, {:struct, [], {:atom, atom}, nil}, stack) do
            {:struct, fields, _, nil} -> {fields, :closed}
            nil -> {[], :open}
            _ -> {[:none], nil}
          end

        nil ->
          {[], :open}

        _ ->
          {[:none], nil}
      end

    fields = Keyword.delete(fields, :__struct__)

    if :none in fields do
      :none
    else
      {:map, fields, tail}
    end
  end

  # Raising functions never return, so `:none` (bottom) — also lets them drop out
  # of branch-result unions (`if c, do: 1, else: raise "x"` is `1`).
  defp expand_call(_env, {:atom, :erlang}, fun, _args, _include_private, _, _stack)
       when fun in [:error, :throw, :exit, :raise] do
    :none
  end

  # Built-in operator result types (after expansion these are `:erlang` calls).
  # Bitwise and integer-only ops always return integer().
  defp expand_call(_env, {:atom, :erlang}, fun, _args, _include_private, _, _stack)
       when fun in [:band, :bor, :bxor, :bsl, :bsr, :bnot, :div, :rem] do
    {:integer, nil}
  end

  # Float division always returns float().
  defp expand_call(_env, {:atom, mod}, :/, _args, _include_private, _, _stack)
       when mod in [:erlang, Kernel] do
    {:float, nil}
  end

  # `+`/`-`/`*` follow the number tower: integer() if every operand is an
  # integer, float() if any is a float, otherwise number().
  defp expand_call(env, {:atom, mod}, fun, args, _include_private, _, stack)
       when mod in [:erlang, Kernel] and fun in [:+, :-, :*] and is_list(args) do
    numeric_result(env, args, stack)
  end

  # Comparisons and (strict) boolean ops / type guards return a boolean.
  defp expand_call(_env, {:atom, mod}, fun, _args, _include_private, _, _stack)
       when mod in [:erlang, Kernel] and
              fun in [
                :==,
                :"/=",
                :"=<",
                :<,
                :>=,
                :>,
                :"=:=",
                :"=/=",
                :and,
                :or,
                :not,
                :xor,
                # `:andalso`/`:orelse` are intentionally NOT here: they are not
                # boolean-typed (`true and 5` is `5`).
                :is_atom,
                :is_integer,
                :is_float,
                :is_number,
                :is_binary,
                :is_bitstring,
                :is_boolean,
                :is_list,
                :is_map,
                :is_tuple,
                :is_function,
                :is_pid,
                :is_port,
                :is_reference
              ] do
    :boolean
  end

  # function call
  defp expand_call(env, {:atom, mod}, fun, arguments, include_private, position, stack)
       when mod not in [nil, true, false] and fun not in [nil, true, false] do
    arity = length(arguments)
    # Expand argument shapes for overload filtering
    expanded_args = Enum.map(arguments, &expand(env, &1, stack))

    case expand_call_from_metadata(
           env,
           mod,
           fun,
           arity,
           expanded_args,
           include_private,
           position,
           stack
         ) do
      result when result not in [:none] ->
        result

      _ ->
        expand_call_from_introspection(
          env,
          mod,
          fun,
          arity,
          expanded_args,
          include_private,
          stack
        )
    end
  end

  # not a module
  defp expand_call(_env, {:atom, _mod}, _fun, _arity, _include_private, _, _stack), do: :none

  defp expand_call(env, {:union, variants}, fun, arity, include_private, position, stack) do
    variants
    |> Enum.map(&expand_call(env, &1, fun, arity, include_private, position, stack))
    |> Enum.reject(&(&1 in [:none, nil]))
    |> case do
      [] ->
        nil

      [single] ->
        single

      results ->
        {:union, Enum.uniq(results)}
    end
  end

  defp expand_call(_env, _target, _fun, _arity, _include_private, _, _stack), do: nil

  defp call_arity_match?(fun_arity, fun_defaults, call_arity) do
    fun_arity - fun_defaults <= call_arity and call_arity <= fun_arity
  end

  defp expand_call_from_introspection(env, mod, fun, arity, arg_shapes, include_private, stack) do
    case ElixirTypes.spec_signature_from_metadata(env, mod, fun, arity) do
      {:ok, sig} ->
        descr = ElixirTypes.extract_return_type_from_sig(sig, arg_shapes)
        shape = ElixirTypes.to_shape(descr)

        case expand(env, shape, stack) do
          nil ->
            # Native sig produced nil shape, fall back to ExCk/legacy
            do_expand_call_from_introspection(
              env,
              mod,
              fun,
              arity,
              arg_shapes,
              include_private,
              stack
            )

          result ->
            result
        end

      :error ->
        do_expand_call_from_introspection(
          env,
          mod,
          fun,
          arity,
          arg_shapes,
          include_private,
          stack
        )
    end
  end

  defp do_expand_call_from_introspection(env, mod, fun, arity, arg_shapes, include_private, stack) do
    maybe_kind_arity =
      case ElixirSense.Core.Normalized.Code.get_docs(mod, :docs) do
        nil ->
          # no docs - use call arity if fun exported
          if function_exported?(mod, fun, arity) do
            {:function, arity}
          end

        list ->
          # correct arity for calls with default params
          list
          |> Enum.find_value(nil, fn {{f, a}, _, kind, _, _, map} ->
            if f == fun and call_arity_match?(a, Map.get(map, :defaults, 0), arity) and
                 (kind != :macro or mod in env.requires) do
              {kind, a}
            end
          end)
      end

    case maybe_kind_arity do
      nil ->
        # def not found
        :none

      {:macro, _} ->
        # do not expand macro result types
        :no_spec

      {:function, arity} ->
        # Try ExCk native signature first, then fall back to legacy spec parsing
        exck_result =
          if ElixirTypes.enabled?() do
            case ExCkReader.lookup_signature(mod, fun, arity) do
              {:ok, %{sig: {sig_kind, _domain, _clauses} = sig}}
              when sig_kind in [:infer, :strong] ->
                descr = ElixirTypes.extract_return_type_from_sig(sig, arg_shapes)
                shape = ElixirTypes.to_shape(descr)
                expand(env, shape, stack)

              _ ->
                nil
            end
          end

        case exck_result do
          nil ->
            case TypeInfo.get_function_spec(mod, fun, arity) do
              {{_fun, _arity}, _asts} = type ->
                return_type = get_return_from_spec(env, type, mod, include_private)
                expand(env, return_type, stack) || :no_spec

              _ ->
                :no_spec
            end

          result ->
            result
        end
    end
  end

  defp expand_call_from_metadata(
         %Binding{specs: specs, mods_funs_to_positions: mods_funs_to_positions} = env,
         mod,
         fun,
         arity,
         arg_shapes,
         include_private,
         position,
         stack
       ) do
    maybe_kind_arity =
      Enum.find_value(mods_funs_to_positions, nil, fn
        {{^mod, ^fun, _}, %State.ModFunInfo{type: fun_type} = info} ->
          visible? =
            cond do
              include_private and
                  (State.ModFunInfo.get_category(info) != :macro or
                     List.last(info.positions) < position) ->
                true

              not include_private and Introspection.is_pub(fun_type) and
                  (State.ModFunInfo.get_category(info) != :macro or mod in env.requires) ->
                true

              true ->
                false
            end

          if visible? do
            # correct arity for calls with default params
            State.ModFunInfo.get_arities(info)
            |> Enum.find_value(nil, fn {a, defaults} ->
              if call_arity_match?(a, defaults, arity) do
                {State.ModFunInfo.get_category(info), a}
              end
            end)
          end

        _ ->
          false
      end)

    case maybe_kind_arity do
      nil ->
        # def not found
        :none

      {:macro, _} ->
        # do not expand macro result types
        :no_spec

      {:function, arity} ->
        case specs[{mod, fun, arity}] do
          nil ->
            :no_spec

          %State.SpecInfo{elixir_types_sig: {sig_kind, _domain, _clauses} = sig} = spec
          when sig_kind in [:infer, :strong] ->
            descr = ElixirTypes.extract_return_type_from_sig(sig, arg_shapes)
            shape = ElixirTypes.to_shape(descr)

            case expand(env, shape, stack) do
              nil ->
                # Native sig produced nil shape (e.g. dynamic(term())), fall back to legacy
                get_return_from_metadata(env, mod, spec, include_private, stack) || :no_spec

              result ->
                result
            end

          %State.SpecInfo{elixir_types_sig: nil} = spec ->
            get_return_from_metadata(env, mod, spec, include_private, stack) || :no_spec
        end
    end
  end

  defp extract_type({:"::", _, [_, type]}), do: {:ok, type}

  defp extract_type({:when, _, [{:"::", _, [_, type]}, type_params]}) do
    # substitute type params
    res =
      Macro.prewalk(type, fn
        {atom, _, nil} = var ->
          Keyword.get(type_params, atom, var)

        other ->
          other
      end)

    {:ok, res}
  end

  defp extract_type(_), do: :error

  defp get_return_from_metadata(
         env,
         mod,
         %State.SpecInfo{specs: [func_spec]},
         include_private,
         stack
       ) do
    case Code.string_to_quoted(func_spec, emit_warnings: false) do
      {:ok, {:@, _, [{_kind, _, [ast]}]}} ->
        case extract_type(ast) do
          {:ok, type} ->
            parsed_type = parse_type(env, type, mod, include_private, [])
            expand(env, parsed_type, stack)

          :error ->
            nil
        end

      _ ->
        nil
    end
  end

  # intersection specs
  # treat as union
  # TODO get correct basing on call args
  defp get_return_from_metadata(
         env,
         mod,
         %State.SpecInfo{specs: [_ | _] = variants} = spec,
         include_private,
         stack
       ) do
    {:union,
     variants
     |> Enum.map(fn variant ->
       get_return_from_metadata(
         env,
         mod,
         %{spec | specs: [variant]},
         include_private,
         stack
       )
     end)}
  end

  defp get_return_from_spec(_env, nil, _, _include_private), do: nil

  defp get_return_from_spec(env, {{fun, _arity}, [ast]}, mod, include_private) do
    case Typespec.spec_to_quoted(fun, ast) |> extract_type do
      {:ok, type} ->
        parse_type(env, type, mod, include_private, [])

      :error ->
        nil
    end
  end

  # intersection specs
  # treat as union
  # TODO get correct basing on call args
  defp get_return_from_spec(env, {{fun, arity}, [_ast | _] = variants}, mod, include_private) do
    {:union,
     variants |> Enum.map(&get_return_from_spec(env, {{fun, arity}, [&1]}, mod, include_private))}
  end

  # union type
  defp parse_type(env, {:|, _, variants}, mod, include_private, stack) do
    {:union, variants |> Enum.map(&parse_type(env, &1, mod, include_private, stack))}
  end

  # struct
  defp parse_type(
         env,
         {:%, _,
          [
            struct_mod,
            {:%{}, _, fields}
          ]},
         mod,
         include_private,
         stack
       ) do
    fields =
      for {field, type} <- fields,
          is_atom(field),
          do: {field, parse_type(env, type, mod, include_private, stack)}

    module =
      case struct_mod do
        m when is_atom(m) ->
          m

        {:__aliases__, _, list} ->
          Module.concat(list)

        _ ->
          nil
      end

    if module do
      {:struct, fields, {:atom, module}, nil}
    end
  end

  # map
  defp parse_type(env, {:%{}, _, fields}, mod, include_private, stack) do
    fields =
      for {field, type} <- fields,
          field = drop_optional(field),
          is_atom(field),
          do: {field, parse_type(env, type, mod, include_private, stack)}

    {:map, fields, nil}
  end

  defp parse_type(_env, {:map, _, []}, _mod, _include_private, _stack) do
    {:map, [], nil}
  end

  defp parse_type(env, {:{}, _, fields}, mod, include_private, stack) do
    {:tuple, length(fields),
     fields |> Enum.map(&parse_type(env, &1, mod, include_private, stack))}
  end

  defp parse_type(_env, [], _mod, _include_private, _stack) do
    {:list, :empty}
  end

  defp parse_type(env, [type | _], mod, include_private, stack) do
    {:list, parse_type(env, type, mod, include_private, stack)}
  end

  # for simplicity we skip terminator type
  defp parse_type(env, {kind, _, [type, _]}, mod, include_private, stack)
       when kind in [:maybe_improper_list, :nonempty_improper_list, :nonempty_maybe_improper_list] do
    {:list, parse_type(env, type, mod, include_private, stack)}
  end

  defp parse_type(_env, {:list, _, []}, _mod, _include_private, _stack) do
    {:list, nil}
  end

  defp parse_type(_env, {:keyword, _, []}, _mod, _include_private, _stack) do
    # no support for atom type for now
    {:list, {:tuple, 2, [nil, nil]}}
  end

  defp parse_type(env, {:keyword, _, [type]}, mod, include_private, stack) do
    # no support for atom type for now
    {:list, {:tuple, 2, [nil, parse_type(env, type, mod, include_private, stack)]}}
  end

  # remote user type
  defp parse_type(env, {{:., _, [remote, type]}, _, args}, _mod, _include_private, stack) do
    module = resolve_type_module(env, remote)

    if module && is_atom(type) do
      # do not propagate include_private when expanding remote types
      expand_type(env, module, type, args, false, stack)
    end
  end

  # no_return
  defp parse_type(_env, {:no_return, _, _}, _, _include_private, _stack), do: :none

  # term, any, dynamic
  defp parse_type(_env, {kind, _, _}, _, _include_private, _stack)
       when kind in [:term, :any, :dynamic],
       do: nil

  # built-in primitive types that must not be resolved as user types
  defp parse_type(_env, {kind, _, []}, _, _include_private, _stack)
       when kind in [
              :atom,
              :integer,
              :float,
              :binary,
              :bitstring,
              :boolean,
              :pid,
              :port,
              :reference,
              :number,
              :char,
              :charlist,
              :fun,
              :module,
              :node,
              :iodata,
              :iolist,
              :timeout,
              :pos_integer,
              :neg_integer,
              :non_neg_integer,
              :byte,
              :arity,
              :identifier,
              :struct,
              :as_boolean,
              :mfa,
              :string,
              :nonempty_binary,
              :nonempty_bitstring,
              :nonempty_charlist,
              :nonempty_list,
              :nonempty_maybe_improper_list,
              :nonempty_improper_list,
              :maybe_improper_list
            ],
       do: nil

  # local user type
  defp parse_type(env, {atom, _, args}, mod, include_private, stack) when is_atom(atom) do
    # propagate include_private when expanding local types
    expand_type(env, mod, atom, args, include_private, stack)
  end

  # atom
  defp parse_type(_env, atom, _, _include_private, _stack) when is_atom(atom), do: {:atom, atom}

  defp parse_type(_env, integer, _, _include_private, _stack) when is_integer(integer) do
    {:integer, integer}
  end

  # other
  defp parse_type(_env, _type, _, _include_private, _stack), do: nil

  defp expand_type(env, mod, type_name, args, include_private, stack) do
    arity = if(is_list(args), do: length(args), else: 0)
    type = {mod, type_name, arity}

    if type in stack do
      # self referential type
      nil
    else
      do_expand_type(env, mod, type_name, args, include_private, [type | stack])
    end
  end

  defp do_expand_type(env, mod, type_name, args, include_private, stack) do
    arity = if(is_list(args), do: length(args), else: 0)

    case expand_type_from_metadata(env, mod, type_name, arity, include_private, stack) do
      nil -> expand_type_from_introspection(env, mod, type_name, arity, include_private, stack)
      res -> res
    end
    |> drop_no_spec
  end

  defguardp type_is_public(kind, include_private)
            when kind == :type or kind == :nominal or include_private

  defp expand_type_from_metadata(
         %Binding{types: types} = env,
         mod,
         type_name,
         arity,
         include_private,
         stack
       ) do
    case types[{mod, type_name, arity}] do
      %State.TypeInfo{specs: [type_spec], kind: kind}
      when type_is_public(kind, include_private) ->
        case Code.string_to_quoted(type_spec, emit_warnings: false) do
          {:ok, {:@, _, [{_kind, _, [ast]}]}} ->
            case extract_type(ast) do
              {:ok, type} ->
                parse_type(env, type, mod, include_private, stack) || :no_spec

              :error ->
                nil
            end

          _ ->
            :no_spec
        end

      nil ->
        nil

      _ ->
        :no_spec
    end
  end

  defp expand_type_from_introspection(env, mod, type_name, arity, include_private, stack) do
    case TypeInfo.get_type_spec(mod, type_name, arity) do
      {kind, spec} when type_is_public(kind, include_private) ->
        {:"::", _, [{_expanded_name, _, _}, type]} = Typespec.type_to_quoted(spec)

        parse_type(env, type, mod, include_private, stack)

      _ ->
        nil
    end
  end

  defp drop_optional({:optional, _, [key]}), do: key
  defp drop_optional(other), do: other

  defp resolve_type_module(_env, module) when is_atom(module), do: module

  defp resolve_type_module(
         %Binding{module: mod, aliases: aliases} = _env,
         {:__MODULE__, _, _} = ast
       ) do
    # AST->module resolution delegated to ModuleResolver. A plain map (not the
    # struct) is passed so dialyzer's success typing is satisfied.
    # credo:disable-for-next-line Credo.Check.Refactor.Nesting
    case ModuleResolver.resolve(ast, %{module: mod, aliases: aliases}) do
      {:ok, module} -> module
      :error -> nil
    end
  end

  defp resolve_type_module(%Binding{attributes: attrs} = env, {:@, _, [{attr, _, _}]})
       when is_atom(attr) do
    case Enum.find(attrs, &(&1.name == attr)) do
      %State.AttributeInfo{type: {:atom, module}} when is_atom(module) ->
        module

      %State.AttributeInfo{type: {:attribute, nested_attr}} when is_atom(nested_attr) ->
        resolve_type_module(env, {:@, [], [{nested_attr, [], nil}]})

      _ ->
        nil
    end
  end

  defp resolve_type_module(%Binding{vars: vars}, {var, _, context})
       when is_atom(var) and is_atom(context) do
    case Enum.find(vars, &(&1.name == var)) do
      %State.VarInfo{type: {:atom, module}} when is_atom(module) -> module
      _ -> nil
    end
  end

  defp resolve_type_module(
         %Binding{module: mod, aliases: aliases} = _env,
         {:__aliases__, _, _list} = ast
       ) do
    # Alias resolution (incl. the no-alias fallback) is delegated to
    # `ModuleResolver.resolve`, which falls back to `Module.concat(parts)` when
    # no alias applies, so `nil`/`:error` here only arises for genuinely
    # unresolvable forms. A plain map (not the struct) is passed so dialyzer's
    # success typing (struct vs plain-map mismatch) is satisfied.
    # credo:disable-for-next-line Credo.Check.Refactor.Nesting
    case ModuleResolver.resolve(ast, %{module: mod, aliases: aliases}) do
      {:ok, module} -> module
      :error -> nil
    end
  end

  defp resolve_type_module(%Binding{} = env, {{:., _, [base, nested]}, _, []})
       when is_atom(nested) do
    case resolve_type_module(env, base) do
      # Exclude nil so the `_ -> nil` fallback stays reachable (dialyzer would
      # otherwise emit pattern_match_cov).
      module when is_atom(module) and not is_nil(module) -> Module.concat(module, nested)
      _ -> nil
    end
  end

  defp resolve_type_module(_env, _), do: nil

  defp combine_intersection(:none, _), do: :none
  defp combine_intersection(_, :none), do: :none
  defp combine_intersection(nil, type), do: type
  defp combine_intersection(type, nil), do: type
  defp combine_intersection(type, type), do: type

  # Descr-backed intersection. When the native typesystem is enabled AND both
  # operands are `descr_exact?` (static coercion loses no information), the real
  # `Descr.intersection/2` is strictly more faithful than the structural
  # approximation below.
  #
  # Dynamic-wrapping is safe: the coercion yields `dynamic(static(shape))` and
  # `dynamic(a) ∩ dynamic(b) = dynamic(a ∩ b)`, so `to_shape/1` (which unwraps the
  # outer `dynamic`) gives exactly the static intersection. (NOT true for
  # difference — see `difference/2`.) Empty intersection → `:none`; an
  # unrepresentable (`nil`) result falls through to the custom clauses; the whole
  # attempt is try/rescue-guarded against backend drift.
  defp combine_intersection(a, b) do
    case descr_backed_intersection(a, b) do
      :__fallthrough__ -> combine_intersection_custom(a, b)
      result -> result
    end
  end

  defp descr_backed_intersection(a, b) do
    with_descr_backing(a, b, fn descr_a, descr_b ->
      result = ElixirTypes.descr_intersection(descr_a, descr_b)

      if ElixirTypes.descr_empty?(result) do
        :none
      else
        case ElixirTypes.to_shape(result) do
          # Unconvertible result — don't lose the custom path's precision.
          nil -> :__fallthrough__
          shape -> shape
        end
      end
    end)
  end

  # Whitelist of shape kinds whose *static* coercion to a `Descr.t()` is exact
  # (round-trips without widening), so the descr-backed intersection above is
  # faithful. Excluded: maps/structs (coercion makes maps OPEN); literal scalars
  # (`{:integer, _}` etc., which widen to their base); `:bitstring`,
  # `:non_struct_map`, generic `:list`/`:tuple`, `:fun`, `:dynamic`, `:term`.
  # `:number` is exact (the `integer() | float()` union coerces exactly).
  defp descr_exact?(:atom), do: true
  defp descr_exact?({:atom, atom}) when is_atom(atom), do: true
  defp descr_exact?(:boolean), do: true
  defp descr_exact?(:integer), do: true
  defp descr_exact?(:float), do: true
  defp descr_exact?(:number), do: true
  defp descr_exact?(:binary), do: true
  defp descr_exact?(:pid), do: true
  defp descr_exact?(:port), do: true
  defp descr_exact?(:reference), do: true
  defp descr_exact?(:empty_list), do: true
  defp descr_exact?({:list, :empty}), do: true
  defp descr_exact?(:empty_map), do: true
  defp descr_exact?({:list, elem}), do: descr_exact?(elem)
  defp descr_exact?({:nonempty_list, elem}), do: descr_exact?(elem)

  defp descr_exact?({:tuple, _arity, elems}) when is_list(elems),
    do: Enum.all?(elems, &descr_exact?/1)

  defp descr_exact?({:union, members}) when is_list(members),
    do: Enum.all?(members, &descr_exact?/1)

  defp descr_exact?(_other), do: false

  # Property-test surface (test/elixir_sense/core/binding_descr_property_test.exs).
  # Exposes the private shape algebra so it can be checked against the native
  # Descr ground truth. Not part of the public API.
  @doc false
  def __combine_intersection__(a, b), do: combine_intersection(a, b)

  @doc false
  def __normalize_union__(members), do: normalize_union(members)

  @doc false
  def __covers__?(a, b), do: covers?(a, b)

  @doc false
  def __descr_exact__?(shape), do: descr_exact?(shape)

  # NOTE intersection is not strict and does an union on map keys

  defp combine_intersection_custom({:struct, fields_1, nil, nil}, {:struct, fields_2, nil, nil}) do
    keys = (safe_keys(fields_1) ++ safe_keys(fields_2)) |> Enum.uniq()

    fields =
      for k <- keys, do: {k, combine_intersection(field_get(fields_1, k), field_get(fields_2, k))}

    if Enum.any?(fields, fn {_k, v} -> v == :none end) do
      :none
    else
      {:struct, fields, nil, nil}
    end
  end

  defp combine_intersection_custom(
         {:struct, fields_1, type, nil},
         {:struct, fields_2, type_2, nil}
       )
       when type_2 == type or is_nil(type_2) do
    keys = safe_keys(fields_1)

    fields =
      for k <- keys, do: {k, combine_intersection(field_get(fields_1, k), field_get(fields_2, k))}

    if Enum.any?(fields, fn {_k, v} -> v == :none end) do
      :none
    else
      {:struct, fields, type, nil}
    end
  end

  # Two structs of *different* concrete modules are disjoint.
  defp combine_intersection_custom(
         {:struct, _fields_1, {:atom, mod_1}, nil},
         {:struct, _fields_2, {:atom, mod_2}, nil}
       )
       when mod_1 != mod_2,
       do: :none

  defp combine_intersection_custom(
         {:struct, _fields_1, nil, nil} = s1,
         {:struct, _fields_2, _type, nil} = s2
       ) do
    combine_intersection(s2, s1)
  end

  defp combine_intersection_custom({:map, fields_1, tail_1}, {:map, fields_2, tail_2})
       when tail_intersectable?(tail_1) and tail_intersectable?(tail_2) do
    keys = (safe_keys(fields_1) ++ safe_keys(fields_2)) |> Enum.uniq()

    fields =
      for k <- keys, do: {k, combine_intersection(field_get(fields_1, k), field_get(fields_2, k))}

    if Enum.any?(fields, fn {_k, v} -> v == :none end) do
      :none
    else
      {:map, fields, intersect_tails(tail_1, tail_2)}
    end
  end

  defp combine_intersection_custom({:struct, fields_1, type, nil}, {:map, fields_2, tail_2})
       when tail_intersectable?(tail_2) do
    keys =
      if type != nil,
        do: safe_keys(fields_1),
        else: (safe_keys(fields_1) ++ safe_keys(fields_2)) |> Enum.uniq()

    fields =
      for k <- keys, do: {k, combine_intersection(field_get(fields_1, k), field_get(fields_2, k))}

    if Enum.any?(fields, fn {_k, v} -> v == :none end) do
      :none
    else
      {:struct, fields, type, nil}
    end
  end

  defp combine_intersection_custom(
         {:map, _fields_1, tail} = map,
         {:struct, _fields_2, _type, nil} = str
       )
       when tail_intersectable?(tail) do
    combine_intersection(str, map)
  end

  defp combine_intersection_custom({:tuple, n, fields_1}, {:tuple, n, fields_2}) do
    combined_fields =
      Enum.zip(fields_1, fields_2) |> Enum.map(fn {f1, f2} -> combine_intersection(f1, f2) end)

    if :none in combined_fields do
      :none
    else
      {:tuple, n, combined_fields}
    end
  end

  # Fixed-arity tuples of different sizes are disjoint.
  defp combine_intersection_custom({:tuple, n1, _}, {:tuple, n2, _}) when n1 != n2, do: :none

  # Union on the left; must come *before* the flip clause below, else `union ∩
  # union` flips left/right forever.
  defp combine_intersection_custom({:union, variants}, other) do
    # Collect *every* non-empty overlap — `(:a | :b | :c) ∩ (:b | :c)` is
    # `:b | :c`, not `:b`. Empty overlap is `:none` (bottom), never nil.
    variants
    |> Enum.map(&combine_intersection(&1, other))
    |> Enum.reject(&(&1 == :none))
    |> case do
      [] -> :none
      results -> normalize_union(results)
    end
  end

  defp combine_intersection_custom(other, {:union, variants}),
    do: combine_intersection({:union, variants}, other)

  # List ∩ list: intersect element types. Both `{:list, _}` include `[]`, so the
  # result is never `:none` even when the element intersection bottoms out (e.g.
  # `list(:a) ∩ list(:b)` is `{:list, :empty}`). A nonempty side forces nonempty.
  defp combine_intersection_custom({:list, e1}, {:list, e2}) do
    {:list, intersect_list_elem(e1, e2)}
  end

  defp combine_intersection_custom({:nonempty_list, e1}, {:nonempty_list, e2}) do
    case intersect_list_elem(e1, e2) do
      :empty -> :none
      elem -> {:nonempty_list, elem}
    end
  end

  defp combine_intersection_custom({:nonempty_list, e1}, {:list, e2}) do
    case intersect_list_elem(e1, e2) do
      :empty -> :none
      elem -> {:nonempty_list, elem}
    end
  end

  defp combine_intersection_custom({:list, _} = l, {:nonempty_list, _} = nl) do
    combine_intersection(nl, l)
  end

  # The generic `:list` atom intersected with a concrete list shape is that
  # concrete shape.
  defp combine_intersection_custom(:list, {tag, _} = l) when tag in [:list, :nonempty_list], do: l
  defp combine_intersection_custom({tag, _} = l, :list) when tag in [:list, :nonempty_list], do: l

  # Two improper non-empty lists (3-tuples): intersect elementwise (prefix via
  # `intersect_list_elem`, tail via general intersection; a `:none` tail bottoms
  # the shape). Mixed proper/improper pairs fall through to the conservative
  # fallback (kinds not provably disjoint — `shape_kind` keeps both `:list`).
  defp combine_intersection_custom({:nonempty_list, e1, t1}, {:nonempty_list, e2, t2}) do
    case combine_intersection(t1, t2) do
      :none -> :none
      tail -> {:nonempty_list, intersect_list_elem(e1, e2), tail}
    end
  end

  # Scalar specificity: if one side subsumes the other the intersection is the
  # narrower (e.g. `integer() and 5` is `5`). Otherwise conservative: a
  # non-subsuming pair is only `:none` when the top-level kinds are provably
  # disjoint; same-kind or unknown-kind pairs intersect to `nil` (avoids
  # over-claiming disjointness).
  defp combine_intersection_custom(a, b) do
    cond do
      covers?(a, b) -> b
      covers?(b, a) -> a
      disjoint_kinds?(a, b) -> :none
      disjoint_literals?(a, b) -> :none
      true -> nil
    end
  end

  # Two distinct concrete literals of the same scalar kind are disjoint
  # (`:a ∩ :b`, `1 ∩ 2` are `:none`). A same-kind pair with an abstract side is
  # handled by `covers?/2` above, so this only fires for two unequal pinned values.
  defp disjoint_literals?({tag, v1}, {tag, v2})
       when tag in [:atom, :integer, :float, :binary] and not is_nil(v1) and not is_nil(v2),
       do: v1 != v2

  defp disjoint_literals?(_a, _b), do: false

  # Element intersection for two list shapes. Unknown (`nil`) yields the other
  # side; `:empty` (element of `[]`) intersected with anything is `:empty`.
  defp intersect_list_elem(:empty, _), do: :empty
  defp intersect_list_elem(_, :empty), do: :empty
  defp intersect_list_elem(nil, e), do: e
  defp intersect_list_elem(e, nil), do: e

  defp intersect_list_elem(e1, e2) do
    case combine_intersection(e1, e2) do
      # Disjoint element types still leave `[]` in a (possibly-empty) list, so the
      # element collapses to `:empty` rather than the whole list to `:none`.
      :none -> :empty
      other -> other
    end
  end

  # Provably-disjoint top-level shape kinds. Only returns true for pairs whose
  # runtime value sets cannot overlap. Unknown/new shape kinds fall through to
  # `false` (conservative). Same-kind pairs are also `false` (handled above or
  # genuinely overlapping).
  defp disjoint_kinds?(a, b) do
    ka = shape_kind(a)
    kb = shape_kind(b)
    ka != nil and kb != nil and ka != kb
  end

  # Coarse top-level kind classifier used only for disjointness. Returns nil for
  # shapes whose kind we don't want to assert disjointness on (unknown/new
  # shapes), so they never get collapsed to `:none`.
  defp shape_kind(:atom), do: :atom
  defp shape_kind(:boolean), do: :atom
  defp shape_kind({:atom, _}), do: :atom
  defp shape_kind(:integer), do: :number
  defp shape_kind({:integer, _}), do: :number
  defp shape_kind(:float), do: :number
  defp shape_kind({:float, _}), do: :number
  defp shape_kind(:number), do: :number
  defp shape_kind(:binary), do: :binary
  defp shape_kind({:binary, _}), do: :binary
  defp shape_kind(:bitstring), do: :binary
  defp shape_kind(:list), do: :list
  defp shape_kind({:list, _}), do: :list
  defp shape_kind({:nonempty_list, _}), do: :list
  # The improper 3-tuple is kept `:list`-kind so the disjointness fallback never
  # claims `:none` against a proper list (an unsound `:none` would be worse).
  defp shape_kind({:nonempty_list, _, _}), do: :list
  defp shape_kind(:tuple), do: :tuple
  defp shape_kind({:tuple, _, _}), do: :tuple
  defp shape_kind({:map, _, _}), do: :map
  defp shape_kind({:struct, _, _, _}), do: :map
  defp shape_kind(:pid), do: :pid
  defp shape_kind(:port), do: :port
  defp shape_kind(:reference), do: :reference
  # integer and float both map to :number, so they are NOT treated as disjoint
  # here (an over-claim of disjointness is worse than a miss). Everything else is
  # left unclassified.
  defp shape_kind(_other), do: nil

  defp expand_map_fields(env, map_or_struct, stack) do
    {fields, _tail} = expand_map_base(env, map_or_struct, stack)
    fields
  end

  # Expand a map/struct base used as an operand of `Map.put`/`merge`/etc.
  # Returns `{fields, tail}` where `tail` is:
  #   * `:closed` — literal-complete map or resolved struct (all keys known);
  #   * `nil`     — partial map (closedness unknown);
  #   * `:open`   — open map (additional unknown keys exist);
  #   * `[:none]` (as fields) — provably not a map (bottom).
  # SOUNDNESS: synthesising results from an OPEN/partial base must NOT assert the
  # absence of unseen keys; only a `:closed` base licenses that.
  defp expand_map_base(env, map_or_struct, stack) do
    case expand(env, map_or_struct, stack) do
      {:map, fields, tail} when tail in [nil, :open, :closed] -> {fields, tail}
      # A resolved struct is literal-complete (all defstruct keys known).
      {:struct, fields, _, nil} -> {fields, :closed}
      # Unknown base: we know nothing about it, so it may carry any keys.
      nil -> {[], :open}
      _ -> {[:none], nil}
    end
  end

  # Combining two map tails (e.g. `Map.merge/2`):
  #   * open if either operand is open (additional unknown keys remain);
  #   * closed only if BOTH operands are closed (both key sets fully known);
  #   * otherwise partial (`nil`) — at least one operand has unknown closedness.
  defp merge_tails(:open, _), do: :open
  defp merge_tails(_, :open), do: :open
  defp merge_tails(:closed, :closed), do: :closed
  defp merge_tails(_, _), do: nil

  # Intersecting two map tails (map ∩ map): a `:closed` operand pins the result
  # closed (it constrains the key set); two partials stay partial.
  defp intersect_tails(:closed, _), do: :closed
  defp intersect_tails(_, :closed), do: :closed
  defp intersect_tails(_, _), do: nil

  def from_var(value) when is_atom(value) do
    {:atom, value}
  end

  def from_var(%type{} = struct) do
    fields =
      for {key, value} <- struct |> Map.from_struct() do
        {key, from_var(value)}
      end

    {:struct, fields |> Keyword.put(:__struct__, {:atom, type}), {:atom, type}, nil}
  end

  def from_var(map) when is_map(map) do
    fields =
      for {key, value} <- map do
        {key, from_var(value)}
      end

    {:map, fields, nil}
  end

  def from_var(int) when is_integer(int), do: {:integer, int}

  def from_var(tuple) when is_tuple(tuple) do
    list =
      tuple
      |> Tuple.to_list()
      |> Enum.map(&from_var(&1))

    {:tuple, length(list), list}
  end

  def from_var(_), do: nil

  # All field keys (atom or `{:domain, _}`), so map-merge conflict detection
  # covers domain keys too. The `{key, _}` filter skips any `:none` sentinel.
  defp safe_keys(maybe_keyword) do
    for {key, _} <- maybe_keyword do
      key
    end
  end
end
