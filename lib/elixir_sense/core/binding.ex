defmodule ElixirSense.Core.Binding do
  @moduledoc """
  Binding holds the type environment (variables, attributes, specs, etc.) for
  a single cursor position and drives the structural type-algebra engine used
  for completion, hover, and go-to-definition.

  ## Shape vocabulary

  Every resolved type is represented as a *shape* — a plain Elixir term.  The
  full vocabulary is defined here.  Unless noted, shapes are produced by
  `expand/3` / `do_expand/3` and consumed by `TypePresentation` (rendering),
  `covers?/2` (subsumption), `combine_intersection/2`, and
  `normalize_union/1`.

  ### `nil` — unknown / top

  Meaning: "we have no information; the value could be anything."

  Producer: any fallback `do_expand` clause, `parse_type` for `:term`/`:any`/
  `:dynamic`, `quoted_to_shape` for `dynamic()`, unresolvable variables, and
  rescued errors (e.g. `Application.get_env` when the key is absent).

  Consumer / algebra:
  - `normalize_union/1`: a `nil` member *poisons* the whole union to `nil`
    (absorbing element). Rationale: once one branch is unbounded the union
    is unbounded.
  - `combine_intersection/2`: `nil ∩ T = T` (nil is the identity, i.e. the
    top); `T ∩ nil = T`.  (See `combine_intersection/2` clauses 3–4.)
  - `covers?(nil, _b)` returns `true` (nil as subtrahend covers anything —
    reached only inside tuple element recursion).
  - Rendering (`TypePresentation.render/1`): `:unknown` → `"term()"` inside
    structures.

  Note: at the `to_shape` boundary `dynamic()` and `dynamic(inner)` are
  both unwrapped to `nil` (or to the inner shape) — see `quoted_to_shape/1`.
  The `{:dynamic, _}` shape tuple is therefore NOT produced by `to_shape/1`;
  it only appears in `TypePresentation` as a display-only remnant from earlier
  code paths (task #20 / policy C).  See the `{:dynamic, _}` entry below.

  ### `:none` — bottom

  Meaning: "this branch never produces a value" (e.g. `raise`, dead code,
  type error).

  Producers: `parse_type` for `no_return()`, unresolvable attribute
  (attribute not found), non-map base in map update, struct with invalid
  module, calls to `:erlang.error/:throw/:exit/:raise`, `Application.get_env`
  returning `:error` for `fetch_env!/1`.

  Consumer / algebra:
  - `normalize_union/1`: `:none` members are *dropped* from unions
    (identity / bottom element).  An all-`:none` union collapses to `:none`.
  - `combine_intersection/2`: `:none ∩ T = :none`; `T ∩ :none = :none`
    (annihilator).
  - `covers?(:none, _)` / `covers?(_, :none)` — NOT handled; `:none` is not
    a valid left-hand of `covers?`.  It falls through to `false`.
  - Rendering: `"none()"`.

  Inconsistency flag: `:none` propagation in `expand_call` for union targets
  (line ~1931) filters `:none` and `nil` together before building the result
  union, which means a fully-`:none` union of targets returns `nil` (unknown)
  rather than `:none`.  This is conservative (avoids false "dead-code"
  claims) but differs from how a strict bottom would behave.

  ### `:not_set` — known-absent map value

  Meaning: a map key that is provably *not present* in the map (the compiler
  knows `not is_map_key(map, k)`).

  Producers: `ElixirTypes.to_shape_eager/1` when the descr has only an
  `:optional` key with no inner type (i.e. `not_set()`); also produced
  directly by `type_inference/guard.ex`'s `not_set_map_type/1` helper which
  builds `{:map, [{key, :not_set}], nil}` shapes for `not is_map_key` guards.
  `Descr.not_set()` round-trips via `coerce_static_descr(:not_set)`.

  Consumer / filter: both `TypePresentation.fields_for_receiver/2` and
  `CompletionEngine` filter out any field whose value is `:not_set` — these
  keys are not real fields for completion or hover purposes.  The
  `segment/1` fallback renders it as `"not_set()"`.

  Note: `:not_set` is only meaningful as a *field value* inside a map shape;
  it is never a stand-alone variable type.  `do_expand` has no clause for it
  and falls through to the `nil`-returning catch-all.

  ### `{:list, :empty}` / `:empty_list`

  Two spellings for the same semantic — the empty list `[]`.

  - `{:list, :empty}` is the *canonical* internal form produced by
    `parse_type` for the `[]` literal, by `Tuple.to_list` on an empty tuple,
    and by `List.wrap(nil)`.
  - `:empty_list` is an alias produced by `ElixirTypes.to_shape` when
    `Descr.to_quoted` emits `{:empty_list, [], []}`.  `do_expand` maps it
    back to `{:list, :empty}` immediately (line ~543).

  Algebra: `covers?({:list, :empty}, {:list, :empty})` is true; both are
  subsumed by `:list` and by `{:list, nil}` (via `list_elem_covers?`).

  Rendering: both render as `"empty_list()"`.

  ### List shapes — proper vs improper (`{:nonempty_list, elem, tail}`)

  Three list shapes (plus the `:list` top and `{:list, :empty}` empty list):

  - `{:list, elem}` — a PROPER list INCLUDING the empty list `[]`. Element type
    `elem` (`nil` = unknown element).
  - `{:nonempty_list, elem}` (2-tuple) — a PROPER NON-EMPTY list. Same element
    semantics; guarantees at least one cell and a `[]` terminator.
  - `{:nonempty_list, elem, tail}` (3-tuple) — a NON-EMPTY, POSSIBLY-IMPROPER
    list: a proper prefix of `elem` elements terminated by `tail` (the final
    tail type, e.g. `2` in `[1 | 2]`). When `tail` is itself a list this would
    be proper, but producers only emit the 3-tuple when `tail` is a KNOWN
    NON-LIST shape, so it always denotes a genuinely improper list.

    Producers:
      * `ElixirTypes.to_shape/1` of a `non_empty_list(a, tail)` quoted form
        (the explicit-tail variant) — both sides converting.
      * the `++` rule (`expand_call`): known non-empty proper LHS `++` known
        non-list RHS → `{:nonempty_list, left_elem, right_shape}`.
      * `TypeInference.type_of` for a cons `[h | t]` whose tail `t` resolves to
        a known non-list shape.

    Algebra (deliberately conservative — improper lists are a separate kind):
      * `covers?`: a proper `{:list, _}`/`{:nonempty_list, _}` does NOT cover
        the 3-tuple, and the 3-tuple covers only an equal-kind 3-tuple
        (elementwise prefix + tail). Improper is neither subsumed by nor
        subsumes proper lists.
      * `combine_intersection`: two 3-tuples intersect elementwise (prefix via
        `intersect_list_elem`, tail via the general intersection; a `:none`
        tail bottoms the whole shape). Any proper/improper mix falls to the
        conservative `nil` (the kinds are not provably disjoint — see below).
      * `shape_kind` keeps the 3-tuple at `:list`-kind so the disjointness
        fallback never claims `:none` against a proper list (an over-claim of
        disjointness would be worse than a missed one).

    Rendering: `non_empty_list(elem, tail)` (the compiler's spelling).

    Coercion (`ElixirTypes`): probes `Descr.non_empty_list/2`; when present →
    `dynamic(non_empty_list(coerce(elem), coerce(tail)))`. If the arity-2
    constructor is missing the fallback is `Descr.dynamic()` (unknown) — NOT a
    widened proper list, which would be UNSOUND (improper lists are not in
    `list(t)`). The 3-tuple is intentionally left OUT of the `descr_exact?`
    whitelist: exactness holds only when the arity-2 constructor exists, and
    keeping it off the whitelist avoids asserting a precision we can't always
    deliver.

  ### Map tail — `:closed` vs `nil` (partial) vs `:open` (three-marker model)

  The third element of `{:map, fields, tail}` records what we know about the
  map's COMPLETENESS — i.e. whether keys beyond `fields` may exist:

  - `:closed` tail ("literal-complete"): **all** keys are known; no other key
    can be present.  Producers (only where completeness is CERTAIN):
      * `TypeInference.type_of` for a map literal in EXPRESSION context —
        `%{a: 1}` as an expression CONSTRUCTS a map with exactly key `:a`, so
        `{:map, [a: {:integer, 1}], :closed}`.  (As a PATTERN it is `nil` —
        see below.)
      * `ElixirTypes.to_shape/1` of a closed descr (no `:...` marker, no domain
        keys) — the descr round-trip is closed-by-default.
      * `Map.new/0` (empty `:closed`), `Map.put`/`merge`/`delete`/`update` on a
        `:closed` base, and `Map.from_struct/1` of a resolved struct.

  - `nil` tail ("partial"): **at least** these keys are present; closedness is
    UNKNOWN (other keys may or may not exist).  This is the conservative legacy
    default.  Producers: map literals in `:match` (PATTERN) context — `%{a: 1}`
    as a pattern matches any map that HAS `:a`; guard facts in
    `type_inference/guard.ex` (e.g. `is_map_key(m, :a)` narrowing); `parse_type`
    for typespec maps; any fallback that knows some keys but not closedness.

  - `:open` tail ("open map"): **additional unknown keys DO exist**.
    Produced when a map update's base cannot be resolved
    (`def f(m), do: %{m | a: 1}` → `{:map, [a: nil], :open}`), when
    `expand_map_base` sees an unknown base (`nil` → `{[], :open}`), and by
    `Map.from_struct` on an unresolvable struct.

  Consumer / algebra:
  - `covers?({:map, [], tail}, {:map, _, _})` when `tail in [nil, :open]`
    returns `true` — the empty-field PARTIAL or OPEN map is the map top.
    A `:closed` empty-field map (`%{}`) is NOT the top (it is the empty map),
    so it is deliberately excluded from that guard.
  - `merge_tails/2` (e.g. `Map.merge`): `:open` if either is `:open`; `:closed`
    only if BOTH are `:closed`; otherwise `nil` (partial).
  - `intersect_tails/2` (map ∩ map): `:closed` if either operand is `:closed`
    (intersection pins the key set); otherwise `nil`.
  - `expand_map_base/3` propagates the tail; a resolved struct base is `:closed`.
  - `map_key` access (`do_expand({:map_key, ...})`): a key MISSING from a
    `:closed` map is `:not_set` (PROVABLY absent — precision win); missing from a
    `nil`-partial or `:open` map is `nil` (unknown — other keys may exist).  A
    `nil`-partial map must NEVER yield `:not_set`; that would be unsound.
  - Coercion (`ElixirTypes`): `:closed` → `Descr.closed_map` (dynamic-wrapped),
    EXACT; `nil` and `:open` → `open_map` (closing a partial/open map would
    wrongly assert other keys absent).

  Rendering note (DELIBERATE display compromise — see `TypePresentation`):
  `:closed` renders WITHOUT a marker (`%{key: type}`), exactly as the old `nil`
  did.  `nil`-PARTIAL also KEEPS that same marker-less rendering FOR NOW: adding
  a `...`/partial marker would churn every guard-fact display expectation, so the
  partial-vs-closed distinction is intentionally NOT surfaced in the rendered
  string.  `:open` keeps its `...` marker (`%{..., key: type}` / `map()`).

  ### `{:dynamic, _}` — display-only, not in Binding algebra

  The shape `{:dynamic, nil}` and `{:dynamic, inner}` are defined only in
  `TypePresentation.segment/1` and `widen_literals/1` as carry-over from an
  earlier design.  **No `do_expand` clause handles them**, so any
  `{:dynamic, _}` that somehow reached `expand/3` would fall through to the
  `nil`-returning catch-all.

  Grep evidence: `grep -rn '{:dynamic' lib/elixir_sense/core/` finds no
  producer in `binding.ex` or `type_inference.ex`.  The only occurrences
  outside `elixir_types.ex` are `type_presentation.ex` (rendering) and
  `compiler/state.ex` (unrelated compiler state tag).

  `to_shape/1` actively strips `{:dynamic, [], []}` → `nil` and
  `{:dynamic, [], [inner]}` → `quoted_to_shape(inner)`, so `{:dynamic, _}`
  shapes never enter the Binding algebra from the native type path either.

  Backlog: if the native engine's gradual semantics need first-class
  representation in shapes (e.g. for hover rendering), a `{:dynamic, inner}`
  shape *with a `do_expand` clause* should be introduced explicitly — it
  must not silently degrade to `nil`.

  ### `:term`-rendering shapes

  Several shapes are display aliases — they carry no extra information beyond
  their kind:

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
  | `:empty_map`     | `"empty_map()"`     | `%{}` from Descr (task #22)           |
  | `:non_struct_map`| `"non_struct_map()"` | open non-struct map from Descr       |
  | `:boolean`       | `"boolean()"`       | `false | true`                        |
  | `:bitstring`     | `"bitstring()"`     | superset of `:binary`                 |

  ### `{:optional, _}` — map-field-only wrapper

  Meaning: a map key that **may or may not be present** when it is present
  its value has the inner type.

  Producers: `ElixirTypes.to_shape_eager/1` when the descr carries an
  `:optional` key alongside inner type bits; `quoted_to_shape` for
  `{:if_set, [], [inner]}` from `Descr.to_quoted`.

  Consumer / algebra:
  - `do_expand` preserves the wrapper: if the inner shape expands to `nil`
    the wrapper is dropped; otherwise `{:optional, expanded}` is returned.
  - `covers?` treats `{:optional, _}` transparently — the inner shapes are
    compared.  This prevents spurious disjointness in `drop_subsumed`.
  - `uninformative_field?` keeps optional fields (renders as
    `"if_set(inner)"`, not `"term()"`).
  - `fields_for_receiver/2` in `TypePresentation` includes optional fields
    (they are accessible — the key *may* exist).

  Note: `{:optional, _}` is only meaningful as a *field value* inside a map
  or struct shape.  Using it as a standalone expression type is undefined.

  ---

  ## Improper-list policy

  The shape algebra DOES now model non-empty improper lists, via the
  `{:nonempty_list, elem, tail}` 3-tuple (see "List shapes" above). The
  compiler's set-theoretic `non_empty_list(head, tail)` with a non-list tail
  maps to that 3-tuple. Three former degradation points are now precise where
  both component types are known; the remaining degradations are noted:

  1. **`to_shape` boundary** (`ElixirTypes.quoted_to_shape/1`): a
     `{:non_empty_list, [], [elem, tail]}` form (explicit improper tail) now
     converts to `{:nonempty_list, elem_shape, tail_shape}` when BOTH sides
     convert; it degrades to `nil` only if either side is unconvertible. A
     PROPER `non_empty_list`'s quoted form has no tail arg → stays the 2-tuple.

  2. **`++` with non-list RHS** (`expand_call` for `++`): a known NON-EMPTY
     proper LHS `++` a known NON-LIST RHS now yields the improper 3-tuple. A
     POSSIBLY-EMPTY LHS (`{:list, _}`/`:list`) with a non-list RHS stays `nil`
     (conservative): `[] ++ x == x` means the result could be the bare RHS,
     which the prefix-bearing 3-tuple can't model. An empty-list LHS yields the
     RHS directly. An unknown RHS stays `nil`.

  3. **Cons patterns** (`parse_type` for `[head | _]`): UNCHANGED — the
     terminator type is still dropped; `[head | _]` parses to `{:list, head}`.
     A type-spec's `maybe_improper_list(h, t)` / `nonempty_improper_list(h, t)`
     remains a sound over-approximation (widened proper list). Cons in PATTERN
     context likewise keeps its under-approximation rules for subtraction
     (`precise_pattern_type` is unchanged); only cons in EXPRESSION context
     produces the 3-tuple (via `TypeInference.type_of`).

  **Remaining backlog**: improper lists from typespecs (point 3) and the
  proper/improper intersection mix (which stays `nil`) are still coarse. The
  3-tuple closes the most common end-to-end path (`[a] ++ b`, `[h | t]`).
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
  # The set-theoretic type backend. Coupled to unstable compiler internals — every
  # use is guarded by `ElixirTypes.enabled?()` + try/rescue so API drift degrades
  # gracefully to the custom algebra (see MEMORY: Module.Types API drift).
  alias Module.Types.Descr

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

  def from_env(%State.Env{} = env, %ElixirSense.Core.Metadata{} = metadata, cursor_position) do
    from_env(env, metadata, cursor_position, [])
  end

  @doc """
  Like `from_env/3` but accepts options.

  ## Options

    * `:local_sigs` — a precomputed local sigs map (the result of
      `ElixirTypes.build_local_sigs_map/2`). When given, the (verified O(N))
      rebuild is skipped and the supplied map is used verbatim. This is the
      per-request memoization hook used by `ElixirSense.Core.TypeHints`; passing
      a stale or wrong-module map only affects native sig lookups, never
      correctness of the structural engine.
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
          # no variable found - treat as a local call. This only applies to the
          # Code.Fragment misclassification case (a no-parens call parsed as a
          # variable), which carries `version == :any`. A *versioned* variable not
          # in scope (e.g. a clause-local pattern var embedded in a
          # `{:case_result}`/`{:difference}` thunk) must NOT resolve to a same-named
          # 0-arity local function — that would inject the function's return type
          # into feasibility/subtraction. Such vars are simply unknown.
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

  # Literal-complete map (`:closed` tail) — all keys are known. Produced by
  # `type_inference` for map literals in EXPRESSION context and by
  # `ElixirTypes.to_shape` for a closed descr. Preserve completeness.
  def do_expand(_env, {:map, fields, :closed}, _stack) do
    {:map, fields, :closed}
  end

  # Partial map (`nil` tail) — at least these keys are present; closedness is
  # unknown (guard facts, the conservative legacy default). Preserve as-is.
  # (Distinct from a map *update* whose base fails to resolve, below.)
  def do_expand(_env, {:map, fields, nil}, _stack) do
    {:map, fields, nil}
  end

  # Already-open map literal/shape — preserve openness.
  def do_expand(_env, {:map, fields, :open}, _stack) do
    {:map, fields, :open}
  end

  # Map update `%{base | fields}` — `updated_map` is the base shape to merge.
  def do_expand(env, {:map, fields, updated_map}, stack) do
    case expand(env, updated_map, stack) do
      # Literal-complete base — a map update keeps the same key set (update only
      # rebinds existing keys), so the result stays `:closed`.
      {:map, expanded_fields, :closed} ->
        {:map, put_fields(expanded_fields, fields), :closed}

      # Partial base (closedness unknown): the result is likewise partial.
      {:map, expanded_fields, nil} ->
        {:map, put_fields(expanded_fields, fields), nil}

      # Known base that was itself open (other unknown keys exist): the result
      # also has unknown keys beyond the ones we can see.
      {:map, expanded_fields, :open} ->
        {:map, put_fields(expanded_fields, fields), :open}

      {:struct, expanded_fields, type, nil} ->
        {:struct, put_fields(expanded_fields, fields), type, nil}

      # Base does not resolve to a concrete map/struct (unknown base, e.g.
      # `def f(m), do: %{m | a: 1}`). Map update preserves the FULL base type
      # with the listed keys refined (see Module.Types.Expr), so the result is
      # an OPEN map: the listed fields are known, but additional unknown keys
      # exist. Marking the tail `:open` keeps this distinct from a closed map
      # literal `{:map, fields, nil}`.
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
        # `Keyword.get/2` tolerates non-atom (domain) keys present in the list.
        # On a `:closed` (literal-complete) map a missing key is PROVABLY absent
        # (`:not_set`); on a `nil`-partial or `:open` map it is merely unknown
        # (`nil`), since other keys may exist.
        case Keyword.fetch(expanded_fields, key) do
          {:ok, value} -> value
          :error when tail == :closed -> :not_set
          :error -> nil
        end

      true ->
        # Non-atom key: project through a matching domain key (`%{"a" => v}["a"]`).
        # Requires the key shape to match exactly — a fuzzier (dynamic) key
        # yields `nil` (unknown).
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

  # Improper (possibly-improper) non-empty list: a proper prefix of `elem`
  # elements terminated by a non-list `tail`. Expand both components.
  def do_expand(env, {:nonempty_list, elem, tail}, stack),
    do: {:nonempty_list, expand(env, elem, stack), expand(env, tail, stack)}

  # Set difference, used for cross-clause occurrence typing (a later `case`
  # clause sees the scrutinee type minus what earlier clauses matched). Resolved
  # lazily so the base — typically `{:variable, ...}` or a remote `{:call, ...}`
  # — is concretized first. Conservative: only narrows when the base resolves to
  # a union (see difference/2).
  def do_expand(env, {:difference, base_candidate, subtracted_candidate}, stack) do
    difference(expand(env, base_candidate, stack), expand(env, subtracted_candidate, stack))
  end

  # `case` result: union the bodies of clauses whose pattern can actually match
  # the scrutinee. A clause whose pattern is disjoint from the (resolved)
  # scrutinee type can't match — the case raises instead of returning that body —
  # so it's dropped. If no clause can match, the result is `:none`.
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
  # `:list` is the generic list shape (from `is_list/1`); keep it as-is so union
  # subsumption (`:list` over `{:list, _}`) and intersection work on it.
  def do_expand(_env, :list, _stack), do: :list
  # Terminals produced by ElixirTypes.to_shape (native descr -> shape).
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

  # Optional map field values (`if_set`, from `ElixirTypes.to_shape`). Preserve
  # the `{:optional, _}` wrapper so consumers (TypePresentation `if_set(...)`,
  # `uninformative_field?`, completion field listing) can distinguish a possibly
  # absent key from a present one. Expand the inner shape and re-wrap; if the
  # inner shape resolves to nothing (`nil`), drop the wrapper too.
  def do_expand(env, {:optional, inner}, stack) do
    case expand(env, inner, stack) do
      nil -> nil
      expanded -> {:optional, expanded}
    end
  end

  def do_expand(_env, _other, _stack), do: nil

  # Conservative set difference over already-expanded shapes. We can only
  # represent "X minus Y" when X is a union: drop the members Y covers. A single
  # (non-union) shape collapses to :none if fully covered, otherwise is returned
  # unchanged (subtracting from an opaque type is not representable, so we keep
  # the base rather than invent a negation).
  defp difference(nil, _subtracted), do: nil
  defp difference(base, nil), do: base
  defp difference(:none, _subtracted), do: :none
  defp difference(base, :none), do: base

  defp difference({:union, members}, subtracted) do
    removed = union_to_list(subtracted)

    # Normalize the remainder so the subtraction result is collapsed/subsumed
    # like any other union (e.g. removing one of `5 | integer()`'s redundant
    # members), rather than left as a raw member list.
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
      # `nil`/`:none` means the scrutinee or pattern is unknown/unresolvable
      # (e.g. an un-inferable local call) — we can't rule the clause out.
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
  # first-occurrence order: same-shape lists become one list whose element type
  # is the union of the originals; maps with the same key set and structs of the
  # same module merge their fields key-by-key (`%{a: 1} | %{a: 2}` -> `%{a:
  # 1 | 2}`). Other members are left untouched.
  defp coalesce_union(members) do
    Enum.reduce(members, [], fn member, acc ->
      case Enum.find_index(acc, &mergeable?(&1, member)) do
        nil -> acc ++ [member]
        index -> List.update_at(acc, index, &merge_members(&1, member))
      end
    end)
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
  # An improper non-empty list is still "list-like" for the purposes of the
  # `++`/`--` argument guard: treating it as non-list would falsely claim
  # `:none` (dead code). Note the `++` improper-producing clause matches only a
  # PROPER non-empty LHS (2-tuple), so an improper 3-tuple LHS still falls
  # through to the conservative `nil`.
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
      {key, normalize_union([value_1, Keyword.get(fields_2, key)])}
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
  # DESCR-BACKED FAST PATH (GPT round-5 P1, item 1). When the native typesystem
  # is enabled AND both operands are `descr_exact?` shape kinds (their static
  # coercion to a `Descr.t()` loses no information), subsumption delegates to the
  # real `Descr.subtype?/2`, which is strictly more faithful than the
  # hand-written structural clauses below (it sees through unions, the number
  # tower, etc.). `covers?(subsumer, member)` is exactly
  # `Descr.subtype?(coerce(member), coerce(subsumer))`.
  #
  # WHY DYNAMIC-WRAPPING IS SAFE HERE: `coerce_var_type_public/1` produces
  # `dynamic(static(shape))`. Both coerced operands are therefore gradual, so
  # `Descr.subtype?/2` takes its `true ->` branch and compares the FULL descrs
  # (`subtype_static?(left, right)`) — the dynamic wrappers cancel. Empirically
  # verified that `subtype?(dynamic(a), dynamic(b)) == subtype?(a, b)` across
  # int<:num, num<:int, atom(:ok)<:atom(), union membership, and literal pairs
  # (see test "descr-backed covers? matches the static subtype relation"). This
  # is the SAME dynamic-cancellation property the descr-backed intersection
  # relies on. Any backend drift falls through to the custom clauses via
  # try/rescue. Non-exact shapes (`nil` wildcard, maps/structs, literal scalars,
  # generic `:list`/`:tuple`, `{:optional, _}`, improper 3-tuples) are NOT exact,
  # so they always take the custom path below — preserving the existing
  # wildcard/transparent-optional/map-top semantics those clauses encode.
  defp covers?(a, b) do
    case descr_backed_covers?(a, b) do
      :__fallthrough__ -> covers_custom?(a, b)
      bool -> bool
    end
  end

  defp descr_backed_covers?(subsumer, member) do
    if ElixirTypes.enabled?() and descr_exact?(subsumer) and descr_exact?(member) do
      try do
        descr_member = ElixirTypes.coerce_var_type_public(member)
        descr_subsumer = ElixirTypes.coerce_var_type_public(subsumer)
        Descr.subtype?(descr_member, descr_subsumer)
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
  # `nil` as a *subtracted* element means "any value here" (e.g. the `_` in
  # `{:ok, _}`), so it covers any member element. Reached only inside tuple
  # recursion — a top-level nil subtrahend is short-circuited by difference/2.
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

  # Improper non-empty list (3-tuple): conservative. An improper list is NOT
  # subsumed by any proper list shape, and a proper-list subtrahend does not
  # cover an improper member; so the 3-tuple only covers an equal-kind 3-tuple
  # whose prefix element and tail both cover (elementwise). It covers no proper
  # list and is covered by no proper list.
  defp covers_custom?({:nonempty_list, sub_elem, sub_tail}, {:nonempty_list, mem_elem, mem_tail}),
    do: list_elem_covers?(sub_elem, mem_elem) and covers?(sub_tail, mem_tail)

  # Map top (`%{}` / `map()`) subsumes any concrete map or struct. An open empty
  # map (`%{...}`) is likewise the map top.
  defp covers_custom?({:map, [], tail}, {:map, _, _}) when tail in [nil, :open], do: true
  defp covers_custom?({:map, [], tail}, {:struct, _, _, _}) when tail in [nil, :open], do: true

  defp covers_custom?({:tuple, n, sub_elems}, {:tuple, n, mem_elems}) do
    sub_elems
    |> Enum.zip(mem_elems)
    |> Enum.all?(fn {sub, mem} -> covers?(sub, mem) end)
  end

  defp covers_custom?({:struct, _, {:atom, mod}, _}, {:struct, _, {:atom, mod}, _}), do: true
  defp covers_custom?({:struct, _, nil, _}, {:struct, _, _, _}), do: true

  # Optional map-field wrappers (`if_set`). Treat the wrapper transparently:
  # coverage is decided by the inner shapes. This keeps `{:optional, x}` from
  # being spuriously disjoint from `x` (or from another optional of a covered
  # type). A possibly-absent key plus its inner value is conservatively handled
  # by comparing the values it can hold when present.
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
        # `a ++ b`. The result is a proper `{:list, elem}` when the RHS is a
        # known proper list (then the elements are the union of both sides). When
        # the LHS is a known NON-EMPTY proper list and the RHS is a known NON-LIST
        # shape, `a ++ b` is a non-empty IMPROPER list with proper prefix `a` and
        # final tail `b` — the `{:nonempty_list, elem, tail}` 3-tuple. When the LHS
        # is possibly-empty (`{:list, _}`/`:list`) the result might be exactly the
        # RHS (`[] ++ x == x`), which we can't model, so we stay conservative
        # (`nil`). When the LHS is `[]` the result is exactly `x`.
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
          # list. The proper prefix carries the LHS element type; the final tail is
          # the RHS shape. (Improper 3-tuple LHS or possibly-empty LHS fall
          # through to the conservative `nil`: a possibly-empty LHS could yield the
          # bare RHS, and chaining onto an already-improper list is ill-typed.)
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

  # Raising functions never return a value, so they are `:none` (bottom). This
  # also lets them drop out of branch-result unions: `if c, do: 1, else: raise "x"`
  # is `1`, and `a and b` (which expands to a `case` with an `:erlang.error`
  # branch) loses that branch.
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
                # NOTE `:andalso`/`:orelse` are intentionally NOT here: they are
                # not boolean-typed. `true and 5` is `5` — only the left operand
                # must be boolean, the result is the right operand (or `false`).
                # They fall through to the generic call path (nil/unknown) rather
                # than over-claiming `:boolean`.
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

  defp resolve_type_module(%Binding{} = env, {:__MODULE__, _, _} = ast) do
    # Pure AST->module resolution delegated to ModuleResolver. Preserve the
    # prior behavior of yielding `nil` (not an error) when there is no current
    # module by mapping `:error` -> `nil`.
    case ModuleResolver.resolve(ast, env) do
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

  defp resolve_type_module(%Binding{} = env, {:__aliases__, _, _list} = ast) do
    # Alias resolution (incl. the no-alias fallback) is delegated to the single
    # canonical path in `ModuleResolver`. Binding previously carried a divergent
    # `resolve_same_root_alias`/`resolve_parent_alias` heuristic that disagreed
    # with the real compiler (e.g. it turned an unaliased single `B` inside
    # `Sib.A` into `Sib.B`, whereas the compiler keeps it `Elixir.B`); that
    # heuristic has been removed. `ModuleResolver.resolve` already falls back to
    # `Module.concat(parts)` when no alias applies, so `nil`/`:error` here only
    # arises for genuinely unresolvable forms.
    case ModuleResolver.resolve(ast, env) do
      {:ok, module} -> module
      :error -> nil
    end
  end

  defp resolve_type_module(%Binding{} = env, {{:., _, [base, nested]}, _, []})
       when is_atom(nested) do
    case resolve_type_module(env, base) do
      module when is_atom(module) -> Module.concat(module, nested)
      _ -> nil
    end
  end

  defp resolve_type_module(_env, _), do: nil

  defp combine_intersection(:none, _), do: :none
  defp combine_intersection(_, :none), do: :none
  defp combine_intersection(nil, type), do: type
  defp combine_intersection(type, nil), do: type
  defp combine_intersection(type, type), do: type

  # Descr-backed intersection (consolidated backlog P2 2.2 / GPT P1).
  #
  # When the native typesystem is enabled AND both operands are "exact" shape
  # kinds (`descr_exact?/1` — kinds whose static coercion to a `Descr.t()` loses
  # no information), the real `Descr.intersection/2` is strictly more faithful
  # than the hand-written structural approximation below. We coerce both via the
  # public `ElixirTypes.coerce_var_type_public/1` path (which dynamic-wraps every
  # shape) and intersect.
  #
  # WHY DYNAMIC-WRAPPING IS SAFE HERE: the public coercion produces
  # `dynamic(static(shape))`. For intersection the identity
  # `dynamic(a) ∩ dynamic(b) = dynamic(a ∩ b)` holds, and `ElixirTypes.to_shape/1`
  # unwraps the outer `dynamic`, so the resulting shape is exactly what a static
  # intersection would yield. (Verified empirically; this is NOT true for
  # difference — see `difference/2` — which is why difference keeps the custom
  # path.) Empty intersection → `:none`. If `to_shape/1` cannot represent the
  # result (`nil`), we fall through to the custom clauses rather than lose the
  # precision they may have. The whole attempt is wrapped in try/rescue so any
  # API drift in the native backend silently falls back to the custom algebra.
  defp combine_intersection(a, b) do
    case descr_backed_intersection(a, b) do
      :__fallthrough__ -> combine_intersection_custom(a, b)
      result -> result
    end
  end

  defp descr_backed_intersection(a, b) do
    if ElixirTypes.enabled?() and descr_exact?(a) and descr_exact?(b) do
      try do
        descr_a = ElixirTypes.coerce_var_type_public(a)
        descr_b = ElixirTypes.coerce_var_type_public(b)
        result = Descr.intersection(descr_a, descr_b)

        if Descr.empty?(result) do
          :none
        else
          case ElixirTypes.to_shape(result) do
            # Unconvertible result — don't lose the custom path's precision.
            nil -> :__fallthrough__
            shape -> shape
          end
        end
      rescue
        _ -> :__fallthrough__
      catch
        _, _ -> :__fallthrough__
      end
    else
      :__fallthrough__
    end
  end

  # Conservative syntactic whitelist of shape kinds whose *static* coercion to a
  # `Descr.t()` is exact (round-trips without widening), so the descr-backed
  # intersection above is faithful. Deliberately EXCLUDED:
  #   * maps / structs — coercion makes maps OPEN, which is not exact for the
  #     algebra used here;
  #   * literal scalars (`{:integer, _}`, `{:float, _}`, `{:binary, _}`) — they
  #     widen to their base type on coercion;
  #   * `:bitstring`, `:non_struct_map`, generic `:list`, generic `:tuple`,
  #     `:fun`/`{:fun, _}`, `:dynamic`, `:term`, `:number`-of-non-exact, etc.
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

  # NOTE intersection is not strict and does an union on map keys

  defp combine_intersection_custom({:struct, fields_1, nil, nil}, {:struct, fields_2, nil, nil}) do
    keys = (safe_keys(fields_1) ++ safe_keys(fields_2)) |> Enum.uniq()
    fields = for k <- keys, do: {k, combine_intersection(fields_1[k], fields_2[k])}

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
    fields = for k <- keys, do: {k, combine_intersection(fields_1[k], fields_2[k])}

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
       when tail_1 in [nil, :closed] and tail_2 in [nil, :closed] do
    keys = (safe_keys(fields_1) ++ safe_keys(fields_2)) |> Enum.uniq()
    fields = for k <- keys, do: {k, combine_intersection(fields_1[k], fields_2[k])}

    if Enum.any?(fields, fn {_k, v} -> v == :none end) do
      :none
    else
      {:map, fields, intersect_tails(tail_1, tail_2)}
    end
  end

  defp combine_intersection_custom({:struct, fields_1, type, nil}, {:map, fields_2, tail_2})
       when tail_2 in [nil, :closed] do
    keys =
      if type != nil,
        do: safe_keys(fields_1),
        else: (safe_keys(fields_1) ++ safe_keys(fields_2)) |> Enum.uniq()

    fields = for k <- keys, do: {k, combine_intersection(fields_1[k], fields_2[k])}

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
       when tail in [nil, :closed] do
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

  # Union on the left is handled here; this clause must come *before* the flip
  # clause below, otherwise `union ∩ union` flips left/right forever.
  defp combine_intersection_custom({:union, variants}, other) do
    # Collect *every* non-empty overlap, not just the first — `(:a | :b | :c) ∩
    # (:b | :c)` is `:b | :c`, not `:b`. Empty overlap is `:none` (bottom), never
    # nil.
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

  # List ∩ list: intersect element types. Both `{:list, _}` shapes include `[]`,
  # so their intersection always includes `[]` and is therefore never `:none`
  # even when the element intersection bottoms out (e.g. `list(:a) ∩ list(:b)`
  # is `[]`, represented as `{:list, :empty}`). A nonempty side forces the result
  # nonempty.
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

  # Two improper non-empty lists (3-tuples): intersect elementwise. The prefix
  # element uses the list-element intersection (a disjoint prefix still leaves a
  # one-element prefix possible, so it collapses to the conservative element
  # rather than `:none`); the tail uses the general intersection. A `:none` tail
  # makes the whole shape `:none`. Any mixed proper/improper or `:list`-vs-3-tuple
  # pair falls through to the conservative fallback (`nil`), since the kinds are
  # not provably disjoint (`shape_kind` keeps both `:list`).
  defp combine_intersection_custom({:nonempty_list, e1, t1}, {:nonempty_list, e2, t2}) do
    case combine_intersection(t1, t2) do
      :none -> :none
      tail -> {:nonempty_list, intersect_list_elem(e1, e2), tail}
    end
  end

  # Scalar specificity: if one side subsumes the other the intersection is the
  # narrower of the two (e.g. `integer() and 5` is `5`, `atom() and :ok` is
  # `:ok`). Otherwise the fallback is *conservative*: `covers?/2` is subsumption,
  # not overlap, so a non-subsuming pair is only `:none` when the two top-level
  # shape kinds are provably disjoint (atom vs tuple vs map vs list vs number vs
  # binary vs ...). Same-kind or unknown/new-kind pairs intersect to `nil`
  # (unknown) rather than `:none` to avoid over-claiming disjointness.
  defp combine_intersection_custom(a, b) do
    cond do
      covers?(a, b) -> b
      covers?(b, a) -> a
      disjoint_kinds?(a, b) -> :none
      disjoint_literals?(a, b) -> :none
      # NOTE (GPT round-5 P1, item 2 — REFUTED): a `Descr.disjoint?/2`-backed
      # clause was prototyped here to turn the conservative `nil` into a sound
      # `:none` for exact same-kind pairs the coarse `shape_kind` check misses
      # (e.g. `integer()` vs `float()`). It is faithful (`disjoint?(dynamic(a),
      # dynamic(b)) == disjoint?(a, b)` — verified) BUT effectively UNREACHABLE
      # through the expand pipeline: every exact same-kind pair that gets here is
      # already resolved earlier — by `disjoint_literals?` (two pinned scalars),
      # by the dedicated tuple/list/union/struct clauses, or by `covers?` — and
      # the remaining abstract atoms/numbers are pre-expanded to NON-exact literal
      # forms (`:integer` -> `{:integer, nil}`) by `do_expand`, so they fail the
      # `descr_exact?` gate. Adding the clause would be dead code, so it is left
      # out (audit: "add only with evidence").
      true -> nil
    end
  end

  # Two distinct concrete literals of the same scalar kind are disjoint
  # (`:a ∩ :b`, `1 ∩ 2`, `1.0 ∩ 2.0`, `"a" ∩ "b"` are all `:none`). Same-kind but
  # at least one *abstract* (e.g. `atom() ∩ :a`) is handled by `covers?/2` above,
  # so this only fires for two pinned values that aren't equal (equality is caught
  # by the `type, type` clause earlier).
  defp disjoint_literals?({tag, v1}, {tag, v2})
       when tag in [:atom, :integer, :float, :binary] and not is_nil(v1) and not is_nil(v2),
       do: v1 != v2

  defp disjoint_literals?(_a, _b), do: false

  # Element intersection for two list shapes. Unknown (`nil`) element on either
  # side means "any", so it yields the other side. `:empty` (element of `[]`)
  # intersected with anything is `:empty` only for two empty lists; an empty list
  # shape is `{:list, :empty}` and never appears as a `:nonempty_list` element.
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
  # The improper 3-tuple is kept `:list`-kind (NOT a distinct kind) so the
  # disjointness fallback never claims `:none` against a proper list shape —
  # conservative. In descr terms an improper non_empty_list(a, tail) with a
  # non-empty-excluding tail IS disjoint from proper list(t), but asserting that
  # here risks an unsound `:none`, so we stay coarse.
  defp shape_kind({:nonempty_list, _, _}), do: :list
  defp shape_kind(:tuple), do: :tuple
  defp shape_kind({:tuple, _, _}), do: :tuple
  defp shape_kind({:map, _, _}), do: :map
  defp shape_kind({:struct, _, _, _}), do: :map
  defp shape_kind(:pid), do: :pid
  defp shape_kind(:port), do: :port
  defp shape_kind(:reference), do: :reference
  # number vs integer/float kinds overlap, so we do NOT treat integer vs float as
  # disjoint here (both map to :number); that is intentional — an over-claim of
  # disjointness is worse than a missed one. Everything else (fun, unknown, new
  # shapes) is left unclassified.
  defp shape_kind(_other), do: nil

  defp expand_map_fields(env, map_or_struct, stack) do
    {fields, _tail} = expand_map_base(env, map_or_struct, stack)
    fields
  end

  # Expand a map/struct base used as an operand of `Map.put`/`merge`/etc.
  # Returns `{fields, tail}` where `tail` is:
  #   * `:closed` — the base is a literal-complete map or a resolved struct (ALL
  #                 keys known); a put/delete/merge keeps the result closed;
  #   * `nil`     — the base is a PARTIAL map (closedness unknown, e.g. a guard
  #                 fact); the result stays partial;
  #   * `:open`   — the base is an open map (additional unknown keys exist), e.g.
  #                 an untyped parameter (`expand/3` → nil) or another open map;
  #   * `[:none]` (as fields) — the base is provably not a map (bottom).
  # Synthesising results from an OPEN/partial base must NOT assert the absence of
  # keys we never saw (P1 unsoundness); only a `:closed` base licenses that.
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
