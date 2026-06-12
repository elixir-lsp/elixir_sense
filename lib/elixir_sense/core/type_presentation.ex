defmodule ElixirSense.Core.TypePresentation do
  @moduledoc """
  The LSP-facing type surface: resolve a stored ElixirSense "shape" through
  `ElixirSense.Core.Binding` and render it as human-readable Elixir-ish text for
  inlay hints, hover, and property-aware completion.

  Stored shapes (in `VarInfo.type`) may contain unresolved thunks —
  `{:variable, ...}`, `{:call, ...}`, `{:map_key, ...}`, `{:tuple_nth, ...}`,
  `{:difference, ...}`, etc. Callers must never display those directly. This
  module guarantees a resolved, thunk-free result.

  ## Dialect notes

  Shared constructs (unions, lists, booleans, etc.) use the **compiler dialect**,
  matching `Module.Types.Descr.to_quoted_string/2` output exactly:

  - Unions join with `" or "` (not `" | "`).
  - Lists render as `list(t)`, `non_empty_list(t)`, `empty_list()`, and
    improper non-empty lists as `non_empty_list(t, tail)`.
  - `boolean()`, `tuple()`, `map()`, `bitstring()`, etc. use the compiler names.

  The custom structural engine adds **extra-precision spellings** that the
  compiler's descr does not have — literal numbers (`5`), literal strings
  (`"x"`), `number()`, `struct()`, `{:fun, arity}` arrows with `term()` args.
  These appear only in the structural (non-native) path. Every construct shared
  with the compiler uses the compiler's spelling.

  ## External API (stable surface for the LSP layer)

    * `resolve_shape/2` — `{:ok, resolved_shape} | :unknown` (shape, not text).
    * `render_hint/2` — `{:ok, text} | :skip` for a `VarInfo` (inlay hints;
      skips uninformative `term()`/`none()`/unknown).
    * `render_hint/3` — `{:ok, %{label: String.t(), full: String.t()}} | :skip`
      with optional `max_length:` elision.
    * `fields_for_receiver/2` — `%{field => rendered_type}` for a map/struct
      receiver (property-aware completion/definition).

  Lower-level helpers: `resolve_and_render/2`, `render_var/2`, `render/1`.

  Rendering is best-effort and total: anything it can't make precise renders as
  `term()` inside a structure, or yields `:unknown` at the top level.
  """

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.ElixirTypes
  alias ElixirSense.Core.State.VarInfo

  @doc """
  Resolve `shape` through `binding`, concretizing all thunks.

  Returns `{:ok, resolved_shape}` (never a raw thunk; may be `:none`) or
  `:unknown` when there is no information (`nil`). This is the shape-level
  entry point for consumers that want to inspect the resolved structure rather
  than display text (e.g. `fields_for_receiver/2`).
  """
  def resolve_shape(%Binding{} = binding, shape) do
    case Binding.expand(binding, shape) do
      nil -> :unknown
      resolved -> {:ok, resolved}
    end
  end

  @doc """
  Resolve `shape` through `binding` (concretizing all thunks) and render it.

  Returns `{:ok, text}` or `:unknown`. Never returns raw internal thunks.
  """
  def resolve_and_render(%Binding{} = binding, shape) do
    binding |> Binding.expand(shape) |> render()
  end

  @doc """
  Render a variable's type for display.

  Prefers the **structural** type (`VarInfo.type`), which carries the L1 /
  cross-clause refinements, falling back to the native `Module.Types`
  descriptor only when the structural type renders to nothing useful
  (`:unknown`, `term()` or `none()`). This keeps a branch-narrowed shape from
  being masked by a broader/stale descriptor.
  """
  def render_var(%Binding{} = binding, %VarInfo{} = var_info) do
    case render_var_sourced(binding, var_info) do
      {:ok, text, _source} -> {:ok, text}
      :unknown -> :unknown
    end
  end

  # Like `render_var/2` but also reports *which* path produced the displayed
  # text and (when available) the resolved shape behind it.
  #
  #   * `:shape`  — the structural engine produced the string (it was
  #     informative, so it wins over the native descriptor). The resolved shape
  #     is returned so the hint path can post-process it (literal widening).
  #   * `:native` — the structural type was uninformative and the native
  #     `Module.Types` descriptor produced the string. The shape is `nil` when
  #     the exact compiler string (`descr_to_string`) was used (no shape), or the
  #     `to_shape`-derived shape when that fallback fired.
  #
  # `:spec`-derived flows are not cheaply distinguishable from inferred shapes
  # here (a spec signature is lowered to the same `{:fun_clauses, ...}` /
  # structural shapes as inference), so they currently surface as `:shape` or
  # `:native`. `:spec` is reserved and not emitted.
  #
  # Returns `{:ok, text, source}` or `:unknown`. (The shape is threaded
  # separately via `render_var_for_hint/2`.)
  defp render_var_sourced(%Binding{} = binding, %VarInfo{
         elixir_types_descr: descr,
         type: type
       }) do
    resolved = Binding.expand(binding, type)
    structural = render(resolved)

    if informative?(structural) do
      {:ok, text} = structural
      {:ok, text, {:shape, resolved}}
    else
      case render_descr_sourced(descr) do
        {:ok, text, shape} ->
          {:ok, text, {:native, shape}}

        :unknown ->
          case structural do
            {:ok, text} -> {:ok, text, {:shape, resolved}}
            :unknown -> :unknown
          end
      end
    end
  end

  @doc """
  Render a variable as an inlay-hint string, or `:skip` for uninformative
  types. Skips `:unknown`, `term()` and `none()` so a provider can drop the
  hint rather than show noise.
  """
  def render_hint(%Binding{} = binding, %VarInfo{} = var_info) do
    case render_var(binding, var_info) do
      {:ok, text} -> if hint_noise?(text), do: :skip, else: {:ok, text}
      :unknown -> :skip
    end
  end

  @doc """
  Render a variable as an inlay-hint result with source attribution, literal
  widening, and optional length elision.

  ## Options

    * `max_length:` (`pos_integer | nil`, default `nil`) — when set, `label` is
      truncated to at most that many graphemes with a trailing `…`; `full`
      always contains the complete text. Elision prefers cutting at an ` or `
      boundary within unions. When `nil`, `label == full` (subject to
      `max_full_length:`).

    * `max_full_length:` (`pos_integer`, default `1000`) — hard cap on the
      `full` text (huge unions can otherwise blow up a tooltip). Truncated
      grapheme-safe with a trailing `…`. `label` is derived from the capped
      `full`.

    * `widen_literals:` (`boolean`, default `true`) — widen literal leaf types
      the compiler would never print as a type (`{:integer, n} → integer()`,
      `{:float, x} → float()`, `{:binary, "s"} → binary()`), recursively inside
      containers/unions. Atom literals are kept (the compiler prints them). This
      applies to **inlay hints only**; hover (`render_var/2`) and completion
      (`render/1`) are unaffected.

  ## Result

  `{:ok, %{label: String.t(), full: String.t(), source: source}}` or `:skip`,
  where `source` is:

    * `:shape`  — text produced by the structural engine.
    * `:native` — text produced by the native `Module.Types` descriptor path.

  Spec-derived signatures are lowered to the same structural shapes as inferred
  types, so they are not separately attributable here; they surface as `:shape`
  or `:native`. The `:spec` value is reserved for a future cheaply-detectable
  signal and is not currently emitted.
  """
  def render_hint(%Binding{} = binding, %VarInfo{} = var_info, opts) when is_list(opts) do
    case render_var_sourced(binding, var_info) do
      :unknown ->
        :skip

      {:ok, text, {source, shape}} ->
        widen? = Keyword.get(opts, :widen_literals, true)

        full_raw =
          if widen? and not is_nil(shape) do
            # Re-render from the widened shape so widening reaches every leaf,
            # including inside containers/unions. Falls back to the original
            # text if rendering the widened shape somehow fails.
            case render(widen_literals(shape)) do
              {:ok, widened} -> widened
              :unknown -> text
            end
          else
            text
          end

        if hint_noise?(full_raw) do
          :skip
        else
          max_full_length = Keyword.get(opts, :max_full_length, 1000)
          full = elide(full_raw, max_full_length)
          max_length = Keyword.get(opts, :max_length, nil)
          label = elide(full, max_length)
          {:ok, %{label: label, full: full, source: source}}
        end
    end
  end

  # ── Literal widening for inlay hints (task: hints only) ─────────────────────
  #
  # The compiler never prints integer/float/binary literals *as types*, so a
  # hint must not show `5`/`1.0`/`"x"`; widen them to `integer()`/`float()`/
  # `binary()`. Atom literals stay (`:ok`, `nil`, module atoms are printed by
  # the compiler). Recurse through every container so `%{a: 1}` widens to
  # `%{a: integer()}`.
  defp widen_literals({:integer, _}), do: {:integer, nil}
  defp widen_literals({:float, _}), do: {:float, nil}
  defp widen_literals({:binary, _}), do: {:binary, nil}

  # Atom literals are preserved verbatim.
  defp widen_literals({:atom, _} = atom), do: atom

  defp widen_literals({:list, :empty}), do: {:list, :empty}
  defp widen_literals({:list, elem}), do: {:list, widen_literals(elem)}
  defp widen_literals({:nonempty_list, elem}), do: {:nonempty_list, widen_literals(elem)}

  defp widen_literals({:nonempty_list, elem, tail}),
    do: {:nonempty_list, widen_literals(elem), widen_literals(tail)}

  defp widen_literals({:tuple, size, elems}) when is_list(elems),
    do: {:tuple, size, Enum.map(elems, &widen_literals/1)}

  defp widen_literals({:tuple_open, elems}) when is_list(elems),
    do: {:tuple_open, Enum.map(elems, &widen_literals/1)}

  defp widen_literals({:optional, inner}), do: {:optional, widen_literals(inner)}

  defp widen_literals({:map, fields, updated}) when is_list(fields),
    do: {:map, widen_fields(fields), updated}

  defp widen_literals({:struct, fields, type, updated}) when is_list(fields),
    do: {:struct, widen_fields(fields), type, updated}

  defp widen_literals({:union, members}) when is_list(members),
    do: {:union, Enum.map(members, &widen_literals/1)}

  defp widen_literals({:intersection, members}) when is_list(members),
    do: {:intersection, Enum.map(members, &widen_literals/1)}

  defp widen_literals({:dynamic, nil}), do: {:dynamic, nil}
  defp widen_literals({:dynamic, inner}), do: {:dynamic, widen_literals(inner)}

  defp widen_literals({:fun, args, return}) when is_list(args),
    do: {:fun, Enum.map(args, &widen_literals/1), widen_literals(return)}

  defp widen_literals({:fun_clauses, clauses}) when is_list(clauses),
    do:
      {:fun_clauses,
       Enum.map(clauses, fn {args, return} ->
         {Enum.map(args, &widen_literals/1), widen_literals(return)}
       end)}

  # Everything else (scalars, atoms, `:none`, `{:fun, arity}`, unresolved
  # thunks) is already non-literal or not widenable — pass through unchanged.
  defp widen_literals(other), do: other

  defp widen_fields(fields) do
    Enum.map(fields, fn
      {{:domain, key_shape}, value} ->
        {{:domain, widen_literals(key_shape)}, widen_literals(value)}

      {key, value} ->
        {key, widen_literals(value)}
    end)
  end

  @doc """
  The resolved field types of a map/struct receiver, for property-aware
  completion/definition. Returns `%{field_name => rendered_type_text}` (the
  `__struct__` field is dropped), or `%{}` when the receiver isn't a map/struct.

  When a `VarInfo` is provided, the function also consults the native
  `elixir_types_descr` (when `ElixirTypes.enabled?/0` is true) to surface
  fields that the structural engine does not know about — optional keys
  (`if_set(...)`), required keys that only appear in the descr, etc.
  Structural fields always win on conflict (they carry literal precision);
  descr-derived fields fill in anything absent from the structural shape.
  """
  def fields_for_receiver(%Binding{} = binding, %VarInfo{
        type: type,
        elixir_types_descr: descr
      }) do
    structural = fields_from_shape(binding, type)
    descr_fields = fields_from_descr(descr)

    # Structural wins on conflict when it carries real information (not just the
    # uninformative "term()" placeholder that Binding.expand fills in for struct
    # fields it has no data about). Descr-derived values fill in any key that is
    # absent from the structural map OR where structural only knows "term()".
    Map.merge(structural, descr_fields, fn _key, s_val, d_val ->
      if s_val == "term()", do: d_val, else: s_val
    end)
  end

  def fields_for_receiver(%Binding{} = binding, shape) do
    fields_from_shape(binding, shape)
  end

  # Derives the field map from a structural shape (the primary path).
  defp fields_from_shape(%Binding{} = binding, shape) do
    case Binding.expand(binding, shape) do
      {:map, fields, _updated} -> field_map(fields)
      {:struct, fields, _type, _updated} -> field_map(Keyword.delete(fields, :__struct__))
      _other -> %{}
    end
  end

  # Derives the field map from a native elixir_types_descr.
  # Returns %{} when native is disabled, descr is nil, or the descr does not
  # reduce to a map/struct shape.
  defp fields_from_descr(nil), do: %{}

  defp fields_from_descr(descr) do
    if ElixirTypes.enabled?() do
      case ElixirTypes.to_shape(descr) do
        {:map, fields, _updated} -> field_map(fields)
        {:struct, fields, _type, _updated} -> field_map(Keyword.delete(fields, :__struct__))
        _other -> %{}
      end
    else
      %{}
    end
  rescue
    _ -> %{}
  catch
    _, _ -> %{}
  end

  defp field_map(fields) do
    # `:not_set` keys are known-absent (from `not is_map_key/2`) — not real fields.
    for {key, value} <- fields, is_atom(key), value != :not_set, into: %{} do
      {key, render_field(value)}
    end
  end

  defp render_field(value) do
    case render(value) do
      {:ok, text} -> text
      :unknown -> "term()"
    end
  end

  # `term()` is "unknown/top" — prefer the native descriptor if it's more
  # specific. Bare `dynamic()` is the gradual top (no information either).
  # `none()` is *definitive* (the value never exists, e.g. a `case`
  # whose every clause is dead), so it is informative and must NOT be overridden
  # by a stale/optimistic descriptor.
  defp informative?({:ok, "term()"}), do: false
  defp informative?({:ok, "dynamic()"}), do: false
  defp informative?({:ok, _text}), do: true
  defp informative?(_other), do: false

  defp hint_noise?("term()"), do: true
  defp hint_noise?("none()"), do: true
  defp hint_noise?("dynamic()"), do: true
  defp hint_noise?(_other), do: false

  @doc """
  Render an already-resolved, thunk-free shape. Returns `{:ok, text}` or
  `:unknown` (the latter for `nil`, i.e. "no information").
  """
  def render(nil), do: :unknown
  def render(shape), do: {:ok, segment(shape)}

  # Prefer the native descriptor: first try ElixirTypes.descr_to_string/1 which
  # returns the exact compiler text (task #17), then fall back to to_shape +
  # render. Both paths are guarded by enabled?() so the config flag is respected
  # (task #37).
  # Native rendering with the originating shape threaded back.
  #
  #   * exact compiler string path (`descr_to_string`) → `{:ok, text, nil}`
  #     (there is no intermediate shape; the string is the compiler's own).
  #   * `to_shape` fallback → `{:ok, text, shape}` (shape available for the hint
  #     path's literal widening).
  defp render_descr_sourced(nil), do: :unknown

  defp render_descr_sourced(descr) do
    if ElixirTypes.enabled?() do
      # Try the exact compiler string first; fall through on :error.
      case safe_descr_to_string(descr) do
        {:ok, text} ->
          {:ok, text, nil}

        :error ->
          # Fall back to shape-based rendering.
          case ElixirTypes.to_shape(descr) do
            nil ->
              :unknown

            shape ->
              # `render/1` only yields `:unknown` for a `nil` shape; here the
              # shape is non-nil so we always get `{:ok, _}`.
              {:ok, text} = render(shape)
              {:ok, text, shape}
          end
      end
    else
      :unknown
    end
  rescue
    _ -> :unknown
  catch
    _, _ -> :unknown
  end

  # Calls ElixirTypes.descr_to_string/1 when the function exists (the other
  # agent may not have landed it yet). Returns {:ok, str} or :error.
  defp safe_descr_to_string(descr) do
    if function_exported?(ElixirTypes, :descr_to_string, 1) do
      case ElixirTypes.descr_to_string(descr) do
        {:ok, str} -> {:ok, str}
        :error -> :error
      end
    else
      :error
    end
  rescue
    _ -> :error
  catch
    _, _ -> :error
  end

  # segment/1 always returns a string. `nil` (unknown) becomes "term()" inside a
  # structure so a single unknown leaf doesn't sink the whole rendering.
  defp segment(nil), do: "term()"
  defp segment(:none), do: "none()"
  defp segment(:not_set), do: "not_set()"
  # :empty and {:list, :empty} are the empty list — compiler calls it empty_list()
  defp segment(:empty), do: "empty_list()"
  defp segment(:empty_list), do: "empty_list()"
  defp segment(:empty_map), do: "empty_map()"
  defp segment(:non_struct_map), do: "non_struct_map()"
  defp segment(:atom), do: "atom()"
  defp segment(:integer), do: "integer()"
  defp segment(:float), do: "float()"
  defp segment(:number), do: "number()"
  defp segment(:binary), do: "binary()"
  defp segment(:bitstring), do: "bitstring()"
  defp segment(:boolean), do: "boolean()"
  defp segment(:pid), do: "pid()"
  defp segment(:port), do: "port()"
  defp segment(:reference), do: "reference()"
  defp segment(:fun), do: "fun()"
  defp segment(:tuple), do: "tuple()"

  # dynamic() / dynamic(inner) (task #20 / policy C)
  defp segment({:dynamic, nil}), do: "dynamic()"
  defp segment({:dynamic, inner}), do: "dynamic(" <> segment(inner) <> ")"

  defp segment({:atom, nil}), do: "nil"
  defp segment({:atom, value}) when is_atom(value), do: inspect(value)

  defp segment({:integer, nil}), do: "integer()"
  defp segment({:integer, value}) when is_integer(value), do: Integer.to_string(value)

  defp segment({:float, value}) when is_float(value), do: Float.to_string(value)
  defp segment({:float, _}), do: "float()"

  defp segment({:binary, value}) when is_binary(value), do: inspect(value)
  defp segment({:binary, _}), do: "binary()"

  # List spellings — compiler dialect (tasks #22/#23):
  #   empty list        → empty_list()
  #   list(t)           → list(t)
  #   non_empty_list(t) → non_empty_list(t)
  defp segment({:list, :empty}), do: "empty_list()"
  defp segment({:list, elem}), do: "list(" <> segment(elem) <> ")"
  defp segment({:nonempty_list, elem}), do: "non_empty_list(" <> segment(elem) <> ")"

  # Improper non-empty list: compiler spelling `non_empty_list(elem, tail)`.
  defp segment({:nonempty_list, elem, tail}),
    do: "non_empty_list(" <> segment(elem) <> ", " <> segment(tail) <> ")"

  # Fixed-arity tuple: {s1, s2, ...}
  defp segment({:tuple, _size, elems}) when is_list(elems),
    do: "{" <> Enum.map_join(elems, ", ", &segment/1) <> "}"

  # Open tuple: {s1, s2, ..., ...} — the trailing `...` is the open marker (task #7 / policy C)
  defp segment({:tuple_open, elems}) when is_list(elems) do
    case elems do
      [] -> "{...}"
      _ -> "{" <> Enum.map_join(elems, ", ", &segment/1) <> ", ...}"
    end
  end

  # Optional map field (task #8) — must NOT be dropped by uninformative_field?
  defp segment({:optional, inner}), do: "if_set(" <> segment(inner) <> ")"

  # Open map (`:open` tail): additional unknown keys exist beyond `fields`.
  # Render with the compiler's open-map marker `...` first (matching descr.ex).
  defp segment({:map, [], :open}), do: "map()"
  defp segment({:map, fields, :open}) when is_list(fields), do: "%{..., " <> fields(fields) <> "}"

  # `:closed` (literal-complete) and `nil` (partial) tails BOTH render without an
  # open marker. This is a DELIBERATE display compromise (three-marker model, see
  # `ElixirSense.Core.Binding`): surfacing the partial-vs-closed distinction in the
  # rendered string (e.g. a `...` for partial) would churn every guard-fact display
  # expectation for no user-facing benefit, so partial maps render exactly as
  # closed ones do. Only `:open` carries the `...` marker (handled above).
  # An empty-field `:closed`/`nil` map renders as `map()` for the same reason
  # (the empty closed map `%{}` round-trips through the distinct `:empty_map` shape).
  defp segment({:map, [], _updated}), do: "map()"
  defp segment({:map, fields, _updated}) when is_list(fields), do: "%{" <> fields(fields) <> "}"

  defp segment({:struct, fields, {:atom, module}, _updated}) when is_atom(module) do
    # Drop fields we have no information about (`term()`): a struct typed only by
    # its module (e.g. a `defimpl for:` arg) renders as `%URI{}`, not
    # `%URI{a: term(), b: term(), …}`. Informative fields are still shown.
    # Note: {:optional, _} renders as if_set(...) — not "term()" — so it IS kept.
    case fields |> Keyword.delete(:__struct__) |> Enum.reject(&uninformative_field?/1) do
      [] -> "%" <> inspect(module) <> "{}"
      kept -> "%" <> inspect(module) <> "{" <> fields(kept) <> "}"
    end
  end

  defp segment({:struct, _fields, _type, _updated}), do: "struct()"

  defp segment({:union, members}) when is_list(members) do
    rendered = members |> Enum.map(&segment/1) |> Enum.uniq()

    # `term()` is the top type, so any union containing it *is* `term()` —
    # rendering `nil or term()` (e.g. an optional struct field) is redundant noise.
    if "term()" in rendered do
      "term()"
    else
      # Compiler dialect: join with " or " (task #18)
      Enum.join(rendered, " or ")
    end
  end

  defp segment({:intersection, members}) when is_list(members),
    do: intersection(members)

  # Functions. `{:fun, arity}` knows only the arity (args/return are unknown);
  # `{:fun, args, return}` knows the signature; `{:fun_clauses, [...]}` carries
  # one arrow per clause.
  defp segment({:fun, arity}) when is_integer(arity) and arity >= 0,
    do: arrow(List.duplicate("term()", arity), "term()")

  defp segment({:fun, args, return}) when is_list(args),
    do: arrow(Enum.map(args, &segment/1), segment(return))

  defp segment({:fun_clauses, []}), do: "fun()"

  defp segment({:fun_clauses, clauses}) when is_list(clauses) do
    clauses
    |> Enum.map(fn {args, return} -> arrow(Enum.map(args, &segment/1), segment(return)) end)
    |> Enum.uniq()
    # Compiler dialect: join with " or " (task #18)
    |> Enum.join(" or ")
  end

  # Unresolved thunks, calls, variables, attributes, etc. — anything we can't
  # render precisely. Kept total on purpose.
  defp segment(_other), do: "term()"

  defp arrow([], return), do: "(-> " <> return <> ")"
  defp arrow(args, return), do: "(" <> Enum.join(args, ", ") <> " -> " <> return <> ")"

  defp fields(fields) do
    Enum.map_join(fields, ", ", fn
      {{:domain, key_shape}, value} -> segment(key_shape) <> " => " <> segment(value)
      {key, value} -> render_map_key(key) <> segment(value)
    end)
  end

  # Render an atom map key in compiler style (task #24).
  # Macro.inspect_atom(:key, key) produces "key:" for identifiers and
  # ~s("foo bar":) for atoms requiring quoting — we append a space.
  defp render_map_key(key) when is_atom(key) do
    Macro.inspect_atom(:key, key) <> " "
  end

  defp uninformative_field?({_key, value}) do
    # term() is uninformative; if_set(...) wrapping something IS informative.
    segment(value) == "term()"
  end

  # An intersection is "all of these at once"; for display pick the most
  # informative member (a concrete struct/map beats a generic), dropping the
  # uninformative "term()" parts. Falls back to joining with " and ".
  defp intersection(members) do
    rendered =
      members
      |> Enum.map(&segment/1)
      |> Enum.uniq()
      |> Enum.reject(&(&1 == "term()"))

    case rendered do
      [] ->
        "term()"

      [one] ->
        one

      many ->
        case Enum.find(many, &struct_or_map?/1) do
          nil -> Enum.join(many, " and ")
          found -> found
        end
    end
  end

  defp struct_or_map?("%" <> _), do: true
  defp struct_or_map?(_), do: false

  # ── Elision for render_hint/3 (task #25) ────────────────────────────────────

  # No limit: return as-is.
  defp elide(text, nil), do: text

  defp elide(text, max_length) when is_integer(max_length) and max_length > 0 do
    graphemes = String.graphemes(text)

    if length(graphemes) <= max_length do
      text
    else
      # We need max_length graphemes total, last one being `…`
      budget = max_length - 1

      # Try smart cut at an " or " boundary (prefer longer prefix)
      case smart_union_cut(text, budget) do
        nil ->
          # Simple grapheme cut
          graphemes |> Enum.take(budget) |> Enum.join() |> Kernel.<>("…")

        label ->
          label
      end
    end
  end

  # Try to find the rightmost " or " boundary where the prefix fits in `budget`
  # graphemes. Returns `prefix <> "…"` or nil.
  defp smart_union_cut(text, budget) do
    separator = " or "

    # Split on all " or " occurrences and pick the longest complete prefix
    # (joined back with " or ") that fits within `budget` graphemes.
    parts = String.split(text, separator)

    # Accumulate parts, tracking the joined length. Stop when adding the next
    # part would exceed budget. Return the longest fitting prefix.
    {_, best} =
      Enum.reduce_while(parts, {[], nil}, fn part, {acc, _best} ->
        candidate_parts = acc ++ [part]
        candidate = Enum.join(candidate_parts, separator)

        if String.length(candidate) <= budget do
          {:cont, {candidate_parts, candidate}}
        else
          {:halt, {acc, Enum.join(acc, separator)}}
        end
      end)

    case best do
      nil -> nil
      "" -> nil
      prefix -> prefix <> "…"
    end
  end
end
