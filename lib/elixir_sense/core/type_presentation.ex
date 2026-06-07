defmodule ElixirSense.Core.TypePresentation do
  @moduledoc """
  The LSP-facing type surface: resolve a stored ElixirSense "shape" through
  `ElixirSense.Core.Binding` and render it as human-readable Elixir-ish text for
  inlay hints, hover, and property-aware completion.

  Stored shapes (in `VarInfo.type`) may contain unresolved thunks —
  `{:variable, ...}`, `{:call, ...}`, `{:map_key, ...}`, `{:tuple_nth, ...}`,
  `{:difference, ...}`, etc. Callers must never display those directly. This
  module guarantees a resolved, thunk-free result.

  ## External API (stable surface for the LSP layer)

    * `resolve_shape/2` — `{:ok, resolved_shape} | :unknown` (shape, not text).
    * `render_hint/2` — `{:ok, text} | :skip` for a `VarInfo` (inlay hints;
      skips uninformative `term()`/`none()`/unknown).
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
  def render_var(%Binding{} = binding, %VarInfo{elixir_types_descr: descr, type: type}) do
    structural = resolve_and_render(binding, type)

    if informative?(structural) do
      structural
    else
      case render_descr(descr) do
        {:ok, _} = ok -> ok
        :unknown -> structural
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
  The resolved field types of a map/struct receiver, for property-aware
  completion/definition. Returns `%{field_name => rendered_type_text}` (the
  `__struct__` field is dropped), or `%{}` when the receiver isn't a map/struct.
  """
  def fields_for_receiver(%Binding{} = binding, shape) do
    case Binding.expand(binding, shape) do
      {:map, fields, _updated} -> field_map(fields)
      {:struct, fields, _type, _updated} -> field_map(Keyword.delete(fields, :__struct__))
      _other -> %{}
    end
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

  defp informative?({:ok, text}) when text not in ["term()", "none()"], do: true
  defp informative?(_other), do: false

  defp hint_noise?("term()"), do: true
  defp hint_noise?("none()"), do: true
  defp hint_noise?(_other), do: false

  @doc """
  Render an already-resolved, thunk-free shape. Returns `{:ok, text}` or
  `:unknown` (the latter for `nil`, i.e. "no information").
  """
  def render(nil), do: :unknown
  def render(shape), do: {:ok, segment(shape)}

  # Prefer the native descriptor by converting it to a shape and rendering that
  # (one renderer for both paths). nil descr or unavailable engine -> :unknown.
  defp render_descr(nil), do: :unknown

  defp render_descr(descr) do
    if ElixirTypes.available?() do
      case ElixirTypes.to_shape(descr) do
        nil -> :unknown
        shape -> render(shape)
      end
    else
      :unknown
    end
  rescue
    _ -> :unknown
  catch
    _, _ -> :unknown
  end

  # segment/1 always returns a string. `nil` (unknown) becomes "term()" inside a
  # structure so a single unknown leaf doesn't sink the whole rendering.
  defp segment(nil), do: "term()"
  defp segment(:none), do: "none()"
  defp segment(:not_set), do: "not_set()"
  defp segment(:empty), do: "[]"
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

  defp segment({:atom, nil}), do: "nil"
  defp segment({:atom, value}) when is_atom(value), do: inspect(value)

  defp segment({:integer, nil}), do: "integer()"
  defp segment({:integer, value}) when is_integer(value), do: Integer.to_string(value)

  defp segment({:float, value}) when is_float(value), do: Float.to_string(value)
  defp segment({:float, _}), do: "float()"

  defp segment({:binary, value}) when is_binary(value), do: inspect(value)
  defp segment({:binary, _}), do: "binary()"

  defp segment({:list, :empty}), do: "[]"
  defp segment({:list, elem}), do: "[" <> segment(elem) <> "]"
  defp segment({:nonempty_list, elem}), do: "[" <> segment(elem) <> ", ...]"

  defp segment({:tuple, _size, elems}) when is_list(elems),
    do: "{" <> Enum.map_join(elems, ", ", &segment/1) <> "}"

  defp segment({:map, [], _updated}), do: "map()"
  defp segment({:map, fields, _updated}) when is_list(fields), do: "%{" <> fields(fields) <> "}"

  defp segment({:struct, fields, {:atom, module}, _updated}) when is_atom(module) do
    case fields |> Keyword.delete(:__struct__) do
      [] -> "%" <> inspect(module) <> "{}"
      kept -> "%" <> inspect(module) <> "{" <> fields(kept) <> "}"
    end
  end

  defp segment({:struct, _fields, _type, _updated}), do: "struct()"

  defp segment({:union, members}) when is_list(members) do
    members |> Enum.map(&segment/1) |> Enum.uniq() |> Enum.join(" | ")
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
    |> Enum.join(" | ")
  end

  # Unresolved thunks, calls, variables, attributes, etc. — anything we can't
  # render precisely. Kept total on purpose.
  defp segment(_other), do: "term()"

  defp arrow([], return), do: "(-> " <> return <> ")"
  defp arrow(args, return), do: "(" <> Enum.join(args, ", ") <> " -> " <> return <> ")"

  defp fields(fields) do
    Enum.map_join(fields, ", ", fn {key, value} -> "#{key}: #{segment(value)}" end)
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
end
