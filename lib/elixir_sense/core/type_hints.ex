defmodule ElixirSense.Core.TypeHints do
  @moduledoc """
  THE stable, LSP-facing seam for type-derived editor hints.

  This module is the single entry point an LSP consumer (e.g. elixir-ls) should
  use to obtain type hints and structured parameter information. It deliberately
  hides the internals — `ElixirSense.Core.Binding`, `ElixirSense.Core.ElixirTypes`,
  `ElixirSense.Core.TypePresentation`, and the metadata layout — behind a small,
  stable API.

  ## Why this seam exists

    * It removes `Binding.from_env` / `TypePresentation` knowledge from the LSP
      layer, so the LSP does not couple to unstable internal shapes.
    * It is the natural home for per-request caching (see `request_context/1`),
      turning the LSP's O(hints × N) per-request cost into O(N + hints).
    * It is the **single swap point** when Elixir ships a public typesystem API:
      the implementations here change, the API does not.

  All rendering policy (suppression of uninformative types, literal widening,
  length truncation) lives in `ElixirSense.Core.TypePresentation`. This module
  does **not** reimplement any of it — `type_hint_for_var/4` is pure delegation.

  ## Request lifecycle and caching

  Create one `Context` per LSP request, in the request process, via
  `request_context/1`. The context carries the metadata plus a unique `ref`.
  Caches (the per-module local-sigs map, the per-position env, and per-MFA
  effective params) are stored in the **process dictionary** keyed by that ref.
  Because the request process dies when the request finishes, these entries are
  request-scoped and require no explicit cleanup. Do not share a `Context`
  across processes.
  """

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.ElixirTypes
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.State.ModFunInfo
  alias ElixirSense.Core.State.VarInfo
  alias ElixirSense.Core.TypePresentation

  defmodule Context do
    @moduledoc """
    Opaque per-request context produced by `ElixirSense.Core.TypeHints.request_context/1`.

    Holds the request `metadata` and a unique `ref` used to key request-scoped
    caches in the process dictionary. Treat as opaque.
    """
    @type t :: %__MODULE__{metadata: Metadata.t(), ref: reference()}
    defstruct [:metadata, :ref]
  end

  @type param :: %{name: String.t() | nil, has_default: boolean()}
  @type hint :: %{label: String.t(), full: String.t(), source: :native | :shape}

  @doc """
  Build a per-request `Context` wrapping `metadata` with a fresh unique ref.

  Call this once per LSP request, in the request process. The returned context
  is the cache scope for `type_hint_for_var/4` and `effective_params/4`.
  """
  @spec request_context(Metadata.t()) :: Context.t()
  def request_context(%Metadata{} = metadata) do
    %Context{metadata: metadata, ref: make_ref()}
  end

  @doc """
  Render a type hint for `var_info` at `position`, or `:skip`.

  Returns `:skip` when there is no env at the position or the rendered type is
  uninformative; otherwise `{:ok, %{label, full, source}}` as produced by
  `ElixirSense.Core.TypePresentation.render_hint/3`. `opts` are passed straight
  through (`:max_length`, `:max_full_length`, `:widen_literals`).

  ## Caching

  The expensive part of building the binding env is
  `ElixirTypes.build_local_sigs_map/2` (an O(N) scan of `mods_funs_to_positions`).
  It is computed once per `{ref, module}` and reused for every subsequent hint in
  the same module via `Binding.from_env/4`'s `:local_sigs` option. The
  `Metadata.get_env/2` result is also O(N) (it scans all scopes), so it is cached
  per `{ref, position}`.
  """
  @spec type_hint_for_var(Context.t(), {pos_integer, pos_integer}, VarInfo.t(), keyword) ::
          :skip | {:ok, hint}
  def type_hint_for_var(%Context{} = ctx, position, %VarInfo{} = var_info, opts \\ []) do
    case env_for(ctx, position) do
      nil ->
        :skip

      env ->
        local_sigs = local_sigs_for(ctx, env.module)
        binding = Binding.from_env(env, ctx.metadata, position, local_sigs: local_sigs)
        TypePresentation.render_hint(binding, var_info, opts)
    end
  end

  @doc """
  Return the structured parameter list for `module.fun/arity` at the requested
  concrete `arity`, or `:error`.

  Each entry is `%{name: String.t() | nil, has_default: boolean()}`. This is the
  structured replacement for parsing signature strings on the LSP side: default
  detection and the default-elision rule (see below) live here. Parameters whose
  pattern is not an identifier (literals, struct-only patterns) yield `name: nil`;
  the caller is expected to skip those.

  ## Default-elision rule (verified empirically)

  Elixir fills positional arguments left-to-right and elides default-bearing
  params from the **right** among the defaults. Concretely, given a clause with
  `M` mandatory params and `D` default params (total `M + D`), calling at concrete
  `arity` keeps all `M` mandatory params plus the **leftmost `arity - M`** default
  params, in original order, dropping the rest.

  Experiment (Elixir 1.20.1):

      def f(a, b \\\\ 1, c), do: {a, b, c}
      f(:x, :y) #=> {:x, 1, :y}     # arity 2 → keeps [a, c], drops default b

      def g(a \\\\ 1, b \\\\ 2, c), do: {a, b, c}
      g(:x)     #=> {1, 2, :x}      # arity 1 → keeps [c], drops a and b
      g(:x, :y) #=> {:x, 2, :y}     # arity 2 → keeps [a, c], drops default b

  i.e. for g/2 the mandatory `c` is kept and the leftmost default `a` is kept,
  while the rightmost default `b` is elided.

  ## Caching

  Cached per `{ref, module, fun, arity}` in the process dictionary.
  """
  @spec effective_params(Context.t(), module, atom, non_neg_integer) ::
          {:ok, [param]} | :error
  def effective_params(%Context{} = ctx, module, fun, arity)
      when is_atom(module) and is_atom(fun) and is_integer(arity) and arity >= 0 do
    cache_key = {__MODULE__, ctx.ref, :params, module, fun, arity}

    case Process.get(cache_key, :__miss__) do
      :__miss__ ->
        result = compute_effective_params(ctx, module, fun, arity)
        Process.put(cache_key, result)
        result

      cached ->
        cached
    end
  end

  # ── env + sigs caching ──────────────────────────────────────────────────────

  defp env_for(%Context{} = ctx, position) do
    cache_key = {__MODULE__, ctx.ref, :env, position}

    case Process.get(cache_key, :__miss__) do
      :__miss__ ->
        env = Metadata.get_env(ctx.metadata, position)
        Process.put(cache_key, env)
        env

      cached ->
        cached
    end
  end

  defp local_sigs_for(%Context{}, nil), do: %{}

  defp local_sigs_for(%Context{} = ctx, module) do
    cache_key = {__MODULE__, ctx.ref, :local_sigs, module}

    case Process.get(cache_key, :__miss__) do
      :__miss__ ->
        sigs =
          if ElixirTypes.enabled?() do
            ElixirTypes.build_local_sigs_map(ctx.metadata, module)
          else
            %{}
          end

        Process.put(cache_key, sigs)
        sigs

      cached ->
        cached
    end
  end

  # ── effective_params computation ─────────────────────────────────────────────

  defp compute_effective_params(%Context{} = ctx, module, fun, arity) do
    case metadata_params(ctx.metadata, module, fun, arity) do
      {:ok, _} = ok -> ok
      :error -> remote_params(module, fun, arity)
    end
  end

  # Metadata (source) modules: ModFunInfo.params is a list of clause param ASTs
  # (list(list(ast))). The metadata key is the *maximum* arity (all params,
  # defaults included). We pick the clause whose [mandatory_count, total] window
  # admits the requested arity, extract per-param info from the AST, then apply
  # the default-elision rule for the concrete arity.
  defp metadata_params(%Metadata{} = metadata, module, fun, arity) do
    clauses =
      metadata.mods_funs_to_positions
      |> Enum.filter(fn
        {{^module, ^fun, key_arity}, _info} when not is_nil(key_arity) -> true
        _ -> false
      end)
      |> Enum.flat_map(fn {_key, %ModFunInfo{params: params_variants}} -> params_variants end)

    chosen =
      Enum.find(clauses, fn clause ->
        params = Enum.map(clause, &param_from_ast/1)
        mandatory = Enum.count(params, &(not &1.has_default))
        total = length(params)
        arity >= mandatory and arity <= total
      end)

    case chosen do
      nil ->
        :error

      clause ->
        params = Enum.map(clause, &param_from_ast/1)
        {:ok, elide_defaults(params, arity)}
    end
  end

  # `{:\\, _, [pattern, _default]}` → has_default: true, name from the pattern.
  defp param_from_ast({:\\, _, [pattern, _default]}) do
    %{name: var_name_from_pattern(pattern), has_default: true}
  end

  defp param_from_ast(pattern) do
    %{name: var_name_from_pattern(pattern), has_default: false}
  end

  # Name extraction from a pattern AST.
  # A plain var: {name, _, ctx} where ctx is an atom.
  defp var_name_from_pattern({name, _meta, ctx}) when is_atom(name) and is_atom(ctx) do
    Atom.to_string(name)
  end

  # A match `lhs = rhs`: whichever side is a plain var names the param. Prefer
  # the rhs (the more common `%{} = opts` shape), fall back to lhs.
  defp var_name_from_pattern({:=, _meta, [lhs, rhs]}) do
    var_name_from_pattern(rhs) || var_name_from_pattern(lhs)
  end

  # Unnamed pattern (literal, struct-only, call, etc.) → no name.
  defp var_name_from_pattern(_other), do: nil

  # Apply Elixir's default-elision rule for a concrete arity: keep all mandatory
  # params plus the leftmost (arity - mandatory) defaults, in order.
  defp elide_defaults(params, arity) do
    mandatory = Enum.count(params, &(not &1.has_default))
    defaults_to_keep = max(arity - mandatory, 0)

    {kept, _} =
      Enum.map_reduce(params, defaults_to_keep, fn
        %{has_default: false} = p, budget ->
          {p, budget}

        %{has_default: true} = p, budget when budget > 0 ->
          {p, budget - 1}

        %{has_default: true}, budget ->
          {nil, budget}
      end)

    kept
    |> Enum.reject(&is_nil/1)
    |> Enum.map(fn p -> %{name: p.name, has_default: p.has_default} end)
  end

  # ── remote / compiled modules ─────────────────────────────────────────────────
  #
  # `Introspection.get_signatures/2` returns one entry per documented arity, with
  # `params` a list of strings; defaults are marked by a " \\ " infix
  # (e.g. "options \\ []"). The string-level default policy lives HERE so the LSP
  # consumes the same structured list as for metadata modules.
  defp remote_params(module, fun, arity) do
    signatures = Introspection.get_signatures(module, fun)

    chosen =
      signatures
      |> Enum.map(fn %{params: raw} -> Enum.map(raw, &param_from_string/1) end)
      |> Enum.find(fn params ->
        mandatory = Enum.count(params, &(not &1.has_default))
        total = length(params)
        arity >= mandatory and arity <= total
      end)

    case chosen do
      nil -> :error
      params -> {:ok, elide_defaults(params, arity)}
    end
  end

  defp param_from_string(raw) when is_binary(raw) do
    case String.split(raw, " \\\\ ", parts: 2) do
      [pattern, _default] ->
        %{name: identifier_or_nil(pattern), has_default: true}

      [pattern] ->
        %{name: identifier_or_nil(pattern), has_default: false}
    end
  end

  # The string form is already the printed param name (e.g. "options", "init_arg").
  # Anything that is not a bare identifier (contains punctuation/whitespace) is a
  # non-identifier pattern → nil.
  defp identifier_or_nil(str) do
    trimmed = String.trim(str)

    if Regex.match?(~r/^[a-z_][a-zA-Z0-9_]*[?!]?$/, trimmed) do
      trimmed
    else
      nil
    end
  end
end
