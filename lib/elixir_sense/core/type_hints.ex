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
  does **not** reimplement any of it — the *text* of `type_hint_for_var/4` is
  pure delegation. This module DOES, however, refine the hint's `source:`
  provenance beyond the low-level `:native | :shape` value that
  `TypePresentation.render_hint/3` reports (see "Hint provenance" below).

  ## Hint provenance (`source:`)

  `type_hint_for_var/4` returns a `source:` describing the **strongest backing**
  of the displayed type, so an LSP can apply a `minimumTrust` filter without
  knowing any internals. The value is one of, strongest first:

    * `:native_exck` — compiler-verified. The displayed type is backed by a
      signature read from a compiled `.beam` ExCk chunk (ground truth), either
      because the native engine inferred the var directly, or because the var is
      bound to a remote call `Mod.fun(args)` whose `{fun, arity}` has an ExCk
      `:sig`.
    * `:native_inferred` — the native `Module.Types` engine inferred the type,
      either as the var's own descriptor (`of_match`/`of_expr` on the current
      file) or as the natively-inferred signature of a local call the var is
      bound to.
    * `:spec` — the type is derived from a `@spec` (for a local call, the only
      backing was a spec; for a remote call, no ExCk `:sig` was found but the
      module publishes a typespec for that `{fun, arity}`).
    * `:shape` — purely structural: a literal/container shape, or a call whose
      backing could not be attributed any better.

  Use `trust_rank/1` to compare without hard-coding the ordering.

  ### Classification scope

  When the var's structural type is a *container* of call thunks
  (`{x, Mod.f(), ...}`, `%{k: g()}`, a union, …), only the **top-level** thunk
  is attributed. Containers and literals are `:shape`; the provenance of nested
  calls is intentionally not aggregated (it would require a recursive,
  potentially expensive walk, and the displayed text is a structural shape
  regardless). A var whose `type` is `nil` but that still rendered (via the
  native descriptor) is classified by the render path alone (`:native_inferred`).

  All provenance lookups are wrapped defensively: any error in attribution falls
  back to `:shape`. Misclassification must never break a hint. Per-request
  lookups are cached in the process dictionary keyed by `{ref, mfa}`, reusing the
  same caching discipline as the rest of the module.

  ## API summary

  | Function | Description |
  |---|---|
  | `request_context/1` | Build a per-request `Context` |
  | `discard/1` | Erase all process-dictionary entries for a context |
  | `type_hint_for_var/4` | Render a type hint given an explicit `VarInfo` |
  | `type_hint_at/4` | Flow-sensitive hint: resolve the var AT a position from the env at that position |
  | `effective_params/4` | Structured parameter list for `module.fun/arity` |
  | `trust_rank/1` | Total ordering over `t:source/0` values |

  ## Request lifecycle and caching

  Create one `Context` per LSP request, in the request process, via
  `request_context/1`. The context carries the metadata plus a unique `ref`.
  Caches (the per-module local-sigs map, the per-position env, and per-MFA
  effective params) are stored in the **process dictionary** keyed by that ref.

  ### Lifecycle contract

  The context **MUST** be either:

    1. **Request-process-scoped** — created and used in a process that terminates
       when the request finishes (the normal LSP case). The process dictionary is
       cleaned up automatically when the process dies.
    2. **Explicitly discarded** — long-lived processes (pools, GenServers, tests)
       that create a context MUST call `discard/1` when they are done with it, to
       avoid unbounded process-dictionary growth.

  Do not share a `Context` across processes.
  """

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.ElixirTypes
  alias ElixirSense.Core.ExCkReader
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Normalized.Typespec
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
  @type source :: :native_exck | :native_inferred | :spec | :shape
  @type hint :: %{label: String.t(), full: String.t(), source: source}

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
  Discard all process-dictionary entries belonging to `ctx`.

  Erases every entry whose key matches the `{TypeHints, ref, _, ...}` prefix for
  `ctx.ref`. After this call the context is effectively dead — subsequent calls
  that reference it will re-populate the cache from scratch (safe, but wasteful).

  Long-lived processes (GenServers, pools, test processes) MUST call `discard/1`
  when finished with a context. Request-process-scoped contexts do not need
  explicit cleanup (the process-dictionary is freed when the process terminates),
  but calling `discard/1` on them is harmless.
  """
  @spec discard(Context.t()) :: :ok
  def discard(%Context{ref: ref}) do
    mod = __MODULE__

    :erlang.get_keys()
    |> Enum.filter(fn
      key when is_tuple(key) and tuple_size(key) >= 3 ->
        elem(key, 0) == mod and elem(key, 1) == ref

      _ ->
        false
    end)
    |> Enum.each(&Process.delete/1)

    :ok
  end

  @doc """
  Render a type hint for `var_info` at `position`, or `:skip`.

  Returns `:skip` when there is no env at the position or the rendered type is
  uninformative; otherwise `{:ok, %{label, full, source}}`. The `label`/`full`
  text is produced verbatim by `ElixirSense.Core.TypePresentation.render_hint/3`;
  the `source` is refined here into the richer provenance value documented in the
  moduledoc ("Hint provenance"). `opts` are passed straight through
  (`:max_length`, `:max_full_length`, `:widen_literals`).

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

        case TypePresentation.render_hint(binding, var_info, opts) do
          {:ok, hint} ->
            source = classify_source(ctx, env.module, hint.source, var_info.type)
            {:ok, %{hint | source: source}}

          :skip ->
            :skip
        end
    end
  end

  @doc """
  Flow-sensitive type hint for variable `var_name` at `position`, or `:skip`.

  Resolves the variable from the environment at the READ position rather than
  requiring the caller to supply a `VarInfo` from the binding site. This is
  the preferred entry point when the variable's refined (narrowed) type at the
  read position differs from its type at the binding site.

  ## Flow sensitivity

  The metadata model captures flow-sensitive narrowing at the per-line level:
  `Metadata.get_env(metadata, position)` returns an environment whose `vars`
  list contains the `VarInfo` for each in-scope variable as it is narrowed at
  that specific position. For example, inside a `cond do is_integer(x) ->`
  clause, `env.vars` contains a `VarInfo` for `x` with `type: :integer`,
  whereas at the binding site `x` may only be typed as `nil` / `term()`.

  This empirically-verified fact holds for:
    - `cond` clause guards (`is_integer/1`, `is_binary/1`, etc.)
    - `case` clause guards
    - Rebindings (a new version of the var replaces the old one in `env.vars`)

  **Scope-narrowed `VarInfo` DOES surface in `env.vars` at read positions.**

  ## Selection rule for `env.vars`

  `env.vars` contains at most one entry per variable name at any given env
  (the env is per-line, so all versions have already been resolved to the
  single in-scope version). The lookup is therefore:

      Enum.find(env.vars, &(&1.name == var_name))

  No version comparison is needed: if a var of the requested name appears in
  `env.vars` at the read position, it is the in-scope, possibly narrowed
  version.

  ## Caching

  Cached per `{ref, :hint_at, position, var_name}` in the process dictionary.
  The underlying `env_for/2` and `local_sigs_for/2` caches are also reused.

  ## Return values

    * `:skip` — no variable with `var_name` is in scope at `position`, or the
      resolved `VarInfo` produces an uninformative type (same as
      `type_hint_for_var/4`).
    * `{:ok, %{label, full, source}}` — the hint for the flow-sensitive
      `VarInfo` at the read position.
  """
  @spec type_hint_at(Context.t(), {pos_integer, pos_integer}, atom, keyword) ::
          :skip | {:ok, hint}
  def type_hint_at(%Context{} = ctx, position, var_name, opts \\ [])
      when is_atom(var_name) do
    cache_key = {__MODULE__, ctx.ref, :hint_at, position, var_name}

    case Process.get(cache_key, :__miss__) do
      :__miss__ ->
        result = compute_hint_at(ctx, position, var_name, opts)
        Process.put(cache_key, result)
        result

      cached ->
        cached
    end
  end

  @doc """
  Total ordering over `t:source/0`, strongest first.

  Returns `0..3` so consumers can apply a `minimumTrust` filter without
  hard-coding the symbol order: `:native_exck` (0, strongest) > `:native_inferred`
  (1) > `:spec` (2) > `:shape` (3). A larger rank means *weaker* provenance, so a
  consumer keeps a hint when `trust_rank(hint.source) <= trust_rank(minimum)`.
  """
  @spec trust_rank(source) :: 0..3
  def trust_rank(:native_exck), do: 0
  def trust_rank(:native_inferred), do: 1
  def trust_rank(:spec), do: 2
  def trust_rank(:shape), do: 3

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

  # Resolve the var from env.vars at the read position, then delegate to the
  # existing hint path with that flow-sensitive VarInfo.
  #
  # Selection rule: env.vars at a given position holds at most one VarInfo per
  # name (the in-scope, possibly flow-narrowed version). A plain name match is
  # sufficient — no version comparison is required.
  defp compute_hint_at(%Context{} = ctx, position, var_name, opts) do
    case env_for(ctx, position) do
      nil ->
        :skip

      env ->
        case Enum.find(env.vars, &(&1.name == var_name)) do
          nil ->
            :skip

          %VarInfo{} = var_info ->
            local_sigs = local_sigs_for(ctx, env.module)
            binding = Binding.from_env(env, ctx.metadata, position, local_sigs: local_sigs)

            case TypePresentation.render_hint(binding, var_info, opts) do
              {:ok, hint} ->
                source = classify_source(ctx, env.module, hint.source, var_info.type)
                {:ok, %{hint | source: source}}

              :skip ->
                :skip
            end
        end
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

  # ── source provenance classification ─────────────────────────────────────────
  #
  # `TypePresentation.render_hint/3` reports only `:native | :shape`. We refine
  # that into `:native_exck | :native_inferred | :spec | :shape` by attributing
  # the strongest backing of the displayed type. See the moduledoc "Hint
  # provenance" for the contract and scoping rules.

  # `:native` always means the native engine produced the text from the var's own
  # descriptor (`of_match`/`of_expr` on the current file).
  defp classify_source(_ctx, _module, :native, _type), do: :native_inferred

  # `:shape` means the structural engine produced the text. Attribute it from the
  # TOP-LEVEL thunk of the var's structural type. Containers/literals → `:shape`.
  defp classify_source(ctx, module, :shape, type) do
    attribute_thunk(ctx, module, type)
  rescue
    # Misclassification must never break a hint.
    _ -> :shape
  end

  # Remote call `Mod.fun(args)`: ExCk `:sig` wins; else a published typespec for
  # the MFA counts as `:spec`; else `:shape`.
  defp attribute_thunk(ctx, _module, {:call, {:atom, mod}, fun, args})
       when is_atom(mod) and is_atom(fun) and is_list(args) do
    arity = length(args)

    cached_attr(ctx, {mod, fun, arity}, fn ->
      cond do
        exck_sig?(mod, fun, arity) -> :native_exck
        remote_spec?(mod, fun, arity) -> :spec
        true -> :shape
      end
    end)
  end

  # Local/imported call: the provenance is NOT carried in the local-sigs map
  # values (they are `{kind, sig}`), so consult `ModFunInfo.elixir_types_sig_source`
  # for the MFA directly, then fall back to a spec in metadata. The local-call
  # thunk does not carry the module, so resolve it against the current `module`.
  defp attribute_thunk(ctx, module, {:local_call, fun, _position, args})
       when is_atom(fun) and is_list(args) and is_atom(module) do
    arity = length(args)

    cached_attr(ctx, {module, fun, arity}, fn ->
      case local_sig_source(ctx.metadata, module, fun, arity) do
        :exck -> :native_exck
        :inferred -> :native_inferred
        _ -> if local_spec?(ctx.metadata, module, fun, arity), do: :spec, else: :shape
      end
    end)
  end

  # Containers, literals, unions, nil types, or any other top-level shape are not
  # attributed to a single backing → `:shape` (see "Classification scope").
  defp attribute_thunk(_ctx, _module, _type), do: :shape

  # Per-request memoised attribution keyed by {ref, mfa}, reusing the pdict
  # caching discipline used elsewhere in this module.
  defp cached_attr(%Context{} = ctx, mfa, fun) do
    cache_key = {__MODULE__, ctx.ref, :source, mfa}

    case Process.get(cache_key, :__miss__) do
      :__miss__ ->
        result = fun.()
        Process.put(cache_key, result)
        result

      cached ->
        cached
    end
  end

  defp exck_sig?(mod, fun, arity) do
    match?({:ok, _}, ExCkReader.lookup_signature(mod, fun, arity))
  rescue
    _ -> false
  end

  defp remote_spec?(mod, fun, arity) do
    Enum.any?(Typespec.get_specs(mod), fn
      {{^fun, ^arity}, _} -> true
      _ -> false
    end)
  rescue
    _ -> false
  end

  defp local_sig_source(metadata, module, fun, call_arity) do
    # Default-arg functions are keyed ONLY under max arity in mods_funs_to_positions.
    # A call `f(x)` for `def f(a, b \\ 1)` has call_arity=1 but the entry is keyed
    # under arity 2. We therefore scan all {module, fun, *} entries and pick the
    # first whose defaults window admits the call arity.
    find_mod_fun_info(metadata, module, fun, call_arity)
    |> case do
      %ModFunInfo{elixir_types_sig_source: source} -> source
      _ -> nil
    end
  rescue
    _ -> nil
  end

  defp local_spec?(metadata, module, fun, arity) do
    match?({:ok, _}, ElixirTypes.spec_signature_from_metadata(metadata, module, fun, arity))
  rescue
    _ -> false
  end

  # Shared helper: find the ModFunInfo entry for {module, fun} whose defaults
  # window admits `call_arity`. The metadata key is the *max* arity (all params,
  # defaults included). A call at a lower arity is valid when
  #   max_arity - default_count <= call_arity <= max_arity.
  # This mirrors the scan already used by `metadata_params/4`.
  defp find_mod_fun_info(%Metadata{} = metadata, module, fun, call_arity) do
    metadata.mods_funs_to_positions
    |> Enum.find_value(fn
      {{^module, ^fun, key_arity}, %ModFunInfo{} = info}
      when not is_nil(key_arity) ->
        params = info.params |> List.first() || []

        defaults =
          params
          |> Enum.count(fn
            {:\\, _, _} -> true
            _ -> false
          end)

        mandatory = key_arity - defaults

        if call_arity >= mandatory and call_arity <= key_arity do
          info
        else
          nil
        end

      _ ->
        nil
    end)
  rescue
    _ -> nil
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
