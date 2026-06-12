defmodule ElixirSense.Core.ElixirTypes do
  @moduledoc """
  Adaptor over Elixir's Module.Types for set-theoretic type inference.

  This module provides a stable interface to Elixir's evolving type system,
  allowing ElixirSense to benefit from precise type inference while maintaining
  compatibility and fallback behavior.

  ## Usage

  The adaptor is disabled by default. To enable it:

      config :elixir_sense, :use_elixir_types, true

  When enabled, ElixirSense will use Elixir's Module.Types for enhanced type
  inference in addition to its own type system.

  ## Requirements

  - Elixir 1.17+ with Module.Types support
  - The feature is automatically disabled if Module.Types is not available

  ## Supported Types

  The adaptor currently supports shape conversion for:
  - Basic types: integers, floats, binaries, atoms
  - Collections: lists, tuples, maps (with atom keys)
  - Nested structures with conservative fallbacks

  ## Integration Points

  - **TypeInference**: Enhanced expression typing for literals, calls, and AST nodes
  - **Compiler**: Pattern matching refinement via `of_match/5`
  - **Binding**: Call resolution via `extract_return_type_from_sig/2` and `spec_signature_from_metadata/4`

  ## Examples

      # Check availability
      ElixirTypes.available?()
      #=> true

      # Type an expression — returns an opaque Descr.t()
      ElixirTypes.of_expr(42)
      #=> {:ok, descr}  # descr is an opaque type descriptor

      # Convert to ElixirSense shape
      {:ok, descr} = ElixirTypes.of_expr([1, 2, 3])
      ElixirTypes.to_shape(descr)
      #=> {:list, {:integer, nil}}

  ## Limitations

  - Pattern matching relies on best-effort conversion and may skip complex types
  - Computed module expressions (from function calls) not fully resolved
  """

  require Logger

  @doc """
  Returns true if the running Elixir exposes a usable `Module.Types` backend.

  Requires both an expression typer (`Expr.of_expr/5` on 1.19+, or `/3` on 1.18)
  AND the modern `Module.Types.stack/7` that `init_stack/6` builds. 1.17 has only
  `of_expr/3` + `stack/5` and no `of_match`, so nothing the adaptor uses works
  there — `available?/0` is false and callers fall back to the custom engine.

  ## Examples

      iex> ElixirTypes.available?()
      true

  """
  # All `apply/3` calls dispatch to version-specific `Module.Types` APIs whose
  # arity differs across Elixir releases; a literal wrong-version call would be an
  # undefined-function compile warning, so `apply/3` is the only safe choice.
  # credo:disable-for-this-file Credo.Check.Refactor.Apply
  alias Module.Types.Descr
  alias ElixirSense.Core.ExCkReader
  alias ElixirSense.Core.ModuleResolver
  alias ElixirSense.Core.State.VarInfo
  alias ElixirSense.Core.State.AttributeInfo
  alias ElixirSense.Core.State.SpecInfo

  # Persistent-term key for the memoized capabilities map.
  @capabilities_pt_key {__MODULE__, :capabilities}

  def available? do
    caps = cached_capabilities()
    Map.get(caps, :expr_basic, false) and Map.get(caps, :local_signature, false)
  end

  @doc """
  Returns true if adaptor is enabled via config and Module.Types is available.

  This combines the availability check with the configuration setting.

  ## Examples

      # With feature disabled (default)
      iex> ElixirTypes.enabled?()
      false

      # After enabling in config
      iex> Application.put_env(:elixir_sense, :use_elixir_types, true)
      iex> ElixirTypes.enabled?()  # if Module.Types is available
      true

  """
  def enabled? do
    Application.get_env(:elixir_sense, :use_elixir_types, false) and available?()
  end

  @doc """
  Returns true if native typing should be used for general expression typing.

  This requires the **expected-type** expression API (`Expr.of_expr/5`, Elixir
  1.19+). On 1.18 only the basic `of_expr/3` exists, which (lacking expected
  types) is generally no better than — and sometimes less precise than — the
  ElixirSense custom engine for plain expressions. So on 1.18 expression typing
  stays on the custom engine, while pattern-match refinement (`of_match`) and
  local-signature inference (`of_head`) still use the native backend.
  """
  def expr_typing_enabled? do
    enabled?() and available?(:expr)
  end

  @typedoc """
  A probed `Module.Types` capability.

  Capabilities are detected at runtime by probing the loaded Elixir's internal
  API surface (`function_exported?/3`), never by matching `System.version/0`,
  keeping the adaptor flexible across Elixir releases whose `Module.Types`
  internals change every minor version.
  """
  @type capability ::
          :expr
          | :expr_basic
          | :pattern_match
          | :head
          | :local_signature
          | :previous

  @doc """
  Probes the running Elixir's `Module.Types` capabilities.

  Returns a map from `t:capability/0` to a boolean. Cheap enough to call per
  operation, but callers may memoize. The mapping to Elixir releases (for
  reference only — dispatch must use the probes, not versions):

    * `:expr` — expected-type expression typing (`Expr.of_expr/5`), 1.19+
    * `:expr_basic` — any expression typing (`of_expr/5` or `/3`), 1.18+
    * `:pattern_match` — `of_match/7` (1.18), `/5` (1.19) or `/6` (1.20)
    * `:head` — `Pattern.of_head/8` (1.20) or `/7` (1.18/1.19)
    * `:local_signature` — local handler on the stack (`Module.Types.stack/7`), 1.18+
    * `:previous` — cross-clause `Pattern.init_previous/0`, 1.20+

  The map ALSO carries memoized internal-dispatch entries (cached in the same
  persistent_term):

    * `:expr_api` / `:pattern_api` — the dispatch VARIANT atoms
      (`:expected`/`:basic`/`:none`, resp. `:v18`/`:v19`/`:v20`/`:none`) read by
      the version-dispatched `call_of_*` helpers.
    * `:descr_gradual` / `:descr_disjoint` / `:descr_compatible` /
      `:descr_only_gradual` / `:descr_bitstring` / `:descr_fun_1` — booleans for
      the corresponding `Module.Types.Descr` private-API probes.
  """
  def capabilities do
    cached_capabilities()
  end

  # Expression-typer shape: 1.19+ has the expected-type `Expr.of_expr/5`; 1.18
  # has the basic `of_expr/3` (no expected type / expr-tracking args).
  defp expr_api do
    cond do
      loaded_exported?(Module.Types.Expr, :of_expr, 5) -> :expected
      loaded_exported?(Module.Types.Expr, :of_expr, 3) -> :basic
      true -> :none
    end
  end

  # Pattern API generation, keyed off the unambiguous `of_match` arity:
  # 1.18 `of_match/7`, 1.19 `of_match/5`, 1.20 `of_match/6`. (1.18 also exports
  # `/6` via a defaulted `guards` arg, so `/7` must be checked first.)
  defp pattern_api do
    cond do
      loaded_exported?(Module.Types.Pattern, :of_match, 7) -> :v18
      loaded_exported?(Module.Types.Pattern, :of_match, 5) -> :v19
      loaded_exported?(Module.Types.Pattern, :of_match, 6) -> :v20
      true -> :none
    end
  end

  @doc """
  Returns whether a specific `t:capability/0` is available in the running Elixir.
  """
  @spec available?(capability()) :: boolean()
  def available?(capability) when is_atom(capability) do
    Map.get(cached_capabilities(), capability, false)
  end

  defp loaded_exported?(module, fun, arity) do
    Code.ensure_loaded?(module) and function_exported?(module, fun, arity)
  end

  # Read the capabilities map from :persistent_term, computing/storing it on the
  # first call. Module exports can't change within a running VM, so caching for
  # the node lifetime is safe. DO NOT memoize `enabled?/0` here — its
  # Application.get_env read is toggled by tests at runtime.
  defp cached_capabilities do
    :persistent_term.get(@capabilities_pt_key, nil) || compute_and_cache_capabilities()
  end

  defp compute_and_cache_capabilities do
    expr_variant = expr_api()
    pattern_variant = pattern_api()

    caps = %{
      expr: expr_variant == :expected,
      expr_basic: expr_variant != :none,
      pattern_match: pattern_variant != :none,
      head:
        loaded_exported?(Module.Types.Pattern, :of_head, 8) or
          loaded_exported?(Module.Types.Pattern, :of_head, 7),
      local_signature: loaded_exported?(Module.Types, :stack, 7),
      previous: loaded_exported?(Module.Types.Pattern, :init_previous, 0),

      # Memoized dispatch variants read by the per-dispatch call_of_* helpers
      # instead of re-probing exports on every call.
      expr_api: expr_variant,
      pattern_api: pattern_variant,

      # Memoized boolean probes for the Descr private-API mirrors.
      descr_gradual: function_exported?(Module.Types.Descr, :gradual?, 1),
      descr_disjoint: function_exported?(Module.Types.Descr, :disjoint?, 2),
      descr_compatible: function_exported?(Module.Types.Descr, :compatible?, 2),
      descr_only_gradual: function_exported?(Module.Types.Descr, :only_gradual?, 1),
      descr_bitstring: function_exported?(Module.Types.Descr, :bitstring, 0),
      descr_fun_1: function_exported?(Module.Types.Descr, :fun, 1)
    }

    :persistent_term.put(@capabilities_pt_key, caps)
    caps
  end

  # Memoized dispatch-variant / boolean-probe readers (persistent_term lookup).
  defp cached_expr_api, do: Map.fetch!(cached_capabilities(), :expr_api)
  defp cached_pattern_api, do: Map.fetch!(cached_capabilities(), :pattern_api)
  defp cached_cap(key), do: Map.fetch!(cached_capabilities(), key)

  # --- Version-dispatched Module.Types.Pattern calls --------------------------
  # The pattern API changed shape across releases; these helpers dispatch on the
  # actually-exported arity via `apply/3` so the adapter works on 1.18/1.19/1.20.

  # of_expr: 1.19+ `of_expr/5` (expected + expr-tracking); 1.18 `of_expr/3`
  # (no expected type). Both return `{type, context}`; 1.18 drops the extra args.
  defp call_of_expr(ast, expected, expr, stack, context) do
    case cached_expr_api() do
      # credo:disable-for-next-line Credo.Check.Refactor.Apply
      :expected -> apply(Module.Types.Expr, :of_expr, [ast, expected, expr, stack, context])
      # credo:disable-for-next-line Credo.Check.Refactor.Apply
      :basic -> apply(Module.Types.Expr, :of_expr, [ast, stack, context])
      :none -> {Descr.dynamic(), context}
    end
  end

  # of_match (all return `{type, context}`):
  #   * 1.20 `of_match/6` — (pattern, expected_fun, expr, meta, stack, context)
  #   * 1.19 `of_match/5` — (pattern, expected_fun, expr, stack, context)
  #   * 1.18 `of_match/7` — (pattern, guards, expected, expr, tag, stack, context)
  # 1.19/1.20 take a lazy `expected_fun`; 1.18 a precomputed `expected` descr.
  defp call_of_match(pattern_ast, value_ast, expected_descr, full_match, stack, context) do
    case cached_pattern_api() do
      :v20 ->
        expected_fun = of_match_expected_fun(value_ast, expected_descr, full_match, stack)

        # credo:disable-for-next-line Credo.Check.Refactor.Apply
        apply(Module.Types.Pattern, :of_match, [
          pattern_ast,
          expected_fun,
          full_match,
          pattern_meta(pattern_ast),
          stack,
          context
        ])

      :v19 ->
        expected_fun = of_match_expected_fun(value_ast, expected_descr, full_match, stack)

        # credo:disable-for-next-line Credo.Check.Refactor.Apply
        apply(Module.Types.Pattern, :of_match, [
          pattern_ast,
          expected_fun,
          full_match,
          stack,
          context
        ])

      :v18 ->
        # 1.18 `of_match/7` expects the tag to carry the expected descr
        # (`{:match, expected}`), not the full match AST (expr.ex:135).
        tag = {:match, expected_descr}

        # credo:disable-for-next-line Credo.Check.Refactor.Apply
        apply(Module.Types.Pattern, :of_match, [
          pattern_ast,
          [],
          expected_descr,
          full_match,
          tag,
          stack,
          context
        ])

      :none ->
        throw({:elixir_types_unsupported, :of_match})
    end
  end

  defp of_match_expected_fun(value_ast, expected_descr, full_match, stack) do
    fn _pattern_type, ctx -> call_of_expr(value_ast, expected_descr, full_match, stack, ctx) end
  end

  # of_head + per-arg domain. Returns `{arg_types, clause_ctx}` where `arg_types`
  # is the list of inferred argument types. The pieces differ by version:
  #   * 1.20 `of_head/8` -> {trees, …}; arg types via `of_domain(trees, stack, ctx)`
  #   * 1.19 `of_head/7` -> {trees, ctx}; arg types via `of_domain(trees, expected, ctx)`
  #   * 1.18 `of_head/7` -> {types, ctx}; `types` ARE the arg types (no of_domain)
  defp call_of_head(args, guards, expected, meta, stack, context, fun_arity) do
    case cached_pattern_api() do
      :v20 ->
        # credo:disable-for-next-line Credo.Check.Refactor.Apply
        previous = apply(Module.Types.Pattern, :init_previous, [])
        # of_head reads this as `{{:def, kind, fun, types}, args, guards}`
        # (pattern.ex:1519/1637); the `fun` element is used in diagnostics.
        info = {{:def, :def, fun_arity, expected}, args, guards}

        # credo:disable-for-next-line Credo.Check.Refactor.Apply
        result_v20 =
          apply(Module.Types.Pattern, :of_head, [
            args,
            guards,
            expected,
            previous,
            info,
            meta,
            stack,
            context
          ])

        {trees, _precise?, _no_prev, _previous, clause_ctx} = result_v20

        {trees, clause_ctx, :trees}

      :v19 ->
        # credo:disable-for-next-line Credo.Check.Refactor.Apply
        result_v19 =
          apply(Module.Types.Pattern, :of_head, [
            args,
            guards,
            expected,
            {:infer, expected},
            meta,
            stack,
            context
          ])

        {trees, clause_ctx} = result_v19

        {trees, clause_ctx, :trees}

      :v18 ->
        # credo:disable-for-next-line Credo.Check.Refactor.Apply
        result_v18 =
          apply(Module.Types.Pattern, :of_head, [
            args,
            guards,
            expected,
            {:infer, expected},
            meta,
            stack,
            context
          ])

        {types, clause_ctx} = result_v18

        # 1.18 has no of_domain; of_head already returns the arg types.
        {types, clause_ctx, :types}

      :none ->
        throw({:elixir_types_unsupported, :of_head})
    end
  end

  # Compute the per-argument types from an of_head result, dispatched the same
  # way of_head was. `apply/3` avoids version-specific contract warnings (1.19's
  # of_domain/3 expects `expected` as 2nd arg; 1.20's expects `stack`).
  defp arg_types_from_head(head_result, :types, _expected, _stack, _clause_ctx) do
    head_result
  end

  defp arg_types_from_head(trees, :trees, expected, stack, clause_ctx) do
    case cached_pattern_api() do
      # credo:disable-for-next-line Credo.Check.Refactor.Apply
      :v20 -> apply(Module.Types.Pattern, :of_domain, [trees, stack, clause_ctx])
      # credo:disable-for-next-line Credo.Check.Refactor.Apply
      _ -> apply(Module.Types.Pattern, :of_domain, [trees, expected, clause_ctx])
    end
  end

  @doc """
  Creates a Module.Types stack for typing operations.
  """
  def init_stack(
        module \\ nil,
        function \\ nil,
        file \\ nil,
        mode \\ :dynamic,
        local_sigs_map \\ nil,
        metadata \\ nil
      ) do
    if available?() do
      local_handler =
        if local_sigs_map && map_size(local_sigs_map) > 0 do
          local_handler_from(local_sigs_map)
        else
          &__MODULE__.local_handler/4
        end

      stack =
        Module.Types.stack(
          mode,
          file || "nofile",
          module || ElixirSense.ElixirTypes,
          function || {:__info__, 1},
          :all,
          checker_cache(),
          local_handler
        )

      # Store metadata on stack so module_from_ast can resolve aliases
      if metadata != nil and is_map(stack) do
        Map.put(stack, :metadata, metadata)
      else
        stack
      end
    else
      nil
    end
  end

  @doc """
  Creates a Module.Types context for typing operations.

  Optionally accepts `variables`, a map describing known variables to seed
  the typing context with. This prevents crashes when typing variable ASTs
  and allows callers to provide best-effort types.

  Accepted `variables` formats (keys are `{name, version}`):
  - `%{{atom, non_neg_integer} => Descr.t()}`
  - `%{{atom, non_neg_integer} => :dynamic | :term}`
  - `%{{atom, non_neg_integer} => var_shape}` where `var_shape` is a minimal
    ElixirSense variable shape (e.g. `{:atom, atom}`, `{:map, keyword}`,
    `{:struct, keyword, module}`)
  """
  def init_context(variables \\ nil) do
    if available?() do
      base = Module.Types.context()

      case variables_to_context_vars(variables) do
        nil -> base
        vars_map when is_map(vars_map) -> %{base | vars: vars_map}
      end
    else
      nil
    end
  end

  @doc """
  Default local handler stub for Module.Types when no local signatures are available.

  Returns `false` to indicate no local type info. When local signatures are
  available, `local_handler_from/1` creates a closure-based handler instead.
  """
  def local_handler(_meta, _fun_arity, _stack, _context) do
    false
  end

  @doc """
  Types an expression using Module.Types.Expr.of_expr/5.

  Takes an AST node and returns a Module.Types descriptor representing
  the type of the expression.

  ## Parameters

  - `ast` - The AST node to type (must be valid Elixir AST)
  - `opts_or_module` - Either a keyword list of options or module context (defaults to [])

  ## Options

  - `:module` - Optional module context (defaults to nil)
  - `:function` - Optional function context (defaults to nil)
  - `:file` - Optional file context (defaults to nil)
  - `:mode` - Typing mode, :dynamic (default)
  - `:local_sigs_map` - Optional local signatures map
  - `:metadata` - Optional metadata
  - `:variables` - Optional map of variables with keys `{name, version}`

  ## Returns

  - `{:ok, descr}` - Success with Module.Types descriptor
  - `:error` - Failure (Module.Types unavailable, invalid AST, etc.)

  ## Examples

      # Type a literal
      # Returns {:ok, descr} where descr is an opaque Descr.t().
      # Use `to_shape/1` to get a stable ElixirSense shape from it.
      ElixirTypes.of_expr(42)
      #=> {:ok, descr}

      # Type a variable with a known type
      ElixirTypes.of_expr({:foo, [version: 0], nil},
        variables: %{{:foo, 0} => Descr.integer()})
      #=> {:ok, descr}

      # Type a list, then convert to a shape
      {:ok, descr} = ElixirTypes.of_expr([1, 2, 3])
      ElixirTypes.to_shape(descr)
      #=> {:list, {:integer, nil}}

      # Type a tuple (requires AST form), then convert to a shape
      {:ok, descr} = ElixirTypes.of_expr({:{}, [], [1, :ok]})
      ElixirTypes.to_shape(descr)
      #=> {:tuple, 2, [{:integer, nil}, {:atom, :ok}]}

  """
  def of_expr(ast, opts_or_module \\ [])

  def of_expr(ast, opts) when is_list(opts) do
    module = Keyword.get(opts, :module)
    function = Keyword.get(opts, :function)
    file = Keyword.get(opts, :file)
    mode = Keyword.get(opts, :mode, :dynamic)
    local_sigs_map = Keyword.get(opts, :local_sigs_map)
    metadata = Keyword.get(opts, :metadata)
    variables = Keyword.get(opts, :variables)

    of_expr_impl(ast, module, function, file, mode, local_sigs_map, metadata, variables)
  end

  def of_expr(ast, module) when is_atom(module) or is_nil(module) do
    of_expr_impl(ast, module, nil, nil, :dynamic, nil, nil, nil)
  end

  defp of_expr_impl(
         ast,
         module,
         function,
         file,
         mode,
         local_sigs_map,
         metadata,
         variables
       ) do
    cond do
      is_list(ast) and not proper_list_ast?(ast) ->
        :error

      is_tuple(ast) and tuple_size(ast) not in [2, 3] ->
        :error

      true ->
        # Public expression typing requires the expected-type API (1.19+); on
        # 1.18 callers fall back to the custom engine.
        if available?(:expr) do
          try do
            stack = init_stack(module, function, file, mode, local_sigs_map, metadata)
            # Module.Types requires every var to carry a :version; stamp any
            # unversioned vars so native typing doesn't raise.
            ast = ensure_body_var_versions(ast)
            auto_vars = variables_from_ast(ast)
            effective_vars = merge_variables(variables, auto_vars)
            context = init_context(effective_vars)

            if stack && context do
              {descr, _context} =
                call_of_expr(ast, Descr.term(), ast, stack, context)

              {:ok, descr}
            else
              :error
            end
          catch
            kind, payload ->
              Logger.warning(
                "Unable to infer type of #{inspect(ast)}: #{Exception.format(kind, payload, __STACKTRACE__)}"
              )

              :error
          end
        else
          :error
        end
    end
  end

  defp proper_list_ast?([]), do: true
  defp proper_list_ast?([_ | tail]), do: proper_list_ast?(tail)
  defp proper_list_ast?(_), do: false

  defp checker_cache do
    case Process.get(:elixir_sense_checker_cache) do
      {checker, table} = cache
      when is_pid(checker) and (is_reference(table) or is_integer(table)) ->
        # Validate liveness, not just shape: a dead checker process or torn-down
        # ETS table makes Apply.export -> fetch_export raise badarg, silently
        # stopping native typing. Start a fresh checker and replace the stale entry.
        if checker_alive?(checker, table) do
          cache
        else
          start_checker_cache()
        end

      _ ->
        start_checker_cache()
    end
  rescue
    _ -> nil
  end

  defp checker_alive?(checker, table) do
    Process.alive?(checker) and :ets.info(table) != :undefined
  rescue
    _ -> false
  end

  defp start_checker_cache do
    case Module.ParallelChecker.start_link() do
      {:ok, cache} ->
        Process.put(:elixir_sense_checker_cache, cache)
        cache

      _ ->
        nil
    end
  end

  def maybe_remote_call_sig(
        {{:., _, [target_ast, fun]}, _meta, args},
        metadata
      )
      when is_atom(fun) and is_list(args) do
    if enabled?() do
      case module_from_ast(target_ast, metadata) do
        {:ok, module} ->
          case ExCkReader.lookup_signature(module, fun, length(args)) do
            {:ok, %{sig: {sig_kind, _domain, _clauses} = sig}}
            when sig_kind in [:infer, :strong] ->
              {:ok, sig}

            _ ->
              :error
          end

        :error ->
          :error
      end
    else
      :error
    end
  end

  def maybe_remote_call_sig(_ast, _metadata), do: :error

  defp module_from_ast(atom, _metadata) when is_atom(atom) do
    {:ok, atom}
  end

  defp module_from_ast({:__MODULE__, _, _} = ast, metadata) when is_map(metadata) do
    # Pure AST->module resolution (current module) is delegated to ModuleResolver.
    case metadata_env(metadata) do
      env when is_map(env) -> ModuleResolver.resolve(ast, env)
      _ -> :error
    end
  end

  defp module_from_ast({:@, _, [{attr, _, _}]}, metadata)
       when is_atom(attr) and is_map(metadata) do
    case metadata_env(metadata) do
      %{attributes: attrs} when is_list(attrs) ->
        case Enum.find(attrs, &(&1.name == attr)) do
          %AttributeInfo{type: {:atom, module}} when is_atom(module) ->
            {:ok, module}

          %AttributeInfo{type: {:attribute, nested_attr}}
          when is_atom(nested_attr) ->
            module_from_ast({:@, [], [{nested_attr, [], nil}]}, metadata)

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  defp module_from_ast({var, _, context}, metadata)
       when is_atom(var) and is_atom(context) and is_map(metadata) do
    case metadata_env(metadata) do
      %{vars: vars} when is_list(vars) ->
        case Enum.find(vars, &(&1.name == var)) do
          %VarInfo{type: {:atom, module}} when is_atom(module) ->
            {:ok, module}

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  defp module_from_ast({{:., _, [base, nested]}, _, []}, metadata) when is_atom(nested) do
    case module_from_ast(base, metadata) do
      {:ok, module} when is_atom(module) -> {:ok, Module.concat(module, nested)}
      _ -> :error
    end
  end

  defp module_from_ast({:__aliases__, _, parts} = ast, metadata)
       when is_list(parts) and is_map(metadata) do
    # Pure AST->module (alias) resolution is delegated to ModuleResolver.
    case metadata_env(metadata) do
      env when is_map(env) -> ModuleResolver.resolve(ast, env)
      # No env: fall back to bare concat to preserve prior behavior.
      _ -> resolve_bare_alias(parts)
    end
  end

  defp module_from_ast(_ast, _metadata), do: :error

  # Mirrors the historical `resolve_alias(parts, non_map_metadata)` fallback that
  # simply concatenated the parts when no env was available.
  defp resolve_bare_alias(parts) do
    {:ok, Module.concat(parts)}
  rescue
    ArgumentError -> :error
  end

  defp metadata_env(metadata) do
    source = if is_map(metadata), do: metadata.cursor_env || metadata.closest_env, else: nil

    case source do
      {_, env} when is_map(env) -> env
      {_from, _to, env} when is_map(env) -> env
      _ -> nil
    end
  end

  @doc """
  Types a pattern match to refine variable types.

  Returns `{:ok, var_shapes, var_descrs}` where `var_shapes` is a map of
  `{var_name, version} => ElixirSense shape` and `var_descrs` is a map of
  `{var_name, version} => Descr.t()`. Returns `:error` when
  typing fails or Module.Types cannot be used.
  """
  def of_match(
        pattern_ast,
        expected_descr,
        match_ast,
        module \\ nil,
        function \\ nil,
        file \\ nil,
        mode \\ :dynamic,
        opts \\ []
      ) do
    if available?() do
      targets = targets_from_opts(opts)

      try do
        stack = init_stack(module, function, file, mode)

        if stack do
          # Stamp :version on unversioned vars and replace `@attr` with placeholder
          # vars before building full_match — native of_match/of_pattern raises on
          # unversioned vars and on `{:@, …}` nodes.
          {pattern_ast, value_ast, full_match} =
            normalize_match(
              pattern_ast |> ensure_body_var_versions() |> replace_module_attributes(),
              match_ast |> ensure_body_var_versions() |> replace_module_attributes()
            )

          # 1.20 dropped `stack.refine_vars`; Pattern.of_match/6 now refines every
          # variable the match references. Seed them all (auto from the AST plus
          # caller-provided types) so `refine_body_var` doesn't crash on a missing
          # version.
          auto_vars = variables_from_ast(full_match)
          effective_vars = merge_variables(Keyword.get(opts, :variables), auto_vars)
          context = init_context(effective_vars)

          expected_descr = expected_descr || Descr.term()

          # Only real match patterns can be typed by `Pattern.of_match`. Quoted/
          # macro code can carry non-pattern AST (calls, typespec operators) on
          # which native `of_pattern` raises; skip native typing for those.
          if native_typeable_pattern?(pattern_ast) do
            do_native_match(
              pattern_ast,
              value_ast,
              expected_descr,
              full_match,
              stack,
              context,
              targets
            )
          else
            :error
          end
        else
          :error
        end
      rescue
        _ -> :error
      catch
        kind, payload ->
          Logger.warning(
            "Unable to infer type of match: #{Exception.format(kind, payload, __STACKTRACE__)}\nPattern: #{inspect(pattern_ast)}\nMatch: #{inspect(match_ast)}"
          )

          :error
      end
    else
      :error
    end
  end

  defp do_native_match(
         pattern_ast,
         value_ast,
         expected_descr,
         full_match,
         stack,
         context,
         targets
       ) do
    case perform_enhanced_match(
           pattern_ast,
           value_ast,
           expected_descr,
           full_match,
           stack,
           context,
           targets
         ) do
      {:ok, var_shapes, var_descrs} ->
        {:ok, var_shapes, var_descrs}

      :error ->
        fallback_match(
          pattern_ast,
          value_ast,
          expected_descr,
          full_match,
          stack,
          context,
          targets
        )
    end
  end

  # AST node names (with list args) legitimate inside a match pattern; anything
  # else with list args is a call/operator `Pattern.of_pattern` can't handle.
  @native_pattern_forms [:{}, :%{}, :%, :<<>>, :^, :=, :|, :"::", :__aliases__, :when, :__block__]

  # True if `ast` is a real match pattern (no function/macro calls, no typespec
  # operators) and so safe to hand to native `Pattern.of_match`.
  defp native_typeable_pattern?(ast) do
    {_ast, safe?} =
      Macro.prewalk(ast, true, fn node, acc -> {node, acc and pattern_node_ok?(node)} end)

    safe?
  end

  # remote call `Mod.fun(...)` — never a pattern
  defp pattern_node_ok?({{:., _, _}, _, _}), do: false

  # `{form, meta, args}` with list args is a call/operator unless it is one of the
  # pattern special forms (a bare var's context is an atom, not a list).
  defp pattern_node_ok?({form, _meta, args}) when is_list(args), do: form in @native_pattern_forms

  defp pattern_node_ok?(_node), do: true

  # True if every node of a guard is something native `of_guard` can type. The
  # metadata expander rewrites Kernel guard BIFs to `:erlang`-qualified remote
  # calls, the only remotes a real compiled guard contains. An unexpanded user
  # `defguard`/record macro reaches us as a non-`:erlang` remote call on which
  # native `of_guard`/`of_head` raises. Guards legitimately contain local
  # calls/operators and `:erlang` remotes, so we reject only the crash classes:
  # non-`:erlang` remote calls and typespec operators.
  defp native_typeable_guard?(ast) do
    {_ast, safe?} =
      Macro.prewalk(ast, true, fn node, acc -> {node, acc and guard_node_ok?(node)} end)

    safe?
  end

  # `:erlang`-qualified guard BIFs are the expanded form of Kernel guards and are
  # safe.
  defp guard_node_ok?({{:., _, [:erlang, _fun]}, _, _}), do: true

  # any other remote call `Mod.fun(...)` — an unexpanded record/defguard macro;
  # of_guard can't type it.
  defp guard_node_ok?({{:., _, _}, _, _}), do: false

  # typespec operators are never valid in a real guard either.
  defp guard_node_ok?({form, _meta, args}) when is_list(args) and form in [:"::", :|], do: false

  defp guard_node_ok?(_node), do: true

  # Native-typeable only when every arg pattern AND every guard is native-typeable.
  # Guards are a list (one entry per `when` branch); `nil`/`[]` means no guard.
  defp native_typeable_clause?(args, guards) do
    Enum.all?(args, &native_typeable_pattern?/1) and
      Enum.all?(List.wrap(guards), &native_typeable_guard?/1)
  end

  defp perform_enhanced_match(
         pattern_ast,
         value_ast,
         expected_descr,
         full_match,
         stack,
         context,
         targets
       ) do
    try do
      value_type = call_of_expr(value_ast, Descr.term(), full_match, stack, context)

      refined_expected =
        case value_type do
          {type_descr, _} -> Descr.intersection(expected_descr, type_descr)
          _ -> expected_descr
        end

      case call_of_match(pattern_ast, value_ast, refined_expected, full_match, stack, context) do
        {_type, %{vars: vars_map} = out_ctx} ->
          var_shapes =
            extract_refined_var_shapes(
              vars_map,
              targets,
              pattern_ast,
              refined_expected,
              value_ast
            )

          var_descrs = vars_ctx_to_descrs(out_ctx)
          {:ok, var_shapes, var_descrs}

        _ ->
          :error
      end
    rescue
      e ->
        Logger.debug(
          "perform_enhanced_match failed: #{Exception.format(:error, e, __STACKTRACE__)}"
        )

        :error
    catch
      kind, payload ->
        Logger.debug(
          "perform_enhanced_match failed: #{Exception.format(kind, payload, __STACKTRACE__)}"
        )

        :error
    end
  end

  # Fallback to original pattern matching approach
  defp fallback_match(pattern_ast, value_ast, expected_descr, full_match, stack, context, targets) do
    try do
      {_type, %{vars: vars_map} = out_ctx} =
        call_of_match(pattern_ast, value_ast, expected_descr, full_match, stack, context)

      var_shapes =
        extract_refined_var_shapes(
          vars_map,
          targets,
          pattern_ast,
          expected_descr,
          value_ast
        )

      var_descrs = vars_ctx_to_descrs(out_ctx)
      {:ok, var_shapes, var_descrs}
    rescue
      e ->
        Logger.debug("fallback_match failed: #{Exception.format(:error, e, __STACKTRACE__)}")
        :error
    catch
      kind, payload ->
        Logger.debug("fallback_match failed: #{Exception.format(kind, payload, __STACKTRACE__)}")
        :error
    end
  end

  # Transform Module.Types context vars into a map of {name, version} => descr
  defp vars_ctx_to_descrs(%{vars: vars_map}) when is_map(vars_map) do
    Enum.into(vars_map, %{}, fn {version, %{name: name, type: descr}} ->
      {{name, version}, descr}
    end)
  end

  defp vars_ctx_to_descrs(_), do: %{}

  # Enhanced variable shape extraction with additional type refinement
  defp extract_refined_var_shapes(vars_map, targets, pattern_ast, expected_descr, value_ast) do
    # Native `Pattern.of_match` descriptors are AUTHORITATIVE; the AST-based
    # `apply_pattern_refinements` engine is a best-effort fallback that only fills
    # variables for which the native path produced nothing.
    base_shapes = extract_var_shapes(vars_map, targets)

    refined_shapes =
      apply_pattern_refinements(base_shapes, pattern_ast, expected_descr, value_ast)

    # Merge refinements UNDER native results (keeps every native key, adds only
    # native-less vars), then drop synthetic-version keys so AST refinements keyed
    # to a stamped version can't leak into the VarInfo-keyed consumer map.
    refined_shapes
    |> Map.merge(base_shapes)
    |> Map.reject(fn {{_name, version}, _shape} -> synthetic_version?(version) end)
    |> normalize_var_versions()
  end

  # Apply additional type refinements based on pattern structure
  defp apply_pattern_refinements(var_shapes, pattern_ast, expected_descr, value_ast) do
    case pattern_ast do
      # Struct pattern refinement
      {:%, _, [struct_ast, {:%{}, _, fields}]} ->
        refine_struct_pattern_vars(var_shapes, struct_ast, fields, expected_descr, value_ast)

      # Map pattern refinement
      {:%{}, _, fields} ->
        refine_map_pattern_vars(var_shapes, fields, expected_descr, value_ast)

      # Tuple pattern refinement
      {:{}, _, elements} ->
        refine_tuple_pattern_vars(var_shapes, elements, expected_descr, value_ast)

      # 2-element tuple (Elixir represents {a, b} without :{} wrapper)
      {a, b} ->
        refine_tuple_pattern_vars(var_shapes, [a, b], expected_descr, value_ast)

      # Match operator pattern: `{:ok, val} = result`
      {:=, _, [left, right]} ->
        left_refinements =
          apply_pattern_refinements(var_shapes, left, expected_descr, value_ast)

        right_refinements =
          apply_pattern_refinements(var_shapes, right, expected_descr, value_ast)

        Map.merge(left_refinements, right_refinements)

      # Binary pattern: variables in binary segments get types from segment spec
      {:<<>>, _, parts} ->
        refine_binary_pattern_vars(var_shapes, parts)

      # List pattern refinement
      list when is_list(list) ->
        refine_list_pattern_vars(var_shapes, list, expected_descr, value_ast)

      _ ->
        %{}
    end
  end

  # Refine variables in struct patterns
  defp refine_struct_pattern_vars(var_shapes, _struct_ast, fields, expected_descr, value_ast) do
    # The struct module variable (the `var` in `%var{...}`) is deliberately NOT
    # refined here: forcing `{:atom, nil}` for a native-less var only invents an
    # imprecise type. Leave it unknown so it falls through to structural typing.
    refine_map_pattern_vars(var_shapes, fields, expected_descr, value_ast)
  end

  # Refine variables in map patterns
  defp refine_map_pattern_vars(var_shapes, fields, _expected_descr, value_ast) do
    value_field_map = map_field_map(shape_from_ast(value_ast))

    Enum.reduce(fields, %{}, fn
      {key_ast, var_ast}, acc ->
        case extract_var_from_ast(var_ast) do
          nil ->
            key_literal = literal_from_key_ast(key_ast)

            if key_literal != nil && Map.has_key?(value_field_map, key_literal) do
              field_value_ast = get_field_value_ast(value_ast, key_literal)

              nested_refinements =
                apply_pattern_refinements(var_shapes, var_ast, nil, field_value_ast)

              Map.merge(acc, nested_refinements)
            else
              acc
            end

          var_key ->
            key_literal = literal_from_key_ast(key_ast)

            field_shape =
              if key_literal != nil && Map.has_key?(value_field_map, key_literal) do
                Map.get(value_field_map, key_literal)
              else
                Map.get(var_shapes, var_key)
              end

            if field_shape do
              Map.put(acc, var_key, field_shape)
            else
              acc
            end
        end

      _other, acc ->
        acc
    end)
  end

  # Refine variables in tuple patterns
  defp refine_tuple_pattern_vars(var_shapes, elements, _expected_descr, value_ast) do
    tuple_shapes =
      case shape_from_ast(value_ast) do
        {:tuple, size, shapes} when size == length(elements) ->
          Enum.map(shapes, &generalize_shape/1)

        _ ->
          Enum.map(elements, fn element_ast ->
            case extract_var_from_ast(element_ast) do
              nil -> nil
              var_key -> Map.get(var_shapes, var_key)
            end
          end)
      end

    elements
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {element_ast, index}, acc ->
      case extract_var_from_ast(element_ast) do
        nil ->
          acc

        var_key ->
          element_shape = Enum.at(tuple_shapes, index)

          if element_shape do
            Map.put(acc, var_key, element_shape)
          else
            acc
          end
      end
    end)
  end

  # Refine variables in list patterns
  defp refine_list_pattern_vars(var_shapes, list, _expected_descr, value_ast) do
    value_list = if is_list(value_ast), do: value_ast, else: nil

    case list do
      [{:|, _, [head_ast, tail_ast]}] ->
        element_shape =
          case value_list do
            [head | _] -> generalize_shape(shape_from_ast(head))
            _ -> Map.get(var_shapes, extract_var_from_ast(head_ast))
          end

        head_refinement =
          case extract_var_from_ast(head_ast) do
            nil -> %{}
            var_key when element_shape != nil -> %{var_key => element_shape}
            _ -> %{}
          end

        tail_refinement =
          case extract_var_from_ast(tail_ast) do
            nil ->
              %{}

            var_key ->
              tail_shape = list_tail_shape(value_list, element_shape)

              if tail_shape do
                %{var_key => tail_shape}
              else
                %{}
              end
          end

        Map.merge(head_refinement, tail_refinement)

      elements when is_list(elements) ->
        # Fixed-length list pattern `[a, b, c]`: each pattern element matches the
        # value element at the SAME position. Refine a var only when we know its
        # own positional value; otherwise leave it unknown.
        elements
        |> Enum.with_index()
        |> Enum.reduce(%{}, fn {element_ast, index}, acc ->
          case extract_var_from_ast(element_ast) do
            nil ->
              acc

            var_key ->
              element_shape =
                case value_list do
                  list when is_list(list) and index < length(list) ->
                    generalize_shape(shape_from_ast(Enum.at(list, index)))

                  _ ->
                    nil
                end

              if element_shape do
                Map.put(acc, var_key, element_shape)
              else
                acc
              end
          end
        end)

      _ ->
        %{}
    end
  end

  # Refine variables in binary pattern segments
  defp refine_binary_pattern_vars(_var_shapes, segments) do
    Enum.reduce(segments, %{}, fn
      {:"::", _, [var_ast, type_spec]}, acc ->
        case extract_var_from_ast(var_ast) do
          nil ->
            acc

          var_key ->
            case binary_segment_shape(type_spec) do
              nil -> acc
              segment_shape -> Map.put(acc, var_key, segment_shape)
            end
        end

      _, acc ->
        acc
    end)
  end

  defp binary_segment_shape({:-, _, [left, _right]}), do: binary_segment_shape(left)
  defp binary_segment_shape({:binary, _, _}), do: {:binary, nil}
  defp binary_segment_shape({:bytes, _, _}), do: {:binary, nil}
  defp binary_segment_shape({:bitstring, _, _}), do: {:binary, nil}
  defp binary_segment_shape({:bits, _, _}), do: {:binary, nil}
  defp binary_segment_shape({:utf8, _, _}), do: {:integer, nil}
  defp binary_segment_shape({:utf16, _, _}), do: {:integer, nil}
  defp binary_segment_shape({:utf32, _, _}), do: {:integer, nil}
  defp binary_segment_shape({:integer, _, _}), do: {:integer, nil}
  defp binary_segment_shape({:float, _, _}), do: {:float, nil}
  # Unknown segment spec: leave the var unknown (nil) rather than guess.
  defp binary_segment_shape(_), do: nil

  # Extract variable information from AST node
  defp extract_var_from_ast({var_name, meta, nil}) when is_atom(var_name) do
    case Keyword.get(meta, :version) do
      nil -> nil
      version -> {var_name, version}
    end
  end

  defp extract_var_from_ast(_), do: nil

  # Extract the AST value for a specific field from a map value AST
  defp get_field_value_ast(value_ast, field_key) do
    case value_ast do
      {:%{}, _, fields} ->
        Enum.find_value(fields, fn
          {key_ast, value_ast} ->
            if literal_from_key_ast(key_ast) == field_key do
              value_ast
            else
              nil
            end

          _ ->
            nil
        end)

      {:%, _, [_, {:%{}, _, fields}]} ->
        get_field_value_ast({:%{}, [], fields}, field_key)

      _ ->
        nil
    end
  end

  defp normalize_var_versions(vars_map) do
    # For each variable name, if version 1 is missing, use the lowest available
    # version. Group by name first to avoid non-deterministic map iteration order.
    vars_map
    |> Enum.filter(fn {{var, version}, _} ->
      is_atom(var) and is_integer(version) and version > 1
    end)
    |> Enum.group_by(fn {{var, _version}, _shape} -> var end)
    |> Enum.reduce(vars_map, fn {var, entries}, acc ->
      primary_key = {var, 1}

      if Map.has_key?(acc, primary_key) do
        acc
      else
        {{_var, _version}, shape} = Enum.min_by(entries, fn {{_v, version}, _s} -> version end)
        Map.put(acc, primary_key, shape)
      end
    end)
  end

  defp map_field_map({:map, fields, _}) when is_list(fields) do
    fields
    |> Enum.into(%{}, fn {key, value} -> {key, value} end)
  end

  defp map_field_map({:struct, fields, {:atom, module}, _}) do
    fields
    |> Enum.reject(fn {key, _} -> key == :__struct__ end)
    |> Enum.into(%{}, fn {key, value} -> {key, value} end)
    |> Map.put(:__struct__, {:atom, module})
  end

  defp map_field_map(_), do: %{}

  defp literal_from_key_ast({:__block__, _, [value]}), do: value
  defp literal_from_key_ast({:<<>>, _, parts}) when is_list(parts), do: IO.iodata_to_binary(parts)
  defp literal_from_key_ast(atom) when is_atom(atom), do: atom
  defp literal_from_key_ast(_), do: nil

  defp shape_from_ast(ast) when is_integer(ast), do: {:integer, ast}
  defp shape_from_ast(ast) when is_float(ast), do: {:float, ast}
  defp shape_from_ast(ast) when is_binary(ast), do: {:binary, ast}
  defp shape_from_ast(ast) when is_atom(ast), do: {:atom, ast}

  defp shape_from_ast({:%{}, _, fields}) do
    mapped =
      fields
      |> Enum.flat_map(fn
        {key, value} ->
          case literal_from_key_ast(key) do
            nil -> []
            literal_key -> [{literal_key, shape_from_ast(value)}]
          end

        _ ->
          []
      end)

    {:map, mapped, nil}
  end

  defp shape_from_ast({:%, _, [struct_ast, {:%{}, _, fields}]}) do
    module =
      case struct_ast do
        {:__aliases__, _, parts} -> Module.concat(parts)
        atom when is_atom(atom) -> atom
        _ -> nil
      end

    field_shapes =
      fields
      |> Enum.flat_map(fn
        {:__struct__, _} ->
          []

        {key, value} ->
          case literal_from_key_ast(key) do
            nil -> []
            literal_key -> [{literal_key, shape_from_ast(value)}]
          end

        _ ->
          []
      end)

    if module do
      {:struct, field_shapes, {:atom, module}, nil}
    else
      {:map, field_shapes, nil}
    end
  end

  defp shape_from_ast({:{}, _, elements}) when is_list(elements) do
    {:tuple, length(elements), Enum.map(elements, &shape_from_ast/1)}
  end

  defp shape_from_ast(list) when is_list(list) do
    case list do
      [] ->
        {:list, :empty}

      _ ->
        element_shapes = Enum.map(list, &shape_from_ast/1)
        merged = first_generalizable_shape(element_shapes)
        {:list, merged}
    end
  end

  defp shape_from_ast(_), do: nil

  # First-wins: the generalized shape of the first element that has one (skipping
  # `nil`/un-generalizable elements). Returns nil when no element generalizes.
  defp first_generalizable_shape(shapes) do
    Enum.find_value(shapes, &generalize_shape/1)
  end

  defp generalize_shape(nil), do: nil
  defp generalize_shape({:integer, _}), do: {:integer, nil}
  defp generalize_shape({:float, _}), do: {:float, nil}
  defp generalize_shape({:binary, value}), do: {:binary, value}
  defp generalize_shape({:atom, value}), do: {:atom, value}

  defp generalize_shape({:tuple, size, elements}) do
    {:tuple, size, Enum.map(elements, &generalize_shape/1)}
  end

  defp generalize_shape({:map, fields, meta}) do
    {:map, Enum.map(fields, fn {key, value} -> {key, generalize_shape(value)} end), meta}
  end

  defp generalize_shape({:struct, fields, type, meta}) do
    {:struct, Enum.map(fields, fn {key, value} -> {key, generalize_shape(value)} end), type, meta}
  end

  defp generalize_shape({:list, :empty}), do: {:list, :empty}
  defp generalize_shape({:list, element}), do: {:list, generalize_shape(element)}

  defp generalize_shape({:union, variants}) do
    {:union, Enum.map(variants, fn variant -> generalize_shape(variant) end)}
  end

  defp generalize_shape({:fun, args, return}) when is_list(args) do
    {:fun, Enum.map(args, &generalize_shape/1), generalize_shape(return)}
  end

  defp generalize_shape({:fun_clauses, clauses}) when is_list(clauses) do
    {:fun_clauses,
     Enum.map(clauses, fn {args, return} ->
       {Enum.map(args, &generalize_shape/1), generalize_shape(return)}
     end)}
  end

  defp generalize_shape({:optional, inner}), do: {:optional, generalize_shape(inner)}

  defp generalize_shape(shape), do: shape

  defp list_tail_shape(nil, element_shape) do
    if element_shape do
      {:list, element_shape}
    else
      nil
    end
  end

  defp list_tail_shape([], _element_shape), do: {:list, :empty}

  defp list_tail_shape([_head | tail], element_shape) do
    tail_shape = if element_shape, do: {:list, element_shape}, else: {:list, :empty}

    case tail do
      [] -> {:list, :empty}
      _ -> {:union, [tail_shape, {:list, :empty}]}
    end
  end

  @doc """
  Converts a Module.Types.Descr to ElixirSense shape format.

  Conservative conversion - only returns shapes for clearly identifiable types.
  Returns nil for complex or uncertain types to avoid false precision.

  """
  def to_shape(descr) do
    to_shape_eager(descr)
  end

  @doc """
  Renders a `Module.Types.Descr` to the compiler's own type string.

  Delegates to `Descr.to_quoted_string/2` (with
  `collapse_structs: true`, matching how the compiler formats warning types) so
  the presentation layer can show types exactly as the compiler would, rather
  than going through the lossy `to_shape/1 -> custom renderer` path.

  Returns `{:ok, String.t()}` on success, or `:error` when the native backend is
  unavailable or rendering fails for any reason (everything is rescued/caught).
  """
  @spec descr_to_string(term()) :: {:ok, String.t()} | :error
  def descr_to_string(descr), do: descr_to_string(descr, collapse_structs: true)

  @spec descr_to_string(term(), keyword()) :: {:ok, String.t()} | :error
  def descr_to_string(descr, opts) when is_list(opts) do
    if available?() and function_exported?(Module.Types.Descr, :to_quoted_string, 2) do
      try do
        opts = Keyword.put_new(opts, :collapse_structs, true)
        # credo:disable-for-next-line Credo.Check.Refactor.Apply
        string = apply(Module.Types.Descr, :to_quoted_string, [descr, opts])

        if is_binary(string) do
          {:ok, string}
        else
          :error
        end
      rescue
        _ -> :error
      catch
        _, _ -> :error
      end
    else
      :error
    end
  end

  # Extract AST meta from a pattern node, defaulting to [] for literals.
  defp pattern_meta({_form, meta, _args}) when is_list(meta), do: meta
  defp pattern_meta(_), do: []

  # Convert a Module.Types.Descr to an ElixirSense shape via Descr.to_quoted/1.
  defp to_shape_eager(descr) do
    if available?() and is_map(descr) do
      try do
        # not_set() has an :optional key that to_quoted doesn't handle.
        if is_map_key(descr, :optional) do
          optional_descr = Map.delete(descr, :optional)

          if map_size(optional_descr) > 0 do
            {:optional, to_shape(optional_descr)}
          else
            :not_set
          end
        else
          descr
          |> Descr.to_quoted()
          |> quoted_to_shape()
        end
      rescue
        e ->
          Logger.debug(
            "to_shape conversion failed: #{Exception.format(:error, e, __STACKTRACE__)}"
          )

          nil
      catch
        kind, payload ->
          Logger.debug(
            "to_shape conversion failed: #{Exception.format(kind, payload, __STACKTRACE__)}"
          )

          nil
      end
    else
      nil
    end
  end

  # Convert Descr.to_quoted/1 AST output into ElixirSense shapes.
  defp quoted_to_shape(quoted) do
    case quoted do
      {:term, [], []} ->
        nil

      {:none, [], []} ->
        :none

      # dynamic() — the gradual top carries no information, so at the shape
      # boundary it is "unknown" (nil); shapes feed the Binding algebra, which has
      # no gradual marker. Compiler-fidelity display is via `descr_to_string/1`.
      {:dynamic, [], []} ->
        nil

      # dynamic(inner) — unwrap to the inner shape.
      {:dynamic, [], [inner]} ->
        quoted_to_shape(inner)

      {:integer, [], []} ->
        {:integer, nil}

      {:float, [], []} ->
        {:float, nil}

      {:binary, [], []} ->
        {:binary, nil}

      {:atom, [], []} ->
        :atom

      {:pid, [], []} ->
        :pid

      {:port, [], []} ->
        :port

      {:reference, [], []} ->
        :reference

      {:empty_list, [], []} ->
        :empty_list

      # closed `%{}` / empty_map() — distinct from the map top type.
      {:empty_map, [], []} ->
        :empty_map

      # boolean() — dedicated shape; the compiler prints `boolean()` rather than
      # decomposing it to `false | true`.
      {:boolean, [], []} ->
        :boolean

      {:bitstring, [], []} ->
        :bitstring

      {:non_struct_map, [], []} ->
        :non_struct_map

      {:not_set, [], []} ->
        :not_set

      # map() — the map top type with no specific fields
      {:map, [], []} ->
        {:map, [], nil}

      {:fun, [], []} ->
        :fun

      # Function with signature: {:__block__, [], [[{:->, [], [[args...], return]}]]}
      {:__block__, [], [clauses]} when is_list(clauses) ->
        fun_clauses =
          for {:->, [], [args, return]} <- clauses do
            {Enum.map(args, &quoted_to_shape/1), quoted_to_shape(return)}
          end

        case fun_clauses do
          [{arg_shapes, return_shape}] ->
            # All args :none and return nil (term) ⇒ just {:fun, arity}
            if Enum.all?(arg_shapes, &(&1 == :none)) and return_shape == nil do
              {:fun, length(arg_shapes)}
            else
              {:fun, arg_shapes, return_shape}
            end

          [_ | _] ->
            {:fun_clauses, fun_clauses}

          [] ->
            quoted_to_shape_block(clauses)
        end

      {:list, [], [elem]} ->
        {:list, quoted_to_shape(elem)}

      {:non_empty_list, [], [elem]} ->
        {:nonempty_list, quoted_to_shape(elem)}

      # non_empty_list with explicit tail. Build `{:nonempty_list, elem, tail}`
      # when BOTH sides convert; a `nil` means "unknown" (not "absent") so we
      # cannot soundly drop it — degrade the whole shape to nil instead.
      {:non_empty_list, [], [elem, tail]} ->
        elem_shape = quoted_to_shape(elem)
        tail_shape = quoted_to_shape(tail)

        if is_nil(elem_shape) or is_nil(tail_shape) do
          nil
        else
          {:nonempty_list, elem_shape, tail_shape}
        end

      # Tuple types. The top type `tuple()` quotes to a single open marker
      # `{:{}, [], [{:..., [], nil}]}` → `:tuple`. A trailing `{:..., [], nil}`
      # marks an open tuple → `{:tuple_open, shapes}`; else `{:tuple, n, shapes}`.
      {:{}, [], [{:..., [], nil}]} ->
        :tuple

      {:{}, [], elems} when is_list(elems) ->
        {open?, elems} = strip_open_tuple_marker(elems)
        shapes = Enum.map(elems, &quoted_to_shape/1)

        if open? do
          {:tuple_open, shapes}
        else
          {:tuple, length(shapes), shapes}
        end

      # Map types (may contain structs)
      {:%{}, [], fields} when is_list(fields) ->
        quoted_map_to_shape(fields)

      # Union types. If ANY member is unconvertible, degrade the whole union to
      # nil: a dropped member would make the displayed type narrower than truth.
      {:or, [], [_left, _right]} = union ->
        members = flatten_quoted_union(union)
        shapes = Enum.map(members, &quoted_to_shape/1)

        cond do
          Enum.any?(shapes, &is_nil/1) -> nil
          match?([_single], shapes) -> hd(shapes)
          true -> {:union, shapes}
        end

      # Intersection types — function intersections become multi-clause funs.
      # Non-function intersections degrade to nil: picking one branch would make
      # the displayed type broader than reality.
      {:and, [], [_left, _right]} = intersection ->
        case extract_fun_clauses_from_intersection(intersection) do
          [_ | _] = fun_clauses -> {:fun_clauses, fun_clauses}
          [] -> nil
        end

      # Negation types — unrepresentable as a shape.
      {:not, [], [_inner]} ->
        nil

      # Struct quoted form `%Struct{...}` (descr.ex map_literal_to_quoted emits
      # this for loaded, complete structs).
      {:%, _, [module_alias, {:%{}, _, fields}]} ->
        quoted_struct_to_shape(module_alias, fields)

      # Optional map field value
      {:if_set, [], [inner]} ->
        {:optional, quoted_to_shape(inner)}

      {:__block__, [], [atom]} when is_atom(atom) ->
        {:atom, atom}

      {:__block__, [], [integer]} when is_integer(integer) ->
        {:integer, integer}

      {:__aliases__, [], parts} when is_list(parts) ->
        {:atom, Module.concat(parts)}

      # Open map/tuple marker
      {:..., [], _} ->
        nil

      atom when is_atom(atom) ->
        {:atom, atom}

      integer when is_integer(integer) ->
        {:integer, integer}

      _ ->
        nil
    end
  end

  # Handle __block__ wrapping a list that isn't function clauses
  defp quoted_to_shape_block([value]) when is_atom(value), do: {:atom, value}
  defp quoted_to_shape_block([value]) when is_integer(value), do: {:integer, value}
  defp quoted_to_shape_block(_), do: nil

  # Split off a trailing open-tuple marker `{:..., [], nil}`. Returns
  # `{open?, element_quoteds}`.
  defp strip_open_tuple_marker(elems) do
    case List.last(elems) do
      {:..., [], _} -> {true, Enum.drop(elems, -1)}
      _ -> {false, elems}
    end
  end

  # Convert a struct quoted form `%Mod{field: val, ...}` into a struct shape.
  defp quoted_struct_to_shape(module_alias, fields) do
    module =
      case module_alias do
        {:__aliases__, _, parts} when is_list(parts) -> Module.concat(parts)
        atom when is_atom(atom) -> atom
        _ -> nil
      end

    if module do
      field_shapes =
        for {key_quoted, value_quoted} <- fields,
            not match?({:..., [], _}, key_quoted),
            key = extract_quoted_map_key(key_quoted),
            key != nil and key != :__struct__,
            do: {key, quoted_to_shape(value_quoted)}

      {:struct, field_shapes, {:atom, module}, nil}
    else
      nil
    end
  end

  # Extract function clauses from an intersection of function types.
  # fun_from_non_overlapping_clauses produces {:and, [], [fun1, fun2]}
  defp extract_fun_clauses_from_intersection({:and, [], [left, right]}) do
    extract_fun_clauses_from_intersection(left) ++
      extract_fun_clauses_from_intersection(right)
  end

  defp extract_fun_clauses_from_intersection({:__block__, [], [clauses]})
       when is_list(clauses) do
    for {:->, [], [args, return]} <- clauses do
      {Enum.map(args, &quoted_to_shape/1), quoted_to_shape(return)}
    end
  end

  defp extract_fun_clauses_from_intersection(_), do: []

  # Convert quoted map fields to either a struct or map shape.
  defp quoted_map_to_shape(fields) do
    # Non-atom (domain) keys like `integer() => binary()` are preserved as
    # `{{:domain, key_shape}, value}`. The open-map marker `{:..., [], nil}` is a
    # bare 3-tuple (skipped by the `{key, value}` generator); its presence is the
    # only signal distinguishing an open map from a closed one, recorded in the
    # shape tail (`:open` vs `nil`) so coercion builds the right descr.
    kv_pairs =
      for {key_quoted, value_quoted} <- fields do
        case extract_quoted_map_key(key_quoted) do
          nil -> {{:domain, quoted_to_shape(key_quoted)}, value_quoted}
          atom_key -> {atom_key, value_quoted}
        end
      end

    # Tail per the shape grammar: `:open` (has the `{:..., [], nil}` marker),
    # `:closed` (literal-complete, all atom keys known), or `nil` (closedness
    # unassertable — domain keys encode openness without the `:...` marker, so
    # closing them would wrongly exclude maps the domain admits).
    has_domain_key? = Enum.any?(kv_pairs, &match?({{:domain, _}, _}, &1))

    map_tail =
      cond do
        open_map_marker?(fields) -> :open
        has_domain_key? -> nil
        true -> :closed
      end

    struct_module =
      Enum.find_value(kv_pairs, fn
        {:__struct__, value_quoted} ->
          case quoted_to_shape(value_quoted) do
            {:atom, module} when is_atom(module) and module != nil -> module
            _ -> nil
          end

        _ ->
          nil
      end)

    if struct_module do
      field_shapes =
        for {key, value_quoted} <- kv_pairs,
            key != :__struct__,
            do: {key, quoted_to_shape(value_quoted)}

      # Struct coercion derives closed/open from the loaded defstruct, not the
      # shape tail, so the struct tail stays `nil` regardless.
      {:struct, field_shapes, {:atom, struct_module}, nil}
    else
      field_shapes =
        for {key, value_quoted} <- kv_pairs,
            do: {key, quoted_to_shape(value_quoted)}

      {:map, field_shapes, map_tail}
    end
  end

  # The open-map quoted marker is the bare element `{:..., [], nil}` — present
  # only when the descr has additional unknown keys beyond the listed ones.
  defp open_map_marker?(fields) do
    Enum.any?(fields, &match?({:..., [], nil}, &1))
  end

  # Extract atom key from quoted map key representation.
  defp extract_quoted_map_key({:__block__, [format: :keyword], [key]}) when is_atom(key), do: key
  defp extract_quoted_map_key({:__block__, _, [key]}) when is_atom(key), do: key
  defp extract_quoted_map_key(key) when is_atom(key), do: key
  defp extract_quoted_map_key(_), do: nil

  # Flatten nested {:or, [], [left, right]} into a flat list.
  defp flatten_quoted_union({:or, [], [left, right]}) do
    flatten_quoted_union(left) ++ flatten_quoted_union(right)
  end

  defp flatten_quoted_union(other), do: [other]

  @doc """
  Merges two shapes, preferring the more specific one.
  """
  def merge_shapes(existing, new) do
    case {existing, new} do
      {:none, _} ->
        :none

      {nil, new} ->
        new

      {existing, nil} ->
        existing

      {existing, :none} ->
        existing

      # Prefer literal integers over generic
      {{:integer, value}, {:integer, nil}} when value != nil ->
        existing

      {{:integer, nil}, {:integer, value}} when value != nil ->
        new

      # Prefer more specific list types
      {{:list, type1}, {:list, type2}} when type1 != nil and type2 == nil ->
        existing

      {{:list, type1}, {:list, type2}} when type1 == nil and type2 != nil ->
        new

      # Element-wise merge for tuples with same arity
      {{:tuple, arity, elems1}, {:tuple, arity, elems2}} ->
        merged_elems =
          elems1
          |> Enum.zip(elems2)
          |> Enum.map(fn {elem1, elem2} -> merge_shapes(elem1, elem2) end)

        {:tuple, arity, merged_elems}

      # Field-wise merge for maps
      {{:map, fields1, nil}, {:map, fields2, nil}} ->
        merged_fields = merge_map_fields(fields1, fields2)
        {:map, merged_fields, nil}

      # List element type merging
      {{:list, elem_type1}, {:list, elem_type2}} ->
        merged_elem_type = merge_shapes(elem_type1, elem_type2)
        {:list, merged_elem_type}

      # Field-wise merge for structs with same type
      {{:struct, fields1, module, updated1}, {:struct, fields2, module, updated2}} ->
        merged_fields = merge_map_fields(fields1, fields2)
        merged_updated = merge_shapes(updated1, updated2)
        {:struct, merged_fields, module, merged_updated}

      # Default: keep existing to avoid surprises
      _ ->
        existing
    end
  end

  defp merge_map_fields(fields1, fields2) do
    map1 = Map.new(fields1)
    map2 = Map.new(fields2)
    all_keys = (Map.keys(map1) ++ Map.keys(map2)) |> Enum.uniq()

    merged_map =
      Enum.reduce(all_keys, %{}, fn key, acc ->
        case {Map.get(map1, key), Map.get(map2, key)} do
          {nil, value2} ->
            Map.put(acc, key, value2)

          {value1, nil} ->
            Map.put(acc, key, value1)

          {value1, value2} ->
            merged_value = merge_shapes(value1, value2)
            Map.put(acc, key, merged_value)
        end
      end)

    Enum.map(merged_map, fn {key, value} -> {key, value} end)
  end

  # -- Variables seeding helpers ------------------------------------------------

  # Convert user-provided variables map into Module.Types context vars map.
  # Input keys are {name, version}; output keys are version only.
  defp variables_to_context_vars(nil), do: nil

  defp variables_to_context_vars(vars) when is_map(vars) do
    Enum.reduce(vars, %{}, fn
      {{name, version}, type_like}, acc when is_atom(name) and is_integer(version) ->
        type_descr = coerce_var_type(type_like)
        # Mirror the fresh-variable shape from Module.Types.Of (1.20 added the
        # `paths`/`deps` fields used by cross-variable refinement).
        data = %{
          type: type_descr,
          name: name,
          context: nil,
          off_traces: [],
          paths: [],
          deps: %{}
        }

        Map.put(acc, version, data)

      _other, acc ->
        acc
    end)
  end

  defp variables_to_context_vars(_), do: nil

  # Accept Module.Types.Descr, sentinel atoms, or ElixirSense minimal shapes and
  # coerce into a Descr.t(). Closedness is carried by the shape's map tail
  # (`:closed`/`nil`/`:open`): a `:closed` tail coerces to a closed_map and
  # `nil`/`:open` stay open.
  def coerce_var_type_public(type_like), do: coerce_var_type(type_like)

  # Binding shapes are best-effort; seeding them as *static* descrs would collapse
  # intersections/matches to none() and filter out valid clauses. The compiler
  # wraps inferred values in `dynamic/1`; mirror that by dynamic-wrapping every
  # coerced shape. Already-built descrs pass through as-is.
  defp coerce_var_type(%{} = descr), do: descr
  defp coerce_var_type(:dynamic), do: Descr.dynamic()
  defp coerce_var_type(:term), do: Descr.term()
  defp coerce_var_type(nil), do: Descr.dynamic()

  defp coerce_var_type(shape), do: Descr.dynamic(coerce_static_descr(shape))

  # Inner coercion: shape -> static descr. Always dynamic-wrapped by
  # coerce_var_type/1 — never call this directly for a seed value. nil (unknown)
  # stays gradual so clause selection treats it as matching all.
  defp coerce_static_descr(%{} = descr), do: descr
  defp coerce_static_descr(:dynamic), do: Descr.dynamic()
  defp coerce_static_descr(nil), do: Descr.dynamic()
  defp coerce_static_descr(:term), do: Descr.term()

  defp coerce_static_descr({:atom, atom}) when is_atom(atom),
    do: Descr.atom([atom])

  defp coerce_static_descr(:atom), do: Descr.atom()
  defp coerce_static_descr(:boolean), do: Descr.atom([true, false])
  defp coerce_static_descr(:integer), do: Descr.integer()
  defp coerce_static_descr(:binary), do: Descr.binary()
  # Prefer the real bitstring() descr when available (1.20), else binary().
  defp coerce_static_descr(:bitstring), do: bitstring_descr()
  defp coerce_static_descr(:float), do: Descr.float()
  defp coerce_static_descr(:pid), do: Descr.pid()
  defp coerce_static_descr(:port), do: Descr.port()
  defp coerce_static_descr(:reference), do: Descr.reference()
  defp coerce_static_descr(:tuple), do: Descr.tuple()
  defp coerce_static_descr(:fun), do: Descr.fun()
  defp coerce_static_descr(:none), do: Descr.none()
  # non_struct_map has no descr constructor; an open map is the closest match.
  defp coerce_static_descr(:non_struct_map), do: Descr.open_map()
  defp coerce_static_descr(:empty_map), do: Descr.empty_map()
  defp coerce_static_descr(:empty_list), do: Descr.empty_list()
  defp coerce_static_descr(:not_set), do: Descr.not_set()

  defp coerce_static_descr(:number),
    do: Descr.union(Descr.integer(), Descr.float())

  defp coerce_static_descr({:integer, _}), do: Descr.integer()
  defp coerce_static_descr({:float, _}), do: Descr.float()
  defp coerce_static_descr({:binary, _}), do: Descr.binary()
  defp coerce_static_descr({:list, :empty}), do: Descr.empty_list()

  defp coerce_static_descr({:list, element_type}) do
    Descr.list(coerce_var_type(element_type))
  end

  defp coerce_static_descr({:nonempty_list, element_type}) do
    Descr.non_empty_list(coerce_var_type(element_type))
  end

  # Possibly-improper non-empty list. Use the arity-2 constructor when available;
  # otherwise fall back to `Descr.dynamic()` — NOT a widened proper list, since an
  # improper list isn't a member of `list(t)` and widening would be UNSOUND.
  defp coerce_static_descr({:nonempty_list, element_type, tail_type}) do
    if loaded_exported?(Descr, :non_empty_list, 2) do
      # credo:disable-for-next-line Credo.Check.Refactor.Apply
      apply(Descr, :non_empty_list, [coerce_var_type(element_type), coerce_var_type(tail_type)])
    else
      Descr.dynamic()
    end
  end

  defp coerce_static_descr({:tuple, _arity, elements}) when is_list(elements) do
    Descr.tuple(Enum.map(elements, &coerce_var_type/1))
  end

  # Open tuple — we know a prefix but not the full arity; build an open tuple.
  defp coerce_static_descr({:tuple_open, elements}) when is_list(elements) do
    Descr.open_tuple(Enum.map(elements, &coerce_var_type/1))
  end

  # The map tail drives closedness: `:closed` builds a CLOSED map (exact round-
  # trip, falling back to open on drift); `nil`/`:open` build an OPEN map since a
  # closed_map would unsoundly exclude maps with other keys.
  defp coerce_static_descr({:map, fields, :closed}) when is_list(fields) do
    maybe_closed_map_descr(fields)
  end

  defp coerce_static_descr({:map, fields, _tail}) when is_list(fields) do
    coerce_map_descr(fields, [])
  end

  defp coerce_static_descr({:map, fields}) when is_list(fields) do
    coerce_map_descr(fields, [])
  end

  defp coerce_static_descr({:struct, fields, {:atom, module}, _updated})
       when is_atom(module) and is_list(fields) do
    coerce_struct_descr(module, fields)
  end

  defp coerce_static_descr({:struct, fields, module}) when is_atom(module) and is_list(fields) do
    coerce_struct_descr(module, fields)
  end

  defp coerce_static_descr({:fun, arity}) when is_integer(arity) do
    fun_descr(arity)
  end

  defp coerce_static_descr({:fun, args, _return}) when is_list(args) do
    fun_descr(length(args))
  end

  defp coerce_static_descr({:fun_clauses, clauses}) when is_list(clauses) do
    # Multi-clause function — coerce as generic fun with arity of first clause.
    case clauses do
      [{args, _return} | _] when is_list(args) -> fun_descr(length(args))
      _ -> Descr.fun()
    end
  end

  # A `dynamic()` marker shape coerces to the gradual top.
  defp coerce_static_descr({:dynamic, nil}), do: Descr.dynamic()
  defp coerce_static_descr({:dynamic, inner}), do: coerce_static_descr(inner)

  defp coerce_static_descr({:union, types}) when is_list(types) do
    types
    |> Enum.map(&coerce_var_type/1)
    |> Enum.reduce(Descr.none(), &Descr.union/2)
  end

  defp coerce_static_descr({:intersection, types}) when is_list(types) do
    types
    |> Enum.map(&coerce_var_type/1)
    |> Enum.reduce(Descr.term(), &Descr.intersection/2)
  end

  # A bare {:optional, _} outside a map-field position has no descr equivalent
  # (if_set/1 is only meaningful as a map value) — coerce the inner type.
  # Field positions go through coerce_field_value/1, which preserves if_set.
  defp coerce_static_descr({:optional, inner}), do: coerce_static_descr(inner)

  defp coerce_static_descr(_), do: Descr.dynamic()

  # Closed-map coercion for a `:closed`-tail (literal-complete) map shape. The
  # default open coercion is the soundness-preserving choice: a partial shape's
  # closed_map would assert other keys absent and filter out valid clauses. Only
  # all-atom-keyed fields can be closed (domain keys would be dropped); falls back
  # to the open coercion when `closed_map/1` is unavailable or construction fails.
  defp maybe_closed_map_descr(fields) do
    if Enum.all?(fields, &match?({k, _} when is_atom(k), &1)) do
      try do
        coerce_map_descr(fields, [], closed: true)
      rescue
        _ -> coerce_map_descr(fields, [])
      catch
        _, _ -> coerce_map_descr(fields, [])
      end
    else
      coerce_map_descr(fields, [])
    end
  end

  defp coerce_map_descr(fields, extra_pairs, opts \\ []) do
    atom_pairs =
      extra_pairs ++ for({k, v} when is_atom(k) <- fields, do: {k, coerce_field_value(v)})

    if Keyword.get(opts, :closed, false) do
      Descr.closed_map(atom_pairs)
    else
      Descr.open_map(atom_pairs)
    end
  end

  # Map-field values preserve optionality: an {:optional, inner} shape coerces to
  # if_set(inner) so maps lacking the key stay included. Apply `if_set/1` to the
  # *static* inner descr: on 1.18 `if_set(dynamic(t))` silently drops the optional
  # marker. The map as a whole is already dynamic()-wrapped, so the per-field value
  # only needs the static type under `if_set`.
  defp coerce_field_value({:optional, inner}), do: Descr.if_set(coerce_static_descr(inner))
  defp coerce_field_value(v), do: coerce_var_type(v)

  # Build a struct descriptor. When the module is loaded, expand the FULL
  # defstruct field set into a precise closed_map; otherwise fall back to an
  # open_map over just the known fields so we don't wrongly assert keys absent.
  defp coerce_struct_descr(module, fields) do
    struct_pair = {:__struct__, Descr.atom([module])}

    known_pairs =
      for {k, v} when is_atom(k) and k != :__struct__ <- fields, do: {k, coerce_field_value(v)}

    case loaded_struct_fields(module) do
      {:ok, all_keys} ->
        known = Map.new(known_pairs)

        full_pairs =
          for key <- all_keys do
            {key, Map.get(known, key, Descr.dynamic())}
          end

        Descr.closed_map([struct_pair | full_pairs])

      :error ->
        Descr.open_map([struct_pair | known_pairs])
    end
  end

  # Safely introspect a loaded struct's field keys (excluding :__struct__).
  defp loaded_struct_fields(module) when is_atom(module) do
    if Code.ensure_loaded?(module) and function_exported?(module, :__struct__, 0) do
      keys =
        module.__struct__()
        |> Map.from_struct()
        |> Map.keys()

      {:ok, keys}
    else
      :error
    end
  rescue
    _ -> :error
  catch
    _, _ -> :error
  end

  # real bitstring() descr when available, else binary().
  defp bitstring_descr do
    if cached_cap(:descr_bitstring) do
      # credo:disable-for-next-line Credo.Check.Refactor.Apply
      apply(Module.Types.Descr, :bitstring, [])
    else
      Descr.binary()
    end
  end

  # Descr.fun/1 (arity-specific function descr) is 1.20+. On 1.18/1.19 fall back
  # to the generic `fun/0`. `apply/3` avoids an undefined-function warning there.
  defp fun_descr(arity) do
    if cached_cap(:descr_fun_1) do
      # credo:disable-for-next-line Credo.Check.Refactor.Apply
      apply(Module.Types.Descr, :fun, [arity])
    else
      Descr.fun()
    end
  end

  # Collect variables from AST and seed them as dynamic() types
  defp variables_from_ast(ast) do
    try do
      {_ast, acc} =
        Macro.prewalk(ast, %{}, fn
          {name, meta, ctx} = node, acc when is_atom(name) and is_list(meta) and is_atom(ctx) ->
            new_acc =
              case Keyword.fetch(meta, :version) do
                {:ok, version} when is_integer(version) ->
                  # Seed `_` too: native typing versions and refines each
                  # underscore, so it must be present in the context.
                  if name == :_ or valid_variable_name?(name) do
                    Map.put(acc, {name, version}, Descr.dynamic())
                  else
                    acc
                  end

                _ ->
                  acc
              end

            {node, new_acc}

          node, acc ->
            {node, acc}
        end)

      acc
    rescue
      e ->
        Logger.debug("variables_from_ast failed: #{Exception.format(:error, e, __STACKTRACE__)}")
        %{}
    end
  end

  defp valid_variable_name?(name) when is_atom(name),
    do: name not in [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__, :_]

  defp merge_variables(nil, other) when is_map(other), do: other
  defp merge_variables(other, nil) when is_map(other), do: other
  defp merge_variables(%{} = a, %{} = b), do: Map.merge(a, b, fn _k, v1, _v2 -> v1 end)

  defp targets_from_opts(opts) do
    case Keyword.get(opts, :target_keys) || Keyword.get(opts, :target_versions) do
      nil -> :all
      %MapSet{} = set -> set
      list when is_list(list) -> MapSet.new(list)
      single -> MapSet.new([single])
    end
  end

  defp normalize_match(pattern_ast, {:=, _, [lhs, rhs]} = match) do
    pattern = pattern_ast || lhs
    {pattern, rhs, match}
  end

  defp normalize_match(pattern_ast, rhs) do
    match = {:=, [], [pattern_ast, rhs]}
    {pattern_ast, rhs, match}
  end

  defp extract_var_shapes(vars_map, targets) do
    Enum.reduce(vars_map, %{}, fn
      {version, %{name: name, type: descr}}, acc ->
        key = {name, version}

        if include_var?(targets, key) do
          case descr_to_shape(descr) do
            nil -> acc
            shape -> Map.put(acc, key, shape)
          end
        else
          acc
        end

      _, acc ->
        acc
    end)
  end

  defp include_var?(:all, _), do: true
  defp include_var?(%MapSet{} = set, key), do: MapSet.member?(set, key)
  defp include_var?(_, _), do: false

  defp descr_to_shape(descr) do
    cond do
      Descr.empty?(descr) -> :none
      shape = to_shape(descr) -> shape
      true -> nil
    end
  end

  @doc """
  Best-effort signature inference for local functions.

  Each `clause` entry should be a map with at least:
    %{meta: meta, args: [ast], guards: guards_ast | nil, body: ast}

  Returns {:infer, domain, clauses} or :error when nothing useful could be inferred.
  """
  def infer_local_signature(module, {fun, arity} = fun_arity, clauses, file, mode \\ :infer)
      when is_atom(module) and is_atom(fun) and is_integer(arity) do
    with true <- enabled?(),
         true <- arity >= 0,
         false <- clauses == [] do
      expected = List.duplicate(Descr.dynamic(), arity)

      stack =
        init_stack(module, fun_arity, file || "nofile", mode)
        |> maybe_disable_local_handler()

      case stack do
        nil ->
          :error

        _stack ->
          context = init_context()
          reduced = do_infer_local_signature(stack, context, clauses, expected, fun_arity)

          case reduced do
            [] ->
              :error

            clause_types ->
              domain = build_domain(clause_types)
              {:infer, domain, clause_types}
          end
      end
    else
      _ -> :error
    end
  end

  defp maybe_disable_local_handler(nil), do: nil

  defp maybe_disable_local_handler(stack) do
    # prevent recursive lookups while we are inferring the function itself
    %{stack | local_handler: fn _, _, _, context -> {:def, :none, context} end}
  end

  defp do_infer_local_signature(stack, context, clauses, expected, fun_arity) do
    # ElixirSense metadata is NOT macro-expanded, so a clause head can contain an
    # unexpanded record/defguard macro on which native of_head/of_pattern/of_guard
    # raises; we detect these syntactically. When ANY clause is untypeable we skip
    # the WHOLE function's native signature — skipping only the offending clause
    # would produce a signature with a MISSING clause, unsound for overload
    # selection.
    if Enum.all?(clauses, &clause_native_typeable?/1) do
      reduce_local_clauses(stack, context, clauses, expected, fun_arity)
    else
      []
    end
  end

  defp clause_native_typeable?(clause) do
    %{args: args, guards: guards} = normalise_clause(clause)
    native_typeable_clause?(args, guards)
  end

  defp reduce_local_clauses(stack, context, clauses, expected, fun_arity) do
    Enum.reduce_while(clauses, [], fn clause, acc ->
      %{meta: meta, args: args, guards: guards, body: body} = normalise_clause(clause)
      body = body |> ensure_body_var_versions() |> replace_module_attributes()

      # Seed every variable referenced in the clause as `:dynamic`. We type the
      # body in isolation, so a body var referencing an earlier binding would
      # otherwise crash native `refine_body_var` on a not-yet-present version.
      seeded_context =
        init_context(collect_versioned_vars([args, guards || [], body])) || context

      try do
        # `head_kind` is :trees (1.19/1.20, needs of_domain) or :types (1.18).
        {head_result, clause_ctx, head_kind} =
          call_of_head(args, guards || [], expected, meta, stack, seeded_context, fun_arity)

        {return_type, clause_ctx} =
          call_of_expr(body, Descr.term(), body, stack, clause_ctx)

        arg_types = arg_types_from_head(head_result, head_kind, expected, stack, clause_ctx)

        {:cont, [{arg_types, return_type} | acc]}
      catch
        kind, payload ->
          Logger.warning(
            "Unable to infer local signature: #{Exception.format(kind, payload, __STACKTRACE__)}\nBody: #{inspect(body)}"
          )

          {:cont, acc}
      end
    end)
    |> Enum.reverse()
  end

  defp build_domain([_]), do: nil

  defp build_domain(clause_types) do
    clause_types
    |> Enum.map(&elem(&1, 0))
    |> Enum.zip()
    |> Enum.map(fn tuple ->
      tuple
      |> Tuple.to_list()
      |> Enum.reduce(&Descr.union/2)
    end)
  end

  defp normalise_clause(%{meta: meta, args: args, guards: guards, body: body}) do
    %{meta: meta || [], args: args || [], guards: guards, body: body || {:__block__, [], []}}
  end

  # Native of_pattern requires every pattern variable (including underscores) to
  # carry a `:version`. Synthetic versions are drawn from high bases so they never
  # collide with the small, densely-allocated compiler/metadata versions:
  #
  #   * `@synthetic_version_base` (1_000_000)+ — stamped by
  #     `ensure_body_var_versions/1` onto unversioned pattern/body vars.
  #   * `@attr_placeholder_version_base` (2_000_000)+ — used by
  #     `replace_module_attributes/1` for `@attr` placeholder vars.
  #
  # These exist purely so native typing doesn't crash; they are NOT real VarInfo
  # versions, so `extract_refined_var_shapes/5` filters every synthetic-keyed
  # entry out (via `synthetic_version?/1`) before returning.
  @synthetic_version_base 1_000_000
  @attr_placeholder_version_base 2_000_000
  @underscore_version_base @synthetic_version_base

  # True for any variable version we synthesized (underscore/unversioned stamps
  # and attr placeholders). Such versions must never leak into VarInfo-keyed
  # consumer maps. Real compiler/metadata versions are always below the base.
  defp synthetic_version?(version) when is_integer(version),
    do: version >= @synthetic_version_base

  defp synthetic_version?(_), do: false

  # All `{name, version} => :dynamic` pairs for versioned variables in `ast`
  # (a list of ASTs), used to pre-seed an isolated native typing context.
  defp collect_versioned_vars(asts) do
    asts
    |> List.wrap()
    |> Enum.reduce(%{}, fn ast, acc ->
      {_ast, acc} =
        Macro.prewalk(ast, acc, fn
          {name, meta, context} = node, acc
          when is_atom(name) and is_list(meta) and is_atom(context) and
                 name not in [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__] ->
            case Keyword.fetch(meta, :version) do
              {:ok, version} when is_integer(version) ->
                {node, Map.put(acc, {name, version}, :dynamic)}

              _ ->
                {node, acc}
            end

          node, acc ->
            {node, acc}
        end)

      acc
    end)
  end

  # Stamp a `:version` on every unversioned variable so native typing (whose
  # context vars are keyed by version only) doesn't alias distinct names. Each
  # distinct *name* gets its own version via a `name => version` map (repeated
  # occurrences share it); each underscore gets a unique version.
  defp ensure_body_var_versions(ast) do
    {ast, _acc} =
      Macro.prewalk(ast, {@underscore_version_base, %{}}, fn
        {:@, _, _} = node, acc ->
          {node, acc}

        {:_, meta, context} = node, {counter, names} when is_list(meta) and is_atom(context) ->
          if Keyword.has_key?(meta, :version) do
            {node, {counter, names}}
          else
            {{:_, Keyword.put(meta, :version, counter), context}, {counter + 1, names}}
          end

        {name, meta, context} = node, {counter, names}
        when is_atom(name) and is_list(meta) and is_atom(context) and
               name not in [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__] ->
          if Keyword.has_key?(meta, :version) do
            {node, {counter, names}}
          else
            case Map.fetch(names, name) do
              {:ok, version} ->
                {{name, Keyword.put(meta, :version, version), context}, {counter, names}}

              :error ->
                version = counter
                names = Map.put(names, name, version)
                {{name, Keyword.put(meta, :version, version), context}, {counter + 1, names}}
            end
          end

        node, acc ->
          {node, acc}
      end)

    ast
  end

  # Replace @attr references with placeholder variables so Module.Types.Expr.of_expr can process them
  defp replace_module_attributes(body) do
    {body, _counter} =
      Macro.prewalk(body, 0, fn
        {:@, _meta, [{_attr_name, _, _}]} = _node, counter ->
          # Replace @attr with a fresh variable of_expr can handle, using a
          # version base above the @underscore_version_base range so placeholders
          # can't alias a real variable's context slot.
          placeholder =
            {:__attr_placeholder__, [version: @attr_placeholder_version_base + counter], nil}

          {placeholder, counter + 1}

        node, counter ->
          {node, counter}
      end)

    body
  end

  @doc """
  Build a local signatures map from metadata for a specific module.

  Returns a map of `{function, arity} => {kind, signature}` for use with local_handler.
  """
  def build_local_sigs_map(metadata, module) when is_atom(module) and is_map(metadata) do
    signatures =
      metadata.mods_funs_to_positions
      |> Enum.filter(fn {{mod, _fun, _arity}, _info} -> mod == module end)
      |> Enum.reduce(%{}, fn {{_mod, fun, arity}, info}, acc ->
        case info do
          %{
            type: type,
            elixir_types_sig: stored_sig,
            elixir_types_sig_source: stored_source
          } ->
            kind = get_def_kind_for_types(type)

            # Pick the most-trusted signature by EXPLICIT provenance (highest
            # first): :exck (compiled .beam ExCk chunk, ground truth), :inferred
            # (native local inference), :spec (@spec-derived fallback). We do NOT
            # rely on the kind tag — a :strong spec must not outrank an inferred sig.
            spec_sig =
              case spec_signature_from_metadata(metadata, module, fun, arity) do
                {:ok, s} -> s
                :error -> nil
              end

            candidates =
              [
                {stored_source, stored_sig},
                {:spec, spec_sig}
              ]
              |> Enum.filter(fn {_source, s} -> s != nil end)

            sig =
              case Enum.min_by(candidates, fn {source, _s} -> sig_source_rank(source) end, fn ->
                     {nil, nil}
                   end) do
                {_source, s} -> s
              end

            if sig != nil do
              Map.put(acc, {fun, arity}, {kind, sig})
            else
              acc
            end

          _ ->
            acc
        end
      end)
      |> Map.put(:__module__, module)

    signatures
  end

  def build_local_sigs_map(_metadata, _module), do: %{}

  # Trust ranking for signature provenance (lower = more trusted): :exck >
  # :inferred > :spec; unknown/nil sources sort last.
  defp sig_source_rank(:exck), do: 0
  defp sig_source_rank(:inferred), do: 1
  defp sig_source_rank(:spec), do: 2
  defp sig_source_rank(_), do: 3

  @doc """
  Create a closure-based local handler from a signatures map.
  """
  def local_handler_from(local_sigs_map) when is_map(local_sigs_map) do
    fn _meta, {_fun, _arity} = fun_arity, _stack, context ->
      case Map.get(local_sigs_map, fun_arity) do
        {kind, {sig_kind, _domain, _clause_types} = sig}
        when sig_kind in [:infer, :strong] ->
          {kind, sig, context}

        {kind, :none} ->
          {kind, :none, context}

        _ ->
          false
      end
    end
  end

  def spec_signature_from_metadata(metadata, module, fun, arity)
      when is_map(metadata) and is_atom(module) and is_atom(fun) and is_integer(arity) do
    case metadata.specs[{module, fun, arity}] do
      %SpecInfo{elixir_types_sig: {kind, _domain, _clauses} = sig}
      when kind in [:infer, :strong] ->
        {:ok, sig}

      _ ->
        :error
    end
  end

  def spec_signature_from_metadata(_, _, _, _), do: :error

  # Helper to convert ElixirSense def types to Module.Types kinds
  defp get_def_kind_for_types(:def), do: :def
  defp get_def_kind_for_types(:defp), do: :defp
  defp get_def_kind_for_types(:defmacro), do: :defmacro
  defp get_def_kind_for_types(:defmacrop), do: :defmacrop
  # For other types, default to :def
  defp get_def_kind_for_types(_), do: :def

  # Above this clause count the compiler stops unioning returns and just returns
  # `dynamic()` (Module.Types.Apply `@max_clauses`, apply.ex:16).
  @max_clauses 16

  @doc """
  Applies a remote/local signature to a list of argument shapes, mirroring
  `Module.Types.Apply.remote_apply/3` (`apply_infer/2` and `apply_strong/4`,
  `~/elixir/lib/elixir/lib/module/types/apply.ex:1416-1879`).

  `arg_shapes` may be a list of ElixirSense shapes (they are coerced to
  `dynamic()`-wrapped descriptors via `coerce_var_type/1`, so unknown/`nil`
  args become `dynamic()` and — being gradual — widen matching exactly like the
  compiler's gradual args), a list of descriptors, or `nil`/`[]` (no argument
  information, all clauses considered).

  Returns:

    * `{:ok, descr}` — the union of the matched clause returns. For `:infer`
      sigs (and the no-arg path) the union is ALWAYS wrapped in `dynamic()`
      (apply.ex:1827 wraps unconditionally). For `:strong` sigs the union is
      routed through `Apply.return/3` (apply.ex:1732-1741), which wraps only when
      an argument is gradual; fully-static args yield the bare static union. A
      bare `dynamic()` is returned when more than `@max_clauses` matched, as the
      compiler does.
    * `:error` — the call is ill-typed for this signature (no `:infer` clause's
      domain is non-disjoint with the args, or a `:strong` domain is violated).
      The compiler treats this as a `:badremote` error; callers should fall back
      to structural typing rather than inventing a return type. CRITICALLY we do
      NOT fall back to the union of every clause's return here — that would
      invent a type for an ill-typed call.

  This is the single place application semantics live; `extract_return_type_from_sig/2`
  delegates here so future consumers stop reimplementing clause selection.
  """
  @spec apply_signature(term(), [term()] | nil) :: {:ok, term()} | :error
  def apply_signature(sig, arg_shapes \\ nil)

  def apply_signature({sig_kind, _domain, clauses}, arg_shapes)
      when sig_kind in [:infer, :strong] and is_list(clauses) do
    arg_descrs = coerce_arg_shapes(arg_shapes)
    # `apply_infer` (apply.ex:1827) ALWAYS dynamic()-wraps; only `apply_strong`
    # routes through `return/3` (apply.ex:1834,1844), wrapping only when an arg is
    # gradual. With no argument information we treat the call as gradual.
    gradual_args? = arg_descrs == nil or Enum.any?(arg_descrs, &descr_gradual?/1)

    cond do
      # No argument information: consider every clause as if every arg were gradual
      # (always wraps), mirroring the compiler with fully-gradual args.
      arg_descrs == nil ->
        apply_no_args(clauses)

      sig_kind == :infer ->
        apply_infer_mirror(clauses, arg_descrs)

      sig_kind == :strong ->
        apply_strong_mirror(clauses, arg_descrs, gradual_args?)
    end
  end

  def apply_signature(:none, _arg_shapes), do: {:ok, Descr.dynamic()}
  def apply_signature(_sig, _arg_shapes), do: :error

  # Thin descr-returning wrapper over `apply_signature/2` for callers (Binding)
  # that expect a descr. `:error` (ill-typed call) maps to `Descr.dynamic()`
  # (shape nil), so the caller's nil-check falls back to structural typing rather
  # than receiving an invented union of all clause returns.
  def extract_return_type_from_sig(sig, arg_shapes \\ nil)

  def extract_return_type_from_sig(sig, arg_shapes) do
    case apply_signature(sig, arg_shapes) do
      {:ok, descr} -> descr
      :error -> Descr.dynamic()
    end
  end

  # Coerce argument shapes into STATIC descriptors for clause selection. The
  # dynamic() wrap from coerce_var_type is wrong here: a dynamic-wrapped arg has
  # an empty static part and would match every clause. Unknown shapes (nil) still
  # coerce to bare dynamic() and stay gradual. Returns `nil` when there is no
  # usable argument information, or when coercion fails.
  defp coerce_arg_shapes(arg_shapes) do
    if is_list(arg_shapes) and arg_shapes != [] and available?() do
      try do
        Enum.map(arg_shapes, &coerce_static_descr/1)
      rescue
        e ->
          Logger.debug(
            "apply_signature coercion failed: #{Exception.format(:error, e, __STACKTRACE__)}"
          )

          nil
      catch
        kind, payload ->
          Logger.debug(
            "apply_signature coercion failed: #{Exception.format(kind, payload, __STACKTRACE__)}"
          )

          nil
      end
    else
      nil
    end
  end

  # All clauses are candidates (no/unusable argument info). Union returns,
  # dynamic-wrap; > @max_clauses ⇒ bare dynamic() (apply.ex:1827).
  defp apply_no_args(clauses) do
    if length(clauses) > @max_clauses do
      {:ok, Descr.dynamic()}
    else
      case extract_return_type_from_clauses(clauses) do
        {:ok, return_type} -> {:ok, wrap_dynamic(return_type)}
        :error -> {:ok, Descr.dynamic()}
      end
    end
  end

  # Mirror apply.ex:1818-1829 `apply_infer/2`: select clauses whose argument domain
  # is per-position NOT disjoint with the actual args; union of matched returns,
  # ALWAYS dynamic()-wrapped (apply.ex:1827).
  defp apply_infer_mirror(clauses, arg_descrs) do
    apply_mirror(
      clauses,
      &args_not_disjoint?(clause_arg_types(&1), arg_descrs),
      &wrap_dynamic/1
    )
  end

  # Shared clause-selection + return-resolution skeleton (apply.ex:1818-1848),
  # parameterized by the `select?` predicate and `wrap`. Zero matches ⇒ :error;
  # > @max_clauses ⇒ bare dynamic(); otherwise the wrapped union of matched returns.
  defp apply_mirror(clauses, select?, wrap) do
    matched = Enum.filter(clauses, select?)

    cond do
      matched == [] ->
        :error

      length(matched) > @max_clauses ->
        {:ok, Descr.dynamic()}

      true ->
        case extract_return_type_from_clauses(matched) do
          {:ok, return_type} -> {:ok, wrap.(return_type)}
          :error -> {:ok, Descr.dynamic()}
        end
    end
  end

  # Mirror apply.ex:1831-1848 `apply_strong/4`: each actual arg must be compatible
  # with the clause's expected domain (the clause's argument tuple). No compatible
  # clause ⇒ :error. Returns route through `return/3` (apply.ex:1834,1844),
  # wrapped only when an arg is gradual, unlike apply_infer which always wraps.
  defp apply_strong_mirror(clauses, arg_descrs, gradual_args?) do
    apply_mirror(
      clauses,
      &args_compatible?(clause_arg_types(&1), arg_descrs),
      &maybe_wrap_dynamic(&1, gradual_args?)
    )
  end

  # Mirror `Apply.return/3` (apply.ex:1732-1741): wrap the matched return in
  # `dynamic()` only when an argument is gradual; static args yield the bare return.
  defp maybe_wrap_dynamic(descr, true), do: wrap_dynamic(descr)
  defp maybe_wrap_dynamic(descr, false), do: descr

  # Wrap a return type in dynamic() (idempotent), as the compiler does for
  # inferred/applied returns when an arg is gradual (apply.ex:1738).
  defp wrap_dynamic(descr) do
    Descr.dynamic(descr)
  rescue
    _ -> descr
  catch
    _, _ -> descr
  end

  # Mirror of `Descr.gradual?/1` (descr.ex:385-386) with a safe fallback: probe
  # the export, else check the `:dynamic` key directly. `:term` is not gradual.
  defp descr_gradual?(:term), do: false

  defp descr_gradual?(descr) do
    cond do
      cached_cap(:descr_gradual) ->
        # credo:disable-for-next-line Credo.Check.Refactor.Apply
        apply(Module.Types.Descr, :gradual?, [descr])

      is_map(descr) ->
        is_map_key(descr, :dynamic)

      true ->
        false
    end
  rescue
    _ -> false
  catch
    _, _ -> false
  end

  defp clause_arg_types({arg_types, _return}) when is_list(arg_types), do: arg_types
  defp clause_arg_types(%{args: arg_types}) when is_list(arg_types), do: arg_types
  defp clause_arg_types(_), do: nil

  # apply_infer clause selection: per-position non-disjoint check (apply.ex:1875
  # `zip_not_disjoint?`). Arity mismatch ⇒ not selected; unknown arg types (nil)
  # are kept as a candidate (can't prove disjoint).
  defp args_not_disjoint?(nil, _arg_descrs), do: true

  defp args_not_disjoint?(clause_args, arg_descrs)
       when length(clause_args) != length(arg_descrs),
       do: false

  defp args_not_disjoint?(clause_args, arg_descrs) do
    try do
      clause_args
      |> Enum.zip(arg_descrs)
      |> Enum.all?(fn {domain, arg} -> not descr_disjoint?(arg, domain) end)
    rescue
      e ->
        Logger.debug("args_not_disjoint? failed: #{Exception.format(:error, e, __STACKTRACE__)}")

        true
    catch
      kind, payload ->
        Logger.debug(
          "args_not_disjoint? failed: #{Exception.format(kind, payload, __STACKTRACE__)}"
        )

        true
    end
  end

  # apply_strong domain check: each actual arg compatible with the expected domain
  # (apply.ex:1869 `zip_compatible?`). Gradual args match anything; falls back to a
  # non-disjoint check if `compatible?/2` is unavailable.
  defp args_compatible?(nil, _arg_descrs), do: true

  defp args_compatible?(clause_args, arg_descrs)
       when length(clause_args) != length(arg_descrs),
       do: false

  defp args_compatible?(clause_args, arg_descrs) do
    try do
      clause_args
      |> Enum.zip(arg_descrs)
      |> Enum.all?(fn {domain, arg} -> descr_arg_compatible?(arg, domain) end)
    rescue
      e ->
        Logger.debug("args_compatible? failed: #{Exception.format(:error, e, __STACKTRACE__)}")
        true
    catch
      kind, payload ->
        Logger.debug(
          "args_compatible? failed: #{Exception.format(kind, payload, __STACKTRACE__)}"
        )

        true
    end
  end

  # `Descr.disjoint?/2` mirror with a safe fallback (empty intersection) if the
  # private API is unavailable on the running Elixir.
  defp descr_disjoint?(left, right) do
    if cached_cap(:descr_disjoint) do
      # credo:disable-for-next-line Credo.Check.Refactor.Apply
      apply(Module.Types.Descr, :disjoint?, [left, right])
    else
      Descr.empty?(Descr.intersection(left, right))
    end
  end

  # Single actual-arg compatibility for :strong (gradual args widen). Uses
  # `Descr.compatible?/2`; falls back to a non-disjoint check.
  defp descr_arg_compatible?(arg, domain) do
    cond do
      cached_cap(:descr_only_gradual) and
          apply(Module.Types.Descr, :only_gradual?, [arg]) ->
        true

      cached_cap(:descr_compatible) ->
        # credo:disable-for-next-line Credo.Check.Refactor.Apply
        apply(Module.Types.Descr, :compatible?, [arg, domain])

      true ->
        not descr_disjoint?(arg, domain)
    end
  end

  defp extract_return_type_from_clauses(clauses) do
    clauses
    |> Enum.reduce_while(nil, fn clause, acc ->
      case clause_return_type(clause) do
        nil ->
          {:cont, acc}

        return_type ->
          try do
            combined =
              if acc == nil, do: return_type, else: Descr.union(acc, return_type)

            {:cont, combined}
          rescue
            _ -> {:halt, :error}
          catch
            _ -> {:halt, :error}
          end
      end
    end)
    |> case do
      :error -> :error
      nil -> :error
      return_type -> {:ok, return_type}
    end
  end

  defp clause_return_type({_, return_type}), do: return_type
  defp clause_return_type(%{return: return_type}), do: return_type
  defp clause_return_type(_), do: nil
end
