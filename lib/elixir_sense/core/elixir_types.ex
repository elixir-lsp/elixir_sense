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
  # All `apply/3` calls in this module are deliberate — they dispatch to
  # version-specific `Module.Types` APIs whose arity differs across Elixir
  # releases.  A literal call to the wrong-version arity would be an
  # undefined-function compile warning on the other Elixir, so `apply/3` is the
  # only safe choice here.
  # credo:disable-for-this-file Credo.Check.Refactor.Apply
  alias Module.Types.Descr
  alias ElixirSense.Core.ExCkReader
  alias ElixirSense.Core.State.VarInfo
  alias ElixirSense.Core.State.AttributeInfo
  alias ElixirSense.Core.State.SpecInfo

  # Persistent-term key for the memoized capabilities map.
  @capabilities_pt_key {__MODULE__, :capabilities}

  def available? do
    caps = cached_capabilities()
    # available?/0 requires both expr_basic AND local_signature (stack/7).
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
  API surface (`function_exported?/3`), never by matching `System.version/0`.
  This keeps the adaptor flexible across Elixir releases — whose `Module.Types`
  internals are `@moduledoc false` and change every minor version — and avoids
  baking version numbers into compile-time constants (which would also defeat
  Dialyzer's reachability analysis).
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

  # Read the capabilities map from :persistent_term, computing and storing it on
  # the first call. Capabilities probe module exports that cannot change within a
  # running VM (modules do not gain or lose exports without a hot-code reload),
  # so this is safe to cache for the lifetime of the node.
  #
  # DO NOT memoize `enabled?/0` here — its Application.get_env read is toggled
  # by tests at runtime.
  defp cached_capabilities do
    :persistent_term.get(@capabilities_pt_key, nil) || compute_and_cache_capabilities()
  end

  defp compute_and_cache_capabilities do
    caps = %{
      expr: expr_api() == :expected,
      expr_basic: expr_api() != :none,
      pattern_match: pattern_api() != :none,
      head:
        loaded_exported?(Module.Types.Pattern, :of_head, 8) or
          loaded_exported?(Module.Types.Pattern, :of_head, 7),
      local_signature: loaded_exported?(Module.Types, :stack, 7),
      previous: loaded_exported?(Module.Types.Pattern, :init_previous, 0)
    }

    :persistent_term.put(@capabilities_pt_key, caps)
    caps
  end

  # --- Version-dispatched Module.Types.Pattern calls --------------------------
  #
  # `enabled?/0` is true on any Elixir with `Expr.of_expr/5` (1.19+), but the
  # pattern API changed shape across releases. These helpers dispatch on the
  # actually-exported arity so the adapter works on 1.19 and 1.20 alike instead
  # of calling 1.20-only forms and degrading on 1.19. Every variant arity goes
  # through `apply/3`: a literal call to the other version's arity would be an
  # undefined-function compile warning on whichever Elixir is not loaded. The
  # safety net against upstream drift is the test suite (run on both versions),
  # not Dialyzer's static `:unknown` check.

  # of_expr: 1.19+ `of_expr/5` (expected + expr-tracking); 1.18 `of_expr/3`
  # (no expected type). Both return `{type, context}`; on 1.18 the `expected`
  # and `expr` arguments are simply dropped.
  defp call_of_expr(ast, expected, expr, stack, context) do
    case expr_api() do
      # credo:disable-for-next-line Credo.Check.Refactor.Apply
      :expected -> apply(Module.Types.Expr, :of_expr, [ast, expected, expr, stack, context])
      # credo:disable-for-next-line Credo.Check.Refactor.Apply
      :basic -> apply(Module.Types.Expr, :of_expr, [ast, stack, context])
      :none -> {Descr.dynamic(), context}
    end
  end

  # of_match. The contract changed shape across versions; all return `{type,
  # context}`:
  #   * 1.20 `of_match/6` — (pattern, expected_fun, expr, meta, stack, context)
  #   * 1.19 `of_match/5` — (pattern, expected_fun, expr, stack, context)
  #   * 1.18 `of_match/7` — (pattern, guards, expected, expr, tag, stack, context)
  # 1.19/1.20 take a lazy `expected_fun`; 1.18 takes a precomputed `expected`
  # descr. We build whichever the running version needs from `value_ast` +
  # `expected_descr`.
  defp call_of_match(pattern_ast, value_ast, expected_descr, full_match, stack, context) do
    case pattern_api() do
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
        # (`{:match, expected}`), not the full match AST (v1.18.4 expr.ex:135).
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
    case pattern_api() do
      :v20 ->
        # credo:disable-for-next-line Credo.Check.Refactor.Apply
        previous = apply(Module.Types.Pattern, :init_previous, [])
        # task #37: thread the real `{fun, arity}` into the info tuple instead of
        # the `:infer` placeholder. of_head reads this as
        # `{{:def, kind, fun, types}, args, guards}` (pattern.ex:1519/1637) — the
        # `fun` element is used in diagnostics, so a placeholder mislabels them.
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
    case pattern_api() do
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
        # 1.18 callers fall back to the custom engine. (of_match / of_head still
        # work on 1.18 — they use call_of_expr internally, not this entry point.)
        if available?(:expr) do
          try do
            stack = init_stack(module, function, file, mode, local_sigs_map, metadata)
            # Module.Types requires every var to carry a :version. Pattern
            # sub-expressions reaching here (e.g. typed via TypeInference for a
            # case clause) can have unversioned vars (notably nested map-pattern
            # vars); stamp them so native typing doesn't raise + log noise.
            ast = ensure_body_var_versions(ast)
            # Seed context with provided variables or infer from AST (dynamic types)
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
        # task #15: validate liveness, not just shape. A dead checker process or
        # a torn-down ETS table makes Apply.export -> fetch_export raise badarg,
        # which gets rescued to :error and silently stops native typing for the
        # rest of this process. Start a fresh checker and replace the stale entry.
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

  defp module_from_ast({:__MODULE__, _, _}, metadata) when is_map(metadata) do
    metadata
    |> metadata_env()
    |> case do
      %{module: module} when is_atom(module) -> {:ok, module}
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

  defp module_from_ast({:__aliases__, _, parts}, metadata) when is_list(parts) do
    try do
      {:ok, resolve_alias(parts, metadata)}
    rescue
      ArgumentError -> :error
    end
  end

  defp module_from_ast(_ast, _metadata), do: :error

  defp resolve_alias(parts, metadata) when is_list(parts) and is_map(metadata) do
    env = metadata_env(metadata)

    aliases =
      case env do
        %{aliases: aliases} when is_list(aliases) -> aliases
        _ -> []
      end

    current_module = if is_map(env), do: env.module, else: nil

    mod = Module.concat(parts)

    case ElixirSense.Core.Introspection.expand_alias(mod, aliases) do
      ^mod ->
        case expand_alias_from_env(current_module, aliases, parts) do
          resolved when is_atom(resolved) and not is_nil(resolved) ->
            resolved

          _ ->
            # For single-element aliases, try parent module resolution as fallback
            case parts do
              [single] when is_atom(single) and is_atom(current_module) ->
                resolve_parent_alias(current_module, single) || Module.concat(parts)

              _ ->
                Module.concat(parts)
            end
        end

      resolved ->
        resolved
    end
  end

  defp resolve_alias(parts, _metadata), do: Module.concat(parts)

  defp resolve_parent_alias(module, single) when is_atom(module) and is_atom(single) do
    parent = module |> Module.split() |> Enum.drop(-1)

    case parent do
      [] -> nil
      parts -> Module.concat(parts ++ [single])
    end
  rescue
    e ->
      Logger.debug("resolve_parent_alias failed: #{Exception.format(:error, e, __STACKTRACE__)}")
      nil
  end

  defp resolve_parent_alias(_, _), do: nil

  defp expand_alias_from_env(module, aliases, list)
       when is_atom(module) and is_list(aliases) and is_list(list) do
    env = %Macro.Env{module: module, aliases: aliases}

    case ElixirSense.Core.Normalized.Macro.Env.expand_alias(env, [], list, trace: false) do
      {:alias, resolved} when is_atom(resolved) -> resolved
      _ -> nil
    end
  rescue
    e ->
      Logger.debug("expand_alias_from_env failed: #{Exception.format(:error, e, __STACKTRACE__)}")
      nil
  end

  defp expand_alias_from_env(_, _, _), do: nil

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
    do_of_match(pattern_ast, expected_descr, match_ast, module, function, file, mode, opts)
  end

  defp do_of_match(
         pattern_ast,
         expected_descr,
         match_ast,
         module,
         function,
         file,
         mode,
         opts
       ) do
    if available?() do
      targets = targets_from_opts(opts)

      try do
        stack = init_stack(module, function, file, mode)

        if stack do
          # Stamp :version on any unversioned pattern/value vars, and replace
          # `@attr` references with placeholder vars, before building full_match —
          # native of_match/of_pattern raises on unversioned vars and on `{:@, …}`
          # nodes (e.g. `<<…, @separator, …>>` binary patterns).
          {pattern_ast, value_ast, full_match} =
            normalize_match(
              pattern_ast |> ensure_body_var_versions() |> replace_module_attributes(),
              match_ast |> ensure_body_var_versions() |> replace_module_attributes()
            )

          # Elixir 1.20 dropped the dev-era `stack.refine_vars` flag; variable
          # refinement now happens unconditionally inside Pattern.of_match/6 — and
          # it refines *every* variable the match references (e.g. a `size(pos)`
          # in a binary pattern, or a value-expression var). Seed them all (auto
          # from the AST, plus any caller-provided types) so `refine_body_var`
          # doesn't crash on a version that isn't in the context.
          auto_vars = variables_from_ast(full_match)
          effective_vars = merge_variables(Keyword.get(opts, :variables), auto_vars)
          context = init_context(effective_vars)

          expected_descr = expected_descr || Descr.term()

          # Only real match patterns can be typed by `Pattern.of_match`. Quoted /
          # macro code routed through here can carry non-pattern AST — local or
          # remote calls (`decompose_args(...)`, `Kernel.to_timeout(...)`),
          # typespec operators (`{:"__::__"}`, `{:"__|__"}`) — on which native
          # `of_pattern` raises a `FunctionClauseError`. Skip native typing for
          # those (the custom engine still handles the match).
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

  # AST node names (with list args) that are legitimate inside a match pattern.
  # Anything else with list args is a call/operator that `Pattern.of_pattern`
  # can't handle.
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

  # Enhanced pattern matching with better variable type refinement
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
      # First, try to get more specific type information from the value
      value_type = call_of_expr(value_ast, Descr.term(), full_match, stack, context)

      # Use more refined expected descriptor if value typing succeeded
      refined_expected =
        case value_type do
          {type_descr, _} -> Descr.intersection(expected_descr, type_descr)
          _ -> expected_descr
        end

      case call_of_match(pattern_ast, value_ast, refined_expected, full_match, stack, context) do
        {_type, %{vars: vars_map} = out_ctx} ->
          # Enhanced variable shape extraction with type refinement
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
    # Native `Pattern.of_match` variable descriptors are AUTHORITATIVE. The
    # AST-based `apply_pattern_refinements` engine is a best-effort fallback that
    # reimplements pieces of pattern typing (with known imprecisions); it must
    # only fill variables for which the native path produced nothing (i.e. the
    # var has no key in `base_shapes` because `descr_to_shape` returned nil).
    # See FABLE #29 / GPT "Pattern And Occurrence" #1.
    base_shapes = extract_var_shapes(vars_map, targets)

    refined_shapes =
      apply_pattern_refinements(base_shapes, pattern_ast, expected_descr, value_ast)

    # Merge refinements UNDER native results: `Map.merge(refined, base)` keeps
    # every native key as-is and only adds refinement entries for native-less
    # vars. Then drop synthetic-version keys (Task 3) so an AST refinement keyed
    # to a version we stamped (underscore/unversioned/attr placeholder) can never
    # leak into the VarInfo-keyed map the consumer merges back.
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
    # NOTE: the struct module variable (the `var` in `%var{...}`) is deliberately
    # NOT refined here. Previously it was forced to `{:atom, nil}` ("some atom"),
    # which is a guess — the native path types it precisely when it can, and
    # forcing `{:atom, nil}` for a native-less var only invents an imprecise
    # type. Leave it unknown (nil) so it falls through to structural typing.
    # See FABLE #29.
    refine_map_pattern_vars(var_shapes, fields, expected_descr, value_ast)
  end

  # Refine variables in map patterns
  defp refine_map_pattern_vars(var_shapes, fields, _expected_descr, value_ast) do
    value_field_map = map_field_map(shape_from_ast(value_ast))

    Enum.reduce(fields, %{}, fn
      {key_ast, var_ast}, acc ->
        case extract_var_from_ast(var_ast) do
          nil ->
            # Check if this is a nested pattern that needs recursive refinement
            key_literal = literal_from_key_ast(key_ast)

            if key_literal != nil && Map.has_key?(value_field_map, key_literal) do
              # Get the value for this field from the AST
              field_value_ast = get_field_value_ast(value_ast, key_literal)
              # Recursively apply pattern refinement for nested patterns
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
        # Fixed-length list pattern `[a, b, c]`. Each pattern element matches the
        # value element at the SAME position — not all the head's shape. (Bug:
        # previously every element var was given the first value element's shape,
        # so `[a, b] = [1, "x"]` typed `b` as integer.) Refine a var only when we
        # actually know its own positional value; otherwise leave it unknown.
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
  # Unknown / unrecognized segment spec: do NOT guess `{:integer, nil}` — leave
  # the var unknown (nil) so it falls through to native/structural typing.
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
      # Handle maps
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

      # Handle structs
      {:%, _, [_, {:%{}, _, fields}]} ->
        get_field_value_ast({:%{}, [], fields}, field_key)

      _ ->
        nil
    end
  end

  defp normalize_var_versions(vars_map) do
    # For each variable name, if version 1 is missing, use the lowest available version.
    # Group entries by var name first to avoid non-deterministic map iteration order.
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
        # Pick the entry with the lowest version number
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
        merged = merge_list_element_shapes(element_shapes)
        {:list, merged}
    end
  end

  defp shape_from_ast(_), do: nil

  defp merge_list_element_shapes([]), do: nil

  defp merge_list_element_shapes(shapes) do
    shapes
    |> Enum.map(&generalize_shape/1)
    |> Enum.reject(&is_nil/1)
    |> List.first()
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

  # Convert a Module.Types.Descr descriptor to an ElixirSense shape.
  # Uses Descr.to_quoted/1 to get stable AST representation, then translates
  # that AST to ElixirSense shapes — avoiding internal BDD pattern matching.
  defp to_shape_eager(descr) do
    if available?() and is_map(descr) do
      # Descr.to_quoted/1 (and the :optional path) require a map. Non-map inputs
      # (nil, atoms, integers, etc.) are not valid descriptors; return nil silently
      # rather than logging a debug warning on every call with garbage input.
      try do
        # not_set() has an :optional key that to_quoted doesn't handle.
        # is_map(descr) is guaranteed by the outer `and is_map(descr)` guard.
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
      # Special types
      {:term, [], []} ->
        nil

      {:none, [], []} ->
        :none

      # dynamic() — the gradual top carries no information, so at the shape
      # boundary it is "unknown" (nil): consumers then fall back to the
      # structural engine. Compiler-fidelity display of `dynamic(...)` is
      # preserved separately via `descr_to_string/1`, which renders the raw
      # descr; shapes feed the Binding algebra, which has no gradual marker.
      {:dynamic, [], []} ->
        nil

      # dynamic(inner) — unwrap to the inner shape for the same reason.
      # Note: `Descr.to_quoted/1` only emits this `{:dynamic, [], [inner]}`
      # form when the inner part is itself non-trivial (e.g. a struct);
      # `dynamic(integer())` quotes to a bare `{:integer, [], []}`, so those
      # stay unwrapped automatically.
      {:dynamic, [], [inner]} ->
        quoted_to_shape(inner)

      # Primitive types
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

      # closed `%{}` / empty_map() — distinct from the map top type (task #22).
      {:empty_map, [], []} ->
        :empty_map

      # boolean() — keep as a dedicated shape (task #22); the compiler prints
      # `boolean()` rather than decomposing it to `false | true`.
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

      # Function types
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
            # When all args are :none and return is nil (term), this is just {:fun, arity}
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

      # List types (task #22 — preserve non-emptiness; drop improper-tail forms)
      {:list, [], [elem]} ->
        {:list, quoted_to_shape(elem)}

      {:non_empty_list, [], [elem]} ->
        {:nonempty_list, quoted_to_shape(elem)}

      # non_empty_list with explicit (non-list) tail — improper list, which we
      # cannot represent; degrade to nil rather than claiming a proper list.
      {:non_empty_list, [], [_elem, _tail]} ->
        nil

      # Tuple types. The top type `tuple()` quotes to a single open marker
      # `{:{}, [], [{:..., [], nil}]}` → `:tuple` (don't claim arity 0). Tuples
      # carrying a trailing `{:..., [], nil}` are open → `{:tuple_open, shapes}`;
      # closed tuples → `{:tuple, n, shapes}` (task #7).
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
      # nil rather than silently dropping the member (task #9) — a dropped member
      # would make the displayed type narrower than the truth.
      {:or, [], [_left, _right]} = union ->
        members = flatten_quoted_union(union)
        shapes = Enum.map(members, &quoted_to_shape/1)

        cond do
          Enum.any?(shapes, &is_nil/1) -> nil
          match?([_single], shapes) -> hd(shapes)
          true -> {:union, shapes}
        end

      # Intersection types — function intersections become multi-clause funs.
      # Non-function intersections are degraded to nil (task #9): picking one
      # convertible branch would make the displayed type broader than reality.
      {:and, [], [_left, _right]} = intersection ->
        case extract_fun_clauses_from_intersection(intersection) do
          [_ | _] = fun_clauses -> {:fun_clauses, fun_clauses}
          [] -> nil
        end

      # Negation types — unrepresentable as a shape (task #9).
      {:not, [], [_inner]} ->
        nil

      # Struct quoted form `%Struct{...}` (descr.ex map_literal_to_quoted emits
      # this for loaded, complete structs). task #19 — biggest display-loss fix.
      {:%, _, [module_alias, {:%{}, _, fields}]} ->
        quoted_struct_to_shape(module_alias, fields)

      # Optional map field value
      {:if_set, [], [inner]} ->
        {:optional, quoted_to_shape(inner)}

      # Literal atoms wrapped in __block__
      {:__block__, [], [atom]} when is_atom(atom) ->
        {:atom, atom}

      # Literal integers wrapped in __block__
      {:__block__, [], [integer]} when is_integer(integer) ->
        {:integer, integer}

      # Module aliases in struct __struct__ fields
      {:__aliases__, [], parts} when is_list(parts) ->
        {:atom, Module.concat(parts)}

      # Open map/tuple marker
      {:..., [], _} ->
        nil

      # Bare literal atoms (unlikely from to_quoted but handle for safety)
      atom when is_atom(atom) ->
        {:atom, atom}

      # Bare literal integers
      integer when is_integer(integer) ->
        {:integer, integer}

      # Fallback
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
    # Extract field key-value pairs. Non-atom (domain) keys like
    # `integer() => binary()` are preserved as `{{:domain, key_shape}, value}`
    # rather than collapsed to a nil key. The open-map marker `{:..., [], nil}`
    # is a bare 3-tuple element, not a `{key, value}` pair, so the `{key, value}`
    # generator already skips it — we treat all maps as open in coercion anyway
    # (task #22), so the previous explicit 2-tuple filter was dead code.
    kv_pairs =
      for {key_quoted, value_quoted} <- fields do
        case extract_quoted_map_key(key_quoted) do
          nil -> {{:domain, quoted_to_shape(key_quoted)}, value_quoted}
          atom_key -> {atom_key, value_quoted}
        end
      end

    # Check if this is a struct (has __struct__ field with a module atom)
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

      {:struct, field_shapes, {:atom, struct_module}, nil}
    else
      field_shapes =
        for {key, value_quoted} <- kv_pairs,
            do: {key, quoted_to_shape(value_quoted)}

      {:map, field_shapes, nil}
    end
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
      # Keep :none
      {:none, _} ->
        :none

      # Use new if existing is nil
      {nil, new} ->
        new

      # Keep existing if new is nil or :none
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

      # Enhanced list element type merging
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

  # Helper function for merging map fields
  defp merge_map_fields(fields1, fields2) do
    # Create maps for easier merging
    map1 = Map.new(fields1)
    map2 = Map.new(fields2)

    # Get all unique keys
    all_keys = (Map.keys(map1) ++ Map.keys(map2)) |> Enum.uniq()

    # Merge field by field
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

    # Convert back to keyword list format
    Enum.map(merged_map, fn {key, value} -> {key, value} end)
  end

  # Helper functions for shape conversion

  # -- Variables seeding helpers ------------------------------------------------

  # Convert user-provided variables map into Module.Types context vars map.
  # Input keys are {name, version}; output keys are version only.
  defp variables_to_context_vars(nil), do: nil

  defp variables_to_context_vars(vars) when is_map(vars) do
    Enum.reduce(vars, %{}, fn
      {{name, version}, type_like}, acc when is_atom(name) and is_integer(version) ->
        type_descr = coerce_var_type(type_like)
        # Mirror the fresh-variable shape from Module.Types.Of (Elixir 1.20 added
        # the `paths` and `deps` fields used by cross-variable refinement).
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

  # Accept Module.Types.Descr, sentinel atoms, or ElixirSense minimal shapes
  # and coerce into a Descr.t()
  def coerce_var_type_public(type_like), do: coerce_var_type(type_like)

  # task #10: Binding shapes are best-effort. Seeding them as *static* descrs
  # makes the typesystem treat them as certain, so intersections/matches collapse
  # to none() and valid clauses get filtered out. The compiler wraps inferred
  # values in `dynamic/1`; mirror that by dynamic-wrapping every coerced shape.
  # (Already-built descrs passed in by callers are taken as-is — the caller owns
  # that contract — and the dynamic sentinels are idempotent.)
  defp coerce_var_type(%{} = descr), do: descr
  defp coerce_var_type(:dynamic), do: Descr.dynamic()
  defp coerce_var_type(:term), do: Descr.term()
  defp coerce_var_type(nil), do: Descr.dynamic()

  defp coerce_var_type(shape), do: Descr.dynamic(coerce_static_descr(shape))

  # Inner coercion: shape -> static descr. Always dynamic-wrapped by
  # coerce_var_type/1 (task #10) — never call this directly for a seed value.
  # Already-built descrs pass through as-is (same contract as coerce_var_type);
  # nil (unknown) stays gradual so clause selection treats it as matching all.
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
  # task #21: prefer the real bitstring() descr when the running Descr exposes
  # it (1.20 quotes `bitstring()`); fall back to binary() on older Elixirs.
  defp coerce_static_descr(:bitstring), do: bitstring_descr()
  defp coerce_static_descr(:float), do: Descr.float()
  defp coerce_static_descr(:pid), do: Descr.pid()
  defp coerce_static_descr(:port), do: Descr.port()
  defp coerce_static_descr(:reference), do: Descr.reference()
  defp coerce_static_descr(:tuple), do: Descr.tuple()
  defp coerce_static_descr(:fun), do: Descr.fun()
  defp coerce_static_descr(:none), do: Descr.none()
  # task #21: non_struct_map has no descr constructor; an open map is the closest
  # representation (any map at all, struct or not).
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

  defp coerce_static_descr({:tuple, _arity, elements}) when is_list(elements) do
    Descr.tuple(Enum.map(elements, &coerce_var_type/1))
  end

  # Open tuple — we know a prefix but not the full arity; build an open tuple.
  defp coerce_static_descr({:tuple_open, elements}) when is_list(elements) do
    Descr.open_tuple(Enum.map(elements, &coerce_var_type/1))
  end

  defp coerce_static_descr({:map, fields, _}) when is_list(fields) do
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
    # fun_clauses is multi-clause function — coerce as generic fun with arity of first clause
    case clauses do
      [{args, _return} | _] when is_list(args) -> fun_descr(length(args))
      _ -> Descr.fun()
    end
  end

  # task #20: a `dynamic()` marker shape coerces to the gradual top.
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

  # Build a map descriptor from plain-map shape fields. task #10: always build an
  # OPEN map — Binding shapes only know a subset of keys, and a closed_map would
  # assert all other keys absent, making the intersection with the real (open or
  # fuller) map empty and filtering out valid clauses.
  defp coerce_map_descr(fields, extra_pairs) do
    atom_pairs =
      extra_pairs ++ for({k, v} when is_atom(k) <- fields, do: {k, coerce_field_value(v)})

    Descr.open_map(atom_pairs)
  end

  # Map-field values preserve optionality: an {:optional, inner} shape coerces
  # to if_set(inner) so maps lacking the key stay included in the descr (the
  # compiler-parity round-trip caught the unwrapping as a soundness loss).
  defp coerce_field_value({:optional, inner}), do: Descr.if_set(coerce_var_type(inner))
  defp coerce_field_value(v), do: coerce_var_type(v)

  # Build a struct descriptor. task #10: when the struct module is loaded we can
  # expand the FULL defstruct field set and build a precise closed_map (known
  # field types layered over the complete key set); otherwise fall back to an
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

  # task #21: real bitstring() descr when available, else binary().
  defp bitstring_descr do
    if function_exported?(Module.Types.Descr, :bitstring, 0) do
      # credo:disable-for-next-line Credo.Check.Refactor.Apply
      apply(Module.Types.Descr, :bitstring, [])
    else
      Descr.binary()
    end
  end

  # Descr.fun/1 (arity-specific function descr) is 1.20+. On 1.18/1.19 fall back
  # to the generic `fun/0`. `apply/3` avoids an undefined-function warning there.
  defp fun_descr(arity) do
    if function_exported?(Module.Types.Descr, :fun, 1) do
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
                  # Seed `_` too: native versions and refines each underscore, so
                  # it must be present in the context (otherwise `refine_body_var`
                  # crashes on `{_, false}`-style patterns typed in isolation).
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
    Enum.reduce_while(clauses, [], fn clause, acc ->
      %{meta: meta, args: args, guards: guards, body: body} = normalise_clause(clause)
      body = body |> ensure_body_var_versions() |> replace_module_attributes()

      # Seed every variable referenced in the clause (args + guards + body) into
      # the context as `:dynamic`. We type the body in isolation, so a body var
      # referencing an earlier binding (`mod = polymod(values)`) would otherwise
      # crash native `refine_body_var` on a not-yet-present version.
      seeded_context =
        init_context(collect_versioned_vars([args, guards || [], body])) || context

      try do
        # of_head / of_domain shapes differ across 1.18/1.19/1.20; the helpers
        # dispatch on pattern_api() (see their definitions). `head_kind` is
        # :trees (1.19/1.20, needs of_domain) or :types (1.18, types as-is).
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

  # Module.Types.Pattern.of_pattern requires every pattern variable — including
  # underscores — to carry a `:version` in its meta (Keyword.fetch!/2). Named
  # vars coming from the metadata builder are already versioned; this is a
  # safety net for any that slipped through. Each `_` is independent, so we hand
  # them unique versions from a high base (well above the small versions the
  # compiler assigns) to avoid aliasing them to each other or to real vars.
  #
  # RESERVED SYNTHETIC VERSION RANGES (must never collide with, nor leak as,
  # compiler/metadata versions — real compiler/metadata versions are small,
  # densely allocated integers starting near 0):
  #
  #   * `@synthetic_version_base` (1_000_000) and up — versions stamped by
  #     `ensure_body_var_versions/1` onto otherwise-unversioned pattern/body
  #     vars (underscores and any named var that slipped through unversioned).
  #   * `@attr_placeholder_version_base` (2_000_000) and up — versions used by
  #     `replace_module_attributes/1` for `@attr` placeholder vars (kept above
  #     the synthetic base so a placeholder can't alias a real variable slot).
  #
  # These versions exist purely so native `Pattern.of_match`/`of_pattern` (which
  # `Keyword.fetch!`es a `:version`) doesn't crash. They are NOT real VarInfo
  # versions: a consumer keying refinement results back onto `VarInfo`
  # (`merge_pattern_types/2` in compiler.ex) would otherwise mint a bogus
  # `{name, synthetic_version}` binding that no real variable holds. Therefore
  # `extract_refined_var_shapes/5` filters every synthetic-keyed entry out of its
  # result via `synthetic_version?/1` before returning.
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
  # context vars are keyed by version only — `Of.declare_var` does
  # `Map.put(vars, version, data)`) doesn't alias distinct names. task #11:
  # previously ALL unversioned named vars were stamped `version: 0`, so two
  # different names collapsed into one context slot and cross-contaminated each
  # other's refinements. We now assign each distinct *name* its own version via
  # a `name => version` map (repeated occurrences of the same name share it),
  # mirroring how each underscore already gets a unique version.
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
          # Replace @attr with a fresh variable that of_expr can handle. Use a
          # version base well above the @underscore_version_base range that
          # ensure_body_var_versions now draws from for both underscores AND
          # distinct named vars (task #11), so attr placeholders can't alias a
          # real variable's context slot.
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
          %{type: type} ->
            kind = get_def_kind_for_types(type)

            # Pick the most-trusted signature by EXPLICIT provenance rather than
            # by the sig's kind tag. Provenance ordering (highest first):
            #   :exck     — read from the compiled .beam ExCk chunk (ground truth)
            #   :inferred — natively inferred locally from the function body
            #   :spec     — derived from a @spec (a fallback, may be imprecise)
            # The stored sig carries its source on ModFunInfo
            # (`elixir_types_sig_source` ∈ {:exck, :inferred}); spec sigs are
            # always tagged `:spec`. We do NOT rely on the kind tag here: spec
            # sigs are always `:infer`, so the old `{:strong, ...}`
            # spec-precedence clause was dead and a hypothetical :strong spec
            # must still NOT outrank a native-inferred sig.
            stored_sig = Map.get(info, :elixir_types_sig)
            stored_source = Map.get(info, :elixir_types_sig_source)

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

  # Trust ranking for signature provenance (lower = more trusted, picked first
  # by `Enum.min_by`). :exck (ExCk chunk ground truth) > :inferred (native
  # local inference) > :spec (@spec-derived fallback). Unknown/nil sources sort
  # last so a real provenance always wins.
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

  # Number of clauses above which the compiler stops unioning returns and just
  # returns `dynamic()` (Module.Types.Apply `@max_clauses`, apply.ex:16). Mirror
  # it so we don't do unbounded work on huge inferred signatures.
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
    # Mirror the compiler's wrapping precisely. `apply_infer` (apply.ex:1827)
    # ALWAYS `dynamic()`-wraps its inferred union — it does not consult
    # `return/3`. Only `apply_strong` routes its return through `return/3`
    # (apply.ex:1834,1844), which wraps only when an argument is gradual (in the
    # dynamic-flavored mode ElixirSense always uses; the `:static`-mode branch of
    # `return/3` never applies here). With no argument information
    # (`arg_descrs == nil`) we have no static facts to narrow on, so we treat the
    # call as gradual (wrap).
    gradual_args? = arg_descrs == nil or Enum.any?(arg_descrs, &descr_gradual?/1)

    cond do
      # No argument information at all: consider every clause (as if every arg
      # were gradual). For :infer this is the dynamic()-wrapped union of all
      # returns; for :strong, ditto (the domain check is trivially satisfied by
      # absent args). Mirrors compiler behavior with fully-gradual args, which
      # always wrap.
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

  # Extract return type from a signature, applied to the given argument shapes.
  #
  # Thin descr-returning wrapper over `apply_signature/2` kept for source
  # compatibility with existing callers (Binding) that expect a descr and then
  # `to_shape/1` it. `apply_signature/2`'s `:error` (ill-typed call) is mapped to
  # `Descr.dynamic()`, whose shape is `nil` — so the caller's existing nil-check
  # triggers a fall back to structural typing instead of receiving an invented
  # union of all clause returns. `:none`/unknown sigs likewise yield `dynamic()`.
  def extract_return_type_from_sig(sig, arg_shapes \\ nil)

  def extract_return_type_from_sig(sig, arg_shapes) do
    case apply_signature(sig, arg_shapes) do
      {:ok, descr} -> descr
      :error -> Descr.dynamic()
    end
  end

  # Coerce argument shapes into STATIC descriptors for clause selection. The
  # dynamic() wrap used when seeding var types (coerce_var_type) is wrong here:
  # a dynamic-wrapped arg has an empty static part, so `only_gradual?`/
  # `compatible?` treat it as matching every clause and selection can never
  # narrow. The compiler also selects a single clause when an argument is
  # statically known. Unknown shapes (nil) still coerce to bare dynamic() via
  # coerce_static_descr's catch-all, so they remain gradual and match all
  # clauses. Returns `nil` when there is no usable argument information so the
  # caller treats every clause as a candidate, or when coercion fails.
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
  # dynamic-wrap. > @max_clauses ⇒ bare dynamic(). Mirrors apply_infer with
  # fully-gradual args (every clause selected), which always wraps
  # (apply.ex:1827).
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

  # Mirror apply.ex:1818-1829 `apply_infer/2`: select clauses whose argument
  # domain is per-position NOT disjoint with the actual args. Zero matches ⇒
  # :error (ill-typed). > @max_clauses matches ⇒ bare dynamic(). Otherwise the
  # union of the matched clauses' returns, ALWAYS wrapped in dynamic()
  # (apply.ex:1827 wraps unconditionally — it does NOT consult `return/3`).
  defp apply_infer_mirror(clauses, arg_descrs) do
    matched =
      Enum.filter(clauses, fn clause ->
        args_not_disjoint?(clause_arg_types(clause), arg_descrs)
      end)

    cond do
      matched == [] ->
        :error

      length(matched) > @max_clauses ->
        {:ok, Descr.dynamic()}

      true ->
        case extract_return_type_from_clauses(matched) do
          {:ok, return_type} -> {:ok, wrap_dynamic(return_type)}
          :error -> {:ok, Descr.dynamic()}
        end
    end
  end

  # Mirror apply.ex:1831-1848 `apply_strong/4`: each actual arg must be
  # compatible with (a subtype of, modulo gradual) the clause's expected domain.
  # We treat each clause's argument tuple as its domain (the signatures we
  # produce have non-overlapping :strong domains). A clause contributes its
  # return only when args are compatible; if NO clause is compatible the call is
  # a domain violation ⇒ :error. Returns are unioned and routed through
  # `return/3` (apply.ex:1834,1844 — wrapped in dynamic() only when an arg is
  # gradual), unlike apply_infer which always wraps.
  defp apply_strong_mirror(clauses, arg_descrs, gradual_args?) do
    matched =
      Enum.filter(clauses, fn clause ->
        args_compatible?(clause_arg_types(clause), arg_descrs)
      end)

    cond do
      matched == [] ->
        :error

      length(matched) > @max_clauses ->
        {:ok, Descr.dynamic()}

      true ->
        case extract_return_type_from_clauses(matched) do
          {:ok, return_type} -> {:ok, maybe_wrap_dynamic(return_type, gradual_args?)}
          :error -> {:ok, Descr.dynamic()}
        end
    end
  end

  # Mirror `Apply.return/3` (apply.ex:1732-1741): only wrap the matched return in
  # `dynamic()` when an argument is gradual. When all args are statically known
  # the bare static return is returned, matching what the compiler records for a
  # static call. `gradual_args?` is computed once in `apply_signature/2`.
  defp maybe_wrap_dynamic(descr, true), do: wrap_dynamic(descr)
  defp maybe_wrap_dynamic(descr, false), do: descr

  # Wrap a return type in dynamic() (idempotent for already-gradual types), as
  # the compiler does for inferred/applied returns when an arg is gradual
  # (apply.ex:1738).
  defp wrap_dynamic(descr) do
    Descr.dynamic(descr)
  rescue
    _ -> descr
  catch
    _, _ -> descr
  end

  # Mirror of the compiler's `Descr.gradual?/1` (descr.ex:385-386) with a safe
  # fallback. `gradual?` is `def` in the source but not exported by the running
  # stdlib, so we probe and otherwise check the `:dynamic` key directly. `:term`
  # (the only non-map descr) is not gradual.
  defp descr_gradual?(:term), do: false

  defp descr_gradual?(descr) do
    cond do
      function_exported?(Module.Types.Descr, :gradual?, 1) ->
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

  # apply_infer clause selection: per-position non-disjoint check
  # (apply.ex:1875 `zip_not_disjoint?`). When the clause arity doesn't match the
  # args, it can't be selected. A clause with unknown arg types (nil) is kept as
  # a candidate (we can't prove it disjoint).
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

  # apply_strong domain check: each actual arg compatible with the expected
  # domain (apply.ex:1869 `zip_compatible?` via `compatible?/2`). Gradual args
  # are compatible with anything (only_gradual?). Falls back to a non-disjoint
  # check if `compatible?/2` is unavailable.
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
    if function_exported?(Module.Types.Descr, :disjoint?, 2) do
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
      function_exported?(Module.Types.Descr, :only_gradual?, 1) and
          apply(Module.Types.Descr, :only_gradual?, [arg]) ->
        true

      function_exported?(Module.Types.Descr, :compatible?, 2) ->
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
