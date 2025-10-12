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

  - **TypeInference**: Enhanced expression typing for literals and AST nodes
  - **Compiler**: Pattern matching refinement (stubbed in M1)

  ## Examples

      # Check availability
      ElixirTypes.available?()
      #=> true

      # Type an expression
      ElixirTypes.of_expr(42)
      #=> {:ok, %{bitmap: 4}}

      # Convert to ElixirSense shape
      {:ok, descr} = ElixirTypes.of_expr([1, 2, 3])
      ElixirTypes.to_shape(descr)
      #=> {:list, {:integer, nil}}

      # Shape merging
      ElixirTypes.merge_shapes({:integer, nil}, {:integer, 42})
      #=> {:integer, 42}

  ## Limitations (M1)

  - Local function calls remain dynamic (no local_handler implementation)
  - Remote function calls remain dynamic (no ExCk cache)
  - Pattern matching relies on best-effort conversion and may skip complex types
  - Conservative shape conversion (only handles clear, simple cases)

  These limitations will be addressed in future milestones (M2-M4).
  """

  require Logger

  @doc """
  Returns true if Module.Types.Expr is available and has of_expr/5.

  This function checks if the current Elixir installation includes the
  Module.Types infrastructure required for the adaptor.

  ## Examples

      iex> ElixirTypes.available?()
      true

  """
  def available?() do
    Code.ensure_loaded?(Module.Types.Expr) and
      function_exported?(Module.Types.Expr, :of_expr, 5)
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
  def enabled?() do
    Application.get_env(:elixir_sense, :use_elixir_types, false) and available?()
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
        _metadata \\ nil
      ) do
    if available?() do
      local_handler =
        if local_sigs_map && map_size(local_sigs_map) > 0 do
          local_handler_from(local_sigs_map)
        else
          &__MODULE__.local_handler/4
        end

      remote_handler = remote_handler_from()

      Module.Types.stack(
        mode,
        file || "nofile",
        module || ElixirSense.ElixirTypes,
        function || {:__info__, 1},
        :all,
        nil,
        local_handler
      )
      |> Map.put(:remote_handler, remote_handler)
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
  - `%{{atom, non_neg_integer} => Module.Types.Descr.t()}`
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
  Local handler for Module.Types - always returns false in M1.

  This will be expanded in M2 to use ElixirSense's module metadata.
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
  - `:mode` - Typing mode, :dynamic (default) or :traversal
  - `:local_sigs_map` - Optional local signatures map
  - `:metadata` - Optional metadata
  - `:variables` - Optional map of variables with keys `{name, version}`

  ## Returns

  - `{:ok, descr}` - Success with Module.Types descriptor
  - `:error` - Failure (Module.Types unavailable, invalid AST, etc.)

  ## Examples

      # Type a literal
      iex> ElixirTypes.of_expr(42)
      {:ok, %{bitmap: 4}}

      # Type a variable with known type
      iex> ElixirTypes.of_expr({:foo, [version: 0], nil}, variables: %{{:foo, 0}: => Module.Types.Descr.integer()})
      {:ok, %{bitmap: 4}}

      # Type a list
      iex> ElixirTypes.of_expr([1, 2, 3])
      {:ok, %{list: [{%{bitmap: 4}, %{bitmap: 2}, []}]}}

      # Type a tuple (requires AST form)
      iex> ElixirTypes.of_expr({:{}, [], [1, :ok]})
      {:ok, %{tuple: [closed: [%{bitmap: 4}, %{atom: {:union, %{ok: []}}}]]}}

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
    track_performance(:of_expr, fn ->
      if available?() do
        try do
          case maybe_local_call_descriptor(ast, local_sigs_map) do
            nil ->
              case maybe_remote_call_descriptor(ast, metadata) do
                nil ->
                  if mode == :traversal && is_simple_ast?(ast) do
                    # Fast path for simple AST nodes in traversal mode
                    track_fast_path_hit()
                    simple_type_of(ast)
                  else
                    # Full Module.Types processing
                    stack = init_stack(module, function, file, mode, local_sigs_map, metadata)
                    # Seed context with provided variables or infer from AST (dynamic types)
                    auto_vars = variables_from_ast(ast)
                    effective_vars = merge_variables(variables, auto_vars)
                    context = init_context(effective_vars)

                    if stack && context do
                      {descr, _context} =
                        Module.Types.Expr.of_expr(
                          ast,
                          Module.Types.Descr.term(),
                          ast,
                          stack,
                          context
                        )

                      {:ok, descr}
                    else
                      :error
                    end
                  end

                descriptor ->
                  {:ok, descriptor}
              end

            descriptor ->
              {:ok, descriptor}
          end
        catch
          kind, payload ->
            Logger.warning("Unable to infer type of #{inspect(ast)}: #{Exception.format(kind, payload, __STACKTRACE__)}")
            :error
        end
      else
        :error
      end
    end)
  end

  # Performance optimization: fast path for simple AST nodes in traversal mode
  defp is_simple_ast?(ast) do
    case ast do
      # Literals
      x when is_atom(x) or is_integer(x) or is_float(x) or is_binary(x) ->
        true

      [] ->
        true

      # Lists can be complex
      [_ | _] ->
        false

      {a, b}
      when (is_atom(a) or is_integer(a) or is_float(a) or is_binary(a)) and
             (is_atom(b) or is_integer(b) or is_float(b) or is_binary(b)) ->
        true

      _ ->
        false
    end
  end

  # Simple type inference for basic literals (fast path)
  defp simple_type_of(ast) do
    case ast do
      x when is_atom(x) ->
        {:ok, Module.Types.Descr.atom(x)}

      x when is_integer(x) ->
        {:ok, Module.Types.Descr.integer()}

      x when is_float(x) ->
        {:ok, Module.Types.Descr.float()}

      x when is_binary(x) ->
        {:ok, Module.Types.Descr.binary()}

      [] ->
        # Empty list
        {:ok, Module.Types.Descr.term()}

      {_a, _b} ->
        # Simple 2-tuple
        {:ok, Module.Types.Descr.term()}

      _ ->
        :error
    end
  rescue
    _ -> :error
  end

  @doc """
  Performance metrics for ElixirTypes operations.

  Returns a map with timing and call count statistics.
  """
  def get_performance_metrics do
    if enabled?() do
      Process.get(:elixir_types_metrics, %{
        of_expr_calls: 0,
        of_expr_total_time: 0,
        of_match_calls: 0,
        of_match_total_time: 0,
        to_shape_calls: 0,
        to_shape_total_time: 0,
        fast_path_hits: 0,
        cache_hits: 0,
        cache_misses: 0
      })
    else
      %{}
    end
  end

  @doc """
  Reset performance metrics.
  """
  def reset_performance_metrics do
    Process.delete(:elixir_types_metrics)
    :ok
  end

  # Performance tracking helpers
  defp track_performance(operation, func) do
    if enabled?() do
      start_time = System.monotonic_time(:microsecond)
      result = func.()
      end_time = System.monotonic_time(:microsecond)
      duration = end_time - start_time

      update_metrics(operation, duration)
      result
    else
      func.()
    end
  end

  defp update_metrics(operation, duration) do
    metrics = get_performance_metrics()
    calls_key = :"#{operation}_calls"
    time_key = :"#{operation}_total_time"

    updated_metrics =
      metrics
      |> Map.update(calls_key, 1, &(&1 + 1))
      |> Map.update(time_key, duration, &(&1 + duration))

    Process.put(:elixir_types_metrics, updated_metrics)
  end

  defp track_cache_hit do
    metrics = get_performance_metrics()
    Process.put(:elixir_types_metrics, Map.update(metrics, :cache_hits, 1, &(&1 + 1)))
  end

  defp track_cache_miss do
    metrics = get_performance_metrics()
    Process.put(:elixir_types_metrics, Map.update(metrics, :cache_misses, 1, &(&1 + 1)))
  end

  defp track_fast_path_hit do
    metrics = get_performance_metrics()
    Process.put(:elixir_types_metrics, Map.update(metrics, :fast_path_hits, 1, &(&1 + 1)))
  end

  defp maybe_local_call_descriptor({fun, _meta, args}, local_sigs_map)
       when is_atom(fun) and is_list(args) and is_map(local_sigs_map) do
    with {kind, sig} when kind in [:def, :defp] <- Map.get(local_sigs_map, {fun, length(args)}),
         descriptor when not is_nil(descriptor) <- descriptor_from_signature(sig) do
      descriptor
    else
      _ -> nil
    end
  end

  defp maybe_local_call_descriptor(_, _), do: nil

  defp descriptor_from_signature({sig_kind, _domain, clauses})
       when sig_kind in [:infer, :strong] and is_list(clauses) do
    case extract_return_type_from_clauses(clauses) do
      {:ok, return_type} -> return_type
      :error -> nil
    end
  end

  defp descriptor_from_signature(:none), do: nil
  defp descriptor_from_signature(_), do: nil

  def maybe_remote_call_shape(ast, metadata) do
    case maybe_remote_call_descriptor(ast, metadata) do
      nil ->
        :error

      descriptor ->
        case to_shape(descriptor) do
          nil -> :error
          shape -> {:ok, shape}
        end
    end
  end

  defp maybe_remote_call_descriptor(
         {{:., _, [target_ast, fun]}, _meta, args},
         _metadata
       )
       when is_atom(fun) and is_list(args) do
    if enabled?() do
      case module_from_ast(target_ast) do
        {:ok, module} ->
          case ElixirSense.Core.ExCkReader.lookup_signature(module, fun, length(args)) do
            {:ok, info} -> descriptor_from_exck(info)
            :error -> nil
          end

        :error ->
          nil
      end
    else
      nil
    end
  end

  defp maybe_remote_call_descriptor(_ast, _metadata), do: nil

  defp descriptor_from_exck(%{sig: sig_info}) do
    extract_return_type_from_sig(sig_info)
  end

  defp descriptor_from_exck(_), do: nil

  defp module_from_ast(atom) when is_atom(atom) do
    {:ok, atom}
  end

  defp module_from_ast({:__aliases__, _, parts}) when is_list(parts) do
    try do
      {:ok, Module.concat(parts)}
    rescue
      ArgumentError -> :error
    end
  end

  defp module_from_ast(_), do: :error

  @doc """
  Types a pattern match to refine variable types.

  Returns `{:ok, map}` with entries keyed by `{var_name, version}` and values
  converted to ElixirSense shapes. Returns `:error` when typing fails or
  Module.Types cannot be used.
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
    track_performance(:of_match, fn ->
      if available?() do
        targets = targets_from_opts(opts)

        try do
          stack = init_stack(module, function, file, mode)

          if stack do
            stack = %{stack | refine_vars: true}
            context = init_context(Keyword.get(opts, :variables))

            {pattern_ast, value_ast, full_match} = normalize_match(pattern_ast, match_ast)
            expected_descr = expected_descr || Module.Types.Descr.term()

            # Enhanced pattern matching refinement
            result =
              perform_enhanced_match(
                pattern_ast,
                value_ast,
                expected_descr,
                full_match,
                stack,
                context,
                targets
              )

            case result do
              {:ok, var_shapes, var_descrs} ->
                # Keep raw Module.Types descriptors for vars so the compiler can store them
                maybe_store_var_descriptors(opts, var_descrs)
                {:ok, var_shapes}

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
    end)
  end

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
      value_type =
        Module.Types.Expr.of_expr(
          value_ast,
          Module.Types.Descr.term(),
          full_match,
          stack,
          context
        )

      # Use more refined expected descriptor if value typing succeeded
      refined_expected =
        case value_type do
          {:ok, {type_descr, _}} -> Module.Types.Descr.intersection(expected_descr, type_descr)
          _ -> expected_descr
        end

      expected_fun = fn _pattern_type, ctx ->
        Module.Types.Expr.of_expr(value_ast, refined_expected, full_match, stack, ctx)
      end

      case Module.Types.Pattern.of_match(pattern_ast, expected_fun, full_match, stack, context) do
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
      _ -> :error
    catch
      _ -> :error
    end
  end

  # Fallback to original pattern matching approach
  defp fallback_match(pattern_ast, value_ast, expected_descr, full_match, stack, context, targets) do
    try do
      expected_fun = fn _pattern_type, ctx ->
        Module.Types.Expr.of_expr(value_ast, expected_descr, full_match, stack, ctx)
      end

      {_type, %{vars: vars_map} = out_ctx} =
        Module.Types.Pattern.of_match(pattern_ast, expected_fun, full_match, stack, context)

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
      _ -> :error
    catch
      _ -> :error
    end
  end

  # If caller passes a state via opts, persist raw Module.Types var descriptors
  defp maybe_store_var_descriptors(opts, var_descrs) when is_map(var_descrs) do
    case Keyword.get(opts, :state) do
      %ElixirSense.Core.Compiler.State{} = state ->
        ElixirSense.Core.Compiler.State.merge_inferred_elixir_types(state, var_descrs)
        :ok

      _ ->
        :ok
    end
  end

  defp maybe_store_var_descriptors(_opts, _), do: :ok

  # Transform Module.Types context vars into a map of {name, version} => descr
  defp vars_ctx_to_descrs(%{vars: vars_map}) when is_map(vars_map) do
    Enum.into(vars_map, %{}, fn {version, %{name: name, type: descr}} ->
      {{name, version}, descr}
    end)
  end

  defp vars_ctx_to_descrs(_), do: %{}

  # Enhanced variable shape extraction with additional type refinement
  defp extract_refined_var_shapes(vars_map, targets, pattern_ast, expected_descr, value_ast) do
    base_shapes = extract_var_shapes(vars_map, targets)

    # Apply pattern-specific type refinements
    refined_shapes =
      apply_pattern_refinements(base_shapes, pattern_ast, expected_descr, value_ast)

    base_shapes
    |> Map.merge(refined_shapes)
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

      # List pattern refinement
      list when is_list(list) ->
        refine_list_pattern_vars(var_shapes, list, expected_descr, value_ast)

      _ ->
        %{}
    end
  end

  # Refine variables in struct patterns
  defp refine_struct_pattern_vars(_var_shapes, struct_ast, _fields, _expected_descr, _value_ast) do
    case struct_ast do
      {var_name, meta, nil} when is_atom(var_name) ->
        # Variable bound to struct module - refine to atom type
        case Keyword.get(meta, :version) do
          nil -> %{}
          version -> %{{var_name, version} => {:atom, nil}}
        end

      _ ->
        %{}
    end
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
              nested_refinements = apply_pattern_refinements(var_shapes, var_ast, nil, field_value_ast)
              Map.merge(acc, nested_refinements)
            else
              acc
            end

          var_key ->
            key_literal = literal_from_key_ast(key_ast)

            field_shape =
              cond do
                key_literal != nil && Map.has_key?(value_field_map, key_literal) ->
                  Map.get(value_field_map, key_literal)

                true ->
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
        element_shape =
          case value_list do
            [head | _] -> generalize_shape(shape_from_ast(head))
            _ -> nil
          end

        if element_shape do
          Enum.reduce(elements, %{}, fn element_ast, acc ->
            case extract_var_from_ast(element_ast) do
              nil -> acc
              var_key -> Map.put(acc, var_key, element_shape)
            end
          end)
        else
          %{}
        end

      _ ->
        %{}
    end
  end

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
    Enum.reduce(vars_map, vars_map, fn
      {{var, version}, shape}, acc when is_atom(var) and is_integer(version) and version > 1 ->
        primary_key = {var, 1}

        if Map.has_key?(acc, primary_key) do
          acc
        else
          Map.put(acc, primary_key, shape)
        end

      _, acc ->
        acc
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

  ## Options

  - `:lazy` - When true, uses lazy evaluation and caching for expensive conversions
  """
  def to_shape(descr, opts \\ []) do
    track_performance(:to_shape, fn ->
      if Keyword.get(opts, :lazy, false) do
        to_shape_lazy(descr, opts)
      else
        to_shape_eager(descr)
      end
    end)
  end

  # Lazy shape conversion with caching
  defp to_shape_lazy(descr, _opts) do
    # Use a simple hash-based cache for M2
    cache_key = :erlang.phash2(descr)

    case Process.get({:elixir_types_shape_cache, cache_key}) do
      nil ->
        track_cache_miss()
        result = to_shape_eager(descr)
        # Cache result with TTL
        Process.put(
          {:elixir_types_shape_cache, cache_key},
          {result, System.monotonic_time(:millisecond)}
        )

        result

      {cached_result, timestamp} ->
        # Check if cache entry is still fresh (5 minute TTL)
        if System.monotonic_time(:millisecond) - timestamp < 300_000 do
          track_cache_hit()
          cached_result
        else
          # Cache expired, recompute
          track_cache_miss()
          Process.delete({:elixir_types_shape_cache, cache_key})
          result = to_shape_eager(descr)

          Process.put(
            {:elixir_types_shape_cache, cache_key},
            {result, System.monotonic_time(:millisecond)}
          )

          result
        end
    end
  end

  # Original eager shape conversion (renamed for clarity)
  defp to_shape_eager(descr) do
    if available?() do
      try do
        # Check for fun_from_inferred_clauses BEFORE unwrapping
        # (has both :dynamic and :fun keys, unwrapping would lose this info)
        fun_inferred_shape =
          if Map.has_key?(descr, :dynamic) and Map.has_key?(descr, :fun) do
            case Map.get(descr, :dynamic) do
              %{fun: {args, return_type}} when is_list(args) ->
                arg_shapes = Enum.map(args, &to_shape/1)
                return_shape = to_shape(return_type)
                {:fun_clauses, [{arg_shapes, return_shape}]}
              _ -> nil
            end
          else
            nil
          end

        if fun_inferred_shape do
          fun_inferred_shape
        else
          descr = unwrap_dynamic(descr)

          cond do
            # if_set - optional type (check before union, as if_set creates a union with not_set)
            is_if_set?(descr) ->
              inner_type = extract_if_set_type(descr)
              {:optional, to_shape(inner_type)}

            # Union types (highest priority to catch complex unions)
          union_types = extract_union(descr) ->
            shapes = Enum.map(union_types, &to_shape_eager/1) |> Enum.reject(&is_nil/1)

            if length(shapes) > 1 do
              {:union, shapes}
            else
              # Fall back to single type or nil
              case shapes do
                [single_shape] -> single_shape
                [] -> nil
              end
            end

          # Struct types
          struct_info = extract_struct(descr) ->
            case struct_info do
              %{module: module, fields: fields} when is_atom(module) ->
                field_shapes = convert_struct_fields(fields)
                # Use Binding-compatible format: {:struct, fields, {:atom, Module}, nil}
                {:struct, field_shapes, {:atom, module}, nil}

              _ ->
                nil
            end

          # Function types
          function_info = extract_function(descr) ->
            case function_info do
              %{arity: arity, type: :function} when is_integer(arity) ->
                {:fun, arity}

              %{arity: :any, type: :function} ->
                {:fun, :any}

              _ ->
                nil
            end

          # Bounded integers
          integer_range = extract_integer_range(descr) ->
            {:integer, integer_range}

          # String literals
          string_literal = extract_string_literal(descr) ->
            {:binary, string_literal}

          # PIDs, ports, refs (check against well-known descriptors)
          Module.Types.Descr.equal?(descr, Module.Types.Descr.pid()) ->
            :pid

          Module.Types.Descr.equal?(descr, Module.Types.Descr.port()) ->
            :port

          Module.Types.Descr.equal?(descr, Module.Types.Descr.reference()) ->
            :reference

          Module.Types.Descr.equal?(descr, Module.Types.Descr.none()) ->
            :none

          # Integer
          Module.Types.Descr.equal?(descr, Module.Types.Descr.integer()) ->
            {:integer, nil}

          # Float
          Module.Types.Descr.equal?(descr, Module.Types.Descr.float()) ->
            :float

          # Binary
          Module.Types.Descr.equal?(descr, Module.Types.Descr.binary()) ->
            :binary

          # Empty list
          Module.Types.Descr.equal?(descr, Module.Types.Descr.empty_list()) ->
            {:list, :empty}

          # not_set
          Module.Types.Descr.equal?(descr, Module.Types.Descr.not_set()) ->
            :not_set

          # Function types
          fun_info = extract_function_info(descr) ->
            fun_info

          # Any atom (atom top type)
          is_atom_top?(descr) ->
            :atom

          # Single atom
          is_single_atom?(descr) ->
            {:atom, extract_single_atom(descr)}

          # Lists with concrete element type
          list_element = extract_list_element(descr) ->
            case to_shape(list_element) do
              nil -> {:list, nil}
              element_shape -> {:list, element_shape}
            end

          # Closed tuples with small arity
          tuple_elements = extract_tuple_elements(descr) ->
            case tuple_elements do
              :any_tuple ->
                :tuple

              elements when is_list(elements) ->
                element_shapes =
                  elements
                  |> Enum.map(&to_shape/1)

                if Enum.all?(element_shapes, &(&1 != nil)) do
                  {:tuple, length(element_shapes), element_shapes}
                else
                  {:tuple, length(element_shapes), Enum.map(element_shapes, &(&1 || nil))}
                end
            end

          # Closed maps with atom keys
          map_fields = extract_map_fields(descr) ->
            fields = convert_map_fields(map_fields) || []
            {:map, fields, nil}

          true ->
            nil
          end
        end
      rescue
        _ -> nil
      catch
        _ -> nil
      end
    else
      nil
    end
  end

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

  # Advanced type extractors for Enhanced Shape Conversion

  defp extract_union(descr) do
    # Try to detect union types by examining descriptor structure
    cond do
      # Check for explicit union in atom types
      is_map(descr) and match?({:union, _}, Map.get(descr, :atom)) ->
        case Map.get(descr, :atom) do
          {:union, set} ->
            set_list = :sets.to_list(set)

            if length(set_list) > 1 do
              Enum.map(set_list, &%{atom: {:union, :sets.from_list([&1])}})
            else
              nil
            end

          _ ->
            nil
        end

      # Check for multiple map keys indicating union
      is_map(descr) and map_size(descr) > 1 ->
        # This could be a union of different types
        types = for {key, _value} <- descr, key != :dynamic, do: %{key => Map.get(descr, key)}
        if length(types) > 1, do: types, else: nil

      true ->
        nil
    end
  end

  defp extract_struct(descr) do
    # Look for struct patterns in the descriptor
    if is_map(descr) do
      case Map.get(descr, :map) do
        # Look for struct field pattern - list format
        %{closed: fields} when is_list(fields) ->
          case Enum.find(fields, fn {key, _} -> key == :__struct__ end) do
            {:__struct__, struct_descr} ->
              struct_module = extract_struct_module(struct_descr)
              other_fields = Enum.reject(fields, fn {key, _} -> key == :__struct__ end)
              %{module: struct_module, fields: other_fields}

            _ ->
              nil
          end

        # Look for struct field pattern - map format
        {:closed, fields} when is_map(fields) ->
          case Map.get(fields, :__struct__) do
            nil ->
              nil

            struct_descr ->
              struct_module = extract_struct_module(struct_descr)
              other_fields = Map.delete(fields, :__struct__)
              %{module: struct_module, fields: other_fields}
          end

        _ ->
          nil
      end
    else
      nil
    end
  end

  defp extract_struct_module(descr) do
    # Extract module name from struct descriptor
    case extract_single_atom(descr) do
      atom when is_atom(atom) -> atom
      _ -> nil
    end
  end

  defp extract_function(descr) do
    # Look for function type patterns
    if is_map(descr) do
      case Map.get(descr, :fun) do
        fun_descr when is_map(fun_descr) ->
          # Extract arity from function descriptor
          arity = Map.get(fun_descr, :arity, :any)
          %{arity: arity, type: :function}

        _ ->
          nil
      end
    else
      nil
    end
  end

  defp extract_integer_range(descr) do
    # Look for bounded integer types
    if is_map(descr) do
      case Map.get(descr, :integer) do
        # Look for range information in integer descriptor
        integer_descr when is_map(integer_descr) ->
          min_val = Map.get(integer_descr, :min)
          max_val = Map.get(integer_descr, :max)

          if min_val != nil and max_val != nil and min_val <= max_val do
            {min_val, max_val}
          else
            nil
          end

        _ ->
          nil
      end
    else
      nil
    end
  end

  defp extract_string_literal(descr) do
    # Look for string literal patterns in binary descriptors
    if is_map(descr) do
      case Map.get(descr, :binary) do
        # Look for specific binary patterns that might indicate literals
        binary_descr when is_map(binary_descr) ->
          case Map.get(binary_descr, :literal) do
            literal when is_binary(literal) -> literal
            _ -> nil
          end

        _ ->
          nil
      end
    else
      nil
    end
  end

  defp is_atom_top?(descr) when is_map(descr) do
    case Map.get(descr, :atom) do
      {:negation, set} ->
        :sets.is_empty(set)

      _ ->
        false
    end
  end

  defp is_atom_top?(_), do: false

  defp is_single_atom?(descr) when is_map(descr) do
    case Map.get(descr, :atom) do
      {:union, set} ->
        set_list = :sets.to_list(set)
        length(set_list) == 1

      _ ->
        false
    end
  end

  defp is_single_atom?(_), do: false

  defp extract_single_atom(descr) when is_map(descr) do
    case Map.get(descr, :atom) do
      {:union, set} ->
        set_list = :sets.to_list(set)

        if length(set_list) == 1 do
          hd(set_list)
        else
          nil
        end

      _ ->
        nil
    end
  end

  defp extract_list_element(descr) when is_map(descr) do
    descr = unwrap_dynamic(descr)
    # This is a simplified extraction - Module.Types list structure is complex
    # For M1, we'll be conservative and only handle clear cases
    case Map.get(descr, :list) do
      [{element_type, _tail, _constraints}] -> element_type
      {element_type, _tail} -> element_type
      %{element: element_type} -> element_type
      _ -> nil
    end
  end

  defp extract_list_element(_), do: nil

  defp extract_tuple_elements(descr) when is_map(descr) do
    descr = unwrap_dynamic(descr)

    case Map.get(descr, :tuple) do
      [{:closed, elements}] when is_list(elements) and length(elements) <= 10 ->
        elements

      {:closed, elements} when is_list(elements) and length(elements) <= 10 ->
        elements

      # Open tuples with known elements - treat as minimum-size tuple
      [{:open, elements}] when is_list(elements) and length(elements) > 0 and length(elements) <= 10 ->
        elements

      {:open, elements} when is_list(elements) and length(elements) > 0 and length(elements) <= 10 ->
        elements

      # Open tuple without elements (any tuple) - return :any_tuple marker
      [{:open, []}] ->
        :any_tuple

      {:open, []} ->
        :any_tuple

      _ ->
        nil
    end
  end

  defp extract_tuple_elements(_), do: nil

  defp extract_map_fields(descr) when is_map(descr) do
    descr = unwrap_dynamic(descr)

    case Map.get(descr, :map) do
      [{:closed, fields, []}] when is_map(fields) ->
        fields

      {:closed, fields} when is_map(fields) ->
        fields

      %{closed: field_list} when is_list(field_list) ->
        Enum.into(field_list, %{})

      # Open maps - extract known atom key fields
      [{:open, fields, _}] when is_map(fields) ->
        extract_atom_keys_from_map(fields)

      {:open, fields} when is_map(fields) ->
        extract_atom_keys_from_map(fields)

      %{open: field_list} when is_list(field_list) ->
        field_list
        |> Enum.into(%{})
        |> extract_atom_keys_from_map()

      # Open map with domains (has default type) - extract atom key fields from second element
      {_domains, fields} when is_map(fields) ->
        extract_atom_keys_from_map(fields)

      _ ->
        nil
    end
  end

  defp extract_map_fields(_), do: nil

  # Extract only atom key fields from an open map, filtering out domain keys
  defp extract_atom_keys_from_map(fields) when is_map(fields) do
    fields
    |> Enum.reject(fn
      {{:domain_key, _}, _} -> true
      _ -> false
    end)
    |> Enum.into(%{})
  end

  defp extract_atom_keys_from_map(_), do: %{}

  # Check if descriptor is if_set (optional type)
  defp is_if_set?(descr) when is_map(descr) do
    Map.has_key?(descr, :optional) and Map.get(descr, :optional) == 1 and
      map_size(descr) > 1
  end

  defp is_if_set?(_), do: false

  # Extract the inner type from if_set
  defp extract_if_set_type(descr) when is_map(descr) do
    descr
    |> Map.delete(:optional)
  end

  # Extract function information from descriptor
  # Note: fun_from_inferred_clauses is handled at the top of to_shape_eager
  # before unwrapping dynamic
  defp extract_function_info(descr) when is_map(descr) do
    descr = unwrap_dynamic(descr)

    case Map.get(descr, :fun) do
      # fun() - top function type
      :bdd_top ->
        :fun

      # fun(arity) or fun(args, return)
      {args, return_type} when is_list(args) ->
        arity = length(args)

        # Check if this is just an arity constraint (args are all none, return is :term)
        if Enum.all?(args, &Module.Types.Descr.equal?(&1, Module.Types.Descr.none())) and
             return_type == :term do
          {:fun, arity}
        else
          # fun(args, return) - function with specific signature
          arg_shapes = Enum.map(args, &to_shape/1)
          return_shape = to_shape(return_type)
          {:fun, arg_shapes, return_shape}
        end

      # fun_from_non_overlapping_clauses
      # Represented as BDD tuple with multiple clauses
      bdd when is_tuple(bdd) ->
        clauses = extract_function_clauses(bdd)
        {:fun_clauses, clauses}

      _ ->
        nil
    end
  end

  defp extract_function_info(_), do: nil

  # Extract function clauses from BDD representation
  defp extract_function_clauses(bdd) when is_tuple(bdd) do
    # BDD is a tuple of clauses, each clause is {args, return}
    # We'll flatten it to extract all clauses
    flatten_bdd_clauses(bdd, [])
  end

  defp flatten_bdd_clauses({args, return}, acc) when is_list(args) do
    # Single clause
    clause = {Enum.map(args, &to_shape/1), to_shape(return)}
    [clause | acc]
  end

  defp flatten_bdd_clauses({clause1, clause2, _, _}, acc) do
    # Multiple clauses in BDD format
    acc = flatten_bdd_clauses(clause1, acc)
    flatten_bdd_clauses(clause2, acc)
  end

  defp flatten_bdd_clauses(:bdd_bot, acc), do: acc
  defp flatten_bdd_clauses(:bdd_top, acc), do: acc
  defp flatten_bdd_clauses(_, acc), do: acc

  defp convert_map_fields(fields) when is_map(fields) do
    try do
      converted =
        for {key, value_descr} <- fields do
          value_descr = unwrap_dynamic(value_descr)

          {key, to_shape(value_descr)}
        end

      converted
    rescue
      _ -> nil
    end
  end

  defp convert_struct_fields(fields) when is_list(fields) do
    try do
      converted =
        for {key, value_descr} <- fields do
          value_descr = unwrap_dynamic(value_descr)

          case to_shape(value_descr) do
            nil -> {key, nil}
            shape -> {key, shape}
          end
        end

      converted
    rescue
      _ -> nil
    end
  end

  defp convert_struct_fields(fields) when is_map(fields) do
    try do
      converted =
        for {key, value_descr} <- fields do
          value_descr = unwrap_dynamic(value_descr)

          case to_shape(value_descr) do
            nil -> {key, nil}
            shape -> {key, shape}
          end
        end

      converted
    rescue
      _ -> nil
    end
  end

  defp convert_struct_fields(_), do: nil

  defp unwrap_dynamic(%{dynamic: dynamic}) when is_map(dynamic), do: dynamic
  defp unwrap_dynamic(descr), do: descr

  # -- Variables seeding helpers ------------------------------------------------

  # Convert user-provided variables map into Module.Types context vars map.
  # Input keys are {name, version}; output keys are version only.
  defp variables_to_context_vars(nil), do: nil

  defp variables_to_context_vars(vars) when is_map(vars) do
    Enum.reduce(vars, %{}, fn
      {{name, version}, type_like}, acc when is_atom(name) and is_integer(version) ->
        type_descr = coerce_var_type(type_like)
        data = %{type: type_descr, name: name, context: nil, off_traces: []}
        Map.put(acc, version, data)

      _other, acc ->
        acc
    end)
  end

  defp variables_to_context_vars(_), do: nil

  # Accept Module.Types.Descr, sentinel atoms, or ElixirSense minimal shapes
  # and coerce into a Module.Types.Descr.t()
  defp coerce_var_type(%{} = descr), do: descr
  defp coerce_var_type(:dynamic), do: Module.Types.Descr.dynamic()
  defp coerce_var_type(:term), do: Module.Types.Descr.term()
  defp coerce_var_type(nil), do: Module.Types.Descr.dynamic()

  defp coerce_var_type({:atom, atom}) when is_atom(atom),
    do: Module.Types.Descr.atom([atom])

  defp coerce_var_type({:map, fields}) when is_list(fields) do
    # Best-effort closed map from atom keys; values default to term()
    pairs = for {k, _v} when is_atom(k) <- fields, do: {k, Module.Types.Descr.term()}
    Module.Types.Descr.closed_map(pairs)
  end

  defp coerce_var_type({:struct, _fields, module}) when is_atom(module) do
    # Represent struct as map with __struct__
    Module.Types.Descr.closed_map([{:__struct__, Module.Types.Descr.atom([module])}])
  end

  defp coerce_var_type(_), do: Module.Types.Descr.dynamic()

  # Collect variables from AST and seed them as dynamic() types
  defp variables_from_ast(ast) do
    try do
      {_ast, acc} =
        Macro.prewalk(ast, %{}, fn
          {name, meta, ctx} = node, acc when is_atom(name) and is_list(meta) and is_atom(ctx) ->
            new_acc =
              case Keyword.fetch(meta, :version) do
                {:ok, version} when is_integer(version) ->
                  if valid_variable_name?(name) do
                    Map.put(acc, {name, version}, Module.Types.Descr.dynamic())
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
      _ -> %{}
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
      Module.Types.Descr.empty?(descr) -> :none
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
      expected = List.duplicate(Module.Types.Descr.dynamic(), arity)

      stack =
        init_stack(module, fun_arity, file || "nofile", mode)
        |> maybe_disable_local_handler()

      case stack do
        nil ->
          :error

        _stack ->
          context = init_context()
          reduced = do_infer_local_signature(stack, context, clauses, expected)

          case reduced do
            [] ->
              :error

            clause_types ->
              domain = build_domain(clause_types)
              {:infer, domain, clause_types}
          end
          |> dbg
      end
      |> dbg
    else
      _ -> :error
    end
    |> dbg
  end

  defp maybe_disable_local_handler(nil), do: nil

  defp maybe_disable_local_handler(stack) do
    # prevent recursive lookups while we are inferring the function itself
    %{stack | local_handler: fn _, _, _, context -> {:def, :none, context} end}
  end

  defp do_infer_local_signature(stack, context, clauses, expected) do
    Enum.reduce_while(clauses, [], fn clause, acc ->
      %{meta: meta, args: args, guards: guards, body: body} = normalise_clause(clause)

      try do
        {trees, clause_ctx} =
          Module.Types.Pattern.of_head(
            args,
            guards || [],
            expected,
            {:infer, expected},
            meta,
            stack,
            context
          )

        {return_type, clause_ctx} =
          Module.Types.Expr.of_expr(body, Module.Types.Descr.term(), body, stack, clause_ctx)

        arg_types =
          case stack.mode do
            :traversal -> expected
            _ -> Module.Types.Pattern.of_domain(trees, expected, clause_ctx)
          end

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

  defp build_domain([]), do: nil
  defp build_domain([_]), do: nil

  defp build_domain(clause_types) do
    clause_types
    |> Enum.map(&elem(&1, 0))
    |> Enum.zip()
    |> Enum.map(fn tuple ->
      tuple
      |> Tuple.to_list()
      |> Enum.reduce(&Module.Types.Descr.union/2)
    end)
  end

  defp normalise_clause(%{meta: meta, args: args, guards: guards, body: body}) do
    %{meta: meta || [], args: args || [], guards: guards, body: body || {:__block__, [], []}}
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
          %{elixir_types_sig: sig, type: type} when sig != nil ->
            kind = get_def_kind_for_types(type)
            Map.put(acc, {fun, arity}, {kind, sig})

          _ ->
            acc
        end
      end)
      |> Map.put(:__module__, module)

    signatures
  end

  def build_local_sigs_map(_metadata, _module), do: %{}

  @doc """
  Create a closure-based local handler from a signatures map.
  """
  def local_handler_from(local_sigs_map) when is_map(local_sigs_map) do
    fn _meta, {_fun, _arity} = fun_arity, _stack, context ->
      case Map.get(local_sigs_map, fun_arity) do
        {kind, {:infer, domain, clause_types}} ->
          # Apply inferred signature based on argument count and types
          # For M2, we'll use a simplified approach with the domain
          case domain do
            nil when length(clause_types) == 1 ->
              # Single clause - use its return type directly
              {_args, return_type} = hd(clause_types)
              {kind, return_type, context}

            domain when is_list(domain) ->
              # Multiple clauses - use union of return types for now
              return_types = Enum.map(clause_types, &elem(&1, 1))
              union_type = Enum.reduce(return_types, &Module.Types.Descr.union/2)
              {kind, union_type, context}

            _ ->
              false
          end

        _ ->
          false
      end
    end
  end

  @doc """
  Creates a remote handler closure that looks up ExCk signatures for remote calls.

  The handler attempts to find type signatures in BEAM ExCk chunks.
  """
  def remote_handler_from() do
    fn module, function, arity, _meta, _stack, context ->
      case ElixirSense.Core.ExCkReader.lookup_signature(module, function, arity) do
        {:ok, info} ->
          apply_exck_signature(info, context)

        :error ->
          :error
      end
    end
  end

  # Helper to convert ElixirSense def types to Module.Types kinds
  defp get_def_kind_for_types(:def), do: :def
  defp get_def_kind_for_types(:defp), do: :defp
  defp get_def_kind_for_types(:defmacro), do: :defmacro
  defp get_def_kind_for_types(:defmacrop), do: :defmacrop
  # For other types, default to :def
  defp get_def_kind_for_types(_), do: :def

  # Apply ExCk signature information to typing context
  defp apply_exck_signature(info, context) do
    kind =
      info
      |> Map.get(:kind, :def)
      |> get_def_kind_for_types()

    case Map.get(info, :sig) do
      nil ->
        false

      sig_info ->
        return_type = extract_return_type_from_sig(sig_info)
        {kind, return_type, context}
    end
  end

  # Extract return type from ExCk signature (simplified for M2)
  defp extract_return_type_from_sig({sig_kind, _domain, clauses})
       when sig_kind in [:infer, :strong] and is_list(clauses) do
    case extract_return_type_from_clauses(clauses) do
      {:ok, return_type} -> return_type
      :error -> Module.Types.Descr.dynamic()
    end
  end

  defp extract_return_type_from_sig(:none), do: Module.Types.Descr.dynamic()
  defp extract_return_type_from_sig(_), do: Module.Types.Descr.dynamic()

  defp extract_return_type_from_clauses(clauses) do
    clauses
    |> Enum.reduce_while(nil, fn clause, acc ->
      case clause_return_type(clause) do
        nil ->
          {:cont, acc}

        return_type ->
          try do
            combined =
              if acc == nil, do: return_type, else: Module.Types.Descr.union(acc, return_type)

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
