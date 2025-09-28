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
        metadata \\ nil
      ) do
    if available?() do
      local_handler =
        if local_sigs_map && map_size(local_sigs_map) > 0 do
          local_handler_from(local_sigs_map)
        else
          &__MODULE__.local_handler/4
        end

      remote_handler = remote_handler_from(metadata)

      Module.Types.stack(
        mode,
        file || "nofile",
        module || ElixirSense.ElixirTypes,
        function || {:__info__, 1},
        :all,
        remote_handler,
        local_handler
      )
    else
      nil
    end
  end

  @doc """
  Creates a Module.Types context for typing operations.
  """
  def init_context() do
    if available?() do
      Module.Types.context()
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
  - `module` - Optional module context (defaults to nil)
  - `function` - Optional function context (defaults to nil)
  - `file` - Optional file context (defaults to nil)
  - `mode` - Typing mode, :dynamic (default) or :traversal

  ## Returns

  - `{:ok, descr}` - Success with Module.Types descriptor
  - `:error` - Failure (Module.Types unavailable, invalid AST, etc.)

  ## Examples

      # Type a literal
      iex> ElixirTypes.of_expr(42)
      {:ok, %{bitmap: 4}}

      # Type a list
      iex> ElixirTypes.of_expr([1, 2, 3])
      {:ok, %{list: [{%{bitmap: 4}, %{bitmap: 2}, []}]}}

      # Type a tuple (requires AST form)
      iex> ElixirTypes.of_expr({:{}, [], [1, :ok]})
      {:ok, %{tuple: [closed: [%{bitmap: 4}, %{atom: {:union, %{ok: []}}}]]}}

  """
  def of_expr(
        ast,
        module \\ nil,
        function \\ nil,
        file \\ nil,
        mode \\ :dynamic,
        local_sigs_map \\ nil,
        metadata \\ nil
      ) do
    track_performance(:of_expr, fn ->
      if available?() do
        try do
          # Performance optimization for traversal mode
          if mode == :traversal && is_simple_ast?(ast) do
            # Fast path for simple AST nodes in traversal mode
            track_fast_path_hit()
            simple_type_of(ast)
          else
            # Full Module.Types processing
            stack = init_stack(module, function, file, mode, local_sigs_map, metadata)
            context = init_context()

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
        rescue
          _ -> :error
        catch
          _ -> :error
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
            context = init_context()

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
              {:ok, var_shapes} ->
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
          _ -> :error
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

      {_type, %{vars: vars_map}} =
        Module.Types.Pattern.of_match(pattern_ast, expected_fun, full_match, stack, context)

      # Enhanced variable shape extraction with type refinement
      var_shapes = extract_refined_var_shapes(vars_map, targets, pattern_ast, refined_expected)
      {:ok, var_shapes}
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

      {_type, %{vars: vars_map}} =
        Module.Types.Pattern.of_match(pattern_ast, expected_fun, full_match, stack, context)

      {:ok, extract_var_shapes(vars_map, targets)}
    rescue
      _ -> :error
    catch
      _ -> :error
    end
  end

  # Enhanced variable shape extraction with additional type refinement
  defp extract_refined_var_shapes(vars_map, targets, pattern_ast, expected_descr) do
    base_shapes = extract_var_shapes(vars_map, targets)

    # Apply pattern-specific type refinements
    refined_shapes = apply_pattern_refinements(base_shapes, pattern_ast, expected_descr)

    Map.merge(base_shapes, refined_shapes)
  end

  # Apply additional type refinements based on pattern structure
  defp apply_pattern_refinements(var_shapes, pattern_ast, expected_descr) do
    case pattern_ast do
      # Struct pattern refinement
      {:%, _, [struct_ast, {:%{}, _, fields}]} ->
        refine_struct_pattern_vars(var_shapes, struct_ast, fields, expected_descr)

      # Map pattern refinement
      {:%{}, _, fields} ->
        refine_map_pattern_vars(var_shapes, fields, expected_descr)

      # Tuple pattern refinement
      {:{}, _, elements} ->
        refine_tuple_pattern_vars(var_shapes, elements, expected_descr)

      # List pattern refinement
      list when is_list(list) ->
        refine_list_pattern_vars(var_shapes, list, expected_descr)

      _ ->
        %{}
    end
  end

  # Refine variables in struct patterns
  defp refine_struct_pattern_vars(_var_shapes, struct_ast, _fields, _expected_descr) do
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
  defp refine_map_pattern_vars(_var_shapes, _fields, _expected_descr) do
    # TODO: Implement map pattern variable refinement
    %{}
  end

  # Refine variables in tuple patterns
  defp refine_tuple_pattern_vars(_var_shapes, _elements, _expected_descr) do
    # TODO: Implement tuple pattern variable refinement
    %{}
  end

  # Refine variables in list patterns
  defp refine_list_pattern_vars(_var_shapes, _list, _expected_descr) do
    # TODO: Implement list pattern variable refinement
    %{}
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
        cond do
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
                {:struct, module, field_shapes}

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

          # Integer
          Module.Types.Descr.equal?(descr, Module.Types.Descr.integer()) ->
            {:integer, nil}

          # Float
          Module.Types.Descr.equal?(descr, Module.Types.Descr.float()) ->
            {:float, nil}

          # Binary
          Module.Types.Descr.equal?(descr, Module.Types.Descr.binary()) ->
            {:binary, nil}

          # Empty list
          Module.Types.Descr.equal?(descr, Module.Types.Descr.empty_list()) ->
            {:list, :empty}

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
            element_shapes = Enum.map(tuple_elements, &to_shape/1)

            if Enum.all?(element_shapes, &(&1 != nil)) do
              {:tuple, length(element_shapes), element_shapes}
            else
              {:tuple, length(element_shapes), Enum.map(element_shapes, &(&1 || nil))}
            end

          # Closed maps with atom keys
          map_fields = extract_map_fields(descr) ->
            case convert_map_fields(map_fields) do
              nil -> nil
              fields -> {:map, fields, nil}
            end

          true ->
            nil
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

      # Prefer tuples with more concrete element shapes
      {{:tuple, arity, elems1}, {:tuple, arity, elems2}} ->
        if count_concrete_shapes(elems1) >= count_concrete_shapes(elems2) do
          existing
        else
          new
        end

      # Prefer maps with more fields
      {{:map, fields1, nil}, {:map, fields2, nil}} ->
        if length(fields1) >= length(fields2) do
          existing
        else
          new
        end

      # Default: keep existing to avoid surprises
      _ ->
        existing
    end
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
        # Look for struct field pattern
        %{closed: fields} when is_list(fields) ->
          case Enum.find(fields, fn {key, _} -> key == :__struct__ end) do
            {:__struct__, struct_descr} ->
              struct_module = extract_struct_module(struct_descr)
              other_fields = Enum.reject(fields, fn {key, _} -> key == :__struct__ end)
              %{module: struct_module, fields: other_fields}

            _ ->
              nil
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
      _ -> nil
    end
  end

  defp extract_list_element(_), do: nil

  defp extract_tuple_elements(descr) when is_map(descr) do
    descr = unwrap_dynamic(descr)

    case Map.get(descr, :tuple) do
      [{:closed, elements}] when is_list(elements) and length(elements) <= 10 ->
        elements

      _ ->
        nil
    end
  end

  defp extract_tuple_elements(_), do: nil

  defp extract_map_fields(descr) when is_map(descr) do
    descr = unwrap_dynamic(descr)

    case Map.get(descr, :map) do
      [{:closed, fields, []}] when is_map(fields) ->
        # Only handle maps with atom keys and no constraints
        if Enum.all?(Map.keys(fields), &is_atom/1) do
          fields
        else
          nil
        end

      _ ->
        nil
    end
  end

  defp extract_map_fields(_), do: nil

  defp convert_map_fields(fields) when is_map(fields) do
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

  defp convert_struct_fields(_), do: nil

  defp count_concrete_shapes(shapes) do
    Enum.count(shapes, fn
      nil -> false
      :none -> false
      _ -> true
    end)
  end

  defp unwrap_dynamic(%{dynamic: dynamic}) when is_map(dynamic), do: dynamic
  defp unwrap_dynamic(descr), do: descr

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
      rescue
        _ -> {:cont, acc}
      catch
        _ -> {:cont, acc}
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

  The handler attempts to find type signatures in BEAM ExCk chunks and falls back
  to ElixirSense metadata if unavailable.
  """
  def remote_handler_from(metadata \\ nil) do
    fn module, function, arity, _meta, _stack, context ->
      case ElixirSense.Core.ExCkReader.lookup_signature(module, function, arity) do
        {:ok, info} ->
          apply_exck_signature(info, context)

        :error ->
          apply_metadata_fallback(metadata, module, function, arity, context)
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

  # Fallback to ElixirSense metadata for remote function typing
  defp apply_metadata_fallback(nil, _module, _function, _arity, _context), do: false

  defp apply_metadata_fallback(metadata, module, function, arity, context) do
    # Look up function in ElixirSense metadata
    case lookup_remote_in_metadata(metadata, module, function, arity) do
      nil -> false
      type_info -> {:def, type_info, context}
    end
  end

  # Look up remote function in ElixirSense metadata (placeholder for M2)
  defp lookup_remote_in_metadata(_metadata, _module, _function, _arity) do
    # For M2, return nil to indicate no metadata available
    # In future versions, this could integrate with ElixirSense's existing type info
    nil
  end
end
