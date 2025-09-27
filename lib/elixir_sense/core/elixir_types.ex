defmodule ElixirSense.Core.ElixirTypes do
  @moduledoc """
  Adaptor over Elixir's Module.Types for set-theoretic type inference.

  This module provides a stable interface to Elixir's evolving type system,
  allowing ElixirSense to benefit from precise type inference while maintaining
  compatibility and fallback behavior.
  """

  @doc """
  Returns true if Module.Types.Expr is available and has of_expr/5.
  """
  def available?() do
    Code.ensure_loaded?(Module.Types.Expr) and
      function_exported?(Module.Types.Expr, :of_expr, 5)
  end

  @doc """
  Returns true if adaptor is enabled via config and Module.Types is available.
  """
  def enabled?() do
    Application.get_env(:elixir_sense, :use_elixir_types, false) and available?()
  end

  @doc """
  Creates a Module.Types stack for typing operations.
  """
  def init_stack(module \\ nil, function \\ nil, file \\ nil, mode \\ :dynamic) do
    if available?() do
      Module.Types.stack(
        mode,
        file || "nofile",
        module || ElixirSense.ElixirTypes,
        function || {:__info__, 1},
        :all,
        nil,
        &__MODULE__.local_handler/4
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

  Returns {:ok, descr} on success or :error on failure.
  """
  def of_expr(ast, module \\ nil, function \\ nil, file \\ nil, mode \\ :dynamic) do
    if available?() do
      try do
        stack = init_stack(module, function, file, mode)
        context = init_context()

        if stack && context do
          {descr, _context} = Module.Types.Expr.of_expr(
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
      rescue
        _ -> :error
      catch
        _ -> :error
      end
    else
      :error
    end
  end

  @doc """
  Types a pattern match to extract variable refinements.

  Stub implementation for M1 - returns :error.
  Will be implemented in M1.5 if time permits.
  """
  def of_match(_pattern_ast, _expected_descr, _expr_ast, _module \\ nil, _function \\ nil, _file \\ nil, _mode \\ :dynamic) do
    :error
  end

  @doc """
  Converts a Module.Types.Descr to ElixirSense shape format.

  Conservative conversion - only returns shapes for clearly identifiable types.
  Returns nil for complex or uncertain types to avoid false precision.
  """
  def to_shape(descr) do
    if available?() do
      try do
        cond do
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
            if Enum.all?(element_shapes, & &1 != nil) do
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
      {:none, _} -> :none

      # Use new if existing is nil
      {nil, new} -> new

      # Keep existing if new is nil or :none
      {existing, nil} -> existing
      {existing, :none} -> existing

      # Prefer literal integers over generic
      {{:integer, value}, {:integer, nil}} when value != nil -> existing
      {{:integer, nil}, {:integer, value}} when value != nil -> new

      # Prefer more specific list types
      {{:list, type1}, {:list, type2}} when type1 != nil and type2 == nil -> existing
      {{:list, type1}, {:list, type2}} when type1 == nil and type2 != nil -> new

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
      _ -> existing
    end
  end

  # Helper functions for shape conversion

  defp is_single_atom?(descr) when is_map(descr) do
    case Map.get(descr, :atom) do
      {:union, set} -> :sets.size(set) == 1
      _ -> false
    end
  end
  defp is_single_atom?(_), do: false

  defp extract_single_atom(descr) when is_map(descr) do
    case Map.get(descr, :atom) do
      {:union, set} ->
        if :sets.size(set) == 1 do
          hd(:sets.to_list(set))
        else
          nil
        end
      _ -> nil
    end
  end

  defp extract_list_element(descr) when is_map(descr) do
    # This is a simplified extraction - Module.Types list structure is complex
    # For M1, we'll be conservative and only handle clear cases
    case Map.get(descr, :list) do
      [{element_type, _tail, _constraints}] -> element_type
      _ -> nil
    end
  end
  defp extract_list_element(_), do: nil

  defp extract_tuple_elements(descr) when is_map(descr) do
    case Map.get(descr, :tuple) do
      [{:closed, elements}] when is_list(elements) and length(elements) <= 10 ->
        elements
      _ -> nil
    end
  end
  defp extract_tuple_elements(_), do: nil

  defp extract_map_fields(descr) when is_map(descr) do
    case Map.get(descr, :map) do
      [{:closed, fields, []}] when is_map(fields) ->
        # Only handle maps with atom keys and no constraints
        if Enum.all?(Map.keys(fields), &is_atom/1) do
          fields
        else
          nil
        end
      _ -> nil
    end
  end
  defp extract_map_fields(_), do: nil

  defp convert_map_fields(fields) when is_map(fields) do
    try do
      converted =
        for {key, value_descr} <- fields do
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

  defp count_concrete_shapes(shapes) do
    Enum.count(shapes, fn
      nil -> false
      :none -> false
      _ -> true
    end)
  end
end