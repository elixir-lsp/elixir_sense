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

      # Type an expression
      ElixirTypes.of_expr(42)
      #=> {:ok, %{bitmap: 4}}

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
    cond do
      is_list(ast) and not proper_list_ast?(ast) ->
        :error

      is_tuple(ast) and tuple_size(ast) not in [2, 3] ->
        :error

      true ->
        if available?() do
          try do
            if mode == :traversal && is_simple_ast?(ast) do
              # Fast path for simple AST nodes in traversal mode
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

  defp proper_list_ast?([]), do: true
  defp proper_list_ast?([_ | tail]), do: proper_list_ast?(tail)
  defp proper_list_ast?(_), do: false

  # Simple type inference for basic literals (fast path)
  defp simple_type_of(ast) do
    case ast do
      x when is_atom(x) ->
        {:ok, Module.Types.Descr.atom([x])}

      x when is_integer(x) ->
        {:ok, Module.Types.Descr.integer()}

      x when is_float(x) ->
        {:ok, Module.Types.Descr.float()}

      x when is_binary(x) ->
        {:ok, Module.Types.Descr.binary()}

      [] ->
        {:ok, Module.Types.Descr.empty_list()}

      {a, b} ->
        {:ok, Module.Types.Descr.tuple([literal_descr(a), literal_descr(b)])}

      _ ->
        :error
    end
  rescue
    _ -> :error
  end

  defp checker_cache do
    case Process.get(:elixir_sense_checker_cache) do
      {checker, table} = cache
      when is_pid(checker) and (is_reference(table) or is_integer(table)) ->
        cache

      _ ->
        case Module.ParallelChecker.start_link() do
          {:ok, cache} ->
            Process.put(:elixir_sense_checker_cache, cache)
            cache

          _ ->
            nil
        end
    end
  rescue
    _ -> nil
  end

  defp literal_descr(x) when is_atom(x), do: Module.Types.Descr.atom([x])
  defp literal_descr(x) when is_integer(x), do: Module.Types.Descr.integer()
  defp literal_descr(x) when is_float(x), do: Module.Types.Descr.float()
  defp literal_descr(x) when is_binary(x), do: Module.Types.Descr.binary()
  defp literal_descr([]), do: Module.Types.Descr.empty_list()
  defp literal_descr(_), do: Module.Types.Descr.term()

  def maybe_remote_call_sig(
        {{:., _, [target_ast, fun]}, _meta, args},
        metadata
      )
      when is_atom(fun) and is_list(args) do
    if enabled?() do
      case module_from_ast(target_ast, metadata) do
        {:ok, module} ->
          case ElixirSense.Core.ExCkReader.lookup_signature(module, fun, length(args)) do
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
          %ElixirSense.Core.State.AttributeInfo{type: {:atom, module}} when is_atom(module) ->
            {:ok, module}

          %ElixirSense.Core.State.AttributeInfo{type: {:attribute, nested_attr}}
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
          %ElixirSense.Core.State.VarInfo{type: {:atom, module}} when is_atom(module) ->
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
          resolved when is_atom(resolved) ->
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
    _ -> nil
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
    _ -> nil
  end

  defp expand_alias_from_env(_, _, _), do: nil

  defp metadata_env(metadata) when is_map(metadata) do
    case metadata.cursor_env || metadata.closest_env do
      {_, env} when is_map(env) -> env
      {_from, _to, env} when is_map(env) -> env
      _ -> nil
    end
  end

  defp metadata_env(_), do: nil

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
    case do_of_match(pattern_ast, expected_descr, match_ast, module, function, file, mode, opts) do
      {:ok, var_shapes, var_descrs} ->
        maybe_store_var_descriptors(opts, var_descrs)
        {:ok, var_shapes}

      :error ->
        :error
    end
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
          {type_descr, _} -> Module.Types.Descr.intersection(expected_descr, type_descr)
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
              nested_refinements =
                apply_pattern_refinements(var_shapes, var_ast, nil, field_value_ast)

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

  ## Options

  - `:lazy` - When true, uses lazy evaluation and caching for expensive conversions
  """
  def to_shape(descr, opts \\ []) do
    if Keyword.get(opts, :lazy, false) do
      to_shape_lazy(descr, opts)
    else
      to_shape_eager(descr)
    end
  end

  # Lazy shape conversion with caching (uses descriptor as key to avoid hash collisions)
  defp to_shape_lazy(descr, _opts) do
    cache_key = {:elixir_types_shape_cache, descr}

    case Process.get(cache_key) do
      nil ->
        result = to_shape_eager(descr)
        Process.put(cache_key, {result, System.monotonic_time(:millisecond)})
        result

      {cached_result, timestamp} ->
        if System.monotonic_time(:millisecond) - timestamp < 300_000 do
          cached_result
        else
          Process.delete(cache_key)
          result = to_shape_eager(descr)
          Process.put(cache_key, {result, System.monotonic_time(:millisecond)})
          result
        end
    end
  end

  # Convert a Module.Types.Descr descriptor to an ElixirSense shape.
  # Uses Descr.to_quoted/1 to get stable AST representation, then translates
  # that AST to ElixirSense shapes — avoiding internal BDD pattern matching.
  defp to_shape_eager(descr) do
    if available?() do
      try do
        cond do
          # not_set() has an :optional key that to_quoted doesn't handle
          is_map(descr) and is_map_key(descr, :optional) ->
            optional_descr = Map.delete(descr, :optional)

            if map_size(optional_descr) > 0 do
              {:optional, to_shape(optional_descr)}
            else
              :not_set
            end

          true ->
            descr
            |> Module.Types.Descr.to_quoted()
            |> quoted_to_shape()
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

  # Convert Descr.to_quoted/1 AST output into ElixirSense shapes.
  defp quoted_to_shape(quoted) do
    case quoted do
      # Special types
      {:term, [], []} -> nil
      {:none, [], []} -> :none
      {:dynamic, [], []} -> nil
      # dynamic(inner) — unwrap and convert the inner type
      {:dynamic, [], [inner]} -> quoted_to_shape(inner)

      # Primitive types
      {:integer, [], []} -> {:integer, nil}
      {:float, [], []} -> {:float, nil}
      {:binary, [], []} -> {:binary, nil}
      {:atom, [], []} -> :atom
      {:pid, [], []} -> :pid
      {:port, [], []} -> :port
      {:reference, [], []} -> :reference
      {:empty_list, [], []} -> {:list, :empty}
      {:empty_map, [], []} -> {:map, [], nil}
      {:boolean, [], []} -> {:union, [{:atom, false}, {:atom, true}]}
      # map() — the map top type with no specific fields
      {:map, [], []} -> {:map, [], nil}

      # Function types
      {:fun, [], []} -> :fun

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

      # List types
      {:list, [], [elem]} -> {:list, quoted_to_shape(elem)}
      {:non_empty_list, [], [elem]} -> {:list, quoted_to_shape(elem)}
      # non_empty_list with explicit tail (improper list) — we lose the tail info
      {:non_empty_list, [], [elem, _tail]} -> {:list, quoted_to_shape(elem)}

      # Tuple types (all arities use {:{}, [], elems} form)
      {:{}, [], elems} when is_list(elems) ->
        # Filter out open tuple marker {:..., [], nil}
        elems = Enum.reject(elems, &match?({:..., [], _}, &1))
        shapes = Enum.map(elems, &quoted_to_shape/1)
        {:tuple, length(shapes), shapes}

      # Map types (may contain structs)
      {:%{}, [], fields} when is_list(fields) ->
        quoted_map_to_shape(fields)

      # Union types
      {:or, [], [_left, _right]} = union ->
        shapes =
          flatten_quoted_union(union)
          |> Enum.map(&quoted_to_shape/1)
          |> Enum.reject(&is_nil/1)

        case shapes do
          [] -> nil
          [single] -> single
          multiple -> {:union, multiple}
        end

      # Intersection types — function intersections become multi-clause funs,
      # non-function intersections fall back to the left branch as best approximation
      {:and, [], [_left, _right]} = intersection ->
        fun_clauses = extract_fun_clauses_from_intersection(intersection)

        case fun_clauses do
          [_ | _] ->
            {:fun_clauses, fun_clauses}

          [] ->
            # For non-function intersections, try both branches and use the first non-nil shape
            flatten_quoted_intersection(intersection)
            |> Enum.find_value(&quoted_to_shape/1)
        end

      # Negation types — try to interpret as the base type when possible
      {:not, [], [_inner]} -> nil

      # Optional map field value
      {:if_set, [], [inner]} -> {:optional, quoted_to_shape(inner)}

      # Literal atoms wrapped in __block__
      {:__block__, [], [atom]} when is_atom(atom) -> {:atom, atom}

      # Literal integers wrapped in __block__
      {:__block__, [], [integer]} when is_integer(integer) -> {:integer, integer}

      # Module aliases in struct __struct__ fields
      {:__aliases__, [], parts} when is_list(parts) ->
        {:atom, Module.concat(parts)}

      # Open map/tuple marker
      {:..., [], _} -> nil

      # Bare literal atoms (unlikely from to_quoted but handle for safety)
      atom when is_atom(atom) -> {:atom, atom}

      # Bare literal integers
      integer when is_integer(integer) -> {:integer, integer}

      # Fallback
      _ -> nil
    end
  end

  # Handle __block__ wrapping a list that isn't function clauses
  defp quoted_to_shape_block([value]) when is_atom(value), do: {:atom, value}
  defp quoted_to_shape_block([value]) when is_integer(value), do: {:integer, value}
  defp quoted_to_shape_block(_), do: nil

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
    # Extract field key-value pairs, skipping open map markers
    kv_pairs =
      for {key_quoted, value_quoted} <- fields,
          not match?({:..., [], _}, key_quoted),
          key = extract_quoted_map_key(key_quoted),
          do: {key, value_quoted}

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

  defp flatten_quoted_intersection({:and, [], [left, right]}) do
    flatten_quoted_intersection(left) ++ flatten_quoted_intersection(right)
  end

  defp flatten_quoted_intersection(other), do: [other]

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
        data = %{type: type_descr, name: name, context: nil, off_traces: []}
        Map.put(acc, version, data)

      _other, acc ->
        acc
    end)
  end

  defp variables_to_context_vars(_), do: nil

  # Accept Module.Types.Descr, sentinel atoms, or ElixirSense minimal shapes
  # and coerce into a Module.Types.Descr.t()
  def coerce_var_type_public(type_like), do: coerce_var_type(type_like)

  defp coerce_var_type(%{} = descr), do: descr
  defp coerce_var_type(:dynamic), do: Module.Types.Descr.dynamic()
  defp coerce_var_type(:term), do: Module.Types.Descr.term()
  defp coerce_var_type(nil), do: Module.Types.Descr.dynamic()

  defp coerce_var_type({:atom, atom}) when is_atom(atom),
    do: Module.Types.Descr.atom([atom])

  defp coerce_var_type(:atom), do: Module.Types.Descr.atom()
  defp coerce_var_type(:integer), do: Module.Types.Descr.integer()
  defp coerce_var_type(:binary), do: Module.Types.Descr.binary()
  defp coerce_var_type(:float), do: Module.Types.Descr.float()
  defp coerce_var_type(:pid), do: Module.Types.Descr.pid()
  defp coerce_var_type(:port), do: Module.Types.Descr.port()
  defp coerce_var_type(:reference), do: Module.Types.Descr.reference()
  defp coerce_var_type(:tuple), do: Module.Types.Descr.tuple()
  defp coerce_var_type(:fun), do: Module.Types.Descr.fun()
  defp coerce_var_type(:none), do: Module.Types.Descr.none()

  defp coerce_var_type(:number),
    do: Module.Types.Descr.union(Module.Types.Descr.integer(), Module.Types.Descr.float())

  defp coerce_var_type({:integer, _}), do: Module.Types.Descr.integer()
  defp coerce_var_type({:float, _}), do: Module.Types.Descr.float()
  defp coerce_var_type({:binary, _}), do: Module.Types.Descr.binary()
  defp coerce_var_type({:list, :empty}), do: Module.Types.Descr.empty_list()

  defp coerce_var_type({:list, element_type}) do
    Module.Types.Descr.list(coerce_var_type(element_type))
  end

  defp coerce_var_type({:tuple, _arity, elements}) when is_list(elements) do
    Module.Types.Descr.tuple(Enum.map(elements, &coerce_var_type/1))
  end

  defp coerce_var_type({:map, fields, _}) when is_list(fields) do
    pairs = for {k, v} when is_atom(k) <- fields, do: {k, coerce_var_type(v)}
    Module.Types.Descr.closed_map(pairs)
  end

  defp coerce_var_type({:map, fields}) when is_list(fields) do
    pairs = for {k, v} when is_atom(k) <- fields, do: {k, coerce_var_type(v)}
    Module.Types.Descr.closed_map(pairs)
  end

  defp coerce_var_type({:struct, fields, {:atom, module}, _updated})
       when is_atom(module) and is_list(fields) do
    pairs =
      [{:__struct__, Module.Types.Descr.atom([module])}] ++
        for {k, v} when is_atom(k) <- fields, do: {k, coerce_var_type(v)}

    Module.Types.Descr.closed_map(pairs)
  end

  defp coerce_var_type({:struct, fields, module}) when is_atom(module) do
    pairs =
      [{:__struct__, Module.Types.Descr.atom([module])}] ++
        for {k, v} when is_atom(k) <- fields, do: {k, coerce_var_type(v)}

    Module.Types.Descr.closed_map(pairs)
  end

  defp coerce_var_type({:fun, arity}) when is_integer(arity) do
    Module.Types.Descr.fun(arity)
  end

  defp coerce_var_type({:fun, args, _return}) when is_list(args) do
    Module.Types.Descr.fun(length(args))
  end

  defp coerce_var_type({:fun_clauses, clauses}) when is_list(clauses) do
    # fun_clauses is multi-clause function — coerce as generic fun with arity of first clause
    case clauses do
      [{args, _return} | _] when is_list(args) -> Module.Types.Descr.fun(length(args))
      _ -> Module.Types.Descr.fun()
    end
  end

  defp coerce_var_type({:union, types}) when is_list(types) do
    types
    |> Enum.map(&coerce_var_type/1)
    |> Enum.reduce(Module.Types.Descr.none(), &Module.Types.Descr.union/2)
  end

  defp coerce_var_type({:intersection, types}) when is_list(types) do
    types
    |> Enum.map(&coerce_var_type/1)
    |> Enum.reduce(Module.Types.Descr.term(), &Module.Types.Descr.intersection/2)
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
      body = ensure_body_var_versions(body)

      if unsupported_signature_body?(body) do
        {:cont, acc}
      else
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

  defp ensure_body_var_versions(ast) do
    Macro.prewalk(ast, fn
      {:@, _, _} = node ->
        node

      {name, meta, context} = node
      when is_atom(name) and is_list(meta) and is_atom(context) and
             name not in [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__, :_] ->
        if Keyword.has_key?(meta, :version) do
          node
        else
          {name, Keyword.put(meta, :version, 0), context}
        end

      node ->
        node
    end)
  end

  defp unsupported_signature_body?(body) do
    {_body, found?} =
      Macro.prewalk(body, false, fn
        {:@, _, _} = node, _acc -> {node, true}
        node, acc -> {node, acc}
      end)

    found?
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
        with %{type: type} <- info do
          kind = get_def_kind_for_types(type)

          sig =
            case {Map.get(info, :elixir_types_sig),
                  spec_signature_from_metadata(metadata, module, fun, arity)} do
              {_inferred, {:ok, {:strong, _domain, _clauses} = spec_sig}} ->
                spec_sig

              {sig, _} when sig != nil ->
                sig

              {nil, {:ok, spec_sig}} ->
                spec_sig

              _ ->
                nil
            end

          if sig != nil do
            Map.put(acc, {fun, arity}, {kind, sig})
          else
            acc
          end
        else
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
    fn meta, {_fun, _arity} = fun_arity, stack, context ->
      case Map.get(local_sigs_map, fun_arity) do
        {kind, {sig_kind, _domain, _clause_types} = sig}
        when sig_kind in [:infer, :strong] ->
          {kind, sig, maybe_put_remote_sig(context, meta, stack)}

        {kind, :none} ->
          {kind, :none, maybe_put_remote_sig(context, meta, stack)}

        _ ->
          false
      end
    end
  end

  def spec_signature_from_metadata(metadata, module, fun, arity)
      when is_map(metadata) and is_atom(module) and is_atom(fun) and is_integer(arity) do
    case metadata.specs[{module, fun, arity}] do
      %ElixirSense.Core.State.SpecInfo{elixir_types_sig: {kind, _domain, _clauses} = sig}
      when kind in [:infer, :strong] ->
        {:ok, sig}

      _ ->
        :error
    end
  end

  def spec_signature_from_metadata(_, _, _, _), do: :error

  defp maybe_put_remote_sig(context, meta, stack) do
    call = build_remote_call_from_meta(meta)

    case call do
      nil ->
        context

      remote_ast ->
        case maybe_remote_call_sig(remote_ast, stack[:metadata]) do
          {:ok, sig} ->
            arity = remote_ast |> elem(2) |> length()
            put_in(context.local_sigs[{:__remote__, arity}], sig)

          :error ->
            context
        end
    end
  end

  defp build_remote_call_from_meta(meta) when is_list(meta) do
    case Keyword.get(meta, :type_check) do
      {:invoked_as, module, fun, arity}
      when is_atom(module) and is_atom(fun) and is_integer(arity) ->
        {{:., [], [module, fun]}, [], List.duplicate(nil, arity)}

      _ ->
        nil
    end
  end

  defp build_remote_call_from_meta(_), do: nil

  # Helper to convert ElixirSense def types to Module.Types kinds
  defp get_def_kind_for_types(:def), do: :def
  defp get_def_kind_for_types(:defp), do: :defp
  defp get_def_kind_for_types(:defmacro), do: :defmacro
  defp get_def_kind_for_types(:defmacrop), do: :defmacrop
  # For other types, default to :def
  defp get_def_kind_for_types(_), do: :def

  # Extract return type from signature by unioning clause returns.
  # When arg_shapes are provided, filters clauses by domain compatibility.
  def extract_return_type_from_sig(sig, arg_shapes \\ nil)

  def extract_return_type_from_sig({sig_kind, _domain, clauses}, arg_shapes)
      when sig_kind in [:infer, :strong] and is_list(clauses) do
    filtered =
      if is_list(arg_shapes) and arg_shapes != [] do
        filter_clauses_by_args(clauses, arg_shapes)
      else
        clauses
      end

    # Fall back to all clauses if no clause matched the args
    effective = if filtered == [], do: clauses, else: filtered

    case extract_return_type_from_clauses(effective) do
      {:ok, return_type} -> return_type
      :error -> Module.Types.Descr.dynamic()
    end
  end

  def extract_return_type_from_sig(:none, _arg_shapes), do: Module.Types.Descr.dynamic()
  def extract_return_type_from_sig(_, _arg_shapes), do: Module.Types.Descr.dynamic()

  # Filter clauses whose arg domains are compatible with the given arg shapes.
  # A clause matches if each arg shape is a subtype of (or intersects with) the clause's arg domain.
  defp filter_clauses_by_args(clauses, arg_shapes) do
    if available?() do
      arg_descrs =
        try do
          Enum.map(arg_shapes, &coerce_var_type/1)
        rescue
          _ -> nil
        end

      if arg_descrs do
        Enum.filter(clauses, fn clause ->
          clause_args = clause_arg_types(clause)
          args_compatible?(clause_args, arg_descrs)
        end)
      else
        clauses
      end
    else
      clauses
    end
  end

  defp clause_arg_types({arg_types, _return}) when is_list(arg_types), do: arg_types
  defp clause_arg_types(%{args: arg_types}) when is_list(arg_types), do: arg_types
  defp clause_arg_types(_), do: nil

  defp args_compatible?(nil, _arg_descrs), do: true
  defp args_compatible?(clause_args, arg_descrs) when length(clause_args) != length(arg_descrs), do: false

  defp args_compatible?(clause_args, arg_descrs) do
    try do
      Enum.zip(clause_args, arg_descrs)
      |> Enum.all?(fn {domain, arg} ->
        # Check if arg type intersects with domain (not empty intersection)
        not Module.Types.Descr.empty?(Module.Types.Descr.intersection(arg, domain))
      end)
    rescue
      _ -> true
    catch
      _ -> true
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
