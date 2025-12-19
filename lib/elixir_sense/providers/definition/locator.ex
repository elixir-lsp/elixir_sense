defmodule ElixirSense.Providers.Definition.Locator do
  @moduledoc """
  Provides a function to find out where symbols are defined.

  Currently finds definition of modules, functions and macros,
  typespecs, variables and attributes.
  """

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.State
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.SurroundContext
  alias ElixirSense.Providers.Location
  alias ElixirSense.Core.Parser
  alias ElixirSense.Core.State.{ModFunInfo, TypeInfo}

  alias ElixirSense.Providers.Plugins.Phoenix.Scope

  def definition(code, line, column, options \\ []) do
    case Code.Fragment.surround_context(code, {line, column}) do
      :none ->
        nil

      context ->
        metadata =
          Keyword.get_lazy(options, :metadata, fn ->
            Parser.parse_string(code, true, false, {line, column})
          end)

        env = Metadata.get_cursor_env(metadata, {line, column}, {context.begin, context.end})

        find(
          context,
          env,
          metadata
        )
    end
  end

  @doc """
  Finds out where a module, function, macro or variable was defined.
  """
  @spec find(
          any(),
          State.Env.t(),
          Metadata.t()
        ) :: %Location{} | nil
  def find(
        context,
        %State.Env{
          module: module,
          attributes: attributes
        } = env,
        metadata
      ) do
    binding_env = Binding.from_env(env, metadata, context.begin)

    type = SurroundContext.to_binding(context.context, module)

    case type do
      nil ->
        nil

      {:keyword, _} ->
        nil

      {:variable, variable, version} ->
        var_info = Metadata.find_var(metadata, variable, version, context.begin)

        if var_info != nil do
          {definition_line, definition_column} = Enum.min(var_info.positions)

          %Location{
            type: :variable,
            file: nil,
            line: definition_line,
            column: definition_column,
            end_line: definition_line,
            end_column: definition_column + String.length(to_string(variable))
          }
        else
          # find local call
          find_function_or_module(
            {nil, variable},
            context,
            env,
            metadata,
            binding_env
          )
        end

      {:attribute, attribute} ->
        attribute_info =
          Enum.find(attributes, fn
            %State.AttributeInfo{name: name} -> name == attribute
          end)

        if attribute_info != nil do
          %State.AttributeInfo{positions: [{line, column} | _]} = attribute_info

          %Location{
            type: :attribute,
            file: nil,
            line: line,
            column: column,
            end_line: line,
            end_column: column + 1 + String.length(to_string(attribute))
          }
        end

      {module, function} ->
        find_function_or_module(
          {module, function},
          context,
          env,
          metadata,
          binding_env
        )
    end
  end

  defp find_function_or_module(
         target,
         context,
         env,
         metadata,
         binding_env,
         visited \\ []
       ) do
    unless target in visited do
      do_find_function_or_module(
        target,
        context,
        env,
        metadata,
        binding_env,
        [target | visited]
      )
    end
  end

  defp do_find_function_or_module(
         {{:variable, _, _} = type, function},
         context,
         env,
         metadata,
         binding_env,
         visited
       ) do
    case Binding.expand(binding_env, type) do
      {:atom, module} ->
        do_find_function_or_module(
          {{:atom, module}, function},
          context,
          env,
          metadata,
          binding_env,
          visited
        )

      _ ->
        nil
    end
  end

  defp do_find_function_or_module(
         {{:attribute, _} = type, function},
         context,
         env,
         metadata,
         binding_env,
         visited
       ) do
    case Binding.expand(binding_env, type) do
      {:atom, module} ->
        do_find_function_or_module(
          {{:atom, module}, function},
          context,
          env,
          metadata,
          binding_env,
          visited
        )

      _ ->
        nil
    end
  end

  defp do_find_function_or_module(
         {nil, :super},
         context,
         %State.Env{function: {function, arity}, module: module} = env,
         metadata,
         binding_env,
         visited
       ) do
    case metadata.mods_funs_to_positions[{module, function, arity}] do
      %ModFunInfo{overridable: {true, origin}} ->
        # overridable function is most likely defined by __using__ macro
        do_find_function_or_module(
          {{:atom, origin}, :__using__},
          context,
          env,
          metadata,
          binding_env,
          visited
        )

      _ ->
        nil
    end
  end

  defp do_find_function_or_module(
         {module, function},
         context,
         env,
         metadata,
         _binding_env,
         _visited
       ) do
    m = get_module(module, context, env, metadata)

    case {m, function}
         |> Introspection.actual_mod_fun(
           env,
           metadata.mods_funs_to_positions,
           metadata.types,
           context.begin,
           true
         ) do
      {_, _, false, _} ->
        nil

      {mod, fun, true, :mod_fun} ->
        {line, column} = context.end
        call_arity = Metadata.get_call_arity(metadata, mod, fun, line, column) || :any

        fn_definition =
          Location.get_function_position_using_metadata(
            mod,
            fun,
            call_arity,
            metadata.mods_funs_to_positions
          )

        case fn_definition do
          nil ->
            # Try to find function in __using__ macro before giving up and pointing to the macro use
            case find_function_in_module_using_macro(mod, fun, metadata) do
              %Location{file: file} = location when not is_nil(file) ->
                location

              _ ->
                Location.find_mod_fun_source(mod, fun, call_arity)
            end

          %ModFunInfo{} = info ->
            {{line, column}, {end_line, end_column}} = Location.info_to_range(info)

            if ModFunInfo.get_category(info) in [:function, :macro] do
              # First try to use find_function_in_module_using_macro for external modules
              # This is needed when the function is defined in a different file via __using__
              case find_function_in_module_using_macro(mod, fun, metadata) do
                %Location{file: file} = location when not is_nil(file) ->
                  location

                _ ->
                  find_function_in_using_macro(
                    metadata,
                    env,
                    line,
                    column,
                    end_line,
                    end_column,
                    fun,
                    info
                  )
              end
            else
              %Location{
                file: nil,
                type: ModFunInfo.get_category(info),
                line: line,
                column: column,
                end_line: end_line,
                end_column: end_column
              }
            end
        end

      {mod, fun, true, :type} ->
        {line, column} = context.end
        call_arity = Metadata.get_call_arity(metadata, mod, fun, line, column) || :any

        type_definition =
          Location.get_type_position_using_metadata(mod, fun, call_arity, metadata.types)

        case type_definition do
          nil ->
            Location.find_type_source(mod, fun, call_arity)

          %TypeInfo{} = info ->
            {{line, column}, {end_line, end_column}} = Location.info_to_range(info)

            %Location{
              file: nil,
              type: :typespec,
              line: line,
              column: column,
              end_line: end_line,
              end_column: end_column
            }
        end
    end
  end

  defp get_module(module, %{end: {line, col}}, env, metadata) do
    with {true, module} <- get_phoenix_module(module, env),
         true <- Introspection.elixir_module?(module) do
      text_before = Source.text_before(metadata.source, line, col)

      case Scope.within_scope(text_before) do
        {false, _} ->
          module

        {true, scope_alias} ->
          Module.concat(scope_alias, module)
      end
    end
  end

  defp get_phoenix_module(module, env) do
    case {Phoenix.Router in env.requires, module} do
      {true, {:atom, module}} -> {true, module}
      {false, {:atom, module}} -> module
      _ -> nil
    end
  end

  defp resolve_use_module({:__aliases__, _, parts}, env) do
    [head | tail] = parts

    case Keyword.fetch(env.aliases, Module.concat(Elixir, head)) do
      {:ok, aliased_mod} ->
        Module.concat([aliased_mod | tail])

      :error ->
        Module.concat(parts)
    end
  end

  defp resolve_use_module(atom, _env) when is_atom(atom), do: atom
  defp resolve_use_module(_, _), do: nil

  defp find_function_in_module_using_macro(mod, fun, metadata) do
    if Map.has_key?(metadata.mods_funs_to_positions, {mod, nil, nil}) do
      # Module is in the current source - use metadata.uses
      used_modules = Map.get(metadata.uses, mod, [])

      Enum.find_value(used_modules, fn used_module ->
        search_in_using_macro(used_module, fun)
      end)
    else
      # Module is external - read the file contents and parse it
      with file when not is_nil(file) <- get_module_source_file(mod),
           {:ok, content} <- File.read(file) do
        external_metadata = Parser.parse_string(content, false, false, nil)
        used_modules = Map.get(external_metadata.uses, mod, [])

        Enum.find_value(used_modules, fn used_module ->
          search_in_using_macro(used_module, fun)
        end)
      else
        _ -> nil
      end
    end
  end

  defp get_module_source_file(mod) do
    compile_info = mod.__info__(:compile)
    source = Keyword.get(compile_info, :source)
    if source, do: to_string(source)
  catch
    _, _ -> nil
  end

  defp search_in_using_macro(module, fun) do
    case Location.find_mod_fun_source(module, :__using__, :any) do
      %Location{file: file, line: using_line, column: using_col} when not is_nil(file) ->
        find_function_def_in_using_macro_source(file, using_line, using_col, fun)

      _ ->
        nil
    end
  end

  defp find_function_def_in_using_macro_source(file, using_line, using_col, fun) do
    content = File.read!(file)
    {_, suffix} = Source.split_at(content, using_line, using_col)
    regex = ~r/def\s+(#{Regex.escape(Atom.to_string(fun))}\b)/

    case Regex.run(regex, suffix, return: :index) do
      [_entire_match, {fun_offset, _fun_len}] ->
        {line_offset, col_offset} = calculate_offset(suffix, fun_offset)
        target_line = using_line + line_offset
        target_column = if line_offset == 0, do: using_col + col_offset, else: 1 + col_offset

        %Location{
          file: file,
          type: :function,
          line: target_line,
          column: target_column,
          end_line: target_line,
          end_column: target_column
        }

      nil ->
        nil
    end
  end

  defp calculate_offset(suffix, fun_offset) do
    suffix
    |> String.slice(0, fun_offset)
    |> Source.split_lines()
    |> then(fn lines -> {length(lines) - 1, String.length(List.last(lines) || "")} end)
  end

  defp fallback_location(line, column, end_line, end_column, info) do
    %Location{
      file: nil,
      type: if(info, do: ModFunInfo.get_category(info), else: :function),
      line: line,
      column: column,
      end_line: end_line,
      end_column: end_column
    }
  end

  defp find_function_in_using_macro(metadata, env, line, column, end_line, end_column, fun, info) do
    # This might be a function defined via `use`
    # We need to find the `use` statement and the module being used
    source_line = Source.split_lines(metadata.source) |> Enum.at(line - 1)

    used_module =
      case Code.string_to_quoted(source_line) do
        {:ok, {:use, _, [module_ast | _]}} ->
          resolve_use_module(module_ast, env)

        _ ->
          nil
      end

    case used_module && Location.find_mod_fun_source(used_module, :__using__, :any) do
      %Location{file: file, line: using_line, column: using_col} = using_location
      when not is_nil(file) ->
        find_function_def_in_using_macro_source(file, using_line, using_col, fun) ||
          using_location

      _ ->
        fallback_location(line, column, end_line, end_column, info)
    end
  end
end
