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
            location = Location.find_mod_fun_source(mod, fun, call_arity)
            redirect_into_using_macro(location, mod, fun, metadata) || location

          %ModFunInfo{} = info ->
            {{line, column}, {end_line, end_column}} = Location.info_to_range(info)
            category = ModFunInfo.get_category(info)

            location = %Location{
              file: nil,
              type: category,
              line: line,
              column: column,
              end_line: end_line,
              end_column: end_column
            }

            if category in [:function, :macro] do
              redirect_into_using_macro(location, mod, fun, metadata) || location
            else
              location
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

  # When a function's recorded definition sits on a `use SomeModule` line, the
  # function was injected by `SomeModule.__using__/1`. In that case we try to
  # locate the real definition (def/defmacro/defdelegate/defguard) inside that
  # macro body and point there.
  #
  # Two guards keep this from misfiring:
  #
  #   * the `use`-site check (`use_site?/3`) — a genuine local or remote `def`
  #     has its position on its own definition line, which is not a `use`
  #     statement, so we leave it untouched (this is what makes a locally
  #     overridden function resolve to the local definition, not the injected
  #     one);
  #   * the search is bounded to the `__using__/1` body, so an unrelated `def`
  #     of the same name elsewhere in the used module's file is never matched.
  #
  # The set of used modules comes from the tracked `uses` (resolved at macro
  # expansion time, so aliases and `Kernel.use/1` are handled correctly).
  defp redirect_into_using_macro(%Location{line: line} = location, mod, fun, metadata) do
    with true <- use_site?(location, metadata, line),
         [_ | _] = used_modules <- used_modules(location, mod, metadata) do
      Enum.find_value(used_modules, fn mod ->
        with %Location{file: file, line: l, end_line: el} when not is_nil(file) <-
               Location.find_mod_fun_source(mod, :__using__, :any) do
          search_using_body(file, l, el, fun)
        end
      end)
    else
      _ -> nil
    end
  end

  defp redirect_into_using_macro(_location, _mod, _fun, _metadata), do: nil

  # Matches the start of a `use Foo` / `use Foo, opts` / `Kernel.use(Foo)` /
  # `Kernel.use Foo` statement. The trailing `[\s(]` ensures we match the `use`
  # keyword and not an identifier like `use_foo`.
  @use_line_detector ~r/^\s*(?:Kernel\s*\.\s*)?use[\s(]/

  # Does the recorded definition sit on a `use Foo` / `Kernel.use(Foo)` line?
  #
  # We match the line textually rather than parsing it: a `use` may span
  # several lines (`use Foo,\n  opt: 1`), in which case the recorded line on its
  # own (`use Foo,`) is not valid Elixir and would fail to parse.
  defp use_site?(location, metadata, line) do
    with source when is_binary(source) <- location_source(location, metadata),
         text when is_binary(text) <- Enum.at(Source.split_lines(source), line - 1) do
      Regex.match?(@use_line_detector, text)
    else
      _ -> false
    end
  end

  # Modules `mod` uses, resolved at expansion time. For the current buffer they
  # are in `metadata.uses`; for an external module we parse its source (only
  # reached once we already know the position is a `use` site, so this is not
  # on the hot path of ordinary remote lookups).
  defp used_modules(%Location{file: nil}, mod, metadata) do
    Map.get(metadata.uses, mod, [])
  end

  defp used_modules(%Location{file: file}, mod, _metadata) do
    case File.read(file) do
      {:ok, content} ->
        external_metadata = Parser.parse_string(content, false, false, nil)
        Map.get(external_metadata.uses, mod, [])

      _ ->
        []
    end
  end

  # Source text backing the location: the in-memory buffer for the current
  # module (file == nil), or the file contents for an external module.
  defp location_source(%Location{file: nil}, metadata), do: metadata.source

  defp location_source(%Location{file: file}, _metadata) do
    case File.read(file) do
      {:ok, content} -> content
      _ -> nil
    end
  end

  defp body_lines(file, using_line, using_end_line) do
    case File.read(file) do
      {:ok, content} ->
        content
        |> Source.split_lines()
        |> Enum.slice((using_line - 1)..(using_end_line - 1)//1)

      _ ->
        []
    end
  end

  defp comment_line?(line_text), do: String.match?(String.trim_leading(line_text), ~r/^#/)

  # Search only within the `__using__/1` macro body (between its def line and
  # end line) for the injected definition. Bounding the search to the macro
  # body avoids matching unrelated defs elsewhere in the same file.
  defp search_using_body(file, using_line, using_end_line, fun) do
    # Matches public definition forms: def, defmacro, defdelegate, defguard.
    # The mandatory whitespace after the keyword excludes private variants
    # (defp/defmacrop/defguardp) and defmodule.
    regex = ~r/\bdef(?:macro|delegate|guard)?\s+(#{Regex.escape(Atom.to_string(fun))})\b/

    lines = body_lines(file, using_line, using_end_line)

    lines
    |> Enum.with_index()
    |> Enum.find_value(fn {line_text, idx} ->
      if comment_line?(line_text) do
        nil
      else
        case Regex.run(regex, line_text, return: :index) do
          [_whole, {name_offset, name_len}] ->
            target_line = using_line + idx

            %Location{
              type: :function,
              file: file,
              line: target_line,
              column: name_offset + 1,
              end_line: target_line,
              end_column: name_offset + 1 + name_len
            }

          _ ->
            nil
        end
      end
    end)
  end
end
