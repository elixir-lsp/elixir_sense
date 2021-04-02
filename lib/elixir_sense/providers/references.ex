defmodule ElixirSense.Providers.References do
  @moduledoc """
  This module provides References support by using
  the `Mix.Tasks.Xref.call/0` task to find all references to
  any function or module identified at the provided location.
  """

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Parser
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State
  alias ElixirSense.Core.State.AttributeInfo
  alias ElixirSense.Core.State.VarInfo
  alias Mix.Tasks.Xref

  @type position :: %{line: pos_integer, column: pos_integer}

  @type range :: %{
          start: position,
          end: position
        }

  @type reference_info :: %{
          uri: String.t() | nil,
          range: range
        }

  @spec find(
          String.t(),
          non_neg_integer,
          State.Env.t(),
          [VarInfo.t()],
          [AttributeInfo.t()],
          Metadata.t()
        ) :: [ElixirSense.Providers.References.reference_info()]
  def find(
        subject,
        arity,
        %State.Env{
          imports: imports,
          aliases: aliases,
          module: module,
          scope: scope
        },
        vars,
        attributes,
        %Metadata{
          mods_funs_to_positions: modules_funs,
          calls: calls,
          types: metadata_types
        }
      ) do
    var_info = vars |> Enum.find(fn %VarInfo{name: name} -> to_string(name) == subject end)

    attribute_info =
      case subject do
        "@" <> attribute_name ->
          attributes
          |> Enum.find(fn %AttributeInfo{name: name} -> to_string(name) == attribute_name end)

        _ ->
          nil
      end

    private_info =
      Enum.find(modules_funs, fn {{_mod, name, _args}, fun_info} ->
        to_string(name) == subject && fun_info.type == :defp
      end)

    cond do
      var_info != nil ->
        %VarInfo{positions: positions} = var_info

        positions
        |> Enum.map(fn pos -> build_var_location(subject, pos) end)

      attribute_info != nil ->
        %AttributeInfo{positions: positions} = attribute_info

        positions
        |> Enum.map(fn pos -> build_var_location(subject, pos) end)

      private_info != nil ->
        calls
        |> Map.values()
        |> List.flatten()
        |> Enum.filter(&(&1.mod == nil && to_string(&1.func) == subject))
        |> Enum.map(fn %{position: pos} -> build_var_location(subject, pos) end)

      true ->
        binding_env = %Binding{
          attributes: attributes,
          variables: vars,
          current_module: module
        }

        {mod, fun, _found} =
          subject
          |> Source.split_module_and_func(module, aliases)
          |> expand(binding_env, aliases)
          |> Introspection.actual_mod_fun(imports, aliases, module, modules_funs, metadata_types)

        {mod, fun}
        |> xref_at_cursor(arity, module, scope, modules_funs)
        |> Enum.map(&build_location/1)
        |> Enum.sort_by(fn %{uri: a, range: %{start: %{line: b, column: c}}} -> {a, b, c} end)
    end
  end

  defp xref_at_cursor(actual_mod_fun, arity, module, scope, modules_funs) do
    mfa =
      actual_mod_fun
      |> callee_at_cursor(module, scope, arity, modules_funs)

    callers(mfa)
  end

  # Cursor over a module
  defp callee_at_cursor({module, nil}, _module, _scope, _arity, _modules_funs) do
    [module]
  end

  # Cursor over a function definition
  defp callee_at_cursor({module, func}, module, {func, arity}, nil, modules_funs) do
    fun_info = modules_funs |> Map.fetch!({module, func, arity})

    if fun_info.params |> hd |> Enum.any?(&match?({:\\, _, _}, &1)) do
      # function has default params, we cannot use arity to filter
      # TODO maybe filter by arity range
      [module, func]
    else
      [module, func, arity]
    end
  end

  # Cursor over a function call but we couldn't introspect the arity
  defp callee_at_cursor({module, func}, _module, _scope, nil, _modules_funs) do
    [module, func]
  end

  # Cursor over a function call
  defp callee_at_cursor({module, func}, _module, _scope, arity, _modules_funs) do
    [module, func, arity]
  end

  def callers(mfa) do
    calls =
      calls()
      |> Enum.filter(caller_filter(mfa))
      |> fix_caller_module()

    arity =
      case mfa do
        [_, _, a] -> a
        _ -> nil
      end

    for call <- calls,
        new_call <- expand_xref_line_calls(call, arity) do
      new_call
    end
  end

  defp calls do
    if Mix.Project.umbrella?() do
      umbrella_calls()
    else
      Xref.calls()
    end
  end

  def umbrella_calls do
    build_dir = Path.expand(Mix.Project.config()[:build_path])
    app_paths = Mix.Project.apps_paths()

    app_paths
    |> Enum.flat_map(fn {app, path} ->
      Mix.Project.in_project(app, path, [build_path: build_dir], fn _ ->
        Xref.calls()
        |> Enum.map(fn call ->
          Map.update!(call, :file, fn file -> Path.expand(file) end)
        end)
      end)
    end)
  end

  defp expand_xref_line_calls(xref_call, buffer_arity) do
    %{callee: {mod, func, arity}, file: file, line: line} = xref_call

    case File.read(file) do
      {:ok, code} ->
        metadata = Parser.parse_string(code, true, true, line)

        %State.Env{
          module: module,
          vars: vars,
          attributes: attributes
        } = env = Metadata.get_env(metadata, line)

        calls =
          metadata
          |> Metadata.get_calls(line)
          |> fix_calls_positions(code)

        binding_env = %Binding{
          attributes: attributes,
          variables: vars,
          current_module: module
        }

        for %State.CallInfo{arity: call_arity, position: {line, column} = call_position} <- calls,
            arity == call_arity,
            check_arity(
              arity,
              buffer_arity,
              metadata.mods_funs_to_positions[{mod, func, nil}]
            ),
            found_mod_fun =
              find_actual_mod_fun(
                code,
                call_position,
                env,
                metadata.mods_funs_to_positions,
                metadata.types,
                binding_env
              ),
            found_mod_fun == {mod, func} do
          Map.merge(xref_call, %{column: column, line: line})
        end

      _ ->
        [xref_call]
    end
  end

  defp check_arity(call_arity, buffer_arity, _info)
       when is_nil(buffer_arity) or call_arity == buffer_arity do
    true
  end

  defp check_arity(call_arity, buffer_arity, %State.ModFunInfo{} = info) do
    State.ModFunInfo.get_arities(info)
    |> Enum.any?(fn
      {arity, default_args} ->
        min_arity = arity - default_args

        min_arity <= call_arity and call_arity <= arity and
          min_arity <= buffer_arity and buffer_arity <= arity
    end)
  end

  defp check_arity(_call_arity, _buffer_arity, _) do
    false
  end

  defp find_actual_mod_fun(
         code,
         {line, col},
         env,
         mods_funs,
         metadata_types,
         binding_env
       ) do
    %State.Env{
      imports: imports,
      aliases: aliases,
      module: module
    } = env

    case Source.subject(code, line, col) do
      nil ->
        {nil, nil}

      subject ->
        {mod, fun, _found} =
          subject
          |> Source.split_module_and_func(module, aliases)
          |> expand(binding_env, env.aliases)
          |> Introspection.actual_mod_fun(imports, aliases, module, mods_funs, metadata_types)

        {mod, fun}
    end
  end

  defp caller_filter([module, func, _arity]), do: &match?(%{callee: {^module, ^func, _}}, &1)
  defp caller_filter([module, func]), do: &match?(%{callee: {^module, ^func, _}}, &1)
  defp caller_filter([module]), do: &match?(%{callee: {^module, _, _}}, &1)

  defp build_location(call) do
    %{callee: {_, func, _}} = call
    func_length = func |> to_string() |> String.length()

    %{
      uri: call.file,
      range: %{
        start: %{line: call.line, column: call.column},
        end: %{line: call.line, column: call.column + func_length}
      }
    }
  end

  defp build_var_location(subject, {line, column}) do
    %{
      uri: nil,
      range: %{
        start: %{line: line, column: column},
        end: %{line: line, column: column + String.length(subject)}
      }
    }
  end

  # For Elixir < v1.10.0
  defp fix_caller_module(calls) do
    calls
    |> Enum.map(fn c -> Map.delete(c, :caller_module) end)
    |> Enum.uniq()
  end

  defp fix_calls_positions(calls, code) do
    for call <- calls do
      case call do
        %State.CallInfo{mod: nil} ->
          call

        %State.CallInfo{position: {line, column}} ->
          text_after = Source.text_after(code, line, column)
          {_rest, line_offset, col_offset} = Source.find_next_word(text_after) || {"", 0, 0}
          col_offset = if line_offset == 0, do: column, else: col_offset

          %State.CallInfo{call | position: {line + line_offset, col_offset}}
      end
    end
  end

  defp expand({{:attribute, _} = type, func}, env, aliases) do
    case Binding.expand(env, type) do
      {:atom, module} -> {Introspection.expand_alias(module, aliases), func}
      _ -> {nil, nil}
    end
  end

  defp expand({type, func}, _env, _aliases) do
    {type, func}
  end
end
