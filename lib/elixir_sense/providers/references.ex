defmodule ElixirSense.Providers.References do
  @moduledoc """
  This module provides References to function or module identified at the provided location.
  """

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State
  alias ElixirSense.Core.State.AttributeInfo
  alias ElixirSense.Core.State.VarInfo

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
          non_neg_integer,
          non_neg_integer,
          State.Env.t(),
          [VarInfo.t()],
          [AttributeInfo.t()],
          Metadata.t(),
          ElixirSense.call_trace_t() | nil
        ) :: [ElixirSense.Providers.References.reference_info()]
  def find(
        subject,
        line,
        column,
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
        },
        trace
      ) do
    var_info =
      Enum.find(vars, fn %VarInfo{name: name, positions: positions} ->
        to_string(name) == subject and {line, column} in positions
      end)

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
        |> xref_at_cursor(arity, module, scope, modules_funs, trace)
        |> Enum.map(&build_location/1)
        |> Enum.sort_by(fn %{uri: a, range: %{start: %{line: b, column: c}}} -> {a, b, c} end)
    end
  end

  defp xref_at_cursor(actual_mod_fun, arity, module, scope, modules_funs, trace) do
    mfa =
      actual_mod_fun
      |> callee_at_cursor(module, scope, arity, modules_funs)

    filtered_calls(mfa, trace)
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
      # TODO consider adding min and max bounds on arity
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

  defp filtered_calls(mfa, trace) do
    mfa = get_corrected_arity(mfa)

    trace
    |> Map.values()
    |> List.flatten()
    |> Enum.filter(caller_filter(mfa))
    |> Enum.uniq()
  end

  defp caller_filter([module, func, arity]) do
    fn
      %{callee: {^module, ^func, callee_arity}} ->
        [_m, _f, corrected_arity] = get_corrected_arity([module, func, callee_arity])
        arity == corrected_arity

      _ ->
        false
    end
  end

  defp caller_filter([module, func]), do: &match?(%{callee: {^module, ^func, _}}, &1)
  defp caller_filter([module]), do: &match?(%{callee: {^module, _, _}}, &1)

  defp build_location(call) do
    %{callee: {_, func, _}} = call

    line = call.line || 1

    {start_column, end_column} =
      if call.line != nil and call.column != nil do
        func_length = func |> to_string() |> String.length()
        {call.column, call.column + func_length}
      else
        {1, 1}
      end

    %{
      uri: call.file,
      range: %{
        start: %{line: line, column: start_column},
        end: %{line: line, column: end_column}
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

  defp expand({{:attribute, _} = type, func}, env, aliases) do
    case Binding.expand(env, type) do
      {:atom, module} -> {Introspection.expand_alias(module, aliases), func}
      _ -> {nil, nil}
    end
  end

  defp expand({type, func}, _env, _aliases) do
    {type, func}
  end

  defp get_corrected_arity([m]) do
    [m]
  end

  defp get_corrected_arity([m, f]) do
    case NormalizedCode.get_docs(m, :docs) do
      nil ->
        [m, f]

      docs ->
        docs
        |> Enum.find_value([m, f], fn {{name, arity}, _, _, _, _, _meta} ->
          if name == f do
            [m, f, arity]
          end
        end)
    end
  end

  defp get_corrected_arity([m, f, a]) do
    case NormalizedCode.get_docs(m, :docs) do
      nil ->
        [m, f, a]

      docs ->
        docs
        |> Enum.find_value([m, f, a], fn {{name, arity}, _, _, _, _, meta} ->
          if name == f do
            defaults = Map.get(meta, :defaults, 0)

            if a + defaults >= arity and a <= arity do
              [m, f, arity]
            end
          end
        end)
    end
  end
end
