defmodule ElixirSense.Providers.Definition do
  @moduledoc """
  Provides a function to find out where symbols are defined.

  Currently finds definition of modules, functions and macros,
  typespecs, variables and attributes.
  """

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.State
  alias ElixirSense.Core.State.ModFunInfo
  alias ElixirSense.Core.State.TypeInfo
  alias ElixirSense.Core.State.VarInfo
  alias ElixirSense.Core.SurroundContext
  alias ElixirSense.Location

  @doc """
  Finds out where a module, function, macro or variable was defined.
  """
  @spec find(
          any(),
          pos_integer,
          pos_integer,
          State.Env.t(),
          State.mods_funs_to_positions_t(),
          list(State.CallInfo.t()),
          State.types_t()
        ) :: %Location{} | nil
  def find(
        context,
        line,
        column,
        %State.Env{
          module: module,
          vars: vars,
          attributes: attributes
        } = env,
        mods_funs_to_positions,
        calls,
        metadata_types
      ) do
    binding_env = %Binding{
      attributes: attributes,
      variables: vars,
      current_module: module
    }

    type = SurroundContext.to_binding(context, module)

    case type do
      nil ->
        nil

      {:keyword, _} ->
        nil

      {:variable, variable} ->
        var_info =
          vars
          |> Enum.find(fn
            %VarInfo{name: name, positions: positions} ->
              name == variable and {line, column} in positions
          end)

        if var_info != nil do
          {definition_line, definition_column} = Enum.min(var_info.positions)

          %Location{type: :variable, file: nil, line: definition_line, column: definition_column}
        else
          find_function_or_module(
            {nil, variable},
            line,
            calls,
            mods_funs_to_positions,
            env,
            metadata_types,
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
          %Location{type: :attribute, file: nil, line: line, column: column}
        end

      {module, function} ->
        find_function_or_module(
          {module, function},
          line,
          calls,
          mods_funs_to_positions,
          env,
          metadata_types,
          binding_env
        )
    end
  end

  defp find_function_or_module(
         {module, function},
         line,
         calls,
         mods_funs_to_positions,
         env,
         metadata_types,
         binding_env,
         visited \\ []
       ) do
    unless {module, function} in visited do
      do_find_function_or_module(
        {module, function},
        line,
        calls,
        mods_funs_to_positions,
        env,
        metadata_types,
        binding_env,
        [{module, function} | visited]
      )
    end
  end

  defp do_find_function_or_module(
         {{:attribute, _attr} = type, function},
         line,
         calls,
         mods_funs_to_positions,
         env,
         metadata_types,
         binding_env,
         visited
       ) do
    case Binding.expand(binding_env, type) do
      {:atom, module} ->
        do_find_function_or_module(
          {{:atom, Introspection.expand_alias(module, env.aliases)}, function},
          line,
          calls,
          mods_funs_to_positions,
          env,
          metadata_types,
          binding_env,
          visited
        )

      _ ->
        nil
    end
  end

  defp do_find_function_or_module(
         {nil, :super},
         line,
         calls,
         mods_funs_to_positions,
         %State.Env{scope: {function, arity}, module: module} = env,
         metadata_types,
         binding_env,
         visited
       ) do
    case mods_funs_to_positions[{module, function, arity}] do
      %ModFunInfo{overridable: {true, origin}} ->
        # overridable function is most likely defined by __using__ macro
        do_find_function_or_module(
          {{:atom, origin}, :__using__},
          line,
          calls,
          mods_funs_to_positions,
          env,
          metadata_types,
          binding_env,
          visited
        )

      _ ->
        nil
    end
  end

  defp do_find_function_or_module(
         {module, function},
         line,
         calls,
         mods_funs_to_positions,
         env,
         metadata_types,
         _binding_env,
         _visited
       ) do
    %State.Env{
      module: current_module,
      imports: imports,
      requires: requires,
      aliases: aliases,
      scope: scope
    } = env

    m =
      case module do
        nil ->
          nil

        {:variable, :__MODULE__} ->
          current_module

        {:variable, _} ->
          # map field call
          nil

        {:atom, a} ->
          a
          # a when is_atom(a) -> a
      end

    case {m, function}
         |> Introspection.actual_mod_fun(
           imports,
           requires,
           aliases,
           current_module,
           scope,
           mods_funs_to_positions,
           metadata_types
         ) do
      {_, _, false, _} ->
        nil

      {mod, fun, true, :mod_fun} ->
        fn_definition = find_fn_definition(mod, fun, line, mods_funs_to_positions, calls)

        case fn_definition || mods_funs_to_positions[{mod, fun, nil}] do
          nil ->
            Location.find_mod_fun_source(mod, fun)

          %ModFunInfo{positions: positions} = mi ->
            # for simplicity take last position here as positions are reversed
            {line, column} = positions |> Enum.at(-1)

            %Location{
              file: nil,
              type: ModFunInfo.get_category(mi),
              line: line,
              column: column
            }
        end

      {mod, fun, true, :type} ->
        case metadata_types[{mod, fun, nil}] do
          nil ->
            Location.find_type_source(mod, fun)

          %TypeInfo{positions: positions} ->
            # for simplicity take last position here as positions are reversed
            {line, column} = positions |> Enum.at(-1)

            %Location{
              file: nil,
              type: :typespec,
              line: line,
              column: column
            }
        end
    end
  end

  defp find_fn_definition(mod, fun, line, mods_funs_to_positions, calls) do
    mods_funs_to_positions
    |> Enum.find(fn
      {{^mod, ^fun, fn_arity}, %{positions: fn_positions}} when not is_nil(fn_arity) ->
        case calls do
          [] -> Enum.any?(fn_positions, fn {fn_line, _fn_col} -> fn_line == line end)
          [%{arity: call_arity} | _] -> fn_arity == call_arity
        end

      _ ->
        false
    end)
    |> case do
      {_, mod_fun_info} -> mod_fun_info
      nil -> nil
    end
  end
end
