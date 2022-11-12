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
  alias ElixirSense.Location
  alias ElixirSense.Core.SurroundContext

  @doc """
  Finds out where a module, function, macro or variable was defined.
  """
  @spec find(
          any(),
          State.Env.t(),
          State.mods_funs_to_positions_t(),
          State.types_t()
        ) :: %Location{} | nil
  def find(
        context,
        %State.Env{
          module: module,
          vars: vars,
          attributes: attributes
        } = env,
        mods_funs_to_positions,
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

      {:variable, variable} ->
        var_info = vars |> Enum.find(fn %VarInfo{name: name} -> name == variable end)

        if var_info != nil do
          %VarInfo{positions: [{line, column} | _]} = var_info
          %Location{type: :variable, file: nil, line: line, column: column}
        else
          find_function_or_module(
            {nil, variable},
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
          mods_funs_to_positions,
          env,
          metadata_types,
          binding_env
        )
    end
  end

  defp find_function_or_module(
         {module, function},
         mods_funs_to_positions,
         env,
         metadata_types,
         binding_env,
         visited \\ []
       ) do
    unless {module, function} in visited do
      do_find_function_or_module(
        {module, function},
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
         mods_funs_to_positions,
         env,
         metadata_types,
         _binding_env,
         _visited
       ) do
    %State.Env{
      module: current_module,
      imports: imports,
      aliases: aliases
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
           aliases,
           current_module,
           mods_funs_to_positions,
           metadata_types
         ) do
      {_, _, false} ->
        nil

      {mod, fun, true} ->
        case mods_funs_to_positions[{mod, fun, nil}] || metadata_types[{mod, fun, nil}] do
          nil ->
            Location.find_source({mod, fun}, current_module)

          %TypeInfo{positions: positions} ->
            # for simplicity take last position here as positions are reversed
            {line, column} = positions |> Enum.at(-1)

            %Location{
              file: nil,
              type: :typespec,
              line: line,
              column: column
            }

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
    end
  end
end
