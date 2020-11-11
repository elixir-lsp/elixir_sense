defmodule ElixirSense.Providers.Implementation do
  @moduledoc """
  Provides a function to find out where symbols are implemented.
  """

  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State
  alias ElixirSense.Location

  @doc """
  Finds out where a module, function, macro or variable was defined.
  """
  @spec find(
          String.t(),
          State.Env.t()
        ) :: [%Location{}]
  def find(
        subject,
        %State.Env{
          aliases: aliases,
          module: module
        }
      ) do
    {maybe_found_module, maybe_fun} = Source.split_module_and_func(subject, module, aliases)

    case maybe_found_module || module do
      nil ->
        []

      found_module ->
        Introspection.get_all_behaviour_implementations(found_module)
        |> Enum.map(fn implementation ->
          Location.find_source({implementation, maybe_fun}, nil)
        end)
    end
    |> Enum.reject(&is_nil/1)
  end

  # defp subject_is_call?(subject, calls) do
  #   Enum.find(calls, fn
  #     %State.CallInfo{func: func} ->
  #       Atom.to_string(func) == subject

  #     _ ->
  #       false
  #   end) != nil
  # end

  # defp find_attribute("@" <> attribute_name, attributes) do
  #   Enum.find(attributes, fn
  #     %State.AttributeInfo{name: name} ->
  #       Atom.to_string(name) == attribute_name

  #     _ ->
  #       false
  #   end)
  # end

  # defp find_attribute(_, _attributes), do: nil

  # defp find_function_or_module(
  #        {module, function},
  #        mods_funs_to_positions,
  #        env,
  #        metadata_types,
  #        binding_env,
  #        visited \\ []
  #      ) do
  #   unless {module, function} in visited do
  #     do_find_function_or_module(
  #       {module, function},
  #       mods_funs_to_positions,
  #       env,
  #       metadata_types,
  #       binding_env,
  #       [{module, function} | visited]
  #     )
  #   end
  # end

  # defp do_find_function_or_module(
  #        {{:attribute, _attr} = type, function},
  #        mods_funs_to_positions,
  #        env,
  #        metadata_types,
  #        binding_env,
  #        visited
  #      ) do
  #   case Binding.expand(binding_env, type) do
  #     {:atom, module} ->
  #       do_find_function_or_module(
  #         {Introspection.expand_alias(module, env.aliases), function},
  #         mods_funs_to_positions,
  #         env,
  #         metadata_types,
  #         binding_env,
  #         visited
  #       )

  #     _ ->
  #       nil
  #   end
  # end

  # defp do_find_function_or_module(
  #        {nil, :super},
  #        mods_funs_to_positions,
  #        %State.Env{scope: {function, arity}, module: module} = env,
  #        metadata_types,
  #        binding_env,
  #        visited
  #      ) do
  #   case mods_funs_to_positions[{module, function, arity}] do
  #     %ModFunInfo{overridable: {true, origin}} ->
  #       # overridable function is most likely defined by __using__ macro
  #       do_find_function_or_module(
  #         {origin, :__using__},
  #         mods_funs_to_positions,
  #         env,
  #         metadata_types,
  #         binding_env,
  #         visited
  #       )

  #     _ ->
  #       nil
  #   end
  # end

  # defp do_find_function_or_module(
  #        {module, function},
  #        mods_funs_to_positions,
  #        env,
  #        metadata_types,
  #        binding_env,
  #        visited
  #      ) do
  #   %State.Env{
  #     module: current_module,
  #     imports: imports,
  #     aliases: aliases
  #   } = env

  #   case {module, function}
  #        |> Introspection.actual_mod_fun(
  #          imports,
  #          aliases,
  #          current_module,
  #          mods_funs_to_positions,
  #          metadata_types
  #        ) do
  #     {_, _, false} ->
  #       nil

  #     {mod, fun, true} ->
  #       case mods_funs_to_positions[{mod, fun, nil}] || metadata_types[{mod, fun, nil}] do
  #         nil ->
  #           {mod, fun} |> find_source(current_module)

  #         %TypeInfo{positions: positions} ->
  #           # for simplicity take last position here as positions are reversed
  #           {line, column} = positions |> Enum.at(-1)

  #           %Location{
  #             file: nil,
  #             type: :typespec,
  #             line: line,
  #             column: column
  #           }

  #         %ModFunInfo{type: :defdelegate, target: target} when not is_nil(target) ->
  #           find_function_or_module(
  #             target,
  #             mods_funs_to_positions,
  #             env,
  #             metadata_types,
  #             binding_env,
  #             visited
  #           )

  #         %ModFunInfo{positions: positions} = mi ->
  #           # for simplicity take last position here as positions are reversed
  #           {line, column} = positions |> Enum.at(-1)

  #           %Location{
  #             file: nil,
  #             type: ModFunInfo.get_category(mi),
  #             line: line,
  #             column: column
  #           }
  #       end
  #   end
  # end
end
