defmodule ElixirSense.Providers.Implementation do
  @moduledoc """
  Provides a function to find out where symbols are implemented.
  """

  alias ElixirSense.Core.Behaviours
  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Normalized
  alias ElixirSense.Core.State
  alias ElixirSense.Core.State.ModFunInfo
  alias ElixirSense.Core.SurroundContext
  alias ElixirSense.Location

  @doc """
  Finds out where a callback, protocol or delegate was implemented.
  """
  @spec find(
          any(),
          State.Env.t(),
          State.mods_funs_to_positions_t(),
          State.types_t()
        ) :: [%Location{}]
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
        []

      {kind, _} when kind in [:attribute] ->
        []

      {module_type, function} ->
        module =
          case Binding.expand(binding_env, module_type) do
            {:atom, module} ->
              Introspection.expand_alias(module, env.aliases)

            _ ->
              env.module
          end

        behaviour_implementations =
          find_behaviour_implementations(module, function, module, env, binding_env)

        if behaviour_implementations == [] do
          find_delegatee(
            {module, function},
            mods_funs_to_positions,
            env,
            metadata_types,
            binding_env
          )
          |> List.wrap()
        else
          behaviour_implementations
        end
    end
  end

  def find_behaviour_implementations(maybe_found_module, maybe_fun, module, env, binding_env) do
    case maybe_found_module || module do
      nil ->
        []

      found_module ->
        found_module = expand(found_module, binding_env)

        cond do
          maybe_fun == nil or is_callback(found_module, maybe_fun) ->
            get_locations(found_module, maybe_fun)

          maybe_fun != nil ->
            for behaviour <- env.behaviours,
                is_callback(behaviour, maybe_fun) do
              get_locations(behaviour, maybe_fun)
            end
            |> List.flatten()

          true ->
            []
        end
    end
    |> Enum.reject(&is_nil/1)
  end

  defp expand({:attribute, _attr} = type, binding_env) do
    case Binding.expand(binding_env, type) do
      {:atom, atom} -> atom
      _ -> nil
    end
  end

  defp expand(other, _binding_env), do: other

  defp get_locations(behaviour, maybe_callback) do
    Behaviours.get_all_behaviour_implementations(behaviour)
    |> Enum.map(fn implementation ->
      Location.find_source({implementation, maybe_callback}, nil)
    end)
  end

  defp is_callback(behaviour, fun) when is_atom(behaviour) do
    Code.ensure_loaded?(behaviour) and
      function_exported?(behaviour, :behaviour_info, 1) and
      behaviour.behaviour_info(:callbacks)
      |> Enum.any?(&match?({^fun, _}, &1))
  end

  defp find_delegatee(
         {module, function},
         mods_funs_to_positions,
         env,
         metadata_types,
         binding_env,
         visited \\ []
       ) do
    unless {module, function} in visited do
      do_find_delegatee(
        {module, function},
        mods_funs_to_positions,
        env,
        metadata_types,
        binding_env,
        [{module, function} | visited]
      )
    end
  end

  defp do_find_delegatee(
         {{:attribute, _attr} = type, function},
         mods_funs_to_positions,
         env,
         metadata_types,
         binding_env,
         visited
       ) do
    case Binding.expand(binding_env, type) do
      {:atom, module} ->
        do_find_delegatee(
          {Introspection.expand_alias(module, env.aliases), function},
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

  defp do_find_delegatee(
         {module, function},
         mods_funs_to_positions,
         env,
         metadata_types,
         binding_env,
         visited
       ) do
    %State.Env{
      module: current_module,
      imports: imports,
      requires: requires,
      aliases: aliases
    } = env

    case {module, function}
         |> Introspection.actual_mod_fun(
           imports,
           requires,
           aliases,
           current_module,
           mods_funs_to_positions,
           metadata_types
         ) do
      {mod, fun, true} when not is_nil(fun) ->
        case mods_funs_to_positions[{mod, fun, nil}] do
          nil ->
            find_delegatee_location(mod, fun, current_module, visited)

          %ModFunInfo{type: :defdelegate, target: target} when not is_nil(target) ->
            find_delegatee(
              target,
              mods_funs_to_positions,
              env,
              metadata_types,
              binding_env,
              visited
            )

          _ ->
            # not a delegate
            nil
        end

      _ ->
        nil
    end
  end

  defp find_delegatee_location(mod, fun, current_module, visited) do
    case Normalized.Code.get_docs(mod, :docs)
         |> List.wrap()
         |> Enum.find(&match?({{^fun, _}, _, :function, _, _, %{delegate_to: _}}, &1)) do
      nil ->
        # ensure we are expanding a delegate
        if length(visited) > 1 do
          Location.find_source({mod, fun}, current_module)
        end

      {_, _, _, _, _,
       %{
         delegate_to: {delegate_mod, delegate_fun, _}
       }} ->
        Location.find_source({delegate_mod, delegate_fun}, current_module)
    end
  end
end
