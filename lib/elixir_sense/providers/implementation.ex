defmodule ElixirSense.Providers.Implementation do
  @moduledoc """
  Provides a function to find out where symbols are implemented.
  """

  alias ElixirSense.Core.Behaviours
  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Normalized
  alias ElixirSense.Core.State
  alias ElixirSense.Core.State.ModFunInfo
  alias ElixirSense.Core.SurroundContext
  alias ElixirSense.Location

  require ElixirSense.Core.Introspection, as: Introspection

  @doc """
  Finds out where a callback, protocol or delegate was implemented.
  """
  @spec find(
          any(),
          State.Env.t(),
          Metadata.t()
        ) :: [%Location{}]
  def find(
        context,
        %State.Env{
          module: module,
          vars: vars,
          attributes: attributes
        } = env,
        metadata
      ) do
    binding_env = %Binding{
      attributes: attributes,
      variables: vars,
      current_module: module
    }

    type = SurroundContext.to_binding(context.context, module)

    case type do
      nil ->
        []

      {kind, _} when kind in [:attribute, :keyword] ->
        []

      {module_type, function} ->
        module =
          case Binding.expand(binding_env, module_type) do
            {:atom, module} ->
              Introspection.expand_alias(module, env.aliases)

            _ ->
              env.module
          end

        {line, column} = context.end
        call_arity = Metadata.get_call_arity(metadata, module, function, line, column) || :any

        behaviour_implementations =
          find_behaviour_implementations(
            module,
            function,
            call_arity,
            module,
            env,
            metadata,
            binding_env
          )

        if behaviour_implementations == [] do
          find_delegatee(
            {module, function},
            call_arity,
            env,
            metadata,
            binding_env
          )
          |> List.wrap()
        else
          behaviour_implementations
        end
    end
  end

  def find_behaviour_implementations(
        maybe_found_module,
        maybe_fun,
        arity,
        module,
        env,
        metadata,
        binding_env
      ) do
    case maybe_found_module || module do
      nil ->
        []

      found_module ->
        found_module = expand(found_module, binding_env)

        cond do
          maybe_fun == nil or is_callback(found_module, maybe_fun, arity, metadata) ->
            # protocol function call
            get_locations(found_module, maybe_fun, arity, metadata)

          maybe_fun != nil ->
            # try to get behaviours from the target module - metadata
            behaviours =
              case metadata.mods_funs_to_positions[{module, maybe_fun, nil}] do
                %{positions: [{l, c} | _]} ->
                  def_env = Metadata.get_env(metadata, {l, c})
                  def_env.behaviours

                _ ->
                  []
              end

            # try to get behaviours from the target module - introspection
            behaviours =
              if behaviours == [] do
                Behaviours.get_module_behaviours(module)
              else
                behaviours
              end

            # get behaviours from current env
            behaviours =
              if behaviours == [] do
                env.behaviours
              else
                behaviours
              end

            # callback/protocol implementation def
            for behaviour <- behaviours,
                is_callback(behaviour, maybe_fun, arity, metadata) do
              get_locations(behaviour, maybe_fun, arity, metadata)
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

  defp get_locations(behaviour, maybe_callback, arity, metadata) do
    metadata_implementations =
      for {_, env} <- metadata.lines_to_env,
          behaviour in env.behaviours,
          module <- env.module_variants,
          uniq: true,
          do: module

    metadata_implementations_locations =
      metadata_implementations
      |> Enum.map(fn module ->
        {{line, column}, type} =
          metadata.mods_funs_to_positions
          |> Enum.find_value(fn
            {{^module, ^maybe_callback, a}, info} when is_nil(maybe_callback) ->
              {List.last(info.positions), info.type}

            {{^module, ^maybe_callback, a}, info} when not is_nil(a) ->
              defaults = info.params |> List.last() |> Introspection.count_defaults()

              if Introspection.matches_arity_with_defaults?(a, defaults, arity) do
                {List.last(info.positions), info.type}
              end

            _ ->
              nil
          end)

        kind =
          case type do
            :defmodule -> :module
            :def -> :function
            :defmacro -> :macro
          end

        {module, %Location{type: kind, file: nil, line: line, column: column}}
      end)

    introspection_implementations_locations =
      Behaviours.get_all_behaviour_implementations(behaviour)
      |> Enum.map(fn implementation ->
        {implementation, Location.find_mod_fun_source(implementation, maybe_callback, arity)}
      end)

    Keyword.merge(introspection_implementations_locations, metadata_implementations_locations)
    |> Keyword.values()
  end

  defp is_callback(behaviour, fun, arity, metadata) when is_atom(behaviour) do
    metadata_callback =
      metadata.specs
      |> Enum.any?(
        &match?(
          {{^behaviour, ^fun, cb_arity}, %{kind: kind}}
          when kind in [:callback, :macrocallback] and
                 Introspection.matches_arity?(cb_arity, arity),
          &1
        )
      )

    metadata_callback or
      (Code.ensure_loaded?(behaviour) and
         function_exported?(behaviour, :behaviour_info, 1) and
         behaviour.behaviour_info(:callbacks)
         |> Enum.map(&Introspection.drop_macro_prefix/1)
         |> Enum.any?(
           &match?({^fun, cb_arity} when Introspection.matches_arity?(cb_arity, arity), &1)
         ))
  end

  defp find_delegatee(
         mf,
         arity,
         env,
         metadata,
         binding_env,
         visited \\ []
       ) do
    unless mf in visited do
      do_find_delegatee(
        mf,
        arity,
        env,
        metadata,
        binding_env,
        [mf | visited]
      )
    end
  end

  defp do_find_delegatee(
         {{:attribute, _attr} = type, function},
         arity,
         env,
         metadata,
         binding_env,
         visited
       ) do
    case Binding.expand(binding_env, type) do
      {:atom, module} ->
        do_find_delegatee(
          {Introspection.expand_alias(module, env.aliases), function},
          arity,
          env,
          metadata,
          binding_env,
          visited
        )

      _ ->
        nil
    end
  end

  defp do_find_delegatee(
         {module, function},
         arity,
         env,
         metadata,
         binding_env,
         visited
       ) do
    %State.Env{
      module: current_module,
      imports: imports,
      requires: requires,
      aliases: aliases,
      scope: scope
    } = env

    case {module, function}
         |> Introspection.actual_mod_fun(
           imports,
           requires,
           aliases,
           current_module,
           scope,
           metadata.mods_funs_to_positions,
           metadata.types
         ) do
      {mod, fun, true, :mod_fun} when not is_nil(fun) ->
        # on defdelegate - no need for arity fallback here
        info =
          Location.get_function_position_using_metadata(
            mod,
            fun,
            arity,
            metadata.mods_funs_to_positions
          )

        case info do
          nil ->
            find_delegatee_location(mod, fun, arity, visited)

          %ModFunInfo{type: :defdelegate, target: target} when not is_nil(target) ->
            find_delegatee(
              target,
              arity,
              env,
              metadata,
              binding_env,
              visited
            )

          %ModFunInfo{positions: positions, type: :def} ->
            # find_delegatee_location(mod, fun, arity, visited)
            if length(visited) > 1 do
              {line, column} = List.last(positions)
              %Location{type: :function, file: nil, line: line, column: column}
            end

          _ ->
            nil
        end

      _ ->
        nil
    end
  end

  defp find_delegatee_location(mod, fun, arity, visited) do
    defdelegate_from_docs = get_defdelegate_by_docs(mod, fun, arity)

    case defdelegate_from_docs do
      nil ->
        # ensure we are expanding a delegate
        if length(visited) > 1 do
          # on defdelegate - no need for arity fallback
          Location.find_mod_fun_source(mod, fun, arity)
        end

      {_, _, _, _, _,
       %{
         delegate_to: {delegate_mod, delegate_fun, delegate_arity}
       }} ->
        # on call of delegated function - arity fallback already done
        Location.find_mod_fun_source(delegate_mod, delegate_fun, delegate_arity)
    end
  end

  defp get_defdelegate_by_docs(mod, fun, arity) do
    Normalized.Code.get_docs(mod, :docs)
    |> List.wrap()
    |> Enum.filter(fn
      {{^fun, a}, _, :function, _, _, %{delegate_to: _} = meta} ->
        default_args = Map.get(meta, :defaults, 0)
        Introspection.matches_arity_with_defaults?(a, default_args, arity)

      _ ->
        false
    end)
    |> Enum.min_by(
      fn {{_, a}, _, _, _, _, _} -> a end,
      &<=/2,
      fn -> nil end
    )
  end
end
