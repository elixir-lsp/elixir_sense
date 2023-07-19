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
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.SurroundContext
  alias ElixirSense.Location

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
        nil

      {:keyword, _} ->
        nil

      {:variable, variable} ->
        var_info =
          vars
          |> Enum.find(fn
            %VarInfo{name: name, positions: positions} ->
              name == variable and context.begin in positions
          end)

        if var_info != nil do
          {definition_line, definition_column} = Enum.min(var_info.positions)

          %Location{type: :variable, file: nil, line: definition_line, column: definition_column}
        else
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
          %Location{type: :attribute, file: nil, line: line, column: column}
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
         {{:attribute, _attr} = type, function},
         context,
         env,
         metadata,
         binding_env,
         visited
       ) do
    case Binding.expand(binding_env, type) do
      {:atom, module} ->
        do_find_function_or_module(
          {{:atom, Introspection.expand_alias(module, env.aliases)}, function},
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
         %State.Env{scope: {function, arity}, module: module} = env,
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
           metadata.mods_funs_to_positions,
           metadata.types
         ) do
      {_, _, false, _} ->
        nil

      {mod, fun, true, :mod_fun} ->
        {line, column} = context.end
        call_arity = Metadata.get_call_arity(metadata, mod, fun, line, column)
        other = Metadata.get_call_arity(metadata, line, column)
        if other != call_arity do
          IO.puts "with fun #{call_arity} old #{other}"
        end
        fn_definition = find_fn_definition(mod, fun, call_arity, line, metadata.mods_funs_to_positions)

        case fn_definition || metadata.mods_funs_to_positions[{mod, fun, nil}] do
          nil ->
            # TODO fallback to other arities?
            Location.find_mod_fun_source(mod, fun, call_arity)

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
        {line, column} = context.end
        call_arity = Metadata.get_call_arity(metadata, mod, fun, line, column)
        other = Metadata.get_call_arity(metadata, line, column)
        if other != call_arity do
          IO.puts "with fun #{call_arity} old #{other}"
        end

        case metadata.types[{mod, fun, call_arity}] || metadata.types[{mod, fun, nil}] do
          nil ->
            # TODO fallback to other arities?
            Location.find_type_source(mod, fun, call_arity)

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

  defp find_fn_definition(_mod, nil, nil, _line, _mods_funs_to_positions) do
    nil
  end

  defp find_fn_definition(mod, fun, call_arity, line, mods_funs_to_positions) do
    mods_funs_to_positions
    |> Enum.find(fn
      {{^mod, ^fun, fn_arity}, %{positions: fn_positions} = fun_info} when not is_nil(fn_arity) ->
        # assume function head is first in code and last in metadata
        default_args = fun_info.params |> Enum.at(-1) |> Introspection.count_defaults()

        case call_arity do
          nil -> Enum.any?(fn_positions, fn {fn_line, _fn_col} -> fn_line == line end)
          _ -> call_arity in (fn_arity - default_args)..fn_arity
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
