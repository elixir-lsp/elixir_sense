defmodule ElixirSense.Core.Options do
  alias ElixirSense.Core.Normalized.Typespec, as: NormalizedTypespec

  def get_param_options(module, function, arity, metadata) do
    case metadata.specs[{module, function, arity}] do
      nil ->
        if Code.ensure_loaded?(module) do
          NormalizedTypespec.get_specs(module) |> dbg
          # does not drop MACRO- prefix
          {_behaviour, specs} =
            ElixirSense.Core.TypeInfo.get_function_specs(module, function, :any) |> dbg

          with [type] <- ElixirSense.Core.TypeInfo.get_param_type_specs(specs, arity - 1),

          {:"::", _,
           [
             {:foo, _, []},
             options
           ]} <- NormalizedTypespec.type_to_quoted({:foo, type, []}) do

            # TODO var args
          extract_options(options |> dbg, [], metadata, module, [])
           else
            _ -> []
        end
        else
          []
        end

      %ElixirSense.Core.State.SpecInfo{specs: [spec | _]} = info ->
        extract_options_from_spec(spec, metadata, module)
    end
  end

  defp extract_options_from_spec(spec, metadata, module) do
    with {:ok,
          {:@, _,
           [
             {:spec, _,
              [
                {:"::", _,
                 [
                   {_fun, _, params},
                   _
                 ]}
              ]}
           ]}}
         when is_list(params) <- Code.string_to_quoted(spec),
         type <- List.last(params) |> dbg do
          # TODO var args
      extract_options(type |> dbg, [], metadata, module, [])
    else
      _ ->
        []
    end
  end

  defp extract_options({:list, meta, [arg]}, acc, metadata, module, named_args), do: extract_options(arg, acc, metadata, module, named_args)

  defp extract_options({:|, _, [{atom, type}, rest]}, acc, metadata, module, named_args) when is_atom(atom) do
    extract_options(rest, [{atom, expand_type(type, metadata, module, named_args)} | acc], metadata, module, named_args)
  end

  defp extract_options({:|, meta, [other, rest]}, acc, metadata, module, named_args) do
    case expand_type(other, metadata, module, named_args) do
      nil -> extract_options(rest, acc, metadata, module, named_args)
      type -> extract_options({:|, meta, [type, rest]}, acc, metadata, module, named_args)
    end
  end

  defp extract_options([{:{}, _, [atom, type]}], acc, metadata, module, named_args) when is_atom(atom) do
    [{atom, expand_type(type, metadata, module, named_args)} | acc] |> Enum.sort()
  end

  defp extract_options([{atom, type} | rest], acc, metadata, module, named_args) when is_atom(atom) do
    extract_options(rest, [{atom, expand_type(type, metadata, module, named_args)} | acc], metadata, module, named_args)
  end

  defp extract_options({atom, type}, acc, metadata, module, named_args) when is_atom(atom) do
    [{atom, expand_type(type, metadata, module, named_args)} | acc] |> Enum.sort()
  end

  defp extract_options({atom, _, args} = local_call, acc, metadata, module, named_args) when is_atom(atom) and (is_list(args) or args == nil) do
    case expand_type(local_call, metadata, module, named_args) do
      nil -> Enum.sort(acc)
      type -> extract_options(type, acc, metadata, module, named_args)
    end
  end

  defp extract_options(_other, acc, _metadata, _module, _named_args), do: Enum.sort(acc)

  def expand_type({{:., dot_meta, [remote, type]}, call_meta, args}, metadata, module, named_args) do
    remote = case remote do
      atom when is_atom(atom) -> remote
      {:__aliases__, _, list} -> Module.concat(list)
    end
    case find_type(remote, type, args, metadata) do
      {:ok, type, new_named_args} -> expand_type(type, metadata, module, new_named_args ++ named_args)
      :error -> {{:., dot_meta, [remote, type]}, call_meta, args}
    end
  end

  def expand_type({name, meta, args}, metadata, module, named_args) when is_atom(name) do
    args = (args || []) |> Enum.map(& expand_type(&1, metadata, module, named_args))
    named_arg = Keyword.fetch(named_args, name)
    cond do
      match?({:ok, _}, named_arg) ->
        {:ok, expanded_arg} = named_arg
        expanded_arg
      :erl_internal.is_type(name, length(args)) ->
        {name, meta, args}
      true ->
        case find_type(module, name, args, metadata) do
          {:ok, type, new_named_args} -> expand_type(type, metadata, module, new_named_args ++ named_args)
          :error -> {{:., meta, [module, name]}, meta, args}
        end
    end
  end

  # def expand_type(list, metadata, module, named_args) when is_list do
  #   list |> Enum.map(&expand_type(&1, metadata, module, named_args))
  # end

  def expand_type(literal, _metadata, _module, _named_args), do: literal

  defp find_type(module, name, args, metadata) do
    args = args || []

    case metadata.types[{module, name, length(args)}] do
      nil ->
        :error
      %ElixirSense.Core.State.TypeInfo{specs: [spec | _]} = info ->
        dbg(spec)
        with {:ok, {:@, _,
        [
          {_kind, _,
           [
             {:"::", _,
              [
                {_name, _, arg_names},
                type
              ]}
           ]}
        ]}} <- Code.string_to_quoted(spec) do
          dbg(arg_names)
          arg_names = for {arg_name, _, context} when is_atom(context) <- arg_names, do: arg_name
          {:ok, type, Enum.zip(arg_names, args)} |> dbg
        else
          _ -> :error
        end
    end
  end
end
