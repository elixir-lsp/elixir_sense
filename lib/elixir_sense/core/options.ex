defmodule ElixirSense.Core.Options do
  alias ElixirSense.Core.Normalized.Typespec, as: NormalizedTypespec

  def get_param_options(module, function, arity, metadata) do
    # TODO is arity ok? what about default arg functions
    # TODO iterate over behaviours as well
    case metadata.specs[{module, function, arity}] do
      nil ->
        if Code.ensure_loaded?(module) do
          NormalizedTypespec.get_specs(module) |> dbg
          # does not drop MACRO- prefix
          # TODO handle macro
          {_behaviour, specs} =
            ElixirSense.Core.TypeInfo.get_function_specs(module, function, arity) |> dbg

          extracted = for {_, spec_entries} <- specs, spec <- spec_entries do
            NormalizedTypespec.spec_to_quoted(function, spec)
            |> Macro.prewalk(&drop_macro_env/1)
            |> get_params_and_named_args()
          end

          case Enum.find(extracted, & match?({:ok, _, _}, &1)) do
            {:ok, type, named_args} ->
              named_args = expand_type(named_args, metadata, module, [])

              type
              |> expand_type(metadata, module, named_args)
              |> extract_options([])
            _ -> []
          end

          # with [spec] <- ElixirSense.Core.TypeInfo.get_param_type_specs(specs, arity - 1) |> dbg,
          #      {:"::", _,
          #       [
          #         {:foo, _, []},
          #         type
          #       ]} <- NormalizedTypespec.type_to_quoted({:foo, spec, []}) |> dbg do
          #   # TODO var args
          #   named_args = []

          #   type
          #   |> expand_type(metadata, module, named_args)
          #   |> extract_options([])
          # else
          #   _ -> []
          # end
        else
          []
        end

      %ElixirSense.Core.State.SpecInfo{specs: [spec | _]} ->
        # TODO iterate over multiple specs
        extract_options_from_spec(spec, metadata, module)
    end
  end

  defp extract_options_from_spec(spec, metadata, module) do
    with {:ok, {:@, _, [{_kind, _, [ast]}]}} <- Code.string_to_quoted(spec),
         {:ok, type, named_args} <- get_params_and_named_args(ast) do
      named_args = expand_type(named_args, metadata, module, [])

      type
      |> expand_type(metadata, module, named_args)
      |> extract_options([])
    else
      _ ->
        []
    end
  end

  defp get_params_and_named_args({:"::", _, [{_fun, _, params}, _]}) when is_list(params) do
    {:ok, List.last(params), []}
  end

  defp get_params_and_named_args({:when, _, [{:"::", _, [{_fun, _, params}, _]}, named_args]}) when is_list(params) and is_list(named_args) do
    {:ok, List.last(params), named_args}
  end

  defp get_params_and_named_args(_), do: :error

  defp extract_options({:list, _meta, [arg]}, acc),
    do: extract_options(arg, acc)

  defp extract_options({:|, _, [{atom, type}, rest]}, acc)
       when is_atom(atom) do
    extract_options(rest, [{atom, type} | acc])
  end

  defp extract_options({:|, _meta, [_other, rest]}, acc) do
    extract_options(rest, acc)
  end

  defp extract_options([{:{}, _, [atom, type]}], acc)
       when is_atom(atom) do
    [{atom, type} | acc] |> Enum.sort()
  end

  defp extract_options([{atom, type} | rest], acc)
       when is_atom(atom) do
    extract_options(rest, [{atom, type} | acc])
  end

  defp extract_options({atom, type}, acc) when is_atom(atom) do
    [{atom, type} | acc] |> Enum.sort()
  end

  defp extract_options(_other, acc), do: Enum.sort(acc)

  def expand_type(type, metadata, module, named_args) do
    dbg(type)
    do_expand_type(type, metadata, module, named_args)
  end

  def do_expand_type(
        {:"::", meta, [{name, name_meta, context}, right]},
        metadata,
        module,
        named_args
      )
      when is_atom(name) and is_atom(context) do
    {:"::", meta, [{name, name_meta, context}, expand_type(right, metadata, module, named_args)]}
  end

  def do_expand_type(
        {{:., dot_meta, [remote, type]}, call_meta, args},
        metadata,
        module,
        named_args
      ) do
    remote =
      case remote do
        atom when is_atom(atom) -> remote
        {:__aliases__, _, list} -> Module.concat(list)
      end

    case find_type(remote, type, args, metadata) do
      {:ok, type, new_named_args} ->
        expand_type(type, metadata, module, new_named_args ++ named_args)

      :error ->
        {{:., dot_meta, [remote, type]}, call_meta, args}
    end
  end

  def do_expand_type({name, meta, args}, metadata, module, named_args) when is_atom(name) do
    args = (args || []) |> Enum.map(&expand_type(&1, metadata, module, named_args))
    named_arg = Keyword.fetch(named_args, name)

    cond do
      match?({:ok, _}, named_arg) ->
        {:ok, expanded_arg} = named_arg
        expanded_arg

      :erl_internal.is_type(name, length(args)) ->
        {name, meta, args}

      name in [
        :{},
        :%{},
        :%,
        :optional,
        :required,
        :__aliases__,
        :...,
        :->,
        :as_boolean,
        :|,
        :charlist,
        :char_list,
        :nonempty_charlist,
        :struct,
        :keyword,
        :string,
        :nonempty_string,
        :__block__,
        :+,
        :-,
        :__MODULE__,
        :__STACKTRACE__,
        :__CALLER__,
        :__ENV__,
        :__DIR__,
        :..
      ] ->
        {name, meta, args}

      true ->
        case find_type(module, name, args, metadata) do
          {:ok, type, new_named_args} ->
            expand_type(type, metadata, module, new_named_args ++ named_args)

          :error ->
            {name, meta, args}
        end
    end
  end

  def do_expand_type(list, metadata, module, named_args) when is_list(list) do
    list |> Enum.map(&expand_type(&1, metadata, module, named_args))
  end

  def do_expand_type({left, right}, metadata, module, named_args) do
    {expand_type(left, metadata, module, named_args),
     expand_type(right, metadata, module, named_args)}
  end

  def do_expand_type(literal, _metadata, _module, _named_args), do: literal

  defp find_type(module, name, args, metadata) do
    args = args || []

    case metadata.types[{module, name, length(args)}] do
      nil ->
        # TODO find remote type
        :error

      %ElixirSense.Core.State.TypeInfo{specs: [spec | _]} ->
        dbg(spec)

        with {:ok,
              {:@, _,
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

  defp drop_macro_env({name, meta, [{:"::", _, [_, {{:., _, [Macro.Env, :t]}, _, _}]} | args]}),
    do: {name, meta, args}

  defp drop_macro_env(other), do: other
end
