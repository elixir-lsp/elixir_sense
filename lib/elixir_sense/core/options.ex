defmodule ElixirSense.Core.Options do
  alias ElixirSense.Core.Normalized.Typespec, as: NormalizedTypespec
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.State.ModFunInfo

  defp get_spec_ast_from_info(spec_info) do
    for spec <- spec_info.specs do
      case Code.string_to_quoted(spec) do
        {:ok, {:@, _, [{_kind, _, [ast]}]}} -> ast
        _ -> nil
      end
    end
    |> Enum.reject(&is_nil/1)
  end

  defp maybe_unpack_caller(type, :function), do: type

  defp maybe_unpack_caller(
         {:type, line1, :fun, [{:type, line2, :product, [_ | args]} | tail]},
         :macro
       ) do
    {:type, line1, :fun, [{:type, line2, :product, args} | tail]}
  end

  defp maybe_unpack_caller({:type, line, :bounded_fun, [head | tail]}, :macro) do
    {:type, line, :bounded_fun, [maybe_unpack_caller(head, :macro) | tail]}
  end

  defp get_spec(module, function, arity, kind, behaviours, metadata) do
    spec_info = Map.get(metadata.specs, {module, function, arity})

    if spec_info do
      get_spec_ast_from_info(spec_info)
    else
      Enum.find_value(behaviours, fn behaviour ->
        if Map.has_key?(metadata.mods_funs_to_positions, {behaviour, nil, nil}) do
          spec_info = Map.get(metadata.specs, {behaviour, function, arity})

          if spec_info do
            get_spec_ast_from_info(spec_info)
          end
        else
          if Code.ensure_loaded?(behaviour) do
            behaviour_specs = ElixirSense.Core.TypeInfo.get_module_callbacks(behaviour)

            {modified_function, modified_arity} =
              case kind do
                :function ->
                  {function, arity}

                :macro ->
                  # modify name and arity for callback retrieval
                  # caller arg is dropped in maybe_unpack_caller
                  {:"MACRO-#{function}", arity + 1}
              end

            callback_specs =
              for {{f, a}, {_, spec_entries}} <- behaviour_specs,
                  f == modified_function,
                  a == modified_arity,
                  spec <- spec_entries do
                spec = maybe_unpack_caller(spec, kind)
                NormalizedTypespec.spec_to_quoted(function, spec)
              end

            callback_specs
          end
        end
      end)
    end
  end

  def get_param_options(module, function, arity, env, metadata) do
    behaviours = env.behaviours
    # TODO filter instead of find_value?
    candidate =
      metadata.mods_funs_to_positions
      |> Enum.find_value(fn
        {{^module, ^function, ^arity}, info} ->
          kind = ModFunInfo.get_category(info)
          specs = get_spec(module, function, arity, kind, behaviours, metadata)

          if specs do
            {specs, (arity - 1)..(arity - 1)}
          end

        {{^module, ^function, a}, info} when a > arity ->
          kind = ModFunInfo.get_category(info)
          # assume function head is first in code and last in metadata
          head_params = Enum.at(info.params, -1)
          default_args = Introspection.count_defaults(head_params)

          if a - default_args <= arity do
            specs = get_spec(module, function, a, kind, behaviours, metadata)

            if specs do
              # we can guess the position of keyword argument in params
              {specs, (arity - 1)..min(arity - 1 + default_args, a - 1)}
            end
          end

        _ ->
          false
      end)

    case candidate do
      nil ->
        if Code.ensure_loaded?(module) do
          # TODO filter instead of find_value
          candidate =
            ElixirSense.Core.Normalized.Code.get_docs(module, :docs)
            |> Enum.find_value(fn
              {{^function, ^arity}, _, kind, _, _, _meta} ->
                {kind, arity, (arity - 1)..(arity - 1)}

              {{^function, a}, _, kind, _, _, %{defaults: default_args}}
              when a > arity and a - default_args <= arity ->
                # we can guess the position of keyword argument in params
                {kind, a, (arity - 1)..min(arity - 1 + default_args, a - 1)}

              _ ->
                false
            end)

          if candidate do
            {kind, modified_function, modified_arity, parameter_position_range} =
              case candidate do
                {:function, arity, parameter_position_range} ->
                  {:function, function, arity, parameter_position_range}

                {:macro, arity, parameter_position_range} ->
                  # we need to add macro argument for typespec retrieval
                  # position range remains unchanged as we drop it in maybe_unpack_caller
                  {:macro, :"MACRO-#{function}", arity + 1, parameter_position_range}
              end

            {_behaviour, specs} =
              ElixirSense.Core.TypeInfo.get_function_specs(
                module,
                modified_function,
                modified_arity
              )

            for {_, spec_entries} <- specs, spec <- spec_entries do
              spec = maybe_unpack_caller(spec, kind)

              NormalizedTypespec.spec_to_quoted(function, spec)
              |> get_params_and_named_args(parameter_position_range)
            end
            |> Enum.flat_map(fn
              {:ok, params, named_args} ->
                extract_from_params(params, named_args, metadata, module)

              _ ->
                []
            end)
          else
            []
          end
        else
          []
        end

      {specs, parameter_position_range} ->
        for spec <- specs do
          get_params_and_named_args(spec, parameter_position_range)
        end
        |> Enum.flat_map(fn
          {:ok, params, named_args} ->
            extract_from_params(params, named_args, metadata, module)

          _ ->
            []
        end)
    end
  end

  defp extract_from_params(params, named_args, metadata, module) do
    for param <- params do
      param
      |> expand_type(metadata, module, named_args)
      |> extract_options([])
    end
    |> List.flatten()
  end

  defp get_params_and_named_args({:"::", _, [{_fun, _, params}, _]}, parameter_position_range)
       when is_list(params) do
    {:ok, Enum.slice(params, parameter_position_range), []}
  end

  defp get_params_and_named_args(
         {:when, _, [{:"::", _, [{_fun, _, params}, _]}, named_args]},
         parameter_position_range
       )
       when is_list(params) and is_list(named_args) do
    {:ok, Enum.slice(params, parameter_position_range), named_args}
  end

  defp get_params_and_named_args(_, _), do: :error

  defp extract_options({:"::", _meta, [{name, _, context}, type]}, acc)
       when is_atom(name) and is_atom(context),
       do: extract_options(type, acc)

  defp extract_options({:list, _meta, [arg]}, acc),
    do: extract_options(arg, acc)

  defp extract_options({:|, _, [atom1, atom2]}, acc)
       when is_atom(atom1) and is_atom(atom2) do
    [atom2, atom1 | acc]
  end

  defp extract_options({:|, _, [atom1, {atom2, type2}]}, acc)
       when is_atom(atom1) and is_atom(atom2) do
    [{atom2, type2}, atom1 | acc]
  end

  defp extract_options({:|, _, [{atom1, type1}, atom2]}, acc)
       when is_atom(atom1) and is_atom(atom2) do
    [atom2, {atom1, type1} | acc]
  end

  defp extract_options({:|, _, [{atom1, type1}, {atom2, type2}]}, acc)
       when is_atom(atom1) and is_atom(atom2) do
    [{atom2, type2}, {atom1, type1} | acc]
  end

  defp extract_options({:|, _, [atom, rest]}, acc)
       when is_atom(atom) do
    extract_options(rest, [atom | acc])
  end

  defp extract_options({:|, _, [{atom, type}, rest]}, acc)
       when is_atom(atom) do
    extract_options(rest, [{atom, type} | acc])
  end

  defp extract_options({:|, _, [atom1, {:{}, _, [atom2, type2]}]}, acc)
       when is_atom(atom1) and is_atom(atom2) do
    [{atom2, type2}, atom1 | acc]
  end

  defp extract_options({:|, _, [{:{}, _, [atom1, type1]}, atom2]}, acc)
       when is_atom(atom1) and is_atom(atom2) do
    [atom2, {atom1, type1} | acc]
  end

  defp extract_options({:|, _, [atom1, atom2]}, acc)
       when is_atom(atom1) and is_atom(atom2) do
    [atom2, atom1 | acc]
  end

  defp extract_options({:|, _, [{:{}, _, [atom1, type1]}, {:{}, _, [atom2, type2]}]}, acc)
       when is_atom(atom1) and is_atom(atom2) do
    [{atom2, type2}, {atom1, type1} | acc]
  end

  defp extract_options({:|, _, [atom, rest]}, acc)
       when is_atom(atom) do
    extract_options(rest, [atom | acc])
  end

  defp extract_options({:|, _, [{:{}, _, [atom, type]}, rest]}, acc)
       when is_atom(atom) do
    extract_options(rest, [{atom, type} | acc])
  end

  defp extract_options({:|, _meta, [other, rest]}, acc) do
    acc = extract_options(other, acc)
    extract_options(rest, acc)
  end

  defp extract_options([{:{}, _, [atom, type]}], acc)
       when is_atom(atom) do
    [{atom, type} | acc] |> Enum.sort()
  end

  defp extract_options([atom | rest], acc)
       when is_atom(atom) do
    extract_options(rest, [atom | acc])
  end

  defp extract_options([{atom, type} | rest], acc)
       when is_atom(atom) do
    extract_options(rest, [{atom, type} | acc])
  end

  defp extract_options([rest], acc) do
    extract_options(rest, acc)
  end

  defp extract_options(atom, acc) when is_atom(atom) do
    [atom | acc] |> Enum.sort()
  end

  defp extract_options({atom, type}, acc) when is_atom(atom) do
    [{atom, type} | acc] |> Enum.sort()
  end

  defp extract_options(_other, acc), do: Enum.sort(acc)

  def expand_type(type, metadata, module, named_args, stack \\ []) do
    if type in stack do
      type
    else
      do_expand_type(type, metadata, module, named_args, [type | stack])
    end
  end

  def do_expand_type(
        {:"::", meta, [{name, name_meta, context}, right]},
        metadata,
        module,
        named_args,
        stack
      )
      when is_atom(name) and is_atom(context) do
    {:"::", meta,
     [{name, name_meta, context}, expand_type(right, metadata, module, named_args, stack)]}
  end

  def do_expand_type(
        {{:., dot_meta, [remote, type]}, call_meta, args},
        metadata,
        module,
        named_args,
        stack
      ) do
    remote =
      case remote do
        atom when is_atom(atom) -> remote
        {:__aliases__, _, list} -> Module.concat(list)
      end

    case find_type(remote, type, args, metadata) do
      {:ok, type, new_named_args} ->
        expand_type(type, metadata, module, new_named_args ++ named_args, stack)

      :error ->
        {{:., dot_meta, [remote, type]}, call_meta, args}
    end
  end

  def do_expand_type({name, meta, args}, metadata, module, named_args, stack)
      when is_atom(name) do
    args = (args || []) |> Enum.map(&expand_type(&1, metadata, module, named_args, stack))
    named_arg = Keyword.fetch(named_args, name)

    cond do
      match?({:ok, _}, named_arg) ->
        {:ok, expanded_arg} = named_arg
        expand_type(expanded_arg, metadata, module, named_args, stack)

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
            expand_type(type, metadata, module, new_named_args ++ named_args, stack)

          :error ->
            {name, meta, args}
        end
    end
  end

  def do_expand_type(list, metadata, module, named_args, stack) when is_list(list) do
    list |> Enum.map(&expand_type(&1, metadata, module, named_args, stack))
  end

  def do_expand_type({left, right}, metadata, module, named_args, stack) do
    {expand_type(left, metadata, module, named_args, stack),
     expand_type(right, metadata, module, named_args, stack)}
  end

  def do_expand_type(literal, _metadata, _module, _named_args, _stack), do: literal

  defp find_type(module, name, args, metadata) do
    args = args || []

    case metadata.types[{module, name, length(args)}] do
      nil ->
        if Code.ensure_loaded?(module) do
          case ElixirSense.Core.TypeInfo.get_type_spec(module, name, length(args)) do
            {_kind, spec} ->
              {:"::", _,
               [
                 {_name, _, arg_names},
                 type
               ]} =
                spec
                |> NormalizedTypespec.type_to_quoted()

              arg_names =
                for {arg_name, _, context} when is_atom(context) <- arg_names, do: arg_name

              {:ok, type, Enum.zip(arg_names, args)}

            _ ->
              :error
          end
        else
          :error
        end

      %ElixirSense.Core.State.TypeInfo{specs: [spec | _]} ->
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
          arg_names = for {arg_name, _, context} when is_atom(context) <- arg_names, do: arg_name
          {:ok, type, Enum.zip(arg_names, args)}
        else
          _ -> :error
        end
    end
  end
end
