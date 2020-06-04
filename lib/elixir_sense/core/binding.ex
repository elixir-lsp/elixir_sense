defmodule ElixirSense.Core.Binding do
  @moduledoc false

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.Normalized.Typespec
  alias ElixirSense.Core.State
  alias ElixirSense.Core.Struct
  alias ElixirSense.Core.TypeInfo

  defstruct structs: %{},
            variables: [],
            attributes: [],
            current_module: nil,
            imports: [],
            specs: %{},
            types: %{},
            mods_and_funs: %{}

  defp get_fields_from({:map, fields, _}), do: fields
  defp get_fields_from({:struct, fields, _, _}), do: fields
  defp get_fields_from(_), do: []

  defp get_struct_fields(%Binding{structs: structs}, fields, module) do
    if Struct.is_struct(module, structs) do
      fields_values =
        for field <- Struct.get_fields(module, structs), field != :__struct__ do
          {field, fields[field]}
        end

      struct =
        case fields[:__struct__] do
          nil -> {:atom, module}
          other -> other
        end

      {Keyword.put(fields_values, :__struct__, struct), module}
    else
      {Keyword.put_new(fields, :__struct__, nil), nil}
    end
  end

  def expand(%Binding{variables: variables} = env, {:variable, variable}) do
    type =
      case Enum.find(variables, fn %{name: name} -> name == variable end) do
        nil ->
          # no variable found - treat a local call
          expand(env, {:local_call, variable, 0})

        %State.VarInfo{name: name, type: type} ->
          # filter underscored variables
          unless name |> Atom.to_string() |> String.starts_with?("_") do
            type
          end
      end

    expand(env, type)
  end

  def expand(%Binding{attributes: attributes} = env, {:attribute, attribute}) do
    type =
      case Enum.find(attributes, fn %{name: name} -> name == attribute end) do
        nil -> nil
        %State.AttributeInfo{type: type} -> type
      end

    expand(env, type)
  end

  def expand(%Binding{structs: structs} = env, {:struct, fields, module, updated_struct}) do
    # struct type must be a compile time atom or attribute
    module =
      case module do
        {:atom, atom} -> {:atom, atom}
        {:attribute, attr} -> {:attribute, attr}
        _ -> nil
      end

    module =
      case expand(env, module) do
        {:atom, atom} -> atom
        _ -> nil
      end

    if module == nil or Struct.is_struct(module, structs) do
      expanded = expand(env, updated_struct)

      {fields, module} =
        get_struct_fields(env, get_fields_from(expanded) |> Keyword.merge(fields), module)

      {:struct, fields, module, nil}
    end
  end

  def expand(env, {:map, fields, updated_map}) do
    case expand(env, updated_map) do
      {:map, expanded_fields, nil} ->
        {:map, expanded_fields |> Keyword.merge(fields), nil}

      {:struct, expanded_fields, type, nil} ->
        {:struct, expanded_fields |> Keyword.merge(fields), type, nil}

      _ ->
        {:map, fields, nil}
    end
  end

  # remote call
  def expand(env, {:call, target, function, arity}) do
    expanded_target = expand(env, target)
    # do not include private funs on remote call
    expand_call(env, expanded_target, function, arity, false)
    |> drop_no_spec
  end

  # local call
  def expand(
        %Binding{imports: imports, current_module: current_module} = env,
        {:local_call, function, arity}
      ) do
    candidate_targets = [current_module] ++ imports ++ [Kernel, Kernel.SpecialForms]

    # take first matching
    Enum.find_value(candidate_targets, fn candidate ->
      # include private from current module
      include_private = candidate == current_module
      expand_call(env, {:atom, candidate}, function, arity, include_private)
    end)
    |> drop_no_spec
  end

  def expand(_env, {:atom, atom}), do: {:atom, atom}

  def expand(_env, _other), do: nil

  defp drop_no_spec(:no_spec), do: nil
  defp drop_no_spec(other), do: other

  # not supported
  defp expand_call(_env, nil, _, _, _), do: nil

  # map field access
  defp expand_call(env, {:map, fields, _}, field, arity, _) do
    # field access is a call with arity 0, other are not allowed
    if arity == 0 do
      expand(env, fields[field])
    end
  end

  # struct field access
  defp expand_call(env, {:struct, fields, _, _}, field, arity, _) do
    # field access is a call with arity 0, other are not allowed
    if arity == 0 do
      expand(env, fields[field])
    end
  end

  # function call
  defp expand_call(env, {:atom, mod}, fun, arity, include_private)
       when mod not in [nil, true, false] and fun not in [nil, true, false] do
    case expand_call_from_metadata(env, mod, fun, arity, include_private) do
      result when not is_nil(result) -> result
      nil -> expand_call_from_introspection(env, mod, fun, arity, include_private)
    end
  end

  # not a module
  defp expand_call(_env, {:atom, _mod}, _fun, _arity, _include_private), do: nil

  defp call_arity_match?(fun_arity, fun_defaults, call_arity) do
    fun_arity - fun_defaults <= call_arity and call_arity <= fun_arity
  end

  defp expand_call_from_introspection(env, mod, fun, arity, include_private) do
    arity =
      case ElixirSense.Core.Normalized.Code.get_docs(mod, :docs) do
        nil ->
          # no docs - use call arity if fun exported
          if function_exported?(mod, fun, arity) do
            arity
          end

        list ->
          # correct arity for calls with default params
          list
          |> Enum.find_value(arity, fn {{f, a}, _, _, _, _, map} ->
            if f == fun and call_arity_match?(a, Map.get(map, :defaults, 0), arity) do
              a
            end
          end)
      end

    # TODO maybe callback from behaviour
    if arity do
      type = TypeInfo.get_spec(mod, fun, arity)
      return_type = get_return_from_spec(env, type, mod, include_private)
      expand(env, return_type) || :no_spec
    end
  end

  defp expand_call_from_metadata(
         %Binding{specs: specs, mods_and_funs: mods_and_funs} = env,
         mod,
         fun,
         arity,
         include_private
       ) do
    arity =
      case mods_and_funs[{mod, fun, nil}] do
        %State.ModFunInfo{type: fun_type} = info
        when (include_private and fun_type != :def) or
               fun_type in [:def, :defmacro, :defguard, :defdelegate] ->
          # correct arity for calls with default params
          State.ModFunInfo.get_arities(info)
          |> Enum.find_value(arity, fn {a, defaults} ->
            if call_arity_match?(a, defaults, arity) do
              a
            end
          end)

        _ ->
          nil
      end

    case {arity, specs[{mod, fun, arity}]} do
      {nil, _} ->
        # fun not found
        nil

      {_, %State.SpecInfo{} = spec} ->
        get_return_from_metadata(env, mod, spec, include_private) || :no_spec

      _ ->
        :no_spec
    end
  end

  defp extract_type({:"::", _, [_, type]}), do: type

  defp extract_type({:when, _, [{:"::", _, [_, type]}, type_params]}) do
    # substitute type params
    Macro.prewalk(type, fn
      {atom, _, nil} = var ->
        Keyword.get(type_params, atom, var)

      other ->
        other
    end)
  end

  defp get_return_from_metadata(env, mod, %State.SpecInfo{specs: [func_spec]}, include_private) do
    case Code.string_to_quoted(func_spec) do
      {:ok, {:@, _, [{_kind, _, [ast]}]}} ->
        type = extract_type(ast)
        parsed_type = parse_type(env, type, mod, include_private)
        expand(env, parsed_type)

      _ ->
        nil
    end
  end

  # intersection specs
  defp get_return_from_metadata(
         env,
         mod,
         %State.SpecInfo{specs: [_ | _] = variants} = spec,
         include_private
       ) do
    # check if all variants return the same type
    # if so return it, otherwise nil
    Enum.reduce_while(variants, :none, fn variant, acc ->
      case {acc,
            get_return_from_metadata(
              env,
              mod,
              %State.SpecInfo{spec | specs: [variant]},
              include_private
            )} do
        {:none, expanded} -> {:cont, expanded}
        {last, last} -> {:cont, last}
        {_, _} -> {:halt, nil}
      end
    end)
  end

  defp get_return_from_spec(_env, nil, _, _include_private), do: nil

  defp get_return_from_spec(env, {{fun, _arity}, [ast]}, mod, include_private) do
    type = Typespec.spec_to_quoted(fun, ast) |> extract_type
    parse_type(env, type, mod, include_private)
  end

  # intersection specs
  defp get_return_from_spec(env, {{fun, arity}, [_ast | _] = variants}, mod, include_private) do
    # check if all variants return the same type
    # if so return it, otherwise nil
    Enum.reduce_while(variants, :none, fn variant, acc ->
      case {acc, get_return_from_spec(env, {{fun, arity}, [variant]}, mod, include_private)} do
        {:none, expanded} -> {:cont, expanded}
        {last, last} -> {:cont, last}
        {_, _} -> {:halt, nil}
      end
    end)
  end

  # union type
  defp parse_type(env, {:|, _, variants}, mod, include_private) do
    # check if all variants expand to the same type
    # if so return it, otherwise nil
    Enum.reduce_while(variants, :none, fn variant, acc ->
      case {acc, parse_type(env, variant, mod, include_private)} do
        {:none, expanded} -> {:cont, expanded}
        {last, last} -> {:cont, last}
        {_, _} -> {:halt, nil}
      end
    end)
  end

  # struct
  defp parse_type(
         env,
         {:%, _,
          [
            struct_mod,
            {:%{}, _, fields}
          ]},
         mod,
         include_private
       ) do
    fields =
      for {field, type} <- fields,
          is_atom(field),
          do: {field, parse_type(env, type, mod, include_private)}

    module =
      case struct_mod do
        m when is_atom(m) ->
          m

        {:__aliases__, _, list} ->
          Module.concat(list)
      end

    {:struct, fields, {:atom, module}, nil}
  end

  # map
  defp parse_type(env, {:%{}, _, fields}, mod, include_private) do
    fields =
      for {field, type} <- fields,
          field = drop_optional(field),
          is_atom(field),
          do: {field, parse_type(env, type, mod, include_private)}

    {:map, fields, nil}
  end

  # remote user type
  defp parse_type(env, {{:., _, [mod, atom]}, _, args}, _mod, _include_private)
       when is_atom(mod) and is_atom(atom) do
    # do not propagate include_private when expanding remote types
    expand_type(env, mod, atom, args, false)
  end

  # local user type
  defp parse_type(env, {atom, _, args}, mod, include_private) when is_atom(atom) do
    # propagate include_private when expanding local types
    expand_type(env, mod, atom, args, include_private)
  end

  # atom
  defp parse_type(_env, atom, _, _include_private) when is_atom(atom), do: {:atom, atom}

  # other
  defp parse_type(_env, _, _, _include_private), do: nil

  defp expand_type(env, mod, type_name, args, include_private) do
    arity = length(args || [])

    case expand_type_from_metadata(env, mod, type_name, arity, include_private) do
      nil -> expand_type_from_introspection(env, mod, type_name, arity, include_private)
      res -> res
    end
    |> drop_no_spec
  end

  defguardp type_is_public(kind, include_private) when kind == :type or include_private

  defp expand_type_from_metadata(
         %Binding{types: types} = env,
         mod,
         type_name,
         arity,
         include_private
       ) do
    case types[{mod, type_name, arity}] do
      %State.TypeInfo{specs: [type_spec], kind: kind}
      when type_is_public(kind, include_private) ->
        case Code.string_to_quoted(type_spec) do
          {:ok, {:@, _, [{_kind, _, [ast]}]}} ->
            type = extract_type(ast)
            parse_type(env, type, mod, include_private) || :no_spec

          _ ->
            :no_spec
        end

      nil ->
        nil

      _ ->
        :no_spec
    end
  end

  defp expand_type_from_introspection(env, mod, type_name, arity, include_private) do
    case TypeInfo.get_type_spec(mod, type_name, arity) do
      {kind, spec} when type_is_public(kind, include_private) ->
        {:"::", _, [_, type]} = Typespec.type_to_quoted(spec)

        parse_type(env, type, mod, include_private)

      _ ->
        nil
    end
  end

  defp drop_optional({:optional, _, [key]}), do: key
  defp drop_optional(other), do: other
end
