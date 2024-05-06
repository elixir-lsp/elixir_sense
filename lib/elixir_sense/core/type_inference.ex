defmodule ElixirSense.Core.TypeInference do
  # TODO remove state arg
  # struct or struct update
  def get_binding_type(
        state,
        {:%, _meta,
         [
           struct_ast,
           {:%{}, _, _} = ast
         ]}
      ) do
    {fields, updated_struct} =
      case get_binding_type(state, ast) do
        {:map, fields, updated_map} -> {fields, updated_map}
        {:struct, fields, _, updated_struct} -> {fields, updated_struct}
        _ -> {[], nil}
      end

    # expand struct type - only compile type atoms or attributes are supported
    type =
      case get_binding_type(state, struct_ast) do
        {:atom, atom} -> {:atom, atom}
        {:attribute, attribute} -> {:attribute, attribute}
        _ -> nil
      end

    {:struct, fields, type, updated_struct}
  end

  # pipe
  def get_binding_type(state, {:|>, _, [params_1, {call, meta, params_rest}]}) do
    params = [params_1 | params_rest || []]
    get_binding_type(state, {call, meta, params})
  end

  # remote call
  def get_binding_type(state, {{:., _, [target, fun]}, _, args})
      when is_atom(fun) and is_list(args) do
    target = get_binding_type(state, target)
    {:call, target, fun, Enum.map(args, &get_binding_type(state, &1))}
  end

  # # current module
  # def get_binding_type(state, {:__MODULE__, _, nil} = module) do
  #   {module, _state, _env} = expand(module, state)
  #   {:atom, module}
  # end

  # # elixir module
  # def get_binding_type(state, {:__aliases__, _, list} = module) when is_list(list) do
  #   try do
  #     {module, _state, _env} = expand(module, state)
  #     {:atom, module}
  #   rescue
  #     _ -> nil
  #   end
  # end

  # variable or local no parens call
  def get_binding_type(_state, {var, _, nil}) when is_atom(var) do
    {:variable, var}
  end

  # attribute
  def get_binding_type(_state, {:@, _, [{attribute, _, nil}]})
      when is_atom(attribute) do
    {:attribute, attribute}
  end

  # erlang module or atom
  def get_binding_type(_state, atom) when is_atom(atom) do
    {:atom, atom}
  end

  # map update
  def get_binding_type(
        state,
        {:%{}, _meta,
         [
           {:|, _meta1,
            [
              updated_map,
              fields
            ]}
         ]}
      )
      when is_list(fields) do
    {:map, get_fields_binding_type(state, fields), get_binding_type(state, updated_map)}
  end

  # map
  def get_binding_type(state, {:%{}, _meta, fields}) when is_list(fields) do
    {:map, get_fields_binding_type(state, fields), nil}
  end

  # match
  def get_binding_type(state, {:=, _, [_, ast]}) do
    get_binding_type(state, ast)
  end

  # stepped range struct
  def get_binding_type(_state, {:"..//", _, [_, _, _]}) do
    {:struct, [], {:atom, Range}}
  end

  # range struct
  def get_binding_type(_state, {:.., _, [_, _]}) do
    {:struct, [], {:atom, Range}}
  end

  @builtin_sigils %{
    sigil_D: Date,
    sigil_T: Time,
    sigil_U: DateTime,
    sigil_N: NaiveDateTime,
    sigil_R: Regex,
    sigil_r: Regex
  }

  # builtin sigil struct
  def get_binding_type(_state, {sigil, _, _}) when is_map_key(@builtin_sigils, sigil) do
    # TODO support custom sigils
    {:struct, [], {:atom, @builtin_sigils |> Map.fetch!(sigil)}}
  end

  # tuple
  # regular tuples use {:{}, [], [field_1, field_2]} ast
  # two element use {field_1, field_2} ast (probably as an optimization)
  # detect and convert to regular
  def get_binding_type(state, ast) when is_tuple(ast) and tuple_size(ast) == 2 do
    get_binding_type(state, {:{}, [], Tuple.to_list(ast)})
  end

  def get_binding_type(state, {:{}, _, list}) do
    {:tuple, length(list), list |> Enum.map(&get_binding_type(state, &1))}
  end

  def get_binding_type(state, list) when is_list(list) do
    type =
      case list do
        [] -> :empty
        [{:|, _, [head, _tail]}] -> get_binding_type(state, head)
        [head | _] -> get_binding_type(state, head)
      end

    {:list, type}
  end

  def get_binding_type(state, list) when is_list(list) do
    {:list, list |> Enum.map(&get_binding_type(state, &1))}
  end

  # pinned variable
  def get_binding_type(state, {:^, _, [pinned]}), do: get_binding_type(state, pinned)

  # local call
  def get_binding_type(state, {var, _, args}) when is_atom(var) and is_list(args) do
    {:local_call, var, Enum.map(args, &get_binding_type(state, &1))}
  end

  # integer
  def get_binding_type(_state, integer) when is_integer(integer) do
    {:integer, integer}
  end

  # other
  def get_binding_type(_state, _), do: nil

  defp get_fields_binding_type(state, fields) do
    for {field, value} <- fields,
        is_atom(field) do
      {field, get_binding_type(state, value)}
    end
  end
end
