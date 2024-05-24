defmodule ElixirSense.Core.TypeInference do
  # TODO remove state arg
  # struct or struct update
  alias ElixirSense.Core.State.VarInfo
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

  def find_vars(state, ast, match_context) do
    {_ast, {vars, _match_context}} =
      Macro.prewalk(ast, {[], match_context}, &match_var(state, &1, &2))

    vars |> Map.new
  end

  defp match_var(
         state,
         {:in, _meta,
          [
            left,
            right
          ]},
         {vars, _match_context}
       ) do
    exception_type =
      case right do
        [elem] ->
          get_binding_type(state, elem)

        list when is_list(list) ->
          types = for elem <- list, do: get_binding_type(state, elem)
          if Enum.all?(types, &match?({:atom, _}, &1)), do: {:atom, Exception}

        elem ->
          get_binding_type(state, elem)
      end

    match_context =
      case exception_type do
        {:atom, atom} -> {:struct, [], {:atom, atom}, nil}
        _ -> nil
      end

    match_var(state, left, {vars, match_context})
  end

  defp match_var(
         state,
         {:=, _meta,
          [
            left,
            right
          ]},
         {vars, _match_context}
       ) do
    {_ast, {vars, _match_context}} =
      match_var(state, left, {vars, get_binding_type(state, right)})

    {_ast, {vars, _match_context}} =
      match_var(state, right, {vars, get_binding_type(state, left)})

    {[], {vars, nil}}
  end

  defp match_var(
         state,
         {:^, _meta, [{var, meta, context}]},
         {vars, match_context}
       )
       when is_atom(var) and is_atom(context) and
       var not in [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__, :_] do
    version = meta |> Keyword.fetch!(:version)
    var_info = state.vars_info |> hd |> Map.fetch!({var, version})

    var_info = %VarInfo{var_info | type: match_context}

    {nil, {[{{var, version}, var_info} | vars], nil}}
  end

  defp match_var(
         state,
         {var, meta, context},
         {vars, match_context}
       )
       when is_atom(var) and is_atom(context) and
              var not in [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__, :_] do
    version = meta |> Keyword.fetch!(:version)
    var_info = state.vars_info |> hd |> Map.fetch!({var, version})

    var_info = %VarInfo{var_info | type: match_context}

    {nil, {[{{var, version}, var_info} | vars], nil}}
  end

  # drop right side of guard expression as guards cannot define vars
  defp match_var(state, {:when, _, [left, _right]}, {vars, match_context}) do
    # TODO should we infer from guard here?
    match_var(state, left, {vars, match_context})
  end

  defp match_var(state, {:%, _, [type_ast, {:%{}, _, ast}]}, {vars, match_context})
       when not is_nil(match_context) do
    # TODO pass mach_context here as map __struct__ key access
    {_ast, {type_vars, _match_context}} = match_var(state, type_ast, {[], nil})

    destructured_vars =
      ast
      |> Enum.flat_map(fn {key, value_ast} ->
        key_type = get_binding_type(state, key)

        {_ast, {new_vars, _match_context}} =
          match_var(state, value_ast, {[], {:map_key, match_context, key_type}})

        new_vars
      end)

    {ast, {vars ++ destructured_vars ++ type_vars, nil}}
  end

  defp match_var(state, {:%{}, _, ast}, {vars, match_context}) when not is_nil(match_context) do
    destructured_vars =
      ast
      |> Enum.flat_map(fn {key, value_ast} ->
        key_type = get_binding_type(state, key)

        {_ast, {new_vars, _match_context}} =
          match_var(state, value_ast, {[], {:map_key, match_context, key_type}})

        new_vars
      end)

    {ast, {vars ++ destructured_vars, nil}}
  end

  # regular tuples use {:{}, [], [field_1, field_2]} ast
  # two element use `{field_1, field_2}` ast (probably as an optimization)
  # detect and convert to regular
  defp match_var(state, ast, {vars, match_context})
       when is_tuple(ast) and tuple_size(ast) == 2 do
    match_var(state, {:{}, [], ast |> Tuple.to_list()}, {vars, match_context})
  end

  defp match_var(state, {:{}, _, ast}, {vars, match_context}) when not is_nil(match_context) do
    indexed = ast |> Enum.with_index()
    total = length(ast)

    destructured_vars =
      indexed
      |> Enum.flat_map(fn {nth_elem_ast, n} ->
        bond =
          {:tuple, total,
           indexed |> Enum.map(&if(n != elem(&1, 1), do: get_binding_type(state, elem(&1, 0))))}

        match_context =
          if match_context != bond do
            {:intersection, [match_context, bond]}
          else
            match_context
          end

        {_ast, {new_vars, _match_context}} =
          match_var(state, nth_elem_ast, {[], {:tuple_nth, match_context, n}})

        new_vars
      end)

    {ast, {vars ++ destructured_vars, nil}}
  end

  # two element tuples on the left of `->` are encoded as list `[field1, field2]`
  # detect and convert to regular
  defp match_var(state, {:->, meta, [[left], right]}, {vars, match_context}) do
    match_var(state, {:->, meta, [left, right]}, {vars, match_context})
  end

  defp match_var(state, list, {vars, match_context})
       when not is_nil(match_context) and is_list(list) do
    match_var_list = fn head, tail ->
      {_ast, {new_vars_head, _match_context}} =
        match_var(state, head, {[], {:list_head, match_context}})

      {_ast, {new_vars_tail, _match_context}} =
        match_var(state, tail, {[], {:list_tail, match_context}})

      {list, {vars ++ new_vars_head ++ new_vars_tail, nil}}
    end

    case list do
      [] ->
        {list, {vars, nil}}

      [{:|, _, [head, tail]}] ->
        match_var_list.(head, tail)

      [head | tail] ->
        match_var_list.(head, tail)
    end
  end

  defp match_var(_state, ast, {vars, match_context}) do
    {ast, {vars, match_context}}
  end
end
