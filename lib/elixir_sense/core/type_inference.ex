defmodule ElixirSense.Core.TypeInference do
  # TODO struct or struct update

  def get_binding_type(
        {:%, _meta,
         [
           struct_ast,
           {:%{}, _, _} = ast
         ]}
      ) do
    {fields, updated_struct} =
      case get_binding_type(ast) do
        {:map, fields, updated_map} -> {fields, updated_map}
        {:struct, fields, _, updated_struct} -> {fields, updated_struct}
        _ -> {[], nil}
      end

    # expand struct type - only compile type atoms or attributes are supported
    type =
      case get_binding_type(struct_ast) do
        {:atom, atom} -> {:atom, atom}
        {:attribute, attribute} -> {:attribute, attribute}
        _ -> nil
      end

    {:struct, fields, type, updated_struct}
  end

  # pipe
  # TODO no pipes in expanded code
  # def get_binding_type({:|>, _, [params_1, {call, meta, params_rest}]}) do
  #   params = [params_1 | params_rest || []]
  #   get_binding_type({call, meta, params})
  # end

  # remote call
  def get_binding_type({{:., _, [target, fun]}, _, args})
      when is_atom(fun) and is_list(args) do
    target = get_binding_type(target)
    {:call, target, fun, Enum.map(args, &get_binding_type(&1))}
  end

  # variable or local no parens call
  # TODO version?
  def get_binding_type({var, _, context}) when is_atom(var) and is_atom(context) do
    {:variable, var}
  end

  # attribute
  # expanded attribute reference has nil arg
  def get_binding_type({:@, _, [{attribute, _, nil}]})
      when is_atom(attribute) do
    {:attribute, attribute}
  end

  # erlang module or atom
  def get_binding_type(atom) when is_atom(atom) do
    {:atom, atom}
  end

  # map update
  def get_binding_type(
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
    {:map, get_fields_binding_type(fields), get_binding_type(updated_map)}
  end

  # map
  def get_binding_type({:%{}, _meta, fields}) when is_list(fields) do
    field_type = get_fields_binding_type(fields)
    case field_type |> Keyword.fetch(:__struct__) do
      {:ok, type} -> {:struct, [], type, nil}
      _ -> {:map, field_type, nil}
    end
  end

  # match
  def get_binding_type({:=, _, [_, ast]}) do
    get_binding_type(ast)
  end

  # stepped range struct
  def get_binding_type({:"..//", _, [_, _, _]}) do
    {:struct, [], {:atom, Range}, nil}
  end

  # range struct
  def get_binding_type({:.., _, [_, _]}) do
    {:struct, [], {:atom, Range}, nil}
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
  def get_binding_type({sigil, _, _}) when is_map_key(@builtin_sigils, sigil) do
    # TODO support custom sigils
    {:struct, [], {:atom, @builtin_sigils |> Map.fetch!(sigil)}, nil}
  end

  # tuple
  # regular tuples use {:{}, [], [field_1, field_2]} ast
  # two element use {field_1, field_2} ast (probably as an optimization)
  # detect and convert to regular
  def get_binding_type(ast) when is_tuple(ast) and tuple_size(ast) == 2 do
    get_binding_type({:{}, [], Tuple.to_list(ast)})
  end

  def get_binding_type({:{}, _, list}) do
    {:tuple, length(list), list |> Enum.map(&get_binding_type(&1))}
  end

  def get_binding_type(list) when is_list(list) do
    type =
      case list do
        [] -> :empty
        [{:|, _, [head, _tail]}] -> get_binding_type(head)
        [head | _] -> get_binding_type(head)
      end

    {:list, type}
  end

  def get_binding_type(list) when is_list(list) do
    {:list, list |> Enum.map(&get_binding_type(&1))}
  end

  # pinned variable
  def get_binding_type({:^, _, [pinned]}), do: get_binding_type(pinned)

  # local call
  def get_binding_type({var, _, args}) when is_atom(var) and is_list(args) do
    {:local_call, var, Enum.map(args, &get_binding_type(&1))}
  end

  # integer
  def get_binding_type(integer) when is_integer(integer) do
    {:integer, integer}
  end

  # other
  def get_binding_type(_), do: nil

  defp get_fields_binding_type(fields) do
    for {field, value} <- fields,
        is_atom(field) do
      {field, get_binding_type(value)}
    end
  end

  def find_vars(ast, match_context) do
    {_ast, {vars, _match_context}} =
      Macro.prewalk(ast, {[], match_context}, &match_var(&1, &2))

    vars
  end

  # TODO not needed
  # defp match_var(
  #        {:in, _meta,
  #         [
  #           left,
  #           right
  #         ]},
  #        {vars, _match_context}
  #      ) do
  #   exception_type =
  #     case right do
  #       [elem] ->
  #         get_binding_type(elem)

  #       list when is_list(list) ->
  #         types = for elem <- list, do: get_binding_type(elem)
  #         if Enum.all?(types, &match?({:atom, _}, &1)), do: {:atom, Exception}

  #       elem ->
  #         get_binding_type(elem)
  #     end

  #   match_context =
  #     case exception_type do
  #       {:atom, atom} -> {:struct, [], {:atom, atom}, nil}
  #       _ -> nil
  #     end

  #   match_var(left, {vars, match_context})
  # end

  defp match_var(
         {:=, _meta,
          [
            left,
            right
          ]},
         {vars, _match_context}
       ) do
    {_ast, {vars, _match_context}} =
      match_var(left, {vars, get_binding_type(right)})

    {_ast, {vars, _match_context}} =
      match_var(right, {vars, get_binding_type(left)})

    {[], {vars, nil}}
  end

  defp match_var(
         {:^, _meta, [{var, meta, context}]} = ast,
         {vars, match_context}
       )
       when is_atom(var) and is_atom(context) and
       var not in [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__, :_] do
      case Keyword.fetch(meta, :version) do
        {:ok, version} ->
          {nil, {[{{var, version}, match_context} | vars], nil}}
        _ ->
          {ast, {vars, match_context}}
        end
  end

  defp match_var(
         {var, meta, context} = ast,
         {vars, match_context}
       )
       when is_atom(var) and is_atom(context) and
              var not in [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__, :_] do
    case Keyword.fetch(meta, :version) do
      {:ok, version} ->
        {nil, {[{{var, version}, match_context} | vars], nil}}
      _ ->
        {ast, {vars, match_context}}
     end
  end

  # drop right side of guard expression as guards cannot define vars
  # TODO not needed
  # defp match_var({:when, _, [left, _right]}, {vars, match_context}) do
  #   # TODO should we infer from guard here?
  #   match_var(left, {vars, match_context})
  # end

  defp match_var({:%, _, [type_ast, {:%{}, _, ast}]}, {vars, match_context})
       when not is_nil(match_context) do
    # TODO pass mach_context here as map __struct__ key access
    {_ast, {type_vars, _match_context}} = match_var(type_ast, {[], nil})

    destructured_vars =
      ast
      |> Enum.flat_map(fn {key, value_ast} ->
        key_type = get_binding_type(key)

        {_ast, {new_vars, _match_context}} =
          match_var(value_ast, {[], {:map_key, match_context, key_type}})

        new_vars
      end)

    {ast, {vars ++ destructured_vars ++ type_vars, nil}}
  end

  defp match_var({:%{}, _, ast}, {vars, match_context}) when not is_nil(match_context) do
    destructured_vars =
      ast
      |> Enum.flat_map(fn
        {:|, _, [_left, _right]} ->
          # map update is forbidden in match, we're in invalid code
          []
        {key, value_ast} ->
          key_type = get_binding_type(key)

          {_ast, {new_vars, _match_context}} =
            match_var(value_ast, {[], {:map_key, match_context, key_type}})

          new_vars
      end)

    {ast, {vars ++ destructured_vars, nil}}
  end

  # regular tuples use {:{}, [], [field_1, field_2]} ast
  # two element use `{field_1, field_2}` ast (probably as an optimization)
  # detect and convert to regular
  defp match_var(ast, {vars, match_context})
       when is_tuple(ast) and tuple_size(ast) == 2 do
    match_var({:{}, [], ast |> Tuple.to_list()}, {vars, match_context})
  end

  defp match_var({:{}, _, ast}, {vars, match_context}) when not is_nil(match_context) do
    indexed = ast |> Enum.with_index()
    total = length(ast)

    destructured_vars =
      indexed
      |> Enum.flat_map(fn {nth_elem_ast, n} ->
        bond =
          {:tuple, total,
           indexed |> Enum.map(&if(n != elem(&1, 1), do: get_binding_type(elem(&1, 0))))}

        match_context =
          if match_context != bond do
            {:intersection, [match_context, bond]}
          else
            match_context
          end

        {_ast, {new_vars, _match_context}} =
          match_var(nth_elem_ast, {[], {:tuple_nth, match_context, n}})

        new_vars
      end)

    {ast, {vars ++ destructured_vars, nil}}
  end

  # two element tuples on the left of `->` are encoded as list `[field1, field2]`
  # detect and convert to regular
  defp match_var({:->, meta, [[left], right]}, {vars, match_context}) do
    match_var({:->, meta, [left, right]}, {vars, match_context})
  end

  defp match_var(list, {vars, match_context})
       when not is_nil(match_context) and is_list(list) do
    match_var_list = fn head, tail ->
      {_ast, {new_vars_head, _match_context}} =
        match_var(head, {[], {:list_head, match_context}})

      {_ast, {new_vars_tail, _match_context}} =
        match_var(tail, {[], {:list_tail, match_context}})

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

  defp match_var(ast, {vars, match_context}) do
    {ast, {vars, match_context}}
  end

  def find_refinable({:=, _, [left, right]}, acc, e), do: find_refinable(right, [left | acc], e)
  def find_refinable(other, acc, e) when e.context == :match, do: [other | acc]
  def find_refinable(_, acc, _), do: acc
end
