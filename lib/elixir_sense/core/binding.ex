defmodule ElixirSense.Core.Binding do
  @moduledoc false

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Normalized.Typespec
  alias ElixirSense.Core.State
  alias ElixirSense.Core.Struct
  alias ElixirSense.Core.TypeInfo

  # TODO refactor to use env
  defstruct structs: %{},
            vars: [],
            attributes: [],
            module: nil,
            function: nil,
            functions: [],
            macros: [],
            requires: [],
            specs: %{},
            types: %{},
            mods_funs_to_positions: %{},
            cursor_position: {1, 1}

  def from_env(%State.Env{} = env, %ElixirSense.Core.Metadata{} = metadata, cursor_position) do
    %Binding{
      vars: env.vars,
      attributes: env.attributes,
      structs: metadata.structs,
      functions: env.functions,
      macros: env.macros,
      requires: env.requires,
      specs: metadata.specs,
      module: env.module,
      function: env.function,
      types: metadata.types,
      mods_funs_to_positions: metadata.mods_funs_to_positions,
      cursor_position: cursor_position
    }
  end

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

  def expand(%Binding{} = env, expanded, stack \\ []) do
    res =
      unless expanded in stack do
        do_expand(env, expanded, [expanded | stack])
      end

    case res do
      {:struct, _, _, _} ->
        do_expand(env, res, [res | stack])

      {:map, _, _} ->
        do_expand(env, res, [res | stack])

      _ ->
        res
    end
  end

  def do_expand(%Binding{} = env, {:intersection, variants}, stack) do
    combined =
      variants
      |> Enum.reduce(nil, fn variant, acc ->
        combine_intersection(acc, expand(env, variant, stack))
      end)

    expand(env, combined, stack)
  end

  def do_expand(%Binding{vars: variables} = env, {:variable, variable, version}, stack) do
    sorted_variables = Enum.sort_by(variables, &{&1.name, -&1.version})

    type =
      case Enum.find(sorted_variables, fn %State.VarInfo{} = var ->
             var.name == variable and (var.version == version or version == :any)
           end) do
        nil ->
          # no variable found - treat as a local call
          # this can happen if no parens call is missclassed as variable e.g. by
          # Code.Fragment APIs
          {:local_call, variable, env.cursor_position, []}

        %State.VarInfo{type: type} ->
          type
      end

    expand(env, type, stack)
  end

  def do_expand(%Binding{attributes: attributes} = env, {:attribute, attribute}, stack) do
    type =
      case Enum.find(attributes, fn %{name: name} -> name == attribute end) do
        nil -> :none
        %State.AttributeInfo{type: type} -> type
      end

    expand(env, type, stack)
  end

  def do_expand(
        %Binding{structs: structs} = env,
        {:struct, fields, module, updated_struct},
        stack
      ) do
    # struct type must be a compile time atom or attribute
    module =
      case module do
        {:atom, atom} -> {:atom, atom}
        {:attribute, attr} -> {:attribute, attr}
        nil -> nil
        _ -> :none
      end

    module =
      case expand(env, module, stack) do
        {:atom, atom} -> atom
        nil -> nil
        _ -> :none
      end

    if module == nil or (module != :none and Struct.is_struct(module, structs)) do
      expanded = expand(env, updated_struct, stack)

      {fields, module} =
        get_struct_fields(env, get_fields_from(expanded) |> Keyword.merge(fields), module)

      {:struct, fields, if(module != nil, do: {:atom, module}), nil}
    else
      :none
    end
  end

  def do_expand(env, {:map, fields, updated_map}, stack) do
    case expand(env, updated_map, stack) do
      {:map, expanded_fields, nil} ->
        {:map, expanded_fields |> Keyword.merge(fields), nil}

      {:struct, expanded_fields, type, nil} ->
        {:struct, expanded_fields |> Keyword.merge(fields), type, nil}

      nil ->
        {:map, fields, nil}

      _ ->
        :none
    end
  end

  def do_expand(env, {:map_key, map_candidate, key_candidate}, stack) do
    case expand(env, key_candidate, stack) do
      {:atom, key} ->
        expanded_fields = expand_map_fields(env, map_candidate, stack)

        if :none in expanded_fields do
          :none
        else
          expanded_fields |> Keyword.get(key)
        end

      _ ->
        nil
    end
  end

  def do_expand(env, {:tuple_nth, tuple_candidate, n}, stack) do
    case expand(env, tuple_candidate, stack) do
      {:tuple, size, fields} when size >= n ->
        fields |> Enum.at(n)

      nil ->
        nil

      _ ->
        :none
    end
  end

  def do_expand(env, {:for_expression, list_candidate}, stack) do
    case expand(env, list_candidate, stack) do
      {:list, type} when type not in [:empty, :none] ->
        type

      {:map, fields, nil} ->
        case fields do
          [{_key, value} | _] ->
            {:tuple, 2, [nil, value]}

          _ ->
            nil
        end

      nil ->
        nil

      _ ->
        :none
    end
  end

  def do_expand(env, {:list_head, list_candidate}, stack) do
    case expand(env, list_candidate, stack) do
      {:list, type} when type not in [:empty, :none] ->
        type

      nil ->
        nil

      _ ->
        :none
    end
  end

  def do_expand(env, {:list_tail, list_candidate}, stack) do
    case expand(env, list_candidate, stack) do
      {:list, type} when type not in [:empty, :none] ->
        {:list, type}

      nil ->
        nil

      _ ->
        :none
    end
  end

  # dependency injection
  def do_expand(env, {:call, {:atom, Application}, fun, args}, stack)
      when fun in ~w(compile_env!)a do
    # `Application.compile_env!/2` underneath works like `fetch_env!/2`
    do_expand(env, {:call, {:atom, Application}, :fetch_env!, args}, stack)
  end

  def do_expand(env, {:call, {:atom, Application}, fun, args}, stack)
      when fun in ~w(compile_env)a do
    # `Application.compile_env/3` underneath works like `get_env/3`
    do_expand(env, {:call, {:atom, Application}, :get_env, args}, stack)
  end

  # TODO maybe handle Application.fetch_env/2

  def do_expand(env, {:call, {:atom, Application}, fun, args}, stack)
      when fun in ~w(get_env fetch_env!)a do
    try do
      expanded_args =
        args
        |> Enum.map(&expand(env, &1, stack))
        |> Enum.map(&elem(&1, 1))

      mod = apply(Application, fun, expanded_args)

      case mod do
        :error ->
          :none

        mod when is_atom(mod) ->
          {:atom, mod}

        # TODO handle other types

        _ ->
          nil
      end
    rescue
      _ ->
        :none
    end
  end

  # remote call
  def do_expand(env, {:call, target, function, arguments}, stack) do
    if :none in arguments do
      :none
    else
      expanded_target = expand(env, target, stack)
      # do not include private funs on remote call
      expand_call(env, expanded_target, function, arguments, false, nil, stack)
      |> drop_no_spec
    end
  end

  # local call
  def do_expand(
        %Binding{functions: functions, macros: macros} = env,
        {:local_call, function, position, arguments},
        stack
      ) do
    if :none in arguments do
      :none
    else
      combined_imports =
        {functions, macros}
        |> Introspection.combine_imports()

      candidate_targets =
        if env.module && env.function do
          # locals are available only in defs
          [env.module]
        else
          []
        end ++ combined_imports ++ [Kernel.SpecialForms]

      # take first matching
      Enum.find_value(candidate_targets, fn
        {candidate, imported} ->
          if {function, length(arguments)} in imported do
            expand_call(env, {:atom, candidate}, function, arguments, false, position, stack)
          end

        candidate ->
          # include private from current module
          include_private = candidate == env.module

          expand_call(
            env,
            {:atom, candidate},
            function,
            arguments,
            include_private,
            position,
            stack
          )
      end)
      |> drop_no_spec
    end
  end

  def do_expand(env, {:tuple, size, fields}, stack),
    do: {:tuple, size, fields |> Enum.map(&expand(env, &1, stack))}

  def do_expand(_env, {:list, :empty}, _stack),
    do: {:list, :empty}

  def do_expand(env, {:list, type}, stack),
    do: {:list, expand(env, type, stack)}

  def do_expand(_env, {:atom, atom}, _stack), do: {:atom, atom}

  def do_expand(_env, {:integer, integer}, _stack), do: {:integer, integer}

  def do_expand(_env, {:union, all}, _stack) do
    # TODO implement union for maps and lists?
    all = Enum.filter(all, &(&1 != :none))

    cond do
      all == [] ->
        :none

      Enum.any?(all, &(&1 == nil)) ->
        nil

      match?([_], all) ->
        hd(all)

      true ->
        first = hd(all)

        if Enum.all?(tl(all), &(&1 == first)) do
          first
        else
          {:union, all}
        end
    end
  end

  def do_expand(_env, :none, _stack), do: :none

  def do_expand(_env, _other, _stack), do: nil

  defp drop_no_spec(:no_spec), do: nil
  defp drop_no_spec(other), do: other

  # not supported
  defp expand_call(_env, nil, _, _, _, _, _stack), do: nil
  defp expand_call(_env, :none, _, _, _, _, _stack), do: :none

  # map field access
  defp expand_call(env, {:map, fields, _}, field, arity, _, _, stack) do
    # field access is a call with arity 0, other are not allowed
    if arity == [] do
      expand(env, fields[field], stack)
    else
      :none
    end
  end

  # struct field access
  defp expand_call(env, {:struct, fields, _, _}, field, arity, _, _, stack) do
    # field access is a call with arity 0, other are not allowed
    if arity == [] do
      expand(env, fields[field], stack)
    else
      :none
    end
  end

  defp expand_call(
         env,
         {:atom, module},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:++, :--] and module in [Kernel, :erlang] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        {:list, type}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Kernel},
         :elem,
         [tuple_candidate, n_candidate],
         _include_private,
         _,
         stack
       ) do
    case expand(env, n_candidate, stack) do
      {:integer, n} ->
        expand(env, {:tuple_nth, tuple_candidate, n}, stack)

      nil ->
        nil

      _ ->
        :none
    end
  end

  # rewritten elem
  defp expand_call(
         env,
         {:atom, :erlang},
         :element,
         [n_candidate, tuple_candidate],
         _include_private,
         _,
         stack
       ) do
    case expand(env, n_candidate, stack) do
      {:integer, n} ->
        expand(env, {:tuple_nth, tuple_candidate, n - 1}, stack)

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Kernel},
         :put_elem,
         [tuple_candidate, n_candidate, value],
         _include_private,
         _,
         stack
       ) do
    with {:tuple, elems_count, elems} <- expand(env, tuple_candidate, stack),
         {:integer, n} when n >= 0 and n < elems_count <- expand(env, n_candidate, stack),
         expanded_value when expanded_value != :none <- expand(env, value, stack) do
      {:tuple, elems_count, elems |> List.replace_at(n, expanded_value)}
    else
      nil ->
        nil

      _ ->
        :none
    end
  end

  # rewritten put_elem
  defp expand_call(
         env,
         {:atom, :erlang},
         :setelement,
         [n_candidate, tuple_candidate, value],
         _include_private,
         _,
         stack
       ) do
    with {:tuple, elems_count, elems} <- expand(env, tuple_candidate, stack),
         {:integer, n} when n >= 1 and n <= elems_count <- expand(env, n_candidate, stack),
         expanded_value when expanded_value != :none <- expand(env, value, stack) do
      {:tuple, elems_count, elems |> List.replace_at(n - 1, expanded_value)}
    else
      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, module},
         fun,
         [tuple_candidate, value],
         _include_private,
         _,
         stack
       )
       when (module == Tuple and fun == :append) or (module == :erlang and fun == :append_element) do
    with {:tuple, elems_count, elems} <- expand(env, tuple_candidate, stack),
         expanded_value when expanded_value != :none <- expand(env, value, stack) do
      {:tuple, elems_count + 1, elems ++ [expanded_value]}
    else
      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Tuple},
         :delete_at,
         [tuple_candidate, n_candidate],
         _include_private,
         _,
         stack
       ) do
    with {:tuple, elems_count, elems} <- expand(env, tuple_candidate, stack),
         {:integer, n} when n >= 0 and n < elems_count <- expand(env, n_candidate, stack) do
      {:tuple, elems_count - 1, elems |> List.delete_at(n)}
    else
      nil ->
        nil

      _ ->
        :none
    end
  end

  # rewritten Tuple.delete_at
  defp expand_call(
         env,
         {:atom, :erlang},
         :delete_element,
         [n_candidate, tuple_candidate],
         _include_private,
         _,
         stack
       ) do
    with {:tuple, elems_count, elems} <- expand(env, tuple_candidate, stack),
         {:integer, n} when n > 0 and n <= elems_count <- expand(env, n_candidate, stack) do
      {:tuple, elems_count - 1, elems |> List.delete_at(n - 1)}
    else
      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Tuple},
         :insert_at,
         [tuple_candidate, n_candidate, value],
         _include_private,
         _,
         stack
       ) do
    with {:tuple, elems_count, elems} <- expand(env, tuple_candidate, stack),
         {:integer, n} when n >= 0 and n <= elems_count <- expand(env, n_candidate, stack),
         expanded_value when expanded_value != :none <- expand(env, value, stack) do
      {:tuple, elems_count + 1, elems |> List.insert_at(n, expanded_value)}
    else
      nil ->
        nil

      _ ->
        :none
    end
  end

  # rewritten Tuple.insert_at
  defp expand_call(
         env,
         {:atom, :erlang},
         :insert_element,
         [n_candidate, tuple_candidate, value],
         _include_private,
         _,
         stack
       ) do
    with {:tuple, elems_count, elems} <- expand(env, tuple_candidate, stack),
         {:integer, n} when n > 0 and n <= elems_count + 1 <- expand(env, n_candidate, stack),
         expanded_value when expanded_value != :none <- expand(env, value, stack) do
      {:tuple, elems_count + 1, elems |> List.insert_at(n - 1, expanded_value)}
    else
      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, module},
         fun,
         [tuple_candidate],
         _include_private,
         _,
         stack
       )
       when (module == Tuple and fun == :to_list) or (module == :erlang and fun == :tuple_to_list) do
    with {:tuple, _elems_count, elems} <- expand(env, tuple_candidate, stack) do
      case elems do
        [] -> {:list, :empty}
        [first | _] -> {:list, first}
      end
    else
      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, module},
         :tuple_size,
         [tuple_candidate],
         _include_private,
         _,
         stack
       )
       when module in [Kernel, :erlang] do
    with {:tuple, elems_count, _elems} <- expand(env, tuple_candidate, stack) do
      {:integer, elems_count}
    else
      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, module},
         fun,
         [value, n_candidate],
         _include_private,
         _,
         stack
       )
       when (module == Tuple and fun == :duplicate) or (module == :erlang and fun == :make_tuple) do
    {value, n_candidate} =
      if module == :erlang do
        {n_candidate, value}
      else
        {value, n_candidate}
      end

    # limit to 5
    with {:integer, n} when n >= 0 <- expand(env, n_candidate, stack),
         expanded_value when expanded_value != :none <- expand(env, value, stack) do
      {:tuple, n, expanded_value |> List.duplicate(n)}
    else
      nil ->
        nil

      {:integer, _n} ->
        nil

      _ ->
        :none
    end
  end

  # hd is inlined
  defp expand_call(
         env,
         {:atom, module},
         :hd,
         [list_candidate],
         _include_private,
         _,
         stack
       )
       when module in [Kernel, :erlang] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        type

      nil ->
        nil

      _ ->
        :none
    end
  end

  # tl is inlined
  defp expand_call(
         env,
         {:atom, module},
         :tl,
         [list_candidate],
         _include_private,
         _,
         stack
       )
       when module in [Kernel, :erlang] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        {:list, type}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Enum},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:at, :fetch, :fetch!, :find, :max, :max_by, :min, :min_by, :random] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        if name == :fetch do
          {:tuple, 2, [{:atom, :ok}, type]}
        else
          type
        end

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Enum},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:split, :split_while] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        {:tuple, 2, [{:list, type}, {:list, type}]}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Enum},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:min_max, :min_max_by] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        {:tuple, 2, [type, type]}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Enum},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:chunk_by, :chunk_every, :chunk_while] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        {:list, {:list, type}}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Enum},
         :concat,
         [list_candidate],
         _include_private,
         _,
         stack
       ) do
    case expand(env, list_candidate, stack) do
      {:list, {:list, type}} ->
        {:list, type}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, Enum},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [
              :concat,
              :dedup,
              :dedup_while,
              :drop,
              :drop_every,
              :drop_while,
              :filter,
              :intrperse,
              :reject,
              :reverse,
              :reverse_slice,
              :shuffle,
              :slice,
              :sort,
              :sort_by,
              :take,
              :take_every,
              :take_random,
              :take_while,
              :to_list,
              :uniq,
              :uniq_by
            ] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        {:list, type}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, List},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:delete, :delete_at, :insert_at, :replace_at, :update_at] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        {:list, type}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, List},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:flatten] do
    case expand(env, list_candidate, stack) do
      {:list, {:list, type}} ->
        {:list, type}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, List},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:wrap] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        {:list, type}

      {:atom, nil} ->
        {:list, :empty}

      nil ->
        nil

      :none ->
        :none

      type ->
        {:list, type}
    end
  end

  defp expand_call(
         env,
         {:atom, List},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:pop_at] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        {:tuple, 2, [type, {:list, type}]}

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, List},
         name,
         [list_candidate | _],
         _include_private,
         _,
         stack
       )
       when name in [:first, :last] do
    case expand(env, list_candidate, stack) do
      {:list, type} ->
        type

      nil ->
        nil

      _ ->
        :none
    end
  end

  defp expand_call(
         env,
         {:atom, List},
         name,
         [element | _],
         _include_private,
         _,
         stack
       )
       when name in [:duplicate] do
    case expand(env, element, stack) do
      nil ->
        nil

      :none ->
        :none

      type ->
        {:list, type}
    end
  end

  defp expand_call(env, {:atom, module}, fun, [map, key], _include_private, _, stack)
       when (module == Map and fun in [:fetch, :fetch!, :get]) or
              (module == :maps and fun in [:find, :get]) do
    {map, key} =
      if module == :maps do
        # rewritten versions have different arg order
        {key, map}
      else
        {map, key}
      end

    fields = expand_map_fields(env, map, stack)

    if :none in fields do
      :none
    else
      case expand(env, key, stack) do
        {:atom, atom} ->
          value = fields |> Keyword.get(atom)

          if fun in [:fetch, :find] and value != nil do
            {:tuple, 2, [{:atom, :ok}, value]}
          else
            value
          end

        nil ->
          nil

        _ ->
          :none
      end
    end
  end

  defp expand_call(env, {:atom, Map}, fun, [map, key, default], _include_private, _, stack)
       when fun in [:get, :get_lazy] do
    fields = expand_map_fields(env, map, stack)

    if :none in fields do
      :none
    else
      case expand(env, key, stack) do
        {:atom, atom} ->
          default = if fun == :get, do: expand(env, default, stack)
          fields |> Keyword.get(atom, default)

        nil ->
          nil

        _ ->
          :none
      end
    end
  end

  defp expand_call(env, {:atom, module}, fun, [map, key, value], _include_private, _, stack)
       when (fun == :put and module in [Map, :maps]) or (fun == :update and module == :maps) or
              (fun == :replace! and module == Map) do
    {map, key, value} =
      if module == :maps do
        # rewritten versions have different parameter order
        {value, map, key}
      else
        {map, key, value}
      end

    fields = expand_map_fields(env, map, stack)

    if :none in fields do
      :none
    else
      case expand(env, key, stack) do
        {:atom, atom} ->
          {:map, fields |> Keyword.put(atom, value), nil}

        :none ->
          :none

        _ ->
          {:map, fields, nil}
      end
    end
  end

  defp expand_call(env, {:atom, Map}, fun, [map, key, value], _include_private, _, stack)
       when fun in [:put_new, :put_new_lazy] do
    fields = expand_map_fields(env, map, stack)

    if :none in fields do
      :none
    else
      case expand(env, key, stack) do
        {:atom, atom} ->
          value = if fun == :put_new, do: value
          {:map, fields |> Keyword.put_new(atom, value), nil}

        :none ->
          :none

        _ ->
          {:map, fields, nil}
      end
    end
  end

  defp expand_call(env, {:atom, module}, fun, [map, key], _include_private, _, stack)
       when (module == Map and fun == :delete) or (module == :maps and fun == :remove) do
    {map, key} =
      if module == :maps do
        # rewritten versions have different arg order
        {key, map}
      else
        {map, key}
      end

    fields = expand_map_fields(env, map, stack)

    if :none in fields do
      :none
    else
      case expand(env, key, stack) do
        {:atom, atom} ->
          {:map, fields |> Keyword.delete(atom), nil}

        :none ->
          :none

        _ ->
          {:map, fields, nil}
      end
    end
  end

  # Map.merge/2 is inlined
  defp expand_call(env, {:atom, module}, :merge, [map, other_map], _include_private, _, stack)
       when module in [Map, :maps] do
    fields = expand_map_fields(env, map, stack)

    other_fields =
      case expand(env, other_map, stack) do
        {:map, fields, nil} -> fields
        nil -> []
        _ -> [:none]
      end

    if :none in (fields ++ other_fields) do
      :none
    else
      {:map, Keyword.merge(fields, other_fields), nil}
    end
  end

  defp expand_call(env, {:atom, Map}, :merge, [map, other_map, _fun], _include_private, _, stack) do
    fields = expand_map_fields(env, map, stack)

    other_fields = expand_map_fields(env, other_map, stack)

    if :none in (fields ++ other_fields) do
      :none
    else
      conflicts =
        MapSet.new(safe_keys(fields))
        |> MapSet.intersection(MapSet.new(safe_keys(other_fields)))
        |> MapSet.to_list()
        |> Enum.map(&{&1, nil})

      merged = fields |> Keyword.merge(other_fields) |> Keyword.merge(conflicts)

      {:map, merged, nil}
    end
  end

  defp expand_call(
         env,
         {:atom, Map},
         :update,
         [map, key, _initial, _fun],
         _include_private,
         _,
         stack
       ) do
    fields = expand_map_fields(env, map, stack)

    if :none in fields do
      :none
    else
      case expand(env, key, stack) do
        {:atom, atom} ->
          {:map, fields |> Keyword.put(atom, nil), nil}

        :none ->
          :none

        _ ->
          {:map, fields, nil}
      end
    end
  end

  defp expand_call(env, {:atom, Map}, :update!, [map, key, _fun], _include_private, _, stack) do
    fields = expand_map_fields(env, map, stack)

    if :none in fields do
      :none
    else
      case expand(env, key, stack) do
        {:atom, atom} ->
          {:map, fields |> Keyword.put(atom, nil), nil}

        :none ->
          :none

        _ ->
          {:map, fields, nil}
      end
    end
  end

  defp expand_call(env, {:atom, Map}, :from_struct, [struct], _include_private, _, stack) do
    fields =
      case expand(env, struct, stack) do
        {:struct, fields, _, nil} ->
          fields

        {:atom, atom} ->
          case expand(env, {:struct, [], {:atom, atom}, nil}, stack) do
            {:struct, fields, _, nil} -> fields
            nil -> []
            _ -> [:none]
          end

        nil ->
          []

        _ ->
          [:none]
      end
      |> Keyword.delete(:__struct__)

    if :none in fields do
      :none
    else
      {:map, fields, nil}
    end
  end

  # function call
  defp expand_call(env, {:atom, mod}, fun, arguments, include_private, position, stack)
       when mod not in [nil, true, false] and fun not in [nil, true, false] do
    arity = length(arguments)

    case expand_call_from_metadata(env, mod, fun, arity, include_private, position, stack) do
      result when result not in [:none] -> result
      _ -> expand_call_from_introspection(env, mod, fun, arity, include_private, stack)
    end
  end

  # not a module
  defp expand_call(_env, {:atom, _mod}, _fun, _arity, _include_private, _, _stack), do: :none

  defp expand_call(env, {:union, variants}, fun, arity, include_private, position, stack) do
    # TODO choose variant by args?
    Enum.find_value(variants, fn variant ->
      res = expand_call(env, variant, fun, arity, include_private, position, stack)

      if res != :none do
        res
      end
    end)
  end

  defp expand_call(_env, _target, _fun, _arity, _include_private, _, _stack), do: nil

  defp call_arity_match?(fun_arity, fun_defaults, call_arity) do
    fun_arity - fun_defaults <= call_arity and call_arity <= fun_arity
  end

  defp expand_call_from_introspection(env, mod, fun, arity, include_private, stack) do
    maybe_kind_arity =
      case ElixirSense.Core.Normalized.Code.get_docs(mod, :docs) do
        nil ->
          # no docs - use call arity if fun exported
          if function_exported?(mod, fun, arity) do
            arity
          end

        list ->
          # correct arity for calls with default params
          list
          |> Enum.find_value(nil, fn {{f, a}, _, kind, _, _, map} ->
            if f == fun and call_arity_match?(a, Map.get(map, :defaults, 0), arity) and
                 (kind != :macro or mod in env.requires) do
              {kind, a}
            end
          end)
      end

    case maybe_kind_arity do
      nil ->
        # def not found
        :none

      {:macro, _} ->
        # do not expand macro result types
        :no_spec

      {:function, arity} ->
        type = TypeInfo.get_function_spec(mod, fun, arity)
        return_type = get_return_from_spec(env, type, mod, include_private)
        expand(env, return_type, stack) || :no_spec
    end
  end

  defp expand_call_from_metadata(
         %Binding{specs: specs, mods_funs_to_positions: mods_funs_to_positions} = env,
         mod,
         fun,
         arity,
         include_private,
         position,
         stack
       ) do
    maybe_kind_arity =
      Enum.find_value(mods_funs_to_positions, nil, fn
        {{^mod, ^fun, _}, %State.ModFunInfo{type: fun_type} = info} ->
          visible? =
            cond do
              include_private and
                  (State.ModFunInfo.get_category(info) != :macro or
                     List.last(info.positions) < position) ->
                true

              not include_private and Introspection.is_pub(fun_type) and
                  (State.ModFunInfo.get_category(info) != :macro or mod in env.requires) ->
                true

              true ->
                false
            end

          if visible? do
            # correct arity for calls with default params
            State.ModFunInfo.get_arities(info)
            |> Enum.find_value(nil, fn {a, defaults} ->
              if call_arity_match?(a, defaults, arity) do
                {State.ModFunInfo.get_category(info), a}
              end
            end)
          end

        _ ->
          false
      end)

    case maybe_kind_arity do
      nil ->
        # def not found
        :none

      {:macro, _} ->
        # do not expand macro result types
        :no_spec

      {:function, arity} ->
        case specs[{mod, fun, arity}] do
          nil ->
            :no_spec

          %State.SpecInfo{} = spec ->
            get_return_from_metadata(env, mod, spec, include_private, stack) || :no_spec
        end
    end
  end

  defp extract_type({:"::", _, [_, type]}), do: {:ok, type}

  defp extract_type({:when, _, [{:"::", _, [_, type]}, type_params]}) do
    # substitute type params
    res =
      Macro.prewalk(type, fn
        {atom, _, nil} = var ->
          Keyword.get(type_params, atom, var)

        other ->
          other
      end)

    {:ok, res}
  end

  defp extract_type(_), do: :error

  defp get_return_from_metadata(
         env,
         mod,
         %State.SpecInfo{specs: [func_spec]},
         include_private,
         stack
       ) do
    case Code.string_to_quoted(func_spec) do
      {:ok, {:@, _, [{_kind, _, [ast]}]}} ->
        case extract_type(ast) do
          {:ok, type} ->
            parsed_type = parse_type(env, type, mod, include_private, [])
            expand(env, parsed_type, stack)

          :error ->
            nil
        end

      _ ->
        nil
    end
  end

  # intersection specs
  # treat as union
  # TODO get correct basing on call args
  defp get_return_from_metadata(
         env,
         mod,
         %State.SpecInfo{specs: [_ | _] = variants} = spec,
         include_private,
         stack
       ) do
    {:union,
     variants
     |> Enum.map(
       &get_return_from_metadata(
         env,
         mod,
         %State.SpecInfo{spec | specs: [&1]},
         include_private,
         stack
       )
     )}
  end

  defp get_return_from_spec(_env, nil, _, _include_private), do: nil

  defp get_return_from_spec(env, {{fun, _arity}, [ast]}, mod, include_private) do
    case Typespec.spec_to_quoted(fun, ast) |> extract_type do
      {:ok, type} ->
        parse_type(env, type, mod, include_private, [])

      :error ->
        nil
    end
  end

  # intersection specs
  # treat as union
  # TODO get correct basing on call args
  defp get_return_from_spec(env, {{fun, arity}, [_ast | _] = variants}, mod, include_private) do
    {:union,
     variants |> Enum.map(&get_return_from_spec(env, {{fun, arity}, [&1]}, mod, include_private))}
  end

  # union type
  defp parse_type(env, {:|, _, variants}, mod, include_private, stack) do
    {:union, variants |> Enum.map(&parse_type(env, &1, mod, include_private, stack))}
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
         include_private,
         stack
       ) do
    fields =
      for {field, type} <- fields,
          is_atom(field),
          do: {field, parse_type(env, type, mod, include_private, stack)}

    module =
      case struct_mod do
        m when is_atom(m) ->
          m

        {:__aliases__, _, list} ->
          Module.concat(list)

        _ ->
          nil
      end

    if module do
      {:struct, fields, {:atom, module}, nil}
    end
  end

  # map
  defp parse_type(env, {:%{}, _, fields}, mod, include_private, stack) do
    fields =
      for {field, type} <- fields,
          field = drop_optional(field),
          is_atom(field),
          do: {field, parse_type(env, type, mod, include_private, stack)}

    {:map, fields, nil}
  end

  defp parse_type(_env, {:map, _, []}, _mod, _include_private, _stack) do
    {:map, [], nil}
  end

  defp parse_type(env, {:{}, _, fields}, mod, include_private, stack) do
    {:tuple, length(fields),
     fields |> Enum.map(&parse_type(env, &1, mod, include_private, stack))}
  end

  defp parse_type(_env, [], _mod, _include_private, _stack) do
    {:list, :empty}
  end

  defp parse_type(env, [type | _], mod, include_private, stack) do
    {:list, parse_type(env, type, mod, include_private, stack)}
  end

  # for simplicity we skip terminator type
  defp parse_type(env, {kind, _, [type, _]}, mod, include_private, stack)
       when kind in [:maybe_improper_list, :nonempty_improper_list, :nonempty_maybe_improper_list] do
    {:list, parse_type(env, type, mod, include_private, stack)}
  end

  defp parse_type(_env, {:list, _, []}, _mod, _include_private, _stack) do
    {:list, nil}
  end

  defp parse_type(_env, {:keyword, _, []}, _mod, _include_private, _stack) do
    # no support for atom type for now
    {:list, {:tuple, 2, [nil, nil]}}
  end

  defp parse_type(env, {:keyword, _, [type]}, mod, include_private, stack) do
    # no support for atom type for now
    {:list, {:tuple, 2, [nil, parse_type(env, type, mod, include_private, stack)]}}
  end

  # remote user type
  defp parse_type(env, {{:., _, [remote, type]}, _, args}, _mod, _include_private, stack) do
    module =
      case remote do
        m when is_atom(m) ->
          m

        {:__aliases__, _, list} ->
          Module.concat(list)

        _ ->
          nil
      end

    if remote && is_atom(type) do
      # do not propagate include_private when expanding remote types
      expand_type(env, module, type, args, false, stack)
    end
  end

  # no_return
  defp parse_type(_env, {:no_return, _, _}, _, _include_private, _stack), do: :none

  # term, any, dynamic
  defp parse_type(_env, {kind, _, _}, _, _include_private, _stack)
       when kind in [:term, :any, :dynamic],
       do: nil

  # local user type
  defp parse_type(env, {atom, _, args}, mod, include_private, stack) when is_atom(atom) do
    # propagate include_private when expanding local types
    expand_type(env, mod, atom, args, include_private, stack)
  end

  # atom
  defp parse_type(_env, atom, _, _include_private, _stack) when is_atom(atom), do: {:atom, atom}

  defp parse_type(_env, integer, _, _include_private, _stack) when is_integer(integer) do
    {:integer, integer}
  end

  # other
  defp parse_type(_env, _type, _, _include_private, _stack), do: nil

  defp expand_type(env, mod, type_name, args, include_private, stack) do
    arity = length(args || [])
    type = {mod, type_name, arity}

    if type in stack do
      # self referential type
      nil
    else
      do_expand_type(env, mod, type_name, args, include_private, [type | stack])
    end
  end

  defp do_expand_type(env, mod, type_name, args, include_private, stack) do
    arity = length(args || [])

    case expand_type_from_metadata(env, mod, type_name, arity, include_private, stack) do
      nil -> expand_type_from_introspection(env, mod, type_name, arity, include_private, stack)
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
         include_private,
         stack
       ) do
    case types[{mod, type_name, arity}] do
      %State.TypeInfo{specs: [type_spec], kind: kind}
      when type_is_public(kind, include_private) ->
        case Code.string_to_quoted(type_spec) do
          {:ok, {:@, _, [{_kind, _, [ast]}]}} ->
            case extract_type(ast) do
              {:ok, type} ->
                parse_type(env, type, mod, include_private, stack) || :no_spec

              :error ->
                nil
            end

          _ ->
            :no_spec
        end

      nil ->
        nil

      _ ->
        :no_spec
    end
  end

  defp expand_type_from_introspection(env, mod, type_name, arity, include_private, stack) do
    case TypeInfo.get_type_spec(mod, type_name, arity) do
      {kind, spec} when type_is_public(kind, include_private) ->
        {:"::", _, [{_expanded_name, _, _}, type]} = Typespec.type_to_quoted(spec)

        parse_type(env, type, mod, include_private, stack)

      _ ->
        nil
    end
  end

  defp drop_optional({:optional, _, [key]}), do: key
  defp drop_optional(other), do: other

  defp combine_intersection(:none, _), do: :none
  defp combine_intersection(_, :none), do: :none
  defp combine_intersection(nil, type), do: type
  defp combine_intersection(type, nil), do: type
  defp combine_intersection(type, type), do: type

  # NOTE intersection is not strict and does an union on map keys

  defp combine_intersection({:struct, fields_1, nil, nil}, {:struct, fields_2, nil, nil}) do
    keys = (safe_keys(fields_1) ++ safe_keys(fields_2)) |> Enum.uniq()
    fields = for k <- keys, do: {k, combine_intersection(fields_1[k], fields_2[k])}

    if Enum.any?(fields, fn {_k, v} -> v == :none end) do
      :none
    else
      {:struct, fields, nil, nil}
    end
  end

  defp combine_intersection({:struct, fields_1, type, nil}, {:struct, fields_2, type_2, nil})
       when type_2 == type or is_nil(type_2) do
    keys = safe_keys(fields_1)
    fields = for k <- keys, do: {k, combine_intersection(fields_1[k], fields_2[k])}

    if Enum.any?(fields, fn {_k, v} -> v == :none end) do
      :none
    else
      {:struct, fields, type, nil}
    end
  end

  defp combine_intersection(
         {:struct, _fields_1, nil, nil} = s1,
         {:struct, _fields_2, _type, nil} = s2
       ) do
    combine_intersection(s2, s1)
  end

  defp combine_intersection({:map, fields_1, nil}, {:map, fields_2, nil}) do
    keys = (safe_keys(fields_1) ++ safe_keys(fields_2)) |> Enum.uniq()
    fields = for k <- keys, do: {k, combine_intersection(fields_1[k], fields_2[k])}

    if Enum.any?(fields, fn {_k, v} -> v == :none end) do
      :none
    else
      {:map, fields, nil}
    end
  end

  defp combine_intersection({:struct, fields_1, type, nil}, {:map, fields_2, nil}) do
    keys =
      if type != nil,
        do: safe_keys(fields_1),
        else: (safe_keys(fields_1) ++ safe_keys(fields_2)) |> Enum.uniq()

    fields = for k <- keys, do: {k, combine_intersection(fields_1[k], fields_2[k])}

    if Enum.any?(fields, fn {_k, v} -> v == :none end) do
      :none
    else
      {:struct, fields, type, nil}
    end
  end

  defp combine_intersection({:map, _fields_1, nil} = map, {:struct, _fields_2, _type, nil} = str) do
    combine_intersection(str, map)
  end

  defp combine_intersection({:tuple, n, fields_1}, {:tuple, n, fields_2}) do
    combined_fields =
      Enum.zip(fields_1, fields_2) |> Enum.map(fn {f1, f2} -> combine_intersection(f1, f2) end)

    if :none in combined_fields do
      :none
    else
      {:tuple, n, combined_fields}
    end
  end

  defp combine_intersection(other, {:union, variants}),
    do: combine_intersection({:union, variants}, other)

  defp combine_intersection({:union, variants}, other) do
    Enum.find_value(variants, fn v ->
      combined = combine_intersection(v, other)

      if combined != :none do
        combined
      end
    end)
  end

  defp combine_intersection(_, _), do: :none

  defp expand_map_fields(env, map_or_struct, stack) do
    case expand(env, map_or_struct, stack) do
      {:map, fields, nil} -> fields
      {:struct, fields, _, nil} -> fields
      nil -> []
      _ -> [:none]
    end
  end

  def from_var(value) when is_atom(value) do
    {:atom, value}
  end

  def from_var(%type{} = struct) do
    fields =
      for {key, value} <- struct |> Map.from_struct() do
        {key, from_var(value)}
      end

    {:struct, fields |> Keyword.put(:__struct__, {:atom, type}), {:atom, type}, nil}
  end

  def from_var(map) when is_map(map) do
    fields =
      for {key, value} <- map do
        {key, from_var(value)}
      end

    {:map, fields, nil}
  end

  def from_var(int) when is_integer(int), do: {:integer, int}

  def from_var(tuple) when is_tuple(tuple) do
    list =
      tuple
      |> Tuple.to_list()
      |> Enum.map(&from_var(&1))

    {:tuple, length(list), list}
  end

  def from_var(_), do: nil

  defp safe_keys(maybe_keyword) do
    for {key, _} when is_atom(key) <- maybe_keyword do
      key
    end
  end
end
