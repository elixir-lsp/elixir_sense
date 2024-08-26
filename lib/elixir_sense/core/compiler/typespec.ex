defmodule ElixirSense.Core.Compiler.Typespec do
  alias ElixirSense.Core.Normalized.Macro.Env, as: NormalizedMacroEnv
  alias ElixirSense.Core.Compiler, as: ElixirExpand
  alias ElixirSense.Core.Compiler.Utils
  import ElixirSense.Core.State
  def spec_to_signature({:when, _, [spec, _]}), do: type_to_signature(spec)
  def spec_to_signature(other), do: type_to_signature(other)

  def type_to_signature({:"::", _, [{name, _, context}, _]})
      when is_atom(name) and name != :"::" and is_atom(context),
      do: {name, []}

  def type_to_signature({:"::", _, [{:__cursor__, _, args}, _]})
      when is_list(args) do
    # type name replaced by cursor
    {:__unknown__, []}
  end

  def type_to_signature({:"::", _, [{name, _, args}, _]})
      when is_atom(name) and name != :"::",
      do: {name, args}

  def type_to_signature({:__cursor__, _, args}) when is_list(args) do
    # type name replaced by cursor
    {:__unknown__, []}
  end

  def type_to_signature({name, _, args}) when is_atom(name) and name != :"::" do
    # elixir returns :error here, we handle incomplete signatures
    {name, args}
  end

  def type_to_signature(_), do: {:__unknown__, []}

  def expand_spec(ast, state, env) do
    # TODO not sure this is correct. Are module vars accessible?
    state =
      state
      |> new_func_vars_scope

    {ast, state, env} = do_expand_spec(ast, state, env)

    state =
      state
      |> remove_func_vars_scope

    {ast, state, env}
  end

  defp do_expand_spec({:when, meta, [spec, guard]}, state, env) do
    {spec, guard, state, env} = do_expand_spec(spec, guard, meta, state, env)
    {{:when, meta, [spec, guard]}, state, env}
  end

  defp do_expand_spec(spec, state, env) do
    {spec, _guard, state, env} = do_expand_spec(spec, [], [], state, env)
    {spec, state, env}
  end

  defp do_expand_spec(
         {:"::", meta, [{name, name_meta, args}, return]},
         guard,
         guard_meta,
         state,
         env
       )
       when is_atom(name) and name != :"::" do
    args =
      if is_atom(args) do
        []
      else
        args
      end
      |> sanitize_args()

    {_, state} = expand_typespec({name, name_meta, args}, :disabled, state, env)

    {guard, state, env} =
      if is_list(guard) do
        {guard, state, env}
      else
        # invalid guard may still have cursor
        {_, state} = expand_typespec(guard, :disabled, state, env)
        {[], state, env}
      end

    {state, var_names} =
      Enum.reduce(guard, {state, []}, fn
        {name, _val}, {state, var_names} when is_atom(name) ->
          # guard is a keyword list so we don't have exact meta on keys
          {add_var_write(state, {name, guard_meta, nil}), [name | var_names]}

        _, acc ->
          # invalid entry
          acc
      end)

    {args_reverse, state} =
      Enum.reduce(args, {[], state}, fn
        arg, {acc, state} ->
          {arg, state} = expand_typespec(arg, var_names, state, env)
          {[arg | acc], state}
      end)

    args = Enum.reverse(args_reverse)

    {return, state} = expand_typespec(return, var_names, state, env)

    {guard_reverse, state} =
      Enum.reduce(guard, {[], state}, fn
        {name, {:var, _, context}} = pair, {acc, state} when is_atom(name) and is_atom(context) ->
          # special type var
          {[pair | acc], state}

        {name, type}, {acc, state} when is_atom(name) ->
          {type, state} = expand_typespec(type, var_names, state, env)
          {[{name, type} | acc], state}

        other, {acc, state} ->
          # there may be cursor in invalid entries
          {_type, state} = expand_typespec(other, var_names, state, env)
          {acc, state}
      end)

    guard = Enum.reverse(guard_reverse)

    {{:"::", meta, [{name, name_meta, args}, return]}, guard, state, env}
  end

  defp do_expand_spec(other, guard, guard_meta, state, env) do
    case other do
      {name, meta, args} when is_atom(name) and name != :"::" ->
        # invalid or incomplete spec
        # try to wrap in :: expression
        do_expand_spec({:"::", meta, [{name, meta, args}, nil]}, guard, guard_meta, state, env)

      _ ->
        # there may be cursor in invalid entries
        {_type, state} = expand_typespec(guard, [], state, env)
        {_type, state} = expand_typespec(other, [], state, env)
        {other, guard, state, env}
    end
  end

  defp sanitize_args(args) do
    Enum.map(args, fn
      {:"::", meta, [left, right]} ->
        {:"::", meta, [remove_default(left), remove_default(right)]}

      other ->
        remove_default(other)
    end)
  end

  defp remove_default({:\\, _, [left, _]}), do: left
  defp remove_default(other), do: other

  def expand_type(ast, state, env) do
    state =
      state
      |> new_func_vars_scope

    {ast, state, env} = do_expand_type(ast, state, env)

    state =
      state
      |> remove_func_vars_scope

    {ast, state, env}
  end

  defp do_expand_type({:"::", meta, [{name, name_meta, args}, definition]}, state, env) do
    args =
      if is_atom(args) do
        []
      else
        args
      end

    {_, state} = expand_typespec({name, name_meta, args}, :disabled, state, env)

    {state, var_names} =
      Enum.reduce(args, {state, []}, fn
        {name, meta, context}, {state, var_names}
        when is_atom(name) and is_atom(context) and name != :_ ->
          {add_var_write(state, {name, meta, context}), [name | var_names]}

        other, acc ->
          # silently skip invalid typespec params
          acc
      end)

    {definition, state} = expand_typespec(definition, var_names, state, env)
    {{:"::", meta, [{name, name_meta, args}, definition]}, state, env}
  end

  defp do_expand_type(other, state, env) do
    case other do
      {name, meta, args} when is_atom(name) and name != :"::" ->
        # invalid or incomplete type
        # try to wrap in :: expression
        do_expand_type({:"::", meta, [{name, meta, args}, nil]}, state, env)

      _ ->
        # there may be cursor in invalid entries
        {_type, state} = expand_typespec(other, [], state, env)
        {other, state, env}
    end
  end

  def expand_typespec(ast, var_names \\ [], state, env) do
    typespec(ast, var_names, env, state)
  end

  # TODO Remove char_list type by v2.0
  def built_in_type?(:char_list, 0), do: true
  def built_in_type?(:charlist, 0), do: true
  def built_in_type?(:as_boolean, 1), do: true
  def built_in_type?(:struct, 0), do: true
  def built_in_type?(:nonempty_charlist, 0), do: true
  def built_in_type?(:keyword, 0), do: true
  def built_in_type?(:keyword, 1), do: true
  def built_in_type?(:var, 0), do: true
  def built_in_type?(name, arity), do: :erl_internal.is_type(name, arity)

  defp typespec({:__cursor__, meta, args}, vars, caller, state) when is_list(args) do
    state =
      unless state.cursor_env do
        state
        |> add_cursor_env(meta, caller)
      else
        state
      end

    node =
      case args do
        [h | _] -> h
        [] -> nil
      end

    typespec(node, vars, caller, state)
  end

  # Handle unions
  defp typespec({:|, meta, [left, right]}, vars, caller, state) do
    {left, state} = typespec(left, vars, caller, state)
    {right, state} = typespec(right, vars, caller, state)

    {{:|, meta, [left, right]}, state}
  end

  # Handle binaries
  defp typespec({:<<>>, meta, args}, vars, caller, state) do
    {args, state} = :lists.mapfoldl(&typespec(&1, vars, caller, &2), state, args)
    # elixir does complex binary spec validation
    {{:<<>>, meta, args}, state}
  end

  ## Handle maps and structs
  defp typespec({:%{}, meta, fields} = map, vars, caller, state) do
    fun = fn
      {{:required, meta2, [k]}, v}, state ->
        {arg1, state} = typespec(k, vars, caller, state)
        {arg2, state} = typespec(v, vars, caller, state)
        {{arg1, arg2}, state}

      {{:optional, meta2, [k]}, v}, state ->
        {arg1, state} = typespec(k, vars, caller, state)
        {arg2, state} = typespec(v, vars, caller, state)
        {{{:optional, meta2, [arg1]}, arg2}, state}

      {k, v}, state ->
        {arg1, state} = typespec(k, vars, caller, state)
        {arg2, state} = typespec(v, vars, caller, state)
        {{arg1, arg2}, state}

      invalid, state ->
        # elixir raises here invalid map specification
        {_, state} = typespec(invalid, vars, caller, state)
        {nil, state}
    end

    {fields, state} = :lists.mapfoldl(fun, state, fields)
    {{:%{}, meta, fields |> Enum.filter(&(&1 != nil))}, state}
  end

  defp typespec({:%, struct_meta, [name, {:%{}, meta, fields}]} = node, vars, caller, state) do
    case ElixirExpand.Macro.expand(name, %{caller | function: {:__info__, 1}}) do
      module when is_atom(module) ->
        # TODO register alias/struct
        struct =
          ElixirExpand.Map.load_struct(module, [], state, caller)
          |> Map.delete(:__struct__)
          |> Map.to_list()

        {fields, state} =
          fields
          |> Enum.reverse()
          |> Enum.reduce({[], state}, fn
            {k, v}, {fields, state} when is_atom(k) ->
              {[{k, v} | fields], state}

            other, {fields, state} ->
              # elixir raises expected key-value pairs in struct
              {_, state} = typespec(other, vars, caller, state)
              {fields, state}
          end)

        types =
          :lists.map(
            fn
              {:__exception__ = field, true} -> {field, Keyword.get(fields, field, true)}
              {field, _} -> {field, Keyword.get(fields, field, quote(do: term()))}
            end,
            :lists.sort(struct)
          )

        # look for cursor in invalid fields
        # elixir raises if there are any
        state =
          fields
          |> Enum.filter(fn {field, _} -> not Keyword.has_key?(struct, field) end)
          |> Enum.reduce(state, fn {_, type}, acc ->
            {_, acc} = typespec(type, vars, caller, acc)
            acc
          end)

        {map, state} = typespec({:%{}, meta, types}, vars, caller, state)
        {{:%, struct_meta, [module, map]}, state}

      other ->
        # elixir raises here unexpected expression in typespec
        {name, state} = typespec(other, vars, caller, state)
        {map, state} = typespec({:%{}, meta, fields}, vars, caller, state)
        {{:%, struct_meta, [name, map]}, state}
    end
  end

  # Handle records
  defp typespec({:record, meta, [atom]}, vars, caller, state) do
    typespec({:record, meta, [atom, []]}, vars, caller, state)
  end

  defp typespec({:record, meta, [tag, field_specs]}, vars, caller, state)
       when is_atom(tag) and is_list(field_specs) do
    # We cannot set a function name to avoid tracking
    # as a compile time dependency because for records it actually is one.
    case ElixirExpand.Macro.expand({tag, [], [{:{}, [], []}]}, caller) do
      {_, _, [name, fields | _]} when is_list(fields) ->
        types =
          :lists.map(
            fn {field, _} ->
              {:"::", [],
               [
                 {field, [], nil},
                 Keyword.get(field_specs, field, quote(do: term()))
               ]}
            end,
            fields
          )

        # look for cursor in invalid fields
        # elixir raises if there are any
        state =
          field_specs
          |> Enum.filter(fn {field, _} -> not Keyword.has_key?(fields, field) end)
          |> Enum.reduce(state, fn {_, type}, acc ->
            {_, acc} = typespec(type, vars, caller, acc)
            acc
          end)

        typespec({:{}, meta, [name | types]}, vars, caller, state)

      _ ->
        # elixir raises here
        {field_specs, state} =
          :lists.mapfoldl(&typespec(&1, vars, caller, &2), state, field_specs)

        {{:record, meta, [tag, field_specs]}, state}
    end
  end

  # Handle ranges
  defp typespec({:.., meta, [left, right]}, vars, caller, state) do
    {left, state} = typespec(left, vars, caller, state)
    {right, state} = typespec(right, vars, caller, state)
    # elixir validates range here

    {{:.., meta, [left, right]}, state}
  end

  # Handle special forms
  defp typespec({:__MODULE__, _, atom}, vars, caller, state) when is_atom(atom) do
    typespec(caller.module, vars, caller, state)
  end

  defp typespec({:__aliases__, _, _} = alias, vars, caller, state) do
    typespec(expand_remote(alias, caller), vars, caller, state)
  end

  # Handle funs
  defp typespec([{:->, meta, [args, return]}], vars, caller, state)
       when is_list(args) do
    {args, state} = fn_args(args, vars, caller, state)
    {spec, state} = typespec(return, vars, caller, state)

    {[{:->, meta, [args, spec]}], state}
  end

  # Handle type operator
  defp typespec(
         {:"::", meta, [{var_name, var_meta, context}, expr]} = ann_type,
         vars,
         caller,
         state
       )
       when is_atom(var_name) and is_atom(context) do
    # elixir warns if :: is nested
    {right, state} = typespec(expr, vars, caller, state)
    {{:"::", meta, [{var_name, var_meta, context}, right]}, state}
  end

  defp typespec({:"::", meta, [left, right]}, vars, caller, state) do
    # elixir warns here
    # invalid type annotation. The left side of :: must be a variable

    {left, state} = typespec(left, vars, caller, state)
    {right, state} = typespec(right, vars, caller, state)
    {{:"::", meta, [left, right]}, state}
  end

  # Handle remote calls in the form of @module_attribute.type.
  # These are not handled by the general remote type clause as calling
  # Macro.expand/2 on the remote does not expand module attributes (but expands
  # things like __MODULE__).
  defp typespec(
         {{:., dot_meta, [{:@, attr_meta, [{attr, _, _}]}, name]}, meta, args} = orig,
         vars,
         caller,
         state
       ) do
    # TODO Module.get_attribute(caller.module, attr)
    state =
      state
      |> add_attribute(caller, attr, attr_meta, nil, nil, false)
      |> add_call_to_line({Kernel, :@, 0}, attr_meta)

    case Map.get(state.attribute_store, {caller.module, attr}) do
      remote when is_atom(remote) and remote != nil ->
        {remote_spec, state} = typespec(remote, vars, caller, state)
        {name_spec, state} = typespec(name, vars, caller, state)
        remote_type({{:., dot_meta, [remote_spec, name_spec]}, meta, args}, vars, caller, state)

      _ ->
        # elixir raises here invalid remote in typespec
        {name_spec, state} = typespec(name, vars, caller, state)
        remote_type({{:., dot_meta, [nil, name_spec]}, meta, args}, vars, caller, state)
    end
  end

  # Handle remote calls
  defp typespec({{:., dot_meta, [remote, name]}, meta, args} = orig, vars, caller, state) do
    remote = expand_remote(remote, caller)

    if remote == caller.module do
      typespec({name, dot_meta, args}, vars, caller, state)
    else
      # elixir raises if remote is not atom
      {remote_spec, state} = typespec(remote, vars, caller, state)
      {name_spec, state} = typespec(name, vars, caller, state)
      remote_type({{:., dot_meta, [remote_spec, name_spec]}, meta, args}, vars, caller, state)
    end
  end

  # Handle tuples
  defp typespec({left, right}, vars, caller, state) do
    {left, state} = typespec(left, vars, caller, state)
    {right, state} = typespec(right, vars, caller, state)
    {{left, right}, state}
  end

  defp typespec({:{}, meta, args}, vars, caller, state) do
    {args, state} = :lists.mapfoldl(&typespec(&1, vars, caller, &2), state, args)

    {{:{}, meta, args}, state}
  end

  # Handle blocks
  defp typespec({:__block__, _meta, [arg]}, vars, caller, state) do
    typespec(arg, vars, caller, state)
  end

  # Handle variables or local calls
  defp typespec({name, meta, atom} = node, :disabled, caller, state) when is_atom(atom) do
    {{name, meta, atom}, state}
  end

  defp typespec({:_, meta, arg}, _vars, _caller, state) when not is_list(arg) do
    {{:_, meta, arg}, state}
  end

  defp typespec({name, meta, atom} = node, vars, caller, state) when is_atom(atom) do
    if :lists.member(name, vars) do
      state = add_var_read(state, node)
      {{name, meta, atom}, state}
    else
      typespec({name, meta, []}, vars, caller, state)
    end
  end

  # Handle local calls

  defp typespec({type, _meta, []}, vars, caller, state) when type in [:charlist, :char_list] do
    typespec(quote(do: :elixir.charlist()), vars, caller, state)
  end

  defp typespec({:nonempty_charlist, _meta, []}, vars, caller, state) do
    typespec(quote(do: :elixir.nonempty_charlist()), vars, caller, state)
  end

  defp typespec({:struct, _meta, []}, vars, caller, state) do
    typespec(quote(do: :elixir.struct()), vars, caller, state)
  end

  defp typespec({:as_boolean, _meta, [arg]}, vars, caller, state) do
    typespec(quote(do: :elixir.as_boolean(unquote(arg))), vars, caller, state)
  end

  defp typespec({:keyword, _meta, args}, vars, caller, state) when length(args) <= 1 do
    typespec(quote(do: :elixir.keyword(unquote_splicing(args))), vars, caller, state)
  end

  defp typespec({name, meta, args}, :disabled, caller, state) when is_atom(name) do
    {args, state} = :lists.mapfoldl(&typespec(&1, :disabled, caller, &2), state, args)
    {{name, meta, args}, state}
  end

  defp typespec({name, meta, args}, vars, caller, state) when is_atom(name) do
    {args, state} = :lists.mapfoldl(&typespec(&1, vars, caller, &2), state, args)
    # elixir raises if type is not defined

    state = add_call_to_line(state, {nil, name, length(args)}, meta)

    {{name, meta, args}, state}
  end

  # Handle literals
  defp typespec(atom, _, _, state) when is_atom(atom) do
    {atom, state}
  end

  defp typespec(integer, _, _, state) when is_integer(integer) do
    {integer, state}
  end

  defp typespec([], vars, caller, state) do
    {[], state}
  end

  defp typespec([{:..., meta, _}], vars, caller, state) do
    typespec({:nonempty_list, [], [{:any, meta, []}]}, vars, caller, state)
  end

  defp typespec([spec, {:..., _, _}], vars, caller, state) do
    typespec({:nonempty_list, [], [spec]}, vars, caller, state)
  end

  defp typespec([spec], vars, caller, state) do
    typespec({:list, [], [spec]}, vars, caller, state)
  end

  defp typespec(list, vars, caller, state) when is_list(list) do
    {list_reversed, state} =
      Enum.reduce(list, {[], state}, fn
        {k, v}, {acc, state} when is_atom(k) ->
          {[{k, v} | acc], state}

        other, {acc, state} ->
          # elixir raises on invalid list entries
          {_, state} = typespec(other, vars, caller, state)
          {acc, state}
      end)

    case list_reversed do
      [head | tail] ->
        union =
          :lists.foldl(
            fn elem, acc -> {:|, [], [elem, acc]} end,
            head,
            tail
          )

        typespec({:list, [], [union]}, vars, caller, state)

      [] ->
        {[], state}
    end
  end

  defp typespec(other, vars, caller, state) do
    # elixir raises here unexpected expression in typespec
    {_, state} =
      if Utils.has_cursor?(other) do
        typespec({:__cursor__, [], []}, vars, caller, state)
      else
        {nil, state}
      end

    {nil, state}
  end

  defp location(meta) do
    line = Keyword.get(meta, :line, 0)

    if column = Keyword.get(meta, :column) do
      {line, column}
    else
      line
    end
  end

  # TODO trace alias?
  # TODO trace calls in expand
  defdelegate expand_remote(other, env), to: ElixirSense.Core.Compiler.Macro, as: :expand

  defp remote_type({{:., dot_meta, [remote_spec, name_spec]}, meta, args}, vars, caller, state) do
    {args, state} = :lists.mapfoldl(&typespec(&1, vars, caller, &2), state, args)
    state = add_call_to_line(state, {remote_spec, name_spec, length(args)}, meta)
    {{{:., dot_meta, [remote_spec, name_spec]}, meta, args}, state}
  end

  defp fn_args(args, vars, caller, state) do
    :lists.mapfoldl(&typespec(&1, vars, caller, &2), state, args)
  end
end
