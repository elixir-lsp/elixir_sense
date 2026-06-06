defmodule ElixirSense.Core.TypeInference do
  @moduledoc false
  def type_of(
        {:%, _struct_meta,
         [
           _struct_ast,
           {:%{}, _map_meta, [{:|, _, _} | _]}
         ]},
        :match
      ),
      do: :none

  def type_of(
        {:%, _meta,
         [
           struct_ast,
           {:%{}, _, _} = ast
         ]},
        context
      ) do
    {fields, updated_struct} =
      case type_of(ast, context) do
        {:map, fields, updated_map} -> {fields, updated_map}
        {:struct, fields, _, updated_struct} -> {fields, updated_struct}
        _ -> {[], nil}
      end

    type = type_of(struct_ast, context) |> known_struct_type()

    {:struct, fields, type, updated_struct}
  end

  def type_of({{:., _, [target, fun]}, _meta, args} = ast, context)
      when is_atom(fun) and is_list(args) do
    if ElixirSense.Core.ElixirTypes.enabled?() do
      type_of_with_elixir_types(ast, context)
    else
      target = type_of(target, context)
      {:call, target, fun, Enum.map(args, &type_of(&1, context))}
    end
  end

  # pinned variable
  def type_of({:^, _, [pinned]}, :match), do: type_of(pinned, nil)
  def type_of({:^, _, [_pinned]}, _context), do: :none

  # variable
  def type_of({:_, _meta, var_context}, context)
      when is_atom(var_context) and context != :match,
      do: :none

  def type_of({var, meta, var_context}, context)
      when is_atom(var) and is_atom(var_context) and
             var not in [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__, :_] and
             context != :match do
    case Keyword.fetch(meta, :version) do
      {:ok, version} ->
        {:variable, var, version}

      _ ->
        nil
    end
  end

  # attribute
  # expanded attribute reference has nil arg
  def type_of({:@, _, [{attribute, _, nil}]}, _context)
      when is_atom(attribute) do
    {:attribute, attribute}
  end

  # module or atom
  def type_of(atom, _context) when is_atom(atom) do
    {:atom, atom}
  end

  # map
  def type_of({:%{}, _meta, [{:|, _, _} | _]}, :match), do: :none

  def type_of({:%{}, _meta, ast}, context) do
    {updated_map, fields} =
      case ast do
        [{:|, _, [left, right]}] ->
          {type_of(left, context), right}

        list ->
          {nil, list}
      end

    field_types = get_fields_type(fields, context)

    case field_types |> Keyword.fetch(:__struct__) do
      {:ok, type} ->
        {:struct, field_types |> Keyword.delete(:__struct__), type |> known_struct_type(),
         updated_map}

      _ ->
        {:map, field_types, updated_map}
    end
  end

  # match
  def type_of({:=, _, [left, right]}, context) do
    intersect(type_of(left, :match), type_of(right, context))
  end

  # stepped range struct
  def type_of({:..//, _, [first, last, step]}, context) do
    {:struct,
     [
       first: type_of(first, context),
       last: type_of(last, context),
       step: type_of(step, context)
     ], {:atom, Range}, nil}
  end

  # range struct
  def type_of({:.., _, [first, last]}, context) do
    {:struct,
     [
       first: type_of(first, context),
       last: type_of(last, context),
       step: type_of(1, context)
     ], {:atom, Range}, nil}
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
  def type_of({sigil, _, _}, _context) when is_map_key(@builtin_sigils, sigil) do
    # TODO support custom sigils?
    {:struct, [], {:atom, @builtin_sigils |> Map.fetch!(sigil)}, nil}
  end

  # tuple
  # regular tuples use {:{}, [], [field_1, field_2]} ast
  # two element use {field_1, field_2} ast (probably as an optimization)
  # detect and convert to regular
  def type_of(ast, context) when is_tuple(ast) and tuple_size(ast) == 2 do
    type_of({:{}, [], Tuple.to_list(ast)}, context)
  end

  def type_of({:{}, _, list} = ast, context) do
    native = type_of_with_elixir_types(ast, context)

    if native do
      native
    else
      {:tuple, length(list), list |> Enum.map(&type_of(&1, context))}
    end
  end

  def type_of(list, context) when is_list(list) do
    native = type_of_with_elixir_types(list, context)

    if native do
      native
    else
      type =
        case list do
          [] ->
            :empty

          [{:|, _, [head, _tail]}] ->
            type_of(head, context)

          [head | _] ->
            type_of(head, context)
        end

      {:list, type}
    end
  end

  def type_of({:fn, _meta, clauses} = ast, context) do
    if ElixirSense.Core.ElixirTypes.enabled?() do
      type_of_with_elixir_types(ast, context)
    else
      # In disabled mode, at least extract arity from the first clause
      case clauses do
        [{:->, _, [args, _body]} | _] when is_list(args) ->
          arity = length(args)

          if arity == 0 do
            {:fun, 0}
          else
            {:fun, List.duplicate(nil, arity), nil}
          end

        _ ->
          nil
      end
    end
  end

  # block expressions
  def type_of({:__block__, _meta, exprs}, context) do
    case List.last(exprs) do
      nil -> nil
      last_expr -> type_of(last_expr, context)
    end
  end

  def type_of({form, _meta, _clauses} = ast, context)
      when form in [:case, :cond, :try, :receive, :with] do
    type_of_with_elixir_types(ast, context)
  end

  # for comprehensions produce a list (unless :into is specified)
  def type_of({:for, _meta, clauses} = ast, context) when is_list(clauses) do
    native_result = type_of_with_elixir_types(ast, context)

    if native_result do
      native_result
    else
      # Basic: check if :into option is present
      opts = List.last(clauses)

      case opts do
        [{:into, into_target} | _] -> type_of_into_target(into_target)
        [{:do, _body} | _] -> {:list, nil}
        _ -> nil
      end
    end
  end

  # Binaries always produce a binary
  def type_of({:<<>>, _meta, _clauses} = ast, context) do
    type_of_with_elixir_types(ast, context) || {:binary, nil}
  end

  # special forms
  # quote/unquote/import/alias/require etc. don't produce meaningful types
  def type_of({form, _meta, _clauses}, _context)
      when form in [
             :quote,
             :unquote,
             :unquote_splicing,
             :import,
             :alias,
             :require,
             :__aliases__,
             :__cursor__,
             :super,
             :"::"
           ],
      do: nil

  # __DIR__ always returns a binary (the current file's directory)
  def type_of({:__DIR__, _meta, _clauses}, _context), do: {:binary, nil}

  # __ENV__ is already expanded to map
  def type_of({form, _meta, _clauses}, _context) when form in [:__CALLER__] do
    {:struct, [], {:atom, Macro.Env}, nil}
  end

  def type_of({:__STACKTRACE__, _meta, _clauses}, _context) do
    {:list, nil}
  end

  def type_of({var, meta, args} = ast, context) when is_atom(var) and is_list(args) do
    if ElixirSense.Core.ElixirTypes.enabled?() do
      type_of_with_elixir_types(ast, context)
    else
      line = Keyword.get(meta, :line, 1)
      column = Keyword.get(meta, :column, 1)
      {:local_call, var, {line, column}, Enum.map(args, &type_of(&1, context))}
    end
  end

  # integer
  def type_of(integer, _context) when is_integer(integer) do
    {:integer, integer}
  end

  # float
  def type_of(float, _context) when is_float(float) do
    {:float, float}
  end

  # binary/string literal
  def type_of(binary, _context) when is_binary(binary) do
    {:binary, binary}
  end

  # other
  def type_of(ast, context), do: type_of_with_elixir_types(ast, context)

  @doc """
  Type an expression with compiler state and env context.

  When native typing is enabled, seeds variable types and module/function/file
  context from the compiler state into the native inference engine. Falls back to
  `type_of/2` when native typing is disabled or returns nil.
  """
  def type_of(ast, context, %{vars_info: [current_scope | _]} = _state, env)
      when is_map(current_scope) do
    if ElixirSense.Core.ElixirTypes.enabled?() do
      env_context = build_env_context(current_scope, env)
      native_result = type_of_with_elixir_types(ast, context, nil, nil, env_context)
      native_result || type_of(ast, context)
    else
      type_of(ast, context)
    end
  end

  def type_of(ast, context, _state, _env), do: type_of(ast, context)

  defp build_env_context(current_scope, env) do
    vars =
      current_scope
      |> Map.values()
      |> Enum.filter(&match?(%ElixirSense.Core.State.VarInfo{}, &1))

    %{
      module: env[:module],
      function: env[:function],
      file: env[:file],
      vars: vars
    }
  end

  # Helper to use ElixirTypes adaptor as fallback
  defp type_of_with_elixir_types(ast, context) do
    type_of_with_elixir_types(ast, context, nil, nil, %{})
  end

  # Helper to use ElixirTypes adaptor with optional local signatures and metadata
  # Used by both the internal type_of/4 path and the external type_of_with_elixir_types/5 API
  def type_of_with_elixir_types(
        ast,
        _context,
        local_sigs_map,
        metadata \\ nil,
        env_context \\ %{}
      ) do
    if ElixirSense.Core.ElixirTypes.enabled?() do
      case type_expr_with_local_sigs(ast, local_sigs_map, metadata, env_context) do
        {:ok, descr} -> ElixirSense.Core.ElixirTypes.to_shape(descr)
        :error -> nil
      end
    else
      nil
    end
  end

  # Helper to type an expression with local signatures and metadata
  defp type_expr_with_local_sigs(ast, local_sigs_map, metadata, env_context) do
    signatures =
      if is_map(local_sigs_map) and not remote_call_ast?(ast) do
        Map.drop(local_sigs_map, [:__module__])
      else
        nil
      end

    module =
      Map.get(env_context, :module) ||
        (is_map(local_sigs_map) && Map.get(local_sigs_map, :__module__)) ||
        extract_module_from_context(ast)

    ElixirSense.Core.ElixirTypes.of_expr(ast,
      module: module,
      function: Map.get(env_context, :function),
      file: Map.get(env_context, :file),
      mode: :dynamic,
      local_sigs_map: signatures,
      metadata: metadata,
      variables: variables_from_env_context(env_context)
    )
  end

  # Fallback module for type_of/2 path (when no compiler env is available)
  defp extract_module_from_context(_ast), do: nil

  # Build ElixirTypes variables map from the elixir_sense compiler env_context
  defp variables_from_env_context(env_context) do
    vars = if is_map(env_context), do: Map.get(env_context, :vars), else: nil

    if is_list(vars) do
      Enum.reduce(vars, %{}, fn
        %ElixirSense.Core.State.VarInfo{name: name, version: version, elixir_types_descr: descr},
        acc
        when is_atom(name) and is_integer(version) ->
          descr = descr || Module.Types.Descr.dynamic()
          Map.put(acc, {name, version}, descr)

        _, acc ->
          acc
      end)
    else
      %{}
    end
  end

  defp remote_call_ast?({{:., _, [_target, fun]}, _, args}) when is_atom(fun) and is_list(args),
    do: true

  defp remote_call_ast?(_), do: false

  # Infer the result type of a `for` comprehension with `:into`
  defp type_of_into_target(target) when is_binary(target), do: {:binary, nil}
  defp type_of_into_target(target) when is_list(target), do: {:list, nil}
  defp type_of_into_target({:%{}, _, []}), do: {:map, [], nil}
  defp type_of_into_target({:%{}, _, [{:|, _, _} | _]}), do: {:map, [], nil}

  defp type_of_into_target(_), do: nil

  defp get_fields_type(fields, context) do
    for {field, value} <- fields,
        is_atom(field) do
      {field, type_of(value, context)}
    end
  end

  # expand struct type - only compile type atoms or attributes are supported
  # variables supported in match context
  defp known_struct_type(type) do
    case type do
      {:atom, atom} -> {:atom, atom}
      {:attribute, attribute} -> {:attribute, attribute}
      {:variable, variable, version} -> {:variable, variable, version}
      _ -> nil
    end
  end

  def find_typed_vars(ast, match_context, context) do
    {_ast, {vars, _match_context, _context}} =
      Macro.prewalk(ast, {[], match_context, context}, &match_var(&1, &2))

    Enum.uniq(vars)
  end

  defp match_var({:when, _, [left, _right]}, match_context) do
    # no variables can be defined in guard context so we skip that subtree
    match_var(left, match_context)
  end

  defp match_var(
         {:=, _meta,
          [
            left,
            right
          ]},
         {vars, match_context, context}
       ) do
    {_ast, {vars, _match_context, _context}} =
      match_var(
        left,
        {vars, intersect(match_context, type_of(right, context)), :match}
      )

    {_ast, {vars, _match_context, _context}} =
      match_var(
        right,
        {vars, intersect(match_context, type_of(left, :match)), context}
      )

    {nil, {vars, nil, context}}
  end

  # pinned variable
  defp match_var(
         {:^, _meta, [{var, _var_meta, var_context}]},
         {vars, match_context, context}
       )
       when is_atom(var) and is_atom(var_context) do
    {nil, {vars, match_context, context}}
  end

  # variable
  defp match_var(
         {var, meta, var_context},
         {vars, match_context, :match}
       )
       when is_atom(var) and is_atom(var_context) and
              var not in [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__, :_] do
    case Keyword.fetch(meta, :version) do
      {:ok, version} ->
        {nil, {[{{var, version}, match_context} | vars], nil, :match}}

      _ ->
        {nil, {vars, match_context, :match}}
    end
  end

  defp match_var({:%, _, [type_ast, {:%{}, _, _ast} = map_ast]}, {vars, match_context, context}) do
    {_ast, {type_vars, _match_context, _context}} =
      match_var(
        type_ast,
        {[],
         propagate_context(
           match_context,
           &{:map_key, &1, type_of(:__struct__, context)}
         ), context}
      )

    {_ast, {map_vars, _match_context, _context}} =
      match_var(map_ast, {[], match_context, context})

    {nil, {vars ++ map_vars ++ type_vars, nil, context}}
  end

  defp match_var({:%{}, _, ast}, {vars, match_context, context}) do
    {updated_vars, list} =
      case ast do
        [{:|, _, [left, right]} | _] ->
          if context == :match do
            # map update is forbidden in match, we're in invalid code
            {[], []}
          else
            {_ast, {updated_vars, _match_context, _context}} = match_var(left, {[], nil, context})
            {updated_vars, right}
          end

        list ->
          {[], list}
      end

    destructured_vars =
      list
      |> Enum.flat_map(fn
        {key, value_ast} ->
          key_type = type_of(key, context)

          {_ast, {new_vars, _match_context, _context}} =
            match_var(
              value_ast,
              {[], propagate_context(match_context, &{:map_key, &1, key_type}), context}
            )

          new_vars
      end)

    {nil, {vars ++ destructured_vars ++ updated_vars, nil, context}}
  end

  # regular tuples use {:{}, [], [field_1, field_2]} ast
  # two element use `{field_1, field_2}` ast (probably as an optimization)
  # detect and convert to regular
  defp match_var(ast, {vars, match_context, context})
       when is_tuple(ast) and tuple_size(ast) == 2 do
    match_var({:{}, [], ast |> Tuple.to_list()}, {vars, match_context, context})
  end

  defp match_var({:{}, _, ast}, {vars, match_context, context}) do
    destructured_vars =
      ast
      |> Enum.with_index()
      |> Enum.flat_map(fn {nth_elem_ast, n} ->
        {_ast, {new_vars, _match_context, _context}} =
          match_var(
            nth_elem_ast,
            {[], propagate_context(match_context, &{:tuple_nth, &1, n}), context}
          )

        new_vars
      end)

    {nil, {vars ++ destructured_vars, nil, context}}
  end

  defp match_var({{:., _, [:erlang, :++]}, _, [left, right]}, {vars, match_context, context})
       when is_list(left) do
    # NOTE this may produce improper lists
    match_var(left ++ right, {vars, match_context, context})
  end

  defp match_var(list, {vars, match_context, context}) when is_list(list) do
    match_var_list = fn head, tail ->
      {_ast, {new_vars_head, _match_context, _context}} =
        match_var(head, {[], propagate_context(match_context, &{:list_head, &1}), context})

      {_ast, {new_vars_tail, _match_context, _context}} =
        match_var(tail, {[], propagate_context(match_context, &{:list_tail, &1}), context})

      {nil, {vars ++ new_vars_head ++ new_vars_tail, nil, context}}
    end

    case list do
      [] ->
        {nil, {vars, nil, context}}

      [{:|, _, [head, tail]}] ->
        match_var_list.(head, tail)

      [head | tail] ->
        match_var_list.(head, tail)
    end
  end

  # Binary/bitstring patterns — variables inside get type based on their segment spec
  defp match_var({:<<>>, _, segments}, {vars, _match_context, context}) do
    destructured_vars =
      segments
      |> Enum.flat_map(fn
        {:"::", _, [var_ast, type_spec]} ->
          # Determine the type from the binary segment type spec
          segment_type = binary_segment_type(type_spec)

          {_ast, {new_vars, _match_context, _context}} =
            match_var(var_ast, {[], segment_type, context})

          new_vars

        _ ->
          []
      end)

    {nil, {vars ++ destructured_vars, nil, context}}
  end

  defp match_var(ast, {vars, _match_context, context}) do
    # traverse literals, not expanded macro calls with nil match_context
    # we cannot assume anything basing on match_context on variables there
    {ast, {vars, nil, context}}
  end

  # Extract type from binary segment type spec (e.g., ::binary, ::integer-8, ::utf8)
  defp binary_segment_type({:-, _, [left, _right]}), do: binary_segment_type(left)
  defp binary_segment_type({:binary, _, _}), do: {:binary, nil}
  defp binary_segment_type({:bytes, _, _}), do: {:binary, nil}
  defp binary_segment_type({:bitstring, _, _}), do: {:binary, nil}
  defp binary_segment_type({:bits, _, _}), do: {:binary, nil}
  defp binary_segment_type({:utf8, _, _}), do: {:integer, nil}
  defp binary_segment_type({:utf16, _, _}), do: {:integer, nil}
  defp binary_segment_type({:utf32, _, _}), do: {:integer, nil}
  defp binary_segment_type({:integer, _, _}), do: {:integer, nil}
  defp binary_segment_type({:float, _, _}), do: {:float, nil}
  defp binary_segment_type(_), do: {:integer, nil}

  defp propagate_context(nil, _), do: nil
  defp propagate_context(:none, _), do: :none
  defp propagate_context(match_context, fun), do: fun.(match_context)

  def intersect(nil, new), do: new
  def intersect(old, nil), do: old
  def intersect(:none, _), do: :none
  def intersect(_, :none), do: :none
  def intersect(old, old), do: old

  def intersect({:intersection, old}, {:intersection, new}) do
    {:intersection, Enum.uniq(old ++ new)}
  end

  def intersect({:intersection, old}, new) do
    {:intersection, Enum.uniq([new | old])}
  end

  def intersect(old, {:intersection, new}) do
    {:intersection, Enum.uniq([old | new])}
  end

  def intersect(old, new), do: {:intersection, [old, new]}
end
