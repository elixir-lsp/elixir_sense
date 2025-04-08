defmodule ElixirSense.Core.Compiler.Macro do
  alias ElixirSense.Core.Normalized.Macro.Env, as: NormalizedMacroEnv

  @spec expand_literals(Macro.input(), Macro.Env.t()) :: Macro.output()
  def expand_literals(ast, env) do
    {ast, :ok} = expand_literals(ast, :ok, fn node, :ok -> {expand(node, env), :ok} end)
    ast
  end

  @spec expand_literals(Macro.t(), acc, (Macro.t(), acc -> {Macro.t(), acc})) :: Macro.t()
        when acc: term()
  def expand_literals(ast, acc, fun)

  def expand_literals({:__aliases__, meta, args}, acc, fun) do
    {args, acc} = expand_literals(args, acc, fun)

    if :lists.all(&is_atom/1, args) do
      fun.({:__aliases__, meta, args}, acc)
    else
      {{:__aliases__, meta, args}, acc}
    end
  end

  def expand_literals({:__MODULE__, _meta, ctx} = node, acc, fun) when is_atom(ctx) do
    fun.(node, acc)
  end

  def expand_literals({:%, meta, [left, right]}, acc, fun) do
    {left, acc} = expand_literals(left, acc, fun)
    {right, acc} = expand_literals(right, acc, fun)
    {{:%, meta, [left, right]}, acc}
  end

  def expand_literals({:%{}, meta, args}, acc, fun) do
    {args, acc} = expand_literals(args, acc, fun)
    {{:%{}, meta, args}, acc}
  end

  def expand_literals({:{}, meta, args}, acc, fun) do
    {args, acc} = expand_literals(args, acc, fun)
    {{:{}, meta, args}, acc}
  end

  def expand_literals({left, right}, acc, fun) do
    {left, acc} = expand_literals(left, acc, fun)
    {right, acc} = expand_literals(right, acc, fun)
    {{left, right}, acc}
  end

  def expand_literals(list, acc, fun) when is_list(list) do
    :lists.mapfoldl(&expand_literals(&1, &2, fun), acc, list)
  end

  def expand_literals(
        {{:., _, [{:__aliases__, _, [:Application]}, :compile_env]} = node, meta,
         [app, key, default]},
        acc,
        fun
      ) do
    # TODO track call?
    {default, acc} = expand_literals(default, acc, fun)
    {{node, meta, [app, key, default]}, acc}
  end

  def expand_literals(term, acc, _fun), do: {term, acc}

  @spec expand(Macro.input(), Macro.Env.t()) :: Macro.output()
  def expand(ast, env) do
    expand_until({ast, true}, env)
  end

  defp expand_until({ast, true}, env) do
    expand_until(do_expand_once(ast, env), env)
  end

  defp expand_until({ast, false}, _env) do
    ast
  end

  @spec expand_once(Macro.input(), Macro.Env.t()) :: Macro.output()
  def expand_once(ast, env) do
    elem(do_expand_once(ast, env), 0)
  end

  defp do_expand_once({:__aliases__, meta, [head | tail] = list} = alias, env) do
    # TODO pass true to track alias_expansion?
    case NormalizedMacroEnv.expand_alias(env, meta, list, trace: false) do
      {:alias, alias} ->
        :elixir_env.trace({:alias_reference, meta, alias}, env)
        {alias, true}

      :error ->
        {head, _} = do_expand_once(head, env)

        if is_atom(head) do
          receiver = Module.concat([head | tail])
          :elixir_env.trace({:alias_reference, meta, receiver}, env)
          {receiver, true}
        else
          {alias, false}
        end
    end
  end

  # Expand compilation environment macros
  defp do_expand_once({:__MODULE__, _, atom}, env) when is_atom(atom), do: {env.module, true}

  defp do_expand_once({:__DIR__, _, atom}, env) when is_atom(atom),
    do: {:filename.dirname(env.file), true}

  defp do_expand_once({:__ENV__, _, atom}, env) when is_atom(atom) and env.context != :match do
    env = update_in(env.versioned_vars, &maybe_escape_map/1)
    {maybe_escape_map(env), true}
  end

  defp do_expand_once({{:., _, [{:__ENV__, _, atom}, field]}, _, []} = original, env)
       when is_atom(atom) and is_atom(field) and env.context != :match do
    if Map.has_key?(env, field) do
      {maybe_escape_map(Map.get(env, field)), true}
    else
      {original, false}
    end
  end

  defp do_expand_once({name, meta, context} = original, _env)
       when is_atom(name) and is_list(meta) and is_atom(context) do
    {original, false}
  end

  defp do_expand_once({name, meta, args} = original, env)
       when is_atom(name) and is_list(args) and is_list(meta) do
    arity = length(args)

    case NormalizedMacroEnv.expand_import(env, meta, name, arity,
           trace: false,
           check_deprecations: false
         ) do
      {:macro, _receiver, expander} ->
        # TODO register call
        # We don't want the line to propagate yet, but generated might!
        {expander.(Keyword.take(meta, [:generated]), args), true}

      {:function, Kernel, op} when op in [:+, :-] and arity == 1 ->
        case expand_once(hd(args), env) do
          integer when is_integer(integer) ->
            # TODO register call
            {apply(Kernel, op, [integer]), true}

          _ ->
            {original, false}
        end

      {:function, _receiver, _name} ->
        {original, false}

      {:error, :not_found} ->
        {original, false}

      {:error, _other} ->
        # elixir raises elixir_dispatch here
        {original, false}
    end
  end

  # Expand possible macro require invocation
  defp do_expand_once({{:., _, [left, name]}, meta, args} = original, env) when is_atom(name) do
    {receiver, _} = do_expand_once(left, env)

    case is_atom(receiver) do
      false ->
        {original, false}

      true ->
        case NormalizedMacroEnv.expand_require(env, meta, receiver, name, length(args),
               trace: false,
               check_deprecations: false
             ) do
          {:macro, _receiver, expander} ->
            # TODO register call
            # We don't want the line to propagate yet, but generated might!
            {expander.(Keyword.take(meta, [:generated]), args), true}

          :error ->
            {original, false}
        end
    end
  end

  # Anything else is just returned
  defp do_expand_once(other, _env), do: {other, false}

  defp maybe_escape_map(map) when is_map(map), do: {:%{}, [], Map.to_list(map)}
  defp maybe_escape_map(other), do: other

  @spec escape(term, term, keyword) :: Macro.t()
  def escape(expr, state, opts \\ []) do
    unquote = Keyword.get(opts, :unquote, false)
    kind = if Keyword.get(opts, :prune_metadata, false), do: :prune_metadata, else: :none
    ElixirSense.Core.Compiler.Quote.escape(expr, kind, unquote, state)
  end
end
