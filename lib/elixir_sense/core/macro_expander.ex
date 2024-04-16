defmodule ElixirSense.Core.MacroExpander do
  @moduledoc false

  def add_default_meta(expr) do
    Macro.update_meta(expr, fn keyword ->
      Keyword.merge(keyword, context: Elixir, import: Kernel)
    end)
  end

  def expand_use(ast, env, meta) do
    # env = %Macro.Env{
    #   module: module,
    #   function: nil,
    #   aliases: current_aliases,
    #   requires: requires,
    #   macros: __ENV__.macros
    # }

    {use_expanded, _env} =
      Macro.prewalk(ast, env, fn ast, env ->
        require_and_expand(ast, env)
      end)

    {use_expanded_with_meta, _meta} = Macro.prewalk(use_expanded, meta, &append_meta/2)
    use_expanded_with_meta
  end

  defp require_and_expand({:require, _, _} = ast, env) do
    {env_after_require, _binding} = Code.eval_string("#{Macro.to_string(ast)}; __ENV__", [], env)
    {ast, env_after_require}
  end

  if Version.match?(System.version(), ">= 1.17.0-dev") do
    defp require_and_expand({atom, meta, args} = ast, env)
         when is_list(meta) and is_list(args) and atom in [:use, :schema] do
      # case Macro.Env.expand_import(env, meta, :use, length(args), trace: false) |> dbg do
      #   {:macro, _receiver, expander} ->
      #     {expander.(Keyword.take(meta, [:generated]), args), env}
      #   _ ->
      #     {ast, env}
      # end
      use_directive_expanded = expand_once(ast, env)

      if use_directive_expanded != ast do
        if atom == :schema do
          dbg(env)
          dbg(use_directive_expanded)
        end

        {{:__block__, [], [use_directive_expanded]}, env}
      else
        if atom == :schema do
          dbg(env)
          dbg(use_directive_expanded)
        end

        {[], env}
      end

      # {use_directive_expanded, env}
    end
  else
    defp require_and_expand({:use, meta, arg}, env) do
      use_directive_expanded = Macro.expand_once({:use, meta, arg}, env)
      {use_directive_expanded, env}
    end
  end

  if Version.match?(System.version(), ">= 1.17.0-dev") do
    defp require_and_expand({{:., meta1, [left, :__using__]}, meta, args} = _ast, env)
         when is_list(meta) and is_list(args) do
      # receiver = expand_once(left, env)
      # if is_atom(receiver) do
      #   case Macro.Env.expand_require(env, meta, receiver, :__using__, length(args), trace: false) do
      #     {:macro, _receiver, expander} ->
      #       {expander.(Keyword.take(meta, [:generated]), args), env}
      #     _ ->
      #       {ast, env}
      #   end
      # else
      #   {ast, env}
      # end
      splitted =
        Module.split(left)
        |> Enum.map(&String.to_atom/1)

      module_expanded = expand_once({:__aliases__, [], splitted}, env)
      ast_with_module_expanded = {{:., meta1, [module_expanded, :__using__]}, meta, args}
      # ast |> dbg
      ast_expanded = expand_once(ast_with_module_expanded, env)

      if ast_with_module_expanded != ast_expanded do
        {{:__block__, [], [ast_expanded]}, env}
      else
        {[], env}
      end
    end
  else
    defp require_and_expand({{:., meta1, [module, :__using__]}, meta2, params}, env)
         when is_atom(module) do
      splitted =
        Module.split(module)
        |> Enum.map(&String.to_atom/1)

      module_expanded = Macro.expand_once({:__aliases__, [], splitted}, env)
      ast_with_module_expanded = {{:., meta1, [module_expanded, :__using__]}, meta2, params}
      ast_expanded = Macro.expand_once(ast_with_module_expanded, env)

      if ast_with_module_expanded != ast_expanded do
        {{:__block__, [], [ast_expanded]}, env}
      else
        {[], env}
      end
    end
  end

  defp require_and_expand(ast, env) do
    {ast, env}
  end

  defp append_meta({:defoverridable, ast_meta, args}, meta) when is_list(ast_meta) do
    {{:defoverridable, Keyword.merge(ast_meta, meta), args}, meta}
  end

  defp append_meta({:__aliases__, ast_meta, args}, meta) when is_list(ast_meta) do
    new_args =
      case ast_meta[:alias] do
        false ->
          args

        nil ->
          args

        alias when is_atom(alias) ->
          Module.split(alias)
          |> Enum.map(&String.to_atom/1)
      end

    {{:__aliases__, meta, new_args}, meta}
  end

  defp append_meta({atom, ast_meta, args}, meta) when is_atom(atom) and is_list(ast_meta) do
    new_args =
      case args do
        atom when is_atom(atom) -> nil
        other -> other
      end

    {{atom, meta, new_args}, meta}
  end

  defp append_meta(other, meta) do
    {other, meta}
  end

  if Version.match?(System.version(), ">= 1.17.0-dev") do
    @expand_opts [trace: false]

    @spec expand_once(Macro.input(), Macro.Env.t()) :: Macro.output()
    def expand_once(ast, env) do
      elem(do_expand_once(ast, env), 0)
    end

    defp do_expand_once({:__aliases__, meta, list} = alias, env) do
      case :elixir_aliases.expand_or_concat(meta, list, env, false) do
        receiver when is_atom(receiver) ->
          # :elixir_env.trace({:alias_reference, meta, receiver}, env)
          {receiver, true}

        [head | tail] ->
          {head, _} = do_expand_once(head, env)

          case is_atom(head) do
            true ->
              receiver = :elixir_aliases.concat([head | tail])
              # :elixir_env.trace({:alias_reference, meta, receiver}, env)
              {receiver, true}

            false ->
              {alias, false}
          end
      end
    end

    # Expand compilation environment macros
    defp do_expand_once({:__MODULE__, _, atom}, env) when is_atom(atom), do: {env.module, true}

    defp do_expand_once({:__DIR__, _, atom}, env) when is_atom(atom),
      do: {:filename.dirname(env.file), true}

    defp do_expand_once({:__ENV__, _, atom}, env) when is_atom(atom) do
      env = update_in(env.versioned_vars, &maybe_escape_map/1)
      {maybe_escape_map(env), true}
    end

    defp do_expand_once({{:., _, [{:__ENV__, _, atom}, field]}, _, []} = original, env)
         when is_atom(atom) and is_atom(field) do
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

      case Macro.Env.expand_import(env, meta, name, arity, @expand_opts) do
        {:macro, _receiver, expander} ->
          # We don't want the line to propagate yet, but generated might!
          {expander.(Keyword.take(meta, [:generated]), args), true}

        {:function, Kernel, op} when op in [:+, :-] and arity == 1 ->
          case expand_once(hd(args), env) do
            integer when is_integer(integer) -> {apply(Kernel, op, [integer]), true}
            _ -> {original, false}
          end

        {:function, _receiver, _name} ->
          {original, false}

        :error ->
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
          case Macro.Env.expand_require(env, meta, receiver, name, length(args), @expand_opts) do
            {:macro, _receiver, expander} ->
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
  end
end
