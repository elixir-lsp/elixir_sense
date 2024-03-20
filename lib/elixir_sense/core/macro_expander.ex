defmodule ElixirSense.Core.MacroExpander do
  @moduledoc false

  def add_default_meta(expr) do
    Macro.update_meta(expr, fn keyword ->
      Keyword.merge(keyword, context: Elixir, import: Kernel)
    end)
  end

  def expand_use(ast, module, current_aliases, meta) do
    env = %Macro.Env{
      module: module,
      function: nil,
      aliases: current_aliases,
      macros: __ENV__.macros
    }

    {use_expanded, _env} = Macro.prewalk(ast, env, &require_and_expand/2)
    {use_expanded_with_meta, _meta} = Macro.prewalk(use_expanded, meta, &append_meta/2)
    use_expanded_with_meta
  end

  defp require_and_expand({:require, _, _} = ast, env) do
    {env_after_require, _binding} = Code.eval_string("#{Macro.to_string(ast)}; __ENV__", [], env)
    {ast, env_after_require}
  end

  if Version.match?(System.version(), ">= 1.17.0-dev") do
    defp require_and_expand({:use, meta, args} = ast, env) when is_list(meta) and is_list(args) do
      case Macro.Env.expand_import(env, meta, :use, length(args), trace: false) |> dbg do
        {:macro, _receiver, expander} ->
          {expander.(Keyword.take(meta, [:generated]), args), env}
        _ ->
          {ast, env}
      end
    end
  else
    defp require_and_expand({:use, meta, arg}, env) do
      use_directive_expanded = Macro.expand_once({:use, meta, arg}, env)
      {use_directive_expanded, env}
    end
  end

  if Version.match?(System.version(), ">= 1.17.0-dev") do
    defp require_and_expand({{:., meta1, [left, :__using__]}, meta, args} = ast, env) when is_list(meta) and is_list(args) do
      # TODO expand receiver and check if it is an atom
      receiver = left
      case Macro.Env.expand_require(env, meta, receiver, :__using__, length(args), trace: false) |> dbg do
        {:macro, _receiver, expander} ->
          {expander.(Keyword.take(meta, [:generated]), args), env}
        _ ->
          {ast, env}
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
end
