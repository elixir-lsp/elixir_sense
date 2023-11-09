defmodule ElixirSense.Plugins.Phoenix.Scope do
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.Binding

  import Module, only: [safe_concat: 2, safe_concat: 1]

  def within_scope(buffer, binding_env \\ %Binding{}) do
    {:ok, ast} = Code.Fragment.container_cursor_to_quoted(buffer)

    with {true, scopes_ast} <- get_scopes(ast),
         scopes_ast = Enum.reverse(scopes_ast),
         scope_alias <- get_scope_alias(scopes_ast, binding_env) do
      {true, scope_alias}
    end
  end

  defp get_scopes(ast) do
    path = Macro.path(ast, &match?({:__cursor__, _, _}, &1))

    scopes =
      path
      |> Enum.filter(&match?({:scope, _, _}, &1))
      |> Enum.map(fn {:scope, meta, params} ->
        # drop `do` block from params
        params = Enum.filter(params, &(not match?([{:do, _} | _], &1)))
        {:scope, meta, params}
      end)

    case scopes do
      [] -> {false, nil}
      scopes -> {true, scopes}
    end
  end

  defp get_scope_alias(scopes_ast, binding_env, module \\ nil)

  # is this possible? scope do ... end
  defp get_scope_alias([{:scope, _, []}], _binding_env, module), do: module

  # scope "/" do ... end
  defp get_scope_alias([{:scope, _, [scope_params]}], _binding_env, module)
       when not is_list(scope_params),
       do: module

  # scope path: "/", alias: ExampleWeb do ... end
  defp get_scope_alias([{:scope, _, [scope_params]}], binding_env, module)
       when is_list(scope_params) do
    scope_alias = Keyword.get(scope_params, :alias)
    scope_alias = get_mod(scope_alias, binding_env)
    safe_concat(module, scope_alias)
  end

  # scope "/", alias: ExampleWeb do ... end
  defp get_scope_alias(
         [{:scope, _, [_scope_path, scope_params]}],
         binding_env,
         module
       )
       when is_list(scope_params) do
    scope_alias = Keyword.get(scope_params, :alias)
    scope_alias = get_mod(scope_alias, binding_env)
    safe_concat(module, scope_alias)
  end

  # scope "/", ExampleWeb do ... end
  defp get_scope_alias(
         [{:scope, _, [_scope_path, scope_alias]}],
         binding_env,
         module
       ) do
    scope_alias = get_mod(scope_alias, binding_env)
    safe_concat(module, scope_alias)
  end

  # scope "/", ExampleWeb, host: "api." do ... end
  defp get_scope_alias(
         [{:scope, _, [_scope_path, scope_alias, _scope_params]}],
         binding_env,
         module
       ) do
    scope_alias = get_mod(scope_alias, binding_env)
    safe_concat(module, scope_alias)
  end

  # recurse
  defp get_scope_alias([head | tail], binding_env, module) do
    scope_alias = get_scope_alias([head], binding_env, module)
    safe_concat([module, scope_alias, get_scope_alias(tail, binding_env)])
  end

  defp get_mod(scope_alias, binding_env) do
    case scope_alias do
      {:__aliases__, _, [scope_alias]} ->
        scope_alias

      _ ->
        Source.get_mod([scope_alias], binding_env)
    end
  end
end
