defmodule ElixirSense.Providers.Expand do
  @moduledoc """
  Provider responsible for code expansion features.
  """

  alias ElixirSense.Core.Ast
  alias ElixirSense.Core.State

  @type expanded_code_map :: %{
          expand_once: String.t(),
          expand: String.t(),
          expand_partial: String.t(),
          expand_all: String.t()
        }

  @doc """
  Returns a map containing the results of all different code expansion methods
  available (expand_once, expand, expand_partial and expand_all).
  """
  @spec expand_full(String.t(), State.Env.t()) :: expanded_code_map
  def expand_full(code, %State.Env{requires: requires, imports: imports, module: module}) do
    env =
      %Macro.Env{macros: __ENV__.macros}
      |> Ast.set_module_for_env(module)
      |> Ast.add_requires_to_env(requires)
      |> Ast.add_imports_to_env(imports)

    try do
      {:ok, expr} = code |> Code.string_to_quoted()

      # Elixir require some meta to expand ast
      expr = Ast.add_default_meta(expr)

      %{
        expand_once: expr |> Macro.expand_once(env) |> Macro.to_string(),
        expand: expr |> Macro.expand(env) |> Macro.to_string(),
        expand_partial: expr |> Ast.expand_partial(env) |> Macro.to_string(),
        expand_all: expr |> Ast.expand_all(env) |> Macro.to_string()
      }
    rescue
      e ->
        message = inspect(e)

        %{
          expand_once: message,
          expand: message,
          expand_partial: message,
          expand_all: message
        }
    end
  end
end
