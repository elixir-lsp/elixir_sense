defmodule ElixirSense.Providers.Docs do
  @moduledoc """
  Doc Provider
  """
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.State
  alias ElixirSense.Core.Source

  @spec all(String.t, [module], [{module, module}], module, State.scope) :: {actual_mod_fun :: String.t, docs :: Introspection.docs}
  def all(subject, imports, aliases, module, scope) do
    mod_fun =
      subject
      |> Source.split_module_and_func(aliases)
      |> Introspection.actual_mod_fun(imports, aliases, module)
    {mod_fun_to_string(mod_fun), Introspection.get_all_docs(mod_fun, scope)}
  end

  defp mod_fun_to_string({nil, fun}) do
    Atom.to_string(fun)
  end

  defp mod_fun_to_string({mod, nil}) do
    Introspection.module_to_string(mod)
  end

  defp mod_fun_to_string({mod, fun}) do
    Introspection.module_to_string(mod) <> "." <> Atom.to_string(fun)
  end

end
