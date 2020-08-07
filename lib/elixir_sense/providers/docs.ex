defmodule ElixirSense.Providers.Docs do
  @moduledoc """
  Doc Provider
  """
  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State

  @spec all(String.t(), State.Env.t(), State.mods_funs_to_positions_t(), State.types_t()) ::
          {actual_mod_fun :: String.t(), docs :: Introspection.docs()}
  def all(
        subject,
        %State.Env{
          imports: imports,
          aliases: aliases,
          module: module,
          scope: scope,
          attributes: attributes,
          vars: vars
        },
        mods_funs,
        metadata_types
      ) do
    binding_env = %Binding{
      attributes: attributes,
      variables: vars,
      current_module: module
    }

    {mod, fun, _found} =
      subject
      |> Source.split_module_and_func(module, aliases)
      |> expand(binding_env, aliases)
      |> Introspection.actual_mod_fun(imports, aliases, module, mods_funs, metadata_types)

    {mod_fun_to_string({mod, fun}), Introspection.get_all_docs({mod, fun}, scope)}
  end

  defp expand({{:attribute, _} = type, func}, env, aliases) do
    case Binding.expand(env, type) do
      {:atom, module} -> {Introspection.expand_alias(module, aliases), func}
      _ -> {nil, nil}
    end
  end

  defp expand({type, func}, _env, _aliases) do
    {type, func}
  end

  defp mod_fun_to_string({nil, nil}), do: ""

  defp mod_fun_to_string({nil, fun}) do
    Atom.to_string(fun)
  end

  defp mod_fun_to_string({mod, nil}) do
    inspect(mod)
  end

  defp mod_fun_to_string({mod, fun}) do
    inspect(mod) <> "." <> Atom.to_string(fun)
  end
end
