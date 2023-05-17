defmodule ElixirSense.Providers.Suggestion.Reducers.Params do
  @moduledoc false

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State
  alias ElixirSense.Core.TypeInfo
  alias ElixirSense.Providers.Suggestion.Matcher

  @type param_option :: %{
          type: :param_option,
          name: String.t(),
          origin: String.t(),
          type_spec: String.t(),
          doc: String.t(),
          expanded_spec: String.t()
        }

  @doc """
  A reducer that adds suggestions of keyword list options.
  """
  def add_options(hint, env, buffer_metadata, cursor_context, acc) do
    prefix = cursor_context.text_before

    %State.Env{
      imports: imports,
      requires: requires,
      aliases: aliases,
      module: module,
      attributes: attributes,
      vars: vars,
      scope: scope
    } = env

    binding_env = %Binding{
      attributes: attributes,
      variables: vars,
      current_module: module
    }

    %Metadata{mods_funs_to_positions: mods_funs, types: metadata_types} = buffer_metadata

    with %{
           candidate: {mod, fun},
           elixir_prefix: elixir_prefix,
           npar: npar
         } <-
           Source.which_func(prefix, binding_env),
         {mod, fun, true} <-
           Introspection.actual_mod_fun(
             {mod, fun},
             imports,
             requires,
             if(elixir_prefix, do: [], else: aliases),
             module,
             scope,
             mods_funs,
             metadata_types
           ) do
      list =
        mod
        |> TypeInfo.extract_param_options(fun, npar)
        |> options_to_suggestions(mod)
        |> Enum.filter(&Matcher.match?(&1.name, hint))

      {:cont, %{acc | result: acc.result ++ list}}
    else
      _ ->
        {:cont, acc}
    end
  end

  defp options_to_suggestions(options, original_module) do
    Enum.map(options, fn
      {mod, name, type} ->
        TypeInfo.get_type_info(mod, type, original_module)
        |> Map.merge(%{type: :param_option, name: name |> Atom.to_string()})

      {mod, name} ->
        %{
          doc: "",
          expanded_spec: "",
          name: name |> Atom.to_string(),
          origin: inspect(mod),
          type: :param_option,
          type_spec: ""
        }
    end)
  end
end
