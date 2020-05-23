defmodule ElixirSense.Providers.Suggestion.Reducers.Params do
  @moduledoc false

  alias ElixirSense.Core.{Introspection, State, Source, TypeInfo, Metadata}

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
  def add_options(hint, prefix, env, buffer_metadata, acc) do
    %State.Env{imports: imports, aliases: aliases, module: module} = env
    %Metadata{mods_funs_to_positions: mods_funs, types: metadata_types} = buffer_metadata

    with %{
           candidate: {mod, fun},
           elixir_prefix: elixir_prefix,
           npar: npar,
           pipe_before: _pipe_before
         } <-
           Source.which_func(prefix, module),
         {mod, fun, true} <-
           Introspection.actual_mod_fun(
             {mod, fun},
             imports,
             if(elixir_prefix, do: [], else: aliases),
             module,
             mods_funs,
             metadata_types
           ) do
      list =
        mod
        |> TypeInfo.extract_param_options(fun, npar)
        |> options_to_suggestions(mod)
        |> Enum.filter(&String.starts_with?(&1.name, hint))
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
