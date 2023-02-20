defmodule ElixirSense.Providers.Suggestion.Reducers.Struct do
  @moduledoc false

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State
  alias ElixirSense.Providers.Suggestion.Matcher

  @type field :: %{
          type: :field,
          subtype: :struct_field | :map_key,
          name: String.t(),
          origin: String.t() | nil,
          call?: boolean
        }

  @doc """
  A reducer that adds suggestions of struct fields.
  """
  def add_fields(hint, env, buffer_metadata, context, acc) do
    text_before = context.text_before

    case find_struct_fields(hint, text_before, env, buffer_metadata) do
      {[], _} ->
        {:cont, acc}

      {fields, nil} ->
        {:halt, %{acc | result: fields}}

      {fields, :maybe_struct_update} ->
        reducers = [:populate_common, :modules, :functions, :macros, :variables, :attributes]
        {:cont, %{acc | result: fields, reducers: reducers}}
    end
  end

  defp find_struct_fields(hint, text_before, env, buffer_metadata) do
    %State.Env{
      module: module,
      vars: vars,
      attributes: attributes,
      imports: imports,
      aliases: aliases
    } = env

    %Metadata{
      structs: structs,
      mods_funs_to_positions: mods_funs,
      types: metadata_types,
      specs: specs
    } = buffer_metadata

    env = %ElixirSense.Core.Binding{
      attributes: attributes,
      variables: vars,
      structs: structs,
      imports: imports,
      current_module: module,
      specs: specs,
      types: metadata_types,
      mods_and_funs: mods_funs
    }

    case Source.which_struct(text_before, module) do
      {type, fields_so_far, elixir_prefix, var} ->
        type =
          case {type, elixir_prefix} do
            {{:atom, mod}, false} ->
              # which_struct returns not expamded aliases
              {:atom, Introspection.expand_alias(mod, aliases)}

            _ ->
              type
          end

        type = Binding.expand(env, {:struct, [], type, var})

        result = get_fields(type, hint, fields_so_far)
        {result, if(fields_so_far == [], do: :maybe_struct_update)}

      {:map, fields_so_far, var} ->
        var = Binding.expand(env, var)

        result = get_fields(var, hint, fields_so_far)
        {result, if(fields_so_far == [], do: :maybe_struct_update)}

      _ ->
        {[], nil}
    end
  end

  defp get_fields({:map, fields, _}, hint, fields_so_far) do
    expand_map_field_access(fields, hint, :map, fields_so_far)
  end

  defp get_fields({:struct, fields, type, _}, hint, fields_so_far) do
    expand_map_field_access(fields, hint, {:struct, type}, fields_so_far)
  end

  defp get_fields(_, _hint, _fields_so_far), do: []

  defp expand_map_field_access(fields, hint, type, fields_so_far) do
    {subtype, origin} =
      case type do
        {:struct, mod} -> {:struct_field, if(mod, do: inspect(mod))}
        :map -> {:map_key, nil}
      end

    for {key, _value} when is_atom(key) <- fields,
        key not in fields_so_far,
        key = Atom.to_string(key),
        Matcher.match?(key, hint) do
      %{type: :field, name: key, subtype: subtype, origin: origin, call?: false}
    end
    |> Enum.sort_by(& &1.name)
  end
end
