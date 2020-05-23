defmodule ElixirSense.Providers.Suggestion.Reducers.Struct do
  @moduledoc false

  alias ElixirSense.Core.{Source, Introspection, State, Struct, Metadata}

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
  def add_fields(hint, text_before, env, buffer_metadata, acc) do
    case find_struct_fields(hint, text_before, env, buffer_metadata) do
      {[], _} ->
        {:cont, acc}

      {fields, nil} ->
        {:halt, %{acc | result: fields}}

      {fields, :maybe_struct_update} ->
        {:cont, %{acc | result: fields, reducers: [:populate_common, :variables]}}
    end
  end

  defp find_struct_fields(hint, text_before, env, buffer_metadata) do
    %State.Env{module: module, vars: vars, attributes: attributes} = env

    %Metadata{
      structs: structs,
      mods_funs_to_positions: mods_funs,
      types: metadata_types
    } = buffer_metadata

    case Source.which_struct(text_before, module) do
      {{:attribute, attr}, fields_so_far, _elixir_prefix, _var} = struct ->
        mod =
          case Enum.find(attributes, fn %_{name: name} -> name == attr end) do
            %_{type: {:atom, mod}} -> mod
            _ -> nil
          end

        result =
          get_fields(
            env,
            mods_funs,
            metadata_types,
            structs,
            hint,
            struct |> put_elem(0, mod)
          )

        {result, if(fields_so_far == [], do: :maybe_struct_update)}

      {mod, fields_so_far, _elixir_prefix, _var} = struct when is_atom(mod) and mod != :_ ->
        result =
          get_fields(
            env,
            mods_funs,
            metadata_types,
            structs,
            hint,
            struct
          )

        {result, if(fields_so_far == [], do: :maybe_struct_update)}

      {:_, fields_so_far, false, _var} when is_list(fields_so_far) ->
        result =
          [:__struct__]
          |> Kernel.--(fields_so_far)
          |> Enum.filter(fn field -> String.starts_with?("#{field}", hint) end)
          |> Enum.map(fn field ->
            %{
              type: :field,
              subtype: :struct_field,
              name: Atom.to_string(field),
              origin: nil,
              call?: false
            }
          end)

        {result, if(fields_so_far == [], do: :maybe_struct_update)}

      {:map, fields_so_far, var_or_attr} when not is_nil(var_or_attr) ->
        result =
          case var_or_attr do
            {:variable, var} ->
              get_fields_from_var_or_attr(vars, structs, var, fields_so_far, hint)

            {:attribute, attr} ->
              get_fields_from_var_or_attr(attributes, structs, attr, fields_so_far, hint)
          end

        {result, if(fields_so_far == [], do: :maybe_struct_update)}

      _ ->
        {[], nil}
    end
  end

  defp get_fields(
         %State.Env{
           imports: imports,
           aliases: aliases,
           module: module,
           vars: vars,
           attributes: attributes
         },
         mods_funs,
         metadata_types,
         structs,
         hint,
         {mod, fields_so_far, elixir_prefix, var_or_attr}
       ) do
    with {actual_mod, _, true} <-
           Introspection.actual_mod_fun(
             {expand_current_module(mod, module), nil},
             imports,
             if(elixir_prefix, do: [], else: aliases),
             module,
             mods_funs,
             metadata_types
           ),
         true <- Struct.is_struct(actual_mod, structs) do
      fields = Struct.get_fields(actual_mod, structs)

      fields
      |> Kernel.--(fields_so_far)
      |> Enum.filter(fn field -> String.starts_with?("#{field}", hint) end)
      |> Enum.map(fn field ->
        %{
          type: :field,
          subtype: :struct_field,
          name: Atom.to_string(field),
          call?: false,
          origin: inspect(actual_mod)
        }
      end)
    else
      _ ->
        case var_or_attr do
          nil ->
            []

          {:variable, var} ->
            get_fields_from_var_or_attr(vars, structs, var, fields_so_far, hint)

          {:attribute, attr} ->
            get_fields_from_var_or_attr(attributes, structs, attr, fields_so_far, hint)
        end
    end
  end

  defp get_fields_from_var_or_attr(vars, structs, var, fields_so_far, hint) do
    case Enum.find(vars, fn %_{name: name} -> name == var end) do
      %_{type: {:map, fields}} ->
        for {field, _} <- fields,
            field not in fields_so_far,
            String.starts_with?("#{field}", hint) do
          %{
            type: :field,
            subtype: :map_key,
            name: Atom.to_string(field),
            origin: nil,
            call?: false
          }
        end

      %_{type: {:struct, fields, nil}} ->
        for {field, _} <- fields |> Keyword.put_new(:__struct__, nil),
            field not in fields_so_far,
            String.starts_with?("#{field}", hint) do
          %{
            type: :field,
            subtype: :struct_field,
            name: Atom.to_string(field),
            origin: nil,
            call?: false
          }
        end

      %_{type: {:struct, _fields, module}} ->
        if Struct.is_struct(module, structs) do
          for field <- Struct.get_fields(module, structs),
              field not in fields_so_far,
              String.starts_with?("#{field}", hint) do
            %{
              type: :field,
              subtype: :struct_field,
              name: Atom.to_string(field),
              origin: inspect(module),
              call?: false
            }
          end
        else
          []
        end

      _otherwise ->
        []
    end
  end

  defp expand_current_module(:__MODULE__, current_module), do: current_module
  defp expand_current_module(module, _current_module), do: module
end
