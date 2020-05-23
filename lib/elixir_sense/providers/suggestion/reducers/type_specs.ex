defmodule ElixirSense.Providers.Suggestion.Reducers.TypeSpecs do
  @moduledoc false

  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State
  alias ElixirSense.Core.TypeInfo

  @type type_spec :: %{
          type: :type_spec,
          name: String.t(),
          arity: non_neg_integer,
          origin: String.t(),
          spec: String.t(),
          doc: String.t(),
          signature: String.t(),
          metadata: map
        }

  @doc """
  A reducer that adds suggestions of types.
  """
  # We don't list typespecs when inside a function
  def add_types(_hint, _text_before, %State.Env{scope: {_m, _f}}, _buffer_metadata, acc) do
    {:cont, acc}
  end

  # We don't list typespecs outside of a module
  def add_types(_hint, _text_before, %State.Env{scope: scope}, _buffer_metadata, acc)
      when scope in [Elixir, nil] do
    {:cont, acc}
  end

  # We don't list typespecs when the hint is most likely an attribute
  def add_types("@" <> _, _text_before, _env, _buffer_metadata, acc) do
    {:cont, acc}
  end

  def add_types(hint, _text_before, env, file_metadata, acc) do
    %State.Env{aliases: aliases, module: module} = env
    %Metadata{mods_funs_to_positions: mods_and_funs, types: metadata_types} = file_metadata

    {mod, hint} =
      hint
      |> Source.split_module_and_hint(module, aliases)

    list =
      find_typespecs_for_mod_and_hint({mod, hint}, aliases, module, mods_and_funs, metadata_types)
      |> Kernel.++(find_builtin_types({mod, hint}))

    {:cont, %{acc | result: acc.result ++ list}}
  end

  defp find_typespecs_for_mod_and_hint(
         {mod, hint},
         aliases,
         module,
         mods_and_funs,
         metadata_types
       ) do
    case Introspection.actual_module(mod, aliases, module, mods_and_funs) do
      {actual_mod, true} ->
        find_module_types(actual_mod, {mod, hint}, metadata_types, module)

      {nil, false} ->
        find_module_types(module, {mod, hint}, metadata_types, module)

      {_, false} ->
        []
    end
  end

  defp find_builtin_types({nil, hint}) do
    TypeInfo.find_all_builtin(&String.starts_with?("#{&1.name}", hint))
    |> Enum.map(&type_info_to_suggestion(&1, nil))
  end

  defp find_builtin_types({_mod, _hint}), do: []

  defp find_module_types(actual_mod, {mod, hint}, metadata_types, module) do
    find_metadata_types(actual_mod, {mod, hint}, metadata_types, module)
    |> Kernel.++(TypeInfo.find_all(actual_mod, &String.starts_with?("#{&1.name}", hint)))
    |> Enum.map(&type_info_to_suggestion(&1, actual_mod))
  end

  defp find_metadata_types(actual_mod, {mod, hint}, metadata_types, module) do
    include_private = mod == nil and actual_mod == module

    for {{mod, type, arity}, type_info} when is_integer(arity) <- metadata_types,
        mod == actual_mod,
        type |> Atom.to_string() |> String.starts_with?(hint),
        include_private or type_info.kind != :typep,
        do: type_info
  end

  defp type_info_to_suggestion(type_info, module) do
    origin = if module, do: inspect(module), else: ""

    case type_info do
      %ElixirSense.Core.State.TypeInfo{args: [args]} ->
        args_stringified = Enum.join(args, ", ")

        %{
          type: :type_spec,
          name: type_info.name |> Atom.to_string(),
          arity: length(args),
          signature: "#{type_info.name}(#{args_stringified})",
          origin: origin,
          doc: "",
          spec: "",
          # TODO extract doc and meta
          metadata: %{}
        }

      _ ->
        %{
          type: :type_spec,
          name: type_info.name |> Atom.to_string(),
          arity: type_info.arity,
          signature: type_info.signature,
          origin: origin,
          doc: type_info.doc,
          spec: type_info.spec,
          metadata: type_info.metadata
        }
    end
  end
end
