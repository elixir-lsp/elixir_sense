defmodule ElixirSense.Core.Struct do
  @moduledoc false

  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.State

  @spec is_struct(module | nil, ElixirSense.Core.Compiler.State.structs_t()) :: boolean
  def is_struct(nil, _metadata_structs), do: false

  def is_struct(module, metadata_structs) do
    Map.has_key?(metadata_structs, module) or Introspection.module_is_struct?(module)
  end

  @spec get_fields(module, ElixirSense.Core.Compiler.State.structs_t()) :: [atom]
  def get_fields(module, metadata_structs) do
    case metadata_structs[module] do
      %State.StructInfo{fields: fields} ->
        Enum.map(fields, &(&1 |> elem(0)))

      nil ->
        try do
          module
          |> struct()
          |> Map.from_struct()
          |> Map.keys()
          |> Kernel.++([:__struct__])
        rescue
          _ ->
            # return dummy in case struct/1 fails
            [:__struct__]
        end
    end
  end
end
