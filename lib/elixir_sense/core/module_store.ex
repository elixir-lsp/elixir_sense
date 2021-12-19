defmodule ElixirSense.Core.ModuleStore do
  @moduledoc """
  Caches the module list and a list of modules keyed by the behaviour they implement.
  """
  defstruct by_behaviour: %{}, list: []

  @type t :: %__MODULE__{by_behaviour: %{optional(atom) => module}, list: list(module)}

  alias ElixirSense.Core.Applications

  def ensure_compiled(context, module_or_modules) do
    modules = List.wrap(module_or_modules)
    Enum.each(modules, &Code.ensure_compiled/1)

    Map.update!(context, :module_store, &build(modules, &1))
  end

  def build(list \\ all_loaded(), module_store \\ %__MODULE__{}) do
    Enum.reduce(list, module_store, fn module, module_store ->
      try do
        module_store = %{module_store | list: [module | module_store.list]}

        module.module_info(:attributes)[:behaviour]
        |> List.wrap()
        |> Enum.reduce(module_store, &add_behaviour(module, &1, &2))
      rescue
        _ ->
          module_store
      end
    end)
  end

  defp all_loaded do
    Applications.get_modules_from_applications()
    |> Enum.filter(fn module ->
      try do
        _ = Code.ensure_compiled(module)
        function_exported?(module, :module_info, 0)
      rescue
        _ ->
          false
      end
    end)
  end

  defp add_behaviour(adopter, behaviour, module_store) do
    new_by_behaviour =
      module_store.by_behaviour
      |> Map.put_new_lazy(behaviour, fn -> MapSet.new() end)
      |> Map.update!(behaviour, &MapSet.put(&1, adopter))

    %{module_store | by_behaviour: new_by_behaviour}
  end
end
