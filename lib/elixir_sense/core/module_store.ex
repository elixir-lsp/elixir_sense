defmodule ElixirSense.Core.ModuleStore do
  defstruct by_behaviour: %{}, list: []

  def ensure_compiled(context, module_or_modules) do
    modules = List.wrap(module_or_modules)
    Enum.map(modules, &Code.ensure_compiled/1)

    Map.update!(context, :module_store, &build(modules, &1))
  end

  def build(list \\ all_loaded(), module_store \\ %__MODULE__{}) do
    Enum.reduce(list, module_store, fn module, module_store ->
      module_store = %{module_store | list: [module | module_store.list]}

      module.module_info(:attributes)[:behaviour]
      |> List.wrap()
      |> Enum.reduce(module_store, &add_behaviour(module, &1, &2))
    end)
  end

  defp all_loaded do
    for {module, _} <- :code.all_loaded(), match?({:module, _}, Code.ensure_compiled(module)) do
      module
    end
  end

  defp add_behaviour(adopter, behaviour, module_store) do
    new_by_behaviour =
      module_store.by_behaviour
      |> Map.put_new_lazy(behaviour, fn -> MapSet.new() end)
      |> Map.update!(behaviour, &MapSet.put(&1, adopter))

    %{module_store | by_behaviour: new_by_behaviour}
  end
end
