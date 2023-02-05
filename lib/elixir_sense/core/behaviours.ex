defmodule ElixirSense.Core.Behaviours do
  alias ElixirSense.Core.Applications

  @spec get_module_behaviours(module) :: [module]
  def get_module_behaviours(module) do
    if Code.ensure_loaded?(module) do
      module.module_info(:attributes) |> Keyword.get_values(:behaviour) |> Enum.concat()
    else
      []
    end
  end

  @spec get_all_behaviour_implementations(module) :: [module]
  def get_all_behaviour_implementations(behaviour) do
    # this function can take a few seconds
    # unfortunately it does not benefit from conversion to Task.async_stream
    # at least on otp 23

    # TODO consider changing this to :code.all_available when otp 23 is required
    all_loaded =
      :code.all_loaded()
      |> Enum.map(&(&1 |> elem(0)))

    from_apps =
      case :code.get_mode() do
        :interactive ->
          Applications.get_modules_from_applications()

        _ ->
          []
      end

    (all_loaded ++ from_apps)
    |> Enum.uniq()
    |> Enum.filter(fn mod ->
      Code.ensure_loaded?(mod) and
        behaviour in Enum.flat_map(mod.module_info(:attributes), fn
          {:behaviour, behaviours} when is_list(behaviours) ->
            behaviours

          _ ->
            []
        end)
    end)
  end
end
