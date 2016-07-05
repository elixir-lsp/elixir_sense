defmodule ElixirSense.Providers.Signature do

  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Source
  alias Alchemist.Helpers.ModuleInfo

  @type signature :: %{name: String.t, params: [String.t]}
  @type signature_info :: %{active_parameter: pos_integer, signatures: [signature]} | :none

  @spec find(String.t, [module], [{module, module}]) :: signature_info
  def find(prefix, imports, aliases) do
    case prefix |> Source.which_func do
      {mod, func, npar} ->
        {mod, func} = real_mod_fun({mod, func}, imports, aliases)
        %{active_parameter: npar, signatures: Introspection.get_signatures(mod, func)}
      :none ->
        :none
    end
  end

  defp real_mod_fun({nil, function}, [], []) do
    look_for_kernel_functions(function)
  end

  defp real_mod_fun({nil, function}, imports, _) do
    module = Enum.filter(imports, &ModuleInfo.has_function?(&1, function))
    |> List.first

    case module do
      nil -> look_for_kernel_functions(function)
      _   -> {module, function}
    end
  end

  defp real_mod_fun({module, function}, _, aliases) do
    mod =
      if elixir_module?(module) do
        module
        |> Module.split
        |> ModuleInfo.expand_alias(aliases)
      else
        module
      end
    {mod, function}
  end

  defp look_for_kernel_functions(function) do
    cond do
      ModuleInfo.docs?(Kernel, function) ->
        {Kernel, function}
      ModuleInfo.docs?(Kernel.SpecialForms, function) ->
        {Kernel.SpecialForms, function}
      true -> {nil, nil}
    end
  end

  defp elixir_module?(module) do
    module == Module.concat(Elixir, module)
  end

end
