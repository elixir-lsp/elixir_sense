defmodule ElixirSense.Providers.Signature do

  @moduledoc """
  Provider responsible for introspection information about function signatures.
  """

  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.Metadata

  @type signature :: %{name: String.t, params: [String.t]}
  @type signature_info :: %{active_param: pos_integer, signatures: [signature]} | :none

  @doc """
  Returns the signature info from the function defined in the prefix, if any.

  ## Examples

      iex> Signature.find("MyList.flatten(par0, par1, ", [], [{MyList, List}], MyModule, %ElixirSense.Core.Metadata{})
      %{active_param: 2,
        pipe_before: false,
        signatures: [
          %{name: "flatten", params: ["list"]},
          %{name: "flatten", params: ["list", "tail"]}]}

  """
  @spec find(String.t, [module], [{module, module}], module, map) :: signature_info
  def find(prefix, imports, aliases, module, metadata) do
    case Source.which_func(prefix) do
      %{candidate: {mod, fun}, npar: npar, pipe_before: pipe_before} ->
        %{active_param: npar, pipe_before: pipe_before, signatures: find_signatures({mod, fun}, imports, aliases, module, metadata)}
      _ ->
        :none
    end
  end

  defp find_signatures(mod_fun, imports, aliases, module, metadata) do
    {mod, fun} = Introspection.actual_mod_fun(mod_fun, imports, aliases, module)

    case Metadata.get_function_signatures(metadata, mod, fun) do
      [] -> Introspection.get_signatures(mod, fun)
      signatures -> signatures
    end |> Enum.uniq_by(fn sig -> sig.params end)
  end

end
