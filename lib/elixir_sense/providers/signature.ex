defmodule ElixirSense.Providers.Signature do
  @moduledoc """
  Provider responsible for introspection information about function signatures.
  """

  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State
  alias ElixirSense.Core.Metadata

  @type signature :: %{name: String.t(), params: [String.t()]}
  @type signature_info :: %{active_param: pos_integer, signatures: [signature]} | :none

  @doc """
  Returns the signature info from the function or type defined in the prefix, if any.
  """
  @spec find(String.t(), State.Env.t(), Metadata.t()) :: signature_info
  def find(prefix, %State.Env{imports: imports, aliases: aliases, module: module}, metadata) do
    with %{candidate: {mod, fun}, npar: npar, pipe_before: pipe_before} <-
           Source.which_func(prefix, module),
         {mod, fun, true} <-
           Introspection.actual_mod_fun(
             {mod, fun},
             imports,
             aliases,
             module,
             metadata.mods_funs_to_positions,
             metadata.types
           ) do
      signatures = find_signatures({mod, fun}, metadata)
      %{active_param: npar, pipe_before: pipe_before, signatures: signatures}
    else
      _ ->
        :none
    end
  end

  defp find_signatures({mod, fun}, metadata) do
    find_function_signatures({mod, fun}, metadata) ++ find_type_signatures({mod, fun}, metadata)
  end

  defp find_function_signatures({mod, fun}, metadata) do
    docs = NormalizedCode.get_docs(mod, :docs)

    signatures =
      case Metadata.get_function_signatures(metadata, mod, fun, docs) do
        [] -> Introspection.get_signatures(mod, fun, docs)
        signatures -> signatures
      end

    signatures |> Enum.uniq_by(fn sig -> sig.params end)
  end

  defp find_type_signatures({mod, fun}, metadata) do
    docs = NormalizedCode.get_docs(mod, :type_docs)

    case Metadata.get_type_signatures(metadata, mod, fun, docs) do
      [] -> ElixirSense.Core.TypeInfo.get_signatures(mod, fun, docs)
      signature -> signature
    end
  end
end
