defmodule ElixirSense.Providers.Signature do
  @moduledoc """
  Provider responsible for introspection information about function signatures.
  """

  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State
  alias ElixirSense.Core.TypeInfo

  @type signature_info :: %{
          active_param: non_neg_integer,
          pipe_before: boolean,
          signatures: [Metadata.signature_t()]
        }

  @doc """
  Returns the signature info from the function or type defined in the prefix, if any.
  """
  @spec find(String.t(), State.Env.t(), Metadata.t()) :: signature_info | :none
  def find(prefix, env, metadata) do
    %State.Env{imports: imports, aliases: aliases, module: module} = env

    with %{
           candidate: {mod, fun},
           elixir_prefix: elixir_prefix,
           npar: npar,
           unfinished_parm: unfinished_parm,
           pipe_before: pipe_before
         } <-
           Source.which_func(prefix, module),
         {mod, fun, true} <-
           Introspection.actual_mod_fun(
             {mod, fun},
             imports,
             if(elixir_prefix, do: [], else: aliases),
             module,
             metadata.mods_funs_to_positions,
             metadata.types
           ) do
      signatures = find_signatures({mod, fun}, npar, unfinished_parm, env, metadata)
      %{active_param: npar, pipe_before: pipe_before, signatures: signatures}
    else
      _ ->
        :none
    end
  end

  defp find_signatures({mod, fun}, npar, unfinished_parm, env, metadata) do
    signatures = find_function_signatures({mod, fun}, env, metadata)

    signatures =
      if Metadata.at_module_body?(metadata, env) do
        signatures ++ find_type_signatures({mod, fun}, metadata)
      else
        signatures
      end

    signatures
    |> Enum.filter(fn %{params: params} ->
      params_length = length(params)

      if params_length == 0 do
        not unfinished_parm and npar == 0
      else
        if unfinished_parm do
          params_length >= npar + 1
        else
          params_length > npar
        end
      end
    end)
    |> Enum.sort_by(&length(&1.params))
  end

  defp find_function_signatures({nil, _fun}, _env, _metadata), do: []

  defp find_function_signatures({mod, fun}, env, metadata) do
    signatures =
      case Metadata.get_function_signatures(metadata, mod, fun) do
        [] ->
          Introspection.get_signatures(mod, fun)

        signatures ->
          callback_docs_specs = Metadata.get_docs_specs_from_behaviours(env)

          for signature <- signatures do
            if signature.documentation == nil or signature.spec == nil do
              arity = length(signature.params)

              {spec, doc, _} =
                Metadata.get_doc_spec_from_behaviours(callback_docs_specs, fun, arity)

              %{
                signature
                | documentation: signature.documentation || doc,
                  spec: signature.spec || spec
              }
            else
              signature
            end
          end
      end

    signatures |> Enum.uniq_by(fn sig -> sig.params end)
  end

  defp find_type_signatures({nil, fun}, _metadata) do
    TypeInfo.get_signatures(nil, fun)
  end

  defp find_type_signatures({mod, fun}, metadata) do
    case Metadata.get_type_signatures(metadata, mod, fun) do
      [] -> TypeInfo.get_signatures(mod, fun)
      signature -> signature
    end
  end
end
