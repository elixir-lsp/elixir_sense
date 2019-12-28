defmodule Alchemist.Helpers.ModuleInfo do
  @moduledoc false

  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode

  @spec moduledoc?(module) :: boolean
  def moduledoc?(module) do
    case NormalizedCode.get_docs(module, :moduledoc) do
      {_, doc} -> is_binary(doc)
      _ -> false
    end
  end

  @spec docs?(module, atom) :: boolean
  def docs?(module, function) do
    docs = NormalizedCode.get_docs(module, :docs)
    do_docs?(docs, function)
  end

  @spec has_function?(module, atom) :: boolean
  def has_function?(module, function) do
    List.keymember?(get_module_funs(module), function, 0)
  end

  defp get_module_funs(Elixir), do: []

  defp get_module_funs(module) do
    case Code.ensure_loaded(module) do
      {:module, _} ->
        module.module_info(:exports)
        |> Enum.reduce([], fn {f, a} = fun, acc ->
          case Atom.to_string(f) do
            "MACRO-" <> macro_name ->
              # extract macro name
              [{String.to_atom(macro_name), a} | acc]

            _ ->
              # normal public fun
              [fun | acc]
          end
        end)

      _otherwise ->
        []
    end
  end

  @spec all_applications_modules() :: [module]
  def all_applications_modules do
    for [app] <- loaded_applications(),
        {:ok, modules} = :application.get_key(app, :modules),
        module <- modules do
      module
    end
  end

  defp do_docs?([head | tail], function) do
    {{func, _}, _, _, _, doc} = head

    if func == function and is_binary(doc) do
      true
    else
      do_docs?(tail, function)
    end
  end

  defp do_docs?([], _function), do: false
  defp do_docs?(nil, _function), do: false

  defp loaded_applications do
    # If we invoke :application.loaded_applications/0,
    # it can error if we don't call safe_fixtable before.
    # Since in both cases we are reaching over the
    # application controller internals, we choose to match
    # for performance.
    :ets.match(:ac_tab, {{:loaded, :"$1"}, :_})
  end
end
