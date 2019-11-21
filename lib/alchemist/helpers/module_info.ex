defmodule Alchemist.Helpers.ModuleInfo do
  @moduledoc false

  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode

  @builtin_functions [{:__info__, 1}, {:module_info, 0}, {:module_info, 1}]

  def moduledoc?(module) do
    case NormalizedCode.get_docs(module, :moduledoc) do
      {_, doc} -> is_binary(doc)
      _ -> false
    end
  end

  def docs?(module, function) do
    docs = NormalizedCode.get_docs(module, :docs)
    do_docs?(docs, function)
  end

  def expand_alias([name | rest] = list, aliases) do
    module = Module.concat(Elixir, name)

    aliases
    |> Enum.find_value(list, fn {alias, mod} ->
      if alias === module do
        case Atom.to_string(mod) do
          "Elixir." <> mod ->
            Module.concat([mod | rest])

          _ ->
            mod
        end
      end
    end)
    |> normalize_module
  end

  def get_functions(module, hint) do
    hint = to_string(hint)
    {module, _} = Code.eval_string(module)
    functions = get_module_funs(module)

    list =
      Enum.reduce(functions, [], fn {f, a}, acc ->
        case :lists.keyfind(f, 1, acc) do
          {f, aa} -> :lists.keyreplace(f, 1, acc, {f, [a | aa]})
          false -> [{f, [a]} | acc]
        end
      end)

    list
    |> do_get_functions(hint)
    |> :lists.sort()
  end

  def has_function?(module, function) do
    List.keymember?(get_module_funs(module), function, 0)
  end

  defp do_get_functions(list, "") do
    all_functions(list)
  end

  defp do_get_functions(list, hint) do
    all_functions(list, hint)
  end

  defp get_module_funs(module) do
    case Code.ensure_loaded(module) do
      {:module, _} ->
        module.module_info(:functions)
        |> Enum.reduce([], fn {f, a} = fun, acc ->
          case Atom.to_string(f) do
            "-" <> _ ->
              # skip anonymous/private
              acc

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

  defp all_functions(list) do
    for {fun, arities} <- list do
      for arity <- arities, {fun, arity} not in @builtin_functions do
        {fun, arity}
      end
    end
    |> List.flatten()
  end

  defp all_functions(list, hint) do
    for {fun, arities} <- list, name = Atom.to_string(fun), String.starts_with?(name, hint) do
      for arity <- arities, {fun, arity} not in @builtin_functions do
        {fun, arity}
      end
    end
    |> List.flatten()
  end

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

  defp normalize_module(mod) do
    if is_list(mod) do
      Module.concat(mod)
    else
      mod
    end
  end
end
