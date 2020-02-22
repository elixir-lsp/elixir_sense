defmodule ElixirSense.Core.Normalized.Code do
  @moduledoc """
  Shim to replicate the behavior of deprecated `Code.get_docs/2`
  """

  @type doc_t :: nil | false | String.t()
  @type fun_doc_entry_t ::
          {{atom, non_neg_integer}, :erl_anno.anno(), :def | :defmacro, term, doc_t}
  @type doc_entry_t ::
          {{atom, non_neg_integer}, :erl_anno.anno(), :callback | :macrocallback | :type, doc_t}
  @type moduledoc_entry_t :: {:erl_anno.anno(), doc_t}

  @spec get_docs(module, :docs) :: nil | [fun_doc_entry_t]
  @spec get_docs(module, :callback_docs | :type_docs) :: nil | [:doc_entry_t]
  @spec get_docs(module, :moduledoc) :: nil | moduledoc_entry_t
  @spec get_docs(module, :all) ::
          nil
          | %{
              moduledoc: moduledoc_entry_t,
              docs: [:doc_entry_t],
              callback_docs: [:doc_entry_t],
              type_docs: [:doc_entry_t]
            }
  def get_docs(module, category) do
    case Code.fetch_docs(module) do
      {:docs_v1, moduledoc_line, _beam_language, "text/markdown", moduledoc, _metadata, docs} ->
        docs = Enum.map(docs, &to_old_format/1)

        case category do
          :moduledoc ->
            moduledoc_en = extract_docs(moduledoc)

            {moduledoc_line, moduledoc_en}

          :docs ->
            get_fun_docs(module, docs)

          :callback_docs ->
            Enum.filter(
              docs,
              &match?({_, _, kind, _} when kind in [:callback, :macrocallback], &1)
            )

          :type_docs ->
            Enum.filter(docs, &match?({_, _, :type, _}, &1))

          :all ->
            [:moduledoc, :docs, :callback_docs, :type_docs]
            |> Enum.map(&{&1, get_docs(module, &1)})
        end

      _ ->
        nil
    end
  end

  defp to_old_format({{kind, name, arity}, line, signatures, docs, _meta}) do
    docs_en = extract_docs(docs)

    case kind do
      kind when kind in [:function, :macro] ->
        args_quoted =
          signatures
          |> Enum.join(" ")
          |> Code.string_to_quoted()
          |> case do
            {:ok, {^name, _, args}} -> args
            _ -> []
          end

        def_type =
          case kind do
            :function -> :def
            :macro -> :defmacro
          end

        {{name, arity}, line, def_type, args_quoted, docs_en}

      _ ->
        {{name, arity}, line, kind, docs_en}
    end
  end

  @spec extract_docs(%{required(String.t()) => String.t()} | :hidden | :none) ::
          String.t() | false | nil
  defp extract_docs(%{"en" => docs_en}), do: docs_en
  defp extract_docs(:hidden), do: false
  defp extract_docs(:none), do: nil

  defp get_fun_docs(module, docs) do
    docs_from_module =
      Enum.filter(
        docs,
        &match?({_, _, def_type, _, _} when def_type in [:def, :defmacro], &1)
      )

    non_documented =
      docs_from_module
      |> Stream.filter(fn {_name_arity, _line, _, _args, doc} -> doc in [nil, false] end)
      |> Enum.into(MapSet.new(), fn {name_arity, _line, _, _args, _doc} -> name_arity end)

    docs_from_behaviours = get_docs_from_behaviour(module, non_documented)

    Enum.map(
      docs_from_module,
      fn
        {name_arity, line, type, args, doc} ->
          {name_arity, line, type, args, Map.get(docs_from_behaviours, name_arity, doc)}

        other ->
          other
      end
    )
  end

  defp get_docs_from_behaviour(module, funs) do
    if Enum.empty?(funs) do
      # Small optimization to avoid needless analysis of behaviour modules if the collection of
      # required functions is empty.
      %{}
    else
      module
      |> behaviours()
      |> Stream.flat_map(&callback_documentation/1)
      |> Stream.filter(fn {name_arity, _doc} -> Enum.member?(funs, name_arity) end)
      |> Enum.into(%{})
    end
  end

  defp callback_documentation(module) do
    docs = get_docs(module, :callback_docs) || []
    Stream.map(docs, fn {name_arity, _line, _, doc} -> {name_arity, doc} end)
  end

  defp behaviours(module) do
    if function_exported?(module, :module_info, 1),
      do: module.module_info(:attributes) |> Keyword.get_values(:behaviour) |> Enum.concat(),
      else: []
  end
end
