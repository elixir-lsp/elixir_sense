defmodule ElixirSense.Core.Normalized.Code do
  @doc """
  Shim to replicate the behavior of `Code.get_docs/2` in Elixir >= 1.7
  """
  @type doc_t :: nil | false | String.t()
  @type fun_doc_entry_t ::
          {{atom, non_neg_integer}, pos_integer, :def | :defmacro, term, doc_t}
  @type doc_entry_t ::
          {{atom, non_neg_integer}, pos_integer, :callback | :macrocallback | :type, doc_t}
  @type moduledoc_entry_t :: {pos_integer, doc_t}

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
    if function_exported?(Code, :fetch_docs, 1) do
      case Code.fetch_docs(module) do
        {:docs_v1, moduledoc_line, _beam_language, "text/markdown", moduledoc, _metadata, docs} ->
          docs = Enum.map(docs, &to_old_format/1)

          case category do
            :moduledoc ->
              moduledoc_en = extract_docs(moduledoc)

              {moduledoc_line, moduledoc_en}

            :docs ->
              Enum.filter(
                docs,
                &match?({_, _, def_type, _, _} when def_type in [:def, :defmacro], &1)
              )

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
    else
      Module.concat([Code]).get_docs(module, category)
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

  defp extract_docs(%{"en" => docs_en}), do: docs_en
  defp extract_docs(:hidden), do: false
  defp extract_docs(:none), do: nil
end
