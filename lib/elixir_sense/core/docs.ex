defmodule ElixirSense.Core.Docs do
  def shim_get_docs(module, category) do
    case Code.fetch_docs(module) do
      {:docs_v1, moduledoc_line, _beam_language, "text/markdown", moduledoc, _metadata, docs} ->
        parse_docs_v1(docs, category, moduledoc, moduledoc_line, module)
      _ ->
        nil
    end
  end

  defp parse_docs_v1(docs, category, moduledoc, moduledoc_line, module) do
    docs = Enum.map(docs, &to_old_format/1)
    do_parse_docs_v1(category, docs, moduledoc, moduledoc_line, module)
  end

  defp do_parse_docs_v1(:moduledoc, docs, moduledoc, moduledoc_line, module) do
    moduledoc_en =
      case moduledoc do
        %{"en" => moduledoc_en} -> moduledoc_en
        false -> false
        _ -> nil
      end

    case {moduledoc_line, moduledoc_en} do
      {_, nil} -> nil
      {nil, _} -> nil
      _ -> {moduledoc_line, moduledoc_en}
    end
  end

  defp do_parse_docs_v1(:docs, docs, moduledoc, moduledoc_line, module) do
    Enum.filter(docs, &match?({_, _, def_type, _, _} when def_type in [:def, :defmacro], &1))
  end

  defp do_parse_docs_v1(:callback_docs, docs, moduledoc, moduledoc_line, module) do
    Enum.filter(
      docs,
      &match?({_, _, kind, _} when kind in [:callback, :macrocallback], &1)
    )
  end

  defp do_parse_docs_v1(:type_docs, docs, moduledoc, moduledoc_line, module) do
    Enum.filter(docs, &match?({_, _, :type, _}, &1))
  end

  defp do_parse_docs_v1(:all, docs, moduledoc, moduledoc_line, module) do
    [:moduledoc, :docs, :callback_docs, :type_docs]
    |> Enum.map(&{&1, get_docs(module, &1)})
  end

  defp to_old_format({{kind, name, arity}, line, signatures, docs, _meta}) do
    docs_en =
      case docs do
        %{"en" => docs_en} -> docs_en
        false -> false
        _ -> nil
      end

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
end
