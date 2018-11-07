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
end
