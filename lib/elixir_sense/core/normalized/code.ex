defmodule ElixirSense.Core.Normalized.Code do
  @doc """
  Shim to replicate the behavior of `Code.get_docs/2` in Elixir >= 1.7
  """
  def get_docs(module, category) do
    if function_exported?(Code, :fetch_docs, 1) do
      case Code.fetch_docs(module) do
        {:docs_v1, moduledoc_line, _beam_language, "text/markdown", moduledoc, _metadata, docs} ->
          docs = Enum.map(docs, &to_old_format/1)

          case category do
            :moduledoc ->
              moduledoc_en =
                case moduledoc do
                  %{"en" => moduledoc_en} -> moduledoc_en
                  false -> false
                  _ -> nil
                end

              case {moduledoc_line, moduledoc_en} do
                {_, nil} -> nil
                _ -> {moduledoc_line, moduledoc_en}
              end

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
    docs_en =
      case docs do
        %{"en" => docs_en} -> docs_en
        :hidden -> false
        :none -> nil
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
