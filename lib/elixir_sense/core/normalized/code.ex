defmodule ElixirSense.Core.Normalized.Code do
  @moduledoc """
  Shims increasing portability of `Code` module
  """

  alias ElixirSense.Core.Behaviours
  alias ElixirSense.Core.ErlangHtml

  @type doc_t :: nil | false | String.t()
  @type fun_doc_entry_t ::
          {{atom, non_neg_integer}, pos_integer, :function | :macro, term, doc_t, map}
  @type doc_entry_t ::
          {{atom, non_neg_integer}, pos_integer, :callback | :macrocallback | :type, doc_t, map}
  @type moduledoc_entry_t :: {pos_integer, doc_t, map}

  @supported_mime_types ["text/markdown", "application/erlang+html"]

  @doc """
  Shim to replicate the behavior of deprecated `Code.get_docs/2`
  """
  @spec get_docs(module, :docs) :: nil | [fun_doc_entry_t]
  @spec get_docs(module, :callback_docs | :type_docs) :: nil | [doc_entry_t]
  @spec get_docs(module, :moduledoc) :: nil | moduledoc_entry_t
  def get_docs(module, category) do
    case fetch_docs(module) do
      {:docs_v1, moduledoc_anno, _language, mime_type, moduledoc, metadata, docs}
      when mime_type in @supported_mime_types ->
        app = ElixirSense.Core.Applications.get_application(module)

        case category do
          :moduledoc ->
            moduledoc_en = extract_docs(moduledoc, mime_type, module, app)

            metadata =
              maybe_mark_as_hidden(metadata, moduledoc_en)
              |> Map.put(:app, app)

            {max(:erl_anno.line(moduledoc_anno), 1), moduledoc_en, metadata}

          :docs ->
            get_fun_docs(module, app, docs, mime_type)

          :callback_docs ->
            for {{kind, _name, _arity}, _anno, _signatures, _docs, _metadata} = entry
                when kind in [:callback, :macrocallback] <- docs do
              map_doc_entry(entry, mime_type, module, app)
            end

          :type_docs ->
            # Erlang doc chunks normalize all type-kinds (`-type`, `-opaque`,
            # `-nominal`) to `:type` in doc entries — the actual kind only
            # appears in `Code.Typespec.fetch_types/1`. Match `:opaque` and
            # `:nominal` defensively in case the normalization changes in a
            # future OTP — downstream consumers read the kind out of the
            # Typespec chunk anyway, so a non-`:type` value here is harmless.
            for {{kind, _name, _arity}, _anno, _signatures, _docs, _metadata} = entry
                when kind in [:type, :opaque, :nominal] <- docs do
              map_doc_entry(entry, mime_type, module, app)
            end
        end

      _ ->
        nil
    end
  end

  defp map_doc_entry(
         {{kind, name, arity}, anno, signatures, docs, metadata},
         mime_type,
         module,
         app,
         original_app \\ nil
       ) do
    docs_en = extract_docs(docs, mime_type, module, app)
    # TODO check if we can get column here
    line = :erl_anno.line(anno)

    metadata =
      maybe_mark_as_hidden(metadata, docs_en)
      |> Map.put(:app, original_app || app)

    case kind do
      kind when kind in [:function, :macro] ->
        args_quoted =
          signatures
          |> Enum.join(" ")
          |> Code.string_to_quoted(emit_warnings: false)
          |> case do
            {:ok, {^name, _, args}} -> args
            _ -> []
          end

        {{name, arity}, line, kind, args_quoted, docs_en, metadata}

      _ ->
        {{name, arity}, line, kind, docs_en, metadata}
    end
  end

  @spec extract_docs(
          %{required(String.t()) => String.t()} | :hidden | :none,
          String.t(),
          module(),
          atom()
        ) ::
          String.t() | false | nil
  def extract_docs(%{"en" => docs_en}, "text/markdown", _module, _app), do: docs_en

  def extract_docs(%{"en" => docs_en}, "application/erlang+html", module, app) do
    ErlangHtml.to_markdown(docs_en, module, app)
  end

  def extract_docs(:hidden, _, _, _), do: false
  def extract_docs(_, _, _, _), do: nil

  defp get_fun_docs(module, app, docs, mime_type) do
    docs_from_module =
      Enum.filter(
        docs,
        &match?(
          {{kind, _name, _arity}, _anno, _signatures, _docs, _metadata}
          when kind in [:function, :macro],
          &1
        )
      )

    non_documented =
      docs_from_module
      |> Stream.filter(fn {{_kind, _name, _arity}, _anno, _signatures, docs, _metadata} ->
        docs in [:hidden, :none] or not Map.has_key?(docs, "en")
      end)
      |> Enum.into(MapSet.new(), fn {{_kind, name, arity}, _anno, _signatures, _docs, _metadata} ->
        {name, arity}
      end)

    docs_from_behaviours = get_docs_from_behaviour(module, non_documented)

    Enum.map(
      docs_from_module,
      fn
        {{kind, name, arity}, anno, fun_signatures, fun_docs, fun_metadata} ->
          {signatures, docs, metadata, mime_type} =
            case Map.get(docs_from_behaviours, {name, arity}) do
              {_behaviour_signatures, behaviour_docs, behaviour_metadata, behaviour_mime_type} ->
                # as of elixir 1.14 behaviours do not store signature
                # prefer signature from function
                # merge metadata from function and callback
                merged_metadata =
                  behaviour_metadata
                  |> Map.merge(fun_metadata)

                merged_metadata =
                  if fun_docs == :hidden do
                    Map.put(merged_metadata, :hidden, true)
                  else
                    merged_metadata
                  end

                {fun_signatures, behaviour_docs, merged_metadata, behaviour_mime_type}

              nil ->
                {fun_signatures, fun_docs, fun_metadata, mime_type}
            end

          {{kind, name, arity}, anno, signatures, docs, metadata}
          |> map_doc_entry(
            mime_type,
            Map.get(metadata, :implementing, module),
            Map.get(metadata, :implementing_module_app, app),
            app
          )
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
      |> Behaviours.get_module_behaviours()
      |> Stream.flat_map(&callback_documentation/1)
      |> Stream.filter(fn {name_arity, {_signatures, _docs, _metadata, _mime_type}} ->
        Enum.member?(funs, name_arity)
      end)
      |> Enum.into(%{})
    end
  end

  def callback_documentation(module) do
    app = ElixirSense.Core.Applications.get_application(module)

    case Code.fetch_docs(module) do
      {:docs_v1, _moduledoc_anno, _language, mime_type, _moduledoc, _metadata, docs}
      when mime_type in @supported_mime_types ->
        docs
        |> Stream.filter(
          &match?(
            {{kind, _name, _arity}, _anno, _signatures, _docs, _metadata}
            when kind in [:callback, :macrocallback],
            &1
          )
        )
        |> Stream.map(fn {{_kind, name, arity}, _anno, signatures, docs, metadata} ->
          docs =
            if module == Exception and name in [:exception, :message] do
              %{"en" => ElixirSense.Core.BuiltinFunctions.get_docs({name, arity})}
            else
              docs
            end

          {{name, arity},
           {signatures, docs,
            metadata |> Map.put(:implementing, module) |> Map.put(:implementing_module_app, app),
            mime_type}}
        end)

      _ ->
        []
    end
  end

  defp maybe_mark_as_hidden(metadata, false) do
    Map.put(metadata, :hidden, true)
  end

  defp maybe_mark_as_hidden(metadata, _text), do: metadata

  # Delegate to `Code.fetch_docs/1`. Since Elixir 1.16 (PRs #13061, #13075,
  # #13286) the upstream implementation honors the `doc/chunks/` fallback,
  # `Path.expand` semantics, and preloaded modules — so the previous local
  # reimplementation is no longer needed.
  @spec fetch_docs(module | String.t()) ::
          {:docs_v1, annotation, beam_language, format, module_doc :: doc_content, metadata,
           docs :: [doc_element]}
          | {:error, any}
        when annotation: :erl_anno.anno(),
             beam_language: :elixir | :erlang | atom(),
             doc_content: %{optional(binary) => binary} | :none | :hidden,
             doc_element:
               {{kind :: atom, function_name :: atom, arity}, annotation, signature, doc_content,
                metadata},
             format: binary,
             signature: [binary],
             metadata: map
  def fetch_docs(module_or_path)

  def fetch_docs(module) when is_atom(module), do: Code.fetch_docs(module)
  def fetch_docs(path) when is_binary(path), do: Code.fetch_docs(path)
end
