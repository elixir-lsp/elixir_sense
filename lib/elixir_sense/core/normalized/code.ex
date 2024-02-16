defmodule ElixirSense.Core.Normalized.Code do
  @moduledoc """
  Shims increasing portability of `Code` module
  """

  alias ElixirSense.Core.Behaviours
  alias ElixirSense.Core.ErlangHtml
  alias ElixirSense.Core.Normalized.Path, as: PathNormalized

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
  @spec get_docs(module, :callback_docs | :type_docs) :: nil | [:doc_entry_t]
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
            for {{:type, _name, _arity}, _anno, _signatures, _docs, _metadata} = entry <- docs do
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
          |> Code.string_to_quoted()
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

  # the functions below are copied from elixir project
  # https://github.com/lukaszsamson/elixir/blob/bf3e2fd3ad78235bda059b80994a90d9a4184353/lib/elixir/lib/code.ex
  # with applied https://github.com/elixir-lang/elixir/pull/13061
  # and https://github.com/elixir-lang/elixir/pull/13075
  # as well as some small stability fixes
  # TODO remove when we require elixir 1.16
  # The original code is licensed as follows:
  #
  # Copyright 2012 Plataformatec
  # Copyright 2021 The Elixir Team
  #
  # Licensed under the Apache License, Version 2.0 (the "License");
  # you may not use this file except in compliance with the License.
  # You may obtain a copy of the License at
  #
  #    https://www.apache.org/licenses/LICENSE-2.0
  #
  # Unless required by applicable law or agreed to in writing, software
  # distributed under the License is distributed on an "AS IS" BASIS,
  # WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  # See the License for the specific language governing permissions and
  # limitations under the License.

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

  def fetch_docs(module) when is_atom(module) do
    case get_beam_and_path(module) do
      {bin, beam_path} ->
        case fetch_docs_from_beam(bin) do
          {:error, :chunk_not_found} ->
            app_root = PathNormalized.expand(Path.join(["..", ".."]), beam_path)
            path = Path.join([app_root, "doc", "chunks", "#{module}.chunk"])
            fetch_docs_from_chunk(path)

          other ->
            other
        end

      :error ->
        case :code.is_loaded(module) do
          {:file, :preloaded} ->
            # The ERTS directory is not necessarily included in releases
            # unless it is listed as an extra application.
            case :code.lib_dir(:erts) do
              path when is_list(path) ->
                path = Path.join([path, "doc", "chunks", "#{module}.chunk"])
                fetch_docs_from_chunk(path)

              {:error, _} ->
                {:error, :chunk_not_found}
            end

          _ ->
            {:error, :module_not_found}
        end
    end
  end

  def fetch_docs(path) when is_binary(path) do
    fetch_docs_from_beam(String.to_charlist(path))
  end

  defp get_beam_and_path(module) do
    with {^module, beam, filename} <- :code.get_object_code(module),
         info_pairs when is_list(info_pairs) <- :beam_lib.info(beam),
         {:ok, ^module} <- Keyword.fetch(info_pairs, :module) do
      {beam, filename}
    else
      _ -> :error
    end
  end

  @docs_chunk [?D, ?o, ?c, ?s]

  # TODO remove when we depend on elixir with fix for https://github.com/elixir-lang/elixir/pull/13286
  defp fetch_docs_from_beam(bin_or_path) do
    case :beam_lib.chunks(bin_or_path, [@docs_chunk]) do
      {:ok, {_module, [{@docs_chunk, bin}]}} ->
        load_docs_chunk(bin)

      {:error, :beam_lib, {:missing_chunk, _, @docs_chunk}} ->
        {:error, :chunk_not_found}

      {:error, :beam_lib, {:file_error, _, :enoent}} ->
        {:error, :module_not_found}

      {:error, :beam_lib, reason} ->
        {:error, reason}
    end
  end

  defp fetch_docs_from_chunk(path) do
    case File.read(path) do
      {:ok, bin} ->
        load_docs_chunk(bin)

      {:error, _} ->
        {:error, :chunk_not_found}
    end
  end

  defp load_docs_chunk(bin) do
    :erlang.binary_to_term(bin)
  rescue
    _ ->
      {:error, {:invalid_chunk, bin}}
  end
end
