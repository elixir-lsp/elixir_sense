defmodule ElixirSense.Core.EdocReader do
  @moduledoc false

  alias ElixirSense.Core.Introspection

  @spec get_moduledoc(module()) :: list
  def get_moduledoc(m) when is_atom(m), do: lookup(m, [:moduledoc])

  @spec get_docs(module(), atom(), non_neg_integer() | :any) :: list
  def get_docs(m, f \\ :any, arity \\ :any)
      when is_atom(m) and is_atom(f) and (is_integer(arity) or arity == :any) do
    lookup({m, f, arity}, [:doc, :spec])
  end

  @spec get_specs(module(), atom(), non_neg_integer() | :any) :: list
  def get_specs(m, f, arity \\ :any)
      when is_atom(m) and is_atom(f) and (is_integer(arity) or arity == :any) do
    lookup({m, f, arity}, [:spec])
  end

  @spec get_typedocs(module()) :: list
  def get_typedocs(m) when is_atom(m) do
    lookup(m, [:type])
  end

  @spec get_typedocs(module(), atom(), non_neg_integer() | :any) :: list
  def get_typedocs(m, t, arity \\ :any)
      when is_atom(m) and is_atom(t) and (is_integer(arity) or arity == :any) do
    lookup({m, t, arity}, [:doc, :type])
  end

  defp lookup(key, kinds) do
    module = key_to_module(key)

    if Introspection.elixir_module?(module) do
      []
    else
      try do
        case :docsh_lib.get_docs(module) do
          {:error, _reason} ->
            []

          {:ok, docs = {:docs_v1, line, :erlang, "text/erlang-edoc", moduledoc, metadata, _docs}} ->
            case kinds do
              [:moduledoc] ->
                [{line, moduledoc, metadata}]

              _ ->
                case :docsh_format.lookup(docs, key, kinds) do
                  {:not_found, _message} ->
                    []

                  {:ok, doc_items} ->
                    doc_items
                end
            end
        end
      rescue
        _ -> []
      end
    end
  end

  @spec extract_docs(:none | :hidden | map) :: nil | false | String.t()
  def extract_docs(%{"en" => edoc_xml}) do
    :docsh_edoc.format_edoc(edoc_xml, %{})
    |> :erlang.iolist_to_binary()
  end

  def extract_docs(:hidden), do: false
  def extract_docs(_), do: nil

  defp key_to_module(m) when is_atom(m), do: m
  defp key_to_module({m, _, _}), do: m
end
