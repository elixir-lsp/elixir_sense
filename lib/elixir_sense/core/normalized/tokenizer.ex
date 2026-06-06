defmodule ElixirSense.Core.Normalized.Tokenizer do
  @moduledoc """
  Handles tokenization of Elixir code snippets

  Uses private api :elixir_tokenizer
  """
  require Logger

  @spec tokenize(String.t(), Keyword.t()) :: [tuple]
  def tokenize(prefix, options \\ []) do
    options = options |> Keyword.put(:emit_warnings, false)

    prefix
    |> String.to_charlist()
    |> do_tokenize(options)
  end

  defp do_tokenize(prefix_charlist, options) do
    result = :elixir_tokenizer.tokenize(prefix_charlist, 1, options)

    case result do
      # < 1.17
      {:ok, _line, _column, _warning, tokens} ->
        Enum.reverse(tokens)

      # >= 1.17
      {:ok, _line, _column, _warning, tokens, _terminators} ->
        tokens

      {:error, _, _, _, sofar} ->
        sofar
    end
  rescue
    e ->
      if Version.match?(System.version(), ">= 1.20.0-dev") do
        Logger.error(
          ":elixir_tokenizer.tokenize raised #{Exception.format(:error, e, __STACKTRACE__)}. Please report that to elixir project."
        )

        reraise e, __STACKTRACE__
      else
        []
      end
  end
end
