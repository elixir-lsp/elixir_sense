defmodule ElixirSense.Core.Normalized.Tokenizer do
  @moduledoc """
  Handles tokenization of Elixir code snippets

  Uses private api :elixir_tokenizer
  """
  require Logger

  @spec tokenize(String.t()) :: [tuple]
  def tokenize(prefix) do
    prefix
    |> String.to_charlist()
    |> do_tokenize()
  end

  defp do_tokenize(prefix_charlist) do
    result =
      if Version.match?(System.version(), ">= 1.14.0-dev") do
        :elixir_tokenizer.tokenize(prefix_charlist, 1, [])
      else
        # fall back to bundled on < 1.13
        # on 1.13 use our version as it has all the fixes from last 1.13 release
        :elixir_sense_tokenizer.tokenize(prefix_charlist, 1, [])
      end

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
      if Version.match?(System.version(), ">= 1.17.0-dev") do
        Logger.error(
          ":elixir_tokenizer.tokenize raised #{Exception.format(:error, e, __STACKTRACE__)}. Please report that to elixir project."
        )

        reraise e, __STACKTRACE__
      else
        []
      end
  end
end
