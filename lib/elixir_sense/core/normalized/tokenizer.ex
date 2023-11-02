defmodule ElixirSense.Core.Normalized.Tokenizer do
  @moduledoc """
  Handles tokenization of Elixir code snippets

  Uses private api :elixir_tokenizer
  """

  @spec tokenize(String.t()) :: [tuple]
  def tokenize(prefix) do
    prefix
    |> String.to_charlist()
    |> do_tokenize_1_7()
  end

  defp do_tokenize_1_7(prefix_charlist) do
    case :elixir_tokenizer.tokenize(prefix_charlist, 1, []) do
      # Elixir < 1.13
      {:ok, tokens} ->
        Enum.reverse(tokens)

      # Elixir >= 1.13
      {:ok, _line, _column, _warning, tokens} ->
        Enum.reverse(tokens)

      # Elixir < 1.13
      {:error, {_line, _column, _error_prefix, _token}, _rest, sofar} ->
        sofar

      # Elixir >= 1.13
      {:error, _, _, _, sofar} ->
        sofar
    end
  end
end
