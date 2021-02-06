defmodule ElixirSense.Providers.Suggestion.Matcher do
  @moduledoc """
  ## Suggestion Matching
  """

  import Kernel, except: [match?: 2]

  @doc """
  Naive sequential fuzzy matching without weight.

  ## Examples

      iex> ElixirSense.Providers.Suggestion.Matcher.match?("map", "map")
      true

      iex> ElixirSense.Providers.Suggestion.Matcher.match?("map", "m")
      true

      iex> ElixirSense.Providers.Suggestion.Matcher.match?("map", "ma")
      true

      iex> ElixirSense.Providers.Suggestion.Matcher.match?("map", "mp")
      true

      iex> ElixirSense.Providers.Suggestion.Matcher.match?("map", "")
      true

      iex> ElixirSense.Providers.Suggestion.Matcher.match?("map", "ap")
      true

      iex> ElixirSense.Providers.Suggestion.Matcher.match?("", "")
      true

      iex> ElixirSense.Providers.Suggestion.Matcher.match?("chunk_by", "chub")
      true

      iex> ElixirSense.Providers.Suggestion.Matcher.match?("chunk_by", "chug")
      false
  """
  @spec match?(name :: String.t(), hint :: String.t()) :: boolean()
  def match?(<<head::utf8, name_rest::binary>>, <<head::utf8, hint_rest::binary>>) do
    match?(name_rest, hint_rest)
  end

  def match?(<<_head::utf8, name_rest::binary>>, <<_not_head::utf8, _hint_rest::binary>> = hint) do
    match?(name_rest, hint)
  end

  def match?(_name_rest, <<>>) do
    true
  end

  def match?(<<>>, <<>>) do
    true
  end

  def match?(<<>>, _) do
    false
  end
end
