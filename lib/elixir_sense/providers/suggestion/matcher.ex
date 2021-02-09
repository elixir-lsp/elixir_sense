defmodule ElixirSense.Providers.Suggestion.Matcher do
  @moduledoc """
  ## Suggestion Matching
  """

  import Kernel, except: [match?: 2]

  @doc """
  Naive sequential fuzzy matching without weight and requiring first char to match.

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
      false

      iex> ElixirSense.Providers.Suggestion.Matcher.match?("", "")
      true

      iex> ElixirSense.Providers.Suggestion.Matcher.match?("chunk_by", "chub")
      true

      iex> ElixirSense.Providers.Suggestion.Matcher.match?("chunk_by", "chug")
      false
  """
  @spec match?(name :: String.t(), hint :: String.t()) :: boolean()
  def match?(<<name_head::utf8, _name_rest::binary>>, <<hint_head::utf8, _hint_rest::binary>>)
      when name_head != hint_head do
    false
  end

  def match?(name, hint) do
    do_match?(name, hint)
  end

  defp do_match?(<<head::utf8, name_rest::binary>>, <<head::utf8, hint_rest::binary>>) do
    do_match?(name_rest, hint_rest)
  end

  defp do_match?(
         <<_head::utf8, name_rest::binary>>,
         <<_not_head::utf8, _hint_rest::binary>> = hint
       ) do
    do_match?(name_rest, hint)
  end

  defp do_match?(_name_rest, <<>>) do
    true
  end

  defp do_match?(<<>>, <<>>) do
    true
  end

  defp do_match?(<<>>, _) do
    false
  end
end
