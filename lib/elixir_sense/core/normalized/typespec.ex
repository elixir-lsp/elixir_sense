defmodule ElixirSense.Core.Normalized.Typespec do
  @moduledoc """
  A module wrapping internal Elixir Code.Typespec APIs
  """

  @spec get_specs(module) :: [tuple]
  def get_specs(module) do
    Code.Typespec.fetch_specs(module)
    |> extract_specs
  end

  @spec get_types(module) :: [tuple]
  def get_types(module) when is_atom(module) do
    Code.Typespec.fetch_types(module)
    |> extract_specs
  end

  @spec get_callbacks(module) :: [tuple]
  def get_callbacks(module) do
    Code.Typespec.fetch_callbacks(module)
    |> extract_specs
  end

  defp extract_specs({:ok, specs}), do: specs
  defp extract_specs(_), do: []

  @spec type_to_quoted(tuple) :: Macro.t()
  def type_to_quoted(type) do
    Code.Typespec.type_to_quoted(type)
  end

  @spec spec_to_quoted(atom, tuple) :: {atom, keyword, [Macro.t()]}
  def spec_to_quoted(name, spec) do
    Code.Typespec.spec_to_quoted(name, spec)
  end
end
