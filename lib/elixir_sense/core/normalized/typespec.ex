defmodule ElixirSense.Core.Normalized.Typespec do
  @moduledoc """
  A module wrapping internal Elixir Code.Typespec APIs
  """

  @spec get_specs(module) :: [tuple]
  def get_specs(module) do
    get_module().fetch_specs(module)
    |> extract_specs
  end

  @spec get_types(module) :: [tuple]
  def get_types(module) when is_atom(module) do
    get_module().fetch_types(module)
    |> extract_specs
  end

  @spec get_callbacks(module) :: [tuple]
  def get_callbacks(module) do
    get_module().fetch_callbacks(module)
    |> extract_specs
  end

  defp extract_specs({:ok, specs}), do: specs
  defp extract_specs(_), do: []

  @spec type_to_quoted(tuple) :: Macro.t()
  def type_to_quoted(type) do
    get_module().type_to_quoted(type)
  end

  @spec spec_to_quoted(atom, tuple) :: {atom, keyword, [Macro.t()]}
  def spec_to_quoted(name, spec) do
    get_module().spec_to_quoted(name, spec)
  end

  defp get_module() do
    if Version.match?(System.version(), ">= 1.14.0-dev") do
      Code.Typespec
    else
      # fall back to bundled on < 1.13 (1.12 is broken on OTP 24)
      # on 1.13 use our version as it has all the fixes from last 1.13 release
      ElixirSense.Core.Normalized.Code.ElixirSense.Typespec
    end
  end
end
