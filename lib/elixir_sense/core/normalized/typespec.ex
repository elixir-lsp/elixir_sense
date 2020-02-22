defmodule ElixirSense.Core.Normalized.Typespec do
  @moduledoc false

  @spec beam_specs(module) :: nil | [{:spec, tuple}]
  def beam_specs(module) do
    specs = case Code.Typespec.fetch_specs(module) do
      {:ok, specs} -> specs
      _ -> []
    end

    beam_specs_tag(specs, :spec)
  end

  @spec get_types(module) :: [tuple]
  def get_types(module) when is_atom(module) do
    case Code.Typespec.fetch_types(module) do
      {:ok, types} -> types
      _ -> []
    end
  end

  @spec get_callbacks(module) :: [tuple]
  def get_callbacks(mod) do
    case Code.Typespec.fetch_callbacks(mod) do
      {:ok, callbacks} -> callbacks
      _ -> []
    end
  end

  @spec type_to_quoted(tuple) :: Macro.t()
  def type_to_quoted(type) do
    Code.Typespec.type_to_quoted(type)
  end

  @spec spec_to_quoted(atom, tuple) :: {atom, keyword, [Macro.t()]}
  def spec_to_quoted(name, spec) do
    Code.Typespec.spec_to_quoted(name, spec)
  end

  defp beam_specs_tag(nil, _), do: nil

  defp beam_specs_tag(specs, tag) do
    Enum.map(specs, &{tag, &1})
  end
end
