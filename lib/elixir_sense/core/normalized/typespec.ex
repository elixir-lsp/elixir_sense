defmodule ElixirSense.Core.Normalized.Typespec do
  @spec beam_specs(module) :: nil | [{:spec, tuple}]
  def beam_specs(module) do
    specs =
      if Code.ensure_loaded?(Code.Typespec) do
        case Code.Typespec.fetch_specs(module) do
          {:ok, specs} -> specs
          _ -> []
        end
      else
        old_typespec().beam_specs(module)
      end

    beam_specs_tag(specs, :spec)
  end

  @spec get_types(module) :: [tuple]
  def get_types(module) when is_atom(module) do
    if Code.ensure_loaded?(Code.Typespec) do
      case Code.Typespec.fetch_types(module) do
        {:ok, types} -> types
        _ -> []
      end
    else
      case old_typespec().beam_types(module) do
        nil -> []
        types -> types
      end
    end
  end

  @spec get_callbacks(module) :: [tuple]
  def get_callbacks(mod) do
    if Code.ensure_loaded?(Code.Typespec) do
      case Code.Typespec.fetch_callbacks(mod) do
        {:ok, callbacks} -> callbacks
        _ -> []
      end
    else
      old_typespec().beam_callbacks(mod)
    end
  end

  @spec type_to_quoted(tuple) :: Macro.t()
  def type_to_quoted(type) do
    if Code.ensure_loaded?(Code.Typespec) do
      Code.Typespec.type_to_quoted(type)
    else
      old_typespec().type_to_ast(type)
    end
  end

  @spec spec_to_quoted(atom, tuple) :: {atom, keyword, [Macro.t()]}
  def spec_to_quoted(name, spec) do
    if Code.ensure_loaded?(Code.Typespec) do
      Code.Typespec.spec_to_quoted(name, spec)
    else
      old_typespec().spec_to_ast(name, spec)
    end
  end

  defp old_typespec() do
    Kernel.Typespec
  end

  defp beam_specs_tag(nil, _), do: nil

  defp beam_specs_tag(specs, tag) do
    Enum.map(specs, &{tag, &1})
  end
end
