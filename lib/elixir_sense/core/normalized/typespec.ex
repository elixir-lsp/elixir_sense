defmodule ElixirSense.Core.Normalized.Typespec do
  @moduledoc """
  A module wrapping internal Elixir Code.Typespec APIs
  """

  @spec get_specs(module) :: [tuple]
  def get_specs(module) do
    safe_fetch(module, &Code.Typespec.fetch_specs/1)
    |> extract_specs
  end

  @spec get_types(module) :: [tuple]
  def get_types(module) when is_atom(module) do
    safe_fetch(module, &Code.Typespec.fetch_types/1)
    |> extract_specs
  end

  @spec get_callbacks(module) :: [tuple]
  def get_callbacks(module) do
    safe_fetch(module, &Code.Typespec.fetch_callbacks/1)
    |> extract_specs
  end

  defp extract_specs({:ok, specs}), do: specs
  defp extract_specs(_), do: []

  # `Code.Typespec.fetch_*` delegates to `:beam_lib`, which only catches throws.
  # Malformed or truncated BEAM data (e.g. a .beam being rewritten by a concurrent
  # compile) raises `error`/`exit` from inside `:beam_lib.info/1` and friends,
  # so treat any such failure as "no typespecs available".
  defp safe_fetch(module, fetch) do
    fetch.(module)
  rescue
    _ -> :error
  catch
    _, _ -> :error
  end

  @spec type_to_quoted(tuple) :: Macro.t()
  def type_to_quoted(type) do
    Code.Typespec.type_to_quoted(type)
  end

  @spec spec_to_quoted(atom, tuple) :: {atom, keyword, [Macro.t()]}
  def spec_to_quoted(name, spec) do
    Code.Typespec.spec_to_quoted(name, spec)
  end
end
