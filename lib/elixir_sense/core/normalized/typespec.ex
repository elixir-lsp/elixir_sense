defmodule ElixirSense.Core.Normalized.Typespec do
  @moduledoc """
  A module wrapping internal Elixir Code.Typespec APIs
  """
  require Logger

  @spec get_specs(module) :: [tuple]
  def get_specs(module) do
    Code.Typespec.fetch_specs(module)
    |> extract_specs
  rescue
    e ->
      # workaround for crash
      # Keyword.fetch({:error, :beam_lib, {:not_a_beam_file, ""}}, :module)
      # fixed in elixir 1.16.0
      if Version.match?(System.version(), ">= 1.20.0-dev") do
        Logger.error(
          "Code.Typespec.fetch_specs raised #{Exception.format(:error, e, __STACKTRACE__)}. Please report that to elixir project."
        )

        reraise e, __STACKTRACE__
      else
        []
      end
  end

  @spec get_types(module) :: [tuple]
  def get_types(module) when is_atom(module) do
    Code.Typespec.fetch_types(module)
    |> extract_specs
  rescue
    e ->
      # workaround for crash
      # Keyword.fetch({:error, :beam_lib, {:not_a_beam_file, ""}}, :module)
      # fixed in elixir 1.16.0
      if Version.match?(System.version(), ">= 1.20.0-dev") do
        Logger.error(
          "Code.Typespec.fetch_types raised #{Exception.format(:error, e, __STACKTRACE__)}. Please report that to elixir project."
        )

        reraise e, __STACKTRACE__
      else
        []
      end
  end

  @spec get_callbacks(module) :: [tuple]
  def get_callbacks(module) do
    Code.Typespec.fetch_callbacks(module)
    |> extract_specs
  rescue
    e ->
      # workaround for crash
      # Keyword.fetch({:error, :beam_lib, {:not_a_beam_file, ""}}, :module)
      # fixed in elixir 1.16.0
      if Version.match?(System.version(), ">= 1.20.0-dev") do
        Logger.error(
          "Code.Typespec.fetch_callbacks raised #{Exception.format(:error, e, __STACKTRACE__)}. Please report that to elixir project."
        )

        reraise e, __STACKTRACE__
      else
        []
      end
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
