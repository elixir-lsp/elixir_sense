defmodule ElixirSense.Core.Normalized.Code.CursorContext do
  @moduledoc false

  def cursor_context(string, opts \\ [])

  # credo:disable-for-lines:10
  def cursor_context(binary, opts) do
    cond do
      Version.match?(System.version(), ">= 1.13.0") ->
        apply(Code.Fragment, :cursor_context, [binary, opts])

      true ->
        apply(Code, :cursor_context, [binary, opts])
    end
  end
end
