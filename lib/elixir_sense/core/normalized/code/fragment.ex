defmodule ElixirSense.Core.Normalized.Code.Fragment do
  @moduledoc false
  require Logger

  def cursor_context(string, opts \\ []) do
    cond do
      Version.match?(System.version(), ">= 1.14.0-dev") ->
        apply(Code.Fragment, :cursor_context, [string, opts])

      true ->
        # fall back to bundled on < 1.13
        # on 1.13 use our version as it has all the fixes from last 1.13 release
        apply(ElixirSense.Core.Normalized.Code.ElixirSense.Fragment, :cursor_context, [
          string,
          opts
        ])
    end
  rescue
    e ->
      if Version.match?(System.version(), ">= 1.16.0-dev") do
        Logger.error(
          "Code.Fragment.cursor_context raised #{Exception.format(:error, e, __STACKTRACE__)}. Please report that to elixir project."
        )

        reraise e, __STACKTRACE__
      else
        :none
      end
  end

  def surround_context(fragment, position, options \\ []) do
    cond do
      Version.match?(System.version(), ">= 1.14.0-dev") ->
        apply(Code.Fragment, :surround_context, [fragment, position, options])

      true ->
        # fall back to bundled on < 1.13
        # on 1.13 use our version as it has all the fixes from last 1.13 release
        apply(ElixirSense.Core.Normalized.Code.ElixirSense.Fragment, :surround_context, [
          fragment,
          position,
          options
        ])
    end
  rescue
    e ->
      if Version.match?(System.version(), ">= 1.16.0-dev") do
        Logger.error(
          "Code.Fragment.surround_context raised #{Exception.format(:error, e, __STACKTRACE__)}. Please report that to elixir project."
        )

        reraise e, __STACKTRACE__
      else
        :none
      end
  end

  def container_cursor_to_quoted(fragment, opts \\ []) do
    cond do
      Version.match?(System.version(), ">= 1.14.0-dev") ->
        apply(Code.Fragment, :container_cursor_to_quoted, [fragment, opts])

      true ->
        # fall back to bundled on < 1.13
        # on 1.13 use our version as it has all the fixes from last 1.13 release
        apply(
          ElixirSense.Core.Normalized.Code.ElixirSense.Fragment,
          :container_cursor_to_quoted,
          [fragment, opts]
        )
    end
  rescue
    e ->
      if Version.match?(System.version(), ">= 1.16.0-dev") do
        try do
          Logger.error(
            "Code.Fragment.container_cursor_to_quoted raised #{Exception.format(:error, e, __STACKTRACE__)}. Please report that to elixir project."
          )
        rescue
          _ -> :ok
        end

        reraise e, __STACKTRACE__
      else
        {:error, {[line: 1, column: 1], "", ""}}
      end
  end
end
