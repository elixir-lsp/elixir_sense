defmodule ElixirSense do
  @moduledoc """
  ElxirSense is a Elixir library that implements useful features for any editor/tool that needs
  to introspect context-aware information about Elixir source code.

  This module provides the basic functionality for context-aware code completion, docs, signature info and more.
  """

  alias ElixirSense.Core.Applications
  alias ElixirSense.Core.Parser

  @type callee_t :: {module, atom, non_neg_integer}

  @type call_t :: %{
          callee: callee_t,
          file: nil | String.t(),
          line: nil | pos_integer,
          column: nil | pos_integer
        }
  @type call_trace_t :: %{optional(callee_t) => [call_t]}

  @doc ~S"""
  Returns a sorted list of all available modules

  ## Example

      iex> ":application" in ElixirSense.all_modules()
      true

      iex> "Version.Parser" in ElixirSense.all_modules()
      true

  """
  @spec all_modules() :: list(String.t())
  def all_modules do
    Applications.get_modules_from_applications()
    |> Enum.map(&inspect/1)
    |> Enum.sort()
  end

  @doc ~S"""
  Provides an error tolerant parser

  ## Example

      iex> code = ~S'''
      ...> defmodule do
      ...> end
      ...> '''
      iex> ElixirSense.string_to_quoted(code, 1)
      {:ok, {:defmodule, [do: [line: 1, column: 11], end: [line: 2, column: 1], line: 1, column: 1], [[do: {:__block__, [], []}]]}}
  """
  @spec string_to_quoted(
          String.t(),
          {pos_integer, pos_integer} | nil,
          non_neg_integer,
          boolean,
          keyword
        ) ::
          {:ok, Macro.t()} | {:error, :parse_error}
  def string_to_quoted(
        source,
        cursor_position \\ nil,
        error_threshold \\ 6,
        fallback_to_container_cursor_to_quoted \\ true,
        parser_options \\ []
      ) do
    string_to_ast_options = [
      errors_threshold: error_threshold,
      cursor_position: cursor_position,
      fallback_to_container_cursor_to_quoted: fallback_to_container_cursor_to_quoted,
      parser_options: parser_options
    ]

    case Parser.string_to_ast(source, string_to_ast_options) do
      {:ok, ast, _source, _error} -> {:ok, ast}
      other -> other
    end
  end
end
