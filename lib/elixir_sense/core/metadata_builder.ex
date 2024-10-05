defmodule ElixirSense.Core.MetadataBuilder do
  @moduledoc """
  This module is responsible for building/retrieving environment information from an AST.
  """

  alias ElixirSense.Core.Compiler.State
  alias ElixirSense.Core.Compiler

  @doc """
  Traverses the AST building/retrieving the environment information.
  It returns a `ElixirSense.Core.State` struct containing the information.
  """
  @spec build(Macro.t(), nil | {pos_integer, pos_integer}) :: State.t()
  def build(ast, cursor_position \\ nil) do
    state_initial = initial_state(cursor_position)

    {_ast, state, _env} = Compiler.expand(ast, state_initial, Compiler.env())

    state
    |> State.remove_attributes_scope()
    |> State.remove_vars_scope(state_initial)
    |> State.remove_module()
  end

  def initial_state(cursor_position) do
    %State{
      cursor_position: cursor_position,
      prematch:
        if Version.match?(System.version(), ">= 1.15.0-dev") do
          Code.get_compiler_option(:on_undefined_variable)
        else
          :warn
        end
    }
  end

  def default_env(cursor_position \\ nil) do
    macro_env = Compiler.env()
    state_initial = initial_state(cursor_position)
    State.get_current_env(state_initial, macro_env)
  end

  # defp post_string_literal(ast, _state, _line, str) do
  #   str
  #   |> Source.split_lines()
  #   |> Enum.with_index()
  #   # |> Enum.reduce(state, fn {_s, i}, acc -> add_current_env_to_line(acc, line + i) end)
  #   # |> result(ast)
  # end

  # # Any other tuple with a line
  # defp pre({_, meta, _} = ast, state) do
  #   case Keyword.get(meta, :line) do
  #     nil ->
  #       {ast, state}

  #     _line ->
  #       state
  #       # |> add_current_env_to_line(line)
  #       # |> result(ast)
  #   end
  # end

  # # String literal
  # defp post({_, [no_call: true, line: line, column: _column], [str]} = ast, state)
  #      when is_binary(str) do
  #   post_string_literal(ast, state, line, str)
  # end

  # # String literal in sigils
  # defp post({:<<>>, [indentation: _, line: line, column: _column], [str]} = ast, state)
  #      when is_binary(str) do
  #   post_string_literal(ast, state, line, str)
  # end
end
