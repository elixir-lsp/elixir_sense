defmodule ElixirSense.Core.MetadataBuilder do
  @moduledoc """
  This module is responsible for building/retrieving environment information from an AST.
  """

  import ElixirSense.Core.State

  alias ElixirSense.Core.State
  alias ElixirSense.Core.Compiler

  @doc """
  Traverses the AST building/retrieving the environment information.
  It returns a `ElixirSense.Core.State` struct containing the information.
  """
  @spec build(Macro.t()) :: State.t()
  def build(ast) do
    {_ast, state, _env} =
      Compiler.expand(
        ast,
        %State{
          # TODO remove default when we require elixir 1.15
          prematch: Code.get_compiler_option(:on_undefined_variable) || :warn
        },
        Compiler.env()
      )

    state
    |> remove_attributes_scope
    |> remove_vars_scope
    |> remove_module
  end

  # defp post_string_literal(ast, _state, _line, str) do
  #   str
  #   |> Source.split_lines()
  #   |> Enum.with_index()
  #   # |> Enum.reduce(state, fn {_s, i}, acc -> add_current_env_to_line(acc, line + i) end)
  #   # |> result(ast)
  # end

  # # incomplete spec
  # # @callback my(integer)
  # defp pre(
  #        {:@, _meta_attr, [{kind, _meta_kind, [{name, _meta_name, type_args}]} = _spec]},
  #        _state
  #      )
  #      when kind in [:spec, :callback, :macrocallback] and is_atom(name) and
  #             (is_nil(type_args) or is_list(type_args)) do
  #   # pre_spec(
  #   #   {:@, meta_attr, [{kind, add_no_call(meta_kind), [{name, meta_name, type_args}]}]},
  #   #   state,
  #   #   meta_attr,
  #   #   name,
  #   #   expand_aliases_in_ast(state, List.wrap(type_args)),
  #   #   expand_aliases_in_ast(state, spec),
  #   #   kind
  #   # )
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
