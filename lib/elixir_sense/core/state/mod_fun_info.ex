defmodule ElixirSense.Core.State.ModFunInfo do
  @moduledoc """
  Module or function info
  """
  alias ElixirSense.Core.State.ModFunInfo
  alias ElixirSense.Core.Introspection

  @type t :: %ElixirSense.Core.State.ModFunInfo{
          params: list(list(term)),
          positions: list(ElixirSense.Core.Compiler.State.position_t()),
          end_positions: list(ElixirSense.Core.Compiler.State.position_t() | nil),
          target: nil | {module, atom},
          overridable: false | {true, module},
          generated: list(boolean),
          doc: String.t(),
          meta: map(),
          # TODO defmodule defprotocol defimpl?
          type:
            :def
            | :defp
            | :defmacro
            | :defmacrop
            | :defdelegate
            | :defguard
            | :defguardp
            | :defmodule
        }

  defstruct params: [],
            positions: [],
            end_positions: [],
            target: nil,
            type: nil,
            generated: [],
            overridable: false,
            doc: "",
            meta: %{}

  def get_arities(%ModFunInfo{params: params_variants}) do
    params_variants
    |> Enum.map(fn params ->
      {length(params), Introspection.count_defaults(params)}
    end)
  end

  def get_category(%ModFunInfo{type: type})
      when type in [:defmacro, :defmacrop, :defguard, :defguardp],
      do: :macro

  def get_category(%ModFunInfo{type: type}) when type in [:def, :defp, :defdelegate],
    do: :function

  def get_category(%ModFunInfo{}), do: :module

  def private?(%ModFunInfo{type: type}), do: type in [:defp, :defmacrop, :defguardp]

  def get_def_kind(%ModFunInfo{type: type})
      when type in [:defmacro, :defguard],
      do: :defmacro

  def get_def_kind(%ModFunInfo{type: type})
      when type in [:defmacrop, :defguardp],
      do: :defmacrop

  def get_def_kind(%ModFunInfo{type: type}) when type in [:def, :defdelegate],
    do: :def

  def get_def_kind(%ModFunInfo{type: type}) when type in [:defp],
    do: :defp
end
