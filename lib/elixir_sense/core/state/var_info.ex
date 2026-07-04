defmodule ElixirSense.Core.State.VarInfo do
  @moduledoc """
  Variable info
  """

  @type t :: %ElixirSense.Core.State.VarInfo{
          name: atom,
          positions: list(ElixirSense.Core.Compiler.State.position_t()),
          scope_id: nil | ElixirSense.Core.Compiler.State.scope_id_t(),
          version: non_neg_integer(),
          type: ElixirSense.Core.Compiler.State.var_type(),
          elixir_types_descr: nil | map()
        }
  defstruct name: nil,
            positions: [],
            scope_id: nil,
            version: 0,
            type: nil,
            elixir_types_descr: nil
end
