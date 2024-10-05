defmodule ElixirSense.Core.State.VarInfo do
  @moduledoc """
  Variable info
  """

  @type t :: %ElixirSense.Core.State.VarInfo{
          name: atom,
          positions: list(ElixirSense.Core.State.position_t()),
          scope_id: nil | ElixirSense.Core.State.scope_id_t(),
          version: non_neg_integer(),
          type: ElixirSense.Core.State.var_type()
        }
  defstruct name: nil,
            positions: [],
            scope_id: nil,
            version: 0,
            type: nil
end
