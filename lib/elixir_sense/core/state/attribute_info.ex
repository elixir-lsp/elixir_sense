defmodule ElixirSense.Core.State.AttributeInfo do
  @moduledoc """
  Variable info
  """
  @type t :: %ElixirSense.Core.State.AttributeInfo{
          name: atom,
          positions: list(ElixirSense.Core.Compiler.State.position_t()),
          type: ElixirSense.Core.Compiler.State.var_type()
        }
  defstruct name: nil, positions: [], type: nil
end
