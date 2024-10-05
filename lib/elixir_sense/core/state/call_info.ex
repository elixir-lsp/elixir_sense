defmodule ElixirSense.Core.State.CallInfo do
  @moduledoc """
  Function call info
  """
  @type t :: %ElixirSense.Core.State.CallInfo{
          arity: non_neg_integer,
          position: ElixirSense.Core.State.position_t(),
          func: atom,
          mod: module | {:attribute, atom}
        }
  defstruct arity: 0,
            position: {1, 1},
            func: nil,
            mod: Elixir
end
