defmodule ElixirSense.Core.State.CallInfo do
  @moduledoc """
  Reference info
  """
  @type t :: %ElixirSense.Core.State.CallInfo{
          arity: non_neg_integer | nil,
          position: {non_neg_integer, pos_integer | nil},
          func: atom,
          mod: module | {:attribute, atom},
          kind: atom
        }
  defstruct arity: 0,
            position: {1, 1},
            func: nil,
            mod: Elixir,
            kind: nil
end
