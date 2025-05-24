defmodule ElixirSense.Core.State.RecordInfo do
  @moduledoc """
  Record definition info
  """
  @type field_t :: {atom, any}
  @type t :: %ElixirSense.Core.State.RecordInfo{
          type: :defrecord | :defrecordp,
          fields: list(field_t),
          doc: String.t(),
          meta: map()
        }
  defstruct type: :defrecord, fields: [], doc: "", meta: %{}
end
