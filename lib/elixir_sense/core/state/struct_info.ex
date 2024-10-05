defmodule ElixirSense.Core.State.StructInfo do
  @moduledoc """
  Structure definition info
  """
  @type field_t :: {atom, any}
  @type t :: %ElixirSense.Core.State.StructInfo{
          type: :defstruct | :defexception,
          fields: list(field_t)
        }
  defstruct type: :defstruct, fields: []
end
