defmodule ElixirSense.Location do
  @moduledoc """
  A location in a source file or buffer
  """

  @type t :: %ElixirSense.Location{
          type: :module | :function | :variable | :typespec | :macro | :attribute | nil,
          file: String.t() | nil,
          line: pos_integer | nil,
          column: pos_integer | nil
        }
  defstruct [:type, :file, :line, :column]
end
