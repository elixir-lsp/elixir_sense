defmodule ElixirSense.Core.State.TypeInfo do
  @moduledoc """
  Type definition info
  """
  @type t :: %ElixirSense.Core.State.TypeInfo{
          name: atom,
          args: list(list(String.t())),
          specs: [String.t()],
          kind: :type | :typep | :opaque,
          positions: [ElixirSense.Core.State.position_t()],
          end_positions: [ElixirSense.Core.State.position_t() | nil],
          doc: String.t(),
          meta: map(),
          generated: list(boolean)
        }
  defstruct name: nil,
            args: [],
            specs: [],
            kind: :type,
            positions: [],
            end_positions: [],
            generated: [],
            doc: "",
            meta: %{}
end
