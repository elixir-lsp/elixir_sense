defmodule ElixirSense.Core.State.SpecInfo do
  @moduledoc """
  Type definition info
  """
  @type t :: %ElixirSense.Core.State.SpecInfo{
          name: atom,
          args: list(list(String.t())),
          specs: [String.t()],
          elixir_types_sig: nil | {:infer | :strong, term(), list()},
          kind: :spec | :callback | :macrocallback,
          positions: [ElixirSense.Core.Compiler.State.position_t()],
          end_positions: [ElixirSense.Core.Compiler.State.position_t() | nil],
          doc: String.t(),
          meta: map(),
          generated: list(boolean)
        }
  defstruct name: nil,
            args: [],
            specs: [],
            elixir_types_sig: nil,
            kind: :spec,
            positions: [],
            end_positions: [],
            generated: [],
            doc: "",
            meta: %{}
end
