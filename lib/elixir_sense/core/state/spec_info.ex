defmodule ElixirSense.Core.State.SpecInfo do
  @moduledoc """
  Type definition info
  """
  @type t :: %ElixirSense.Core.State.SpecInfo{
          name: atom,
          args: list(list(String.t())),
          specs: [String.t()],
          elixir_types_sig: nil | {:infer | :strong, term(), list()},
          # Provenance of elixir_types_sig. :spec means the sig was derived from a
          # user @spec annotation (not compiler-verified). nil means the field is
          # unset (no sig was built). Stored as a parallel field rather than a
          # 4th tuple element because every reader pattern-matches on a 3-tuple.
          elixir_types_sig_source: nil | :spec,
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
            elixir_types_sig_source: nil,
            kind: :spec,
            positions: [],
            end_positions: [],
            generated: [],
            doc: "",
            meta: %{}
end
