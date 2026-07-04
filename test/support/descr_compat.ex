defmodule ElixirSense.Test.DescrCompat do
  @moduledoc """
  Version-tolerant helpers over `Module.Types.Descr` for the native type tests.

  Some `Descr` functions used by the test suite landed in different Elixir
  releases:

    * `upper_bound/1` — 1.20+ (extracts the static upper bound of a gradual type)
    * `bitstring/0` — 1.20+ (the bare `bitstring()` constructor)
    * `to_domain_keys/1` — 1.20+ (domain-key classification)

  Tests that only run when `Module.Types.Descr` exists at all are tagged
  `:requires_native_types`, but that tag still lets them run on 1.18/1.19 — where
  those particular functions are absent. These helpers provide a portable
  `upper_bound/1` (with a hand-rolled fallback) and capability predicates so
  individual assertions/corpus entries can be gated per capability rather than
  per Elixir version.

  All dispatch is via `apply/3` + `function_exported?/3`, so this module compiles
  cleanly even on 1.16/1.17 (no `Module.Types.Descr` module at all).
  """

  @descr Module.Types.Descr

  @doc "True when `Descr.bitstring/0` is available (1.20+)."
  def bitstring?, do: exported?(:bitstring, 0)

  @doc "True when `Descr.to_domain_keys/1` is available (1.20+)."
  def to_domain_keys?, do: exported?(:to_domain_keys, 1)

  @doc "True when `Descr.upper_bound/1` is available natively (1.20+)."
  def upper_bound?, do: exported?(:upper_bound, 1)

  @doc """
  Static upper bound of a (possibly gradual) descr.

  Uses the native `Descr.upper_bound/1` when present, otherwise mirrors its
  definition (`upper_bound(%{dynamic: d}) -> d`; `upper_bound(static) -> static`).
  """
  def upper_bound(descr) do
    if upper_bound?() do
      # credo:disable-for-next-line Credo.Check.Refactor.Apply
      apply(@descr, :upper_bound, [descr])
    else
      case descr do
        %{dynamic: dynamic} -> dynamic
        static -> static
      end
    end
  end

  defp exported?(fun, arity) do
    Code.ensure_loaded?(@descr) and function_exported?(@descr, fun, arity)
  end
end
