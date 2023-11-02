defmodule ElixirSense.Core.Normalized.Code.Formatter do
  def locals_without_parens do
    cond do
      Version.match?(System.version(), ">= 1.14.0-dev") ->
        apply(Code.Formatter, :locals_without_parens, [])

      true ->
        # fall back to bundled on < 1.13
        # on 1.13 use our version as it has all the fixes from last 1.13 release
        apply(ElixirSense.Core.Normalized.Code.ElixirSense.Formatter, :locals_without_parens, [])
    end
  end

  def local_without_parens?(fun, arity, locals_without_parens) do
    cond do
      Version.match?(System.version(), ">= 1.14.0-dev") ->
        apply(Code.Formatter, :local_without_parens?, [fun, arity, locals_without_parens])

      true ->
        # fall back to bundled on < 1.13
        # on 1.13 use our version as it has all the fixes from last 1.13 release
        apply(ElixirSense.Core.Normalized.Code.ElixirSense.Formatter, :local_without_parens?, [
          fun,
          arity,
          locals_without_parens
        ])
    end
  end
end
