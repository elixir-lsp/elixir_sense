defmodule ExUnitConfig do
  def excludes do
    base = [requires_source: true]

    # :requires_native_types — needs the native Module.Types backend at all
    # (Elixir 1.18+; on 1.17 only of_expr/3 + stack/5 exist, which the adaptor
    # can't use, so available?/0 is false there).
    base =
      if ElixirSense.Core.ElixirTypes.available?() do
        base
      else
        [{:requires_native_types, true} | base]
      end

    # :requires_expected_type_native — needs the expected-type typer
    # (Module.Types.Expr.of_expr/5, Elixir 1.19+). On 1.18 (basic of_expr/3 only)
    # expression typing stays on the custom engine, so exclude them there.
    base =
      if ElixirSense.Core.ElixirTypes.available?(:expr) do
        base
      else
        [{:requires_expected_type_native, true} | base]
      end

    # :perf — performance regression tests; excluded by default so the normal
    # suite stays fast. Run explicitly with: mix test --include perf
    [{:perf, true} | base]
  end
end

ExUnit.configure(exclude: ExUnitConfig.excludes())
ExUnit.start()

Application.load(:erts)
