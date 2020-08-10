# TODO: revert this commit when "warnings-as-errors: true" on CI moves to Elixir 1.11
if Version.match?(System.build_info().version, ">= 1.10.0") do
  defmodule ElixirSenseExample.ModuleWithBuiltinTypeShadowing do
    @compile {:no_warn_undefined, {B.Callee, :fun, 0}}
    def plain_fun do
      B.Callee.fun()
    end
  end
end
