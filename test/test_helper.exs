defmodule ExUnitConfig do
  def excludes do
    [requires_source: true]
  end
end

ExUnit.configure(exclude: ExUnitConfig.excludes())
ExUnit.start()

Application.load(:erts)
