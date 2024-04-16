defmodule ElixirSense.Providers.ModulesTest do
  use ExUnit.Case, async: true

  test "test all modules available modules are listed" do
    modules = ElixirSense.all_modules()
    assert "ElixirSense" in modules
    assert ":kernel" in modules
    assert ":erlang" in modules
  end
end
