defmodule Alchemist.Helpers.ModuleTest do
  use ExUnit.Case, async: true

  alias Alchemist.Helpers.ModuleInfo

  test "has_function? return true" do
    assert ModuleInfo.has_function?(List, :flatten) == true
    assert ModuleInfo.has_function?(List, :to_string) == true
  end

  test "has_function? :erlang builtins" do
    assert ModuleInfo.has_function?(:erlang, :andalso) == true
    assert ModuleInfo.has_function?(:erlang, :orelse) == true
  end

  test "has_function? return false" do
    assert ModuleInfo.has_function?(List, :split) == false
    assert ModuleInfo.has_function?(List, :map) == false
  end
end
