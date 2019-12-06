defmodule Alchemist.Helpers.ModuleTest do
  use ExUnit.Case, async: true

  alias Alchemist.Helpers.ModuleInfo

  test "moduledoc? returns true" do
    assert ModuleInfo.moduledoc?(List) == true
  end

  test "moduledoc? returns false" do
    assert ModuleInfo.moduledoc?(List.Chars.Atom) == false
  end

  test "docs? returns true" do
    assert ModuleInfo.docs?(List, :flatten) == true
    assert ModuleInfo.docs?(Kernel, :def) == true
  end

  test "docs? returns false" do
    assert ModuleInfo.docs?(List, :dance) == false
    assert ModuleInfo.docs?(nil, :dance) == false
  end

  test "has_function? return true" do
    assert ModuleInfo.has_function?(List, :flatten) == true
    assert ModuleInfo.has_function?(List, :to_string) == true
  end

  test "has_function? return false" do
    assert ModuleInfo.has_function?(List, :split) == false
    assert ModuleInfo.has_function?(List, :map) == false
  end
end
