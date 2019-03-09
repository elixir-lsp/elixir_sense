defmodule ElixirSense.Core.NormalizedTypespecTest do

  use ExUnit.Case

  import ElixirSense.Core.Normalized.Typespec

  test "get_types ignores privates types (:opaque and :typep)" do
    types = get_types(ModuleWithPrivateTypes)
    assert types == [type: {:type_t, {:type, 4, :atom, []}, []}]
  end
end
