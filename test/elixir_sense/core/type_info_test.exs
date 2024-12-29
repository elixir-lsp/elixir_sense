defmodule ElixirSense.Core.TypeInfoTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.TypeInfo

  test "builtin_type_documentation" do
    assert [%{name: "any", params: [], spec: "@type any"}] =
             TypeInfo.get_signatures(nil, :any, nil)

    assert [%{name: "pid", params: [], spec: "@type pid"}] =
             TypeInfo.get_signatures(nil, :pid, nil)
  end
end
