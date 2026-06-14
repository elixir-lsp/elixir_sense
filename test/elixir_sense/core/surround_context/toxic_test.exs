defmodule ElixirSense.Core.SurroundContext.ToxicTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Core.SurroundContext.Toxic

  describe "surround_context/2 matches Code.Fragment for navigable shapes" do
    # Each entry is parsed at the given column and must equal Code.Fragment.surround_context/2
    # (same begin/end/context), because the locators feed `context` to `SurroundContext.to_binding`
    # and `begin`/`end` to `get_cursor_env` / `get_call_arity` / `find_var`.
    cases = [
      {"foo", 2},
      {"Foo.Bar.Baz", 6},
      {"Mod.func(1)", 2},
      {"Mod.func(1)", 6},
      {"@attr", 3},
      {"@type t :: integer", 4},
      {"@spec foo(x) :: y", 4},
      {"@moduledoc \"hi\"", 5},
      {"x = foo", 6},
      {"%MyStruct{a: 1}", 4},
      {"%Foo.Bar{}", 4},
      {"~r/abc/", 2},
      {"a |> b", 3},
      {"x + y", 3},
      {":erlang.foo", 9},
      {"a.b.c", 3},
      {"a.b.c", 5},
      {"&1", 2},
      {"foo(1, 2)", 2},
      {"String.length(s)", 9},
      {"@attr.method", 9},
      {"build(x).cursor_env", 12},
      {"calls[line]", 9},
      {"Foo.{Bar, Baz}", 7},
      # regressions found by adversarial review:
      {"__MODULE__.Foo", 1},
      {"@attr.Foo", 1},
      {"@@", 2},
      {"@@@", 2},
      {"&+/2", 1},
      {"x = %__MODULE__{}", 7}
    ]

    for {source, column} <- cases do
      test "#{inspect(source)} @#{column}" do
        source = unquote(source)
        column = unquote(column)

        assert Toxic.surround_context(source, {1, column}) ==
                 Code.Fragment.surround_context(source, {1, column})
      end
    end
  end

  describe "surround_context/2 totality" do
    test "never raises and returns :none or a context map on broken input" do
      for source <- ["def foo(", "%{a: ", "(\n", "@", "&", "Foo.", "x[", ""] do
        for column <- 1..(String.length(source) + 1) do
          result = Toxic.surround_context(source, {1, column})
          assert result == :none or is_map(result)
        end
      end
    end

    test "incomplete parenthesized call resolves like Code.Fragment (local_call)" do
      source = "defimpl P, for: String do\n  def some(t\nend\n"

      assert Toxic.surround_context(source, {2, 8}) ==
               Code.Fragment.surround_context(source, {2, 8})
    end
  end
end
