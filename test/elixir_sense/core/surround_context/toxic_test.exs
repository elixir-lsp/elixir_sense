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

    # `Code.Fragment.surround_context/2` gained the `:capture_arg` context in Elixir 1.18. toxic2
    # parses `&1` structurally, so Toxic returns it on every supported version; only assert parity
    # with the (version-dependent) oracle where the oracle also produces it.
    test "&1 capture arg" do
      assert %{context: {:capture_arg, ~c"&1"}, begin: {1, 1}, end: {1, 3}} =
               Toxic.surround_context("&1", {1, 2})

      if Version.match?(System.version(), ">= 1.18.0") do
        assert Toxic.surround_context("&1", {1, 2}) ==
                 Code.Fragment.surround_context("&1", {1, 2})
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

  # regressions found by adversarial review (gpt-5.5)
  describe "surround_context/2 multi-line / lexical edge cases" do
    # the dot end must come from the callee position (line+col), not the dot - otherwise a remote
    # call whose name is on the next line gets an impossible same-line end and breaks arity lookup.
    test "multi-line remote call, cursor on the dot" do
      source = "A.\n  bar\n"

      assert Toxic.surround_context(source, {1, 2}) ==
               Code.Fragment.surround_context(source, {1, 2})
    end

    # `@ attr` (space) is the unary `@` operator on a local var, not a module attribute.
    test "spaced @ attr is a local var, not a module attribute" do
      source = "defmodule A do\n  @attr 1\n  def t do\n    attr = 1\n    @ attr\n  end\nend\n"

      assert Toxic.surround_context(source, {5, 7}) ==
               Code.Fragment.surround_context(source, {5, 7})
    end

    # the slash of remote arity `A.bar/1` must defer to Code.Fragment (:none), not be a `/` operator.
    test "remote arity slash defers to Code.Fragment" do
      source = "A.bar/1\n"

      assert Toxic.surround_context(source, {1, 6}) ==
               Code.Fragment.surround_context(source, {1, 6})
    end

    # `a..b//c` lowers to a single ternary `:..//` node, but Code.Fragment classifies the `..`
    # and `//` as two separate operators. The cursor on `..` must report `..` (not the whole
    # `..//` atom name); the `//` part falls back to Code.Fragment.
    test "step-range ..// reports `..` on the `..` columns and matches Code.Fragment" do
      for {source, col} <- [
            {"1..10//2", 2},
            {"1..10//2", 3},
            {"1..10//2", 6},
            {"x = 1..10//2", 6},
            {"for i <- 1..10//2, do: i", 11}
          ] do
        assert Toxic.surround_context(source, {1, col}) ==
                 Code.Fragment.surround_context(source, {1, col}),
               "mismatch for #{inspect(source)} @#{col}"
      end
    end
  end

  # the literal_encoder gives bare `:atom`s a range so they are classified from the parse tree
  # (not the lexical Code.Fragment fallback). Operator/special atoms (`:%{}`, `:+`) are not navigable.
  describe "surround_context/2 atom literals (via literal_encoder)" do
    test "bare :atom and :erlang.foo operand classify natively and match Code.Fragment" do
      for {source, col} <- [{":atom", 3}, {"x = :ok", 6}, {":erlang.foo", 3}, {":erlang.foo", 9}] do
        assert Toxic.surround_context(source, {1, col}) ==
                 Code.Fragment.surround_context(source, {1, col}),
               "mismatch for #{inspect(source)} @#{col}"
      end
    end

    test "operator / special-form atoms are deferred (not classified as unquoted_atom)" do
      for {source, col} <- [{"[:%{}, :foo]", 3}, {"[:+, :x]", 3}, {"[:., :y]", 3}] do
        assert Toxic.surround_context(source, {1, col}) ==
                 Code.Fragment.surround_context(source, {1, col}),
               "mismatch for #{inspect(source)} @#{col}"
      end
    end

    test "keyword keys classify as :key (and :none on the colon)" do
      # Each entry: {source, column, expected Toxic context}. `Code.Fragment` only classifies
      # keyword keys as `{:key, _}` from Elixir 1.18 on, so Toxic's own classification is asserted on
      # every version and parity with the oracle is only checked on >= 1.18.
      cases = [
        {"[key: 1]", 3, {:key, ~c"key"}},
        {"[key: 1]", 5, :none},
        {"%{key: 1}", 4, {:key, ~c"key"}},
        {"foo(key: 1)", 7, {:key, ~c"key"}},
        {"[a: 1, bb: 2]", 8, {:key, ~c"bb"}}
      ]

      modern? = Version.match?(System.version(), ">= 1.18.0")

      for {source, col, expected} <- cases do
        result = Toxic.surround_context(source, {1, col})

        case expected do
          :none -> assert result == :none, "expected :none for #{inspect(source)} @#{col}"
          ctx -> assert %{context: ^ctx} = result, "mismatch for #{inspect(source)} @#{col}"
        end

        if modern? do
          assert result == Code.Fragment.surround_context(source, {1, col}),
                 "oracle mismatch for #{inspect(source)} @#{col}"
        end
      end
    end

    test "bare nil/true/false classify as :keyword, :nil/:true as :unquoted_atom" do
      for {source, col} <- [{"nil", 1}, {"true", 2}, {"false", 3}, {":nil", 2}, {":true", 3}] do
        assert Toxic.surround_context(source, {1, col}) ==
                 Code.Fragment.surround_context(source, {1, col}),
               "mismatch for #{inspect(source)} @#{col}"
      end
    end
  end
end
