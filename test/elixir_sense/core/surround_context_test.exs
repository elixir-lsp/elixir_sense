defmodule ElixirSense.Core.SurroundContextTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Core.SurroundContext

  defp context_at(source, column) do
    %{context: context} = Code.Fragment.surround_context(source, {1, column})
    context
  end

  describe "to_binding/2 on struct with a dotted variable (crash regression)" do
    test "%nav.foo{} resolves the variable inside the struct dot" do
      context = context_at("%nav.foo{}", 7)

      assert context == {:struct, {:dot, {:local_or_var, ~c"nav"}, ~c"foo"}}
      assert SurroundContext.to_binding(context, nil) == {{:variable, :nav, :any}, :foo}
    end

    test "does not crash at any column of the struct dot" do
      for column <- 1..11,
          %{context: context} <- [Code.Fragment.surround_context("%nav.foo{}", {1, column})] do
        # any result is fine; the test fails only if to_binding raises
        _ = SurroundContext.to_binding(context, nil)
      end
    end

    test "%nav.foo{} at the variable part still resolves" do
      assert SurroundContext.to_binding(context_at("%nav.foo{}", 3), nil) ==
               {:variable, :nav, :any}
    end

    test "%nav.foo.bar{} resolves the nested dot" do
      context = context_at("%nav.foo.bar{}", 11)

      assert SurroundContext.to_binding(context, nil) ==
               {{:call, {:variable, :nav, :any}, :foo, []}, :bar}
    end

    test "%x.y{a: 1} does not crash" do
      context = context_at("%x.y{a: 1}", 4)

      assert SurroundContext.to_binding(context, nil) == {{:variable, :x, :any}, :y}
    end

    test "%__MODULE__.foo{} resolves to the current module" do
      context = context_at("%__MODULE__.foo{}", 13)

      assert SurroundContext.to_binding(context, MyModule) == {{:atom, MyModule}, :foo}
    end

    test "%__MODULE__.foo{} without a current module does not crash" do
      context = context_at("%__MODULE__.foo{}", 13)

      assert SurroundContext.to_binding(context, nil) == {nil, :foo}
    end

    test "%__MODULE__.foo.bar{} does not crash" do
      context = context_at("%__MODULE__.foo.bar{}", 19)

      assert {_, :bar} = SurroundContext.to_binding(context, MyModule)
    end
  end

  describe "to_binding/2 on other struct inner shapes" do
    test "%@attr.foo{} resolves the attribute" do
      context = context_at("%@attr.foo{}", 8)

      assert SurroundContext.to_binding(context, nil) == {{:attribute, :attr}, :foo}
    end

    test "%Foo.bar{} resolves the alias" do
      context = context_at("%Foo.bar{}", 7)

      assert SurroundContext.to_binding(context, nil) == {{:atom, Foo}, :bar}
    end

    test "%foo().bar{} does not crash" do
      context = context_at("%foo().bar{}", 8)

      assert {_, :bar} = SurroundContext.to_binding(context, nil)
    end
  end
end
