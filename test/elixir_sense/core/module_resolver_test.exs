defmodule ElixirSense.Core.ModuleResolverTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Core.ModuleResolver

  describe "resolve/3 atoms" do
    test "plain elixir module atom" do
      assert ModuleResolver.resolve(List, %{module: nil, aliases: []}) == {:ok, List}
    end

    test "erlang atom module" do
      assert ModuleResolver.resolve(:lists, %{module: nil, aliases: []}) == {:ok, :lists}
    end

    test "Elixir-prefixed atom" do
      assert ModuleResolver.resolve(:"Elixir.Foo.Bar", %{module: nil, aliases: []}) ==
               {:ok, Foo.Bar}
    end
  end

  describe "resolve/3 __aliases__" do
    test "plain alias, no aliases in env" do
      ast = {:__aliases__, [], [:Foo, :Bar]}
      assert ModuleResolver.resolve(ast, %{module: nil, aliases: []}) == {:ok, Foo.Bar}
    end

    test "nested alias" do
      ast = {:__aliases__, [], [:Foo, :Bar, :Baz]}
      assert ModuleResolver.resolve(ast, %{module: nil, aliases: []}) == {:ok, Foo.Bar.Baz}
    end

    test "aliased alias: alias Foo.Bar, as: B -> B resolves to Foo.Bar" do
      # aliases stored as {alias_module, expansion}
      env = %{module: nil, aliases: [{B, Foo.Bar}]}
      ast = {:__aliases__, [], [:B]}
      assert ModuleResolver.resolve(ast, env) == {:ok, Foo.Bar}
    end

    test "aliased alias with submodule: B.Sub -> Foo.Bar.Sub" do
      env = %{module: nil, aliases: [{B, Foo.Bar}]}
      ast = {:__aliases__, [], [:B, :Sub]}
      assert ModuleResolver.resolve(ast, env) == {:ok, Foo.Bar.Sub}
    end

    test "alias expanding to erlang module" do
      env = %{module: nil, aliases: [{E, :erl}]}
      ast = {:__aliases__, [], [:E]}
      assert ModuleResolver.resolve(ast, env) == {:ok, :erl}
    end

    test "Elixir-prefixed alias parts" do
      ast = {:__aliases__, [], [Elixir, :Foo]}
      assert ModuleResolver.resolve(ast, %{module: nil, aliases: []}) == {:ok, Foo}
    end

    test "__MODULE__-prefixed alias resolves against current module" do
      env = %{module: My.Mod, aliases: []}
      ast = {:__aliases__, [], [{:__MODULE__, [], nil}, :Sub]}
      assert ModuleResolver.resolve(ast, env) == {:ok, My.Mod.Sub}
    end

    test "__MODULE__-prefixed alias with no current module drops prefix" do
      env = %{module: nil, aliases: []}
      ast = {:__aliases__, [], [{:__MODULE__, [], nil}, :Sub]}
      assert ModuleResolver.resolve(ast, env) == {:ok, Sub}
    end
  end

  describe "resolve/3 __MODULE__" do
    test "bare __MODULE__ resolves to current module" do
      env = %{module: My.Mod, aliases: []}
      assert ModuleResolver.resolve({:__MODULE__, [], nil}, env) == {:ok, My.Mod}
    end

    test "bare __MODULE__ with no current module is :error" do
      assert ModuleResolver.resolve({:__MODULE__, [], nil}, %{module: nil, aliases: []}) == :error
    end
  end

  describe "resolve/3 dotted nesting" do
    test "alias base with atom nested" do
      env = %{module: nil, aliases: [{B, Foo.Bar}]}
      ast = {{:., [], [{:__aliases__, [], [:B]}, :Sub]}, [], []}
      assert ModuleResolver.resolve(ast, env) == {:ok, Foo.Bar.Sub}
    end

    test "atom base with atom nested" do
      ast = {{:., [], [Foo, :Bar]}, [], []}
      assert ModuleResolver.resolve(ast, %{module: nil, aliases: []}) == {:ok, Foo.Bar}
    end
  end

  describe "resolve/3 out-of-scope receivers -> :error" do
    test "attribute receiver is out of scope" do
      ast = {:@, [], [{:my_attr, [], nil}]}
      assert ModuleResolver.resolve(ast, %{module: nil, aliases: []}) == :error
    end

    test "variable receiver is out of scope" do
      ast = {:some_var, [], nil}
      assert ModuleResolver.resolve(ast, %{module: nil, aliases: []}) == :error
    end

    test "unrelated AST is :error" do
      assert ModuleResolver.resolve({:foo, [], [1, 2]}, %{module: nil, aliases: []}) == :error
    end
  end

  describe "env shape tolerance" do
    test "missing aliases key defaults to []" do
      assert ModuleResolver.resolve({:__aliases__, [], [:Foo]}, %{module: nil}) == {:ok, Foo}
    end

    test "non-map env yields :error for forms needing env, ok for atoms" do
      assert ModuleResolver.resolve(List, nil) == {:ok, List}
      assert ModuleResolver.resolve({:__MODULE__, [], nil}, nil) == :error
    end
  end
end
