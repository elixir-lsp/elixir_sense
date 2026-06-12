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

  describe "resolve/3 alias fallback (pinned to real Elixir semantics)" do
    # Each assertion was checked against what the real Elixir compiler resolves
    # (compiling tiny modules and reading back __MODULE__-derived atoms). These
    # are the cases where Binding's old resolve_same_root_alias heuristic
    # disagreed with the canonical path; ModuleResolver is now the single source.

    test "alias Foo.Bar then Bar.Baz -> Foo.Bar.Baz" do
      env = %{module: nil, aliases: [{Bar, Foo.Bar}]}
      ast = {:__aliases__, [], [:Bar, :Baz]}
      assert ModuleResolver.resolve(ast, env) == {:ok, Foo.Bar.Baz}
    end

    test "alias MyApp.String then String -> MyApp.String (alias wins over top-level)" do
      env = %{module: nil, aliases: [{String, MyApp.String}]}
      ast = {:__aliases__, [], [:String]}
      assert ModuleResolver.resolve(ast, env) == {:ok, MyApp.String}
    end

    test "nested defmodule alias entry: Inner -> Outer.Inner" do
      # A nested `defmodule Inner` inside `Outer` records {Inner, Outer.Inner}.
      env = %{module: Outer, aliases: [{Inner, Outer.Inner}]}
      ast = {:__aliases__, [], [:Inner]}
      assert ModuleResolver.resolve(ast, env) == {:ok, Outer.Inner}
    end

    test "unaliased single part is NOT resolved to a parent/sibling module" do
      # Real Elixir: in module `Sib.A`, an unaliased `B` is `Elixir.B`, NOT `Sib.B`.
      # The removed resolve_parent_alias fallback used to (wrongly) return Sib.B.
      env = %{module: Sib.A, aliases: []}
      ast = {:__aliases__, [], [:B]}
      assert ModuleResolver.resolve(ast, env) == {:ok, B}
    end

    test "unaliased multi-part ref stays fully-qualified even when first part matches own root" do
      # Real Elixir: in `Rooty.Child`, unaliased `Rooty.Sibling` -> `Rooty.Sibling`.
      # The old root-sharing heuristic happened to return the same answer, but for
      # the wrong reason; the canonical path keeps the reference as written.
      env = %{module: Rooty.Child, aliases: []}
      ast = {:__aliases__, [], [:Rooty, :Sibling]}
      assert ModuleResolver.resolve(ast, env) == {:ok, Rooty.Sibling}
    end

    test "unaliased multi-part ref whose first part matches own LAST segment stays as written" do
      # Real Elixir: in `Zed.Child`, unaliased `Child.Deep` -> `Child.Deep`.
      env = %{module: Zed.Child, aliases: []}
      ast = {:__aliases__, [], [:Child, :Deep]}
      assert ModuleResolver.resolve(ast, env) == {:ok, Child.Deep}
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
