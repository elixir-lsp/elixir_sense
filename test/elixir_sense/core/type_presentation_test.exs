defmodule ElixirSense.Core.TypePresentationTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.State.VarInfo
  alias ElixirSense.Core.TypePresentation, as: TP

  @env %Binding{functions: __ENV__.functions, macros: __ENV__.macros}

  defp var_env(type) do
    %Binding{
      functions: __ENV__.functions,
      macros: __ENV__.macros,
      vars: [%VarInfo{version: 1, name: :x, type: type}]
    }
  end

  describe "render/1" do
    test "nil shape is :unknown (no information)" do
      assert TP.render(nil) == :unknown
    end

    test "scalars and literals" do
      assert TP.render(:atom) == {:ok, "atom()"}
      assert TP.render(:integer) == {:ok, "integer()"}
      assert TP.render({:atom, :ok}) == {:ok, ":ok"}
      assert TP.render({:atom, nil}) == {:ok, "nil"}
      assert TP.render({:atom, URI}) == {:ok, "URI"}
      assert TP.render({:integer, 5}) == {:ok, "5"}
      assert TP.render({:integer, nil}) == {:ok, "integer()"}
      assert TP.render({:binary, "x"}) == {:ok, ~s("x")}
      assert TP.render({:binary, nil}) == {:ok, "binary()"}
    end

    test "containers" do
      assert TP.render({:list, {:atom, :ok}}) == {:ok, "[:ok]"}
      assert TP.render({:list, :empty}) == {:ok, "[]"}

      assert TP.render({:tuple, 2, [{:atom, :ok}, {:integer, nil}]}) ==
               {:ok, "{:ok, integer()}"}

      assert TP.render({:map, [foo: {:binary, nil}], nil}) == {:ok, "%{foo: binary()}"}
      assert TP.render({:map, [], nil}) == {:ok, "map()"}
    end

    test "structs drop the __struct__ field" do
      shape = {:struct, [__struct__: {:atom, URI}, host: {:binary, nil}], {:atom, URI}, nil}
      assert TP.render(shape) == {:ok, "%URI{host: binary()}"}
      assert TP.render({:struct, [], {:atom, URI}, nil}) == {:ok, "%URI{}"}
    end

    test "unions" do
      assert TP.render({:union, [{:atom, :a}, {:atom, :b}]}) == {:ok, ":a | :b"}
    end

    test "functions" do
      assert TP.render({:fun, 0}) == {:ok, "(-> term())"}
      assert TP.render({:fun, 2}) == {:ok, "(term(), term() -> term())"}
      assert TP.render({:fun, [{:atom, :ok}], {:integer, nil}}) == {:ok, "(:ok -> integer())"}
      assert TP.render({:fun, [nil, nil], nil}) == {:ok, "(term(), term() -> term())"}
      assert TP.render({:fun_clauses, [{[nil], {:atom, :ok}}]}) == {:ok, "(term() -> :ok)"}

      assert TP.render({:fun_clauses, [{[], {:atom, :ok}}, {[{:integer, nil}], {:atom, :error}}]}) ==
               {:ok, "(-> :ok) | (integer() -> :error)"}
    end

    test "an unknown leaf inside a structure renders as term()" do
      assert TP.render({:tuple, 2, [{:atom, :ok}, nil]}) == {:ok, "{:ok, term()}"}
    end
  end

  describe "resolve_and_render/2 (never leaks thunks)" do
    test "resolves a {:difference} thunk to the narrowed type" do
      env = var_env({:union, [{:binary, nil}, {:atom, nil}]})

      assert TP.resolve_and_render(env, {:difference, {:variable, :x, 1}, {:atom, nil}}) ==
               {:ok, "binary()"}
    end

    test "resolves variable + projection thunks" do
      env = var_env({:tuple, 2, [{:atom, :ok}, {:integer, nil}]})
      assert TP.resolve_and_render(env, {:tuple_nth, {:variable, :x, 1}, 1}) == {:ok, "integer()"}
    end

    test "an unresolvable difference base renders as none(), not a thunk" do
      assert TP.resolve_and_render(@env, {:difference, {:variable, :missing, 9}, {:atom, :a}}) ==
               {:ok, "none()"}
    end
  end

  describe "render_var/2" do
    test "falls back to the structural shape when no native descriptor" do
      var = %VarInfo{version: 1, name: :x, type: {:atom, :ok}, elixir_types_descr: nil}
      assert TP.render_var(@env, var) == {:ok, ":ok"}
    end

    test "prefers the native descriptor when present" do
      descr = Module.Types.Descr.integer()
      var = %VarInfo{version: 1, name: :x, type: nil, elixir_types_descr: descr}
      assert TP.render_var(@env, var) == {:ok, "integer()"}
    end
  end
end
