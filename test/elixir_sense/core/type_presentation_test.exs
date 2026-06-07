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
      assert TP.render({:nonempty_list, {:integer, nil}}) == {:ok, "[integer(), ...]"}

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

    test "renders a field-wise-merged map union" do
      m1 = {:map, [a: {:integer, 1}], nil}
      m2 = {:map, [a: {:integer, 2}], nil}
      assert TP.resolve_and_render(@env, {:union, [m1, m2]}) == {:ok, "%{a: 1 | 2}"}
    end
  end

  describe "render_var/2 — structural type takes precedence" do
    test "renders the structural shape" do
      var = %VarInfo{version: 1, name: :x, type: {:atom, :ok}, elixir_types_descr: nil}
      assert TP.render_var(@env, var) == {:ok, ":ok"}
    end

    test "uses the native descriptor only when the structural type is uninformative" do
      descr = Module.Types.Descr.integer()
      var = %VarInfo{version: 1, name: :x, type: nil, elixir_types_descr: descr}
      assert TP.render_var(@env, var) == {:ok, "integer()"}
    end

    test "structural branch refinement wins over a broader/stale descriptor" do
      # y : binary() | nil; x is `y` with nil subtracted (a later clause).
      y = %VarInfo{version: 2, name: :y, type: {:union, [{:binary, nil}, {:atom, nil}]}}

      env = %Binding{
        functions: __ENV__.functions,
        macros: __ENV__.macros,
        vars: [y]
      }

      # the descriptor is the broader, pre-narrowing `binary() | nil`
      descr =
        Module.Types.Descr.union(Module.Types.Descr.binary(), Module.Types.Descr.atom([nil]))

      x = %VarInfo{
        version: 1,
        name: :x,
        type: {:difference, {:variable, :y, 2}, {:atom, nil}},
        elixir_types_descr: descr
      }

      # structural narrowing (binary()) must win over the descriptor (binary() | nil)
      assert TP.render_var(env, x) == {:ok, "binary()"}
      assert TP.render_hint(env, x) == {:ok, "binary()"}
    end
  end

  describe "render_hint/2" do
    test "skips uninformative types" do
      unknown = %VarInfo{version: 1, name: :x, type: nil, elixir_types_descr: nil}
      assert TP.render_hint(@env, unknown) == :skip

      # a difference with an unresolvable base -> none() -> :skip (not "none()")
      bottom = %VarInfo{
        version: 1,
        name: :x,
        type: {:difference, {:variable, :z, 9}, {:atom, :a}}
      }

      assert TP.render_hint(@env, bottom) == :skip
    end

    test "returns informative text" do
      var = %VarInfo{version: 1, name: :x, type: {:atom, :ok}}
      assert TP.render_hint(@env, var) == {:ok, ":ok"}
    end
  end

  describe "resolve_shape/2" do
    test "returns the resolved shape" do
      assert TP.resolve_shape(@env, {:atom, :ok}) == {:ok, {:atom, :ok}}
    end

    test "nil is :unknown" do
      assert TP.resolve_shape(@env, nil) == :unknown
    end
  end

  describe "fields_for_receiver/2" do
    test "maps a map's fields to rendered types" do
      shape = {:map, [a: {:integer, 1}, b: {:binary, nil}], nil}
      assert TP.fields_for_receiver(@env, shape) == %{a: "1", b: "binary()"}
    end

    test "drops __struct__ for structs" do
      shape = {:struct, [__struct__: {:atom, URI}, host: {:binary, "h"}], {:atom, URI}, nil}
      fields = TP.fields_for_receiver(@env, shape)
      assert fields[:host] == ~s("h")
      refute Map.has_key?(fields, :__struct__)
    end

    test "is empty for a non-map/struct receiver" do
      assert TP.fields_for_receiver(@env, {:integer, 1}) == %{}
    end

    test "skips :not_set (known-absent) keys" do
      shape = {:map, [present: {:integer, 1}, missing: :not_set], nil}
      assert TP.fields_for_receiver(@env, shape) == %{present: "1"}
    end
  end

  describe "not_set rendering" do
    test "renders a known-absent map key" do
      assert TP.render({:map, [foo: :not_set], nil}) == {:ok, "%{foo: not_set()}"}
    end
  end
end
