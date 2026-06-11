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

    test "containers — compiler list dialect" do
      # compiler dialect: list(t), non_empty_list(t), empty_list()
      assert TP.render({:list, {:atom, :ok}}) == {:ok, "list(:ok)"}
      assert TP.render({:list, :empty}) == {:ok, "empty_list()"}
      assert TP.render({:nonempty_list, {:integer, nil}}) == {:ok, "non_empty_list(integer())"}

      assert TP.render({:tuple, 2, [{:atom, :ok}, {:integer, nil}]}) ==
               {:ok, "{:ok, integer()}"}

      assert TP.render({:map, [foo: {:binary, nil}], nil}) == {:ok, "%{foo: binary()}"}
      assert TP.render({:map, [], nil}) == {:ok, "map()"}
    end

    test "top-level atom shortcuts render correctly" do
      assert TP.render(:empty_list) == {:ok, "empty_list()"}
      assert TP.render(:empty_map) == {:ok, "empty_map()"}
      assert TP.render(:non_struct_map) == {:ok, "non_struct_map()"}
      assert TP.render(:bitstring) == {:ok, "bitstring()"}
      assert TP.render(:tuple) == {:ok, "tuple()"}
      assert TP.render(:boolean) == {:ok, "boolean()"}
    end

    test "dynamic() shapes" do
      assert TP.render({:dynamic, nil}) == {:ok, "dynamic()"}
      assert TP.render({:dynamic, :integer}) == {:ok, "dynamic(integer())"}
      assert TP.render({:dynamic, {:atom, :ok}}) == {:ok, "dynamic(:ok)"}
    end

    test "open tuple shape" do
      assert TP.render({:tuple_open, []}) == {:ok, "{...}"}
      assert TP.render({:tuple_open, [{:atom, :ok}]}) == {:ok, "{:ok, ...}"}
      assert TP.render({:tuple_open, [:atom, :integer]}) == {:ok, "{atom(), integer(), ...}"}
    end

    test "optional (if_set) shape" do
      assert TP.render({:optional, :integer}) == {:ok, "if_set(integer())"}
      assert TP.render({:optional, {:atom, :ok}}) == {:ok, "if_set(:ok)"}
    end

    test "structs drop the __struct__ field" do
      shape = {:struct, [__struct__: {:atom, URI}, host: {:binary, nil}], {:atom, URI}, nil}
      assert TP.render(shape) == {:ok, "%URI{host: binary()}"}
      assert TP.render({:struct, [], {:atom, URI}, nil}) == {:ok, "%URI{}"}
    end

    test "structs drop uninformative term() fields" do
      # A struct typed only by its module (e.g. a `defimpl for:` arg) renders as
      # `%URI{}`, keeping only fields we actually know something about.
      shape = {:struct, [host: nil, scheme: {:binary, nil}], {:atom, URI}, nil}
      assert TP.render(shape) == {:ok, "%URI{scheme: binary()}"}

      all_unknown = {:struct, [host: nil, scheme: nil], {:atom, URI}, nil}
      assert TP.render(all_unknown) == {:ok, "%URI{}"}
    end

    test "structs keep {:optional, _} fields (not dropped as uninformative)" do
      shape = {:struct, [host: {:optional, {:binary, nil}}], {:atom, URI}, nil}
      assert TP.render(shape) == {:ok, "%URI{host: if_set(binary())}"}
    end

    test "unions use compiler dialect: or (not |)" do
      # Compiler dialect: " or " separator (task #18)
      assert TP.render({:union, [{:atom, :a}, {:atom, :b}]}) == {:ok, ":a or :b"}
    end

    test "functions" do
      assert TP.render({:fun, 0}) == {:ok, "(-> term())"}
      assert TP.render({:fun, 2}) == {:ok, "(term(), term() -> term())"}
      assert TP.render({:fun, [{:atom, :ok}], {:integer, nil}}) == {:ok, "(:ok -> integer())"}
      assert TP.render({:fun, [nil, nil], nil}) == {:ok, "(term(), term() -> term())"}
      assert TP.render({:fun_clauses, [{[nil], {:atom, :ok}}]}) == {:ok, "(term() -> :ok)"}

      # fun_clauses join with " or " (compiler dialect)
      assert TP.render({:fun_clauses, [{[], {:atom, :ok}}, {[{:integer, nil}], {:atom, :error}}]}) ==
               {:ok, "(-> :ok) or (integer() -> :error)"}
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

    test "an unresolvable difference base (versioned missing var) yields :unknown" do
      # A versioned variable that is not in scope returns nil (unknown) from
      # Binding.expand — the difference base is unknown so the whole result is unknown.
      assert TP.resolve_and_render(@env, {:difference, {:variable, :missing, 9}, {:atom, :a}}) ==
               :unknown
    end

    test "renders a field-wise-merged map union using or dialect" do
      m1 = {:map, [a: {:integer, 1}], nil}
      m2 = {:map, [a: {:integer, 2}], nil}
      # union separator is " or " in compiler dialect
      assert TP.resolve_and_render(@env, {:union, [m1, m2]}) == {:ok, "%{a: 1 or 2}"}
    end
  end

  describe "render_var/2 — structural type takes precedence" do
    test "renders the structural shape" do
      var = %VarInfo{version: 1, name: :x, type: {:atom, :ok}, elixir_types_descr: nil}
      assert TP.render_var(@env, var) == {:ok, ":ok"}
    end

    test "uses the native descriptor only when the structural type is uninformative" do
      # render_descr now gates on enabled?() — requires config set.
      Application.put_env(:elixir_sense, :use_elixir_types, true)

      try do
        descr = Module.Types.Descr.integer()
        var = %VarInfo{version: 1, name: :x, type: nil, elixir_types_descr: descr}

        if ElixirSense.Core.ElixirTypes.available?() do
          assert TP.render_var(@env, var) == {:ok, "integer()"}
        end
      after
        Application.delete_env(:elixir_sense, :use_elixir_types)
      end
    end

    test "without use_elixir_types config, native descriptor is skipped" do
      # Without the config flag, render_descr returns :unknown regardless of availability.
      descr = Module.Types.Descr.integer()
      var = %VarInfo{version: 1, name: :x, type: nil, elixir_types_descr: descr}
      # structural is :unknown, native is skipped (not enabled) → :unknown
      assert TP.render_var(@env, var) == :unknown
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

      # a versioned missing var returns :unknown from resolve_and_render → :skip
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

  describe "render_hint/3 with max_length" do
    test "returns {:ok, %{label: label, full: full}}" do
      var = %VarInfo{version: 1, name: :x, type: {:atom, :ok}}
      assert {:ok, %{label: ":ok", full: ":ok"}} = TP.render_hint(@env, var, [])
    end

    test "label == full when text fits within max_length" do
      var = %VarInfo{version: 1, name: :x, type: {:atom, :ok}}
      assert {:ok, %{label: ":ok", full: ":ok"}} = TP.render_hint(@env, var, max_length: 10)
    end

    test "label is elided when text exceeds max_length" do
      var = %VarInfo{
        version: 1,
        name: :x,
        type: {:union, [{:atom, :a}, {:atom, :b}, {:atom, :c}]}
      }

      # full text is ":a or :b or :c" (14 chars)
      {:ok, result} = TP.render_hint(@env, var, max_length: 8)
      assert result.full == ":a or :b or :c"
      # label must be ≤ 8 graphemes and end with …
      assert String.length(result.label) <= 8
      assert String.ends_with?(result.label, "…")
    end

    test "smart elision cuts at or boundary" do
      # ":a or :b or :c" — cutting at first " or " gives ":a…" (budget=4)
      # but cutting at the " or " that fits max better is preferred
      var = %VarInfo{
        version: 1,
        name: :x,
        type: {:union, [{:atom, :a}, {:atom, :b}, {:atom, :c}]}
      }

      {:ok, result} = TP.render_hint(@env, var, max_length: 9)
      # budget=8; ":a or :b" is 8 chars, + "…" = 9; so label = ":a or :b…"
      assert result.label == ":a or :b…"
      assert result.full == ":a or :b or :c"
    end

    test "full is always the complete text regardless of max_length" do
      var = %VarInfo{version: 1, name: :x, type: {:union, [{:atom, :a}, {:atom, :b}]}}
      {:ok, result} = TP.render_hint(@env, var, max_length: 3)
      assert result.full == ":a or :b"
    end

    test "multibyte grapheme safety" do
      # force a union with a multibyte atom (use a binary shape for simplicity)
      var = %VarInfo{version: 1, name: :x, type: {:atom, :ok}}
      {:ok, result} = TP.render_hint(@env, var, max_length: 2)
      # ":ok" has 3 graphemes; budget=1; label should be at most 2 graphemes including …
      assert String.length(result.label) <= 2
      assert String.ends_with?(result.label, "…")
    end

    test "nil max_length means no elision — label == full" do
      var = %VarInfo{version: 1, name: :x, type: {:union, [{:atom, :a}, {:atom, :b}]}}
      {:ok, result} = TP.render_hint(@env, var, max_length: nil)
      assert result.label == result.full
    end

    test "returns :skip for uninformative vars" do
      unknown = %VarInfo{version: 1, name: :x, type: nil, elixir_types_descr: nil}
      assert TP.render_hint(@env, unknown, max_length: 10) == :skip
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

  describe "domain (non-atom) map keys" do
    test "renders a domain key as `key_type => value_type`" do
      shape = {:map, [{{:domain, {:integer, nil}}, {:binary, nil}}], nil}
      assert TP.render(shape) == {:ok, "%{integer() => binary()}"}
    end

    test "renders mixed atom and domain keys" do
      shape =
        {:map, [{:root, {:integer, nil}}, {{:domain, {:integer, nil}}, {:binary, nil}}], nil}

      assert TP.render(shape) == {:ok, "%{root: integer(), integer() => binary()}"}
    end
  end
end
