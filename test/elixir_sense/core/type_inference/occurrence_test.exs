defmodule ElixirSense.Core.TypeInference.OccurrenceTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Core.TypeInference.Occurrence

  defp var(name, version), do: {name, [version: version], nil}

  describe "scrutinee_refinements/2" do
    test "narrows a variable scrutinee to a tagged-tuple pattern" do
      assert Occurrence.scrutinee_refinements(var(:x, 0), {:ok, var(:v, 1)}) ==
               [{{:x, 0}, {:tuple, 2, [{:atom, :ok}, nil]}}]
    end

    test "narrows a variable scrutinee to a list pattern" do
      pattern = [{:|, [], [var(:h, 1), var(:t, 2)]}]

      assert [{{:x, 0}, {:list, _}}] =
               Occurrence.scrutinee_refinements(var(:x, 0), pattern)
    end

    test "narrows through a guard wrapper, ignoring the guard" do
      clause = {:when, [], [{:ok, var(:v, 1)}, {:is_binary, [], [var(:v, 1)]}]}

      assert Occurrence.scrutinee_refinements(var(:x, 0), clause) ==
               [{{:x, 0}, {:tuple, 2, [{:atom, :ok}, nil]}}]
    end

    test "returns [] when the scrutinee is not a plain variable" do
      assert Occurrence.scrutinee_refinements({:foo, [], []}, {:ok, var(:v, 1)}) == []
    end

    test "returns [] when the scrutinee variable has no version" do
      assert Occurrence.scrutinee_refinements({:x, [], nil}, {:ok, var(:v, 1)}) == []
    end

    test "returns [] for a bare-variable pattern (nothing more specific)" do
      assert Occurrence.scrutinee_refinements(var(:x, 0), var(:value, 1)) == []
    end

    test "ignores reserved pseudo-variables as scrutinee" do
      assert Occurrence.scrutinee_refinements({:__MODULE__, [version: 0], nil}, {:ok, var(:v, 1)}) ==
               []
    end
  end

  describe "condition_refinements/1" do
    test "refines via a guard-style predicate condition" do
      cond_ast = {{:., [], [:erlang, :is_map]}, [], [var(:x, 0)]}
      assert Occurrence.condition_refinements(cond_ast) == [{{:x, 0}, {:map, [], nil}}]
    end

    test "drops nil refinements (no usable information)" do
      # `not is_integer(x)` forgets the constraint -> nil, which must be dropped
      cond_ast =
        {{:., [], [:erlang, :not]}, [], [{{:., [], [:erlang, :is_integer]}, [], [var(:x, 0)]}]}

      assert Occurrence.condition_refinements(cond_ast) == []
    end
  end
end
