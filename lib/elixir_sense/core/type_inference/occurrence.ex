defmodule ElixirSense.Core.TypeInference.Occurrence do
  @moduledoc """
  ElixirSense-native occurrence typing (the always-on "L1" layer).

  When a `case`/`with` scrutinee — or a `cond`/`with` condition — is a plain
  variable, the clause that matched tells us something more specific about that
  variable *inside the clause body*. For example:

      case x do
        %User{} = u -> u.<cursor>   # here `x` is a `%User{}`
        {:ok, v}    -> v.<cursor>   # here `x` is `{:ok, _}`
      end

  This module computes those per-branch refinements over ElixirSense's shape
  vocabulary (see `ElixirSense.Core.TypeInference`). It is intentionally
  version-independent and does **not** call into Elixir's `Module.Types`; the
  optional, more precise reverse-arrow pass (Elixir 1.20+) lives behind the
  capability probe in `ElixirSense.Core.ElixirTypes` and only ever *improves* on
  the result computed here.

  ## Scope and conservatism

  The shape model is structural and has no negation/difference, so this layer
  performs **positive narrowing only** (narrowing the scrutinee to the type the
  branch matched). Cross-clause subtraction (e.g. inferring `value` is non-`nil`
  in the second clause of `nil -> ...; value -> ...`) requires negation shapes
  (`not_set`, non-empty, arity bounds) that the shape model does not yet carry;
  those refinements are deliberately left out until that vocabulary exists, in
  line with the "do not subtract when unsure" rule.

  Refinements are returned as a list of `{{name, version}, type}` tuples, the
  same shape `ElixirSense.Core.Compiler.State.merge_inferred_types/2` consumes
  (it intersects with any existing type and ignores variables absent from the
  current scope).
  """

  alias ElixirSense.Core.TypeInference
  alias ElixirSense.Core.TypeInference.Guard

  @reserved_vars [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__, :_]

  @doc """
  Per-branch refinements for one `case`/`with` clause.

  When `scrutinee_ast` is a plain variable and the clause pattern implies a
  concrete shape, narrow that variable to the pattern's type within the branch.
  Returns `[]` when nothing can be said (e.g. the scrutinee is not a variable,
  or the pattern is itself just a variable).
  """
  def scrutinee_refinements(scrutinee_ast, clause_pattern_ast) do
    case scrutinee_var(scrutinee_ast) do
      {:ok, var_key} ->
        pattern = unwrap_guard(clause_pattern_ast)
        type = TypeInference.type_of(pattern, :match)

        if informative?(type) do
          [{var_key, type}]
        else
          []
        end

      :error ->
        []
    end
  end

  @doc """
  Refinements for one `cond`/`if`-style clause condition.

  In the branch where the condition holds, predicate-shaped conditions
  (`is_integer(x)`, `is_map(x)`, `x.key`, …) refine the tested variables exactly
  as they would in a guard. Reuses `ElixirSense.Core.TypeInference.Guard` so the
  two paths stay consistent. Returns a list of `{{name, version}, type}`.
  """
  def condition_refinements(condition_ast) do
    condition_ast
    |> Guard.type_information_from_guards()
    |> Enum.reject(fn {_key, type} -> is_nil(type) end)
  end

  # Strip a `when` guard wrapper, keeping only the pattern part.
  defp unwrap_guard({:when, _meta, [pattern | _guards]}), do: pattern
  defp unwrap_guard(other), do: other

  # A type is informative if it tells us more than "some variable" / nothing.
  defp informative?(nil), do: false
  defp informative?(:none), do: false
  defp informative?({:variable, _name, _version}), do: false
  defp informative?(_type), do: true

  # Extract the {name, version} key of a scrutinee that is a plain variable.
  defp scrutinee_var({name, meta, ctx})
       when is_atom(name) and is_atom(ctx) and name not in @reserved_vars do
    case Keyword.fetch(meta, :version) do
      {:ok, version} -> {:ok, {name, version}}
      :error -> :error
    end
  end

  defp scrutinee_var(_other), do: :error
end
