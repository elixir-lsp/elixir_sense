defmodule ElixirSense.Core.TypeInference.Occurrence do
  @moduledoc """
  ElixirSense-native occurrence typing — the *positive* half.

  This module narrows a scrutinee/condition variable to the type the *current*
  clause matched. The complementary *negative* half — subtracting the patterns
  earlier clauses already matched — is implemented in
  `ElixirSense.Core.Compiler.Clauses` (`expand_case`), which types each clause
  against a `{:difference, scrutinee, prior_patterns}` shape resolved by
  `ElixirSense.Core.Binding`. Together they give, for a union scrutinee:

      case System.get_env("X") do   # binary() | nil
        nil   -> :none
        value -> value   # narrowed to binary() (nil subtracted by Clauses)
      end

  Subtraction is only as precise as `Binding.difference/2` (it narrows union
  bases; opaque bases pass through unchanged).

  What *this* module handles — narrowing the scrutinee to the matched pattern
  within the branch:

      case x do
        %User{} = u -> u.<cursor>   # here `x` is a `%User{}`
        {:ok, v}    -> v.<cursor>   # here `x` is `{:ok, _}`
      end

  Refinements are computed over ElixirSense's shape vocabulary (see
  `ElixirSense.Core.TypeInference`). The module is intentionally
  version-independent and does **not** call into Elixir's `Module.Types`.

  ## Division of labour

  This module emits only positive facts; the negative (subtraction) side is the
  lazy `{:difference, ...}` shape built in `Clauses` and resolved by `Binding`.
  Keeping them separate means subtraction stays representable without a general
  negation operator: `Binding.difference/2` narrows a union base and leaves an
  opaque base untouched, so we never invent a "not X" shape we can't display.

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
