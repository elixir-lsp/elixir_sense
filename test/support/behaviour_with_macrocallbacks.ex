defmodule ElixirSenseExample.BehaviourWithMacrocallback do
  @doc """
  A required macrocallback
  """
  @macrocallback required(atom) :: Macro.t()

  @doc """
  An optional macrocallback
  """
  if Version.match?(System.version(), ">= 1.8.0") do
    @macrocallback optional(a) :: Macro.t() when a: atom
  else
    # upper version does not compile on 1.7
    @macrocallback optional(atom) :: Macro.t()
  end

  @optional_callbacks [optional: 1]
end

defmodule ElixirSenseExample.BehaviourWithMacrocallback.Impl do
  @behaviour ElixirSenseExample.BehaviourWithMacrocallback
  defmacro required(var), do: Macro.expand(var, __CALLER__)
  defmacro optional(var), do: Macro.expand(var, __CALLER__)

  @doc """
  some macro
  """
  if Version.match?(System.version(), ">= 1.8.0") do
    @spec some(integer) :: Macro.t()
    @spec some(b) :: Macro.t() when b: float
  else
    # upper version does not compile on 1.7
    @spec some(integer) :: Macro.t()
    @spec some(float) :: Macro.t()
  end

  defmacro some(var), do: Macro.expand(var, __CALLER__)

  @doc """
  some macro with default arg
  """
  @spec with_default(atom, list, integer) :: Macro.t()
  defmacro with_default(a \\ :asdf, b, var \\ 0), do: Macro.expand({a, b, var}, __CALLER__)
end
