defmodule ElixirSenseExample.BehaviourWithMacrocallback do
  @doc """
  A required macrocallback
  """
  @macrocallback required(atom) :: Macro.t()

  @doc """
  An optional macrocallback
  """
  @macrocallback optional(atom) :: Macro.t()
  @optional_callbacks [optional: 1]
end

defmodule ElixirSenseExample.BehaviourWithMacrocallback.Impl do
  @behaviour ElixirSenseExample.BehaviourWithMacrocallback
  defmacro required(var), do: Macro.expand(var, __CALLER__)
  defmacro optional(var), do: Macro.expand(var, __CALLER__)
end
