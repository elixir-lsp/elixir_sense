defmodule ElixirSenseExample.Options.Foo1 do
  @spec bar([{:option1, integer()}]) :: :ok
  def bar(options), do: :ok
end

defmodule ElixirSenseExample.Options.Foo do
  @spec bar([{:option1, integer()} | {:option2, String.t()}]) :: :ok
  def bar(options), do: :ok
end

defmodule ElixirSenseExample.Options.With do
  @spec bar(x) :: :ok when x: [{:option1, integer()} | {:option2, String.t()}]
  def bar(options), do: :ok
end
