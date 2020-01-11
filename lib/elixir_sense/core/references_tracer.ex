defmodule ElixirSense.Core.References.Tracer do
  @moduledoc """
  Elixir Compiler tracer that registers function calls
  """
  use Agent

  @type callee_t :: {module, atom, non_neg_integer}

  @type call_t :: %{
          callee: callee_t,
          file: nil | String.t(),
          line: nil | pos_integer
        }
  @type call_trace_t :: %{optional(callee_t) => [call_t]}

  @spec start_link(call_trace_t) :: Agent.on_start()
  def start_link(initial \\ %{}) do
    Agent.start_link(fn -> initial end, name: __MODULE__)
  end

  @spec get :: call_trace_t
  def get do
    Agent.get(__MODULE__, & &1)
  end

  @spec register_call(call_t) :: :ok
  def register_call(%{callee: callee} = call) do
    Agent.update(__MODULE__, fn calls ->
      updated_calls =
        case calls[callee] do
          nil -> [call]
          callee_calls -> [call | callee_calls]
        end

      calls |> Map.put(callee, updated_calls)
    end)
  end

  def trace({:imported_function, meta, module, name, arity}, env) do
    register_call(%{
      callee: {module, name, arity},
      file: env.file |> Path.relative_to_cwd(),
      line: meta[:line]
    })

    :ok
  end

  def trace({:remote_function, meta, module, name, arity}, env) do
    register_call(%{
      callee: {module, name, arity},
      file: env.file |> Path.relative_to_cwd(),
      line: meta[:line]
    })

    :ok
  end

  def trace(_trace, _env) do
    :ok
  end
end
