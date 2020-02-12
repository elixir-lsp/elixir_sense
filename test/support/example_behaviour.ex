defmodule MyMacros do
  defmodule Nested do
  end

  defmodule One do
  end

  defmodule Two.Three do
  end
end

defmodule MyImports do
  defmodule NestedImports do
  end

  defmodule OneImports do
  end

  defmodule Two.ThreeImports do
  end
end

defmodule UseWithCallbacks do
  defmacro __before_compile__(_env) do
    quote do: :ok
  end
end

defmodule ElixirSenseExample.Delegates do
  def delegated_func(_a), do: :ok
end

defprotocol ProtocolOutside do
  def reverse(term)
end

defmodule ElixirSenseExample.ExampleBehaviour do
  @moduledoc """
  Example of a module that has a __using__ that defines callbacks. Patterned directly off of GenServer from Elixir 1.8.0
  """

  @type name :: any

  @typedoc "The server reference"
  @type server :: pid | name | {atom, node}

  @typedoc """
  Tuple describing the client of a call request.
  `pid` is the PID of the caller and `tag` is a unique term used to identify the
  call.
  """
  @type from :: {pid, tag :: term}

  @callback handle_call(request :: term, from, state :: term) ::
              {:reply, reply, new_state}
              | {:reply, reply, new_state, timeout | :hibernate | {:continue, term}}
              | {:noreply, new_state}
              | {:noreply, new_state, timeout | :hibernate | {:continue, term}}
              | {:stop, reason, reply, new_state}
              | {:stop, reason, new_state}
            when reply: term, new_state: term, reason: term

  alias ElixirSenseExample.ExampleBehaviour

  defmacro __using__(opts) do
    quote location: :keep, bind_quoted: [opts: opts] do
      @behaviour ExampleBehaviour

      if Module.get_attribute(__MODULE__, :doc) == nil do
        @doc """
        Returns a specification to start this module under a supervisor.
        See `Supervisor`.
        """
      end

      # TODO: Remove this on Elixir v2.0
      @before_compile UseWithCallbacks

      @doc false
      def handle_call(msg, _from, state) do
        proc =
          case Process.info(self(), :registered_name) do
            {_, []} -> self()
            {_, name} -> name
          end

        # We do this to trick Dialyzer to not complain about non-local returns.
        case :erlang.phash2(1, 1) do
          0 ->
            raise "attempted to call ExampleBehaviour #{inspect(proc)} but no handle_call/3 clause was provided"

          1 ->
            {:stop, {:bad_call, msg}, state}
        end
      end

      defoverridable handle_call: 3

      alias MyModule.Some.Nested, as: Utils
      alias MyModule.Other.Nested
      alias :ets, as: Ets
      alias MyModule.{One, Two.Three}
      alias MyModule.{Four}
      alias :lists
      alias Elixir.Three.OutsideOfMyModule
      require MyMacros
      require MyMacros.Nested, as: NestedMacros
      require :ets, as: ErlangMacros
      require MyMacros.{One, Two.Three}
      import Some.{List}, only: []
      import MyImports
      import MyImports.NestedImports
      import MyImports.{OneImports, Two.ThreeImports}
      import :lists, only: []
      @my_attribute "my_attr"

      defp private_func, do: @my_attribute
      def public_func, do: :ok

      defp private_func_arg(a), do: :ok
      def public_func_arg(b, a \\ "def"), do: :ok

      defmacrop private_macro, do: :ok
      defmacro public_macro, do: :ok

      defmacrop private_macro_arg(a), do: :ok
      defmacro public_macro_arg(a), do: :ok

      defguardp private_guard when 1 == 1
      defguard public_guard when 1 == 1

      defguardp private_guard_arg(a) when is_integer(a)
      defguard public_guard_arg(a) when is_integer(a)

      defmodule Nested do
        def public_func_nested_arg(a), do: :ok

        defmodule Nested.Child do
          def public_func_nested_child_arg(a), do: :ok
        end
      end

      defmodule Elixir.Outside do
        def public_func_nested_arg(a), do: :ok
      end

      defmodule Deeply.Nested do
        def public_func_deeply_nested_arg(a), do: :ok
      end

      defprotocol ProtocolEmbedded do
        def reverse(term)
      end

      defimpl ProtocolEmbedded, for: String do
        def reverse(a), do: :ok
      end

      defimpl ProtocolOutside, for: String do
        def reverse(a), do: :ok
      end

      defdelegate delegated_func, to: ElixirSenseExample.Delegates
    end
  end

  defmacro __before_compile__(env) do
    IO.puts("BEFORE COMPILE!")

    unless Module.defines?(env.module, {:init, 1}) do
      message = """
      function init/1 required by behaviour GenServer is not implemented \
      (in module #{inspect(env.module)}).
      We will inject a default implementation for now:
      def init(args) do
      {:ok, args}
      end
      You can copy the implementation above or define your own that converts \
      the arguments given to GenServer.start_link/3 to the server state.
      """

      IO.warn(message, Macro.Env.stacktrace(env))

      quote do
        @doc false
        def init(args) do
          {:ok, args}
        end

        defoverridable init: 1
      end
    end
  end

  @spec reply(from, term) :: :ok
  def reply(client, reply)

  def reply({to, tag}, reply) when is_pid(to) do
    send(to, {tag, reply})
    :ok
  end
end

defmodule ElixirSenseExample.ExampleBehaviourWithDoc do
  @doc "Docs for foo"
  @callback foo() :: :ok

  @doc "Docs for bar"
  @macrocallback bar() :: Macro.t()
end

defmodule ElixirSenseExample.ExampleBehaviourWithDocCallback do
  @behaviour ElixirSenseExample.ExampleBehaviourWithDoc

  def foo(), do: :ok

  defmacro bar(), do: quote(do: :ok)
end
