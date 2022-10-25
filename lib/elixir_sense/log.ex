defmodule ElixirSense.Log do
  @moduledoc """
  A simple logger for the project that allows it to be muted via application config
  """
  require Logger

  def enabled? do
    Application.get_env(:elixir_sense, :logging_enabled, true)
  end

  defmacro info(message) do
    quote do
      require Logger

      if ElixirSense.Log.enabled?() do
        Logger.info(unquote(message))
      end
    end
  end

  defmacro warn(message) do
    quote do
      require Logger

      if ElixirSense.Log.enabled?() do
        Logger.warning(unquote(message))
      end
    end
  end

  defmacro error(message) when is_binary(message) do
    quote do
      require Logger

      if ElixirSense.Log.enabled?() do
        Logger.error(unquote(message))
      end
    end
  end
end
