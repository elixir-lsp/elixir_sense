defmodule ElixirSense.Log do
  @moduledoc """
  A simple logger for the project that allows it to be muted via application config
  """

  def info(message) do
    info(:stdio, message, [])
  end

  def info(message, opts) when is_binary(message) and is_list(opts) do
    info(:stdio, message, opts)
  end

  def info(device, message) when is_atom(device) and is_binary(message) do
    info(device, message, [])
  end

  def info(device, message, opts) do
    log(device, message, Keyword.put_new(opts, :label, "info"))
  end

  def warn(message, stacktrace_info \\ nil) when is_binary(message) do
    opts =
      if is_nil(stacktrace_info) do
        [logger_fn: fn _device, message -> IO.warn(message) end]
      else
        [logger_fn: fn _device, message -> IO.warn(message, stacktrace_info) end]
      end

    log(:stdio, message, opts)
  end

  def error(message) when is_binary(message) do
    error(:stderr, message, [])
  end

  def error(message, opts) when is_binary(message) and is_list(opts) do
    error(:stderr, message, opts)
  end

  def error(device, message) when is_atom(device) and is_binary(message) do
    error(device, message, [])
  end

  def error(device, message, opts) do
    log(device, message, Keyword.put_new(opts, :label, "error"))
  end

  def log(device \\ :stdio, message, opts \\ []) do
    if Application.get_env(:elixir_sense, :logging_enabled, true) do
      logger = Keyword.get(opts, :logger_fn, &IO.puts/2)
      message = maybe_append_label(message, opts)
      logger.(device, message)
    end

    :ok
  end

  defp maybe_append_label(message, opts) do
    case Keyword.get(opts, :label) do
      label_text when is_binary(label_text) ->
        label_text <> ": " <> message

      _ ->
        message
    end
  end
end
