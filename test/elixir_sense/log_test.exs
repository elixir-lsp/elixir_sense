defmodule ElixirSense.LogTest do
  use ExUnit.Case
  import ExUnit.CaptureLog
  import ElixirSense.Log

  def with_logging_disabled(_) do
    orig_value = Application.get_env(:elixir_sense, :logging_enabled)
    Application.put_env(:elixir_sense, :logging_enabled, false)
    on_exit(fn -> Application.put_env(:elixir_sense, :logging_enabled, orig_value) end)
    :ok
  end

  describe "log messages" do
    test "an info message has an info label by default" do
      message = assert capture_log(fn -> info("good morning") end)
      assert message =~ "[info] "
      assert message =~ "good morning\n"
    end

    test "an error message has an error label by default" do
      message = assert capture_log(fn -> error("good morning") end)
      assert message =~ "[error] "
      assert message =~ "good morning\n"
    end
  end

  describe "with logging disabled" do
    setup [:with_logging_disabled]

    test "info emits no output" do
      assert capture_log(fn -> info("hello") end) == ""
    end

    test "warn emits no output" do
      assert capture_log(fn -> warn("hello") end) == ""
      assert capture_log(fn -> warn("hello") end) == ""
    end

    test "error emits no output" do
      assert capture_log(fn -> error("hello") end) == ""
    end
  end
end
