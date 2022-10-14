defmodule ElixirSense.LogTest do
  use ExUnit.Case
  import ExUnit.CaptureIO
  import ElixirSense.Log

  def with_logging_disabled(_) do
    orig_value = Application.get_env(:elixir_sense, :logging_enabled)
    Application.put_env(:elixir_sense, :logging_enabled, false)
    on_exit(fn -> Application.put_env(:elixir_sense, :logging_enabled, orig_value) end)
    :ok
  end

  describe "log messages" do
    test "an info message has an info label by default" do
      assert capture_io(fn -> info("good morning") end) == "info: good morning\n"
    end

    test "info supports adding a label" do
      assert capture_io(fn -> info("good morning", label: "heya") end) == "heya: good morning\n"
    end

    test "info suppoerts changing the device" do
      assert capture_io(:stderr, fn -> info(:stderr, "good morning") end)

      assert capture_io(:stderr, fn -> info(:stderr, "good morning", label: "e") end) ==
               "e: good morning\n"
    end

    test "an error message has an error label by default" do
      assert capture_io(:stderr, fn -> error("good morning") end) == "error: good morning\n"
    end

    test "error supports adding a label" do
      assert capture_io(:stderr, fn -> error("good morning", label: "heya") end) ==
               "heya: good morning\n"
    end

    test "error suppoerts changing the device" do
      assert capture_io(fn -> error(:stdio, "good morning") end)

      assert capture_io(fn -> error(:stdio, "good morning", label: "e") end) ==
               "e: good morning\n"
    end

    test "warn emits a stack trace by default" do
      this_unit_test_path = Path.relative_to_cwd(__ENV__.file)
      message = capture_io(:stderr, fn -> warn("did I do that?") end)

      assert String.contains?(message, "did I do that?")
      assert String.contains?(message, "warning: ")
      assert String.contains?(message, "lib/elixir_sense/log.ex")
      assert String.contains?(message, this_unit_test_path)
    end

    test "warn allows you to override the stack trace" do
      message =
        capture_io(:stderr, fn -> warn("finger wagging", line: 3, col: 8, file: "broken.ex") end)

      assert String.contains?(message, "broken.ex:3")
    end
  end

  describe "with logging disabled" do
    setup [:with_logging_disabled]

    test "info emits no output" do
      assert capture_io(fn -> info("hello") end) == ""
    end

    test "warn emits no output" do
      assert capture_io(fn -> warn("hello") end) == ""
      assert capture_io(:stderr, fn -> warn("hello") end) == ""
    end

    test "error emits no output" do
      assert capture_io(:stderr, fn -> error("hello") end) == ""
    end
  end
end
