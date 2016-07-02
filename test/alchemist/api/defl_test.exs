defmodule Alchemist.API.DeflTest do

  use ExUnit.Case
  import ExUnit.CaptureIO
  alias Alchemist.API.Defl

  defp fixture(file) do
    Path.expand("../../fixtures/#{file}", __DIR__)
  end

  test "DEFL request prints file:line" do
    assert capture_io(fn ->
      Defl.request(~s({"nil,defmodule", "some_path", ") <> fixture("my_module.ex") <> ~s(", 1}))
    end) =~ "lib/elixir/lib/kernel.ex:3068"
  end

end
