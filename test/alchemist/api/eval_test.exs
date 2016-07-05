defmodule Alchemist.API.EvalTest do

  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  alias Alchemist.API.Eval

  defp fixture(file) do
    Path.expand("../../fixtures/#{file}", __DIR__)
  end

  test "DOCL request" do
    assert capture_io(fn ->
      Eval.request(~s({:signature_info, "#{fixture("my_module.ex")}", "#{fixture("signature_prefix.txt")}", 1}))
    end) =~ """
    1
    flatten;list
    flatten;list,tail
    """
  end

end
