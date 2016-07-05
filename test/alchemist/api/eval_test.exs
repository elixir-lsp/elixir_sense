defmodule Alchemist.API.EvalTest do

  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  alias Alchemist.API.Eval

  defp fixture(file) do
    Path.expand("../../fixtures/#{file}", __DIR__)
  end

  test "EVAL request finds signatures" do
    assert capture_io(fn ->
      Eval.request(~s({:signature_info, "#{fixture("my_module.ex")}", "#{fixture("signature_prefix.txt")}", 3}))
    end) =~ """
    1
    flatten;list
    flatten;list,tail
    """
  end

  test "EVAL request finds signatures from Kernel functions" do
    assert capture_io(fn ->
      Eval.request(~s({:signature_info, "#{fixture("my_module.ex")}", "#{fixture("signature_prefix_kernel.txt")}", 3}))
    end) =~ """
    1
    apply;fun,args
    apply;module,fun,args
    """
  end

  test "EVAL request when NOT finding signatures" do
    assert capture_io(fn ->
      Eval.request(~s({:signature_info, "#{fixture("my_module.ex")}", "#{fixture("signature_prefix_none.txt")}", 1}))
    end) =~ """
    none
    """
  end

end
