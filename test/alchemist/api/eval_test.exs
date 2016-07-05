defmodule Alchemist.API.EvalTest do

  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  alias Alchemist.API.Eval

  defp fixture(file) do
    Path.expand("../../fixtures/#{file}", __DIR__)
  end

  describe "signature" do

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

  describe "match" do

    test "EVAL request match with bindings" do
      assert capture_io(fn ->
        Eval.request(~s({:match, "#{fixture("eval_match.txt")}"}))
      end) =~ """
      # Bindings

      name = :func

      par1 = {:par1, [line: 1], nil}

      par2 = {:par2, [line: 1], nil}
      """
    end

    test "EVAL request match without bindings" do
      assert capture_io(fn ->
        Eval.request(~s({:match, "#{fixture("eval_match_no_bindings.txt")}"}))
      end) == """
      # No bindings
      END-OF-EVAL
      """
    end

    test "EVAL request match with error" do
      assert capture_io(fn ->
        Eval.request(~s({:match, "#{fixture("eval_match_error.txt")}"}))
      end) =~ """
      # TokenMissingError on line 2:
      #  ↳ missing terminator: } (for "{" starting at line 1)
      """
    end

  end

end
