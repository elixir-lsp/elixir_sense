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

    test "EVAL request match with token missing error" do
      assert capture_io(fn ->
        Eval.request(~s({:match, "#{fixture("eval_match_token_missing_error.txt")}"}))
      end) =~ """
      # TokenMissingError on line 2:
      #  ↳ missing terminator: } (for "{" starting at line 1)
      """
    end

    test "EVAL request match with match error" do
      assert capture_io(fn ->
        Eval.request(~s({:match, "#{fixture("eval_match_no_match.txt")}"}))
      end) == """
      # No match
      END-OF-EVAL
      """
    end

  end

  describe "expand full" do

    test "EVAL request expand full" do
      output = capture_io(fn ->
        Eval.request(~s({:expand_full, "#{fixture("my_module.ex")}", "#{fixture("eval_expand_full.txt")}", 2}))
      end)
      [expanded_once, expanded, expanded_partial, expanded_all] = String.split(output, "\u000B")

      assert expanded_once =~ """
      (
        require(Application)
        Application.__using__([])
      )
      """

      assert expanded =~ """
      (
        require(Application)
        Application.__using__([])
      )
      """

      assert expanded_partial =~ """
      (
        require(Application)
        (
          @behaviour(Application)
          @doc(false)
          def(stop(_state)) do
            :ok
          end
          defoverridable(stop: 1)
        )
      )
      """

      assert expanded_all =~ """
      (
        require(Application)
        (
          Module.put_attribute(MyModule, :behaviour, Application)
          Module.put_attribute(MyModule, :doc, {0, false}, [{MyModule, :__MODULE__, 0, [file: \"lib/alchemist/api/eval.ex\", line: 0]}])
      """

    end

    test "EVAL request expand full with error" do
      output = capture_io(fn ->
        Eval.request(~s({:expand_full, "#{fixture("my_module.ex")}", "#{fixture("eval_expand_full_error.txt")}", 2}))
      end)
      [expanded_once, expanded, expanded_partial] = String.split(output, "\u000B")

      assert expanded_once =~ """
      {2, "missing terminator: } (for \\"{\\" starting at line 1)", ""}
      """

      assert expanded =~ """
      {2, "missing terminator: } (for \\"{\\" starting at line 1)", ""}
      """

      assert expanded_partial =~ """
      %FunctionClauseError{arity: 4, function: :do_traverse_args, module: Macro}
      """
    end

  end

  describe "quote" do

    test "EVAL request quote" do
      assert capture_io(fn ->
        Eval.request(~s({:quote, "#{fixture("eval_quote.txt")}"}))
      end) =~ """
      {:func, [line: 1], [{:par1, [line: 1], nil}, {:par2, [line: 1], nil}]}
      """
    end

    test "EVAL request quote with error" do
      assert capture_io(fn ->
        Eval.request(~s({:quote, "#{fixture("eval_quote_error.txt")}"}))
      end) =~ """
      {2, "missing terminator: ) (for \\"(\\" starting at line 1)", \""}
      """
    end

  end

end
