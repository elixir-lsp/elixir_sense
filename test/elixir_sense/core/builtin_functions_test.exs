defmodule ElixirSense.Core.BuiltinFunctionsTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.BuiltinFunctions

  test "gets specs" do
    assert [
             "@spec module_info(:module) :: atom",
             "@spec module_info(:attributes | :compile) :: [{atom, term}]",
             "@spec module_info(:md5) :: binary",
             "@spec module_info(:exports | :functions | :nifs) :: [{atom, non_neg_integer}]",
             "@spec module_info(:native) :: boolean"
           ] == BuiltinFunctions.get_specs({:module_info, 1})
  end

  test "gets args" do
    assert ["key"] == BuiltinFunctions.get_args({:module_info, 1})
  end
end
