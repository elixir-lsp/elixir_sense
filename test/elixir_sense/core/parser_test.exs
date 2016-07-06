defmodule ElixirSense.Core.ParserTest do
  use ExUnit.Case

  import ElixirSense.Core.Parser

  test "parse_string with parse error" do
    source = """
    defmodule MyModule do
      func(
    end
    """
    assert parse_string(source, true, true, 2) ==
      %ElixirSense.Core.Metadata{
        error: nil,
        lines_to_env: %{2 => %ElixirSense.Core.State.Env{aliases: [], attributes: [], behaviours: [], imports: [], module: MyModule, requires: [], scope: :MyModule, vars: []}},
        mods_funs_to_lines: %{{MyModule, nil, nil} => 1},
        source: "defmodule MyModule do\n  func(\nend\n"
      }
  end

  test "parse_string with missing terminator error" do
    source = """
    defmodule MyModule do

    """
    assert parse_string(source, true, true, 2) ==
      %ElixirSense.Core.Metadata{
        error: {3,"missing terminator: end (for \"do\" starting at line 1)", ""},
        lines_to_env: %{}, mods_funs_to_lines: %{},
        source: "defmodule MyModule do\n\n"
      }
  end

end
