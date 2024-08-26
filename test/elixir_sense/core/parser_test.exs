defmodule ElixirSense.Core.ParserTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO
  import ElixirSense.Core.Parser
  alias ElixirSense.Core.{Metadata, State.Env, State.VarInfo}

  test "parse_string creates a Metadata struct" do
    source = """
    defmodule MyModule do
      import List

    end
    """

    assert %Metadata{
             error: nil,
             mods_funs_to_positions: %{{MyModule, nil, nil} => %{positions: [{1, 1}]}},
             cursor_env: {_, %Env{functions: functions}},
             source: "defmodule MyModule" <> _
           } = parse_string(source, true, true, {3, 3})

    assert Keyword.has_key?(functions, List)
  end

  test "parse_string with syntax error" do
    source = """
    defmodule MyModule do
      import List
      Enum +
    end
    """

    assert %Metadata{
             error: {:error, :parse_error},
             cursor_env: {_, %Env{functions: functions3, module: MyModule}}
           } = parse_string(source, true, true, {3, 10})

    assert Keyword.has_key?(functions3, List)
  end

  test "parse_string with syntax error (missing param)" do
    source = """
    defmodule MyModule do
      import List
      IO.puts(:stderr, )
    end
    """

    assert %Metadata{
             error: {:error, :parse_error},
             cursor_env: {_, %Env{functions: functions}}
           } = parse_string(source, true, true, {3, 20})

    assert Keyword.has_key?(functions, List)
  end

  test "parse_string with missing terminator \")\"" do
    source = """
    defmodule MyModule do
      import List
      func(
    end
    """

    assert %Metadata{
             error: {:error, :parse_error},
             closest_env: {_, _, %Env{functions: functions}}
           } = parse_string(source, true, true, {3, 8})

    assert Keyword.has_key?(functions, List)
  end

  test "parse_string with missing terminator \"]\"" do
    source = """
    defmodule MyModule do
      import List
      list = [
    end
    """

    assert %Metadata{
             error: {:error, :parse_error},
             closest_env: {_, _, %Env{functions: functions}}
           } = parse_string(source, true, true, {3, 11})

    assert Keyword.has_key?(functions, List)
  end

  test "parse_string with missing terminator \"}\"" do
    source = """
    defmodule MyModule do
      import List
      tuple = {
    end
    """

    assert %Metadata{
             error: {:error, :parse_error},
             closest_env: {_, _, %Env{functions: functions}}
           } = parse_string(source, true, true, {3, 12})

    assert Keyword.has_key?(functions, List)
  end

  test "parse_string with missing terminator \"\"\"" do
    source = """
    defmodule MyModule do
      import List
      var = "
    end
    """

    assert %Metadata{
             error: {:error, :parse_error},
             lines_to_env: %{
               1 => %Env{},
               3 => %Env{functions: functions}
             }
           } = parse_string(source, true, true, {3, 10})

    assert Keyword.has_key?(functions, List)
  end

  test "parse_string with missing terminator \"\'\"" do
    source = """
    defmodule MyModule do
      import List
      var = '
    end
    """

    assert %Metadata{
             error: {:error, :parse_error},
             lines_to_env: %{
               1 => %Env{},
               3 => %Env{functions: functions}
             }
           } = parse_string(source, true, true, {3, 10})

    assert Keyword.has_key?(functions, List)
  end

  test "parse_string with missing heredoc terminator" do
    source = """
    defmodule MyModule do
      import List
      var = \"\"\"
    end
    """

    assert %Metadata{
             error: {:error, :parse_error},
             closest_env: {_, _, %Env{functions: functions}}
           } = parse_string(source, true, true, {3, 12})

    assert Keyword.has_key?(functions, List)
  end

  test "parse_string with missing interpolation terminator in \"\"\"" do
    source = """
    defmodule MyModule do
      import List
      var = "\#{
    end
    """

    assert %Metadata{
             error: {:error, :parse_error},
             closest_env: {_, _, %Env{functions: functions}}
           } = parse_string(source, true, true, {3, 12})

    assert Keyword.has_key?(functions, List)
  end

  test "parse_string with missing interpolation terminator in \"\'\"" do
    source = """
    defmodule MyModule do
      import List
      var = '\#{
    end
    """

    assert %Metadata{
             error: {:error, :parse_error},
             closest_env: {_, _, %Env{functions: functions}}
           } = parse_string(source, true, true, {3, 12})

    assert Keyword.has_key?(functions, List)
  end

  test "parse_string with missing interpolation terminator in heredoc" do
    source = """
    defmodule MyModule do
      import List
      var = \"\"\"\#{
    end
    """

    assert %Metadata{
             error: {:error, :parse_error},
             cursor_env: {_, %Env{functions: functions}}
           } = parse_string(source, true, true, {3, 14})

    assert Keyword.has_key?(functions, List)
  end

  test "parse_string with missing terminator \"end\" attempts to fix it by inserting end at line from error" do
    source = """
    defmodule MyModule do
      []
      |> Enum.map(fn x ->

      a = 5
    end
    """

    # assert capture_io(:stderr, fn ->
    result = parse_string(source, true, true, {3, 23})
    #  send(self(), {:result, result})
    #  end) =~ "an expression is always required on the right side of ->"

    # assert_received {:result, result}

    assert %Metadata{
             error: {:error, :parse_error},
             closest_env:
               {_, _,
                %Env{
                  vars: [
                    %VarInfo{name: :x}
                  ]
                }}
           } = result
  end

  test "parse_string with missing terminator \"end\" attempts to insert `end` at correct indentation" do
    source = """
    defmodule MyModule do

    """

    assert %Metadata{
             error: {:error, :parse_error},
             cursor_env: {_, %Env{module: MyModule}}
           } = parse_string(source, true, true, {2, 3})

    source = """
    defmodule MyModule do

    defmodule MyModule1 do
    end
    """

    assert %Metadata{
             error: {:error, :parse_error},
             closest_env: {_, _, %Env{module: MyModule}}
           } = parse_string(source, true, true, {3, 1})

    assert %Metadata{
             error: {:error, :parse_error},
             cursor_env: {_, %Env{module: MyModule}}
           } = parse_string(source, true, true, {2, 1})

    source = """
    defmodule MyModule do

      defmodule MyModule1 do
      end
    """

    assert %Metadata{
             error: {:error, :parse_error},
             cursor_env: {_, %Env{module: MyModule}},
             lines_to_env: %{
               1 => %Env{module: MyModule},
               3 => %Env{module: MyModule.MyModule1}
             }
           } = parse_string(source, true, true, {2, 1})

    assert %Metadata{
             error: {:error, :parse_error},
             closest_env: {_, _, %Env{module: MyModule}},
             lines_to_env: %{
               1 => %Env{module: MyModule},
               3 => %Env{module: MyModule.MyModule1}
             }
           } = parse_string(source, true, true, {3, 1})
  end

  test "parse_string with incomplete key for multiline keyword as argument" do
    source = """
    defmodule MyModule do
      IO.inspect(
        :stderr,
        label: "label",
        limit
      )
    end
    """

    capture_io(:stderr, fn ->
      assert %Metadata{error: {:error, :parse_error}, cursor_env: {_, _}} =
               parse_string(source, true, true, {5, 10})
    end)
  end

  test "parse_string with missing value for multiline keyword as argument" do
    source = """
    defmodule MyModule do
      IO.inspect(
        :stderr,
        label: "label",
        limit:
      )
    end
    """

    assert %Metadata{error: {:error, :parse_error}, cursor_env: {_, _}} =
             parse_string(source, true, true, {5, 12})
  end

  @tag capture_log: true
  test "parse_string ignores non existing modules in `use`" do
    source = """
    defmodule MyModule do
      use EnumFake
      import List
      foo()

    end
    """

    assert %Metadata{
             error: nil,
             mods_funs_to_positions: %{{MyModule, nil, nil} => %{positions: [{1, 1}]}},
             cursor_env: {_, %Env{functions: functions}},
             source: "defmodule MyModule" <> _
           } = parse_string(source, true, true, {5, 3})

    assert Keyword.has_key?(functions, List)
  end

  test "parse_string with malformed `do` expression" do
    source = """
    defmodule MyModule, do
    """

    assert %ElixirSense.Core.Metadata{
             error: {:error, :parse_error},
             lines_to_env: %{
               1 => %Env{module: MyModule}
             }
           } = parse_string(source, true, true, {1, 23})
  end

  test "parse_string with literal strings" do
    source = ~S'''
    defmodule MyMod do
      @my_attr """
      Some docs.
      """
      def func do
        1
      end
    end
    '''

    assert %ElixirSense.Core.Metadata{
             cursor_env:
               {_,
                %ElixirSense.Core.State.Env{
                  attributes: [%ElixirSense.Core.State.AttributeInfo{name: :my_attr}]
                }}
           } = parse_string(source, true, true, {6, 6})
  end

  test "parse_string with literal strings in sigils" do
    source = ~S'''
    defmodule MyMod do
      def render(y) do
        x = y
        ~E"""
        <h1><%= x %></h1>
        """
      end
    end
    '''

    assert %ElixirSense.Core.Metadata{
             closest_env: {
               _,
               _,
               %ElixirSense.Core.State.Env{
                 vars: vars
               }
             }
           } = parse_string(source, true, true, {5, 14})

    assert [
             %ElixirSense.Core.State.VarInfo{name: :x},
             %ElixirSense.Core.State.VarInfo{name: :y}
           ] = Enum.sort(vars)
  end

  test "parse struct" do
    source = """
    defmodule MyModule do
      def func() do
        %{
          data: foo()
        }
      end
    end
    """

    assert %ElixirSense.Core.Metadata{
             calls: %{
               4 => [%{func: :foo}]
             }
           } = parse_string(source, true, true, {4, 7})
  end

  test "parse struct with missing terminator" do
    source = """
    defmodule MyModule do
      def func() do
        %{
          data: foo()

      end
    end
    """

    assert %ElixirSense.Core.Metadata{
             calls: %{
               4 => [%{func: :foo}]
             }
           } = parse_string(source, true, true, {4, 8})
  end
end
