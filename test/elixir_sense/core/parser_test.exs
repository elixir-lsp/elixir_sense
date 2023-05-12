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
             mods_funs_to_positions: %{{MyModule, nil, nil} => %{positions: [{1, 11}]}},
             lines_to_env: %{
               1 => %Env{imports: [{Kernel, []}]},
               3 => %Env{imports: [{Kernel, []}, {List, []}]}
             },
             source: "defmodule MyModule" <> _
           } = parse_string(source, true, true, 3)
  end

  test "parse_string with syntax error" do
    source = """
    defmodule MyModule do
      import List
      Enum +
    end
    """

    assert %Metadata{
             error: nil,
             lines_to_env: %{
               1 => %Env{imports: [{Kernel, []}], module: MyModule},
               2 => %Env{imports: [{Kernel, []}, {List, []}], module: MyModule},
               3 => %Env{imports: [{Kernel, []}, {List, []}], module: MyModule}
             }
           } = parse_string(source, true, true, 3)
  end

  test "parse_string with syntax error (missing param)" do
    source = """
    defmodule MyModule do
      import List
      IO.puts(:stderr, )
    end
    """

    assert %Metadata{
             error: nil,
             lines_to_env: %{
               1 => %Env{imports: [{Kernel, []}]},
               3 => %Env{imports: [{Kernel, []}, {List, []}]}
             }
           } = parse_string(source, true, true, 3)
  end

  test "parse_string with missing terminator \")\"" do
    source = """
    defmodule MyModule do
      import List
      func(
    end
    """

    assert %Metadata{
             error: nil,
             lines_to_env: %{
               1 => %Env{imports: [{Kernel, []}]},
               3 => %Env{imports: [{Kernel, []}, {List, []}]}
             }
           } = parse_string(source, true, true, 3)
  end

  test "parse_string with missing terminator \"]\"" do
    source = """
    defmodule MyModule do
      import List
      list = [
    end
    """

    assert %Metadata{
             error: nil,
             lines_to_env: %{
               1 => %Env{imports: [{Kernel, []}]},
               3 => %Env{imports: [{Kernel, []}, {List, []}]}
             }
           } = parse_string(source, true, true, 3)
  end

  test "parse_string with missing terminator \"}\"" do
    source = """
    defmodule MyModule do
      import List
      tuple = {
    end
    """

    assert %Metadata{
             error: nil,
             lines_to_env: %{
               1 => %Env{imports: [{Kernel, []}]},
               3 => %Env{imports: [{Kernel, []}, {List, []}]}
             }
           } = parse_string(source, true, true, 3)
  end

  test "parse_string with missing terminator \"\"\"" do
    source = """
    defmodule MyModule do
      import List
      var = "
    end
    """

    assert %Metadata{
             error: nil,
             lines_to_env: %{
               1 => %Env{imports: [{Kernel, []}]},
               3 => %Env{imports: [{Kernel, []}, {List, []}]}
             }
           } = parse_string(source, true, true, 3)
  end

  test "parse_string with missing terminator \"\'\"" do
    source = """
    defmodule MyModule do
      import List
      var = '
    end
    """

    assert %Metadata{
             error: nil,
             lines_to_env: %{
               1 => %Env{imports: [{Kernel, []}]},
               3 => %Env{imports: [{Kernel, []}, {List, []}]}
             }
           } = parse_string(source, true, true, 3)
  end

  test "parse_string with missing heredoc terminator" do
    source = """
    defmodule MyModule do
      import List
      var = \"\"\"
    end
    """

    assert %Metadata{
             error: nil,
             lines_to_env: %{
               1 => %Env{imports: [{Kernel, []}]},
               3 => %Env{imports: [{Kernel, []}, {List, []}]}
             }
           } = parse_string(source, true, true, 3)
  end

  test "parse_string with missing interpolation terminator in \"\"\"" do
    source = """
    defmodule MyModule do
      import List
      var = "\#{
    end
    """

    assert %Metadata{
             error: nil,
             lines_to_env: %{
               1 => %Env{imports: [{Kernel, []}]},
               3 => %Env{imports: [{Kernel, []}, {List, []}]}
             }
           } = parse_string(source, true, true, 3)
  end

  test "parse_string with missing interpolation terminator in \"\'\"" do
    source = """
    defmodule MyModule do
      import List
      var = '\#{
    end
    """

    assert %Metadata{
             error: nil,
             lines_to_env: %{
               1 => %Env{imports: [{Kernel, []}]},
               3 => %Env{imports: [{Kernel, []}, {List, []}]}
             }
           } = parse_string(source, true, true, 3)
  end

  test "parse_string with missing interpolation terminator in heredoc" do
    source = """
    defmodule MyModule do
      import List
      var = \"\"\"\#{
    end
    """

    assert %Metadata{
             error: nil,
             lines_to_env: %{
               1 => %Env{imports: [{Kernel, []}]},
               3 => %Env{imports: [{Kernel, []}, {List, []}]}
             }
           } = parse_string(source, true, true, 3)
  end

  test "parse_string with missing terminator \"end\" attempts to fix it by inserting end at line from error" do
    source = """
    defmodule MyModule do
      []
      |> Enum.map(fn x ->

      a = 5
    end
    """

    assert capture_io(:stderr, fn ->
             result = parse_string(source, true, true, 3)
             send(self(), {:result, result})
           end) =~ "an expression is always required on the right side of ->"

    assert_received {:result, result}

    assert %Metadata{
             error: nil,
             lines_to_env: %{
               1 => %Env{
                 module: MyModule,
                 scope_id: 1
               },
               3 => %Env{
                 module: MyModule,
                 requires: _,
                 scope: :MyModule,
                 scope_id: 4,
                 vars: [
                   %VarInfo{name: :x}
                 ]
               }
             }
           } = result
  end

  test "parse_string with missing terminator \"end\" attemtps to insert `end` at correct intendation" do
    source = """
    defmodule MyModule do

    """

    assert %Metadata{
             error: nil,
             lines_to_env: %{
               1 => %Env{module: MyModule},
               2 => %Env{module: MyModule}
             }
           } = parse_string(source, true, true, 2)

    source = """
    defmodule MyModule do

    defmodule MyModule1 do
    end
    """

    assert %Metadata{
             error: nil,
             lines_to_env: %{
               1 => %Env{module: MyModule},
               3 => %Env{module: MyModule1}
             }
           } = parse_string(source, true, true, 3)

    assert %Metadata{
             error: nil,
             lines_to_env: %{
               1 => %Env{module: MyModule},
               2 => %Env{module: MyModule}
             }
           } = parse_string(source, true, true, 2)

    source = """
    defmodule MyModule do

      defmodule MyModule1 do
      end
    """

    assert %Metadata{
             error: nil,
             lines_to_env: %{
               1 => %Env{module: MyModule},
               2 => %Env{module: MyModule},
               3 => %Env{module: MyModule.MyModule1}
             }
           } = parse_string(source, true, true, 2)

    assert %Metadata{
             error: nil,
             lines_to_env: %{
               1 => %Env{module: MyModule},
               3 => %Env{module: MyModule.MyModule1}
             }
           } = parse_string(source, true, true, 3)
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
      assert %Metadata{error: nil} = parse_string(source, true, true, 5)
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

    %Metadata{error: nil} = parse_string(source, true, true, 5)
  end

  test "parse_string ignores non existing modules in `use`" do
    source = """
    defmodule MyModule do
      use EnumFake
      import List

    end
    """

    assert %Metadata{
             error: nil,
             mods_funs_to_positions: %{{MyModule, nil, nil} => %{positions: [{1, 11}]}},
             lines_to_env: %{
               1 => %Env{imports: [{Kernel, []}]},
               3 => %Env{imports: [{Kernel, []}, {List, []}]}
             },
             source: "defmodule MyModule" <> _
           } = parse_string(source, true, true, 4)
  end

  test "parse_string with malformed `do` expression" do
    source = """
    defmodule MyModule, do
    """

    assert %ElixirSense.Core.Metadata{
             error: nil,
             lines_to_env: %{
               1 => %Env{module: MyModule}
             }
           } = parse_string(source, true, true, 1)
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
             lines_to_env: %{
               6 => %ElixirSense.Core.State.Env{
                 attributes: [%ElixirSense.Core.State.AttributeInfo{name: :my_attr}]
               }
             }
           } = parse_string(source, true, true, 6)
  end

  @tag only_this: true
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
             lines_to_env: %{
               5 => %ElixirSense.Core.State.Env{
                 vars: vars
               }
             }
           } = parse_string(source, true, true, 5)

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
          data: "foo"
        }
      end
    end
    """

    assert %ElixirSense.Core.Metadata{
             calls: %{
               3 => [%{func: :%{}}]
             }
           } = parse_string(source, true, true, 4)
  end

  test "parse struct with missing terminator" do
    source = """
    defmodule MyModule do
      def func() do
        %{
          data: "foo"

      end
    end
    """

    assert %ElixirSense.Core.Metadata{
             calls: %{
               3 => [%{func: :%{}}]
             }
           } = parse_string(source, true, true, 4)
  end
end
