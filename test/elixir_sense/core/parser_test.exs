defmodule ElixirSense.Core.ParserTest do
  use ExUnit.Case

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
        1 => %Env{imports: []},
        3 => %Env{imports: [List]}
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
        1 => %Env{imports: [], module: MyModule},
        2 => %Env{imports: [List], module: MyModule},
        3 => %Env{imports: [List], module: MyModule}
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
        1 => %Env{imports: []},
        3 => %Env{imports: [List]}
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
        1 => %Env{imports: []},
        3 => %Env{imports: [List]}
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
        1 => %Env{imports: []},
        3 => %Env{imports: [List]}
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
        1 => %Env{imports: []},
        3 => %Env{imports: [List]}
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
        1 => %Env{imports: []},
        3 => %Env{imports: [List]}
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
        1 => %Env{imports: []},
        3 => %Env{imports: [List]}
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
        1 => %Env{imports: []},
        3 => %Env{imports: [List]}
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
        1 => %Env{imports: []},
        3 => %Env{imports: [List]}
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
        1 => %Env{imports: []},
        3 => %Env{imports: [List]}
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
        1 => %Env{imports: []},
        3 => %Env{imports: [List]}
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

    assert %Metadata{
      error: nil,
      lines_to_env: %{
        1 => %Env{
          module: MyModule,
          scope_id: 1,
        },
        3 => %Env{
          module: MyModule,
          requires: [],
          scope: :MyModule,
          scope_id: 4,
          vars: [
            %VarInfo{name: :x,}
          ]
        }
      }
    } = parse_string(source, true, true, 3)
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
        1 => %Env{imports: []},
        3 => %Env{imports: [List]}
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
      },
    } = parse_string(source, true, true, 1)
  end

  test "parse_string with literal strings" do
    source = ~S'''
    defmodule MyMod do
      @doc """
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
          attributes: [:doc],
        }
      }
    } = parse_string(source, true, true, 6)
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
      lines_to_env: %{
        5 => %ElixirSense.Core.State.Env{
          vars: [
            %ElixirSense.Core.State.VarInfo{name: :x},
            %ElixirSense.Core.State.VarInfo{name: :y}
          ]
        }
      }
    } = parse_string(source, true, true, 5)
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
