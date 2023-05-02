defmodule ElixirSense.Core.MetadataBuilder.RequireTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Core.MetadataBuilder
  alias ElixirSenseExample.Fixtures.MetadataBuilder.Require

  for module <- [
        Require.Empty,
        Require.Require12,
        Require.RequireOfAlias,
        Require.RequireExternal,
        Require.SimpleRequire,
        Require.RequireInSubmodule,
        Require.RequireWithAs,
        Require.RequireWithAsOnePart,
        Require.RequireSubmodule,
        Require.RequireNoLeakFunction,
        Require.RequireNoLeakSubmodule,
        Require.RequireInheritFunction,
        Require.RequireInheritSubmodule,
        Require.RequireNoLeakBlock,
        Require.RequireNoLeakClause,
        Require.RequireOnePart
      ] do
    test "require rules properly handled in #{inspect(module)}" do
      state =
        unquote(module).module_info()[:compile][:source]
        |> File.read!()
        |> Code.string_to_quoted(columns: true)
        |> MetadataBuilder.build()

      env = unquote(module).env()

      assert metadata_env = state.lines_to_env[env.line]

      assert Enum.sort(metadata_env.requires) == Enum.sort(env.requires)
    end
  end

  test "auto required" do
    code = """
    __ENV__
    """

    state =
      code
      |> Code.string_to_quoted(columns: true)
      |> MetadataBuilder.build()

    {env, _} = Code.eval_string(code, [])
    assert metadata_env = state.lines_to_env[env.line]

    assert Enum.sort(metadata_env.requires) == Enum.sort(env.requires)
  end

  test "auto required after last module" do
    code = """
    defmodule Other.B.C do
    end
    __ENV__
    """

    state =
      code
      |> Code.string_to_quoted(columns: true)
      |> MetadataBuilder.build()

    {env, _} = Code.eval_string(code, [])
    assert metadata_env = state.lines_to_env[env.line]

    assert Enum.sort(metadata_env.requires) == Enum.sort(env.requires)
  after
    :code.delete(Other.B.C)
    :code.purge(Other.B.C)
  end
end
