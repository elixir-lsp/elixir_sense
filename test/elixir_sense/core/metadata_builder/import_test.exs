defmodule ElixirSense.Core.MetadataBuilder.ImportTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Core.MetadataBuilder
  alias ElixirSenseExample.Fixtures.MetadataBuilder.Import

  for module <- [
        Import.Empty,
        Import.SimpleImport,
        Import.ImportOnlyFunctions,
        Import.ImportOnlyMacros,
        Import.ImportOnlySigils,
        Import.ImportOnlyUnderscored,
        Import.ImportOnlyList,
        Import.ImportOverwrite,
        Import.ImportOverwriteInScope,
        Import.ImportOverwriteNoLeak,
        Import.ImportExceptList,
        Import.ImportExceptModify,
        Import.ImportErlang,
        Import.ImportErlangBehaviour,
        Import.ImportElixirBehaviour,
        Import.Transitive,
        Import.Import12,
        Import.ImportOfAlias,
        Import.ImportExternal,
        Import.ImportWithWarn,
        Import.ImportOnePart,
        Import.ImportInSubmodule,
        Import.ImportInheritFunction,
        Import.ImportInheritSubmodule,
        Import.ImportNoLeakFunction,
        Import.ImportNoLeakSubmodule,
        Import.ImportNoLeakBlock,
        Import.ImportNoLeakClause
      ] do
    test "import rules properly handled in #{inspect(module)}" do
      state =
        unquote(module).module_info()[:compile][:source]
        |> File.read!()
        |> Code.string_to_quoted(columns: true, token_metadata: true)
        |> MetadataBuilder.build()

      env = unquote(module).env()

      assert metadata_env = state.lines_to_env[env.line]

      assert deep_sort(metadata_env.functions) == deep_sort(env.functions)
      assert deep_sort(metadata_env.macros) == deep_sort(env.macros)
    end
  end

  defp deep_sort(keyword) do
    keyword
    |> Enum.map(fn {k, v} -> {k, Enum.sort(v)} end)
    |> Enum.sort_by(fn {k, _} -> k end)
  end

  test "auto imported" do
    code = """
    __ENV__
    """

    state =
      code
      |> Code.string_to_quoted(columns: true, token_metadata: true)
      |> MetadataBuilder.build()

    {env, _} = Code.eval_string(code, [])
    assert metadata_env = state.lines_to_env[env.line]

    assert deep_sort(metadata_env.functions) == deep_sort(env.functions)
    assert deep_sort(metadata_env.macros) == deep_sort(env.macros)
  end

  test "auto imported after last module" do
    code = """
    defmodule Import.Some.B.C do
    end
    __ENV__
    """

    state =
      code
      |> Code.string_to_quoted(columns: true, token_metadata: true)
      |> MetadataBuilder.build()

    {env, _} = Code.eval_string(code, [])
    assert metadata_env = state.lines_to_env[env.line]

    assert deep_sort(metadata_env.functions) == deep_sort(env.functions)
    assert deep_sort(metadata_env.macros) == deep_sort(env.macros)
  after
    :code.delete(Some.B.C)
    :code.purge(Some.B.C)
  end
end
