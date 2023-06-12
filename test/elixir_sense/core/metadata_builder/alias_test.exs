defmodule ElixirSense.Core.MetadataBuilder.AliasTest do
  use ExUnit.Case, async: true

  alias ElixirSense.Core.MetadataBuilder
  alias ElixirSenseExample.Fixtures.MetadataBuilder.Alias

  for module <- [
        Alias.Empty,
        Alias.SimpleAlias,
        Alias.Alias12,
        Alias.AliasOfAlias,
        Alias.AliasExternal,
        Alias.AliasModuleSpecial12,
        Alias.AliasModuleSpecialSubmodule,
        Alias.AliasModuleSpecialWithAs,
        Alias.AliasModuleSpecial,
        Alias.AliasWithAsOnePart,
        Alias.AliasOnePart,
        Alias.AliasSubmoduleExternal,
        Alias.AliasSubmoduleNestedExternal,
        Alias.AliasSubmoduleNested,
        Alias.AliasSubmodule,
        Alias.AliasInSubmodule,
        Alias.AliasWithAsErlang,
        Alias.AliasWithAs,
        Alias.AliasInheritFunction,
        Alias.AliasInheritSubmodule,
        Alias.AliasNoLeakFunction,
        Alias.AliasNoLeakSubmodule,
        Alias.AliasNoLeakBlock,
        Alias.AliasNoLeakClause,
        Alias.RealiasInScope,
        Alias.Realias,
        Alias.Unalias,
        Alias.NoUnaliasNested,
        Alias.RequireWithAs
      ] do
    test "alias rules properly handled in #{inspect(module)}" do
      state =
        unquote(module).module_info()[:compile][:source]
        |> File.read!()
        |> Code.string_to_quoted(columns: true, token_metadata: true)
        |> MetadataBuilder.build()

      env = unquote(module).env()

      assert metadata_env = state.lines_to_env[env.line]

      assert metadata_env.aliases == env.aliases
    end
  end

  test "auto aliased" do
    code = """
    __ENV__
    """

    state =
      code
      |> Code.string_to_quoted(columns: true, token_metadata: true)
      |> MetadataBuilder.build()

    {env, _} = Code.eval_string(code, [])
    assert metadata_env = state.lines_to_env[env.line]

    assert Enum.sort(metadata_env.aliases) == Enum.sort(env.aliases)
  end

  test "auto aliased after last module" do
    code = """
    defmodule Alias.Some.B.C do
    end
    __ENV__
    """

    state =
      code
      |> Code.string_to_quoted(columns: true, token_metadata: true)
      |> MetadataBuilder.build()

    {env, _} = Code.eval_string(code, [])
    assert metadata_env = state.lines_to_env[env.line]

    assert Enum.sort(metadata_env.aliases) == Enum.sort(env.aliases)
  after
    :code.delete(Some.B.C)
    :code.purge(Some.B.C)
  end
end
