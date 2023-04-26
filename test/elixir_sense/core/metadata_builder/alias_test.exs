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
        Alias.Realias,
        Alias.Unalias,
        Alias.NoUnaliasNested,
        Alias.RequireWithAs
      ] do
    test "alias rules properly handled in #{inspect(module)}" do
      state =
        unquote(module).module_info()[:compile][:source]
        |> File.read!()
        |> Code.string_to_quoted(columns: true)
        |> MetadataBuilder.build()

      env = unquote(module).env()

      assert metadata_env = state.lines_to_env[env.line]

      assert metadata_env.aliases == env.aliases
    end
  end
end
