defmodule ElixirSense.Providers.DefinitionTest do

  use ExUnit.Case
  alias ElixirSense.Providers.Definition

  doctest Definition

  test "find definition of functions from Kernel" do
      {file, line} = Definition.find(nil, :defmodule, [], [])
      assert file =~ "lib/elixir/lib/kernel.ex"
      assert line == 3068
  end

  test "find definition of functions from Kernel.SpecialForms" do
    {file, line} = Definition.find(nil, :import, [], [])
    assert file =~ "lib/elixir/lib/kernel/special_forms.ex"
    assert line == 644
  end

  test "find definition of functions from imports" do
    {file, line} = Definition.find(nil, :create_file, [Mix.Generator], [])
    assert file =~ "lib/mix/lib/mix/generator.ex"
    assert line == 25
  end

  test "find definition of functions from aliased modules" do
    {file, line} = Definition.find(MyList, :flatten, [], [{MyList, List}])
    assert file =~ "lib/elixir/lib/list.ex"
    assert line == 94
  end

  test "find definition of modules" do
    {file, line} = Definition.find(String, nil, [], [{MyList, List}])
    assert file =~ "lib/elixir/lib/string.ex"
    assert line == 3
  end

  test "find definition of erlang modules" do
    {file, line} = Definition.find(:lists, nil, [], [])
    assert file =~ "/src/lists.erl"
    assert line == 1
  end

  test "find definition of remote erlang functions" do
    {file, line} = Definition.find(:lists, :duplicate, [], [])
    assert file =~ "/src/lists.erl"
    assert line == 303
  end

  test "non existing modules" do
    assert Definition.find(SilverBulletModule, :run, [], []) == {"non_existing", nil}
  end

  test "preloaded modules" do
    assert Definition.find(:erlang, nil, [], []) == {"non_existing", nil}
  end

  # Call this when running `mix test`, but not when running `elixir run_test.exs`
  if Process.whereis(Elixir.Mix.Supervisor) do
    test "erlang modules from deps" do
      {file, line} = Definition.find(:hackney, nil, [], [])
      assert file =~ "deps/hackney/src/hackney.erl"
      assert line == 1
    end
  end

  test "find the related module when searching for built-in functions" do
    {file, line} = Definition.find(List, :module_info, [], [])
    assert file =~ "lib/elixir/lib/list.ex"
    assert line == nil
  end

end
