defmodule ElixirSense.LocationTest do
  use ExUnit.Case, async: false

  import ElixirSense.Location

  setup do
    elixir_src = Path.join(File.cwd!(), "/test/misc/mock_elixir_src")
    Application.put_env(:elixir_sense, :elixir_src, elixir_src)

    on_exit(fn ->
      Application.delete_env(:elixir_sense, :elixir_src)
    end)
  end

  describe "find_mod_fun_source/3" do
    test "returns location of a core Elixir function" do
      assert %ElixirSense.Location{type: :function, line: 26, column: 3, file: file} =
               find_mod_fun_source(String, :length, 1)

      assert String.ends_with?(file, "/mock_elixir_src/lib/elixir/lib/string.ex")
    end
  end

  describe "find_type_source/3" do
    test "returns location of a core Elixir type" do
      assert %ElixirSense.Location{type: :typespec, line: 11, column: 3, file: file} =
               find_type_source(String, :t, 0)

      assert String.ends_with?(file, "/mock_elixir_src/lib/elixir/lib/string.ex")
    end
  end
end
