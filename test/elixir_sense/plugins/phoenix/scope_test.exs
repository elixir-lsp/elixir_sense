defmodule ElixirSense.Plugins.Phoenix.ScopeTest do
  use ExUnit.Case
  alias ElixirSense.Plugins.Phoenix.Scope

  describe "within_scope/1" do
    test "returns true and nil alias" do
      buffer = """
        scope "/" do
          get "/",
      """

      assert {true, nil} = Scope.within_scope(buffer)
    end

    test "returns true and alias when passing alias as option" do
      buffer = """
        scope "/", alias: ExampleWeb do
          get "/",
      """

      assert {true, ExampleWeb} = Scope.within_scope(buffer)
    end

    test "returns true and alias when passing alias as second parameter" do
      buffer = """
        scope "/", ExampleWeb do
          get "/",
      """

      assert {true, ExampleWeb} = Scope.within_scope(buffer)
    end

    test "returns true and alias when nested within other scopes" do
      _define_existing_atom = ExampleWeb.Admin
      _define_existing_atom = Admin

      buffer = """
        scope "/", ExampleWeb do
          scope "/admin", Admin do
            get "/",
      """

      assert {true, ExampleWeb.Admin} = Scope.within_scope(buffer)
    end

    test "returns false" do
      buffer = "get \"\\\" ,"

      assert {false, nil} = Scope.within_scope(buffer)
    end
  end
end
