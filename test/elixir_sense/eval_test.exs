defmodule ElixirSense.Evaltest do
  use ExUnit.Case, async: true

  describe "match" do
    test "with bindings" do
      code =
        "{name, _, [par1, par2]} = {:func, [line: 1], [{:par1, [line: 1], nil}, {:par2, [line: 1], nil}]}"

      assert ElixirSense.match(code) =~
               """
               # Bindings

               name = :func

               par1 = {:par1, [line: 1], nil}

               par2 = {:par2, [line: 1], nil}
               """
               |> String.trim()
    end

    test "without bindings" do
      code =
        "{_, _, [_, _]} = {:func, [line: 1], [{:par1, [line: 1], nil}, {:par2, [line: 1], nil}]}"

      assert ElixirSense.match(code) == """
             # No bindings

             """
    end

    test "with token missing error" do
      code = "{ = {:func, [line: 1], [{:par1, [line: 1], nil}, {:par2, [line: 1], nil}]}"

      assert ElixirSense.match(code) =~
               """
               # TokenMissingError on line 1:
               #  ↳ missing terminator: } (for "{" starting at line 1)
               """
               |> String.trim()
    end

    test "EVAL request match with match error" do
      code = "{var} = {:func, [line: 1], [{:par1, [line: 1], nil}, {:par2, [line: 1], nil}]}"
      assert ElixirSense.match(code) == "# No match"
    end
  end

  describe "expand full" do
    test "without errors" do
      buffer = """
      defmodule MyModule do

      end
      """

      code = "use Application"
      result = ElixirSense.expand_full(buffer, code, 2)

      assert result.expand_once =~
               """
               (
                 require(Application)
                 Application.__using__([])
               )
               """
               |> String.trim()

      assert result.expand =~
               """
               (
                 require(Application)
                 Application.__using__([])
               )
               """
               |> String.trim()

      assert result.expand_partial =~
               """
               (
                 require(Application)
                 (
                   @behaviour(Application)
                   @doc(false)
                   def(stop(_state)) do
                     :ok
                   end
                   defoverridable(Application)
                 )
               )
               """
               |> String.trim()

      if Version.match?(System.version(), "< 1.11.0-dev") do
        # apparently no full expansion was preformed on earlier elixir versions

        assert result.expand_all =~
                 """
                 (
                   require(Application)
                   (
                     @behaviour(Application)
                     @doc(false)
                     def(stop(_state)) do
                       :ok
                     end
                     Module.make_overridable(MyModule, Application)
                 """
                 |> String.trim()
      else
        assert result.expand_all =~
                 """
                 (
                   require(Application)
                   (
                     Module.__put_attribute__(MyModule, :behaviour, Application, nil)
                     Module.__put_attribute__(MyModule, :doc, {0, false}, nil)
                     def(stop(_state)) do
                       :ok
                     end
                     Module.make_overridable(MyModule, Application)
                 """
                 |> String.trim()
      end
    end

    test "with errors" do
      buffer = """
      defmodule MyModule do

      end
      """

      code = "{"
      result = ElixirSense.expand_full(buffer, code, 2)

      assert result.expand_once =~
               """
               "missing terminator: } (for \\"{\\" starting at line 1)", ""}
               """
               |> String.trim()

      assert result.expand =~
               """
               "missing terminator: } (for \\"{\\" starting at line 1)", ""}
               """
               |> String.trim()

      assert result.expand_partial =~
               """
               "missing terminator: } (for \\"{\\" starting at line 1)", ""}
               """
               |> String.trim()

      assert result.expand_all =~
               """
               "missing terminator: } (for \\"{\\" starting at line 1)", ""}
               """
               |> String.trim()
    end
  end

  describe "quote" do
    test "without error" do
      code = "func(par1, par2)"

      assert ElixirSense.quote(code) =~
               "{:func, [line: 1], [{:par1, [line: 1], nil}, {:par2, [line: 1], nil}]}"
    end

    test "with error" do
      code = "func(par1, par2"

      assert ElixirSense.quote(code) =~
               """
               "missing terminator: ) (for \\"(\\" starting at line 1)", \""}
               """
               |> String.trim()
    end
  end
end
