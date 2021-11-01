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
               (if Version.match?(System.version(), "< 1.13.0-dev") do
                  """
                  (
                    require(Application)
                    Application.__using__([])
                  )
                  """
                else
                  """
                  (require Application
                  Application.__using__([]))
                  """
                end)
               |> String.trim()

      assert result.expand =~
               (if Version.match?(System.version(), "< 1.13.0-dev") do
                  """
                  (
                    require(Application)
                    Application.__using__([])
                  )
                  """
                else
                  """
                  (require Application
                  Application.__using__([]))
                  """
                end)
               |> String.trim()

      assert result.expand_partial =~
               (if Version.match?(System.version(), "< 1.13.0-dev") do
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
                else
                  """
                  (require Application
                  (@behaviour Application
                  @doc false
                  def stop(_state) do
                    :ok
                  end
                  defoverridable Application))
                  """
                end)
               |> String.trim()

      assert result.expand_all =~
               (if Version.match?(System.version(), "< 1.13.0-dev") do
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
                else
                  """
                  (require Application
                  (Module.__put_attribute__(MyModule, :behaviour, Application, nil)
                  Module.__put_attribute__(MyModule, :doc, {0, false}, nil)
                  def stop(_state) do
                    :ok
                  end
                  Module.make_overridable(MyModule, Application)))
                  """
                end)
               |> String.trim()
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
