defmodule Alchemist.API.DoclTest do

  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  alias Alchemist.API.Docl

  defp fixture(file) do
    Path.expand("../../fixtures/#{file}", __DIR__)
  end

  test "DOCL request" do
    assert capture_io(fn ->
      Docl.request("{\"defmodule\", \"#{fixture("my_module.ex")}\", 1}")
    end) =~ """
    Defines a module given by name with the given contents.
    """
  end

  test "DOCL request for List.flatten" do
    output = capture_io(fn ->
      Docl.request("{\"List.flatten\", \"#{fixture("my_module.ex")}\", 1}")
    end)
    [docs, _types] = output |> String.split("\u000B")

    assert docs =~ """
    > List.flatten(list)

    ### Specs

    `@spec flatten(deep_list) :: list when deep_list: [any | deep_list]`

    Flattens the given `list` of nested lists.

    ## Examples

        iex> List.flatten([1, [[2], 3]])
        [1, 2, 3]



    ____

    > List.flatten(list, tail)

    ### Specs

    `@spec flatten(deep_list, [elem]) :: [elem] when deep_list: [elem | deep_list], elem: var`

    Flattens the given `list` of nested lists.
    The list `tail` will be added at the end of
    the flattened list.

    ## Examples

        iex> List.flatten([1, [[2], 3]], [4, 5])
        [1, 2, 3, 4, 5]

    """
  end

  test "DOCL request for MyCustomList.flatten with alias" do
    output = capture_io(fn ->
      Docl.request("{\"MyCustomList.flatten\", \"#{fixture("my_module.ex")}\", 4}")
      # Docl.process(["MyCustomList.flatten", [], [{MyCustomList, List}]])
    end)
    [docs, _types] = output |> String.split("\u000B")

    assert docs =~ """
    > List.flatten(list)

    ### Specs

    `@spec flatten(deep_list) :: list when deep_list: [any | deep_list]`

    Flattens the given `list` of nested lists.

    ## Examples

        iex> List.flatten([1, [[2], 3]])
        [1, 2, 3]



    ____

    > List.flatten(list, tail)

    ### Specs

    `@spec flatten(deep_list, [elem]) :: [elem] when deep_list: [elem | deep_list], elem: var`

    Flattens the given `list` of nested lists.
    The list `tail` will be added at the end of
    the flattened list.

    ## Examples

        iex> List.flatten([1, [[2], 3]], [4, 5])
        [1, 2, 3, 4, 5]

    """
  end

  test "DOCL request for search create_file with import" do
    assert capture_io(fn ->
      Docl.request("{\"create_file\", \"#{fixture("my_module.ex")}\", 4}")
    end) =~ """
    > Mix.Generator.create_file(path, contents, opts \\\\\\\\ [])

    ### Specs

    `@spec create_file(Path.t, iodata, Keyword.t) :: any`

    Creates a file with the given contents.
    If the file already exists, asks for user confirmation.

    ## Options

      * `:force` - forces installation without a shell prompt.

    """
  end

  test "DOCL request for defmacro" do
    output = capture_io(fn ->
      Docl.request("{\"defmacro\", \"#{fixture("my_module.ex")}\", 4}")
    end)
    [docs, _types] = output |> String.split("\u000B")

    assert docs =~ """
    > Kernel.defmacro(call, expr \\\\\\\\ nil)

    Defines a macro with the given name and body.

    ## Examples

        defmodule MyLogic do
          defmacro unless(expr, opts) do
            quote do
              if !unquote(expr), unquote(opts)
            end
          end
        end

        require MyLogic
        MyLogic.unless false do
          IO.puts \"It works\"
        end

    """
  end

end
