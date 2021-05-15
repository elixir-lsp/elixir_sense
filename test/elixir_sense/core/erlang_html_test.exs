defmodule ElixirSense.Core.ErlangHtmlTest do
  use ExUnit.Case, async: true
  import ElixirSense.Core.ErlangHtml

  # test "integration" do
  #   ElixirSense.Core.Applications.load_all 
  #   for m <- ElixirSense.Core.Applications.loadable_modules, t <- [:moduledoc, :docs, :type_docs, :callback_docs], do: ElixirSense.Core.Normalized.Code.get_docs(m, t)
  # end

  test "binary" do
    ast = "binary"

    assert "binary" == to_markdown(ast)
  end

  test "special chars are sanitized" do
    for char <- [
          "\\",
          "`",
          "*",
          "_",
          "{",
          "}",
          "[",
          "]",
          "<",
          ">",
          "(",
          ")",
          "#",
          "+",
          "-",
          ".",
          "!",
          "|"
        ] do
      ast = char <> " binary"
      expected = "\\" <> char <> " binary"

      assert expected == to_markdown(ast)
    end
  end

  test "list" do
    ast = [
      "bin1",
      "bin2"
    ]

    assert "bin1bin2" == to_markdown(ast)
  end

  test "paragraph" do
    ast = {:p, [], "lorem ipsum"}

    assert """
           lorem ipsum

           """ == to_markdown(ast)
  end

  test "paragraphs" do
    ast = [
      {:p, [], "lorem ipsum"},
      {:p, [], "quick brown fox"}
    ]

    assert """
           lorem ipsum

           quick brown fox

           """ == to_markdown(ast)
  end

  test "line break inside paragraph" do
    ast =
      {:p, [],
       [
         "lorem",
         {:br, [], []},
         "ipsum"
       ]}

    assert """
           lorem  
           ipsum

           """ == to_markdown(ast)
  end

  test "unordered list" do
    ast =
      {:ul, [],
       [
         {:li, [], "first"},
         {:li, [], "second"},
         {:li, [], "third"}
       ]}

    assert """
           - first
           - second
           - third

           """ == to_markdown(ast)
  end

  test "unordered list with paragraphs" do
    ast =
      {:ul, [],
       [
         {:li, [],
          [
            {:p, [], "lorem ipsum"},
            {:p, [], "quick brown fox"}
          ]},
         {:li, [], "second"},
         {:li, [], "third"}
       ]}

    assert """
           - lorem ipsum
             
             quick brown fox
             
             
           - second
           - third

           """ == to_markdown(ast)
  end

  test "ordered list" do
    ast =
      {:ol, [],
       [
         {:li, [], "first"},
         {:li, [], "second"},
         {:li, [], "third"}
       ]}

    assert """
           1. first
           2. second
           3. third

           """ == to_markdown(ast)
  end

  test "description list" do
    ast =
      {:dl, [],
       [
         {:dt, [], "first"},
         {:dd, [], "first definition"},
         {:dt, [], "second"},
         {:dd, [], "second definition"}
       ]}

    assert """
           **first:** first definition  
           **second:** second definition  

           """ == to_markdown(ast)
  end

  test "headings" do
    ast = [
      {:h1, [], "level 1"},
      {:h2, [], "level 2"},
      {:h3, [], "level 3"},
      {:h4, [], "level 4"},
      {:h5, [], "level 5"},
      {:h6, [], "level 6"}
    ]

    assert """
           # level 1

           ## level 2

           ### level 3

           #### level 4

           ##### level 5

           ###### level 6

           """ == to_markdown(ast)
  end

  test "emphasis" do
    ast = [
      {:i, [], "i"},
      {:br, [], []},
      {:em, [], "em"},
      {:br, [], []},
      {:b, [], "bold"},
      {:br, [], []},
      {:strong, [], "strong"},
      {:br, [], []}
    ]

    assert """
           *i*  
           *em*  
           **bold**  
           **strong**  
           """ == to_markdown(ast)
  end

  test "code" do
    ast = [
      {:code, [], "var = asd()"},
      {:br, [], []}
    ]

    assert """
           `var = asd()`  
           """ == to_markdown(ast)
  end

  test "code with backtick" do
    ast = [
      {:code, [], "var = `asd()"},
      {:br, [], []}
    ]

    assert """
           ``var = `asd()``  
           """ == to_markdown(ast)
  end

  test "prerendered code" do
    ast = [
      {:pre, [],
       [
         {:code, [], "var = asd()"}
       ]}
    ]

    assert """
           ```
           var = asd()
           ```
           """ == to_markdown(ast)
  end

  test "link" do
    ast = [
      {:a, [href: "asd"], ["some link"]},
      {:br, [], []}
    ]

    assert """
           [some link]  
           """ == to_markdown(ast)
  end

  test "empty link" do
    ast = {:a, [href: "asd"], []}

    assert "" == to_markdown(ast)
  end

  test "div element" do
    ast =
      {:div, [class: "note"],
       [
         "asd"
       ]}

    assert """


           ---

           NOTE:  
           asd

           ---

           """ == to_markdown(ast)
  end

  test "div element without class" do
    ast =
      {:div, [],
       [
         "asd"
       ]}

    assert """


           ---

           asd

           ---

           """ == to_markdown(ast)
  end
end
