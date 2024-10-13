defmodule ElixirSense.Core.ErlangHtmlTest do
  use ExUnit.Case, async: true

  @tag requires_source: true
  test "integration" do
    for a <- [
          :asn1,
          :common_test,
          :compiler,
          :crypto,
          :debugger,
          :dialyzer,
          :diameter,
          :edoc,
          :eldap,
          :erl_docgen,
          :erl_interface,
          :erts,
          :et,
          :eunit,
          :ftp,
          :inets,
          :jinterface,
          :kernel,
          :megaco,
          :mnesia,
          :observer,
          :odbc,
          :os_mon,
          :parsetools,
          :public_key,
          :reltool,
          :runtime_tools,
          :sasl,
          :snmp,
          :ssh,
          :ssl,
          :stdlib,
          :syntax_tools,
          :tftp,
          :tools,
          :wx,
          :xmerl
        ] do
      Application.load(a)
    end

    loadable = :code.all_available() |> Enum.map(fn {m, _, _} -> :"#{m}" end)

    for m <- loadable,
        t <- [:moduledoc, :docs, :type_docs, :callback_docs],
        do: ElixirSense.Core.Normalized.Code.get_docs(m, t)
  end

  defp to_markdown(ast) do
    ElixirSense.Core.ErlangHtml.to_markdown(ast, :my_mod, :my_app)
  end

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

  test "prerendered" do
    ast = [
      {:pre, [], ["var = asd()"]}
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
           [some link](asd)  
           """ == to_markdown(ast)
  end

  test "empty link" do
    ast = {:a, [href: "asd"], []}

    assert "[](asd)" == to_markdown(ast)
  end

  describe "seemfa" do
    test "full" do
      ast =
        {:a,
         [
           href: "stdlib:gen_server#Module:handle_call/3",
           rel: "https://erlang.org/doc/link/seemfa"
         ], ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/gen_server#Module:handle_call-3)" ==
               to_markdown(ast)
    end

    test "no app" do
      ast =
        {:a, [href: "gen_server#Module:handle_call/3", rel: "https://erlang.org/doc/link/seemfa"],
         ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/gen_server#Module:handle_call-3)" ==
               to_markdown(ast)
    end

    test "no app no module" do
      ast =
        {:a, [href: "#Module:handle_call/3", rel: "https://erlang.org/doc/link/seemfa"],
         ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/my_mod#Module:handle_call-3)" ==
               to_markdown(ast)
    end
  end

  describe "seeerl" do
    test "full" do
      ast =
        {:a, [href: "stdlib:string#oldapi", rel: "https://erlang.org/doc/link/seeerl"],
         ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/string#oldapi)" == to_markdown(ast)
    end

    test "no app" do
      ast = {:a, [href: "init", rel: "https://erlang.org/doc/link/seeerl"], ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/init)" == to_markdown(ast)
    end

    test "no app no module" do
      ast = {:a, [href: "#some", rel: "https://erlang.org/doc/link/seeerl"], ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/my_mod#some)" == to_markdown(ast)
    end
  end

  describe "seetype" do
    test "full" do
      ast =
        {:a, [href: "stdlib:gen_server#server_ref", rel: "https://erlang.org/doc/link/seetype"],
         ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/gen_server#type-server_ref)" ==
               to_markdown(ast)
    end

    test "no app" do
      ast =
        {:a, [href: "gen_server#server_ref", rel: "https://erlang.org/doc/link/seetype"],
         ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/gen_server#type-server_ref)" ==
               to_markdown(ast)
    end

    test "no app no module" do
      ast = {:a, [href: "#server_ref", rel: "https://erlang.org/doc/link/seetype"], ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/my_mod#type-server_ref)" ==
               to_markdown(ast)
    end
  end

  describe "seeapp" do
    test "full index" do
      ast = {:a, [href: "stdlib:index", rel: "https://erlang.org/doc/link/seeapp"], ["some link"]}

      assert "[some link](https://www.erlang.org/doc/apps/stdlib/)" == to_markdown(ast)
    end

    test "full app capitalized" do
      ast =
        {:a, [href: "stdlib:STDLIB_app#some", rel: "https://erlang.org/doc/link/seeapp"],
         ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/stdlib_app#some)" == to_markdown(ast)
    end

    test "no app" do
      ast =
        {:a, [href: "os_mon_app#some", rel: "https://erlang.org/doc/link/seeapp"], ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/os_mon_app#some)" == to_markdown(ast)
    end

    test "only hash" do
      ast = {:a, [href: "#some", rel: "https://erlang.org/doc/link/seeapp"], ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/my_app_app#some)" == to_markdown(ast)
    end
  end

  describe "seecom" do
    test "full" do
      ast =
        {:a, [href: "stdlib:string#oldapi", rel: "https://erlang.org/doc/link/seecom"],
         ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/string#oldapi)" == to_markdown(ast)
    end

    test "no app" do
      ast = {:a, [href: "init", rel: "https://erlang.org/doc/link/seecom"], ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/init)" == to_markdown(ast)
    end

    test "no app no module" do
      ast = {:a, [href: "#some", rel: "https://erlang.org/doc/link/seecom"], ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/my_mod#some)" == to_markdown(ast)
    end
  end

  describe "seecref" do
    test "full" do
      ast =
        {:a, [href: "stdlib:string#oldapi", rel: "https://erlang.org/doc/link/seecref"],
         ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/string#oldapi)" == to_markdown(ast)
    end

    test "no app" do
      ast = {:a, [href: "init", rel: "https://erlang.org/doc/link/seecref"], ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/init)" == to_markdown(ast)
    end

    test "no app no module" do
      ast = {:a, [href: "#some", rel: "https://erlang.org/doc/link/seecref"], ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/my_mod#some)" == to_markdown(ast)
    end
  end

  describe "seefile" do
    test "full" do
      ast =
        {:a, [href: "stdlib:string#oldapi", rel: "https://erlang.org/doc/link/seefile"],
         ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/string#oldapi)" == to_markdown(ast)
    end

    test "no app" do
      ast =
        {:a, [href: "figures/perf-beamasm.svg", rel: "https://erlang.org/doc/link/seefile"],
         ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/figures/perf-beamasm.svg)" ==
               to_markdown(ast)
    end

    test "no app no module" do
      ast = {:a, [href: "#some", rel: "https://erlang.org/doc/link/seefile"], ["some link"]}

      assert "[some link](https://www.erlang.org/doc/man/my_mod#some)" == to_markdown(ast)
    end
  end

  describe "seeguide" do
    test "system" do
      ast =
        {:a,
         [
           href: "system/design_principles:gen_server_concepts",
           rel: "https://erlang.org/doc/link/seeguide"
         ], ["some link"]}

      assert "[some link](https://www.erlang.org/doc/design_principles/gen_server_concepts)" ==
               to_markdown(ast)
    end

    test "system index" do
      ast =
        {:a,
         [href: "system/design_principles:index", rel: "https://erlang.org/doc/link/seeguide"],
         ["some link"]}

      assert "[some link](https://www.erlang.org/doc/design_principles/users_guide)" ==
               to_markdown(ast)
    end

    test "full guide" do
      ast =
        {:a, [href: "stdlib:string#old api", rel: "https://erlang.org/doc/link/seeguide"],
         ["some link"]}

      assert "[some link](https://www.erlang.org/doc/apps/stdlib/string#old%20api)" ==
               to_markdown(ast)
    end

    test "full index" do
      ast =
        {:a, [href: "stdlib:index", rel: "https://erlang.org/doc/link/seeguide"], ["some link"]}

      assert "[some link](https://www.erlang.org/doc/apps/stdlib/users_guide)" == to_markdown(ast)
    end

    test "no app" do
      ast = {:a, [href: "guide", rel: "https://erlang.org/doc/link/seeguide"], ["some link"]}

      assert "[some link](https://www.erlang.org/doc/apps/my_app/guide)" == to_markdown(ast)
    end

    test "no app no module" do
      ast = {:a, [href: "#some", rel: "https://erlang.org/doc/link/seeguide"], ["some link"]}

      assert "[some link](https://www.erlang.org/doc/apps/my_app/#some)" == to_markdown(ast)
    end
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
