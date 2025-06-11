defmodule ElixirSense.Core.SourceTest do
  use ExUnit.Case, async: true
  doctest ElixirSense.Core.Source

  import ElixirSense.Core.Source

  describe "which_func/1" do
    test "at the beginning of a defmodule" do
      assert nil == which_func("defmo")
    end

    test "functions without namespace" do
      assert %{
               candidate: {nil, :func},
               elixir_prefix: false,
               npar: 0,
               pos: {{1, 7}, {1, nil}}
             } = which_func("var = func(")

      assert %{
               candidate: {nil, :func},
               elixir_prefix: false,
               npar: 1,
               pos: {{1, 7}, {1, nil}}
             } = which_func("var = func(param1, ")
    end

    test "functions with namespace" do
      assert %{
               candidate: {Mod, :func},
               elixir_prefix: false,
               npar: 1,
               pos: {{1, 10}, {1, nil}}
             } = which_func("var = Mod.func(param1, par")

      assert %{
               candidate: {Mod.SubMod, :func},
               elixir_prefix: false,
               npar: 2,
               pos: {{1, 17}, {1, nil}}
             } = which_func("var = Mod.SubMod.func(param1, param2, par")

      assert %{
               candidate: {SubMod, :func},
               elixir_prefix: true,
               npar: 2,
               pos: {{1, 20}, {1, nil}}
             } = which_func("var = Elixir.SubMod.func(param1, param2, par")
    end

    test "functions with namespace atom module" do
      assert %{
               candidate: {Mod, :func},
               elixir_prefix: false,
               npar: 1,
               pos: {{1, 20}, {1, nil}}
             } = which_func("var = :\"Elixir.Mod\".func(param1, par")
    end

    test "functions with namespace __MODULE__" do
      assert %{
               candidate: {Mod, :func},
               elixir_prefix: false,
               npar: 1,
               pos: {{1, 17}, {1, nil}}
             } =
               which_func("var = __MODULE__.func(param1, par", %ElixirSense.Core.Binding{
                 module: Mod
               })

      assert %{
               candidate: {Mod.Sub, :func},
               elixir_prefix: false,
               npar: 1,
               pos: {{1, 21}, {1, nil}}
             } =
               which_func("var = __MODULE__.Sub.func(param1, par", %ElixirSense.Core.Binding{
                 module: Mod
               })
    end

    test "nested functions calls" do
      assert %{
               candidate: {Mod.SubMod, :func},
               elixir_prefix: false,
               npar: 1,
               pos: {{1, 28}, {1, nil}}
             } = which_func("var = outer_func(Mod.SubMod.func(param1,")

      assert %{
               candidate: {nil, :inner_func},
               elixir_prefix: false,
               npar: 0,
               pos: {{1, 43}, {1, nil}}
             } = which_func("var = outer_func(Mod.SubMod.func(param1, [inner_func(")

      assert %{
               candidate: {nil, :func},
               elixir_prefix: false,
               npar: 2,
               pos: {{1, 18}, {1, nil}}
             } = which_func("var = outer_func(func(param1, inner_func, ")

      assert %{
               candidate: {nil, :func},
               elixir_prefix: false,
               npar: 2,
               pos: {{1, 18}, {1, nil}}
             } = which_func("var = outer_func(func(param1, inner_func(), ")

      assert %{
               candidate: {nil, :func},
               elixir_prefix: false,
               npar: 3,
               pos: {{1, 7}, {1, nil}}
             } = which_func("var = func(param1, func2(fun(p3), 4, 5), func3(p1, p2), ")
    end

    test "function call with multiple lines" do
      assert %{
               candidate: {Mod, :func},
               elixir_prefix: false,
               npar: 2,
               pos: {{1, 10}, {1, nil}}
             } =
               which_func("""
               var = Mod.func(param1,
                 param2,

               """)
    end

    test "after double quotes" do
      assert %{
               candidate: {nil, :func},
               elixir_prefix: false,
               npar: 1,
               pos: {{1, 7}, {1, nil}}
             } = which_func("var = func(param1, \"not_a_func(, ")

      assert %{
               candidate: {nil, :func},
               elixir_prefix: false,
               npar: 1,
               pos: {{1, 7}, {1, nil}}
             } = which_func("var = func(\"a_string_(param1\", ")
    end

    test "with operators" do
      assert %{
               candidate: {nil, :func2},
               elixir_prefix: false,
               npar: 1,
               pos: {{1, 26}, {1, nil}}
             } = which_func("var = Mod.func1(param) + func2(param1, ")
    end

    test "unfinished param" do
      assert %{
               candidate: {nil, :func2},
               elixir_prefix: false,
               npar: 0,
               pos: {{1, 1}, {1, nil}}
             } = which_func("func2(param1")
    end

    test "no param" do
      assert %{
               candidate: {nil, :func2},
               elixir_prefix: false,
               npar: 0,
               pos: {{1, 1}, {1, nil}}
             } = which_func("func2(")

      assert %{
               candidate: {nil, :func2},
               elixir_prefix: false,
               npar: 1,
               pos: {{1, 1}, {1, nil}}
             } = which_func("func2(a + b,")
    end

    test "erlang functions" do
      assert %{
               candidate: {:global, :whereis_name},
               elixir_prefix: false,
               npar: 0,
               pos: {{1, 14}, {1, nil}}
             } = which_func("var = :global.whereis_name( ")
    end

    test "call on variable" do
      assert nil == which_func("var = my_var.(")
    end

    test "call dynamic module variable function" do
      assert nil == which_func("var = my_var.some(")

      assert nil ==
               which_func("var = my_var.some(", %ElixirSense.Core.Binding{
                 vars: [
                   %ElixirSense.Core.State.VarInfo{
                     name: "my_var",
                     version: 1,
                     type: {:atom, Some}
                   }
                 ]
               })
    end

    test "call on result of other call" do
      assert nil == which_func("var = my_fun().( ")
    end

    test "call on dynamic module from function" do
      assert nil == which_func("var = my_fun().some( ")
    end

    test "call on dynamic module from attribute" do
      assert nil == which_func("var = @my_var.some(")

      assert nil ==
               which_func("var = @my_var.some(", %ElixirSense.Core.Binding{
                 attributes: [%{name: "my_var", type: {:atom, Some}}]
               })
    end

    test "with fn" do
      assert nil == which_func("fn(a, ")
    end

    test "inside fn body" do
      if Version.match?(System.version(), ">= 1.17.0") do
        assert %{
                 candidate: {nil, :*},
                 elixir_prefix: false,
                 npar: 1,
                 pos: {{1, 33}, {1, nil}}
               } = which_func("var = Enum.map([1,2], fn(i) -> i*")
      else
        assert %{
                 candidate: {Enum, :map},
                 elixir_prefix: false,
                 npar: 1,
                 pos: {{1, 11}, {1, nil}}
               } = which_func("var = Enum.map([1,2], fn(i) -> i*")
      end
    end

    test "inside a list" do
      assert %{
               candidate: {Enum, :map},
               cursor_at_option: true,
               npar: 0,
               option: nil,
               options_so_far: []
             } = which_func("var = Enum.map([")

      assert %{
               candidate: {Enum, :map},
               cursor_at_option: true,
               npar: 0,
               option: nil,
               options_so_far: []
             } = which_func("var = Enum.map([1")

      assert %{
               candidate: {Enum, :map},
               cursor_at_option: true,
               npar: 0,
               option: nil,
               options_so_far: []
             } = which_func("var = Enum.map([1,")

      assert %{
               candidate: {Enum, :map},
               cursor_at_option: true,
               npar: 0,
               option: nil,
               options_so_far: []
             } = which_func("var = Enum.map([1, ")

      assert %{
               candidate: {Enum, :map},
               cursor_at_option: true,
               npar: 0,
               option: nil,
               options_so_far: []
             } = which_func("var = Enum.map([1, 2")

      assert %{
               candidate: {Enum, :map},
               cursor_at_option: true,
               npar: 0,
               option: nil,
               options_so_far: []
             } = which_func("var = Enum.map([1,2,3")
    end

    test "inside a keyword list" do
      assert %{
               candidate: {Enum, :map},
               cursor_at_option: true,
               npar: 0,
               option: nil,
               options_so_far: []
             } = which_func("var = Enum.map([")

      assert %{
               candidate: {Enum, :map},
               cursor_at_option: true,
               npar: 0,
               option: nil,
               options_so_far: []
             } = which_func("var = Enum.map([a")

      assert %{
               candidate: {Enum, :map},
               cursor_at_option: true,
               npar: 0,
               option: nil,
               options_so_far: []
             } = which_func("var = Enum.map([a:")

      assert %{
               candidate: {Enum, :map},
               cursor_at_option: false,
               npar: 0,
               option: :a,
               options_so_far: []
             } = which_func("var = Enum.map([a: ")

      assert %{
               candidate: {Enum, :map},
               cursor_at_option: false,
               npar: 0,
               option: :a,
               options_so_far: []
             } = which_func("var = Enum.map([a: 1")

      assert %{
               candidate: {Enum, :map},
               cursor_at_option: true,
               npar: 0,
               option: nil,
               options_so_far: [:a]
             } = which_func("var = Enum.map([a: 1,")

      assert %{
               candidate: {Enum, :map},
               cursor_at_option: true,
               npar: 0,
               option: nil,
               options_so_far: [:a]
             } = which_func("var = Enum.map([a: 1, ")

      assert %{
               candidate: {Enum, :map},
               cursor_at_option: true,
               npar: 0,
               option: nil,
               options_so_far: [:a]
             } = which_func("var = Enum.map([a: 1, b")

      assert %{
               candidate: {Enum, :map},
               cursor_at_option: true,
               npar: 0,
               option: nil,
               options_so_far: [:a]
             } = which_func("var = Enum.map([a: 1, b:")

      assert %{
               candidate: {Enum, :map},
               cursor_at_option: false,
               npar: 0,
               option: :b,
               options_so_far: [:a]
             } = which_func("var = Enum.map([a: 1, b: ")

      assert %{
               candidate: {Enum, :map},
               cursor_at_option: true,
               npar: 0,
               option: nil,
               options_so_far: []
             } = which_func("var = Enum.map([:a")

      assert %{
               candidate: {Enum, :map},
               cursor_at_option: true,
               npar: 0,
               option: nil,
               options_so_far: []
             } = which_func("var = Enum.map([:a,")

      assert %{
               candidate: {Enum, :map},
               cursor_at_option: true,
               npar: 0,
               option: nil,
               options_so_far: []
             } = which_func("var = Enum.map([:a, ")

      assert %{
               candidate: {Enum, :map},
               cursor_at_option: false,
               npar: 0,
               option: :b,
               options_so_far: []
             } = which_func("var = Enum.map([:a, b: ")
    end

    test "inside a list with a list before" do
      assert %{
               params: [[1, 2]],
               candidate: {Enum, :map},
               npar: 1,
               cursor_at_option: true,
               option: nil,
               options_so_far: []
             } = which_func("var = Enum.map([1,2], [1, ")
    end

    test "inside a keyword list as last arg" do
      assert %{
               candidate: {IO, :inspect},
               elixir_prefix: false,
               npar: 1,
               pos: {{1, 9}, {1, nil}}
             } = which_func("var = IO.inspect([1,2], limit: 100, ")
    end

    test "inside a keyword list as last arg with last key without value" do
      assert %{
               candidate: {IO, :inspect},
               elixir_prefix: false,
               npar: 1
               #  pos: {{1, 7}, {1, nil}}
             } = which_func("var = IO.inspect([1,2], limit: 100, labe: ")
    end

    test "inside a keyword list as last arg with last key with value" do
      assert %{
               candidate: {IO, :inspect},
               elixir_prefix: false,
               npar: 1
               #  pos: {{1, 7}, {1, nil}}
             } = which_func("var = IO.inspect([1,2], limit: 100, labe: :a")
    end

    test "inside a keyword list as last arg with more than one key" do
      assert %{
               candidate: {IO, :inspect},
               elixir_prefix: false,
               npar: 1,
               pos: {{1, 9}, {1, nil}}
             } = which_func("var = IO.inspect([1,2], limit: 100, label: :a, ")
    end

    test "inside a delimited keyword list as last arg" do
      assert %{
               candidate: {IO, :inspect},
               elixir_prefix: false,
               npar: 1
               #  pos: {{1, 7}, {1, nil}}
             } = which_func("var = IO.inspect([1,2], [limit: 1, ")
    end

    test "inside a map" do
      assert nil == which_func("var = IO.inspect(%{a")
      assert nil == which_func("var = IO.inspect(%{a:")
      assert nil == which_func("var = IO.inspect(%{a: ")
      assert nil == which_func("var = IO.inspect(%{a: 2")
      assert nil == which_func("var = IO.inspect(%{a: 2,")
      assert nil == which_func("var = IO.inspect(%{a: 2, ")
      assert nil == which_func("var = IO.inspect(%{a: 1, b: ")
    end

    test "inside a struct" do
      assert nil == which_func("var = IO.inspect(%Str{a")
      assert nil == which_func("var = IO.inspect(%Str{a:")
      assert nil == which_func("var = IO.inspect(%Str{a: ")
      assert nil == which_func("var = IO.inspect(%Str{a: 2")
      assert nil == which_func("var = IO.inspect(%Str{a: 2,")
      assert nil == which_func("var = IO.inspect(%Str{a: 2, ")
      assert nil == which_func("var = IO.inspect(%Str{a: 1, b: ")
    end

    test "inside a tuple" do
      assert nil == which_func("var = Enum.map({")
      assert nil == which_func("var = Enum.map({1")
      assert nil == which_func("var = Enum.map({1,2")
      assert nil == which_func("var = Enum.map({1,2,3")
    end

    test "inside a tuple with another tuple before" do
      assert nil == which_func("var = Enum.map({1,2}, {1, ")
    end

    test "inside a tuple inside a list" do
      assert nil == which_func("var = Enum.map({1,2}, [{1, ")
    end

    test "inside a tuple after comma" do
      assert nil == which_func("var = Enum.map([{1,")
    end

    test "inside a list inside a tuple inside a list" do
      assert nil == which_func("var = Enum.map([{1,[a, ")
    end

    test "fails when code has parsing errors before the cursor" do
      assert nil == which_func("} = Enum.map(list, ")
    end

    test "inside parens" do
      assert %{
               candidate: {Enum, :map},
               elixir_prefix: false,
               npar: 0,
               pos: {{1, 11}, {1, nil}}
             } = which_func("var = Enum.map((1")
    end

    test "after parens" do
      assert %{
               candidate: {Enum, :map},
               elixir_prefix: false,
               npar: 1,
               pos: {{1, 11}, {1, nil}}
             } = which_func("var = Enum.map((1 + 2), ")
    end

    test "detect if cursor is at a viable option" do
      code = "func(option1: 1, "
      assert %{cursor_at_option: true} = which_func(code)

      code = "func("
      assert %{cursor_at_option: :maybe} = which_func(code)

      code = """
      from(
        u in User,
        \
      """

      assert %{cursor_at_option: :maybe} = which_func(code)

      code = """
      from(
        u in User,
        whe\
      """

      assert %{cursor_at_option: :maybe} = which_func(code)

      code = """
      from(
        u in User,
        where: is_nil(u.email),
        \
      """

      assert %{cursor_at_option: true} = which_func(code)

      code = """
      from(
        u in User,
        where: is_nil(u.email),
        sel\
      """

      assert %{cursor_at_option: true} = which_func(code)

      code = """
      from(
        u in User,
        where: is_nil(u.email),
        limit: \
      """

      assert %{cursor_at_option: false} = which_func(code)

      code = """
      from(
        u in User,
        preload: [friends: [], per\
      """

      # assert %{cursor_at_option: false} = which_func(code)
      assert nil == which_func(code)
    end

    test "retrieve options so far" do
      code = """
      from(
        u in User,
        where: is_nil(u.id),
        preload: [assoc1: [assoc1_1: [], assoc1_2: []]],
        limit: 10,
        sel\
      """

      assert %{options_so_far: [:where, :preload, :limit]} = which_func(code)

      code = """
      from(
        u in User,
        where: is_nil(u.id),
        preload: [assoc1: [assoc1_1: [], assoc1_2: [], \
      """

      # assert %{options_so_far: [:where, :preload]} = which_func(code)
      assert nil == which_func(code)
    end

    test "identify current option, if any" do
      code = """
      from(
        u in User,
        where: is_nil(u.id),
        preload: [assoc1: [assoc1_1: [], assoc1_2: []]],
        limit: 10,
        select: \
      """

      assert %{option: :select} = which_func(code)

      code = """
      from(
        u in User,
        where: is_nil(u.id),
        preload: [assoc1: [assoc1_1: [], assoc1_2: []]],
        limit: 10,
        select: u\
      """

      assert %{option: :select} = which_func(code)

      code = """
      from(
        u in User,
        where: is_nil(u.id),
        preload: [assoc1: [assoc1_1: [], assoc1_2: []]],
        limit: 10,
        sel\
      """

      assert %{option: nil} = which_func(code)

      code = """
      from(
        u in User,
        where: is_nil(u.id),
        preload: [assoc1: [assoc1_1: [], assoc1_2: [], \
      """

      # assert %{option: nil} = which_func(code)
      assert nil == which_func(code)
    end

    test "functions without parens on first argument" do
      code = "from "
      assert %{candidate: {nil, :from}, npar: 0} = which_func(code)

      code = "from u in "

      if Version.match?(System.version(), ">= 1.15.0") do
        assert %{candidate: {nil, :in}, npar: 1} = which_func(code)
      else
        assert nil == which_func(code)
      end
    end

    test "finds assoc" do
      code = """
      import Ecto.Query
      alias ElixirSense.Plugins.Ecto.FakeSchemas.Post

      def query() do
        from(
          p in Post,
          join: c in assoc(p,\
      """

      assert %{candidate: {nil, :assoc}, npar: 1} = which_func(code)
    end

    test "functions without parens on second argument" do
      code = "field :name, "
      assert %{candidate: {nil, :field}, npar: 1} = which_func(code)

      code = """
      field :name,
      """

      assert %{candidate: {nil, :field}, npar: 1} = which_func(code)

      code = """
      from u in User,
        where: not is_nil(u.id),
        preload: [assoc1: [assoc1_1: [], assoc1_2: []]],
        limit: 10,
        where: u.name == u.email,
        sel\
      """

      assert %{candidate: {nil, :from}, npar: 1} = which_func(code)

      code = "from c in Comment, join: p in Post, "
      assert %{candidate: {nil, :from}, npar: 1} = which_func(code)

      code = """
      from c in Comment,
        join: p in Post,
        on:
      """

      assert %{candidate: {nil, :from}, npar: 1} = which_func(code)
    end

    test "function without parens is ignored on a new line if it's already valid" do
      code = """
      field
      """

      assert nil == which_func(code)

      code = """
      field :name
      """

      assert nil == which_func(code)
    end
  end

  describe "text_before/3" do
    test "functions without namespace" do
      code = """
      defmodule MyMod do
        def my_func(par1, )
      end
      """

      text =
        """
        defmodule MyMod do
          def my_func(par1,
        """
        |> String.trim()

      assert text_before(code, 2, 20) == text
    end
  end

  describe "split_at/2" do
    test "empty list" do
      code = """
      defmodule Abcd do
        def go do
          :ok
        end
      end
      """

      assert split_at(code, []) == [code]
    end

    test "one element list" do
      code = """
      defmodule Abcd do
        def go do
          :ok
        end
      end
      """

      parts = split_at(code, [{2, 3}])
      assert parts == ["defmodule Abcd do\n  ", "def go do\n    :ok\n  end\nend\n"]
      assert Enum.join(parts) == code
    end

    test "one element list past line length" do
      code = """
      defmodule Abcd do
        def go do
          :ok
        end
      end
      """

      parts = split_at(code, [{2, 12}])
      assert parts == ["defmodule Abcd do\n  def go do", "\n    :ok\n  end\nend\n"]

      parts = split_at(code, [{2, 13}])
      assert parts == ["defmodule Abcd do\n  def go do ", "\n    :ok\n  end\nend\n"]
    end

    test "two element list same line" do
      code = """
      defmodule Abcd do
        def go do
          :ok
        end
      end
      """

      parts = split_at(code, [{2, 3}, {2, 6}])
      assert parts == ["defmodule Abcd do\n  ", "def", " go do\n    :ok\n  end\nend\n"]
      assert Enum.join(parts) == code
    end

    test "two element list different lines" do
      code = """
      defmodule Abcd do
        def go do
          :ok
        end
      end
      """

      parts = split_at(code, [{2, 3}, {4, 6}])
      assert parts == ["defmodule Abcd do\n  ", "def go do\n    :ok\n  end", "\nend\n"]
      assert Enum.join(parts) == code
    end

    test "handles positions at start and end of code" do
      code = "abcdef"
      positions = [{1, 1}, {1, 7}]
      assert split_at(code, positions) == ["", "abcdef", ""]
    end

    test "handles positions beyond code length" do
      code = "short"
      positions = [{0, 0}, {10, 15}]
      assert split_at(code, positions) == ["", "short         ", ""]
    end
  end

  describe "which_struct" do
    test "map" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %{
      """

      assert which_struct(code, MyMod) == {:map, [], nil}
    end

    test "map update variable" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %{asd |
      """

      assert which_struct(code, MyMod) == {:map, [], {:variable, :asd, :any}}
    end

    test "map update attribute" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %{@asd |
      """

      assert which_struct(code, MyMod) == {:map, [], {:attribute, :asd}}
    end

    test "map update with fields" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %{asd | qwe: "ds",
      """

      assert which_struct(code, MyMod) == {:map, [:qwe], {:variable, :asd, :any}}
    end

    test "pattern match with _" do
      code = """
      defmodule MyMod do
        def my_func(%_{
      """

      assert which_struct(code, MyMod) == {nil, [], false, nil}
    end

    test "pattern match with variable name" do
      code = """
      defmodule MyMod do
        def my_func(%my_var{
      """

      assert which_struct(code, MyMod) == {nil, [], false, nil}
    end

    test "modules without namespace" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %Mod{
      """

      assert which_struct(code, MyMod) == {{:atom, Mod}, [], false, nil}
    end

    test "modules with Elixir prefix" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %Elixir.Mod{
      """

      assert which_struct(code, MyMod) == {{:atom, Mod}, [], true, nil}
    end

    test "modules with namespace" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %ModA.ModB{
      """

      assert which_struct(code, MyMod) == {{:atom, ModA.ModB}, [], false, nil}
    end

    test "`__MODULE__` special form" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %__MODULE__{
      """

      assert which_struct(code, MyMod) == {{:atom, MyMod}, [], false, nil}
    end

    test "attribute struct type" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %@attr{
      """

      assert which_struct(code, MyMod) == {{:attribute, :attr}, [], false, nil}
    end

    test "`__MODULE__.Submodule` special form" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %__MODULE__.Submodule{
      """

      assert which_struct(code, MyMod) == {{:atom, MyMod.Submodule}, [], false, nil}
    end

    test "modules atom form" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %:"Elixir.IO.Stream"{
      """

      assert which_struct(code, MyMod) == {{:atom, IO.Stream}, [], false, nil}
    end

    test "modules erlang atom" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %:my_module{
      """

      assert which_struct(code, MyMod) == {{:atom, :my_module}, [], false, nil}
    end

    test "nested structs" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %Mod{field1: %InnerMod{}, field2: {}, field3: []}
        end
      end
      """

      assert which_struct(text_before(code, 3, 16), MyMod) == {{:atom, Mod}, [], false, nil}
      assert which_struct(text_before(code, 3, 23), MyMod) == nil
      assert which_struct(text_before(code, 3, 34), MyMod) == {{:atom, InnerMod}, [], false, nil}

      assert which_struct(text_before(code, 3, 37), MyMod) ==
               {{:atom, Mod}, [:field1], false, nil}

      assert which_struct(text_before(code, 3, 39), MyMod) ==
               {{:atom, Mod}, [:field1], false, nil}

      assert which_struct(text_before(code, 3, 50), MyMod) ==
               {{:atom, Mod}, [:field1, :field2], false, nil}
    end

    test "nested structs with multiple lines" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %Mod{
            field1: %InnerMod{},
            field2: {},
            field3: %{
              field4: %{}
            },
          }
        end
      end
      """

      assert which_struct(text_before(code, 7, 8), MyMod) == {:map, [], nil}

      assert which_struct(text_before(code, 8, 9), MyMod) ==
               {{:atom, Mod}, [:field1, :field2, :field3], false, nil}
    end

    test "nested structs with multiple lines when line shorter than col" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %Mod{
            field1: %InnerMod{},

          }
        end
      end
      """

      assert which_struct(text_before(code, 5, 7), MyMod) == {{:atom, Mod}, [:field1], false, nil}
    end

    test "struct update variable syntax" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %Mod{par1 |
            field1: %InnerMod{},

          }
        end
      end
      """

      assert which_struct(text_before(code, 3, 23), MyMod) ==
               {{:atom, Mod}, [], false, {:variable, :par1, :any}}

      assert which_struct(text_before(code, 5, 7), MyMod) ==
               {{:atom, Mod}, [:field1], false, {:variable, :par1, :any}}
    end

    test "struct update attribute syntax" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %Mod{@par1 |
            field1: %InnerMod{},

          }
        end
      end
      """

      assert which_struct(text_before(code, 3, 23), MyMod) ==
               {{:atom, Mod}, [], false, {:attribute, :par1}}

      assert which_struct(text_before(code, 5, 7), MyMod) ==
               {{:atom, Mod}, [:field1], false, {:attribute, :par1}}
    end
  end

  describe "multi alias syntax" do
    test "single level" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          alias Mod.{
      """

      assert get_v12_module_prefix(code, MyMod) == "Mod"
    end

    test "Elixir prefix" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          alias Elixir.Mod.{
      """

      assert get_v12_module_prefix(code, MyMod) == "Elixir.Mod"
    end

    test "Elixir prefix single level" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          alias Elixir.{
      """

      assert get_v12_module_prefix(code, MyMod) == "Elixir"
    end

    test "single level with submodule" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          require Mod.{Su
      """

      assert get_v12_module_prefix(code, MyMod) == "Mod"
    end

    test "single level with submodules" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          use Mod.{Su.Bmod, Other
      """

      assert get_v12_module_prefix(code, MyMod) == "Mod"
    end

    test "multi level with submodules" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          import Mod.Sub.{
      """

      assert get_v12_module_prefix(code, MyMod) == "Mod.Sub"
    end

    test "__MODULE__ special form level with submodules" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          alias __MODULE__.{
      """

      assert get_v12_module_prefix(code, MyMod) == "MyMod"
    end

    test "__MODULE__.Submodule special form level with submodules" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          alias __MODULE__.Submodule.{
      """

      assert get_v12_module_prefix(code, MyMod) == "MyMod.Submodule"
    end

    test "atom module special form level with submodules" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          alias :"Elixir.Mod".{
      """

      assert get_v12_module_prefix(code, MyMod) == "Mod"
    end

    test "nil when closed" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          alias Mod.{}
      """

      assert get_v12_module_prefix(code, MyMod) == nil
    end

    test "nil when not on last line" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          alias Mod.{A, B}
          alias C
      """

      assert get_v12_module_prefix(code, MyMod) == nil
    end

    test "multiline" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          alias Mod.{A,
            B
      """

      assert get_v12_module_prefix(code, MyMod) == "Mod"
    end
  end

  describe "walk_text/3" do
    test "walks through text until a criteria is reached" do
      text = """
      a b c
      d-e-f
      g.h.i
      j,k,l
      """

      {_, line, col} =
        walk_text(text, nil, fn
          grapheme, rest, _, _, _ when grapheme != "i" ->
            {rest, nil}

          _grapheme, rest, line, col, _ ->
            {"", {rest, line - 1, col - 1}}
        end)

      assert line == 2
      assert col == 4
    end
  end

  describe "bitstring_options" do
    test "single line empty" do
      text = "<<ident::"
      assert "" == bitstring_options(text)
    end

    test "single line not empty" do
      text = "<<ident::int"
      assert "int" == bitstring_options(text)
    end

    test "single line multiple" do
      text = "<<ident::integer, some::"
      assert "" == bitstring_options(text)
    end

    test "single line multiple not empty" do
      text = "<<ident::integer, some::integer-un"
      assert "integer-un" == bitstring_options(text)
    end

    test "single closed" do
      text = "<<ident::integer, some::binary>>"
      assert nil == bitstring_options(text)
    end

    test "single line ident" do
      text = "<<ident"
      assert nil == bitstring_options(text)
    end

    test "single line opening" do
      text = "<<"
      assert nil == bitstring_options(text)
    end

    test "multi line multiple not empty" do
      text = """
      <<ident::integer,
        some::integer-un\
      """

      assert "integer-un" == bitstring_options(text)
    end
  end

  describe "prefix/3" do
    test "returns empty string when no prefix is found" do
      code = "def example do\n  :ok\nend"
      assert "" == prefix(code, 2, 3)
    end

    test "returns the correct prefix" do
      code = "defmodule Test do\n  def example_func do\n    :ok\n  end\nend"
      assert "example_f" == prefix(code, 2, 16)
    end

    test "handles line shorter than column" do
      code = "short"
      assert "" == prefix(code, 1, 10)
    end

    test "handles line outside range" do
      code = "short"
      assert "" == prefix(code, 3, 1)
    end

    test "returns prefix with special characters" do
      code = "def example?!:@&^~+-<>=*/|\\() do\n  :ok\nend"
      assert "example?!:@&^~+-<>=*/|\\" == prefix(code, 1, 28)
    end

    test "returns prefix at the end of line" do
      code = "def example\ndef another"
      assert "example" == prefix(code, 1, 12)
    end

    test "handles empty lines" do
      code = "\n\ndef example"
      assert "" == prefix(code, 2, 1)
    end

    test "returns prefix with numbers" do
      code = "variable123 = 42"
      assert "variable12" == prefix(code, 1, 11)
    end
  end

  describe "prefix_suffix/3" do
    test "returns empty string when no prefix is found" do
      code = "def example do\n  :ok\nend"
      assert {"", ":ok"} == prefix_suffix(code, 2, 3)
    end

    test "returns the correct prefix" do
      code = "defmodule Test do\n  def example_func do\n    :ok\n  end\nend"
      assert {"example_f", "unc"} == prefix_suffix(code, 2, 16)
    end

    test "handles line shorter than column" do
      code = "short"
      assert {"", ""} == prefix_suffix(code, 1, 10)
    end

    test "handles line outside range" do
      code = "short"
      assert {"", ""} == prefix_suffix(code, 3, 1)
    end

    test "returns prefix with special characters" do
      code = "def example?!:@&^~+-<>=*/|\\() do\n  :ok\nend"
      assert {"example?!:@&^~+-<>=*/", "|\\"} == prefix_suffix(code, 1, 26)
    end

    test "returns prefix at the end of line" do
      code = "def example\ndef another"
      assert {"example", ""} == prefix_suffix(code, 1, 12)
    end

    test "handles empty lines" do
      code = "\n\ndef example"
      assert {"", ""} == prefix_suffix(code, 2, 1)
    end

    test "returns prefix with numbers" do
      code = "variable123 = 42"
      assert {"variable12", "3"} == prefix_suffix(code, 1, 11)
    end
  end
end
