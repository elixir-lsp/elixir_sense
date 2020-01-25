defmodule ElixirSense.Core.SourceTest do
  use ExUnit.Case, async: true
  doctest ElixirSense.Core.Source

  import ElixirSense.Core.Source

  describe "which_func/1" do
    test "at the beginning of a defmodule" do
      assert which_func("defmo") ==
               %{
                 candidate: :none,
                 elixir_prefix: false,
                 npar: 0,
                 pipe_before: false,
                 unfinished_parm: false,
                 pos: nil
               }
    end

    test "functions without namespace" do
      assert which_func("var = func(") == %{
               candidate: {nil, :func},
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: false,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }

      assert which_func("var = func(param1, ") == %{
               candidate: {nil, :func},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: false,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "functions with namespace" do
      assert which_func("var = Mod.func(param1, par") == %{
               candidate: {Mod, :func},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }

      assert which_func("var = Mod.SubMod.func(param1, param2, par") == %{
               candidate: {Mod.SubMod, :func},
               elixir_prefix: false,
               npar: 2,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }

      assert which_func("var = Elixir.SubMod.func(param1, param2, par") == %{
               candidate: {SubMod, :func},
               elixir_prefix: true,
               npar: 2,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "functions with namespace atom module" do
      assert which_func("var = :\"Elixir.Mod\".func(param1, par") == %{
               candidate: {Mod, :func},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "functions with namespace __MODULE__" do
      assert which_func("var = __MODULE__.func(param1, par", Mod) == %{
               candidate: {Mod, :func},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }

      assert which_func("var = __MODULE__.Sub.func(param1, par", Mod) == %{
               candidate: {Mod.Sub, :func},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "nested functions calls" do
      assert which_func("var = outer_func(Mod.SubMod.func(param1,") == %{
               candidate: {Mod.SubMod, :func},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: false,
               pipe_before: false,
               pos: {{1, 18}, {1, nil}}
             }

      assert which_func("var = outer_func(Mod.SubMod.func(param1, [inner_func(") == %{
               candidate: {nil, :inner_func},
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: false,
               pipe_before: false,
               pos: {{1, 43}, {1, nil}}
             }

      assert which_func("var = outer_func(func(param1, inner_func, ") == %{
               candidate: {nil, :func},
               elixir_prefix: false,
               npar: 2,
               unfinished_parm: false,
               pipe_before: false,
               pos: {{1, 18}, {1, nil}}
             }

      assert which_func("var = outer_func(func(param1, inner_func(), ") == %{
               candidate: {nil, :func},
               elixir_prefix: false,
               npar: 2,
               unfinished_parm: false,
               pipe_before: false,
               pos: {{1, 18}, {1, nil}}
             }

      assert which_func("var = func(param1, func2(fun(p3), 4, 5), func3(p1, p2), ") == %{
               candidate: {nil, :func},
               elixir_prefix: false,
               npar: 3,
               unfinished_parm: false,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "function call with multiple lines" do
      assert which_func("""
             var = Mod.func(param1,
               param2,

             """) == %{
               candidate: {Mod, :func},
               elixir_prefix: false,
               npar: 2,
               unfinished_parm: false,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "after double quotes" do
      assert which_func("var = func(param1, \"not_a_func(, ") == %{
               candidate: {nil, :func},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: false,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }

      assert which_func("var = func(\"a_string_(param1\", ") == %{
               candidate: {nil, :func},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: false,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "with operators" do
      assert which_func("var = Mod.func1(param) + func2(param1, ") == %{
               candidate: {nil, :func2},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: false,
               pipe_before: false,
               pos: {{1, 26}, {1, nil}}
             }
    end

    test "unfinished param" do
      assert which_func("func2(param1") == %{
               candidate: {nil, :func2},
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 1}, {1, nil}}
             }
    end

    test "no param" do
      assert which_func("func2(") == %{
               candidate: {nil, :func2},
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: false,
               pipe_before: false,
               pos: {{1, 1}, {1, nil}}
             }

      assert which_func("func2(a + b,") == %{
               candidate: {nil, :func2},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: false,
               pipe_before: false,
               pos: {{1, 1}, {1, nil}}
             }
    end

    test "erlang functions" do
      assert which_func("var = :global.whereis_name( ") == %{
               candidate: {:global, :whereis_name},
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: false,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "call on variable" do
      assert which_func("var = my_var.( ") == %{
               candidate: :none,
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: false,
               pipe_before: false,
               pos: nil
             }
    end

    test "call on result of other call" do
      assert which_func("var = my_fun().( ") == %{
               candidate: :none,
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: false,
               pipe_before: false,
               pos: nil
             }
    end

    # FIXME should return {nil, :some} or :none
    test "call on dynamic module from function" do
      assert which_func("var = my_fun().some( ") == %{
               candidate: {nil, :my_fun},
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: false,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    # FIXME handle attribute or return :none
    test "call on dynamic module from attribute" do
      assert which_func("var = @my_attr.some( ") == %{
               candidate: {nil, :some},
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: false,
               pipe_before: false,
               pos: {{1, 16}, {1, nil}}
             }
    end

    test "with fn" do
      assert which_func("fn(a, ") == %{
               candidate: :none,
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: false,
               pipe_before: false,
               pos: nil
             }
    end

    test "with another fn before" do
      assert which_func("var = Enum.sort_by(list, fn(i) -> i*i end, fn(a, ") == %{
               candidate: {Enum, :sort_by},
               elixir_prefix: false,
               npar: 2,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "inside fn body" do
      assert which_func("var = Enum.map([1,2], fn(i) -> i*") == %{
               candidate: {Enum, :map},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "inside a list" do
      assert which_func("var = Enum.map([1,2,3") == %{
               candidate: {Enum, :map},
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "inside a list after comma" do
      assert which_func("var = Enum.map([1,") == %{
               candidate: {Enum, :map},
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "inside an list without items" do
      assert which_func("var = Enum.map([") == %{
               candidate: {Enum, :map},
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "inside a list with a list before" do
      assert which_func("var = Enum.map([1,2], [1, ") == %{
               candidate: {Enum, :map},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "inside a keyword list as last arg" do
      assert which_func("var = IO.inspect([1,2], limit: 100, ") == %{
               candidate: {IO, :inspect},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "inside a keyword list as last arg with last key without value" do
      assert which_func("var = IO.inspect([1,2], limit: 100, labe: ") == %{
               candidate: {IO, :inspect},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "inside a keyword list as last arg with last key with value" do
      assert which_func("var = IO.inspect([1,2], limit: 100, labe: :a") == %{
               candidate: {IO, :inspect},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "inside a keyword list as last arg with more than one key" do
      assert which_func("var = IO.inspect([1,2], limit: 100, label: :a, ") == %{
               candidate: {IO, :inspect},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "inside a delimited keyword list as last arg" do
      assert which_func("var = IO.inspect([1,2], [limit: 1, ") == %{
               candidate: {IO, :inspect},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "inside a map" do
      assert which_func("var = IO.inspect(%{a: 1, b: ") == %{
               candidate: {IO, :inspect},
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "inside a tuple" do
      assert which_func("var = Enum.map({1,2,3") == %{
               candidate: {Enum, :map},
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "inside a tuple with another tuple before" do
      assert which_func("var = Enum.map({1,2}, {1, ") == %{
               candidate: {Enum, :map},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "inside a tuple inside a list" do
      assert which_func("var = Enum.map({1,2}, [{1, ") == %{
               candidate: {Enum, :map},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "inside a tuple after comma" do
      assert which_func("var = Enum.map([{1,") == %{
               candidate: {Enum, :map},
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "inside a list inside a tuple inside a list" do
      assert which_func("var = Enum.map([{1,[a, ") == %{
               candidate: {Enum, :map},
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "fails when code has parsing errors before the cursor" do
      assert which_func("} = Enum.map(list, ") == %{
               candidate: :none,
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: false,
               pipe_before: false,
               pos: nil
             }
    end

    test "inside parens" do
      assert which_func("var = Enum.map((1 + 2") == %{
               candidate: {Enum, :map},
               elixir_prefix: false,
               npar: 0,
               unfinished_parm: true,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
    end

    test "after parens" do
      assert which_func("var = Enum.map((1 + 2), ") == %{
               candidate: {Enum, :map},
               elixir_prefix: false,
               npar: 1,
               unfinished_parm: false,
               pipe_before: false,
               pos: {{1, 7}, {1, nil}}
             }
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

  describe "subject" do
    test "functions without namespace" do
      code = """
      defmodule MyMod do
        my_func(par1, )
      end
      """

      assert subject(code, 2, 5) == "my_func"
    end

    test "functions with namespace" do
      code = """
      defmodule MyMod do
        Mod.func(par1, )
      end
      """

      assert subject(code, 2, 8) == "Mod.func"
    end

    test "functions ending with !" do
      code = """
      defmodule MyMod do
        Mod.func!
      end
      """

      assert subject(code, 2, 8) == "Mod.func!"
    end

    test "functions ending with ?" do
      code = """
      defmodule MyMod do
        func?(par1, )
      end
      """

      assert subject(code, 2, 8) == "func?"
    end

    test "erlang modules" do
      code = """
        :lists.concat([1,2])
      """

      assert subject(code, 1, 5) == ":lists"
    end

    test "atom modules" do
      code = """
        :"Elixir.List".concat([1,2])
        :'Elixir.List'.concat([1,2])
      """

      assert subject(code, 1, 13) == ":\"Elixir.List\""
      assert subject(code, 2, 13) == ":\'Elixir.List\'"
    end

    test "functions from erlang modules" do
      code = """
        :lists.concat([1,2])
      """

      assert subject(code, 1, 12) == ":lists.concat"
    end

    test "functions from atom modules" do
      code = """
        :"Elixir.List".concat([1,2])
        :'Elixir.List'.concat([1,2])
      """

      assert subject(code, 1, 20) == ":\"Elixir.List\".concat"
      assert subject(code, 2, 20) == ":\'Elixir.List\'.concat"
    end

    test "capture operator" do
      code = """
        Emum.map(list, &func/1)
      """

      assert subject(code, 1, 21) == "func"
    end

    test "functions with `!` operator before" do
      code = """
        if !match({_,_}, var) do
      """

      assert subject(code, 1, 8) == "match"
    end

    test "module and function in different lines" do
      code = """
        Mod.
          func
      """

      assert subject(code, 2, 7) == "Mod.func"
    end

    test "elixir module" do
      code = """
      defmodule MyMod do
        ModA.ModB.func
      end
      """

      assert subject(code, 2, 4) == "ModA"
      assert subject(code, 2, 9) == "ModA.ModB"
      assert subject(code, 2, 14) == "ModA.ModB.func"
    end

    test "anonymous functions call" do
      code = """
        my_func.(1,2)
      """

      assert subject(code, 1, 4) == "my_func"
    end

    test "no empty/stop grapheme after subject" do
      code = "Mod.my_func"

      assert subject(code, 1, 2) == "Mod"
      assert subject(code, 1, 6) == "Mod.my_func"
    end

    test "find closest on the edges" do
      code = """
      defmodule MyMod do
        Mod.my_func(par1, par2)
      end
      """

      assert subject(code, 2, 2) == nil
      assert subject(code, 2, 3) == "Mod"
      assert subject(code, 2, 5) == "Mod"
      assert subject(code, 2, 6) == "Mod"
      assert subject(code, 2, 7) == "Mod.my_func"
      assert subject(code, 2, 14) == "Mod.my_func"
      assert subject(code, 2, 15) == "par1"
      assert subject(code, 2, 19) == "par1"
      assert subject(code, 2, 20) == nil
      assert subject(code, 2, 21) == "par2"
    end

    test "module from struct" do
      code = """
      defmodule MyMod do
        Mod.my_func(%MyMod{a: 1})
      end
      """

      assert subject(code, 2, 17) == "MyMod"
    end

    test "function call after comment ending in ." do
      code = """
      defmodule MyMod do
        defp loaded_applications do
          # for performance.
          :ets.match(:ac_tab, {{:loaded, :"$1"}, :_})

          :ets. # for performance.
            match(:ac_tab, {{:loaded, :"$1"}, :_})
        end
      end
      """

      assert subject(code, 4, 10) == ":ets.match"
      assert subject(code, 7, 7) == ":ets.match"
    end
  end

  describe "which_struct" do
    test "patern match with _" do
      code = """
      defmodule MyMod do
        def my_func(%_{
      """

      assert which_struct(code, MyMod) == {:_, [], false}
    end

    test "patern match with variable name" do
      code = """
      defmodule MyMod do
        def my_func(%my_var{
      """

      assert which_struct(code, MyMod) == {:_, [], false}
    end

    test "modules without namespace" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %Mod{
      """

      assert which_struct(code, MyMod) == {Mod, [], false}
    end

    test "modules with Elixir prefix" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %Elixir.Mod{
      """

      assert which_struct(code, MyMod) == {Mod, [], true}
    end

    test "modules with namespace" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %ModA.ModB{
      """

      assert which_struct(code, MyMod) == {ModA.ModB, [], false}
    end

    test "`__MODULE__` special form" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %__MODULE__{
      """

      assert which_struct(code, MyMod) == {MyMod, [], false}
    end

    test "`__MODULE__.Submodule` special form" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %__MODULE__.Submodule{
      """

      assert which_struct(code, MyMod) == {MyMod.Submodule, [], false}
    end

    test "modules atom form" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %:"Elixir.IO.Stream"{
      """

      assert which_struct(code, MyMod) == {IO.Stream, [], false}
    end

    test "modules erlang atom" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %:my_module{
      """

      assert which_struct(code, MyMod) == {:my_module, [], false}
    end

    test "nested structs" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %Mod{field1: %InnerMod{}, field2: {}, field3: []}
        end
      end
      """

      assert which_struct(text_before(code, 3, 16), MyMod) == {Mod, [], false}
      assert which_struct(text_before(code, 3, 23), MyMod) == nil
      assert which_struct(text_before(code, 3, 34), MyMod) == {InnerMod, [], false}
      assert which_struct(text_before(code, 3, 37), MyMod) == {Mod, [:field1], false}
      assert which_struct(text_before(code, 3, 39), MyMod) == {Mod, [:field1], false}
      assert which_struct(text_before(code, 3, 50), MyMod) == {Mod, [:field1, :field2], false}
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

      assert which_struct(text_before(code, 7, 8), MyMod) == nil

      assert which_struct(text_before(code, 8, 9), MyMod) ==
               {Mod, [:field1, :field2, :field3], false}
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

      assert which_struct(text_before(code, 5, 7), MyMod) == {Mod, [:field1], false}
    end

    test "struct update syntax" do
      code = """
      defmodule MyMod do
        def my_func(par1) do
          var = %Mod{par1 |
            field1: %InnerMod{},

          }
        end
      end
      """

      assert which_struct(text_before(code, 3, 23), MyMod) == {Mod, [], false}
      assert which_struct(text_before(code, 5, 7), MyMod) == {Mod, [:field1], false}
    end
  end

  describe "alias syntax v1.2" do
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
end
