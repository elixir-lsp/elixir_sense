defmodule ElixirSense.Core.Normalized.TypespecTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.Normalized.Typespec

  test "get_specs" do
    assert [
             {{:"MACRO-some_macro", 2},
              [
                {:type, _, :fun,
                 [
                   {:type, _, :product, [{:type, _, :term, []}, {:type, _, :integer, []}]},
                   {:remote_type, _, [{:atom, 0, Macro}, {:atom, 0, :t}, []]}
                 ]}
              ]},
             {{:some_fun_priv, 1},
              [
                {:type, _, :fun,
                 [{:type, _, :product, [{:type, _, :integer, []}]}, {:type, _, :integer, []}]}
              ]},
             {{:some_fun, 1},
              [
                {:type, _, :fun,
                 [{:type, _, :product, [{:type, _, :integer, []}]}, {:type, _, :integer, []}]}
              ]}
           ] = Typespec.get_specs(ElixirSenseExample.ModuleWithTypes)

    assert [] == Typespec.get_specs(ElixirSenseExample.NotExistingModule)
  end

  test "get_types" do
    assert [
             typep: {:priv_type, {:type, _, :integer, []}, []},
             opaque: {:opaque_type, {:user_type, _, :priv_type, []}, []},
             type: {:pub_type, {:type, _, :integer, []}, []}
           ] = Typespec.get_types(ElixirSenseExample.ModuleWithTypes)

    assert [] == Typespec.get_types(ElixirSenseExample.NotExistingModule)
  end

  test "get_callbacks" do
    assert [
             {{:"MACRO-some_macrocallback", 2},
              [
                {:type, _, :fun,
                 [
                   {:type, _, :product, [{:type, _, :term, []}, {:type, _, :integer, []}]},
                   {:type, _, :atom, []}
                 ]}
              ]},
             {{:some_callback, 1},
              [
                {:type, _, :fun,
                 [{:type, _, :product, [{:type, _, :integer, []}]}, {:type, _, :atom, []}]}
              ]}
           ] = Typespec.get_callbacks(ElixirSenseExample.ModuleWithTypes)

    assert [] == Typespec.get_callbacks(ElixirSenseExample.NotExistingModule)
  end

  describe "malformed beam files" do
    @tag :tmp_dir
    test "return [] instead of raising when the beam is corrupt", %{tmp_dir: tmp_dir} do
      module = :ElixirSenseCorruptBeamProbe
      {_, beam, _} = :code.get_object_code(ElixirSenseExample.ModuleWithTypes)

      # Flip a byte inside the atom table chunk: `:beam_lib` raises a MatchError
      # here instead of returning an error tuple, which used to escape
      # `Code.Typespec.fetch_*`.
      <<head::binary-size(25), byte, rest::binary>> = beam
      corrupt = <<head::binary, Bitwise.bxor(byte, 0xFF), rest::binary>>

      File.write!(Path.join(tmp_dir, "#{module}.beam"), corrupt)
      tmp_dir_charlist = String.to_charlist(tmp_dir)
      :code.add_patha(tmp_dir_charlist)
      on_exit(fn -> :code.del_path(tmp_dir_charlist) end)

      assert [] == Typespec.get_specs(module)
      assert [] == Typespec.get_types(module)
      assert [] == Typespec.get_callbacks(module)
    end
  end

  test "type_to_quoted" do
    type = {:t, {:remote_type, 249, [{:atom, 0, Enumerable}, {:atom, 0, :t}, []]}, []}

    assert {:"::", [], [{:t, [], []}, {{:., [line: 249], [Enumerable, :t]}, [line: 249], []}]} ==
             Typespec.type_to_quoted(type)
  end

  test "spec_to_quoted" do
    spec =
      {:type, 456, :fun,
       [
         {:type, 456, :product, [{:type, 456, :term, []}]},
         {:type, 456, :maybe_improper_list, []}
       ]}

    assert {:"::", [line: 456],
            [
              {:wrap, [line: 456], [{:term, [line: 456], []}]},
              {:maybe_improper_list, [line: 456], []}
            ]} == Typespec.spec_to_quoted(:wrap, spec)
  end
end
