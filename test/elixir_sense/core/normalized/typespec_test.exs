defmodule ElixirSense.Core.Normalized.TypespecTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.Normalized.Typespec

  test "beam_specs" do
    assert [
             spec:
               {{:"MACRO-some_macro", 2},
                [
                  {:type, 17, :fun,
                   [
                     {:type, 17, :product, [{:type, 17, :term, []}, {:type, 17, :integer, []}]},
                     {:remote_type, 17, [{:atom, 0, Macro}, {:atom, 0, :t}, []]}
                   ]}
                ]},
             spec:
               {{:some_fun_priv, 1},
                [
                  {:type, 8, :fun,
                   [{:type, 8, :product, [{:type, 8, :integer, []}]}, {:type, 8, :integer, []}]}
                ]},
             spec:
               {{:some_fun, 1},
                [
                  {:type, 11, :fun,
                   [{:type, 11, :product, [{:type, 11, :integer, []}]}, {:type, 11, :integer, []}]}
                ]}
           ] == Typespec.beam_specs(ElixirSenseExample.ModuleWithTypes)

    assert [] == Typespec.beam_specs(ElixirSenseExample.NotExistingModule)
  end

  test "get_types" do
    assert [
             typep: {:priv_type, {:type, 3, :integer, []}, []},
             opaque: {:opaque_type, {:user_type, 4, :priv_type, []}, []},
             type: {:pub_type, {:type, 2, :integer, []}, []}
           ] == Typespec.get_types(ElixirSenseExample.ModuleWithTypes)

    assert [] == Typespec.get_types(ElixirSenseExample.NotExistingModule)
  end

  test "get_callbacks" do
    assert [
             {{:"MACRO-some_macrocallback", 2},
              [
                {:type, 6, :fun,
                 [
                   {:type, 6, :product, [{:type, 6, :term, []}, {:type, 6, :integer, []}]},
                   {:type, 6, :atom, []}
                 ]}
              ]},
             {{:some_callback, 1},
              [
                {:type, 5, :fun,
                 [{:type, 5, :product, [{:type, 5, :integer, []}]}, {:type, 5, :atom, []}]}
              ]}
           ] == Typespec.get_callbacks(ElixirSenseExample.ModuleWithTypes)

    assert [] == Typespec.get_callbacks(ElixirSenseExample.NotExistingModule)
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
