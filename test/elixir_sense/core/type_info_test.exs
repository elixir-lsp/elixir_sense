defmodule ElixirSense.Core.TypeInfoTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.TypeInfo
  alias ElixirSenseExample.ModuleWithTypespecs.{Local, Remote}

  @tag timeout: :infinity
  @tag requires_source: true
  test "does not crash on standard library" do
    for {application, _, _} <- Application.started_applications() do
      {:ok, modules} = :application.get_key(application, :modules)

      for mod <- modules, {fun, ar} <- mod.module_info(:exports), i <- 0..ar do
        TypeInfo.extract_param_options(mod, fun, i)
      end
    end
  end

  test "func_with_options" do
    assert [
             {Local, :local_o, {:user_type, _, :local_t, []}},
             {Local, :local_with_params_o,
              {:user_type, _, :local_t, [{:type, _, :atom, []}, {:type, _, :integer, []}]}},
             {Local, :union_o, {:user_type, _, :union_t, []}},
             {Local, :inline_union_o, {:type, _, :union, [{:atom, _, :a}, {:atom, _, :b}]}},
             {Local, :list_o, {:user_type, _, :list_t, []}},
             {Local, :inline_list_o,
              {:type, _, :list, [{:type, _, :union, [{:atom, _, :trace}, {:atom, _, :log}]}]}},
             {Local, :basic_o, {:type, _, :pid, []}},
             {Local, :basic_with_params_o, {:type, _, :nonempty_list, [{:type, _, :atom, []}]}},
             {Local, :builtin_o,
              {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :keyword}, []]}},
             {Local, :builtin_with_params_o,
              {:remote_type, _,
               [{:atom, _, :elixir}, {:atom, _, :keyword}, [{:type, _, :term, []}]]}},
             {Local, :remote_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :remote_t}, []]}},
             {Local, :remote_with_params_o,
              {:remote_type, _,
               [
                 {:atom, _, Remote},
                 {:atom, _, :remote_t},
                 [{:type, _, :atom, []}, {:type, _, :integer, []}]
               ]}},
             {Local, :remote_aliased_o, {:user_type, _, :remote_aliased_t, []}},
             {Local, :remote_aliased_inline_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :remote_t}, []]}},
             {Local, :private_o, {:user_type, _, :private_t, []}},
             {Local, :opaque_o, {:user_type, _, :opaque_t, []}},
             {Local, :non_existent_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :non_existent}, []]}},
             {Local, :large_o, {:user_type, _, :large_t, []}}
           ] = TypeInfo.extract_param_options(Local, :func_with_options, 0)
  end

  test "func_with_union_of_options" do
    assert [
             {Local, :local_o, {:user_type, _, :local_t, []}},
             {Local, :local_with_params_o,
              {:user_type, _, :local_t, [{:type, _, :atom, []}, {:type, _, :integer, []}]}},
             {Local, :union_o, {:user_type, _, :union_t, []}},
             {Local, :inline_union_o, {:type, _, :union, [{:atom, _, :a}, {:atom, _, :b}]}},
             {Local, :list_o, {:user_type, _, :list_t, []}},
             {Local, :inline_list_o,
              {:type, _, :list, [{:type, _, :union, [{:atom, _, :trace}, {:atom, _, :log}]}]}},
             {Local, :basic_o, {:type, _, :pid, []}},
             {Local, :basic_with_params_o, {:type, _, :nonempty_list, [{:type, _, :atom, []}]}},
             {Local, :builtin_o,
              {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :keyword}, []]}},
             {Local, :builtin_with_params_o,
              {:remote_type, _,
               [{:atom, _, :elixir}, {:atom, _, :keyword}, [{:type, _, :term, []}]]}},
             {Local, :remote_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :remote_t}, []]}},
             {Local, :remote_with_params_o,
              {:remote_type, _,
               [
                 {:atom, _, Remote},
                 {:atom, _, :remote_t},
                 [{:type, _, :atom, []}, {:type, _, :integer, []}]
               ]}},
             {Local, :remote_aliased_o, {:user_type, _, :remote_aliased_t, []}},
             {Local, :remote_aliased_inline_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :remote_t}, []]}},
             {Local, :private_o, {:user_type, _, :private_t, []}},
             {Local, :opaque_o, {:user_type, _, :opaque_t, []}},
             {Local, :non_existent_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :non_existent}, []]}},
             {Local, :large_o, {:user_type, _, :large_t, []}},
             {Local, :option_1, {:type, _, :atom, []}},
             {Local, :option_2, {:type, _, :integer, []}}
           ] = TypeInfo.extract_param_options(Local, :func_with_union_of_options, 0)
  end

  test "func_with_union_of_options_as_type" do
    assert [
             {Local, :option_1, {:type, _, :boolean, []}},
             {Local, :option_2, {:type, _, :timeout, []}},
             {Remote, :remote_option_1, {:user_type, _, :remote_t, []}},
             {Remote, :remote_option_2, {:user_type, _, :remote_list_t, []}}
           ] = TypeInfo.extract_param_options(Local, :func_with_union_of_options_as_type, 0)
  end

  test "func_with_union_of_options_inline" do
    assert [
             {Local, :option_1, {:type, _, :atom, []}},
             {Local, :option_2, {:type, _, :integer, []}},
             {Local, :local_o, {:user_type, _, :local_t, []}},
             {Local, :local_with_params_o,
              {:user_type, _, :local_t, [{:type, _, :atom, []}, {:type, _, :integer, []}]}},
             {Local, :union_o, {:user_type, _, :union_t, []}},
             {Local, :inline_union_o, {:type, _, :union, [{:atom, _, :a}, {:atom, _, :b}]}},
             {Local, :list_o, {:user_type, _, :list_t, []}},
             {Local, :inline_list_o,
              {:type, _, :list, [{:type, _, :union, [{:atom, _, :trace}, {:atom, _, :log}]}]}},
             {Local, :basic_o, {:type, _, :pid, []}},
             {Local, :basic_with_params_o, {:type, _, :nonempty_list, [{:type, _, :atom, []}]}},
             {Local, :builtin_o,
              {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :keyword}, []]}},
             {Local, :builtin_with_params_o,
              {:remote_type, _,
               [{:atom, _, :elixir}, {:atom, _, :keyword}, [{:type, _, :term, []}]]}},
             {Local, :remote_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :remote_t}, []]}},
             {Local, :remote_with_params_o,
              {:remote_type, _,
               [
                 {:atom, _, Remote},
                 {:atom, _, :remote_t},
                 [{:type, _, :atom, []}, {:type, _, :integer, []}]
               ]}},
             {Local, :remote_aliased_o, {:user_type, _, :remote_aliased_t, []}},
             {Local, :remote_aliased_inline_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :remote_t}, []]}},
             {Local, :private_o, {:user_type, _, :private_t, []}},
             {Local, :opaque_o, {:user_type, _, :opaque_t, []}},
             {Local, :non_existent_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :non_existent}, []]}},
             {Local, :large_o, {:user_type, _, :large_t, []}}
           ] = TypeInfo.extract_param_options(Local, :func_with_union_of_options_inline, 0)
  end

  test "func_with_named_options" do
    assert [
             {Local, :local_o, {:user_type, _, :local_t, []}},
             {Local, :local_with_params_o,
              {:user_type, _, :local_t, [{:type, _, :atom, []}, {:type, _, :integer, []}]}},
             {Local, :union_o, {:user_type, _, :union_t, []}},
             {Local, :inline_union_o, {:type, _, :union, [{:atom, _, :a}, {:atom, _, :b}]}},
             {Local, :list_o, {:user_type, _, :list_t, []}},
             {Local, :inline_list_o,
              {:type, _, :list, [{:type, _, :union, [{:atom, _, :trace}, {:atom, _, :log}]}]}},
             {Local, :basic_o, {:type, _, :pid, []}},
             {Local, :basic_with_params_o, {:type, _, :nonempty_list, [{:type, _, :atom, []}]}},
             {Local, :builtin_o,
              {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :keyword}, []]}},
             {Local, :builtin_with_params_o,
              {:remote_type, _,
               [{:atom, _, :elixir}, {:atom, _, :keyword}, [{:type, _, :term, []}]]}},
             {Local, :remote_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :remote_t}, []]}},
             {Local, :remote_with_params_o,
              {:remote_type, _,
               [
                 {:atom, _, Remote},
                 {:atom, _, :remote_t},
                 [{:type, _, :atom, []}, {:type, _, :integer, []}]
               ]}},
             {Local, :remote_aliased_o, {:user_type, _, :remote_aliased_t, []}},
             {Local, :remote_aliased_inline_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :remote_t}, []]}},
             {Local, :private_o, {:user_type, _, :private_t, []}},
             {Local, :opaque_o, {:user_type, _, :opaque_t, []}},
             {Local, :non_existent_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :non_existent}, []]}},
             {Local, :large_o, {:user_type, _, :large_t, []}}
           ] = TypeInfo.extract_param_options(Local, :func_with_named_options, 0)
  end

  test "func_with_options_as_inline_list" do
    assert [
             {Local, :local_o, {:user_type, _, :local_t, []}},
             {Local, :builtin_o,
              {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :keyword}, []]}}
           ] = TypeInfo.extract_param_options(Local, :func_with_options_as_inline_list, 0)
  end

  test "func_with_option_var_defined_in_when" do
    assert [
             {Local, :local_o, {:user_type, _, :local_t, []}},
             {Local, :local_with_params_o,
              {:user_type, _, :local_t, [{:type, _, :atom, []}, {:type, _, :integer, []}]}},
             {Local, :union_o, {:user_type, _, :union_t, []}},
             {Local, :inline_union_o, {:type, _, :union, [{:atom, _, :a}, {:atom, _, :b}]}},
             {Local, :list_o, {:user_type, _, :list_t, []}},
             {Local, :inline_list_o,
              {:type, _, :list, [{:type, _, :union, [{:atom, _, :trace}, {:atom, _, :log}]}]}},
             {Local, :basic_o, {:type, _, :pid, []}},
             {Local, :basic_with_params_o, {:type, _, :nonempty_list, [{:type, _, :atom, []}]}},
             {Local, :builtin_o,
              {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :keyword}, []]}},
             {Local, :builtin_with_params_o,
              {:remote_type, _,
               [{:atom, _, :elixir}, {:atom, _, :keyword}, [{:type, _, :term, []}]]}},
             {Local, :remote_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :remote_t}, []]}},
             {Local, :remote_with_params_o,
              {:remote_type, _,
               [
                 {:atom, _, Remote},
                 {:atom, _, :remote_t},
                 [{:type, _, :atom, []}, {:type, _, :integer, []}]
               ]}},
             {Local, :remote_aliased_o, {:user_type, _, :remote_aliased_t, []}},
             {Local, :remote_aliased_inline_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :remote_t}, []]}},
             {Local, :private_o, {:user_type, _, :private_t, []}},
             {Local, :opaque_o, {:user_type, _, :opaque_t, []}},
             {Local, :non_existent_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :non_existent}, []]}},
             {Local, :large_o, {:user_type, _, :large_t, []}}
           ] = TypeInfo.extract_param_options(Local, :func_with_option_var_defined_in_when, 0)
  end

  test "func_with_options_var_defined_in_when" do
    assert [
             {Local, :local_o, {:user_type, _, :local_t, []}},
             {Local, :local_with_params_o,
              {:user_type, _, :local_t, [{:type, _, :atom, []}, {:type, _, :integer, []}]}},
             {Local, :union_o, {:user_type, _, :union_t, []}},
             {Local, :inline_union_o, {:type, _, :union, [{:atom, _, :a}, {:atom, _, :b}]}},
             {Local, :list_o, {:user_type, _, :list_t, []}},
             {Local, :inline_list_o,
              {:type, _, :list, [{:type, _, :union, [{:atom, _, :trace}, {:atom, _, :log}]}]}},
             {Local, :basic_o, {:type, _, :pid, []}},
             {Local, :basic_with_params_o, {:type, _, :nonempty_list, [{:type, _, :atom, []}]}},
             {Local, :builtin_o,
              {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :keyword}, []]}},
             {Local, :builtin_with_params_o,
              {:remote_type, _,
               [{:atom, _, :elixir}, {:atom, _, :keyword}, [{:type, _, :term, []}]]}},
             {Local, :remote_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :remote_t}, []]}},
             {Local, :remote_with_params_o,
              {:remote_type, _,
               [
                 {:atom, _, Remote},
                 {:atom, _, :remote_t},
                 [{:type, _, :atom, []}, {:type, _, :integer, []}]
               ]}},
             {Local, :remote_aliased_o, {:user_type, _, :remote_aliased_t, []}},
             {Local, :remote_aliased_inline_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :remote_t}, []]}},
             {Local, :private_o, {:user_type, _, :private_t, []}},
             {Local, :opaque_o, {:user_type, _, :opaque_t, []}},
             {Local, :non_existent_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :non_existent}, []]}},
             {Local, :large_o, {:user_type, _, :large_t, []}}
           ] = TypeInfo.extract_param_options(Local, :func_with_options_var_defined_in_when, 0)
  end

  test "func_with_one_option" do
    assert [
             {Local, :local_o, {:user_type, _, :local_t, []}},
             {Local, :local_with_params_o,
              {:user_type, _, :local_t, [{:type, _, :atom, []}, {:type, _, :integer, []}]}},
             {Local, :union_o, {:user_type, _, :union_t, []}},
             {Local, :inline_union_o, {:type, _, :union, [{:atom, _, :a}, {:atom, _, :b}]}},
             {Local, :list_o, {:user_type, _, :list_t, []}},
             {Local, :inline_list_o,
              {:type, _, :list, [{:type, _, :union, [{:atom, _, :trace}, {:atom, _, :log}]}]}},
             {Local, :basic_o, {:type, _, :pid, []}},
             {Local, :basic_with_params_o, {:type, _, :nonempty_list, [{:type, _, :atom, []}]}},
             {Local, :builtin_o,
              {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, :keyword}, []]}},
             {Local, :builtin_with_params_o,
              {:remote_type, _,
               [{:atom, _, :elixir}, {:atom, _, :keyword}, [{:type, _, :term, []}]]}},
             {Local, :remote_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :remote_t}, []]}},
             {Local, :remote_with_params_o,
              {:remote_type, _,
               [
                 {:atom, _, Remote},
                 {:atom, _, :remote_t},
                 [{:type, _, :atom, []}, {:type, _, :integer, []}]
               ]}},
             {Local, :remote_aliased_o, {:user_type, _, :remote_aliased_t, []}},
             {Local, :remote_aliased_inline_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :remote_t}, []]}},
             {Local, :private_o, {:user_type, _, :private_t, []}},
             {Local, :opaque_o, {:user_type, _, :opaque_t, []}},
             {Local, :non_existent_o,
              {:remote_type, _, [{:atom, _, Remote}, {:atom, _, :non_existent}, []]}},
             {Local, :large_o, {:user_type, _, :large_t, []}}
           ] = TypeInfo.extract_param_options(Local, :func_with_named_options, 0)
  end

  test "fun_without_options" do
    assert [] = TypeInfo.extract_param_options(Local, :fun_without_options, 0)
  end

  test "fun_with_atom_option" do
    assert [{Local, :option_name}] ==
             TypeInfo.extract_param_options(Local, :fun_with_atom_option, 0)
  end

  test "fun_with_atom_option_in_when" do
    assert [{Local, :option_name}] ==
             TypeInfo.extract_param_options(Local, :fun_with_atom_option_in_when, 0)
  end

  test "fun_with_recursive_remote_type_option" do
    assert [
             {Remote, :remote_option_1, {:user_type, _, :remote_t, []}},
             {Remote, :remote_option_2, {:user_type, _, :remote_list_t, []}}
           ] = TypeInfo.extract_param_options(Local, :fun_with_recursive_remote_type_option, 0)
  end

  test "fun_with_recursive_user_type_option" do
    assert [
             {Local, :option_1, {:type, _, :atom, []}},
             {Local, :option_2, {:type, _, :integer, []}}
           ] = TypeInfo.extract_param_options(Local, :fun_with_recursive_user_type_option, 0)
  end

  test "fun_with_tuple_option_in_when" do
    assert [{Local, :opt_name, {:atom, _, :opt_value}}] =
             TypeInfo.extract_param_options(Local, :fun_with_tuple_option_in_when, 0)
  end

  test "fun_with_tuple_option" do
    assert [{Local, :opt_name, {:atom, _, :opt_value}}] =
             TypeInfo.extract_param_options(Local, :fun_with_tuple_option, 0)
  end

  test "fun_with_atom_user_type_option_in_when" do
    assert [{Local, :atom_opt}] ==
             TypeInfo.extract_param_options(Local, :fun_with_atom_user_type_option_in_when, 0)
  end

  test "fun_with_atom_user_type_option" do
    assert [{Local, :atom_opt}] ==
             TypeInfo.extract_param_options(Local, :fun_with_atom_user_type_option, 0)
  end

  test "fun_with_list_of_lists" do
    assert [] == TypeInfo.extract_param_options(Local, :fun_with_list_of_lists, 0)
  end

  test "fun_with_recursive_type" do
    assert [] == TypeInfo.extract_param_options(Local, :fun_with_recursive_type, 0)
  end

  test "fun_with_multiple_specs" do
    assert [{Local, :opt_name, {:atom, _, :opt_value}}] =
             TypeInfo.extract_param_options(Local, :fun_with_multiple_specs, 0)
  end

  test "fun_with_multiple_specs_when" do
    assert [{Local, :opt_name, {:atom, _, :opt_value}}] =
             TypeInfo.extract_param_options(Local, :fun_with_multiple_specs_when, 0)
  end

  test "fun_with_local_opaque" do
    assert [] = TypeInfo.extract_param_options(Local, :fun_with_local_opaque, 0)
  end

  test "fun_with_remote_opaque" do
    assert [] = TypeInfo.extract_param_options(Local, :fun_with_remote_opaque, 0)
  end
end
