defmodule ElixirSense.Core.TypeInfoTest do
  use ExUnit.Case
  alias ElixirSense.Core.TypeInfo
  alias ElixirSenseExample.ModuleWithTypespecs.{Local}

  @tag timeout: :infinity
  @tag requires_source: true
  test "does not crash on standard library" do
    for {application, _, _} <- Application.started_applications() do
      {:ok, modules} = :application.get_key(application, :modules)

      for mod <- modules, {fun, ar} <- mod.module_info(:functions), i <- 0..ar do
        TypeInfo.extract_param_options(mod, fun, i)
      end
    end
  end

  test "return :ets.new options" do
    assert [
             {:ets, :set},
             {:ets, :ordered_set},
             {:ets, :bag},
             {:ets, :duplicate_bag},
             {:ets, :public},
             {:ets, :protected},
             {:ets, :private},
             {:ets, :named_table},
             {:ets, :keypos, {:type, _, :pos_integer, []}},
             {:ets, :heir, {:atom, _, :none}},
             {:ets, :write_concurrency, {:type, _, :boolean, []}},
             {:ets, :read_concurrency, {:type, _, :boolean, []}},
             {:ets, :compressed}
           ] = TypeInfo.extract_param_options(:ets, :new, 1)
  end

  test "dont crash on :lists.reverse" do
    assert [] == TypeInfo.extract_param_options(:lists, :reverse, 1)
  end

  test "dont crash on fun_without_options" do
    assert [] == TypeInfo.extract_param_options(Local, :fun_without_options, 1)
  end

  test "dont crash on :auth.print" do
    assert [] == TypeInfo.extract_param_options(:auth, :print, 2)
  end

  test "dont crash on :code.set_path" do
    assert [] == TypeInfo.extract_param_options(:auth, :print, 1)
  end

  test "dont crash on :error_logger.logfile" do
    assert [] == TypeInfo.extract_param_options(:error_logger, :logfile, 0)
  end

  test "return :heart.set_option options" do
    assert [{:heart, :check_schedulers}] ==
             TypeInfo.extract_param_options(:heart, :set_options, 0)
  end

  test "dont crash on :beam_jump.remove_unused_labels" do
    assert [] == TypeInfo.extract_param_options(:beam_jump, :remove_unused_labels, 0)
  end

  test "dont crash on :beam_ssa_bsm.check_context_call" do
    assert [] == TypeInfo.extract_param_options(:beam_ssa_bsm, :check_context_call, 2)
  end

  test "dont crash on :beam_ssa_lint.vvars_assert_unique_1" do
    assert [] == TypeInfo.extract_param_options(:beam_ssa_lint, :vvars_assert_unique_1, 0)
  end

  test "dont crash on :cerl_sets.from_list" do
    assert [] == TypeInfo.extract_param_options(:cerl_sets, :from_list, 0)
  end

  test "dont crash on :cerl_sets.union" do
    assert [] == TypeInfo.extract_param_options(:cerl_sets, :union, 0)
  end

  test "return :beam_lib.chunks options" do
    assert [{:beam_lib, :allow_missing_chunks}] ==
             TypeInfo.extract_param_options(:beam_lib, :chunks, 2)
  end

  test "dont crash on :erl_eval.match_clause" do
    assert [] == TypeInfo.extract_param_options(:erl_eval, :match_clause, 0)
  end

  test "dont crash on :ets.test_ms" do
    assert [] == TypeInfo.extract_param_options(:ets, :test_ms, 0)
  end

  test "dont crash on :io_lib_pretty.print" do
    assert [
             {:io_lib_pretty, :chars_limit, {:user_type, _, :chars_limit, []}},
             {:io_lib_pretty, :column, {:user_type, _, :column, []}},
             {:io_lib_pretty, :depth, {:user_type, _, :depth, []}},
             {:io_lib_pretty, :encoding, {:user_type, _, :encoding, []}},
             {:io_lib_pretty, :line_length, {:user_type, _, :line_length, []}},
             {:io_lib_pretty, :line_max_chars, {:user_type, _, :line_max_chars, []}},
             {:io_lib_pretty, :record_print_fun, {:user_type, _, :rec_print_fun, []}},
             {:io_lib_pretty, :strings, {:type, _, :boolean, []}}
           ] = TypeInfo.extract_param_options(:io_lib_pretty, :print, 1)
  end

  test "dont crash on :lists.append" do
    assert [] == TypeInfo.extract_param_options(:lists, :append, 0)
  end

  test "dont do infinite recursion in :lists.flatten" do
    assert [] == TypeInfo.extract_param_options(:lists, :flatten, 0)
  end

  test "dont crash on :ordsets.union" do
    assert [] == TypeInfo.extract_param_options(:ordsets, :union, 0)
  end

  test "dont crash on :sets.union" do
    assert [] == TypeInfo.extract_param_options(:sets, :union, 0)
  end

  test "dont crash on Config.Reader.read!" do
    assert [] == TypeInfo.extract_param_options(Config.Reader, :read!, 1)
  end

  test "dont crash on :tls_record.lowest_protocol_version" do
    assert [] == TypeInfo.extract_param_options(:tls_record, :lowest_protocol_version, 0)
  end

end
