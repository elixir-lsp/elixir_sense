defmodule ElixirSense.Providers.References do
  @moduledoc """
  This module provides References support by using
  the `Mix.Tasks.Xref.call/0` task to find all references to
  any function or module identified at the provided location.
  """

  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.State.VarInfo
  alias ElixirSense.Core.Source
  alias Mix.Tasks.Xref
  alias ElixirSense.Core.State
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Parser

  @type position :: %{line: pos_integer, character: pos_integer}

  @type range :: %{
    start: position,
    end: position
  }

  @type reference_info :: %{
    uri: String.t,
    range: range
  }

  def find(nil, _, _, _, _) do
    []
  end

  def find(subject, imports, aliases, module, scope, vars) do
    var_info = vars |> Enum.find(fn %VarInfo{name: name} -> to_string(name) == subject end)
    case var_info do
      %VarInfo{positions: positions} ->
        positions
        |> Enum.map(fn pos -> build_var_location(subject, pos) end)
      _ ->
        subject
        |> Source.split_module_and_func
        |> Introspection.actual_mod_fun(imports, aliases, module)
        |> xref_at_cursor(module, scope)
        |> Enum.map(&build_location/1)
        |> Enum.sort_by(fn %{uri: a, range: %{start: %{line: b, character: c}}} -> {a, b, c} end)
        |> Enum.uniq()
    end
  end

  defp xref_at_cursor(actual_mod_fun, module, scope) do
    actual_mod_fun
    |> callee_at_cursor(module, scope)
    |> case do
      {:ok, mfa} -> callers(mfa, actual_mod_fun)
      _ -> []
    end
  end

  defp callee_at_cursor({module, func}, module, {func, arity}) do
    {:ok, [module, func, arity]}
  end

  defp callee_at_cursor({module, func}, _module, _scope) do
    {:ok, [module, func]}
  end

  def callers(mfa, actual_mod_fun) do
    for original_call <- Xref.calls(),
        caller_filter(mfa).(original_call),
        call <- expand_line_calls(original_call, actual_mod_fun) do
      call
    end
  end

  defp expand_line_calls(call, actual_mod_fun) do
    %{callee: {_, f, _}, file: file, line: line} = call
    func = to_string(f)

    case File.read(file) do
      {:ok, code} ->
        metadata = Parser.parse_string(code, true, true, line)
        %State.Env{
          imports: imports,
          aliases: aliases,
          module: module,
        } = Metadata.get_env(metadata, line)

        line_text = Source.get_line_text(code, line)
        cols = find_refererences_in_line(line_text, func)
        for col <- cols,
            found_mod_fun = find_actual_mod_fun(code, line, col, imports, aliases, module),
            found_mod_fun == actual_mod_fun do
          Map.put(call, :column, col)
        end
      _ ->
        [call]
    end
  end

  defp find_refererences_in_line(line_text, func) do
    {_, [_|cols]} = line_text |> String.split(func) |> Enum.reduce({1, []}, fn str, {count, list} ->
      count = count + String.length(str)
      {count + String.length(func), [count|list]}
    end)
    Enum.reverse(cols)
  end

  defp find_actual_mod_fun(code, line, col, imports, aliases, module) do
    code
    |> Source.subject(line, col)
    |> Source.split_module_and_func
    |> Introspection.actual_mod_fun(imports, aliases, module)
  end

  defp caller_filter([module, func, arity]), do: &match?(%{callee: {^module, ^func, ^arity}}, &1)
  defp caller_filter([module, func]), do: &match?(%{callee: {^module, ^func, _}}, &1)
  defp caller_filter([module]), do: &match?(%{callee: {^module, _, _}}, &1)

  defp build_location(call) do
    %{callee: {_, func, _}} = call
    func_length = func |> to_string() |> String.length()

    %{
      uri: call.file,
      range: %{
        start: %{line: call.line, character: call.column},
        end: %{line: call.line, character: call.column + func_length}
      }
    }
  end

  defp build_var_location(subject, {line, column}) do
    %{
      uri: nil,
      range: %{
        start: %{line: line, character: column},
        end: %{line: line, character: column + String.length(subject)}
      }
    }
  end

end
