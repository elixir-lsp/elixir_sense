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

  @type position :: %{line: pos_integer, column: pos_integer}

  @type range :: %{
    start: position,
    end: position
  }

  @type reference_info :: %{
    uri: String.t,
    range: range
  }

  def find(nil, _, _, _, _, _) do
    []
  end

  def find(subject, arity, imports, aliases, module, scope, vars) do
    var_info = vars |> Enum.find(fn %VarInfo{name: name} -> to_string(name) == subject end)
    case var_info do
      %VarInfo{positions: positions} ->
        positions
        |> Enum.map(fn pos -> build_var_location(subject, pos) end)
      _ ->
        subject
        |> Source.split_module_and_func(aliases)
        |> Introspection.actual_mod_fun(imports, aliases, module)
        |> xref_at_cursor(arity, module, scope)
        |> Enum.map(&build_location/1)
        |> Enum.sort_by(fn %{uri: a, range: %{start: %{line: b, column: c}}} -> {a, b, c} end)
    end
  end

  defp xref_at_cursor(actual_mod_fun, arity, module, scope) do
    actual_mod_fun
    |> callee_at_cursor(module, scope, arity)
    |> case do
      {:ok, mfa} -> callers(mfa)
      _ -> []
    end
  end

  # Cursor over a module
  defp callee_at_cursor({module, nil}, _module, _scope, _arity) do
    {:ok, [module]}
  end

  # Cursor over a function definition
  defp callee_at_cursor({module, func}, module, {func, arity}, _) do
    {:ok, [module, func, arity]}
  end

  # Cursor over a function call but we couldn't introspect the arity
  defp callee_at_cursor({module, func}, _module, _scope, nil) do
    {:ok, [module, func]}
  end

  # Cursor over a function call
  defp callee_at_cursor({module, func}, _module, _scope, arity) do
    {:ok, [module, func, arity]}
  end

  def callers(mfa) do
    calls =
      Xref.calls()
      |> Enum.filter(caller_filter(mfa))
      |> fix_caller_module()

    for call <- calls,
        new_call <- expand_xref_line_calls(call) do
      new_call
    end
  end

  defp expand_xref_line_calls(xref_call) do
    %{callee: {mod, func, arity}, file: file, line: line} = xref_call

    case File.read(file) do
      {:ok, code} ->
        metadata = Parser.parse_string(code, true, true, line)
        %State.Env{
          imports: imports,
          aliases: aliases,
          module: module,
        } = Metadata.get_env(metadata, line)

        calls =
          metadata
          |> Metadata.get_calls(line)
          |> fix_calls_positions(code)

        for call <- calls,
            found_mod_fun = find_actual_mod_fun(code, call.line, call.col, imports, aliases, module),
            found_mod_fun == {mod, func},
            arity == call.arity do
          Map.merge(xref_call, %{column: call.col, line: call.line})
        end
      _ ->
        [xref_call]
    end
  end

  defp find_actual_mod_fun(code, line, col, imports, aliases, module) do
    code
    |> Source.subject(line, col)
    |> Source.split_module_and_func(aliases)
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
        start: %{line: call.line, column: call.column},
        end: %{line: call.line, column: call.column + func_length}
      }
    }
  end

  defp build_var_location(subject, {line, column}) do
    %{
      uri: nil,
      range: %{
        start: %{line: line, column: column},
        end: %{line: line, column: column + String.length(subject)}
      }
    }
  end

  # For Elixir < v1.10.0
  defp fix_caller_module(calls) do
    calls
    |> Enum.map(fn c -> Map.delete(c, :caller_module) end)
    |> Enum.uniq()
  end

  defp fix_calls_positions(calls, code) do
    for call <- calls do
      case call do
        %{mod: nil} ->
          call
        %{line: line, col: col} ->
          text_after = Source.text_after(code, line, col+1)
          {_rest, line_offset, col_offset} = Source.find_next_word(text_after)
          col_offset = if line_offset == 0, do: col + 1, else: col_offset

          %{call | line: line + line_offset, col: col_offset}
      end
    end
  end
end
