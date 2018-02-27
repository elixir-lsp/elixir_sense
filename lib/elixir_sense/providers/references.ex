defmodule ElixirSense.Providers.References do
  @moduledoc """
  This module provides References support by using
  the `Mix.Tasks.Xref.call/0` task to find all references to
  any function or module identified at the provided location.
  """

  alias ElixirSense.Core.{Metadata, Parser, Source}

  def find(text, line, character) do
    xref_at_cursor(text, line, character) |> Enum.map(&build_location/1)
  end

  defp xref_at_cursor(text, line, character) do
    subject_at_cursor(text, line, character)
    |> callee_at_cursor(text, line)
    |> case do
      {:ok, mfa} -> callers(mfa)
      _ -> []
    end
  end

  defp callee_at_cursor(nil, _, _), do: :error

  defp callee_at_cursor(subject_at_cursor, text, line) do
    case function_context_at_cursor(text, line) do
      [_module, ^subject_at_cursor, _arity] = mfa -> {:ok, mfa}
      _ -> Mix.Utils.parse_mfa(subject_at_cursor)
    end
  end

  def callers(mfa), do: Mix.Tasks.Xref.calls() |> Enum.filter(caller_filter(mfa))

  defp caller_filter([module, func, arity]), do: &match?(%{callee: {^module, ^func, ^arity}}, &1)

  defp caller_filter([module, func]), do: &match?(%{callee: {^module, ^func, _}}, &1)

  defp caller_filter([module]), do: &match?(%{callee: {^module, _, _}}, &1)

  defp subject_at_cursor(text, line, character) do
    case Source.subject(text, line, character) do
      nil -> nil
      subject -> String.to_atom(subject)
    end
  end

  defp function_context_at_cursor(text, line) do
    Parser.parse_string(text, true, true, line)
    |> Metadata.get_env(line)
    |> case do
      %{module: module, scope: {scope, arity}} -> [module, scope, arity]
      _ -> nil
    end
  end

  defp build_location(call) do
    %{
      uri: call.file,
      range: %{
        start: %{line: call.line, character: 0},
        end: %{line: call.line, character: 0}
      }
    }
  end

end
