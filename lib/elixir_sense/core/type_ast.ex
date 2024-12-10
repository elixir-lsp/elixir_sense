defmodule ElixirSense.Core.TypeAst do
  @moduledoc false

  alias ElixirSense.Core.Normalized.Typespec

  def from_typedef({_kind, type}) do
    Typespec.type_to_quoted(type)
  end

  def extract_signature(nil) do
    nil
  end

  def extract_signature(ast) do
    ast
    |> extract_spec_ast_parts
    |> Map.get(:name)
    |> Macro.to_string()
  end

  defp extract_spec_ast_parts({:when, _, [{:"::", _, [name_part, return_part]}, when_part]}) do
    %{name: name_part, returns: extract_return_part(return_part, []), when_part: when_part}
  end

  defp extract_spec_ast_parts({:"::", _, [name_part, return_part]}) do
    %{name: name_part, returns: extract_return_part(return_part, [])}
  end

  defp extract_return_part({:|, _, [lhs, rhs]}, returns) do
    [lhs | extract_return_part(rhs, returns)]
  end

  defp extract_return_part(ast, returns) do
    [ast | returns]
  end
end
