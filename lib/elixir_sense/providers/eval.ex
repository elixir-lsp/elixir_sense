defmodule ElixirSense.Providers.Eval do
  @moduledoc """
  Provider responsible for evaluating Elixr expressions.
  """

  alias ElixirSense.Core.Introspection

  @type binding :: {name :: String.t(), value :: String.t()}
  @type bindings :: [binding] | :no_match | {:error, message :: String.t()}

  @doc """
  Converts a string to its quoted form.
  """
  @spec quote(String.t()) :: String.t()
  def quote(code) do
    code
    |> Code.string_to_quoted()
    |> elem(1)
    |> inspect(limit: :infinity, printable_limit: :infinity)
  end

  @doc """
  Evaluate a pattern matching expression and returns its bindings, if any.
  """
  @spec match(String.t()) :: bindings
  def match(code) do
    try do
      {:=, _, [pattern | _]} = code |> Code.string_to_quoted!()
      vars = extract_vars(pattern)

      bindings =
        code
        |> Code.eval_string()
        |> elem(1)

      Enum.map(vars, fn var ->
        {var, Keyword.get(bindings, var)}
      end)
    rescue
      MatchError ->
        :no_match

      e ->
        %{__struct__: type, description: description, line: line} = e

        {:error,
         "# #{Introspection.module_to_string(type)} on line #{line}:\n#  ↳ #{description}"}
    end
  end

  @doc """
  Evaluate a pattern matching expression using `ElixirSense.Providers.Eval.match/1`
  and format the results.
  """
  @spec match_and_format(String.t()) :: String.t()
  def match_and_format(code) do
    case match(code) do
      :no_match ->
        "# No match"

      {:error, message} ->
        message

      bindings ->
        bindings_to_string(bindings)
    end
  end

  defp bindings_to_string(bindings) do
    header =
      if Enum.empty?(bindings) do
        "# No bindings"
      else
        "# Bindings"
      end

    body =
      Enum.map_join(bindings, "\n\n", fn {var, val} ->
        "#{var} = #{inspect(val, limit: :infinity, printable_limit: :infinity)}"
      end)

    header <> "\n\n" <> body
  end

  defp extract_vars(ast) do
    {_ast, acc} = Macro.postwalk(ast, [], &extract_var/2)
    acc |> Enum.reverse()
  end

  defp extract_var(ast = {var_name, [line: _], nil}, acc)
       when is_atom(var_name) and var_name != :_ do
    {ast, [var_name | acc]}
  end

  defp extract_var(ast, acc) do
    {ast, acc}
  end
end
