defmodule ElixirSense.Providers.Suggestion.Hint do
  @moduledoc false

  def combine(
        %{hint: %{type: :hint, value: old_hint}, suggestions: old_suggestions},
        new_suggestions
      ) do
    names = for %{name: name} <- new_suggestions, do: name

    names =
      case old_suggestions do
        [] -> names
        _ -> [old_hint | names]
      end

    [build(old_hint, names)] ++ old_suggestions ++ new_suggestions
  end

  def get(old_hint, new_suggestions) do
    names = for %{name: name} <- new_suggestions, do: name

    [build(old_hint, names) | new_suggestions]
  end

  defp build(old_hint, []), do: %{type: :hint, value: old_hint}

  defp build(_old_hint, names) do
    prefix = :binary.longest_common_prefix(names)
    new_hint = binary_part(names |> hd, 0, prefix)
    %{type: :hint, value: new_hint}
  end
end
