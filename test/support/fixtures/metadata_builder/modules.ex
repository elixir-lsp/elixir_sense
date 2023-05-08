defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Aliased do
end

defmodule ElixirSenseExample.Fixtures.MetadataBuilder.AliasedSibling do
end

defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Aliased.Child do
end

defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Imported do
  def public_fun, do: :ok
  def _underscored_fun, do: :ok
  defp private_fun, do: :ok
  defdelegate delegated_func(a), to: ElixirSenseExample.Delegates
  defmacro public_macro(), do: :ok
  defmacro _underscored_macro(), do: :ok
  defmacrop private_macro(), do: :ok
  defguard public_guard(a) when is_integer(a)
  defguard _underscored_guard(a) when is_integer(a)
  defguardp private_guard(a) when is_integer(a)
  def sigil_i(string, []), do: String.to_integer(string)
  defp sigil_j(string, []), do: String.to_integer(string)

  defmacro sigil_x(term, [?r]) do
    quote do
      unquote(term) |> String.reverse()
    end
  end

  defmacrop sigil_y(term, [?r]) do
    quote do
      unquote(term) |> String.reverse()
    end
  end
end

defmodule ElixirSenseExample.Fixtures.MetadataBuilder.ImportedSibling do
  def public_fun_sibling, do: :ok
end

defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Imported.Child do
  def public_fun_child, do: :ok
end

defmodule ElixirSenseExample.Fixtures.MetadataBuilder.Imported.Transitive do
  import ElixirSenseExample.Fixtures.MetadataBuilder.Imported
end
