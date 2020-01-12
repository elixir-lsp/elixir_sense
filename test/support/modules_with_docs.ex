defmodule ElixirSenseExample.ModuleWithDocs do
  @moduledoc """
  An example module
  """

  @typedoc """
  An example type
  """
  @typedoc since: "1.1.0"
  @type some_type :: integer
  @typedoc false
  @type some_type_doc_false :: integer
  @type some_type_no_doc :: integer

  @doc """
  An example fun
  """
  @doc since: "1.1.0"
  def some_fun(a, b \\ nil), do: a + b
  @doc false
  def some_fun_doc_false(a, b \\ nil), do: a + b
  def some_fun_no_doc(a, b \\ nil), do: a + b

  @doc """
  An example macro
  """
  @doc since: "1.1.0"
  def some_macro(a, b \\ nil), do: a + b
  @doc false
  def some_macro_doc_false(a, b \\ nil), do: a + b
  def some_macro_no_doc(a, b \\ nil), do: a + b

  @doc """
  An example callback
  """
  @doc since: "1.1.0"
  @callback some_callback(integer) :: atom
  @doc false
  @callback some_callback_doc_false(integer) :: atom
  @callback some_callback_no_doc(integer) :: atom

  @doc """
  An example callback
  """
  @doc since: "1.1.0"
  @macrocallback some_macrocallback(integer) :: atom
  @doc false
  @macrocallback some_macrocallback_doc_false(integer) :: atom
  @macrocallback some_macrocallback_no_doc(integer) :: atom
end

defmodule ElixirSenseExample.ModuleWithDocFalse do
  @moduledoc false
end

defmodule ElixirSenseExample.ModuleWithNoDocs do
end
