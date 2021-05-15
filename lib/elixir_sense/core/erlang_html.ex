defmodule ElixirSense.Core.ErlangHtml do
  @moduledoc false

  # those typedefs mimic erl_docgen types (as of OTP 24) to not introduce dependency 
  @type chunk_elements() :: [chunk_element()]
  @type chunk_element() ::
          {chunk_element_type(), chunk_element_attrs(), chunk_elements()}
          | :unicode.unicode_binary()
  @type chunk_element_attrs() :: [chunk_element_attr()]
  @type chunk_element_attr() ::
          {atom, :unicode.unicode_binary()}
  @type chunk_element_type() :: chunk_element_inline_type() | chunk_element_block_type()
  @type chunk_element_inline_type() :: :a | :code | :strong | :b | :em | :i
  @type chunk_element_block_type() ::
          :p
          | :div
          | :br
          | :pre
          | :ul
          | :ol
          | :li
          | :dl
          | :dt
          | :dd
          | :h1
          | :h2
          | :h3
          | :h4
          | :h5
          | :h6

  @doc """
  Transform application/erlang+html AST into markdown string.

  Document AST is defined in http://erlang.org/doc/apps/erl_docgen/doc_storage.html
  """

  @spec to_markdown(chunk_element()) :: String.t
  def to_markdown(ast), do: to_markdown(ast, [], :normal)

  def to_markdown(binary, _parents, sanitize_mode) when is_binary(binary) do
    sanitize(binary, sanitize_mode)
  end

  def to_markdown(list, parents, sanitize_mode) when is_list(list) do
    Enum.map_join(list, "", &to_markdown(&1, parents, sanitize_mode))
  end

  def to_markdown({:br, _attrs, _inner}, parents, _sanitize_mode) do
    "  \n" <> build_prefix(parents)
  end

  def to_markdown({:p, _attrs, inner}, parents, sanitize_mode) do
    prefix = build_prefix(parents)
    to_markdown(inner, parents, sanitize_mode) <> "\n" <> prefix <> "\n" <> prefix
  end

  def to_markdown({tag, _attrs, inner}, parents, sanitize_mode) when tag in [:em, :i] do
    "*" <> to_markdown(inner, parents, sanitize_mode) <> "*"
  end

  def to_markdown({tag, _attrs, inner}, parents, sanitize_mode) when tag in [:strong, :b] do
    "**" <> to_markdown(inner, parents, sanitize_mode) <> "**"
  end

  def to_markdown({:ul, _attrs, inner}, parents, sanitize_mode) do
    prefix = build_prefix(parents)

    items =
      inner
      |> Enum.map_join("", fn {:li, _attrs, li_inner} ->
        "- #{to_markdown(li_inner, [:li, :ul | parents], sanitize_mode)}\n" <> prefix
      end)

    items <> "\n" <> prefix
  end

  def to_markdown({:ol, _attrs, inner}, parents, sanitize_mode) do
    prefix = build_prefix(parents)

    items =
      inner
      |> Enum.with_index(1)
      |> Enum.map_join("", fn {{:li, _attrs, li_inner}, index} ->
        "#{index}. #{to_markdown(li_inner, [:li, :ol | parents], sanitize_mode)}\n" <> prefix
      end)

    items <> "\n" <> prefix
  end

  def to_markdown({:dl, _attrs, inner}, parents, sanitize_mode) do
    prefix = build_prefix(parents)
    to_markdown(inner, parents, sanitize_mode) <> "\n" <> prefix
  end

  def to_markdown({:dt, _attrs, inner}, parents, sanitize_mode) do
    "**" <> to_markdown(inner, parents, sanitize_mode) <> ":** "
  end

  def to_markdown({:dd, _attrs, inner}, parents, sanitize_mode) do
    prefix = build_prefix(parents)
    to_markdown(inner, parents, sanitize_mode) <> "  \n" <> prefix
  end

  def to_markdown({:pre, _attrs1, [{:code, _attrs2, inner}]}, parents, _sanitize_mode) do
    prefix = build_prefix(parents)
    "```\n" <> prefix <> to_markdown(inner, parents, :none) <> "\n" <> prefix <> "```\n" <> prefix
  end

  for i <- 1..6,
      tag = :"h#{i}",
      prefix = for(_ <- 1..i, into: "", do: "#") do
    def to_markdown({unquote(tag), _attrs, inner}, parents, sanitize_mode) do
      unquote(prefix) <> " " <> to_markdown(inner, parents, sanitize_mode) <> "\n\n"
    end
  end

  def to_markdown({:div, _attrs, inner}, parents, sanitize_mode) do
    to_markdown(inner, parents, sanitize_mode)
  end

  def to_markdown({:code, _attrs, inner}, parents, _sanitize_mode) do
    "`" <> to_markdown(inner, parents, :backtick) <> "`"
  end

  def to_markdown({:a, _attrs, []}, _parents, _sanitize_mode) do
    ""
  end

  def to_markdown({:a, _attrs, inner}, parents, sanitize_mode) do
    "[" <> to_markdown(inner, parents, sanitize_mode) <> "]"
  end

  defp build_prefix(list), do: build_prefix(list, "")
  defp build_prefix([], acc), do: acc
  defp build_prefix([:li | rest], acc), do: build_prefix(rest, " " <> acc)
  defp build_prefix([:ul | rest], acc), do: build_prefix(rest, " " <> acc)
  defp build_prefix([:ol | rest], acc), do: build_prefix(rest, "  " <> acc)
  defp build_prefix([_other | rest], acc), do: build_prefix(rest, acc)

  @special_chars [
    "\\",
    "`",
    "*",
    "_",
    "{",
    "}",
    "[",
    "]",
    "<",
    ">",
    "(",
    ")",
    "#",
    "+",
    "-",
    ".",
    "!",
    "|"
  ]

  defp sanitize(binary, :normal) do
    Enum.reduce(@special_chars, binary, fn pattern, acc ->
      String.replace(acc, pattern, "\\" <> pattern)
    end)
  end

  defp sanitize(binary, :backtick) do
    if String.contains?(binary, "`") do
      "`" <> binary <> "`"
    else
      binary
    end
  end

  defp sanitize(binary, :none), do: binary
end
