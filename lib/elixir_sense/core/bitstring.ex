defmodule ElixirSense.Core.Bitstring do
  @moduledoc false

  @types [
    :integer,
    :float,
    :bitstring,
    :binary,
    :utf8,
    :utf16,
    :utf32
  ]

  @type_aliases %{
    bits: :bitstring,
    bytes: :binary
  }

  @modifiers %{
    signed: [:integer],
    unsigned: [:integer],
    little: [:integer, :float, :utf16, :utf32],
    big: [:integer, :float, :utf16, :utf32],
    native: [:integer, :utf16, :utf32]
  }

  @sign_modifiers [:signed, :unsigned]
  @endianness_modifiers [:little, :big, :native]
  @default %{
    type: nil,
    sign_modifier: nil,
    endianness_modifier: nil,
    size: nil,
    unit: nil
  }

  def parse(binary, acc \\ @default)

  for type <- @types do
    def parse(<<unquote(Atom.to_string(type)), rest::binary>>, acc) do
      parse(rest, %{acc | type: unquote(type)})
    end
  end

  for {type_alias, type} <- @type_aliases do
    def parse(<<unquote(Atom.to_string(type_alias)), rest::binary>>, acc) do
      parse(rest, %{acc | type: unquote(type)})
    end
  end

  for sign_modifier <- @sign_modifiers do
    def parse(<<unquote(Atom.to_string(sign_modifier)), rest::binary>>, acc) do
      parse(rest, %{acc | sign_modifier: unquote(sign_modifier)})
    end
  end

  for endianness_modifier <- @endianness_modifiers do
    def parse(<<unquote(Atom.to_string(endianness_modifier)), rest::binary>>, acc) do
      parse(rest, %{acc | endianness_modifier: unquote(endianness_modifier)})
    end
  end

  def parse(<<"-", rest::binary>>, acc), do: parse(rest, acc)

  def parse(<<"size", rest::binary>>, acc) do
    parse(rest, %{acc | size: true})
  end

  def parse(<<"unit", rest::binary>>, acc) do
    parse(rest, %{acc | unit: true})
  end

  def parse(<<_::binary-size(1), rest::binary>>, acc), do: parse(rest, acc)

  def parse(<<>>, acc), do: acc

  def available_options(map) do
    available_types(map)
    |> Kernel.++(available_sign_modifiers(map))
    |> Kernel.++(available_endianness_modifiers(map))
    |> Kernel.++(available_size(map))
    |> Kernel.++(available_unit(map))
  end

  def available_types(%{type: nil, sign_modifier: nil, endianness_modifier: nil}), do: @types

  def available_types(%{type: nil, sign_modifier: nil, endianness_modifier: endianness_modifier}),
    do: @modifiers[endianness_modifier]

  def available_types(%{type: nil}), do: [:integer]
  def available_types(_), do: []

  def available_sign_modifiers(%{type: type, sign_modifier: nil}) when type in [nil, :integer],
    do: @sign_modifiers

  def available_sign_modifiers(_), do: []

  def available_endianness_modifiers(%{type: type, endianness_modifier: nil})
      when type in [nil, :integer, :utf16, :utf32],
      do: @endianness_modifiers

  def available_endianness_modifiers(%{type: :float, endianness_modifier: nil}),
    do: [:little, :big]

  def available_endianness_modifiers(_), do: []

  def available_size(%{size: nil}), do: [:size]
  def available_size(_), do: []

  def available_unit(%{unit: nil}), do: [:unit]
  def available_unit(_), do: []
end
