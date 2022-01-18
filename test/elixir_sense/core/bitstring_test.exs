defmodule ElixirSense.Core.BitstringTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.Bitstring

  test "parse type" do
    assert %{type: :integer} = Bitstring.parse("integer")
    assert %{type: :utf16} = Bitstring.parse("utf16")
  end

  test "parse type alias" do
    assert %{type: :bitstring} = Bitstring.parse("bits")
    assert %{type: :binary} = Bitstring.parse("bytes")
  end

  test "parse sign" do
    assert %{sign_modifier: :unsigned} = Bitstring.parse("unsigned")
    assert %{sign_modifier: :signed} = Bitstring.parse("signed")
  end

  test "parse endianness" do
    assert %{endianness_modifier: :little} = Bitstring.parse("little")
    assert %{endianness_modifier: :big} = Bitstring.parse("big")
  end

  test "parse size" do
    assert %{size: true} = Bitstring.parse("size(8)")
  end

  test "parse unit" do
    assert %{unit: true} = Bitstring.parse("unit(8)")
  end

  test "parse complex" do
    expected1 = %{
      endianness_modifier: :native,
      sign_modifier: nil,
      type: :integer,
      size: nil,
      unit: nil
    }

    assert expected1 == Bitstring.parse("integer-native")
    assert expected1 == Bitstring.parse("native-integer")

    expected2 = %{
      endianness_modifier: :big,
      sign_modifier: :unsigned,
      type: :integer,
      size: true,
      unit: nil
    }

    # assert expected2 == Bitstring.parse("unsigned-big-integer")
    assert expected2 == Bitstring.parse("unsigned-big-integer-size(8)")
    # assert expected2 == Bitstring.parse("unsigned-big-integer-8")
    # assert expected2 == Bitstring.parse("8-integer-big-unsigned")
  end

  test "available options" do
    assert [:size, :unit] ==
             Bitstring.available_options(Bitstring.parse("unsigned-integer-native"))

    assert [:unit] ==
             Bitstring.available_options(Bitstring.parse("unsigned-integer-native-size(2)"))

    assert [:size] ==
             Bitstring.available_options(Bitstring.parse("unsigned-integer-native-unit(1)"))

    assert [:signed, :unsigned, :size, :unit] ==
             Bitstring.available_options(Bitstring.parse("integer-native"))

    assert [:signed, :unsigned, :little, :big, :native, :size, :unit] ==
             Bitstring.available_options(Bitstring.parse("integer"))

    assert [:little, :big, :native, :size, :unit] ==
             Bitstring.available_options(Bitstring.parse("unsigned-integer"))

    assert [:little, :big, :size, :unit] == Bitstring.available_options(Bitstring.parse("float"))

    assert [:little, :big, :native] ==
             Bitstring.available_options(Bitstring.parse("utf16"))

    assert [:size, :unit] == Bitstring.available_options(Bitstring.parse("binary"))

    assert [:integer, :float, :utf16, :utf32, :signed, :unsigned, :size, :unit] ==
             Bitstring.available_options(Bitstring.parse("big"))

    assert [:integer, :utf16, :utf32, :signed, :unsigned, :size, :unit] ==
             Bitstring.available_options(Bitstring.parse("native"))

    assert [:integer, :little, :big, :native, :size, :unit] ==
             Bitstring.available_options(Bitstring.parse("signed"))

    assert [
             :integer,
             :float,
             :bitstring,
             :binary,
             :signed,
             :unsigned,
             :little,
             :big,
             :native,
             :unit
           ] ==
             Bitstring.available_options(Bitstring.parse("size(2)"))

    assert [
             :integer,
             :float,
             :bitstring,
             :binary,
             :signed,
             :unsigned,
             :little,
             :big,
             :native,
             :size
           ] ==
             Bitstring.available_options(Bitstring.parse("unit(2)"))

    assert [
             :integer,
             :float,
             :bitstring,
             :binary,
             :utf8,
             :utf16,
             :utf32,
             :signed,
             :unsigned,
             :little,
             :big,
             :native,
             :size,
             :unit
           ] ==
             Bitstring.available_options(Bitstring.parse(""))
  end
end
