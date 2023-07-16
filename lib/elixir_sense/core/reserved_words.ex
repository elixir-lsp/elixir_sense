defmodule ElixirSense.Core.ReservedWords do
  @moduledoc """
  Provides docs on reserved words
  """

  @atoms ~w(true false nil)a
  @operators ~w(when and or not in)a
  @anonymous_function_definitions ~w(fn)a
  @do_end_blocks ~w(do end catch rescue after else)a

  @all @atoms ++ @operators ++ @anonymous_function_definitions ++ @do_end_blocks

  def all, do: @all

  def docs(keyword) when keyword in [true, false] do
    "Boolean value atom"
  end

  def docs(nil) do
    "nil value atom"
  end

  def docs(:fn) do
    "Anonymous function definition"
  end

  def docs(keyword) when keyword in [:and, :or, :not] do
    "Strict boolean operator"
  end

  def docs(:when) do
    "Guard expression operator"
  end

  def docs(:in) do
    "Membership check operator"
  end

  def docs(keyword) when keyword in @do_end_blocks do
    "do-end block control keyword"
  end
end
