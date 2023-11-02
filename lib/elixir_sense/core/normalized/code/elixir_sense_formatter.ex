# This file includes modified code extracted from the elixir project. Namely:
#
# https://raw.githubusercontent.com/elixir-lang/elixir/v1.13.4/lib/elixir/lib/code/formatter.ex
#
# The original code is licensed as follows:
#
# Copyright 2012 Plataformatec
# Copyright 2021 The Elixir Team
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# The only changes are module renames

defmodule ElixirSense.Core.Normalized.Code.ElixirSense.Formatter do
  @locals_without_parens [
    # Special forms
    alias: 1,
    alias: 2,
    case: 2,
    cond: 1,
    for: :*,
    import: 1,
    import: 2,
    quote: 1,
    quote: 2,
    receive: 1,
    require: 1,
    require: 2,
    try: 1,
    with: :*,

    # Kernel
    def: 1,
    def: 2,
    defp: 1,
    defp: 2,
    defguard: 1,
    defguardp: 1,
    defmacro: 1,
    defmacro: 2,
    defmacrop: 1,
    defmacrop: 2,
    defmodule: 2,
    defdelegate: 2,
    defexception: 1,
    defoverridable: 1,
    defstruct: 1,
    destructure: 2,
    raise: 1,
    raise: 2,
    reraise: 2,
    reraise: 3,
    if: 2,
    unless: 2,
    use: 1,
    use: 2,

    # Stdlib,
    defrecord: 2,
    defrecord: 3,
    defrecordp: 2,
    defrecordp: 3,

    # Testing
    assert: 1,
    assert: 2,
    assert_in_delta: 3,
    assert_in_delta: 4,
    assert_raise: 2,
    assert_raise: 3,
    assert_receive: 1,
    assert_receive: 2,
    assert_receive: 3,
    assert_received: 1,
    assert_received: 2,
    doctest: 1,
    doctest: 2,
    refute: 1,
    refute: 2,
    refute_in_delta: 3,
    refute_in_delta: 4,
    refute_receive: 1,
    refute_receive: 2,
    refute_receive: 3,
    refute_received: 1,
    refute_received: 2,
    setup: 1,
    setup: 2,
    setup_all: 1,
    setup_all: 2,
    test: 1,
    test: 2,

    # Mix config
    config: 2,
    config: 3,
    import_config: 1
  ]

  @doc """
  Lists all default locals without parens.
  """
  def locals_without_parens do
    @locals_without_parens
  end

  @doc """
  Checks if a function is a local without parens.
  """
  def local_without_parens?(fun, arity, locals_without_parens) do
    arity > 0 and
      Enum.any?(locals_without_parens, fn {key, val} ->
        key == fun and (val == :* or val == arity)
      end)
  end
end
