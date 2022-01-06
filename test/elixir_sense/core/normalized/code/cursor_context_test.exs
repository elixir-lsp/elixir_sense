# This file includes modified code extracted from the elixir project. Namely:
#
# https://github.com/elixir-lang/elixir/blob/v1.12/lib/elixir/test/code.exs
#
# The original code is licensed as follows:
#
# Copyright 2012 Plataformatec
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

defmodule ElixirSense.Core.Normalized.Code.CursorContextTest do
  use ExUnit.Case, async: true
  alias ElixirSense.Core.Normalized.Code.CursorContext, as: Code

  describe "cursor_context/2" do
    test "expressions" do
      assert Code.cursor_context([]) == :expr
      assert Code.cursor_context(",") == :expr
      assert Code.cursor_context("[") == :expr
      assert Code.cursor_context("<<") == :expr
      assert Code.cursor_context("=>") == :expr
      assert Code.cursor_context("->") == :expr
      assert Code.cursor_context("foo(<<") == :expr
      assert Code.cursor_context("hello: ") == :expr
      assert Code.cursor_context("\n") == :expr
      assert Code.cursor_context('\n') == :expr
      assert Code.cursor_context("\n\n") == :expr
      assert Code.cursor_context('\n\n') == :expr
    end

    test "local_or_var" do
      assert Code.cursor_context("hello_wo") == {:local_or_var, 'hello_wo'}
      assert Code.cursor_context("hello_world?") == {:local_or_var, 'hello_world?'}
      assert Code.cursor_context("hello_world!") == {:local_or_var, 'hello_world!'}
      assert Code.cursor_context("hello/wor") == {:local_or_var, 'wor'}
      assert Code.cursor_context("hello..wor") == {:local_or_var, 'wor'}
      assert Code.cursor_context("hello::wor") == {:local_or_var, 'wor'}
      assert Code.cursor_context("[hello_wo") == {:local_or_var, 'hello_wo'}
      assert Code.cursor_context("'hello_wo") == {:local_or_var, 'hello_wo'}
      assert Code.cursor_context("hellò_wó") == {:local_or_var, 'hellò_wó'}
    end

    test "dot" do
      assert Code.cursor_context("hello.") == {:dot, {:var, 'hello'}, ''}
      assert Code.cursor_context(":hello.") == {:dot, {:unquoted_atom, 'hello'}, ''}
      assert Code.cursor_context("nested.map.") == {:dot, {:dot, {:var, 'nested'}, 'map'}, ''}

      assert Code.cursor_context("Hello.") == {:dot, {:alias, 'Hello'}, ''}
      assert Code.cursor_context("Hello.World.") == {:dot, {:alias, 'Hello.World'}, ''}
      assert Code.cursor_context("Hello.wor") == {:dot, {:alias, 'Hello'}, 'wor'}
      assert Code.cursor_context("hello.wor") == {:dot, {:var, 'hello'}, 'wor'}
      assert Code.cursor_context("Hello.++") == {:dot, {:alias, 'Hello'}, '++'}
      assert Code.cursor_context(":hello.wor") == {:dot, {:unquoted_atom, 'hello'}, 'wor'}
      assert Code.cursor_context(":hell@o.wor") == {:dot, {:unquoted_atom, 'hell@o'}, 'wor'}
      assert Code.cursor_context(":he@ll@o.wor") == {:dot, {:unquoted_atom, 'he@ll@o'}, 'wor'}
      assert Code.cursor_context(":hell@@o.wor") == {:dot, {:unquoted_atom, 'hell@@o'}, 'wor'}
      assert Code.cursor_context("@hello.wor") == {:dot, {:module_attribute, 'hello'}, 'wor'}

      assert Code.cursor_context("nested.map.wor") ==
               {:dot, {:dot, {:var, 'nested'}, 'map'}, 'wor'}
    end

    test "local_arity" do
      assert Code.cursor_context("hello/") == {:local_arity, 'hello'}
    end

    test "local_call" do
      assert Code.cursor_context("hello\s") == {:local_call, 'hello'}
      assert Code.cursor_context("hello\t") == {:local_call, 'hello'}
      assert Code.cursor_context("hello(") == {:local_call, 'hello'}
      assert Code.cursor_context("hello(\s") == {:local_call, 'hello'}
      assert Code.cursor_context("hello(\t") == {:local_call, 'hello'}
    end

    test "dot_arity" do
      assert Code.cursor_context("Foo.hello/") == {:dot_arity, {:alias, 'Foo'}, 'hello'}
      assert Code.cursor_context("foo.hello/") == {:dot_arity, {:var, 'foo'}, 'hello'}
      assert Code.cursor_context("Foo.+/") == {:dot_arity, {:alias, 'Foo'}, '+'}
      assert Code.cursor_context(":foo.hello/") == {:dot_arity, {:unquoted_atom, 'foo'}, 'hello'}
      assert Code.cursor_context("@f.hello/") == {:dot_arity, {:module_attribute, 'f'}, 'hello'}
    end

    test "dot_call" do
      assert Code.cursor_context("Foo.hello\s") == {:dot_call, {:alias, 'Foo'}, 'hello'}
      assert Code.cursor_context("Foo.hello\t") == {:dot_call, {:alias, 'Foo'}, 'hello'}
      assert Code.cursor_context("Foo.hello(") == {:dot_call, {:alias, 'Foo'}, 'hello'}
      assert Code.cursor_context("Foo.hello(\s") == {:dot_call, {:alias, 'Foo'}, 'hello'}
      assert Code.cursor_context("Foo.hello(\t") == {:dot_call, {:alias, 'Foo'}, 'hello'}

      assert Code.cursor_context(":foo.hello\s") == {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}
      assert Code.cursor_context(":foo.hello\t") == {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}
      assert Code.cursor_context(":foo.hello(") == {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}
      assert Code.cursor_context(":foo.hello(\s") == {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}
      assert Code.cursor_context(":foo.hello(\t") == {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}
      assert Code.cursor_context(":foo.hello\s") == {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}

      assert Code.cursor_context("foo.hello\s") == {:dot_call, {:var, 'foo'}, 'hello'}
      assert Code.cursor_context("foo.hello\t") == {:dot_call, {:var, 'foo'}, 'hello'}
      assert Code.cursor_context("foo.hello(") == {:dot_call, {:var, 'foo'}, 'hello'}
      assert Code.cursor_context("foo.hello(\s") == {:dot_call, {:var, 'foo'}, 'hello'}
      assert Code.cursor_context("foo.hello(\t") == {:dot_call, {:var, 'foo'}, 'hello'}

      assert Code.cursor_context("@f.hello\s") == {:dot_call, {:module_attribute, 'f'}, 'hello'}
      assert Code.cursor_context("@f.hello\t") == {:dot_call, {:module_attribute, 'f'}, 'hello'}
      assert Code.cursor_context("@f.hello(") == {:dot_call, {:module_attribute, 'f'}, 'hello'}
      assert Code.cursor_context("@f.hello(\s") == {:dot_call, {:module_attribute, 'f'}, 'hello'}
      assert Code.cursor_context("@f.hello(\t") == {:dot_call, {:module_attribute, 'f'}, 'hello'}

      assert Code.cursor_context("Foo.+\s") == {:dot_call, {:alias, 'Foo'}, '+'}
      assert Code.cursor_context("Foo.+\t") == {:dot_call, {:alias, 'Foo'}, '+'}
      assert Code.cursor_context("Foo.+(") == {:dot_call, {:alias, 'Foo'}, '+'}
      assert Code.cursor_context("Foo.+(\s") == {:dot_call, {:alias, 'Foo'}, '+'}
      assert Code.cursor_context("Foo.+(\t") == {:dot_call, {:alias, 'Foo'}, '+'}
    end

    test "alias" do
      assert Code.cursor_context("HelloWor") == {:alias, 'HelloWor'}
      assert Code.cursor_context("Hello.Wor") == {:alias, 'Hello.Wor'}
      assert Code.cursor_context("Hello::Wor") == {:alias, 'Wor'}
      assert Code.cursor_context("Hello..Wor") == {:alias, 'Wor'}
      assert Code.cursor_context("%Hello.Wor") == {:alias, 'Hello.Wor'}
    end

    test "unquoted atom" do
      assert Code.cursor_context(":") == {:unquoted_atom, ''}
      assert Code.cursor_context(":HelloWor") == {:unquoted_atom, 'HelloWor'}
      assert Code.cursor_context(":HelloWór") == {:unquoted_atom, 'HelloWór'}
      assert Code.cursor_context(":hello_wor") == {:unquoted_atom, 'hello_wor'}
      assert Code.cursor_context(":Óla_mundo") == {:unquoted_atom, 'Óla_mundo'}
      assert Code.cursor_context(":Ol@_mundo") == {:unquoted_atom, 'Ol@_mundo'}
      assert Code.cursor_context(":Ol@") == {:unquoted_atom, 'Ol@'}
      assert Code.cursor_context("foo:hello_wor") == {:unquoted_atom, 'hello_wor'}

      # Operators from atoms
      assert Code.cursor_context(":+") == {:unquoted_atom, '+'}
      assert Code.cursor_context(":or") == {:unquoted_atom, 'or'}
      assert Code.cursor_context(":<") == {:unquoted_atom, '<'}
      assert Code.cursor_context(":.") == {:unquoted_atom, '.'}
      assert Code.cursor_context(":..") == {:unquoted_atom, '..'}
      assert Code.cursor_context(":->") == {:unquoted_atom, '->'}
    end

    test "operators" do
      assert Code.cursor_context("+") == {:operator, '+'}
      assert Code.cursor_context("++") == {:operator, '++'}
      assert Code.cursor_context("!") == {:operator, '!'}
      assert Code.cursor_context("<") == {:operator, '<'}
      assert Code.cursor_context("<<<") == {:operator, '<<<'}
      assert Code.cursor_context("..") == {:operator, '..'}
      assert Code.cursor_context("<~") == {:operator, '<~'}
      assert Code.cursor_context("=~") == {:operator, '=~'}
      assert Code.cursor_context("<~>") == {:operator, '<~>'}

      assert Code.cursor_context("+ ") == {:operator_call, '+'}
      assert Code.cursor_context("++ ") == {:operator_call, '++'}
      assert Code.cursor_context("! ") == {:operator_call, '!'}
      assert Code.cursor_context("< ") == {:operator_call, '<'}
      assert Code.cursor_context("<<< ") == {:operator_call, '<<<'}
      assert Code.cursor_context(".. ") == {:operator_call, '..'}
      assert Code.cursor_context("<~ ") == {:operator_call, '<~'}
      assert Code.cursor_context("=~ ") == {:operator_call, '=~'}
      assert Code.cursor_context("<~> ") == {:operator_call, '<~>'}

      assert Code.cursor_context("+/") == {:operator_arity, '+'}
      assert Code.cursor_context("++/") == {:operator_arity, '++'}
      assert Code.cursor_context("!/") == {:operator_arity, '!'}
      assert Code.cursor_context("</") == {:operator_arity, '<'}
      assert Code.cursor_context("<<</") == {:operator_arity, '<<<'}
      assert Code.cursor_context("../") == {:operator_arity, '..'}
      assert Code.cursor_context("<~/") == {:operator_arity, '<~'}
      assert Code.cursor_context("=~/") == {:operator_arity, '=~'}
      assert Code.cursor_context("<~>/") == {:operator_arity, '<~>'}

      # Unknown operators altogether
      assert Code.cursor_context("***") == :none

      # Textual operators are shown as local_or_var UNLESS there is space
      assert Code.cursor_context("when") == {:local_or_var, 'when'}
      assert Code.cursor_context("when ") == {:operator_call, 'when'}
      assert Code.cursor_context("when.") == :none

      assert Code.cursor_context("not") == {:local_or_var, 'not'}
      assert Code.cursor_context("not ") == {:operator_call, 'not'}
      assert Code.cursor_context("not.") == :none
    end

    test "incomplete operators" do
      assert Code.cursor_context("~") == {:operator, '~'}
      assert Code.cursor_context("~~") == {:operator, '~~'}
      assert Code.cursor_context("~ ") == :none
      assert Code.cursor_context("~~ ") == :none
      assert Code.cursor_context("^^") == {:operator, '^^'}
      assert Code.cursor_context("^^ ") == :none

      assert Code.cursor_context("Foo.~") == {:dot, {:alias, 'Foo'}, '~'}
      assert Code.cursor_context("Foo.~~") == {:dot, {:alias, 'Foo'}, '~~'}
      assert Code.cursor_context("Foo.~ ") == :none
      assert Code.cursor_context("Foo.~~ ") == :none
      assert Code.cursor_context("Foo.^^") == {:dot, {:alias, 'Foo'}, '^^'}
      assert Code.cursor_context("Foo.^^ ") == :none
    end

    test "module attribute" do
      assert Code.cursor_context("@") == {:module_attribute, ''}
      assert Code.cursor_context("@hello_wo") == {:module_attribute, 'hello_wo'}
    end

    test "none" do
      # Punctuation
      assert Code.cursor_context(")") == :none
      assert Code.cursor_context("}") == :none
      assert Code.cursor_context(">>") == :none
      assert Code.cursor_context("'") == :none
      assert Code.cursor_context("\"") == :none

      # Numbers
      assert Code.cursor_context("123") == :none
      assert Code.cursor_context("123?") == :none
      assert Code.cursor_context("123!") == :none
      assert Code.cursor_context("123var?") == :none
      assert Code.cursor_context("0x") == :none

      # Codepoints
      assert Code.cursor_context("?") == :none
      assert Code.cursor_context("?a") == :none
      assert Code.cursor_context("?foo") == :none

      # Dots
      assert Code.cursor_context(".") == :none
      assert Code.cursor_context("Mundo.Óla") == :none
      assert Code.cursor_context(":hello.World") == :none

      # Aliases
      assert Code.cursor_context("Hello::Wór") == :none
      assert Code.cursor_context("ÓlaMundo") == :none
      assert Code.cursor_context("HelloWór") == :none
      assert Code.cursor_context("@Hello") == :none
      assert Code.cursor_context("Hello(") == :none
      assert Code.cursor_context("Hello ") == :none
      assert Code.cursor_context("hello.World") == :none

      # Identifier
      assert Code.cursor_context("foo@bar") == :none
      assert Code.cursor_context("@foo@bar") == :none
    end

    test "newlines" do
      assert Code.cursor_context("this+does-not*matter\nHello.") == {:dot, {:alias, 'Hello'}, ''}
      assert Code.cursor_context('this+does-not*matter\nHello.') == {:dot, {:alias, 'Hello'}, ''}
    end
  end
end
