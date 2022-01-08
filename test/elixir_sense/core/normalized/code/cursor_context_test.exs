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

  if Version.match?(System.version(), "< 1.12.0") do
    describe "cursor_context/2" do
      import ElixirSense.Core.Normalized.Code.CursorContext.Fallback, warn: false

      test "expressions" do
        assert cursor_context([]) == :expr
        assert cursor_context("++") == :expr
        assert cursor_context(",") == :expr
        assert cursor_context("[") == :expr
        assert cursor_context("hello: ") == :expr
        assert cursor_context("\n") == :expr
        assert cursor_context('\n') == :expr
        assert cursor_context("\n\n") == :expr
        assert cursor_context('\n\n') == :expr
      end

      test "local_or_var" do
        assert cursor_context("hello_wo") == {:local_or_var, 'hello_wo'}
        assert cursor_context("hello_world?") == {:local_or_var, 'hello_world?'}
        assert cursor_context("hello_world!") == {:local_or_var, 'hello_world!'}
        assert cursor_context("hello/wor") == {:local_or_var, 'wor'}
        assert cursor_context("hello..wor") == {:local_or_var, 'wor'}
        assert cursor_context("hello::wor") == {:local_or_var, 'wor'}
        assert cursor_context("[hello_wo") == {:local_or_var, 'hello_wo'}

        assert cursor_context("hellò_wó") == {:local_or_var, 'hellò_wó'}
      end

      test "dot" do
        assert cursor_context("hello.") == {:dot, {:var, 'hello'}, ''}
        assert cursor_context(":hello.") == {:dot, {:unquoted_atom, 'hello'}, ''}
        assert cursor_context("nested.map.") == {:dot, {:dot, {:var, 'nested'}, 'map'}, ''}

        assert cursor_context("Hello.") == {:dot, {:alias, 'Hello'}, ''}
        assert cursor_context("Hello.World.") == {:dot, {:alias, 'Hello.World'}, ''}
        assert cursor_context("Hello.wor") == {:dot, {:alias, 'Hello'}, 'wor'}
        assert cursor_context("hello.wor") == {:dot, {:var, 'hello'}, 'wor'}
        assert cursor_context(":hello.wor") == {:dot, {:unquoted_atom, 'hello'}, 'wor'}
        assert cursor_context(":hell@o.wor") == {:dot, {:unquoted_atom, 'hell@o'}, 'wor'}
        assert cursor_context(":he@ll@o.wor") == {:dot, {:unquoted_atom, 'he@ll@o'}, 'wor'}
        assert cursor_context(":hell@@o.wor") == {:dot, {:unquoted_atom, 'hell@@o'}, 'wor'}
        assert cursor_context("@hello.wor") == {:dot, {:module_attribute, 'hello'}, 'wor'}

        assert cursor_context("nested.map.wor") ==
                 {:dot, {:dot, {:var, 'nested'}, 'map'}, 'wor'}
      end

      test "local_arity" do
        assert cursor_context("hello/") == {:local_arity, 'hello'}
      end

      test "local_call" do
        assert cursor_context("hello\s") == {:local_call, 'hello'}
        assert cursor_context("hello\t") == {:local_call, 'hello'}
        assert cursor_context("hello(") == {:local_call, 'hello'}
        assert cursor_context("hello(\s") == {:local_call, 'hello'}
        assert cursor_context("hello(\t") == {:local_call, 'hello'}
      end

      test "dot_arity" do
        assert cursor_context("Foo.hello/") == {:dot_arity, {:alias, 'Foo'}, 'hello'}

        assert cursor_context(":foo.hello/") ==
                 {:dot_arity, {:unquoted_atom, 'foo'}, 'hello'}

        assert cursor_context("foo.hello/") == {:dot_arity, {:var, 'foo'}, 'hello'}
        assert cursor_context("@f.hello/") == {:dot_arity, {:module_attribute, 'f'}, 'hello'}
      end

      test "dot_call" do
        assert cursor_context("Foo.hello\s") == {:dot_call, {:alias, 'Foo'}, 'hello'}
        assert cursor_context("Foo.hello\t") == {:dot_call, {:alias, 'Foo'}, 'hello'}
        assert cursor_context("Foo.hello(") == {:dot_call, {:alias, 'Foo'}, 'hello'}
        assert cursor_context("Foo.hello(\s") == {:dot_call, {:alias, 'Foo'}, 'hello'}
        assert cursor_context("Foo.hello(\t") == {:dot_call, {:alias, 'Foo'}, 'hello'}

        assert cursor_context(":foo.hello\s") ==
                 {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}

        assert cursor_context(":foo.hello\t") ==
                 {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}

        assert cursor_context(":foo.hello(") == {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}

        assert cursor_context(":foo.hello(\s") ==
                 {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}

        assert cursor_context(":foo.hello(\t") ==
                 {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}

        assert cursor_context("foo.hello\s") == {:dot_call, {:var, 'foo'}, 'hello'}
        assert cursor_context("foo.hello\t") == {:dot_call, {:var, 'foo'}, 'hello'}
        assert cursor_context("foo.hello(") == {:dot_call, {:var, 'foo'}, 'hello'}
        assert cursor_context("foo.hello(\s") == {:dot_call, {:var, 'foo'}, 'hello'}
        assert cursor_context("foo.hello(\t") == {:dot_call, {:var, 'foo'}, 'hello'}

        assert cursor_context("@f.hello\s") == {:dot_call, {:module_attribute, 'f'}, 'hello'}
        assert cursor_context("@f.hello\t") == {:dot_call, {:module_attribute, 'f'}, 'hello'}
        assert cursor_context("@f.hello(") == {:dot_call, {:module_attribute, 'f'}, 'hello'}

        assert cursor_context("@f.hello(\s") ==
                 {:dot_call, {:module_attribute, 'f'}, 'hello'}

        assert cursor_context("@f.hello(\t") ==
                 {:dot_call, {:module_attribute, 'f'}, 'hello'}
      end

      test "alias" do
        assert cursor_context("HelloWor") == {:alias, 'HelloWor'}
        assert cursor_context("Hello.Wor") == {:alias, 'Hello.Wor'}
        assert cursor_context("Hello::Wor") == {:alias, 'Wor'}
        assert cursor_context("Hello..Wor") == {:alias, 'Wor'}
        assert cursor_context("%Hello.Wor") == {:alias, 'Hello.Wor'}
      end

      test "unquoted atom" do
        assert cursor_context(":") == {:unquoted_atom, ''}
        assert cursor_context(":HelloWor") == {:unquoted_atom, 'HelloWor'}
        assert cursor_context(":HelloWór") == {:unquoted_atom, 'HelloWór'}
        assert cursor_context(":hello_wor") == {:unquoted_atom, 'hello_wor'}
        assert cursor_context(":Óla_mundo") == {:unquoted_atom, 'Óla_mundo'}
        assert cursor_context(":Ol@_mundo") == {:unquoted_atom, 'Ol@_mundo'}
        assert cursor_context(":Ol@") == {:unquoted_atom, 'Ol@'}
        assert cursor_context("foo:hello_wor") == {:unquoted_atom, 'hello_wor'}
      end

      test "module attribute" do
        assert cursor_context("@") == {:module_attribute, ''}
        assert cursor_context("@hello_wo") == {:module_attribute, 'hello_wo'}
      end

      test "none" do
        # Containers
        assert cursor_context(")") == :none
        assert cursor_context("}") == :none

        # Numbers
        assert cursor_context("123") == :none
        assert cursor_context("123?") == :none
        assert cursor_context("123!") == :none
        assert cursor_context("123var?") == :none
        assert cursor_context("0x") == :none

        # Dots
        assert cursor_context("Mundo.Óla") == :none
        assert cursor_context(":hello.World") == :none

        # Aliases
        assert cursor_context("Hello::Wór") == :none
        assert cursor_context("ÓlaMundo") == :none
        assert cursor_context("HelloWór") == :none
        assert cursor_context("@Hello") == :none
        assert cursor_context("Hello(") == :none
        assert cursor_context("Hello ") == :none
        assert cursor_context("hello.World") == :none

        # Identifier
        assert cursor_context("foo@bar") == :none
        assert cursor_context("@foo@bar") == :none
      end

      test "newlines" do
        assert cursor_context("this+does-not*matter\nHello.") ==
                 {:dot, {:alias, 'Hello'}, ''}

        assert cursor_context('this+does-not*matter\nHello.') ==
                 {:dot, {:alias, 'Hello'}, ''}
      end
    end
  end
end
