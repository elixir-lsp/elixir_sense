defmodule ElixirSense do
  @moduledoc """
  ElxirSense is a Elixir library that implements useful features for any editor/tool that needs
  to introspect context-aware information about Elixir source code.

  This module provides the basic functionality for context-aware code completion, docs, signature info and more.
  """

  alias ElixirSense.Core.Applications
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.ModuleStore
  alias ElixirSense.Core.Parser
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State
  alias ElixirSense.Core.State.VarInfo
  alias ElixirSense.Location
  alias ElixirSense.Providers.Definition
  alias ElixirSense.Providers.Docs
  alias ElixirSense.Providers.Eval
  alias ElixirSense.Providers.Expand
  alias ElixirSense.Providers.Implementation
  alias ElixirSense.Providers.References
  alias ElixirSense.Providers.Signature
  alias ElixirSense.Providers.Suggestion

  @type callee_t :: {module, atom, non_neg_integer}

  @type call_t :: %{
          callee: callee_t,
          file: nil | String.t(),
          line: nil | pos_integer,
          column: nil | pos_integer
        }
  @type call_trace_t :: %{optional(callee_t) => [call_t]}

  @doc ~S"""
  Returns all documentation related a module or function, including types and callback information.

  ## Example

      iex> code = ~S'''
      ...> defmodule MyModule do
      ...>   alias Enum, as: MyEnum
      ...>   MyEnum.to_list(1..3)
      ...> end
      ...> '''
      iex> %{docs: %{types: types, docs: docs}} = ElixirSense.docs(code, 3, 11)
      iex> docs |> String.split("\n") |> Enum.at(8)
      "Converts `enumerable` to a list."
      iex> types |> String.split("\n") |> Enum.at(4)
      "@type default :: any"
  """
  @spec docs(String.t(), pos_integer, pos_integer) ::
          %{
            actual_subject: String.t(),
            docs: Introspection.docs(),
            range: %{
              begin: {pos_integer, pos_integer},
              end: {pos_integer, pos_integer}
            }
          }
          | nil
  def docs(code, line, column) do
    case Code.Fragment.surround_context(code, {line, column}) do
      :none ->
        nil

      %{begin: begin_pos, end: end_pos, context: context} ->
        metadata = Parser.parse_string(code, true, true, line)

        env = Metadata.get_env(metadata, line)

        case Docs.all(context, env, metadata.mods_funs_to_positions, metadata.types) do
          {actual_subject, docs} ->
            %{
              actual_subject: actual_subject,
              docs: docs,
              range: %{
                begin: begin_pos,
                end: end_pos
              }
            }

          nil ->
            nil
        end
    end
  end

  @doc ~S"""
  Returns the location (file and line) where a module, function or macro was defined.

  ## Example

      iex> code = ~S'''
      ...> defmodule MyModule do
      ...>   alias ElixirSenseExample.ModuleWithFunctions, as: MyMod
      ...>   MyMod.function_arity_one("some string")
      ...> end
      ...> '''
      iex>  %{file: path, line: line, column: column} = ElixirSense.definition(code, 3, 11)
      iex> "#{Path.basename(path)}:#{to_string(line)}:#{to_string(column)}"
      "module_with_functions.ex:6:3"
  """
  @spec definition(String.t(), pos_integer, pos_integer) :: Location.t() | nil
  def definition(code, line, column) do
    case Code.Fragment.surround_context(code, {line, column}) do
      :none ->
        nil

      %{context: context, begin: {line, col}} ->
        buffer_file_metadata = Parser.parse_string(code, true, true, line)

        env = Metadata.get_env(buffer_file_metadata, line)

        calls =
          buffer_file_metadata.calls[line]
          |> List.wrap()
          |> Enum.filter(fn %State.CallInfo{position: {_call_line, call_column}} ->
            call_column <= column
          end)

        Definition.find(
          context,
          line,
          col,
          env,
          buffer_file_metadata.mods_funs_to_positions,
          calls,
          buffer_file_metadata.types
        )
    end
  end

  @doc ~S"""
  Returns the locations (file and line) where a behaviour or protocol was implemented.

  ## Example

      iex> code = ~S'''
      ...> ElixirSenseExample.ExampleProtocol.some()
      ...> '''
      iex>  [%{file: path, line: line, column: column}, _] = ElixirSense.implementations(code, 1, 37) |> Enum.sort
      iex> "#{Path.basename(path)}:#{to_string(line)}:#{to_string(column)}"
      "example_protocol.ex:7:3"
  """
  @spec implementations(String.t(), pos_integer, pos_integer) :: [Location.t()]
  def implementations(code, line, column) do
    case Code.Fragment.surround_context(code, {line, column}) do
      :none ->
        []

      %{context: context} ->
        buffer_file_metadata = Parser.parse_string(code, true, true, line)

        env = Metadata.get_env(buffer_file_metadata, line)

        Implementation.find(
          context,
          env,
          buffer_file_metadata.mods_funs_to_positions,
          buffer_file_metadata.types
        )
    end
  end

  @doc ~S"""
  Returns a sorted list of all available modules

  ## Example

      iex> ":application" in ElixirSense.all_modules()
      true

      iex> "Version.Parser" in ElixirSense.all_modules()
      true

  """
  @spec all_modules() :: list(String.t())
  def all_modules do
    Applications.get_modules_from_applications()
    |> Enum.map(&inspect/1)
    |> Enum.sort()
  end

  @doc """
  Finds suggestions by a given hint.

  Returned suggestions:

    * Modules, functions, variables, function params and module attributes available in the current scope.
    * Callbacks defined in behaviours (works also when @behaviour is injected by use directives)
    * Lists the accepted "returns" specs when inside a callback implementation

  Additional information:

    * Type of the module (Module, Struct, Protocol, Implementation or Exception)
    * Documentation summary for each module or function
    * Function and callback specs
    * Origin: where the function was originally defined (for aliased, imported modules or callbacks)
    * Smart snippets for functions

  ## Example

      iex> code = ~S'''
      ...> defmodule MyModule do
      ...>   alias List, as: MyList
      ...>   MyList.ins
      ...> end
      ...> '''
      iex> ElixirSense.suggestions(code, 3, 12)
      [%{origin: "List", type: :function, args: "list, index, value",
        args_list: ["list", "index", "value"],
        arity: 3, def_arity: 3, needed_require: nil, needed_import: nil,
        name: "insert_at", metadata: %{}, snippet: nil, visibility: :public,
        spec: "@spec insert_at(list, integer, any) :: list", summary: "Returns a list with `value` inserted at the specified `index`."}]
  """
  @spec suggestions(String.t(), pos_integer, pos_integer, keyword()) :: [Suggestion.suggestion()]
  def suggestions(buffer, line, column, opts \\ []) do
    hint = Source.prefix(buffer, line, column)
    buffer_file_metadata = Parser.parse_string(buffer, true, true, line)
    {text_before, text_after} = Source.split_at(buffer, line, column)

    buffer_file_metadata =
      maybe_fix_autocomple_on_cursor(buffer_file_metadata, text_before, text_after, line)

    env = Metadata.get_env(buffer_file_metadata, {line, column})
    module_store = ModuleStore.build()

    cursor_context = %{
      text_before: text_before,
      text_after: text_after,
      at_module_body?: Metadata.at_module_body?(buffer_file_metadata, env, {line, column})
    }

    Suggestion.find(hint, env, buffer_file_metadata, cursor_context, module_store, opts)
  end

  @doc """
  Returns the signature info from the function when inside a function call.

  ## Example

      iex> code = ~S'''
      ...> defmodule MyModule do
      ...>   alias List, as: MyList
      ...>   MyList.flatten(par0
      ...> end
      ...> '''
      iex> ElixirSense.signature(code, 3, 22)
      %{active_param: 0,
        signatures: [
          %{name: "flatten",
            params: ["list"],
            documentation: "Flattens the given `list` of nested lists.",
            spec: "@spec flatten(deep_list) :: list when deep_list: [any | deep_list]"},
          %{name: "flatten",
            params: ["list", "tail"],
            documentation: "Flattens the given `list` of nested lists.\\nThe list `tail` will be added at the end of\\nthe flattened list.",
            spec: "@spec flatten(deep_list, [elem]) :: [elem] when deep_list: [elem | deep_list], elem: var"}
        ]
      }
  """
  @spec signature(String.t(), pos_integer, pos_integer) :: Signature.signature_info() | :none
  def signature(code, line, column) do
    prefix = Source.text_before(code, line, column)
    buffer_file_metadata = Parser.parse_string(code, true, true, line)

    env = Metadata.get_env(buffer_file_metadata, line)

    Signature.find(prefix, env, buffer_file_metadata)
  end

  @doc """
  Returns a map containing the results of all different code expansion methods
  available.

  Available axpansion methods:

    * `expand_once` - Calls `Macro.expand_once/2`
    * `expand` - Calls `Macro.expand/2`
    * `expand_all` - Recursively calls `Macro.expand/2`
    * `expand_partial` - The same as `expand_all`, but does not expand `:def, :defp, :defmodule, :@, :defmacro,
    :defmacrop, :defoverridable, :__ENV__, :__CALLER__, :raise, :if, :unless, :in`

  > **Notice**: In order to expand the selected code properly, ElixirSense parses/expands the source file and tries to introspect context information
  like requires, aliases, import, etc. However the environment during the real compilation process may still be diffent from the one we
  try to simulate, therefore, in some cases, the expansion might not work as expected or, in some cases, not even be possible.

  ## Example

  Given the following code:

  ```
  unless ok do
    IO.puts to_string(:error)
  else
    IO.puts to_string(:ok)
  end

  ```

  A full expansion will generate the following results based on each method:

  ### expand_once

  ```
  if(ok) do
    IO.puts(to_string(:ok))
  else
    IO.puts(to_string(:error))
  end
  ```

  ### expand

  ```
  case(ok) do
    x when x in [false, nil] ->
      IO.puts(to_string(:error))
    _ ->
      IO.puts(to_string(:ok))
  end
  ```

  ### expand_partial

  ```
  unless(ok) do
    IO.puts(String.Chars.to_string(:error))
  else
    IO.puts(String.Chars.to_string(:ok))
  end
  ```

  ### expand_all

  ```
  case(ok) do
    x when :erlang.or(:erlang.=:=(x, nil), :erlang.=:=(x, false)) ->
      IO.puts(String.Chars.to_string(:error))
    _ ->
      IO.puts(String.Chars.to_string(:ok))
  end
  ```

  """
  @spec expand_full(String.t(), String.t(), pos_integer) :: Expand.expanded_code_map()
  def expand_full(buffer, code, line) do
    buffer_file_metadata = Parser.parse_string(buffer, true, true, line)

    env = Metadata.get_env(buffer_file_metadata, line)

    Expand.expand_full(code, env)
  end

  @doc """
  Converts a string to its quoted form.
  """
  @spec quote(String.t()) :: String.t()
  def quote(code) do
    Eval.quote(code)
  end

  @doc ~S"""
  Evaluate a pattern matching expression and format its results, including
  the list of bindings, if any.

  ## Example

      iex> code = '''
      ...>   {_, %{status: status, msg: message}, [arg1|_]} = {:error, %{status: 404, msg: "Not found"}, [1,2,3]}
      ...> '''
      iex> ElixirSense.match(code)
      "# Bindings\n\nstatus = 404\n\nmessage = \"Not found\"\n\narg1 = 1"
  """
  @spec match(String.t()) :: String.t()
  def match(code) do
    Eval.match_and_format(code)
  end

  @doc ~S"""
  Returns all references to a function, module or variable identified at the provided location.

  ## Example

      iex> code = ~S'''
      ...> defmodule MyModule do
      ...>   alias ElixirSense.Providers.ReferencesTest.Modules.Callee1, as: C
      ...>   C.func()
      ...> end
      ...> '''
      ...> trace = %{
      ...>   {ElixirSense.Providers.ReferencesTest.Modules.Callee1, :func, 0} => %{
      ...>     callee: {ElixirSense.Providers.ReferencesTest.Modules.Callee1, :func, 0},
      ...>     file: "test/support/modules_with_references.ex",
      ...>     line: 36,
      ...>     column: 60,
      ...>   }
      ...> }
      iex> ElixirSense.references(code, 3, 6, trace)
      [
        %{
          uri: "test/support/modules_with_references.ex",
          range: %{
            start: %{line: 36, column: 60},
            end: %{line: 36, column: 64}
          }
        }
      ]
  """
  @spec references(
          String.t(),
          pos_integer,
          pos_integer,
          call_trace_t()
        ) :: [References.reference_info()]
  def references(code, line, column, trace) do
    case Code.Fragment.surround_context(code, {line, column}) do
      :none ->
        []

      %{
        context: context,
        begin: {begin_line, begin_col},
        end: {line, col}
      } ->
        buffer_file_metadata = Parser.parse_string(code, true, true, line)

        env =
          %State.Env{
            module: module,
            vars: vars
          } = Metadata.get_env(buffer_file_metadata, line)

        # find last env of current module
        attributes = get_attributes(buffer_file_metadata.lines_to_env, module)

        # one line can contain variables from many scopes
        vars =
          case Enum.find(vars, fn %VarInfo{positions: positions} ->
                 {begin_line, begin_col} in positions
               end) do
            %VarInfo{scope_id: scope_id} ->
              # in (h|l)?eex templates vars_info_per_scope_id[scope_id] is nil
              if buffer_file_metadata.vars_info_per_scope_id[scope_id] do
                buffer_file_metadata.vars_info_per_scope_id[scope_id]
              else
                []
              end

            nil ->
              []
          end

        arity = Metadata.get_call_arity(buffer_file_metadata, line, col)

        References.find(
          context,
          begin_line,
          begin_col,
          arity,
          env,
          vars,
          attributes,
          buffer_file_metadata,
          trace
        )
    end
  end

  defp get_attributes(_lines_to_env, module) when module in [nil, Elixir], do: []

  defp get_attributes(lines_to_env, module) do
    %State.Env{attributes: attributes} =
      lines_to_env
      |> Enum.filter(fn {_k, v} -> v.module == module end)
      |> Enum.max_by(fn {k, _v} -> k end)
      |> elem(1)

    attributes
  end

  @doc ~S"""
  Provides an error tolerant parser

  ## Example

      iex> code = ~S'''
      ...> defmodule do
      ...> end
      ...> '''
      iex> ElixirSense.string_to_quoted(code, 1)
      {:ok, {:defmodule, [do: [line: 1, column: 11], end: [line: 2, column: 1], line: 1, column: 1], [[do: {:__block__, [], []}]]}}
  """
  @spec string_to_quoted(String.t(), pos_integer | nil, non_neg_integer, keyword) ::
          {:ok, Macro.t()} | {:error, {line :: pos_integer(), term(), term()}}
  def string_to_quoted(source, cursor_line_number \\ nil, error_threshold \\ 6, opts \\ []) do
    case Parser.string_to_ast(source, error_threshold, cursor_line_number, nil, opts) do
      {:ok, ast, _source} -> {:ok, ast}
      other -> other
    end
  end

  # Provides a last attempt to fix a couple of parse errors that
  # commonly appear when working with autocomplete on functions
  # without parens.
  #
  # Note: This will be removed after refactoring the parser to
  # allow unparseable nodes in the AST.
  defp maybe_fix_autocomple_on_cursor(%Metadata{error: nil} = meta, _, _, _) do
    meta
  end

  defp maybe_fix_autocomple_on_cursor(metadata, text_before, text_after, line) do
    # Fix incomplete call, e.g. cursor after `var.`
    fix_incomplete_call = fn text_before, text_after ->
      if String.ends_with?(text_before, ".") do
        text_before <> "__fake_call__" <> text_after
      end
    end

    # Fix incomplete kw, e.g. cursor after `option1: 1,`
    fix_incomplete_kw = fn text_before, text_after ->
      if Regex.match?(~r/\,\s*$/, text_before) do
        text_before <> "__fake_key__: :__fake_value__" <> text_after
      end
    end

    # Fix incomplete kw key, e.g. cursor after `option1: 1, opt`
    fix_incomplete_kw_key = fn text_before, text_after ->
      if Regex.match?(~r/\,\s*[a-z][a-zA-Z0-9_]*$/, text_before) do
        text_before <> ": :__fake_value__" <> text_after
      end
    end

    fixers = [
      fix_incomplete_call,
      fix_incomplete_kw,
      fix_incomplete_kw_key
    ]

    Enum.reduce_while(fixers, nil, fn fun, _ ->
      new_buffer = fun.(text_before, text_after)

      with true <- new_buffer != nil,
           meta <- Parser.parse_string(new_buffer, false, true, line),
           %Metadata{error: error} <- meta,
           true <- error == nil do
        {:halt, meta}
      else
        _ ->
          {:cont, metadata}
      end
    end)
  end
end
