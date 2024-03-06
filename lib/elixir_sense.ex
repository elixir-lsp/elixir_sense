defmodule ElixirSense do
  @moduledoc """
  ElxirSense is a Elixir library that implements useful features for any editor/tool that needs
  to introspect context-aware information about Elixir source code.

  This module provides the basic functionality for context-aware code completion, docs, signature info and more.
  """

  alias ElixirSense.Core.Applications
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
  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode

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
      iex> %{docs: [doc]} = ElixirSense.docs(code, 3, 11)
      iex> doc.docs |> String.split("\n") |> Enum.at(0)
      "Converts `enumerable` to a list."
  """
  @deprecated "providers will be dropped in the future"
  @spec docs(String.t(), pos_integer, pos_integer, keyword) ::
          %{
            docs: nonempty_list(Docs.doc()),
            range: %{
              begin: {pos_integer, pos_integer},
              end: {pos_integer, pos_integer}
            }
          }
          | nil
  def docs(code, line, column, options \\ []) do
    case NormalizedCode.Fragment.surround_context(code, {line, column}) do
      :none ->
        nil

      %{begin: begin_pos, end: end_pos} = context ->
        metadata =
          Keyword.get_lazy(options, :metadata, fn ->
            Parser.parse_string(code, true, true, {line, column})
          end)

        env = Metadata.get_env(metadata, {line, column})

        case Docs.all(context, env, metadata) do
          [] ->
            nil

          list ->
            %{
              docs: list,
              range: %{
                begin: begin_pos,
                end: end_pos
              }
            }
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
  @deprecated "providers will be dropped in the future"
  @spec definition(String.t(), pos_integer, pos_integer, keyword) :: Location.t() | nil
  def definition(code, line, column, options \\ []) do
    case NormalizedCode.Fragment.surround_context(code, {line, column}) do
      :none ->
        nil

      context ->
        metadata =
          Keyword.get_lazy(options, :metadata, fn ->
            Parser.parse_string(code, true, true, {line, column})
          end)

        env =
          Metadata.get_env(metadata, {line, column})
          |> Metadata.add_scope_vars(metadata, {line, column})

        Definition.find(
          context,
          env,
          metadata
        )
    end
  end

  @doc ~S"""
  Returns the locations (file and line) where a behaviour or protocol was implemented.

  ## Example

      iex> code = ~S'''
      ...> ElixirSenseExample.ExampleProtocol.some(1)
      ...> '''
      iex>  [%{file: path, line: line, column: column}, _] = ElixirSense.implementations(code, 1, 37) |> Enum.sort
      iex> "#{Path.basename(path)}:#{to_string(line)}:#{to_string(column)}"
      "example_protocol.ex:7:3"
  """
  @deprecated "providers will be dropped in the future"
  @spec implementations(String.t(), pos_integer, pos_integer, keyword) :: [Location.t()]
  def implementations(code, line, column, options \\ []) do
    case NormalizedCode.Fragment.surround_context(code, {line, column}) do
      :none ->
        []

      context ->
        metadata =
          Keyword.get_lazy(options, :metadata, fn ->
            Parser.parse_string(code, true, true, {line, column})
          end)

        env = Metadata.get_env(metadata, {line, column})

        Implementation.find(
          context,
          env,
          metadata
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
        name: "insert_at", metadata: %{app: :elixir}, snippet: nil, visibility: :public,
        spec: "@spec insert_at(list(), integer(), any()) :: list()", summary: "Returns a list with `value` inserted at the specified `index`."}]
  """
  @deprecated "providers will be dropped in the future"
  @spec suggestions(String.t(), pos_integer, pos_integer, keyword()) :: [Suggestion.suggestion()]
  def suggestions(code, line, column, options \\ []) do
    hint = Source.prefix(code, line, column)

    metadata =
      Keyword.get_lazy(options, :metadata, fn ->
        Parser.parse_string(code, true, true, {line, column})
      end)

    {text_before, text_after} = Source.split_at(code, line, column)

    metadata =
      maybe_fix_autocomple_on_cursor(
        metadata,
        text_before,
        text_after,
        {line, column}
      )

    env =
      Metadata.get_env(metadata, {line, column})
      |> Metadata.add_scope_vars(
        metadata,
        {line, column},
        &(to_string(&1.name) != hint)
      )

    # if variable is rebound then in env there are many variables with the same name
    # find the one defined closest to cursor
    vars =
      env.vars
      |> Enum.group_by(fn %State.VarInfo{name: name} -> name end)
      |> Enum.map(fn {_name, list} ->
        list
        |> Enum.max_by(fn
          %State.VarInfo{positions: [_position]} ->
            # variable is being defined - it's not a good candidate
            {0, 0}

          %State.VarInfo{positions: positions} ->
            Enum.min(positions)
        end)
      end)

    env = %{env | vars: vars}

    module_store = ModuleStore.build()

    cursor_context = %{
      cursor_position: {line, column},
      text_before: text_before,
      text_after: text_after,
      at_module_body?: Metadata.at_module_body?(env)
    }

    Suggestion.find(hint, env, metadata, cursor_context, module_store, options)
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
            spec: "@spec flatten(deep_list) :: list() when deep_list: [any() | deep_list]"},
          %{name: "flatten",
            params: ["list", "tail"],
            documentation: "Flattens the given `list` of nested lists.\\nThe list `tail` will be added at the end of\\nthe flattened list.",
            spec: "@spec flatten(deep_list, [elem]) :: [elem] when deep_list: [elem | deep_list], elem: var"}
        ]
      }
  """
  @deprecated "providers will be dropped in the future"
  @spec signature(String.t(), pos_integer, pos_integer, keyword) ::
          Signature.signature_info() | :none
  def signature(code, line, column, options \\ []) do
    prefix = Source.text_before(code, line, column)

    metadata =
      Keyword.get_lazy(options, :metadata, fn ->
        Parser.parse_string(code, true, true, {line, column})
      end)

    env = Metadata.get_env(metadata, {line, column})

    Signature.find(prefix, {line, column}, env, metadata)
  end

  @doc """
  Returns a map containing the results of all different code expansion methods
  available.

  Available expansion methods:

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
  @deprecated "providers will be dropped in the future"
  @spec expand_full(String.t(), String.t(), pos_integer) :: Expand.expanded_code_map()
  def expand_full(buffer, code, line) do
    buffer_file_metadata = Parser.parse_string(buffer, true, true, {line, 1})

    env = Metadata.get_env(buffer_file_metadata, {line, 1})

    Expand.expand_full(code, env)
  end

  @doc """
  Converts a string to its quoted form.
  """
  @deprecated "providers will be dropped in the future"
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
  @deprecated "providers will be dropped in the future"
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
        %{range: %{end: %{column: 9, line: 3}, start: %{column: 5, line: 3}}, uri: nil},
        %{
          uri: "test/support/modules_with_references.ex",
          range: %{
            start: %{line: 36, column: 60},
            end: %{line: 36, column: 64}
          }
        }
      ]
  """
  @deprecated "providers will be dropped in the future"
  @spec references(
          String.t(),
          pos_integer,
          pos_integer,
          call_trace_t(),
          keyword
        ) :: [References.reference_info()]
  def references(code, line, column, trace, options \\ []) do
    case NormalizedCode.Fragment.surround_context(code, {line, column}) do
      :none ->
        []

      %{
        begin: {begin_line, begin_col}
      } = context ->
        metadata =
          Keyword.get_lazy(options, :metadata, fn ->
            Parser.parse_string(code, true, true, {line, column})
          end)

        env =
          %State.Env{
            module_variants: module_variants
          } =
          Metadata.get_env(metadata, {line, column})
          |> Metadata.add_scope_vars(metadata, {line, column})

        # find last env of current module
        attributes = get_attributes(metadata, module_variants)

        # one line can contain variables from many scopes
        # if the cursor is over variable take variables from the scope as it will
        # be more correct than the env scope
        vars =
          case Enum.find(env.vars, fn %VarInfo{positions: positions} ->
                 {begin_line, begin_col} in positions
               end) do
            %VarInfo{scope_id: scope_id} ->
              # in (h|l)?eex templates vars_info_per_scope_id[scope_id] is nil
              if metadata.vars_info_per_scope_id[scope_id] do
                metadata.vars_info_per_scope_id[scope_id]
              else
                []
              end

            nil ->
              []
          end

        References.find(
          context,
          env,
          vars,
          attributes,
          metadata,
          trace
        )
    end
  end

  defp get_attributes(_metadata, []), do: []

  defp get_attributes(metadata, [module | _]) do
    %State.Env{attributes: attributes} = Metadata.get_last_module_env(metadata, module)

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
  @spec string_to_quoted(
          String.t(),
          {pos_integer, pos_integer} | nil,
          non_neg_integer,
          boolean,
          keyword
        ) ::
          {:ok, Macro.t()} | {:error, :parse_error}
  def string_to_quoted(
        source,
        cursor_position \\ nil,
        error_threshold \\ 6,
        fallback_to_container_cursor_to_quoted \\ true,
        parser_options \\ []
      ) do
    string_to_ast_options = [
      errors_threshold: error_threshold,
      cursor_position: cursor_position,
      fallback_to_container_cursor_to_quoted: fallback_to_container_cursor_to_quoted,
      parser_options: parser_options
    ]

    case Parser.string_to_ast(source, string_to_ast_options) do
      {:ok, ast, _source, _error} -> {:ok, ast}
      other -> other
    end
  end

  # Provides a last attempt to fix a couple of parse errors that
  # commonly appear when working with autocomplete on functions
  # without parens.
  #
  # Note: This will be removed after refactoring the parser to
  # allow unparsable nodes in the AST.
  defp maybe_fix_autocomple_on_cursor(%Metadata{error: nil} = meta, _, _, _) do
    meta
  end

  defp maybe_fix_autocomple_on_cursor(metadata, text_before, text_after, {line, column}) do
    # Fix incomplete call, e.g. cursor after `var.`
    fix_incomplete_call = fn text_before, text_after ->
      if String.ends_with?(text_before, ".") do
        text_before <> "__fake_call__" <> text_after
      end
    end

    # Fix incomplete kw, e.g. cursor after `option1: 1,`
    fix_incomplete_kw = fn text_before, text_after ->
      if Regex.match?(~r/\,\s*$/u, text_before) do
        text_before <> "__fake_key__: :__fake_value__" <> text_after
      end
    end

    # Fix incomplete kw key, e.g. cursor after `option1: 1, opt`
    fix_incomplete_kw_key = fn text_before, text_after ->
      if Regex.match?(~r/\,\s*([\p{L}_][\p{L}\p{N}_@]*[?!]?)?$/u, text_before) do
        text_before <> ": :__fake_value__" <> text_after
      end
    end

    # TODO this may no longer be needed
    # only fix_incomplete_call has some tests depending on it
    fixers = [
      fix_incomplete_call,
      fix_incomplete_kw,
      fix_incomplete_kw_key
    ]

    Enum.reduce_while(fixers, nil, fn fun, _ ->
      new_buffer = fun.(text_before, text_after)

      with true <- new_buffer != nil,
           meta <- Parser.parse_string(new_buffer, false, true, {line, column}),
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
