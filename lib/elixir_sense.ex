defmodule ElixirSense do
  @moduledoc """
  ElxirSense is a Elixir library that implements useful features for any editor/tool that needs
  to introspect context-aware information about Elixir source code.

  This module provides the basic functionality for context-aware code completion, docs, signature info and more.
  """

  alias ElixirSense.Core.State
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Parser
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Source
  alias ElixirSense.Providers.Docs
  alias ElixirSense.Providers.Definition
  alias ElixirSense.Providers.Suggestion
  alias ElixirSense.Providers.Signature
  alias ElixirSense.Providers.Expand
  alias ElixirSense.Providers.Eval
  alias ElixirSense.Providers.References

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
      iex> docs |> String.split("\n") |> Enum.at(6)
      "Converts `enumerable` to a list."
      iex> types |> String.split("\n") |> Enum.at(0)
      "`@type default :: any"
  """
  @spec docs(String.t(), pos_integer, pos_integer) :: %{
          subject: String.t(),
          actual_subject: String.t(),
          docs: Introspection.docs()
        }
  def docs(code, line, column) do
    subject = Source.subject(code, line, column)
    metadata = Parser.parse_string(code, true, true, line)

    %State.Env{
      imports: imports,
      aliases: aliases,
      module: module,
      scope: scope
    } = Metadata.get_env(metadata, line)

    {actual_subject, docs} = Docs.all(subject, imports, aliases, module, scope)
    %{subject: subject, actual_subject: actual_subject, docs: docs}
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
      iex>  %{found: true, file: path, line: line, column: column} = ElixirSense.definition(code, 3, 11)
      iex> "#{Path.basename(path)}:#{to_string(line)}:#{to_string(column)}"
      "module_with_functions.ex:6:7"
  """
  @spec definition(String.t(), pos_integer, pos_integer) :: Definition.location()
  def definition(code, line, column) do
    subject = Source.subject(code, line, column) |> IO.inspect(label: "subject")
    buffer_file_metadata = Parser.parse_string(code, true, true, line) |> IO.inspect(label: "buffer_file_metadata")

    %State.Env{
      imports: imports,
      aliases: aliases,
      module: module,
      vars: vars
    } = Metadata.get_env(buffer_file_metadata, line) |> IO.inspect(label: "env")

    Definition.find(subject, imports, aliases, module, vars, buffer_file_metadata.mods_funs_to_positions)
  end

  @doc ~S"""
  Returns a sorted list of all available modules

  ## Example

      iex> ElixirSense.all_modules() |> Enum.take(4)
      [":application", ":application_controller", ":application_master", ":application_starter"]

      iex> ElixirSense.all_modules() |> Enum.take(-4)
      ["Version.InvalidVersionError", "Version.Parser", "Version.Requirement", "WithClauseError"]

  """
  def all_modules do
    Introspection.all_modules()
    |> Enum.map(&Atom.to_string(&1))
    |> Enum.map(fn x ->
      if String.downcase(x) == x do
        ":" <> x
      else
        x
      end
    end)
    |> Enum.map(&String.replace_prefix(&1, "Elixir.", ""))
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
      [%{type: :hint, value: "MyList.insert_at"},
        %{origin: "List", type: "function", args: "list,index,value", arity: 3, name: "insert_at",
        spec: "@spec insert_at(list, integer, any) :: list", summary: "Returns a list with `value` inserted at the specified `index`."}]
  """
  @spec suggestions(String.t(), non_neg_integer, non_neg_integer) :: [Suggestion.suggestion()]
  def suggestions(buffer, line, column) do
    hint = Source.prefix(buffer, line, column)
    buffer_file_metadata = Parser.parse_string(buffer, true, true, line)
    text_before = Source.text_before(buffer, line, column)

    %State.Env{
      imports: imports,
      aliases: aliases,
      vars: vars,
      attributes: attributes,
      behaviours: behaviours,
      module: module,
      scope: scope,
      protocol: protocol,
      structs: structs
    } = Metadata.get_env(buffer_file_metadata, line)

    Suggestion.find(
      hint,
      [module | imports],
      aliases,
      module,
      vars,
      attributes,
      behaviours,
      scope,
      protocol,
      buffer_file_metadata.mods_funs,
      structs,
      text_before
    )
  end

  @doc """
  Returns the signature info from the function when inside a function call.

  ## Example

      iex> code = ~S'''
      ...> defmodule MyModule do
      ...>   alias List, as: MyList
      ...>   MyList.flatten(par0,
      ...> end
      ...> '''
      iex> ElixirSense.signature(code, 3, 23)
      %{active_param: 1,
        pipe_before: false,
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
  @spec signature(String.t(), pos_integer, pos_integer) :: Signature.signature_info()
  def signature(code, line, column) do
    prefix = Source.text_before(code, line, column)
    buffer_file_metadata = Parser.parse_string(code, true, true, line)

    %State.Env{
      imports: imports,
      aliases: aliases,
      module: module
    } = Metadata.get_env(buffer_file_metadata, line)

    Signature.find(prefix, imports, aliases, module, buffer_file_metadata)
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

    %State.Env{
      requires: requires,
      imports: imports,
      module: module
    } = Metadata.get_env(buffer_file_metadata, line)

    Expand.expand_full(code, requires, imports, module)
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
      iex> ElixirSense.references(code, 3, 6) |> Enum.take(2)
      [
        %{
          uri: "test/support/modules_with_references.ex",
          range: %{
            start: %{line: 36, column: 60},
            end: %{line: 36, column: 64}
          }
        },
        %{
          uri: "test/support/modules_with_references.ex",
          range: %{
            start: %{line: 65, column: 16},
            end: %{line: 65, column: 20}
          }
        }
      ]
  """
  @spec references(String.t(), pos_integer, pos_integer) :: [References.reference_info()]
  def references(code, line, column) do
    {subject, {line, col}} = Source.subject_with_position(code, line, column)

    buffer_file_metadata = Parser.parse_string(code, true, true, line)

    %State.Env{
      imports: imports,
      aliases: aliases,
      module: module,
      scope: scope,
      scope_id: scope_id
    } = Metadata.get_env(buffer_file_metadata, line)

    vars = buffer_file_metadata.vars_info_per_scope_id[scope_id] |> Map.values()
    arity = Metadata.get_call_arity(buffer_file_metadata, line, col)

    References.find(subject, arity, imports, aliases, module, scope, vars)
  end
end
