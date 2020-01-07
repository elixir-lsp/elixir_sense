# This file includes modified code extracted from the elixir project. Namely:
#
# https://github.com/elixir-lang/elixir/blob/c983b3db6936ce869f2668b9465a50007ffb9896/lib/iex/lib/iex/introspection.ex
# https://github.com/elixir-lang/ex_doc/blob/82463a56053b29a406fd271e9e2e2f05e87d6248/lib/ex_doc/retriever.ex
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

defmodule ElixirSense.Core.Introspection do
  @moduledoc """
  A collection of functions to introspect/format docs, specs, types and callbacks.
  """

  alias Alchemist.Helpers.ModuleInfo
  alias ElixirSense.Core.TypeInfo
  alias ElixirSense.Core.Normalized.Typespec
  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode

  @type mod_fun :: {mod :: module | nil, fun :: atom | nil}
  @type markdown :: String.t()
  @type mod_docs :: %{docs: markdown, types: markdown, callbacks: markdown}
  @type fun_docs :: %{docs: markdown, types: markdown}
  @type docs :: mod_docs | fun_docs

  @wrapped_behaviours %{
    :gen_server => GenServer,
    :gen_event => GenEvent
  }

  def all_modules do
    ModuleInfo.all_applications_modules()
  end

  @spec get_all_docs(mod_fun, scope :: ElixirSense.Core.State.scope()) :: docs
  def get_all_docs({mod, nil}, _) do
    %{docs: get_docs_md(mod), types: get_types_md(mod), callbacks: get_callbacks_md(mod)}
  end

  def get_all_docs({mod, fun}, scope) do
    docs =
      with(
        [] <- get_func_docs_md(mod, fun),
        [] <- get_type_docs_md(mod, fun, scope)
      ) do
        "No documentation available"
      else
        docs ->
          Enum.join(docs, "\n\n---\n\n")
      end

    %{docs: docs, types: get_types_md(mod)}
  end

  def count_defaults(nil), do: 0

  def count_defaults(args) do
    Enum.count(args, &match?({:\\, _, _}, &1))
  end

  def get_signatures(mod, fun, code_docs \\ nil) do
    case code_docs || NormalizedCode.get_docs(mod, :docs) do
      docs when is_list(docs) ->
        for {{f, arity}, _, kind, args, text} <- docs, f == fun do
          fun_args = Enum.map(args || [], &format_doc_arg(&1))
          fun_str = Atom.to_string(fun)
          doc = extract_summary_from_docs(text)

          spec = get_spec_as_string(mod, fun, arity, kind)
          %{name: fun_str, params: fun_args, documentation: doc, spec: spec}
        end

      nil ->
        # We are not expecting macros here
        for {_kind, {{_name, _arity}, [params | _]}} = spec <-
              TypeInfo.get_function_specs(mod, fun) do
          params = TypeInfo.extract_params(params) |> Enum.map(&Atom.to_string/1)

          %{
            name: Atom.to_string(fun),
            params: params,
            documentation: "No documentation available",
            spec: spec |> spec_to_string
          }
        end
        |> Enum.sort_by(&length(&1.params))
    end
  end

  def get_func_docs_md(mod, fun) do
    case NormalizedCode.get_docs(mod, :docs) do
      nil ->
        []

      docs ->
        for {{f, arity}, _, kind, args, text} <- docs, f == fun do
          args = args || []

          fun_args_text =
            args
            |> Enum.map_join(", ", &format_doc_arg(&1))
            |> String.replace("\\\\", "\\\\\\\\")

          mod_str = inspect(mod)
          fun_str = Atom.to_string(fun)

          "> #{mod_str}.#{fun_str}(#{fun_args_text})\n\n#{get_spec_text(mod, fun, arity, kind)}#{
            text
          }"
        end
    end
  end

  def get_docs_md(mod) when is_atom(mod) do
    mod_str = inspect(mod)

    case NormalizedCode.get_docs(mod, :moduledoc) do
      {_line, doc} when is_binary(doc) ->
        "> #{mod_str}\n\n" <> doc

      _ ->
        "No documentation available"
    end
  end

  def get_type_docs_md(_, _, {_f, _a}) do
    []
  end

  def get_type_docs_md(nil, fun, _scope) do
    result =
      for info <- ElixirSense.Core.BuiltinTypes.get_builtin_type_info(fun) do
        spec =
          case info do
            %{signature: sig} -> sig
            %{spec: spec_ast} -> TypeInfo.format_type_spec_ast(spec_ast, :type)
            _ -> "#{fun}()"
          end

        format_type_doc_md(info[:doc], spec)
      end

    case result do
      [] -> []
      list -> list ++ ["_* Built-in type_"]
    end
  end

  def get_type_docs_md(mod, fun, _scope) do
    docs = TypeInfo.get_type_docs(mod, fun)

    for {{f, arity}, _, _, text} <- docs, f == fun do
      spec =
        mod
        |> TypeInfo.get_type_spec(f, arity)
        |> TypeInfo.format_type_spec()

      format_type_doc_md(text, spec)
    end
  end

  def get_types_md(mod) when is_atom(mod) do
    for %{type: type, doc: doc} <- get_types_with_docs(mod) do
      """
      ```
      #{type}
      ```

      #{doc}
      """
    end
    |> Enum.join("\n\n---\n\n")
  end

  def get_callbacks_md(mod) when is_atom(mod) do
    for %{callback: callback, signature: signature, doc: doc} <- get_callbacks_with_docs(mod) do
      """
      > #{signature}

      ### Specs

      ```
      #{callback}
      ```

      #{doc}
      """
    end
    |> Enum.join("\n\n---\n\n")
  end

  def get_callbacks_with_docs(mod) when is_atom(mod) do
    mod =
      @wrapped_behaviours
      |> Map.get(mod, mod)

    case get_callbacks_and_docs(mod) do
      {callbacks, []} ->
        Enum.map(callbacks, fn {{name, arity}, [spec | _]} ->
          spec_ast =
            Typespec.spec_to_quoted(name, spec)
            |> Macro.prewalk(&drop_macro_env/1)

          signature = get_typespec_signature(spec_ast, arity)
          definition = format_spec_ast(spec_ast)

          %{
            name: name,
            arity: arity,
            callback: "@callback #{definition}",
            signature: signature,
            doc: nil
          }
        end)

      {callbacks, docs} ->
        Enum.map(docs, fn
          {{fun, arity}, _, kind, doc} ->
            get_callback_with_doc(kind, doc, {fun, arity}, callbacks)
        end)
    end
    |> Enum.sort_by(&{&1.name, &1.arity})
  end

  def get_types_with_docs(module) when is_atom(module) do
    module
    |> Typespec.get_types()
    |> Enum.map(fn {_, {t, _, _args}} = type ->
      %{type: format_type(type), doc: TypeInfo.get_type_doc_desc(module, t)}
    end)
  end

  def extract_summary_from_docs(doc) when doc in [nil, "", false], do: ""

  def extract_summary_from_docs(doc) do
    doc
    |> String.split("\n\n")
    |> Enum.at(0)
  end

  defp format_type({:opaque, type}) do
    {:"::", _, [ast, _]} = Typespec.type_to_quoted(type)
    "@opaque #{format_spec_ast(ast)}"
  end

  defp format_type({kind, type}) do
    ast = Typespec.type_to_quoted(type)
    "@#{kind} #{format_spec_ast(ast)}"
  end

  def format_spec_ast(spec_ast) do
    parts =
      spec_ast
      |> Macro.prewalk(&drop_macro_env/1)
      |> extract_spec_ast_parts

    name_str = Macro.to_string(parts.name)

    when_str =
      case parts[:when_part] do
        nil ->
          ""

        ast ->
          {:when, [], [:fake_lhs, ast]}
          |> Macro.to_string()
          |> String.replace_prefix(":fake_lhs", "")
      end

    returns_str =
      if parts.returns != [] do
        returns_str =
          parts.returns
          |> Enum.map(&Macro.to_string(&1))
          |> Enum.join(" |\n  ")

        case length(parts.returns) do
          1 -> " :: #{returns_str}#{when_str}"
          _ -> " ::\n  #{returns_str}#{when_str}"
        end
      else
        ""
      end

    formated_spec = name_str <> returns_str

    formated_spec |> String.replace("()", "")
  end

  def define_callback?(mod, fun, arity) do
    mod
    |> Typespec.get_callbacks()
    |> Enum.any?(fn {{f, a}, _} ->
      {f, a} == {fun, arity} or {f, a} == {:"MACRO-#{fun}", arity + 1}
    end)
  end

  def get_returns_from_callback(module, func, arity) do
    parts =
      @wrapped_behaviours
      |> Map.get(module, module)
      |> get_callback_ast(func, arity)
      |> Macro.prewalk(&drop_macro_env/1)
      |> extract_spec_ast_parts

    for return <- parts.returns do
      ast = return |> strip_return_types()

      return =
        case parts[:when_part] do
          nil -> return
          _ -> {:when, [], [return, parts.when_part]}
        end

      spec = return |> TypeInfo.spec_ast_to_string()
      stripped = ast |> TypeInfo.spec_ast_to_string()
      snippet = ast |> return_to_snippet()
      %{description: stripped, spec: spec, snippet: snippet}
    end
  end

  defp extract_spec_ast_parts({:when, _, [{:"::", _, [name_part, return_part]}, when_part]}) do
    %{name: name_part, returns: extract_return_part(return_part, []), when_part: when_part}
  end

  defp extract_spec_ast_parts({:"::", _, [name_part, return_part]}) do
    %{name: name_part, returns: extract_return_part(return_part, [])}
  end

  defp extract_spec_ast_parts({_name, _meta, _args} = name_part) do
    %{name: name_part, returns: []}
  end

  defp extract_return_part({:|, _, [lhs, rhs]}, returns) do
    [lhs | extract_return_part(rhs, returns)]
  end

  defp extract_return_part(ast, returns) do
    [ast | returns]
  end

  defp get_callback_with_doc(kind, doc, key = {name, arity}, callbacks) do
    key =
      {spec_name, _spec_arity} =
      if kind == :macrocallback do
        {:"MACRO-#{name}", arity + 1}
      else
        key
      end

    {_, [spec | _]} = List.keyfind(callbacks, key, 0)

    spec_ast =
      Typespec.spec_to_quoted(spec_name, spec)
      |> Macro.prewalk(&drop_macro_env/1)

    spec_ast =
      if kind == :macrocallback do
        spec_ast |> remove_first_macro_arg()
      else
        spec_ast
      end

    signature = get_typespec_signature(spec_ast, arity)
    definition = format_spec_ast(spec_ast)

    %{
      name: name,
      arity: arity,
      callback: "@#{kind} #{definition}",
      signature: signature,
      doc: doc
    }
  end

  defp get_callbacks_and_docs(mod) do
    callbacks = Typespec.get_callbacks(mod)

    docs =
      @wrapped_behaviours
      |> Map.get(mod, mod)
      |> NormalizedCode.get_docs(:callback_docs)

    {callbacks, docs || []}
  end

  defp drop_macro_env({name, meta, [{:"::", _, [_, {{:., _, [Macro.Env, :t]}, _, _}]} | args]}),
    do: {name, meta, args}

  defp drop_macro_env(other), do: other

  defp get_typespec_signature({:when, _, [{:"::", _, [{name, meta, args}, _]}, _]}, arity) do
    Macro.to_string({name, meta, strip_types(args, arity)})
  end

  defp get_typespec_signature({:"::", _, [{name, meta, args}, _]}, arity) do
    Macro.to_string({name, meta, strip_types(args, arity)})
  end

  defp get_typespec_signature({name, meta, args}, arity) do
    Macro.to_string({name, meta, strip_types(args, arity)})
  end

  defp strip_types(args, arity) do
    args
    |> Enum.take(-arity)
    |> Enum.with_index()
    |> Enum.map(fn
      {{:"::", _, [left, _]}, i} -> to_var(left, i)
      {{:|, _, _}, i} -> to_var({}, i)
      {left, i} -> to_var(left, i)
    end)
  end

  defp strip_return_types(returns) when is_list(returns) do
    returns |> Enum.map(&strip_return_types/1)
  end

  defp strip_return_types({:"::", _, [left, _]}) do
    left
  end

  defp strip_return_types({:|, meta, args}) do
    {:|, meta, strip_return_types(args)}
  end

  defp strip_return_types({:{}, meta, args}) do
    {:{}, meta, strip_return_types(args)}
  end

  defp strip_return_types(value) do
    value
  end

  defp return_to_snippet(ast) do
    {ast, _} = Macro.prewalk(ast, 1, &term_to_snippet/2)
    ast |> Macro.to_string()
  end

  defp term_to_snippet({name, _, nil} = ast, index) when is_atom(name) do
    next_snippet(ast, index)
  end

  defp term_to_snippet({:__aliases__, _, _} = ast, index) do
    next_snippet(ast, index)
  end

  defp term_to_snippet({{:., _, _}, _, _} = ast, index) do
    next_snippet(ast, index)
  end

  defp term_to_snippet({:|, _, _} = ast, index) do
    next_snippet(ast, index)
  end

  defp term_to_snippet(ast, index) do
    {ast, index}
  end

  defp next_snippet(ast, index) do
    {"${#{index}:#{TypeInfo.spec_ast_to_string(ast)}}$", index + 1}
  end

  def param_to_var({{:=, _, [_lhs, {name, _, _} = rhs]}, arg_index}) when is_atom(name) do
    rhs
    |> to_var(arg_index + 1)
    |> Macro.to_string()
  end

  def param_to_var({{:=, _, [{name, _, _} = lhs, _rhs]}, arg_index}) when is_atom(name) do
    lhs
    |> to_var(arg_index + 1)
    |> Macro.to_string()
  end

  def param_to_var({{:\\, _, _} = ast, _}) do
    ast
    |> Macro.to_string()
  end

  def param_to_var({ast, arg_index}) do
    ast
    |> to_var(arg_index + 1)
    |> Macro.to_string()
  end

  defp to_var({:%, meta, [name, _]}, _), do: {:%, meta, [name, {:%{}, meta, []}]}
  defp to_var({name, meta, _}, _) when is_atom(name), do: {name, meta, nil}
  defp to_var([{:->, _, _} | _], _), do: {:function, [], nil}
  defp to_var({:<<>>, _, _}, _), do: {:binary, [], nil}
  defp to_var({:%{}, _, _}, _), do: {:map, [], nil}
  defp to_var({:{}, _, _}, _), do: {:tuple, [], nil}
  defp to_var({_, _}, _), do: {:tuple, [], nil}
  defp to_var(integer, _) when is_integer(integer), do: {:integer, [], nil}
  defp to_var(float, _) when is_integer(float), do: {:float, [], nil}
  defp to_var(list, _) when is_list(list), do: {:list, [], nil}
  defp to_var(atom, _) when is_atom(atom), do: {:atom, [], nil}
  defp to_var(_, position), do: {:"arg#{position}", [], nil}

  def get_module_docs_summary(module) do
    case NormalizedCode.get_docs(module, :moduledoc) do
      {_, doc} -> extract_summary_from_docs(doc)
      _ -> ""
    end
  end

  @doc ~S"""
  Returns module subtype if known

  ## Examples

      iex> ElixirSense.Core.Introspection.get_module_subtype(ArgumentError)
      :exception
      iex> ElixirSense.Core.Introspection.get_module_subtype(Enumerable)
      :protocol
      iex> ElixirSense.Core.Introspection.get_module_subtype(Enumerable.List)
      :implementation
      iex> ElixirSense.Core.Introspection.get_module_subtype(Access)
      :behaviour
      iex> ElixirSense.Core.Introspection.get_module_subtype(URI)
      :struct
      iex> ElixirSense.Core.Introspection.get_module_subtype(Mix.Tasks.Run)
      :task
      iex> ElixirSense.Core.Introspection.get_module_subtype(NotExistingModule)
      nil
  """
  def get_module_subtype(module) do
    has_func = fn f, a -> module_has_function(module, f, a) end

    cond do
      has_func.(:__protocol__, 1) ->
        :protocol

      has_func.(:__impl__, 1) ->
        :implementation

      has_func.(:__struct__, 0) ->
        if Map.get(module.__struct__, :__exception__) do
          :exception
        else
          :struct
        end

      has_func.(:behaviour_info, 1) ->
        :behaviour

      match?("Elixir.Mix.Tasks." <> _, Atom.to_string(module)) ->
        :task

      true ->
        nil
    end
  end

  # NOTE does not work for macro
  def module_has_function(module, func, arity) do
    Code.ensure_loaded?(module) && Kernel.function_exported?(module, func, arity)
  end

  def module_is_struct?(module) do
    module_has_function(module, :__struct__, 0)
  end

  def extract_fun_args_and_desc({{_fun, _}, _line, _kind, args, doc}) do
    formatted_args =
      (args || [])
      |> Enum.map_join(",", &format_doc_arg(&1))
      |> String.replace(Regex.recompile!(~r/\s+/), " ")

    desc = extract_summary_from_docs(doc)
    {formatted_args, desc}
  end

  def extract_fun_args_and_desc(nil) do
    {"", ""}
  end

  def get_spec_as_string(module, function, arity, :defmacro) do
    TypeInfo.get_spec(module, :"MACRO-#{function}", arity + 1) |> spec_to_string()
  end

  def get_spec_as_string(module, function, arity, :def) do
    TypeInfo.get_spec(module, function, arity) |> spec_to_string()
  end

  def get_spec_text(mod, fun, arity, kind) do
    case get_spec_as_string(mod, fun, arity, kind) do
      "" ->
        ""

      spec ->
        "### Specs\n\n```\n#{spec}\n```\n\n"
    end
  end

  # This function is used only for protocols so no macros
  # and we dont expect docs to be nil
  def module_functions_info(module) do
    docs = NormalizedCode.get_docs(module, :docs) || []
    specs = TypeInfo.get_module_specs(module)

    for {{f, a}, _line, func_kind, _sign, doc} = func_doc <- docs, doc != false, into: %{} do
      spec = Map.get(specs, {f, a})
      {fun_args, desc} = extract_fun_args_and_desc(func_doc)

      {{f, a}, {func_kind, fun_args, desc, spec_to_string(spec)}}
    end
  end

  def get_callback_ast(module, callback, arity) do
    {{name, _}, [spec | _]} =
      module
      |> Typespec.get_callbacks()
      |> Enum.find(fn {{f, a}, _} ->
        {f, a} == {callback, arity} or {f, a} == {:"MACRO-#{callback}", arity + 1}
      end)

    Typespec.spec_to_quoted(name, spec)
    |> Macro.prewalk(&drop_macro_env/1)
  end

  defp format_doc_arg({:\\, _, [left, right]}) do
    format_doc_arg(left) <> " \\\\ " <> Macro.to_string(right)
  end

  defp format_doc_arg({var, _, _}) do
    Atom.to_string(var)
  end

  def remove_first_macro_arg(
        {:when, info3, [{:"::", info, [{name, info2, rest_args}, ret]}, var_params]}
      ) do
    "MACRO-" <> rest = Atom.to_string(name)

    # Prior to 1.10 elixir generated invalid specs for macros and macrocallbacks with `when`
    # https://github.com/elixir-lang/elixir/issues/9687
    sub =
      if Version.match?(System.version(), ">= 1.10.0") do
        [{String.to_atom(rest), info2, rest_args |> tl}, ret]
      else
        # specs is missing first argument of type term so nothing to drop here
        [{String.to_atom(rest), info2, rest_args}, ret]
      end

    {:when, info3, [{:"::", info, sub}, var_params]}
  end

  def remove_first_macro_arg({:"::", info, [{name, info2, [_term_arg | rest_args]}, return]}) do
    "MACRO-" <> rest = Atom.to_string(name)
    {:"::", info, [{String.to_atom(rest), info2, rest_args}, return]}
  end

  def spec_to_string(nil) do
    ""
  end

  def spec_to_string({kind, {{name, _arity}, specs}}) do
    is_macro = Atom.to_string(name) |> String.starts_with?("MACRO-")

    specs
    |> Enum.map_join("\n", fn spec ->
      quoted =
        Typespec.spec_to_quoted(name, spec)
        |> Macro.prewalk(&drop_macro_env/1)

      quoted =
        if is_macro do
          quoted |> remove_first_macro_arg()
        else
          quoted
        end

      binary = Macro.to_string(quoted)
      "@#{kind} #{binary}" |> String.replace("()", "")
    end)
  end

  @spec actual_module(
          nil | module,
          [{module, module}],
          nil | module,
          ElixirSense.Core.State.mods_funs_to_positions_t()
        ) :: {nil | module, boolean}
  def actual_module(module, aliases, current_module, mods_funs) do
    {m, nil, res} =
      actual_mod_fun(
        {module, nil},
        [],
        aliases,
        current_module,
        mods_funs,
        %{}
      )

    {m, res}
  end

  @doc ~S"""
  Expand module using aliases list

  ## Examples

      iex> ElixirSense.Core.Introspection.expand_alias(MyList, [])
      MyList
      iex> ElixirSense.Core.Introspection.expand_alias(:erlang_module, [])
      :erlang_module
      iex> ElixirSense.Core.Introspection.expand_alias(MyList, [{MyList, List}])
      List
      iex> ElixirSense.Core.Introspection.expand_alias(MyList.Sub, [{MyList, List}])
      List.Sub
      iex> ElixirSense.Core.Introspection.expand_alias(MyList.Sub, [{MyList, List.Some}])
      List.Some.Sub
      iex> ElixirSense.Core.Introspection.expand_alias(MyList, [{MyList, :lists}])
      :lists
      iex> ElixirSense.Core.Introspection.expand_alias(MyList.Sub, [{MyList, :lists}])
      :"Elixir.lists.Sub"
      iex> ElixirSense.Core.Introspection.expand_alias(Elixir, [{MyList, :lists}])
      Elixir
      iex> ElixirSense.Core.Introspection.expand_alias(E.String, [{E, Elixir}])
      String
      iex> ElixirSense.Core.Introspection.expand_alias(E, [{E, Elixir}])
      Elixir
      iex> ElixirSense.Core.Introspection.expand_alias(nil, [])
      nil
  """
  def expand_alias(mod, aliases) do
    if elixir_module?(mod) do
      [mod_head | mod_tail] = Module.split(mod)

      case Enum.find(aliases, fn {alias_mod, _alias_expansion} ->
             [alias_head] = Module.split(alias_mod)
             alias_head == mod_head
           end) do
        nil ->
          # alias not found
          mod

        {_alias_mod, alias_expansion} ->
          # Elixir alias expansion rules are strange as they allow submodules to aliased erlang modules
          # however when they are expanded an elixir module is created, e.g. in
          # alias :erl, as: E
          # E.Sub.fun()
          # results in
          # :"Elixir.erl.Sub".fun()
          if mod_tail != [] do
            # append submodule to expansion
            Module.concat([alias_expansion | mod_tail])
          else
            # alias expands to erlang module
            alias_expansion
          end
      end
    else
      # erlang module or `Elixir`
      mod
    end
  end

  @spec actual_mod_fun(
          {nil | module, nil | atom},
          [module],
          [{module, module}],
          nil | module,
          ElixirSense.Core.State.mods_funs_to_positions_t(),
          ElixirSense.Core.State.types_t()
        ) :: {nil | module, nil | atom, boolean}
  def actual_mod_fun({nil, nil}, _, _, _, _, _), do: {nil, nil, false}

  def actual_mod_fun(
        mod_fun = {mod, fun},
        imports,
        aliases,
        current_module,
        mods_funs,
        metadata_types
      ) do
    expanded_mod = expand_alias(mod, aliases)

    with {nil, nil} <- find_kernel_function(mod_fun),
         {nil, nil} <-
           find_metadata_function(
             {expanded_mod, fun},
             current_module,
             imports,
             mods_funs,
             metadata_types,
             true
           ),
         {nil, nil} <- find_builtin_type(mod_fun) do
      {expanded_mod, fun, false}
    else
      {m, f} -> {m, f, true}
    end
  end

  defp has_type?(_mod, _type, _metadata_types, false), do: false

  defp has_type?(mod, type, metadata_types, true) do
    Map.has_key?(metadata_types, {mod, type, nil}) or
      ElixirSense.Core.Normalized.Typespec.get_types(mod)
      |> Enum.any?(fn {_kind, {name, _def, _args}} -> name == type end)
  end

  defp find_metadata_function(
         {nil, fun},
         current_module,
         imports,
         mods_funs,
         metadata_types,
         include_typespecs
       ) do
    mods =
      case current_module do
        nil -> imports
        _ -> [{current_module, :current_module} | imports]
      end

    case Enum.find(mods, fn
           {mod, :current_module} ->
             find_metadata_function(
               {mod, fun},
               current_module,
               imports,
               mods_funs,
               metadata_types,
               include_typespecs
             ) != {nil, nil}

           mod ->
             # typespecs are not imported
             find_metadata_function(
               {mod, fun},
               current_module,
               imports,
               mods_funs,
               metadata_types,
               false
             ) != {nil, nil}
         end) do
      nil -> {nil, nil}
      {current_module, :current_module} -> {current_module, fun}
      mod -> {mod, fun}
    end
  end

  defp find_metadata_function(
         {Elixir, _},
         _current_module,
         _imports,
         _mods_funs,
         _metadata_types,
         _include_typespecs
       ),
       do: {nil, nil}

  defp find_metadata_function(
         {mod, nil},
         _current_module,
         _imports,
         mods_funs,
         _metadata_types,
         _include_typespecs
       ) do
    if Map.has_key?(mods_funs, {mod, nil, nil}) or match?({:module, _}, Code.ensure_loaded(mod)) do
      {mod, nil}
    else
      {nil, nil}
    end
  end

  defp find_metadata_function(
         {mod, fun},
         current_module,
         _imports,
         mods_funs,
         metadata_types,
         include_typespecs
       ) do
    found_in_metadata =
      case mods_funs[{mod, nil, nil}] do
        nil ->
          false

        _funs ->
          Enum.any?(mods_funs, fn {{m, f, _a}, info} ->
            m == mod and f == fun and (mod == current_module or is_pub(info.type))
          end)
      end

    if found_in_metadata or ModuleInfo.has_function?(mod, fun) or
         has_type?(mod, fun, metadata_types, include_typespecs) do
      {mod, fun}
    else
      {nil, nil}
    end
  end

  defp find_builtin_type({nil, fun}) do
    if ElixirSense.Core.BuiltinTypes.builtin_type?(fun) do
      {nil, fun}
    else
      {nil, nil}
    end
  end

  defp find_builtin_type({_mod, _fun}) do
    {nil, nil}
  end

  defp find_kernel_function({nil, fun}) do
    cond do
      ModuleInfo.docs?(Kernel, fun) ->
        {Kernel, fun}

      ModuleInfo.docs?(Kernel.SpecialForms, fun) ->
        {Kernel.SpecialForms, fun}

      true ->
        {nil, nil}
    end
  end

  defp find_kernel_function({_mod, _fun}) do
    {nil, nil}
  end

  @doc ~S"""
  Tests if term is an elixir module

  ## Examples

      iex> ElixirSense.Core.Introspection.elixir_module?(List)
      true
      iex> ElixirSense.Core.Introspection.elixir_module?(List.Submodule)
      true
      iex> ElixirSense.Core.Introspection.elixir_module?(:lists)
      false
      iex> ElixirSense.Core.Introspection.elixir_module?(:"NotAModule")
      false
      iex> ElixirSense.Core.Introspection.elixir_module?(:"Elixir.SomeModule")
      true
      iex> ElixirSense.Core.Introspection.elixir_module?(:"Elixir.someModule")
      true
      iex> ElixirSense.Core.Introspection.elixir_module?(Elixir.SomeModule)
      true
      iex> ElixirSense.Core.Introspection.elixir_module?(Elixir)
      false
  """
  def elixir_module?(term) when is_atom(term) do
    term == Module.concat(Elixir, term)
  end

  def elixir_module?(_) do
    false
  end

  defp format_type_doc_md(doc, spec, footer \\ "") do
    formatted_spec = "```\n#{spec}\n```"
    "#{doc}\n\n#{formatted_spec}#{footer}"
  end

  def is_pub(type), do: type in [:def, :defmacro, :defdelegate, :defguard]
end
