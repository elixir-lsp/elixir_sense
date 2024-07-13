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

  alias ElixirSense.Core.BuiltinFunctions
  alias ElixirSense.Core.BuiltinTypes
  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode
  alias ElixirSense.Core.Normalized.Typespec
  alias ElixirSense.Core.State
  alias ElixirSense.Core.TypeInfo
  require Logger

  @type mod_fun :: {module | nil, atom | nil}

  @type module_subtype ::
          :exception | :protocol | :implementation | :behaviour | :struct | :task | :alias | nil

  defguard matches_arity?(is, expected)
           when is != nil and
                  (expected == :any or is == expected or
                     (is_tuple(expected) and elem(expected, 0) == :gte and is >= elem(expected, 1)))

  defguard matches_arity_with_defaults?(is, defaults, expected)
           when is != nil and
                  (expected == :any or
                     (is_integer(expected) and expected in (is - defaults)..is//1) or
                     (is_tuple(expected) and elem(expected, 0) == :gte and is >= elem(expected, 1)))

  @spec get_exports(module) :: [{atom, {non_neg_integer, :macro | :function}}]
  def get_exports(Elixir), do: []

  def get_exports(module) do
    case Code.ensure_loaded(module) do
      {:module, _} ->
        exports =
          try do
            module.module_info(:exports)
          rescue
            ArgumentError ->
              # in case of race conditions the module might get unloaded
              []
          end

        for {f, a} <- exports do
          {f_dropped, a_dropped} = drop_macro_prefix({f, a})

          kind =
            if {f_dropped, a_dropped} != {f, a} do
              :macro
            else
              :function
            end

          {f_dropped, {a_dropped, kind}}
        end
        |> Kernel.++(
          BuiltinFunctions.erlang_builtin_functions(module)
          |> Enum.map(fn {f, a} -> {f, {a, :function}} end)
        )

      _otherwise ->
        []
    end
  end

  @spec get_callbacks(module) :: [{atom, non_neg_integer}]
  def get_callbacks(Elixir), do: []

  def get_callbacks(module) do
    with {:module, _} <- Code.ensure_loaded(module),
         true <- function_exported?(module, :behaviour_info, 1) do
      for {f, a} <- module.behaviour_info(:callbacks) do
        drop_macro_prefix({f, a})
      end
    else
      _ -> []
    end
  end

  def drop_macro_prefix({f, a}) do
    case Atom.to_string(f) do
      "MACRO-" <> macro_name ->
        # extract macro name
        {String.to_atom(macro_name), a - 1}

      _ ->
        # normal public fun
        {f, a}
    end
  end

  @doc ~S"""
  Checks if function or macro is exported from module

  ## Example

      iex> ElixirSense.Core.Introspection.exported?(Atom, :to_string)
      true

      iex> ElixirSense.Core.Introspection.exported?(Kernel, :defmodule)
      true

      iex> ElixirSense.Core.Introspection.exported?(:erlang, :orelse)
      true

      iex> ElixirSense.Core.Introspection.exported?(:lists, :flatten)
      true

  """
  @spec exported?(module, atom) :: boolean
  def exported?(module, fun) do
    Keyword.has_key?(get_exports(module), fun)
  end

  def count_defaults(nil), do: 0

  def count_defaults(args) do
    Enum.count(args, &match?({:\\, _, _}, &1))
  end

  @spec get_signatures(module, atom, nil | [NormalizedCode.fun_doc_entry_t()]) :: [
          ElixirSense.Core.Metadata.signature_t()
        ]
  def get_signatures(mod, fun, code_docs \\ nil)

  def get_signatures(mod, fun, _code_docs)
      when not is_nil(mod) and fun in [:module_info, :behaviour_info, :__info__] do
    for {f, a} <- BuiltinFunctions.all(), f == fun do
      spec = BuiltinFunctions.get_specs({f, a}) |> Enum.join("\n")
      params = BuiltinFunctions.get_args({f, a})
      %{name: Atom.to_string(fun), params: params, documentation: "Built-in function", spec: spec}
    end
  end

  def get_signatures(mod, fun, code_docs) when not is_nil(mod) and not is_nil(fun) do
    case code_docs || NormalizedCode.get_docs(mod, :docs) do
      docs when is_list(docs) ->
        results =
          for {{f, arity}, _, kind, args, text, metadata} <- docs, f == fun do
            fun_args = get_fun_args_from_doc_or_typespec(mod, f, arity, args, metadata)

            fun_str = Atom.to_string(fun)
            doc = extract_summary_from_docs(text)

            spec = get_spec_as_string(mod, fun, arity, kind, metadata)
            %{name: fun_str, params: fun_args, documentation: doc, spec: spec}
          end

        case results do
          [] ->
            get_spec_from_typespec(mod, fun)

          other ->
            other
        end

      nil ->
        get_spec_from_typespec(mod, fun)
    end
    |> Enum.sort_by(&length(&1.params))
  end

  defp get_spec_from_typespec(mod, fun) do
    # TypeInfo.get_function_specs does fallback to behaviours
    {behaviour, specs} = TypeInfo.get_function_specs(mod, fun, :any)

    results =
      for {{_name, _arity}, [params | _]} = spec <- specs do
        params = TypeInfo.extract_params(params)

        %{
          name: Atom.to_string(fun),
          params: params,
          documentation: "",
          spec: spec |> spec_to_string(if(behaviour, do: :callback, else: :spec))
        }
      end

    case results do
      [] ->
        # no typespecs
        # provide dummy spec basing on module_info(:exports)
        get_spec_from_module_info(mod, fun)

      other ->
        other
    end
  end

  defp get_spec_from_module_info(mod, fun) do
    # no fallback to behaviours here - we assume that behaviours have typespecs
    # and were already handled
    for {f, {a, _kind}} <- get_exports(mod),
        f == fun do
      dummy_params = if a == 0, do: [], else: Enum.map(1..a, fn _ -> "term" end)

      %{
        name: Atom.to_string(fun),
        params: dummy_params,
        documentation: "",
        spec: ""
      }
    end
  end

  def get_callbacks_with_docs(mod) when is_atom(mod) do
    optional_callbacks =
      if Code.ensure_loaded?(mod) and function_exported?(mod, :behaviour_info, 1),
        do: mod.behaviour_info(:optional_callbacks),
        else: []

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
            kind: :callback,
            arity: arity,
            callback: "@callback #{definition}",
            signature: signature,
            doc: nil,
            metadata: %{optional: {name, arity} in optional_callbacks}
          }
        end)

      {callbacks, docs} ->
        Enum.map(docs, fn
          {{fun, arity}, _, kind, doc, metadata} ->
            get_callback_with_doc(
              kind,
              doc,
              metadata,
              {fun, arity},
              callbacks,
              optional_callbacks
            )
        end)
    end
    |> Enum.sort_by(&{&1.name, &1.arity})
  end

  def get_types_with_docs(module) when is_atom(module) do
    docs = NormalizedCode.get_docs(module, :type_docs)

    module
    |> Typespec.get_types()
    |> Enum.filter(fn {kind, {_t, _, _args}} -> kind in [:type, :opaque] end)
    |> Enum.map(fn {_, {t, _, args}} = type ->
      {doc, metadata} =
        if docs do
          TypeInfo.get_type_doc_desc(docs, t, length(args))
        else
          {"", %{}}
        end

      type_args = Enum.map_join(args, ", ", &(&1 |> elem(2) |> Atom.to_string()))

      %{
        type_name: t,
        type_args: type_args,
        type: format_type(type),
        doc: doc,
        metadata: metadata
      }
    end)
  end

  def extract_summary_from_docs(doc) when doc in [nil, "", false], do: ""

  def extract_summary_from_docs(doc) when is_binary(doc) do
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
          |> Enum.map_join(" |\n  ", &Macro.to_string(&1))

        case length(parts.returns) do
          1 -> " :: #{returns_str}#{when_str}"
          _ -> " ::\n  #{returns_str}#{when_str}"
        end
      else
        ""
      end

    name_str <> returns_str
  end

  def get_returns_from_callback(behaviour, callback, arity) do
    callbacks = Typespec.get_callbacks(behaviour)

    found =
      callbacks
      |> Enum.find(fn {{f, a}, _} ->
        drop_macro_prefix({f, a}) == {callback, arity}
      end)

    case found do
      {{name, _}, [spec | _]} ->
        ast =
          Typespec.spec_to_quoted(name, spec)
          |> Macro.prewalk(&drop_macro_env/1)

        get_returns_from_spec_ast(ast)

      nil ->
        []
    end
  end

  def get_returns_from_spec_ast(ast) do
    parts = extract_spec_ast_parts(ast)

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

  defp get_callback_with_doc(
         kind,
         doc,
         metadata,
         {name, arity},
         callbacks,
         optional_callbacks
       ) do
    key =
      {spec_name, _spec_arity} =
      if kind == :macrocallback do
        {:"MACRO-#{name}", arity + 1}
      else
        {name, arity}
      end

    case List.keyfind(callbacks, key, 0) do
      nil ->
        Logger.error(
          "Unable to match callback #{inspect(key)} from doc chunk, with any of callbacks from typespec #{inspect(Enum.map(callbacks, &elem(&1, 0)))}"
        )

        args = if(arity == 0, do: "", else: Enum.map_join(1..arity, ", ", fn _ -> "term" end))

        %{
          name: name,
          kind: kind,
          arity: arity,
          callback: "@#{kind} #{name}(#{args})",
          signature: nil,
          doc: doc,
          metadata: metadata |> Map.put(:optional, key in optional_callbacks)
        }

      {_, [spec | _]} ->
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
          kind: kind,
          arity: arity,
          callback: "@#{kind} #{definition}",
          signature: signature,
          doc: doc,
          metadata: metadata |> Map.put(:optional, key in optional_callbacks)
        }
    end
  end

  defp get_callbacks_and_docs(mod) do
    callbacks = Typespec.get_callbacks(mod)
    docs = NormalizedCode.get_docs(mod, :callback_docs)

    {callbacks, docs || []}
  end

  defp drop_macro_env({name, meta, [{:"::", _, [_, {{:., _, [Macro.Env, :t]}, _, _}]} | args]}),
    do: {name, meta, args}

  defp drop_macro_env(other), do: other

  defp get_typespec_signature({:when, _, [{:"::", _, [{name, meta, args}, _]}, _]}, arity) do
    to_string_with_parens({name, meta, strip_types(args, arity)})
  end

  defp get_typespec_signature({:"::", _, [{name, meta, args}, _]}, arity) do
    to_string_with_parens({name, meta, strip_types(args, arity)})
  end

  defp get_typespec_signature({name, meta, args}, arity) do
    to_string_with_parens({name, meta, strip_types(args, arity)})
  end

  def to_string_with_parens({name, meta, args}) when is_atom(name) do
    if ElixirSense.Core.Normalized.Code.Formatter.local_without_parens?(
         name,
         length(args || []),
         ElixirSense.Core.Normalized.Code.Formatter.locals_without_parens()
       ) do
      # Macro.to_string formats some locals without parens
      # notable case is Phoenix.Endpoint.config/2 callback
      replacement = :__elixir_sense_replace_me__

      Macro.to_string({replacement, meta, args})
      |> String.replace(Atom.to_string(replacement), Atom.to_string(name))
    else
      Macro.to_string({name, meta, args})
    end
  end

  def to_string_with_parens(ast) do
    Macro.to_string(ast)
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
  defp to_var([{:->, _, _} | _], _), do: {:function, [], nil}
  defp to_var({:<<>>, _, _}, _), do: {:binary, [], nil}
  defp to_var({:%{}, _, _}, _), do: {:map, [], nil}
  defp to_var({:{}, _, _}, _), do: {:tuple, [], nil}
  defp to_var({name, meta, _}, _) when is_atom(name), do: {name, meta, nil}
  defp to_var({_, _}, _), do: {:tuple, [], nil}
  defp to_var(integer, _) when is_integer(integer), do: {:integer, [], nil}
  defp to_var(float, _) when is_integer(float), do: {:float, [], nil}
  defp to_var(list, _) when is_list(list), do: {:list, [], nil}
  defp to_var(atom, _) when is_atom(atom), do: {:atom, [], nil}
  defp to_var(_, position), do: {:"arg#{position}", [], nil}

  def get_module_docs_summary(module, docs \\ nil) do
    case docs || NormalizedCode.get_docs(module, :moduledoc) do
      {_, doc, metadata} ->
        {extract_summary_from_docs(doc), metadata}

      _ ->
        {"", %{}}
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
      :alias
      iex> ElixirSense.Core.Introspection.get_module_subtype(Elixir)
      :alias
  """
  @spec get_module_subtype(module()) :: module_subtype()
  def get_module_subtype(module) do
    has_func = fn f, a -> module_has_function(module, f, a) end

    cond do
      has_func.(:__protocol__, 1) ->
        :protocol

      has_func.(:__impl__, 1) ->
        :implementation

      has_func.(:__struct__, 0) ->
        if has_func.(:exception, 1) do
          :exception
        else
          :struct
        end

      has_func.(:behaviour_info, 1) ->
        :behaviour

      match?("Elixir.Mix.Tasks." <> _, Atom.to_string(module)) ->
        if has_func.(:run, 1) do
          :task
        end

      module == Elixir ->
        :alias

      not has_func.(:module_info, 1) and match?("Elixir." <> _, Atom.to_string(module)) ->
        :alias

      true ->
        nil
    end
  end

  # NOTE does not work for macro
  def module_has_function(Elixir, _func, _arity), do: false

  def module_has_function(module, func, arity) do
    Code.ensure_loaded?(module) and Kernel.function_exported?(module, func, arity)
  end

  def module_is_struct?(module) do
    module_has_function(module, :__struct__, 0)
  end

  def extract_fun_args({{_fun, _}, _line, _kind, args, _doc, _metadata}) do
    (args || [])
    |> Enum.map(&format_doc_arg(&1))
  end

  def extract_fun_args(atom) when atom in [nil, false] do
    []
  end

  # erlang docs have signature included in metadata
  def get_spec_as_string(_module, _function, _arity, :function, %{signature: signature}) do
    [{:attribute, _, :spec, spec}] = signature
    spec |> spec_to_string()
  end

  def get_spec_as_string(_module, function, arity, :macro, %{implementing: behaviour}) do
    TypeInfo.get_callback(behaviour, :"MACRO-#{function}", arity + 1)
    |> spec_to_string(:macrocallback)
  end

  def get_spec_as_string(_module, function, arity, :function, %{implementing: behaviour}) do
    TypeInfo.get_callback(behaviour, function, arity) |> spec_to_string(:callback)
  end

  def get_spec_as_string(module, function, arity, :macro, _) do
    TypeInfo.get_spec(module, :"MACRO-#{function}", arity + 1) |> spec_to_string()
  end

  def get_spec_as_string(module, function, arity, :function, _) do
    TypeInfo.get_spec(module, function, arity) |> spec_to_string()
  end

  def get_specs_text(mod, fun, arity, kind, metadata) do
    for arity <- (arity - Map.get(metadata, :defaults, 0))..arity,
        spec = get_spec_as_string(mod, fun, arity, kind, metadata),
        spec != "",
        do: spec
  end

  # This function is used only for protocols so no macros
  # and we don't expect docs to be nil
  def module_functions_info(module) do
    docs = NormalizedCode.get_docs(module, :docs) || []
    specs = TypeInfo.get_module_specs(module)

    for {{f, a}, _line, func_kind, args, doc, metadata} <- docs, doc != false, into: %{} do
      spec = Map.get(specs, {f, a})

      formatted_args =
        (args || [])
        |> Enum.map(&format_doc_arg(&1))

      desc = extract_summary_from_docs(doc)

      {{f, a}, {func_kind, formatted_args, desc, metadata, spec_to_string(spec, :callback)}}
    end
  end

  defp format_doc_arg({:\\, _, [left, right]}) do
    format_doc_arg(left) <> " \\\\ " <> Macro.to_string(right)
  end

  defp format_doc_arg(arg) do
    Macro.to_string(arg)
  end

  def remove_first_macro_arg(
        {:when, info3, [{:"::", info, [{name, info2, rest_args}, ret]}, var_params]}
      ) do
    "MACRO-" <> rest = Atom.to_string(name)

    sub = [{String.to_atom(rest), info2, rest_args |> tl}, ret]

    {:when, info3, [{:"::", info, sub}, var_params]}
  end

  def remove_first_macro_arg({:"::", info, [{name, info2, [_term_arg | rest_args]}, return]}) do
    "MACRO-" <> rest = Atom.to_string(name)
    {:"::", info, [{String.to_atom(rest), info2, rest_args}, return]}
  end

  def spec_to_string(spec, kind \\ :spec)

  def spec_to_string(nil, _kind) do
    ""
  end

  def spec_to_string({{name, arity}, specs}, kind) when is_atom(name) and is_integer(arity) do
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
      "@#{kind} #{binary}"
    end)
  end

  def spec_to_string({{_module, name, arity}, specs}, kind)
      when is_atom(name) and is_integer(arity) do
    # spec with module - transform it to moduleless form
    spec_to_string({{name, arity}, specs}, kind)
  end

  @spec actual_module(
          nil | module,
          ElixirSense.Core.State.Env.t(),
          ElixirSense.Core.State.mods_funs_to_positions_t(),
          boolean
        ) :: {nil | module, boolean}
  def actual_module(module, env, mods_funs, expand_aliases?) do
    {m, nil, res, _} =
      actual_mod_fun(
        {module, nil},
        env,
        mods_funs,
        %{},
        {1, 1},
        expand_aliases?
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
  rescue
    e ->
      Logger.warning(
        "Unable to expand alias #{inspect(mod)}; env aliases #{inspect(aliases)}: #{Exception.format(:error, e, __STACKTRACE__)}"
      )

      mod
  end

  @spec actual_mod_fun(
          {nil | module, nil | atom},
          ElixirSense.Core.State.Env.t(),
          ElixirSense.Core.State.mods_funs_to_positions_t(),
          ElixirSense.Core.State.types_t(),
          {pos_integer, pos_integer},
          boolean
        ) :: {nil | module, nil | atom, boolean, nil | :mod_fun | :type}
  def actual_mod_fun({nil, nil}, _, _, _, _, _), do: {nil, nil, false, nil}

  def actual_mod_fun(
        {mod, fun} = mod_fun,
        %State.Env{} = env,
        mods_funs,
        metadata_types,
        cursor_position,
        expand_aliases?
      ) do
    expanded_mod = if expand_aliases?, do: expand_alias(mod, env.aliases), else: mod

    with {:mod_fun, {nil, nil}} <- {:mod_fun, find_kernel_special_forms_macro(mod_fun)},
         {:mod_fun, {nil, nil}} <-
           {:mod_fun,
            find_function_or_module(
              {expanded_mod, fun},
              env,
              mods_funs,
              cursor_position
            )},
         {:type, {nil, nil}} <-
           {:type, find_type({expanded_mod, fun}, env, metadata_types)} do
      {expanded_mod, fun, false, nil}
    else
      {kind, {m, f}} -> {m, f, true, kind}
    end
  end

  # local type
  defp find_type(
         {nil, type},
         %State.Env{module: current_module, typespec: {_, _}},
         metadata_types
       ) do
    found_in_metadata =
      Enum.any?(metadata_types, fn
        {{^current_module, ^type, _}, _} -> true
        _ -> false
      end)

    if found_in_metadata do
      {current_module, type}
    else
      if BuiltinTypes.builtin_type?(type) do
        {nil, type}
      else
        {nil, nil}
      end
    end
  end

  # Elixir proxy
  defp find_type({Elixir, _type}, _env, _metadata_types), do: {nil, nil}

  # invalid case
  defp find_type({_mod, nil}, _env, _metadata_types), do: {nil, nil}

  # remote type
  defp find_type({mod, type}, %State.Env{typespec: {_, _}}, metadata_types) do
    found =
      Enum.any?(metadata_types, fn
        {{^mod, ^type, _}, %State.TypeInfo{kind: kind}} when kind in [:type, :opaque] -> true
        _ -> false
      end) or
        Typespec.get_types(mod)
        |> Enum.any?(fn {kind, {name, _def, _args}} ->
          name == type and kind in [:type, :opaque]
        end)

    if found do
      {mod, type}
    else
      {nil, nil}
    end
  end

  defp find_type({_mod, _type}, _env, _metadata_types), do: {nil, nil}

  defp find_function_or_module(
         {_mod, fun},
         %State.Env{typespec: {_, _}},
         _mods_funs,
         _cursor_position
       )
       when fun != nil,
       do: {nil, nil}

  # local call
  defp find_function_or_module(
         {nil, fun},
         _env,
         _mods_funs,
         _cursor_position
       )
       when fun in [:module_info, :behaviour_info, :__info__],
       do: {nil, nil}

  defp find_function_or_module(
         {nil, fun},
         %State.Env{
           module: current_module,
           functions: functions,
           macros: macros
         },
         mods_funs,
         cursor_position
       ) do
    found_in_metadata =
      Enum.any?(mods_funs, fn
        {{^current_module, ^fun, _}, info} ->
          State.ModFunInfo.get_category(info) != :macro or
            List.last(info.positions) < cursor_position

        _ ->
          false
      end)

    if found_in_metadata do
      {current_module, fun}
    else
      found_in_imports =
        Enum.find_value(functions ++ macros, fn {module, imported} ->
          if Keyword.has_key?(imported, fun) do
            {module, fun}
          end
        end)

      if found_in_imports do
        found_in_imports
      else
        {nil, nil}
      end
    end
  end

  # Elixir proxy
  defp find_function_or_module(
         {Elixir, _},
         _env,
         _mods_funs,
         _cursor_position
       ),
       do: {nil, nil}

  # module
  defp find_function_or_module(
         {mod, nil},
         _env,
         mods_funs,
         _cursor_position
       ) do
    if Map.has_key?(mods_funs, {mod, nil, nil}) or match?({:module, _}, Code.ensure_loaded(mod)) do
      {mod, nil}
    else
      {nil, nil}
    end
  end

  # remote call
  defp find_function_or_module(
         {mod, fun},
         %State.Env{requires: requires},
         mods_funs,
         _cursor_position
       ) do
    found =
      Enum.any?(mods_funs, fn
        {{^mod, ^fun, _}, info} ->
          is_pub(info.type) and
            (is_function_type(info.type) or mod in requires)

        _ ->
          false
      end) or
        case get_exports(mod) |> Keyword.get(fun) do
          nil -> false
          {_arity, :function} -> true
          {_arity, :macro} -> mod in requires
        end

    if found do
      {mod, fun}
    else
      {nil, nil}
    end
  end

  defp find_kernel_special_forms_macro({nil, fun}) when fun not in [:__info__, :module_info] do
    if exported?(Kernel.SpecialForms, fun) do
      {Kernel.SpecialForms, fun}
    else
      {nil, nil}
    end
  end

  defp find_kernel_special_forms_macro({_mod, _fun}) do
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

  def is_pub(type), do: type in [:def, :defmacro, :defdelegate, :defguard]
  def is_priv(type), do: type in [:defp, :defmacrop, :defguardp]
  def is_function_type(type), do: type in [:def, :defp, :defdelegate]
  def is_macro_type(type), do: type in [:defmacro, :defmacrop, :defguard, :defguardp]

  def expand_import({functions, macros}, module, opts, mods_funs) do
    opts =
      if Keyword.keyword?(opts) do
        opts
      else
        []
      end

    {all_exported_functions, all_exported_macros} =
      if (functions[module] != nil or macros[module] != nil) and Keyword.keyword?(opts[:except]) do
        {functions[module], macros[module]}
      else
        if Map.has_key?(mods_funs, {module, nil, nil}) do
          funs_macros =
            mods_funs
            |> Enum.filter(fn {{m, _f, a}, info} ->
              m == module and a != nil and is_pub(info.type)
            end)
            |> Enum.split_with(fn {_, info} -> is_macro_type(info.type) end)

          functions =
            funs_macros
            |> elem(1)
            |> Enum.flat_map(fn {{_m, f, _a}, info} ->
              for {arity, default_args} <- State.ModFunInfo.get_arities(info),
                  args <- (arity - default_args)..arity do
                {f, args}
              end
            end)

          macros =
            funs_macros
            |> elem(0)
            |> Enum.flat_map(fn {{_m, f, _a}, info} ->
              for {arity, default_args} <- State.ModFunInfo.get_arities(info),
                  args <- (arity - default_args)..arity do
                {f, args}
              end
            end)

          {functions, macros}
        else
          {macros, functions} =
            get_exports(module)
            |> Enum.split_with(fn {_, {_, kind}} -> kind == :macro end)

          {functions |> Enum.map(fn {f, {a, _}} -> {f, a} end),
           macros |> Enum.map(fn {f, {a, _}} -> {f, a} end)}
        end
      end

    imported_functions =
      all_exported_functions
      |> Enum.reject(fn
        {:__info__, 1} ->
          true

        {:module_info, arity} when arity in [0, 1] ->
          true

        {:behaviour_info, 1} ->
          if Version.match?(System.version(), ">= 1.15.0-dev") do
            true
          else
            # elixir < 1.15 imports behaviour_info from erlang behaviours
            # https://github.com/elixir-lang/elixir/commit/4b26edd8c164b46823e1dc1ec34b639cc3563246
            elixir_module?(module)
          end

        {:orelse, 2} ->
          module == :erlang

        {:andalso, 2} ->
          module == :erlang

        {name, arity} ->
          reject_import(name, arity, :function, opts)
      end)

    imported_macros =
      all_exported_macros
      |> Enum.reject(fn
        {name, arity} ->
          reject_import(name, arity, :macro, opts)
      end)

    functions =
      case imported_functions do
        [] ->
          Keyword.delete(functions, module)

        _ ->
          Keyword.put(functions, module, imported_functions)
      end

    macros =
      case imported_macros do
        [] ->
          Keyword.delete(macros, module)

        _ ->
          Keyword.put(macros, module, imported_macros)
      end

    {functions, macros}
  end

  defp reject_import(name, arity, kind, opts) do
    name_string = name |> Atom.to_string()

    rejected_after_only? =
      cond do
        opts[:only] == :sigils and not String.starts_with?(name_string, "sigil_") ->
          true

        opts[:only] == :macros and kind != :macro ->
          true

        opts[:only] == :functions and kind != :function ->
          true

        Keyword.keyword?(opts[:only]) ->
          {name, arity} not in opts[:only]

        String.starts_with?(name_string, "_") ->
          true

        true ->
          false
      end

    if rejected_after_only? do
      true
    else
      if Keyword.keyword?(opts[:except]) do
        {name, arity} in opts[:except]
      else
        false
      end
    end
  end

  def combine_imports({functions, macros}) do
    Enum.reduce(functions, macros, fn {module, imports}, acc ->
      case acc[module] do
        nil -> Keyword.put(acc, module, imports)
        acc_imports -> Keyword.put(acc, module, acc_imports ++ imports)
      end
    end)
  end

  def is_callback(behaviour, fun, arity, metadata) when is_atom(behaviour) do
    metadata_callback =
      metadata.specs
      |> Enum.any?(
        &match?(
          {{^behaviour, ^fun, cb_arity}, %{kind: kind}}
          when kind in [:callback, :macrocallback] and
                 matches_arity?(cb_arity, arity),
          &1
        )
      )

    metadata_callback or
      (Code.ensure_loaded?(behaviour) and
         function_exported?(behaviour, :behaviour_info, 1) and
         behaviour.behaviour_info(:callbacks)
         |> Enum.map(&drop_macro_prefix/1)
         |> Enum.any?(&match?({^fun, cb_arity} when matches_arity?(cb_arity, arity), &1)))
  end

  def get_fun_args_from_doc_or_typespec(mod, f, arity, args, metadata) do
    # as of otp 25 erlang modules do not return args
    # instead they return typespecs in metadata[:signature]
    case metadata[:signature] do
      nil ->
        if args != nil and length(args) == arity do
          # elixir doc
          args
          |> List.wrap()
          |> Enum.map(&format_doc_arg(&1))
        else
          # as of otp 25 erlang callback implementation do not have signature metadata

          behaviour = metadata[:implementing]

          if arity != 0 and behaviour != nil do
            # try to get callback spec
            # we don't expect macros here
            case TypeInfo.get_callback(behaviour, f, arity) do
              {_, [params | _]} ->
                TypeInfo.extract_params(params)

              _ ->
                # provide dummy
                Enum.map(1..arity, fn _ -> "term" end)
            end
          else
            # provide dummy
            if arity == 0, do: [], else: Enum.map(1..arity, fn _ -> "term" end)
          end
        end

      [{:attribute, _, :spec, {{^f, ^arity}, [params | _]}}] ->
        # erlang doc with signature meta - moduleless form
        TypeInfo.extract_params(params)

      [{:attribute, _, :spec, {{^mod, ^f, ^arity}, [params | _]}}] ->
        # erlang doc with signature meta - form with module
        TypeInfo.extract_params(params)
    end
  end
end
