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

  alias ElixirSense.Core.Applications
  alias ElixirSense.Core.BuiltinFunctions
  alias ElixirSense.Core.BuiltinTypes
  alias ElixirSense.Core.EdocReader
  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode
  alias ElixirSense.Core.Normalized.Typespec
  alias ElixirSense.Core.State
  alias ElixirSense.Core.TypeInfo

  @type mod_fun :: {module | nil, atom | nil}
  @type markdown :: String.t()
  @type mod_docs :: %{docs: markdown, types: markdown, callbacks: markdown}
  @type fun_docs :: %{docs: markdown, types: markdown}
  @type docs :: mod_docs | fun_docs

  @no_documentation "No documentation available\n"

  # TODO consider removing this when EEP 48 support lands
  @wrapped_behaviours %{
    :gen_server => GenServer,
    :gen_event => GenEvent,
    :supervisor => Supervisor,
    :application => Application
  }

  @spec get_exports(module) :: [{atom, non_neg_integer}]
  def get_exports(Elixir), do: []

  def get_exports(module) do
    case Code.ensure_loaded(module) do
      {:module, _} ->
        for {f, a} <- module.module_info(:exports) do
          drop_macro_prefix({f, a})
        end
        |> Kernel.++(BuiltinFunctions.erlang_builtin_functions(module))

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

  @spec get_all_docs(mod_fun, ElixirSense.Core.State.scope()) :: docs
  def get_all_docs({mod, nil}, _) do
    %{docs: get_docs_md(mod), types: get_types_md(mod), callbacks: get_callbacks_md(mod)}
  end

  def get_all_docs({mod, fun}, scope) do
    docs =
      with(
        [] <- get_func_docs_md(mod, fun),
        [] <- get_type_docs_md(mod, fun, scope)
      ) do
        @no_documentation
      else
        docs ->
          Enum.join(docs, "\n\n---\n\n") <> "\n"
      end

    %{docs: docs, types: get_types_md(mod)}
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
            # as of otp 23 erlang modules do not return args
            # instead they return typespecs in metadata[:signature]
            fun_args =
              case metadata[:signature] do
                nil ->
                  if args != nil and length(args) == arity do
                    args
                    |> List.wrap()
                    |> Enum.map(&format_doc_arg(&1))
                  else
                    # as of otp 23 erlang callback implementation do not have signature metadata
                    if arity == 0, do: [], else: Enum.map(1..arity, fn _ -> "term" end)
                  end

                [{:attribute, _, :spec, {{^f, ^arity}, [params | _]}}] ->
                  TypeInfo.extract_params(params) |> Enum.map(&Atom.to_string/1)

                [{:attribute, _, :spec, {{^mod, ^f, ^arity}, [params | _]}}] ->
                  TypeInfo.extract_params(params) |> Enum.map(&Atom.to_string/1)
              end

            fun_str = Atom.to_string(fun)
            doc = extract_summary_from_docs(text)

            spec = get_spec_as_string(mod, fun, arity, kind)
            %{name: fun_str, params: fun_args, documentation: doc, spec: spec}
          end

        case results do
          [] ->
            get_spec_from_typespec(mod, fun)

          other ->
            other
        end

      nil ->
        edoc_results =
          EdocReader.get_docs(mod, fun)
          |> Map.new(fn {{:function, ^fun, arity}, _, _, maybe_doc, _} ->
            {arity, EdocReader.extract_docs(maybe_doc) |> extract_summary_from_docs}
          end)

        get_spec_from_typespec(mod, fun, edoc_results)
    end
    |> Enum.sort_by(&length(&1.params))
  end

  defp get_spec_from_typespec(mod, fun, edoc_results \\ %{}) do
    results =
      for {{_name, arity}, [params | _]} = spec <-
            TypeInfo.get_function_specs(mod, fun) do
        params = TypeInfo.extract_params(params) |> Enum.map(&Atom.to_string/1)

        %{
          name: Atom.to_string(fun),
          params: params,
          documentation: edoc_results[arity] || "",
          spec: spec |> spec_to_string
        }
      end

    case results do
      [] ->
        # no typespecs
        # provide dummy spec basing on module_info(:exports)
        get_spec_from_module_info(mod, fun, edoc_results)

      other ->
        other
    end
  end

  defp get_spec_from_module_info(mod, fun, edoc_results) do
    for {f, a} <- get_exports(mod),
        f == fun do
      dummy_params = if a == 0, do: [], else: Enum.map(1..a, fn _ -> "term" end)

      %{
        name: Atom.to_string(fun),
        params: dummy_params,
        documentation: edoc_results[a] || "",
        spec: ""
      }
    end
  end

  def get_func_docs_md(mod, fun)
      when mod != nil and fun in [:module_info, :behaviour_info, :__info__] do
    for {f, a} <- BuiltinFunctions.all(), f == fun do
      spec = BuiltinFunctions.get_specs({f, a})
      args = BuiltinFunctions.get_args({f, a})

      fun_args_text = Enum.join(args, ", ")

      mod_str = inspect(mod)
      fun_str = Atom.to_string(fun)

      spec_text = "### Specs\n\n```\n#{spec |> Enum.join("\n")}\n```\n\n"
      metadata = %{builtin: true}

      "> #{mod_str}.#{fun_str}(#{fun_args_text})\n\n#{get_metadata_md(metadata)}#{spec_text}#{@no_documentation}"
    end
  end

  def get_func_docs_md(mod, fun) do
    case NormalizedCode.get_docs(mod, :docs) do
      nil ->
        edoc_results =
          EdocReader.get_docs(mod, fun)
          |> Map.new(fn {{:function, ^fun, arity}, _, _, maybe_doc, metadata} ->
            {arity, {EdocReader.extract_docs(maybe_doc), metadata}}
          end)

        # no docs, fallback to typespecs
        get_func_docs_md_from_typespec(mod, fun, edoc_results)

      docs ->
        results =
          for {{f, arity}, _, kind, args, text, metadata} <- docs, f == fun do
            # as of otp 23 erlang modules do not return args
            # instead they return typespecs in metadata[:signature]
            fun_args_text =
              case metadata[:signature] do
                nil ->
                  if args != nil and length(args) == arity do
                    args
                    |> List.wrap()
                    |> Enum.map_join(", ", &format_doc_arg(&1))
                    |> String.replace("\\\\", "\\\\\\\\")
                  else
                    # as of otp 23 erlang callback implementation do not have signature metadata
                    if arity == 0, do: "", else: Enum.map_join(1..arity, ", ", fn _ -> "term" end)
                  end

                [{:attribute, _, :spec, {{^f, ^arity}, [params | _]}}] ->
                  TypeInfo.extract_params(params) |> Enum.map_join(", ", &Atom.to_string/1)

                [{:attribute, _, :spec, {{^mod, ^f, ^arity}, [params | _]}}] ->
                  TypeInfo.extract_params(params) |> Enum.map_join(", ", &Atom.to_string/1)
              end

            "> #{inspect(mod)}.#{fun}(#{fun_args_text})\n\n#{get_metadata_md(metadata)}#{get_spec_text(mod, fun, arity, kind)}#{text}"
          end

        case results do
          [] ->
            get_func_docs_md_from_typespec(mod, fun)

          other ->
            other
        end
    end
  end

  defp get_func_docs_md_from_typespec(mod, fun, edoc_results \\ %{}) do
    results =
      for {{_name, arity}, [params | _]} <-
            TypeInfo.get_function_specs(mod, fun) do
        fun_args_text = TypeInfo.extract_params(params) |> Enum.map_join(", ", &Atom.to_string/1)

        {text, metadata} = edoc_results[arity] || {"", %{}}

        "> #{inspect(mod)}.#{fun}(#{fun_args_text})\n\n#{get_metadata_md(metadata)}#{get_spec_text(mod, fun, arity, :function)}#{text || @no_documentation}"
      end

    case results do
      [] ->
        # no docs and no typespecs
        get_func_docs_md_from_module_info(mod, fun, edoc_results)

      other ->
        other
    end
  end

  defp get_func_docs_md_from_module_info(mod, fun, edoc_results) do
    # provide dummy docs basing on module_info(:exports)
    for {f, arity} <- get_exports(mod),
        f == fun do
      fun_args_text =
        if arity == 0, do: "", else: Enum.map_join(1..arity, ", ", fn _ -> "term" end)

      {text, metadata} =
        if {f, arity} in BuiltinFunctions.erlang_builtin_functions(mod) do
          {nil, %{builtin: true}}
        else
          edoc_results[arity] || {"", %{}}
        end

      "> #{inspect(mod)}.#{fun}(#{fun_args_text})\n\n#{get_metadata_md(metadata)}#{get_spec_text(mod, fun, arity, :function)}#{text || @no_documentation}"
    end
  end

  def get_docs_md(mod) when is_atom(mod) do
    mod_str = inspect(mod)

    case NormalizedCode.get_docs(mod, :moduledoc) do
      {_line, doc, metadata} when is_binary(doc) ->
        "> #{mod_str}\n\n" <> get_metadata_md(metadata) <> doc

      _ ->
        case EdocReader.get_moduledoc(mod) do
          [{_line, doc, metadata}] when is_map(doc) ->
            "> #{mod_str}\n\n" <> get_metadata_md(metadata) <> EdocReader.extract_docs(doc)

          _ ->
            if Code.ensure_loaded?(mod) do
              "> #{mod_str}\n\n" <> @no_documentation
            else
              @no_documentation
            end
        end
    end
  end

  def get_metadata_md(metadata) do
    text =
      metadata
      |> Enum.map(&get_metadata_entry_md/1)
      |> Enum.reject(&is_nil/1)
      |> Enum.join("\n")

    case text do
      "" -> ""
      not_empty -> not_empty <> "\n\n"
    end
  end

  # erlang name
  defp get_metadata_entry_md({:name, _text}), do: nil

  # erlang signature
  defp get_metadata_entry_md({:signature, _text}), do: nil

  # erlang edit_url
  defp get_metadata_entry_md({:edit_url, _text}), do: nil

  # erlang :otp_doc_vsn
  defp get_metadata_entry_md({:otp_doc_vsn, _text}), do: nil

  # erlang :source
  defp get_metadata_entry_md({:source, _text}), do: nil

  # erlang :types
  defp get_metadata_entry_md({:types, _text}), do: nil

  # erlang :equiv
  defp get_metadata_entry_md({:equiv, {:function, name, arity}}) do
    "**Equivalent**\n#{name}/#{arity}"
  end

  defp get_metadata_entry_md({:deprecated, text}) do
    "**Deprecated**\n#{text}"
  end

  defp get_metadata_entry_md({:since, text}) do
    "**Since**\n#{text}"
  end

  defp get_metadata_entry_md({:group, text}) do
    "**Group**\n#{text}"
  end

  defp get_metadata_entry_md({:guard, true}) do
    "**Guard**"
  end

  defp get_metadata_entry_md({:builtin, true}) do
    "**Built-in**"
  end

  defp get_metadata_entry_md({:optional, true}) do
    "**Optional**"
  end

  defp get_metadata_entry_md({:optional, false}), do: nil

  defp get_metadata_entry_md({:opaque, true}) do
    "**Opaque**"
  end

  defp get_metadata_entry_md({:defaults, _}), do: nil

  defp get_metadata_entry_md({:delegate_to, {m, f, a}}) do
    "**Delegates to**\n#{inspect(m)}.#{f}/#{a}"
  end

  defp get_metadata_entry_md({_, _}), do: nil

  # no types inside a function
  def get_type_docs_md(_, _, {_f, _a}) do
    []
  end

  # no types outside a module
  def get_type_docs_md(_, _, scope) when scope in [Elixir, nil] do
    []
  end

  def get_type_docs_md(nil, fun, _scope) do
    for info <- BuiltinTypes.get_builtin_type_info(fun) do
      {spec, args} =
        case info do
          %{signature: sig, params: params} ->
            {sig, Enum.map_join(params, ", ", &(&1 |> Atom.to_string()))}

          %{spec: spec_ast, params: params} ->
            {TypeInfo.format_type_spec_ast(spec_ast, :type),
             Enum.map_join(params, ", ", &(&1 |> Atom.to_string()))}

          _ ->
            {"#{fun}()", ""}
        end

      format_type_doc_md({nil, fun}, args, info[:doc], spec, %{builtin: true})
    end
  end

  def get_type_docs_md(mod, fun, _scope) do
    case TypeInfo.get_type_docs(mod, fun) do
      [] ->
        edoc_results =
          EdocReader.get_typedocs(mod, fun)
          |> Map.new(fn {{:type, ^fun, arity}, _, _, maybe_doc, metadata} ->
            {arity, {EdocReader.extract_docs(maybe_doc), metadata}}
          end)

        for {kind, {name, _type, args}} = typedef <- Typespec.get_types(mod),
            name == fun,
            kind in [:type, :opaque] do
          spec = TypeInfo.format_type_spec(typedef)

          type_args = Enum.map_join(args, ", ", &(&1 |> elem(2) |> Atom.to_string()))

          {text, metadata} = edoc_results[length(args)] || {"", %{}}

          format_type_doc_md({mod, fun}, type_args, text || @no_documentation, spec, metadata)
        end

      docs ->
        for {{f, arity}, _, _, text, metadata} <- docs, f == fun do
          spec =
            mod
            |> TypeInfo.get_type_spec(f, arity)

          {_kind, {_name, _def, args}} = spec
          type_args = Enum.map_join(args, ", ", &(&1 |> elem(2) |> Atom.to_string()))

          format_type_doc_md(
            {mod, fun},
            type_args,
            text,
            TypeInfo.format_type_spec(spec),
            metadata
          )
        end
    end
  end

  def get_types_md(mod) when is_atom(mod) do
    for %{type_name: type_name, type_args: type_args, type: type, doc: doc, metadata: metadata} <-
          get_types_with_docs(mod) do
      """
      > #{inspect(mod)}.#{type_name}(#{type_args})

      #{get_metadata_md(metadata)}### Specs
      ```
      #{type}
      ```

      #{doc}
      """
    end
    |> Enum.join("\n\n---\n\n")
  end

  def get_callbacks_md(mod) when is_atom(mod) do
    for %{callback: callback, signature: signature, doc: doc, metadata: metadata} <-
          get_callbacks_with_docs(mod) do
      """
      > #{signature}

      #{get_metadata_md(metadata)}### Specs

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

    edocs =
      if docs == nil do
        EdocReader.get_typedocs(module)
        |> Map.new(fn {{:type, fun, arity}, _, _, maybe_doc, metadata} ->
          {{fun, arity}, {EdocReader.extract_docs(maybe_doc), metadata}}
        end)
      end

    module
    |> Typespec.get_types()
    |> Enum.filter(fn {kind, {_t, _, _args}} -> kind in [:type, :opaque] end)
    |> Enum.map(fn {_, {t, _, args}} = type ->
      {doc, metadata} =
        if edocs != nil do
          edocs[{t, length(args)}] || {"", %{}}
        else
          TypeInfo.get_type_doc_desc(docs, t, length(args))
        end

      type_args = Enum.map_join(args, ", ", &(&1 |> elem(2) |> Atom.to_string()))

      %{
        type_name: t,
        type_args: type_args,
        type: format_type(type),
        doc: doc || @no_documentation,
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

    formated_spec = name_str <> returns_str

    formated_spec |> String.replace("()", "")
  end

  def define_callback?(mod, fun, arity) do
    mod
    |> Typespec.get_callbacks()
    |> Enum.any?(fn {{f, a}, _} ->
      drop_macro_prefix({f, a}) == {fun, arity}
    end)
  end

  def get_returns_from_callback(module, func, arity) do
    @wrapped_behaviours
    |> Map.get(module, module)
    |> get_callback_ast(func, arity)
    |> get_returns_from_spec_ast
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
        # some erlang callbacks have broken docs e.g. :gen_statem.state_name
        {name |> Atom.to_string() |> Macro.underscore() |> String.to_atom(), arity}
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
      kind: kind,
      arity: arity,
      callback: "@#{kind} #{definition}",
      signature: signature,
      doc: doc,
      metadata: metadata |> Map.put(:optional, key in optional_callbacks)
    }
  end

  defp get_callbacks_and_docs(mod) do
    callbacks = Typespec.get_callbacks(mod)

    docs =
      @wrapped_behaviours
      |> Map.get(mod, mod)
      |> NormalizedCode.get_docs(:callback_docs)

    # no fallback here as :docsh as ov v0.7.2 does not seem to support callbacks

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

  def get_module_docs_summary(module) do
    case NormalizedCode.get_docs(module, :moduledoc) do
      {_, doc, metadata} ->
        {extract_summary_from_docs(doc), metadata}

      _ ->
        case EdocReader.get_moduledoc(module) do
          [{_line, doc, metadata}] ->
            doc =
              EdocReader.extract_docs(doc)
              |> extract_summary_from_docs

            {doc, metadata}

          _ ->
            {"", %{}}
        end
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
      iex> ElixirSense.Core.Introspection.get_module_subtype(Elixir)
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

    # |> String.replace(Regex.recompile!(~r/\s+/), " ")
  end

  def extract_fun_args(atom) when atom in [nil, false] do
    []
  end

  def get_spec_as_string(module, function, arity, :macro) do
    TypeInfo.get_spec(module, :"MACRO-#{function}", arity + 1) |> spec_to_string()
  end

  def get_spec_as_string(module, function, arity, :function) do
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

    for {{f, a}, _line, func_kind, args, doc, metadata} <- docs, doc != false, into: %{} do
      spec = Map.get(specs, {f, a})

      formatted_args =
        (args || [])
        |> Enum.map(&format_doc_arg(&1))

      desc = extract_summary_from_docs(doc)

      {{f, a}, {func_kind, formatted_args, desc, metadata, spec_to_string(spec)}}
    end
  end

  def get_callback_ast(module, callback, arity) do
    {{name, _}, [spec | _]} =
      module
      |> Typespec.get_callbacks()
      |> Enum.find(fn {{f, a}, _} ->
        drop_macro_prefix({f, a}) == {callback, arity}
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

    sub = [{String.to_atom(rest), info2, rest_args |> tl}, ret]

    {:when, info3, [{:"::", info, sub}, var_params]}
  end

  def remove_first_macro_arg({:"::", info, [{name, info2, [_term_arg | rest_args]}, return]}) do
    "MACRO-" <> rest = Atom.to_string(name)
    {:"::", info, [{String.to_atom(rest), info2, rest_args}, return]}
  end

  def spec_to_string(nil) do
    ""
  end

  def spec_to_string({{name, arity}, specs}) when is_atom(name) and is_integer(arity) do
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
      "@spec #{binary}" |> String.replace("()", "")
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
        {mod, fun} = mod_fun,
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

  defp has_type?(_mod, _type, _current_module, _metadata_types, false), do: false

  defp has_type?(mod, type, current_module, metadata_types, true) do
    case metadata_types[{mod, type, nil}] do
      nil ->
        Typespec.get_types(mod)
        |> Enum.any?(fn {kind, {name, _def, _args}} ->
          name == type and kind in [:type, :opaque]
        end)

      %State.TypeInfo{kind: kind} ->
        mod == current_module or kind in [:type, :opaque]
    end
  end

  defp find_metadata_function(
         {nil, fun},
         _current_module,
         _imports,
         _mods_funs,
         _metadata_types,
         _include_typespecs
       )
       when fun in [:module_info, :behaviour_info, :__info__],
       do: {nil, nil}

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

    if found_in_metadata or exported?(mod, fun) or
         has_type?(mod, fun, current_module, metadata_types, include_typespecs) do
      {mod, fun}
    else
      {nil, nil}
    end
  end

  defp find_builtin_type({nil, fun}) do
    if BuiltinTypes.builtin_type?(fun) do
      {nil, fun}
    else
      {nil, nil}
    end
  end

  defp find_builtin_type({_mod, _fun}) do
    {nil, nil}
  end

  defp find_kernel_function({nil, fun}) when fun not in [:__info__, :module_info] do
    cond do
      exported?(Kernel, fun) ->
        {Kernel, fun}

      exported?(Kernel.SpecialForms, fun) ->
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

  defp format_type_doc_md({mod, fun}, type_args, doc, spec, metadata) when is_binary(type_args) do
    formatted_spec = "```\n#{spec}\n```"

    mod_formatted =
      case mod do
        nil -> ""
        atom -> inspect(atom) <> "."
      end

    "> #{mod_formatted}#{fun}(#{type_args})\n\n#{get_metadata_md(metadata)}### Specs\n\n#{formatted_spec}\n\n#{doc}"
  end

  def is_pub(type), do: type in [:def, :defmacro, :defdelegate, :defguard]

  @spec get_all_behaviour_implementations(module) :: [module]
  def get_all_behaviour_implementations(behaviour) do
    # this function can take a few seconds
    # unfortunately it does not benefit from conversion to Task.async_stream
    # at least on otp 23

    # TODO consider changing this to :code.all_available when otp 23 is required
    all_loaded =
      :code.all_loaded()
      |> Enum.map(&(&1 |> elem(0)))

    from_apps =
      case :code.get_mode() do
        :interactive ->
          Applications.get_modules_from_applications()

        _ ->
          []
      end

    (all_loaded ++ from_apps)
    |> Enum.uniq()
    |> Enum.filter(fn mod ->
      Code.ensure_loaded?(mod) and
        behaviour in Enum.flat_map(mod.module_info(:attributes), fn
          {:behaviour, behaviours} when is_list(behaviours) ->
            behaviours

          _ ->
            []
        end)
    end)
  end
end
