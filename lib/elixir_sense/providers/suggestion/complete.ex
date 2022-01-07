# This file includes modified code extracted from the elixir project. Namely:
#
# https://github.com/elixir-lang/elixir/blob/v1.1/lib/iex/lib/iex/autocomplete.exs
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

# This module is based on IEx.Autocomplete from version ~ 1.1
# with some changes inspired by Alchemist.Completer (itself based on IEx.Autocomplete).
# Since then the codebases have diverged as the requirements
# put on editor and REPL autocomplete are different.
# However some relevant changes have been merged back
# from upstream Elixir (1.12).
# Changes made to the original version include:
# - different result format with added docs and spec
# - built in and private funcs are not excluded
# - hint generation removed
# - added expansion basing on metadata besides introspection
# - uses custom docs extraction function
# - gets metadata by argument instead of environment variables
#   (original Elixir 1.1) and later GenServer
# - no signature completion as it's handled by signature provider
# - added attribute completion
# - improved completion after %, ^ and & operators

defmodule ElixirSense.Providers.Suggestion.Complete do
  alias ElixirSense.Core.Applications
  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.BuiltinAttributes
  alias ElixirSense.Core.BuiltinFunctions
  alias ElixirSense.Core.EdocReader
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State.AttributeInfo
  alias ElixirSense.Core.State.VarInfo
  alias ElixirSense.Core.Struct
  alias ElixirSense.Core.TypeInfo

  alias ElixirSense.Providers.Suggestion.Matcher

  @erlang_module_builtin_functions [{:module_info, 0}, {:module_info, 1}]
  @elixir_module_builtin_functions [{:__info__, 1}]
  @builtin_functions @erlang_module_builtin_functions ++ @elixir_module_builtin_functions

  @moduledoc false

  defmodule Env do
    @moduledoc false

    @type t :: %ElixirSense.Providers.Suggestion.Complete.Env{
            aliases: [{module, module}],
            imports: [module],
            scope_module: nil | module,
            mods_and_funs: ElixirSense.Core.State.mods_funs_to_positions_t(),
            specs: ElixirSense.Core.State.specs_t(),
            vars: [ElixirSense.Core.State.VarInfo.t()],
            attributes: [ElixirSense.Core.State.AttributeInfo.t()],
            structs: ElixirSense.Core.State.structs_t(),
            types: ElixirSense.Core.State.types_t(),
            scope: ElixirSense.Core.State.scope(),
            behaviours: [module]
          }
    defstruct aliases: [],
              imports: [],
              scope_module: nil,
              mods_and_funs: %{},
              specs: %{},
              vars: [],
              attributes: [],
              structs: %{},
              types: %{},
              scope: Elixir,
              behaviours: []
  end

  @spec complete(String.t(), Env.t()) ::
          [
            ElixirSense.Providers.Suggestion.Reducers.Common.func()
            | ElixirSense.Providers.Suggestion.Reducers.Common.mod()
            | ElixirSense.Providers.Suggestion.Reducers.Common.variable()
            | ElixirSense.Providers.Suggestion.Reducers.Common.attribute()
            | ElixirSense.Providers.Suggestion.Reducers.Struct.field()
          ]
  def complete(hint, %Env{} = env) do
    do_expand(hint |> String.to_charlist(), env)
  end

  def do_expand(code, env) do
    only_structs =
      case code do
        [?% | _] -> true
        _ -> false
      end

    case NormalizedCode.CursorContext.cursor_context(code) do
      {:alias, alias} ->
        expand_aliases(List.to_string(alias), env, false)

      {:unquoted_atom, unquoted_atom} ->
        expand_erlang_modules(List.to_string(unquoted_atom), env)

      {:dot, path, hint} ->
        # TODO kill only_structs?
        expand_dot(path, List.to_string(hint), false, env, only_structs)

      {:dot_arity, path, hint} ->
        expand_dot(path, List.to_string(hint), true, env, only_structs)

      {:dot_call, _path, _hint} ->
        # no need to expand signatures here, we have signatures provider
        # IEx calls
        # expand_dot_call(path, List.to_atom(hint), env)
        # to provide signatures and falls back to expand_local_or_var
        expand_expr(env)

      :expr ->
        # IEx calls expand_local_or_var("", env)
        # we choose to retun more and handle some special cases
        # TODO
        case code do
          [?^] -> expand_var("", env)
          # [?%] -> expand_aliases("", env, true)
          _ -> expand_expr(env)
        end

      {:local_or_var, local_or_var} ->
        # expand_struct_fields_or_local_or_var(code, List.to_string(local_or_var), shell)
        expand_local_or_var(List.to_string(local_or_var), env)

      {:local_arity, local} ->
        expand_local(List.to_string(local), true, env)

      {:local_call, _local} ->
        # TODO check this
        # no need to expand signatures here, we have signatures provider
        # expand_local_call(List.to_atom(local), env)
        # IEx calls
        # expand_local_call(path, List.to_atom(hint), env)
        # to provide signatures and falls back to expand_local_or_var
        expand_expr(env)

      {:operator, operator} ->
        case operator do
          [?^] -> expand_var("", env)
          [?&] -> expand_expr(env)
          _ -> expand_local(List.to_string(operator), false, env)
        end
        # IO.inspect operator
        # expand_local(List.to_string(operator), env)

      {:operator_arity, operator} ->
        expand_local(List.to_string(operator), true, env)

      {:operator_call, _operator} ->
        expand_local_or_var("", env)

      {:sigil, []} ->
        expand_sigil(env)

      {:sigil, [_]} ->
        # {:yes, [], ~w|" """ ' ''' \( / < [ { \||c}
        # we choose to not provide sigil chars
        no()

      {:struct, struct} ->
        # TODO
        expand_aliases(List.to_string(struct), env, true)
        # expand_structs(List.to_string(struct), shell)

      {:module_attribute, attribute} ->
        expand_attribute(List.to_string(attribute), env)

      :none ->
        no()
    end
  end

  defp expand_dot(path, hint, exact?, env, only_structs) do
    filter = struct_module_filter(only_structs, env)

    case expand_dot_path(path, env) do
      {:ok, {:atom, mod}} when hint == "" ->
        expand_aliases(mod, "", [], not only_structs, env, filter)

      {:ok, {:atom, mod}} ->
        expand_require(mod, hint, exact?, env)

      {:ok, {:map, fields, _}} ->
        expand_map_field_access(fields, hint, :map)

      {:ok, {:struct, fields, type, _}} ->
        expand_map_field_access(fields, hint, {:struct, type})

      _ ->
        no()
    end
  end

  defp expand_dot_path({:var, var}, env) do
    value_from_binding({:variable, List.to_atom(var)}, env)
  end

  defp expand_dot_path({:module_attribute, attribute}, env) do
    value_from_binding({:attribute, List.to_atom(attribute)}, env)
  end

  defp expand_dot_path({:alias, var}, env) do
    alias = var |> List.to_string() |> String.split(".") |> value_from_alias(env)

    case alias do
      {:ok, atom} -> {:ok, {:atom, atom}}
      :error -> :error
    end
  end

  defp expand_dot_path({:unquoted_atom, var}, _env) do
    {:ok, {:atom, List.to_atom(var)}}
  end

  defp expand_dot_path({:dot, parent, call}, env) do
    case expand_dot_path(parent, env) do
      {:ok, expanded} ->
        value_from_binding({:call, expanded, List.to_atom(call), []}, env)

      :error ->
        :error
    end
  end

  defp expand_expr(env) do
    local_or_var = expand_local_or_var("", env)
    erlang_modules = expand_erlang_modules("", env)
    elixir_modules = expand_aliases("", env, false)
    attributes = expand_attribute("", env)

    local_or_var ++ erlang_modules ++ elixir_modules ++ attributes
  end

  defp no do
    []
  end

  ## Formatting

  defp format_expansion(entries) do
    Enum.flat_map(entries, &to_entries/1)
  end

  defp expand_map_field_access(fields, hint, type) do
    case match_map_fields(fields, hint, type) do
      [%{kind: :field, name: ^hint, value_is_map: false}] ->
        no()

      map_fields when is_list(map_fields) ->
        format_expansion(map_fields)
    end
  end

  defp expand_require(mod, hint, exact?, env) do
    format_expansion(match_module_funs(mod, hint, exact?, true, env))
  end

  ## Expand local or var

  defp expand_local_or_var(hint, env) do
    format_expansion(match_var(hint, env) ++ match_local(hint, false, env))
  end

  defp expand_local(hint, exact?, env) do
    format_expansion(match_local(hint, exact?, env))
  end

  defp expand_var(hint, env) do
    variables = match_var(hint, env)
    format_expansion(variables)
  end

  defp expand_sigil(env) do
    sigils = match_local("sigil_", false, env)
      |> Enum.map(fn %{name: "sigil_" <> rest} = local -> %{local | name: rest, func_kind: :sigil} end)

    locals = match_local("~", false, env)

    format_expansion(sigils ++ locals)
  end

  defp match_local(hint, exact?, env) do
    match_module_funs(Kernel, hint, exact?, false, env) ++
      match_module_funs(Kernel.SpecialForms, hint, exact?, false, env) ++
      match_module_funs(env.scope_module, hint, exact?, false, env) ++
      (env.imports
       |> Enum.flat_map(fn scope_import -> match_module_funs(scope_import, hint, exact?, false, env) end))
  end

  defp match_var(hint, %Env{vars: vars}) do
    for(
      %VarInfo{name: name} when is_atom(name) <- vars,
      name = Atom.to_string(name),
      Matcher.match?(name, hint),
      do: name
    )
    |> Enum.sort()
    |> Enum.map(&%{kind: :variable, name: &1})
  end

  # do not suggest attributes outside of a module
  defp expand_attribute(_, %Env{scope: scope}) when scope in [Elixir, nil], do: no()

  defp expand_attribute(hint, %Env{attributes: attributes, scope: scope}) do
    attribute_names =
      attributes
      |> Enum.map(fn %AttributeInfo{name: name} -> name end)

    attribute_names =
      case scope do
        {_fun, _arity} ->
          attribute_names

        module when not is_nil(module) ->
          # include module attributes in module scope
          attribute_names ++ BuiltinAttributes.all()
      end

    for(
      attribute_name when is_atom(attribute_name) <- attribute_names,
      name = Atom.to_string(attribute_name),
      Matcher.match?(name, hint),
      do: name
    )
    |> Enum.sort()
    |> Enum.map(&%{kind: :attribute, name: &1})
    |> format_expansion()
  end

  ## Erlang modules

  defp expand_erlang_modules(hint, env) do
    format_expansion(match_erlang_modules(hint, env))
  end

  defp match_erlang_modules(hint, env) do
    for mod <- match_modules(hint, true, env),
        usable_as_unquoted_module?(mod) do
      mod_as_atom = String.to_atom(mod)
      subtype = Introspection.get_module_subtype(mod_as_atom)

      desc =
        if hint != "" do
          Introspection.get_module_docs_summary(mod_as_atom)
        else
          # performance optimization
          # TODO is it still needed on OTP 23+?
          {"", %{}}
        end

      %{kind: :module, name: ":" <> mod, type: :erlang, desc: desc, subtype: subtype}
    end
  end

  defp struct_module_filter(true, env) do
    fn module -> Struct.is_struct(module, env.structs) end
  end

  defp struct_module_filter(false, _) do
    fn _ -> true end
  end

  ## Elixir modules

  defp expand_aliases(all, env, only_structs) do
    filter = struct_module_filter(only_structs, env)

    case String.split(all, ".") do
      [hint] ->
        aliases = match_aliases(hint, env)
        expand_aliases(Elixir, hint, aliases, false, env, filter)

      parts ->
        hint = List.last(parts)
        list = Enum.take(parts, length(parts) - 1)

        case value_from_alias(list, env) do
          {:ok, alias} -> expand_aliases(alias, hint, [], false, env, filter)
          :error -> no()
        end
    end
  end

  # TODO exact?
  defp expand_aliases(mod, hint, aliases, include_funs, env, filter) do
    aliases
    |> Kernel.++(match_elixir_modules(mod, hint, env, filter))
    |> Kernel.++(if include_funs, do: match_module_funs(mod, hint, false, true, env), else: [])
    |> format_expansion()
  end

  defp value_from_alias(mod_parts, env) do
    mod_parts
    |> Enum.map(&String.to_atom/1)
    |> Source.concat_module_parts(env.scope_module, env.aliases)
  end

  defp match_aliases(hint, env) do
    for {alias, _mod} <- env.aliases,
        [name] = Module.split(alias),
        Matcher.match?(name, hint) do
      %{kind: :module, type: :alias, name: name, desc: {"", %{}}, subtype: nil}
    end
  end

  defp match_elixir_modules(module, hint, env, filter) do
    name = Atom.to_string(module)
    depth = length(String.split(name, ".")) + 1
    base = name <> "." <> hint

    for mod <- match_modules(base, module === Elixir, env),
        mod_as_atom = mod |> String.to_atom(),
        filter.(mod_as_atom),
        parts = String.split(mod, "."),
        depth <= length(parts),
        name = Enum.at(parts, depth - 1),
        valid_alias_piece?("." <> name),
        concatted = parts |> Enum.take(depth) |> Module.concat(),
        filter.(concatted) do
      {name, concatted}
    end
    |> Enum.uniq_by(&elem(&1, 1))
    |> Enum.map(fn {name, module} ->
      {desc, meta} = Introspection.get_module_docs_summary(module)
      subtype = Introspection.get_module_subtype(module)
      %{kind: :module, type: :elixir, name: name, desc: {desc, meta}, subtype: subtype}
    end)
  end

  defp valid_alias_piece?(<<?., char, rest::binary>>) when char in ?A..?Z,
    do: valid_alias_rest?(rest)

  defp valid_alias_piece?(_),
    do: false

  defp valid_alias_rest?(<<char, rest::binary>>)
       when char in ?A..?Z
       when char in ?a..?z
       when char in ?0..?9
       when char == ?_,
       do: valid_alias_rest?(rest)

  defp valid_alias_rest?(<<>>),
    do: true

  defp valid_alias_rest?(rest),
    do: valid_alias_piece?(rest)

  ## Helpers

  defp usable_as_unquoted_module?(name) do
    # Conversion to atom is not a problem because
    # it is only called with existing modules names.
    Code.Identifier.classify(String.to_atom(name)) != :other
  end

  defp match_modules(hint, root, env) do
    hint_parts = hint |> String.split(".")
    hint_parts_length = length(hint_parts)
    [hint_suffix | hint_prefix] = hint_parts |> Enum.reverse()

    root
    |> get_modules(env)
    |> Enum.sort()
    |> Enum.dedup()
    |> Enum.filter(fn mod ->
      [mod_suffix | mod_prefix] =
        mod |> String.split(".") |> Enum.take(hint_parts_length) |> Enum.reverse()

      hint_prefix == mod_prefix and Matcher.match?(mod_suffix, hint_suffix)
    end)
  end

  defp get_modules(true, env) do
    ["Elixir.Elixir"] ++ get_modules(false, env)
  end

  defp get_modules(false, env) do
    # TODO consider changing this to :code.all_available when otp 23 is required
    modules = Enum.map(:code.all_loaded(), &Atom.to_string(elem(&1, 0)))

    case :code.get_mode() do
      :interactive -> modules ++ get_modules_from_applications() ++ get_modules_from_metadata(env)
      _otherwise -> modules ++ get_modules_from_metadata(env)
    end
  end

  defp get_modules_from_applications do
    for module <- Applications.get_modules_from_applications() do
      Atom.to_string(module)
    end
  end

  defp get_modules_from_metadata(env) do
    for {{k, nil, nil}, _} <- env.mods_and_funs, do: Atom.to_string(k)
  end

  defp match_module_funs(mod, hint, exact?, include_builtin, env) do
    falist =
      cond do
        env.mods_and_funs |> Map.has_key?({mod, nil, nil}) ->
          get_metadata_module_funs(mod, include_builtin, env)

        match?({:module, _}, ensure_loaded(mod)) ->
          get_module_funs(mod, include_builtin)

        true ->
          []
      end
      |> Enum.sort_by(fn {f, a, _, _, _, _, _} -> {f, -a} end)

    list =
      Enum.reduce(falist, [], fn {f, a, def_a, func_kind, {doc_str, meta}, spec, arg}, acc ->
        doc = {Introspection.extract_summary_from_docs(doc_str), meta}

        case :lists.keyfind(f, 1, acc) do
          {f, aa, def_arities, func_kind, docs, specs, args} ->
            :lists.keyreplace(
              f,
              1,
              acc,
              {f, [a | aa], [def_a | def_arities], func_kind, [doc | docs], [spec | specs],
               [arg | args]}
            )

          false ->
            [{f, [a], [def_a], func_kind, [doc], [spec], [arg]} | acc]
        end
      end)

    for {fun, arities, def_arities, func_kind, docs, specs, args} <- list,
        name = Atom.to_string(fun),
        if(exact?, do: hint == name, else: Matcher.match?(name, hint)) do
      %{
        kind: :function,
        name: name,
        arities: arities,
        def_arities: def_arities,
        module: mod,
        func_kind: func_kind,
        docs: docs,
        specs: specs,
        args: args
      }
    end
    |> Enum.sort_by(& &1.name)
  end

  defp get_metadata_module_funs(mod, include_builtin, env) do
    case env.mods_and_funs[{mod, nil, nil}] do
      nil ->
        []

      _funs ->
        callback_docs_specs = Metadata.get_docs_specs_from_behaviours(env)

        for {{^mod, f, a}, info} <- env.mods_and_funs,
            a != nil,
            (mod == env.scope_module and not include_builtin) or Introspection.is_pub(info.type),
            include_builtin || {f, a} not in @builtin_functions do
          {specs, docs, metadata} =
            case env.specs[{mod, f, a}] do
              nil ->
                Metadata.get_doc_spec_from_behaviours(callback_docs_specs, f, a)

              %ElixirSense.Core.State.SpecInfo{specs: specs} ->
                {specs |> Enum.join("\n"), "", %{}}
            end

          # TODO docs and meta from metadata
          {f, a, a, info.type, {docs, metadata}, specs,
           info.params |> hd |> Enum.map(&Macro.to_string/1)}
        end
    end
  end

  def get_module_funs(mod, include_builtin) do
    docs = NormalizedCode.get_docs(mod, :docs)
    specs = TypeInfo.get_module_specs(mod)

    if docs != nil and function_exported?(mod, :__info__, 1) do
      exports = mod.__info__(:macros) ++ mod.__info__(:functions) ++ special_buildins(mod)

      default_arg_functions = default_arg_functions(docs)

      for {f, a} <- exports do
        {f, new_arity} =
          case default_arg_functions[{f, a}] do
            nil -> {f, a}
            new_arity -> {f, new_arity}
          end

        {func_kind, func_doc} = find_doc({f, new_arity}, docs)

        spec =
          case func_kind do
            :macro -> Map.get(specs, {:"MACRO-#{f}", new_arity + 1})
            :function -> Map.get(specs, {f, new_arity})
            nil -> nil
          end

        doc =
          case func_doc do
            nil ->
              {"", %{}}

            {{_fun, _}, _line, _kind, _args, doc, metadata} ->
              {doc, metadata}
          end

        fun_args = Introspection.extract_fun_args(func_doc)

        # as of Elixir 1.11 some functions/macros, e.g. Kernel.SpecialForms.fn
        # have broken specs in docs
        # in that case we fill a dummy fun_args
        fun_args =
          if length(fun_args) != new_arity do
            format_params(nil, new_arity)
          else
            fun_args
          end

        {f, a, new_arity, func_kind, doc, Introspection.spec_to_string(spec), fun_args}
      end
      |> Kernel.++(
        for {f, a} <- @builtin_functions,
            include_builtin,
            do: {f, a, a, :function, {"", %{}}, nil, nil}
      )
    else
      funs =
        mod.module_info(:exports)
        |> Kernel.--(if include_builtin, do: [], else: @builtin_functions)
        |> Kernel.++(BuiltinFunctions.erlang_builtin_functions(mod))

      edoc_results =
        if docs == nil do
          get_edocs(mod)
        else
          %{}
        end

      for {f, a} <- funs do
        spec = specs[{f, a}]
        spec_str = Introspection.spec_to_string(spec)

        doc_result =
          if docs != nil do
            {_kind, func_doc} = find_doc({f, a}, docs)

            case func_doc do
              nil ->
                nil

              {{_fun, _}, _line, _kind, _args, doc, metadata} ->
                {doc, metadata}
            end
          end

        case f |> Atom.to_string() do
          "MACRO-" <> name ->
            arity = a - 1
            params = format_params(spec, arity)
            {String.to_atom(name), arity, arity, :macro, {"", %{}}, spec_str, params}

          _name ->
            params = format_params(spec, a)

            {f, a, a, :function, doc_result || edoc_results[{f, a}] || {"", %{}}, spec_str,
             params}
        end
      end
    end
  end

  defp format_params({{_name, _arity}, [params | _]}, _arity_1) do
    TypeInfo.extract_params(params)
    |> Enum.map(&Atom.to_string/1)
  end

  defp format_params(nil, 0), do: []

  defp format_params(nil, arity) do
    for _ <- 1..arity, do: "term"
  end

  defp get_edocs(mod) do
    EdocReader.get_docs(mod, :any)
    |> Map.new(fn {{:function, fun, arity}, _, _, maybe_doc, metadata} ->
      doc =
        EdocReader.extract_docs(maybe_doc)
        |> Introspection.extract_summary_from_docs()

      {{fun, arity}, {doc || "", metadata}}
    end)
  end

  defp special_buildins(mod) do
    mod.module_info(:exports)
    |> Enum.filter(fn {f, a} ->
      {f, a} in [{:behaviour_info, 1}]
    end)
  end

  def find_doc(fun, _docs) when fun in @builtin_functions, do: {:function, nil}

  def find_doc(fun, docs) do
    doc =
      docs
      |> Enum.find(&match?({^fun, _, _, _, _, _}, &1))

    case doc do
      nil -> {nil, nil}
      {_, _, func_kind, _, _, _} = d -> {func_kind, d}
    end
  end

  defp default_arg_functions(docs) do
    for {{fun_name, arity}, _, _kind, args, _, _} <- docs,
        count = Introspection.count_defaults(args),
        count > 0,
        new_arity <- (arity - count)..(arity - 1),
        into: %{},
        do: {{fun_name, new_arity}, arity}
  end

  defp ensure_loaded(Elixir), do: {:error, :nofile}
  defp ensure_loaded(mod), do: Code.ensure_compiled(mod)

  defp match_map_fields(fields, hint, type) do
    for {key, value} when is_atom(key) <- fields,
        key = Atom.to_string(key),
        Matcher.match?(key, hint) do
      value_is_map =
        case value do
          {:map, _, _} -> true
          {:struct, _, _, _} -> true
          _ -> false
        end

      {subtype, origin} =
        case type do
          {:struct, mod} -> {:struct_field, if(mod, do: inspect(mod))}
          :map -> {:map_key, nil}
        end

      %{kind: :field, name: key, subtype: subtype, value_is_map: value_is_map, origin: origin}
    end
    |> Enum.sort_by(& &1.name)
  end

  ## Ad-hoc conversions
  @spec to_entries(map) ::
          [
            ElixirSense.Providers.Suggestion.Reducers.Common.mod()
            | ElixirSense.Providers.Suggestion.Reducers.Common.func()
            | ElixirSense.Providers.Suggestion.Reducers.Common.variable()
            | ElixirSense.Providers.Suggestion.Reducers.Struct.field()
            | ElixirSense.Providers.Suggestion.Reducers.Common.attribute()
          ]
  defp to_entries(%{kind: :field, subtype: subtype, name: name, origin: origin}) do
    [%{type: :field, name: name, subtype: subtype, origin: origin, call?: true}]
  end

  defp to_entries(%{kind: :module, name: name, desc: {desc, metadata}, subtype: subtype}) do
    [%{type: :module, name: name, subtype: subtype, summary: desc, metadata: metadata}]
  end

  defp to_entries(%{kind: :variable, name: name}) do
    [%{type: :variable, name: name}]
  end

  defp to_entries(%{kind: :attribute, name: name}) do
    [%{type: :attribute, name: "@" <> name}]
  end

  defp to_entries(%{
         kind: :function,
         name: name,
         arities: arities,
         def_arities: def_arities,
         module: mod,
         func_kind: func_kind,
         docs: docs,
         specs: specs,
         args: args
       }) do
    for e <- Enum.zip([arities, docs, specs, args, def_arities]),
        {a, {doc, metadata}, spec, args, def_arity} = e do
      kind =
        case func_kind do
          k when k in [:macro, :defmacro, :defmacrop, :defguard, :defguardp] -> :macro
          _ -> :function
        end

      visibility =
        if func_kind in [:defp, :defmacrop, :defguardp] do
          :private
        else
          :public
        end

      mod_name = inspect(mod)

      fa = {name |> String.to_atom(), a}

      if fa in BuiltinFunctions.all() do
        args = BuiltinFunctions.get_args(fa)

        %{
          type: kind,
          visibility: visibility,
          name: name,
          arity: a,
          def_arity: def_arity,
          args: args |> Enum.join(", "),
          args_list: args,
          origin: mod_name,
          summary: "Built-in function",
          metadata: %{builtin: true},
          spec: BuiltinFunctions.get_specs(fa) |> Enum.join("\n"),
          snippet: nil
        }
      else
        %{
          type: kind,
          visibility: visibility,
          name: name,
          arity: a,
          def_arity: def_arity,
          args: args |> Enum.join(", "),
          args_list: args,
          origin: mod_name,
          summary: doc,
          metadata: metadata,
          spec: spec || "",
          snippet: nil
        }
      end
    end
  end

  defp value_from_binding(binding_ast, env) do
    case Binding.expand(
           %Binding{
             variables: env.vars,
             attributes: env.attributes,
             structs: env.structs,
             imports: env.imports,
             specs: env.specs,
             current_module: env.scope_module,
             types: env.types,
             mods_and_funs: env.mods_and_funs
           },
           binding_ast
         ) do
      :none -> :error
      nil -> :error
      other -> {:ok, other}
    end
  end
end
