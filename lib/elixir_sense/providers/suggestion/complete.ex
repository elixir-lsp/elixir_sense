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
# from upstream Elixir (1.10).
# Changes made to the original version include:
# - different result format with added docs and spec
# - built in and private funcs are not excluded
# - hint generation removed
# - added expansion basing on metadata besides introspection
# - uses custom docs extraction function
# - gets metadata by argument instead of environment variables
# (original Elixir 1.1) and later GenServer

defmodule ElixirSense.Providers.Suggestion.Complete do
  alias ElixirSense.Core.Applications
  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.BuiltinAttributes
  alias ElixirSense.Core.BuiltinFunctions
  alias ElixirSense.Core.EdocReader
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.MetadataBuilder
  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State
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
    do_expand(hint, hint |> String.to_charlist(), env)
  end

  def do_expand(_hint, code, env) do
    only_struct = case code do
      [?% | _] -> true
      _ -> false
    end

    case NormalizedCode.CursorContext.cursor_context(code) |> IO.inspect(label: "context for #{code}") do
      {:alias, alias} ->
        expand_aliases(List.to_string(alias), env, false)
      {:unquoted_atom, unquoted_atom} ->
        expand_erlang_modules(List.to_string(unquoted_atom), env)
      {:dot, path, hint} ->
        expand_dot(path, List.to_string(hint), env, only_struct)
      {:dot_arity, path, hint} ->
        expand_dot(path, List.to_string(hint), env, only_struct)
      {:dot_call, _path, _hint} ->
        # no need to expand signatures here, we have signatures provider
        # expand_dot_call(path, List.to_atom(hint), env)
        # TODO expand_local_or_var?
        no()
      :expr ->
        case code do
          [?&] -> expand_expr("", env)
          [?^] -> expand_variable("", env)
          [?%] -> expand_struct_modules([], "", env)
          _ ->
            # TODO expand_expr?
            expand_local_or_var("", env)
        end
      {:local_or_var, local_or_var} ->
        expand_local_or_var(List.to_string(local_or_var), env)
      {:local_arity, local} ->
        expand_local(List.to_string(local), env)
      {:local_call, _local} ->
        # no need to expand signatures here, we have signatures provider
        # expand_local_call(List.to_atom(local), env)
        # expand_dot_call(path, List.to_atom(hint), env)
        # TODO expand_local_or_var?
        no()
      {:module_attribute, attribute} ->
        expand_attribute(List.to_string(attribute), env)
      other ->
        IO.inspect(other)
        no()

      # _ -> expand(code |> Enum.reverse, env)
    end
  end

  defp expand('', env) do
    expand_expr("", env)
  end

  # credo:disable-for-lines:35
  defp expand([h | t] = expr, env) do
    cond do
      h === ?. and t != [] ->
        expand_dot(reduce(t), Enum.reverse(expr), env, false)

      h === ?: and t == [] ->
        # we are expanding all erlang modules
        # for performance reasons we do not extract edocs
        expand_erlang_modules(env, false)

      h === ?@ and t == [] ->
        expand_attribute("", env)

      h === ?^ and t == [] ->
        expand_variable("", env)

      identifier?(h) ->
        expand_expr(reduce(expr), env)

      h == ?/ and t != [] and identifier?(hd(t)) ->
        expand_expr(reduce(t), env)

      h in '([{' ->
        expand('', env)

      h === ?% and t == [] ->
        expand_struct_modules([], "", env)

      h === ?& and t == [] ->
        expand_expr("", env)

      true ->
        no()
    end
  end

  defp identifier?(h) do
    h in ?a..?z or h in ?A..?Z or h in ?0..?9 or h in [?_, ??, ?!]
  end

  defp expand_dot(path, hint, env, only_structs) do
    filter = if only_structs do
      IO.puts "only structs"
      fn
        module -> Struct.is_struct(module, env.structs) |> IO.inspect(label: "module #{module} is struct")
      end
    else
      fn _ -> true end
    end
    case expand_dot_path(path, env) |> IO.inspect(label: "expand_dot_path") do
      {:ok, mod} when is_atom(mod) and hint == "" ->
        mod |> IO.inspect(label: "empty hint expand_aliases")
        expand_aliases(mod, "", [], not only_structs, env, filter)
      {:ok, mod} when is_atom(mod) -> expand_require(mod, hint, env) |> IO.inspect(label: "require")
      {:ok, {:map, fields, _}} -> expand_map_field_access(fields, hint, :map)
      {:ok, {:struct, fields, type, _}} -> expand_map_field_access(fields, hint, {:struct, type})
      _ -> no()
    end
  end

  # defp expand_dot(expr, full_exp, env) do
  #   case Code.string_to_quoted(expr) do
  #     {:ok, atom} when is_atom(atom) ->
  #       expand_call(atom, "", env)

  #     {:ok, {:__aliases__, _, list}} ->
  #       if full_exp |> hd == ?% do
  #         expand_struct_modules(list, "", env)
  #       else
  #         expand_elixir_modules(list, "", env)
  #       end

  #     {:ok, {_, _, _} = ast_node} ->
  #       expand_call(ast_node, "", env)

  #     _ ->
  #       no()
  #   end
  # end

  defp expand_dot_path({:var, var}, env) do
    value_from_binding({List.to_atom(var), [], nil}, env) |> IO.inspect(label: "var")
  end

  defp expand_dot_path({:module_attribute, attribute}, env) do
    value_from_binding({:@, [], [{List.to_atom(attribute), [], nil}]}, env) |> IO.inspect(label: "module_attribute")
  end

  defp expand_dot_path({:alias, var}, env) do
    var |> List.to_string() |> String.split(".") |> value_from_alias(env) |> IO.inspect(label: "alias")
  end

  defp expand_dot_path({:unquoted_atom, var}, _env) do
    {:ok, List.to_atom(var)} |> IO.inspect(label: "unquoted_atom")
  end

  defp expand_dot_path({:dot, parent, call}, env) do
    IO.inspect {:dot, parent, call}
    e = expand_dot_path(parent, env) |> IO.inspect(label: "expand_dot for #{inspect({:dot, parent, call})}")

    case e do
      {:ok, atom} when is_atom(atom) ->
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
          {:call, {:atom, atom}, List.to_atom(call), []}
        ) |> IO.inspect(label: "bind") do
          :none -> :error
          nil -> :error
          {:atom, a} -> {:ok, a}
          other -> {:ok, other}
        end
      {:ok, x} ->
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
          {:call, x, List.to_atom(call), []}
        ) |> IO.inspect(label: "bind") do
          :none -> :error
          nil -> :error
          {:atom, a} -> {:ok, a}
          other -> {:ok, other}
        end
      :error -> :error
    end
    

    # r = case expand_dot_path(parent, env) do
    #   # {:ok, %{} = map} -> Map.fetch(map, List.to_atom(call))
    #   # TODO {:atom, at} ?
    #   {:ok, {:map, fields, _}} ->
    #     Keyword.fetch(fields, List.to_atom(call))
    #   {:ok, {:struct, fields, type, _}} ->
    #     Keyword.fetch(fields, List.to_atom(call))
    #   o ->
    #     IO.inspect(o, label: "other")
    #     :error
    # end

    # case r do
    #   {:ok, {:atom, a}} -> {:ok, a}
    #   o -> o
    # end
    # |> IO.inspect(label: "dot")
  end

  defp expand_expr("", env) do
    variable_or_import = expand_variable_or_import("", env)
    # we are expanding all erlang modules
    # for performance reasons we do not extract edocs
    erlang_modules = expand_erlang_modules("", env)
    elixir_modules = expand_elixir_modules([], "", env)

    attributes = expand_attribute("", env)

    variable_or_import ++ erlang_modules ++ elixir_modules ++ attributes
  end

  defp expand_expr(expr, env) do
    case Code.string_to_quoted(expr) do
      {:ok, atom} when is_atom(atom) ->
        expand_erlang_modules(Atom.to_string(atom), env)

      {:ok, {atom, _, nil}} when is_atom(atom) ->
        expand_variable_or_import(Atom.to_string(atom), env)

      {:ok, {:__aliases__, _, [root]}} ->
        expand_elixir_modules([], Atom.to_string(root), env)

      {:ok, {:__aliases__, _, [h | _] = list}} when is_atom(h) ->
        hint = Atom.to_string(List.last(list))
        list = Enum.take(list, length(list) - 1)
        expand_elixir_modules(list, hint, env)

      {:ok, {{:., _, [ast_node, fun]}, _, []}} when is_atom(fun) ->
        expand_call(ast_node, Atom.to_string(fun), env)

      {:ok, {:@, _, [{atom, _, nil}]}} when is_atom(atom) ->
        expand_attribute(Atom.to_string(atom), env)

      _ ->
        no()
    end
  end

  defp expand_struct_modules(list, hint, env) do
    expand_elixir_modules(
      list,
      hint,
      env,
      fn
        module -> Struct.is_struct(module, env.structs)
      end,
      false
    )
  end

  defp reduce(expr) do
    Enum.reduce(' ([{', expr, fn token, acc ->
      hd(:string.tokens(acc, [token]))
    end)
    |> Enum.reverse()
    |> trim_leading(?&)
    |> trim_leading(?%)
    |> trim_leading(?!)
    |> trim_leading(?^)
  end

  defp trim_leading([char | rest], char),
    do: rest

  defp trim_leading(expr, _char),
    do: expr

  defp no do
    []
  end

  ## Formatting

  defp format_expansion(entries) do
    Enum.flat_map(entries, &to_entries/1)
  end

  ## Expand calls

  # :atom.fun
  defp expand_call(mod, hint, env) when is_atom(mod) do
    expand_require(mod, hint, env)
  end

  # Elixir.fun
  defp expand_call({:__aliases__, _, list}, hint, env) do
    case value_from_alias(list, env) do
      {:ok, alias} -> expand_require(alias, hint, env)
      :error -> no()
    end
  end

  # variable.fun_or_key
  # @attribute.fun_or_key
  defp expand_call(
         {_, _, _} = ast_node,
         hint,
         %Env{
           aliases: aliases,
           vars: vars,
           attributes: attributes,
           structs: structs,
           imports: imports,
           specs: specs,
           scope_module: scope_module,
           types: types,
           mods_and_funs: mods_and_funs
         } = env
       ) do
    # need to init namespace and aliases here so expansion work as expected
    namespace =
      case scope_module do
        m when m in [nil, Elixir] ->
          [[Elixir]]

        _ ->
          split =
            Module.split(scope_module)
            |> Enum.reverse()
            |> Enum.map(&String.to_atom/1)
            |> Kernel.++([Elixir])

          [split, [Elixir]]
      end

    binding_type =
      MetadataBuilder.get_binding_type(
        %State{
          namespace: namespace,
          aliases: [aliases]
        },
        ast_node
      )

    value_from_binding =
      Binding.expand(
        %Binding{
          variables: vars,
          attributes: attributes,
          structs: structs,
          imports: imports,
          specs: specs,
          current_module: scope_module,
          types: types,
          mods_and_funs: mods_and_funs
        },
        binding_type |> IO.inspect
      )

    case value_from_binding do
      {:atom, mod} -> expand_call(mod, hint, env)
      {:map, fields, _} -> expand_map_field_access(fields, hint, :map)
      {:struct, fields, type, _} -> expand_map_field_access(fields, hint, {:struct, type})
      _otherwise -> no()
    end
  end

  defp expand_call(_, _, _) do
    no()
  end

  defp expand_map_field_access(fields, hint, type) do
    case match_map_fields(fields, hint, type) do
      [%{kind: :field, name: ^hint, value_is_map: false}] ->
        no()

      map_fields when is_list(map_fields) ->
        format_expansion(map_fields)
    end
  end

  defp expand_require(mod, hint, env) do
    format_expansion(match_module_funs(mod, hint, true, env))
  end

  ## Expand local or var

  defp expand_local_or_var(hint, env) do
    format_expansion(match_var(hint, env) ++ match_local(hint, env))
  end

  defp expand_local(hint, env) do
    format_expansion(match_local(hint, env))
  end

  defp match_local(hint, env) do
    match_module_funs(Kernel, hint, false, env) ++
        match_module_funs(Kernel.SpecialForms, hint, false, env) ++
        match_module_funs(env.scope_module, hint, false, env) ++
        (env.imports
         |> Enum.flat_map(fn scope_import -> match_module_funs(scope_import, hint, false, env) end))
  end

  defp match_var(hint, %Env{vars: vars}) do
    for(
      %VarInfo{name: name} when is_atom(name) <- vars,
      name = Atom.to_string(name),
      String.starts_with?(name |> IO.inspect(label: "var name"), hint |> IO.inspect(label: "hint")),
      do: name
    )
    |> Enum.sort()
    |> Enum.map(&%{kind: :variable, name: &1})
    |> IO.inspect(label: "match_var")
  end

  defp expand_variable_or_import(hint, env) do
    variables = do_expand_variable(hint, env)
    # import calls of builtin functions are not possible
    funs =
      match_module_funs(Kernel, hint, false, env) ++
        match_module_funs(Kernel.SpecialForms, hint, false, env) ++
        match_module_funs(env.scope_module, hint, false, env) ++
        (env.imports
         |> Enum.flat_map(fn scope_import -> match_module_funs(scope_import, hint, false, env) end))

    format_expansion(variables ++ funs)
  end

  defp expand_variable(hint, env) do
    variables = do_expand_variable(hint, env)
    format_expansion(variables)
  end

  defp do_expand_variable(hint, %Env{vars: vars}) do
    for(
      %VarInfo{name: name} when is_atom(name) <- vars,
      name = Atom.to_string(name),
      String.starts_with?(name, hint),
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
      String.starts_with?(name, hint),
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

  ## Elixir modules

  defp expand_elixir_modules(list, hint, env, filter \\ fn _ -> true end, include_funs \\ true)

  defp expand_elixir_modules([], hint, env, filter, include_funs) do
    expand_aliases(
      Elixir,
      hint,
      match_aliases(hint, env),
      include_funs,
      env,
      filter
    )
  end

  defp expand_elixir_modules(list, hint, env, filter, include_funs) do
    case value_from_alias(list, env) do
      {:ok, alias} ->
        expand_aliases(alias, hint, [], include_funs, env, filter)

      :error ->
        no()
    end
  end

  defp expand_aliases(all, env, only_structs) do
    IO.inspect {all, only_structs}
    filter = if only_structs do
      fn
        module -> Struct.is_struct(module, env.structs)
      end
    else
      fn _ -> true end
    end

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

  defp expand_aliases(mod, hint, aliases, include_funs, env, filter) do
    aliases
    |> Kernel.++(match_elixir_modules(mod, hint, env, filter))
    # TODO get_module_funs
    |> Kernel.++(if include_funs, do: match_module_funs(mod, hint, true, env), else: [])
    |> format_expansion()
  end

  defp value_from_alias(mod_parts, env) do
    # TODO remove
    mod_parts = case mod_parts do
      [p | _] when is_binary(p) -> mod_parts |> Enum.map(&String.to_atom/1)
      o -> o
    end
    Source.concat_module_parts(mod_parts, env.scope_module, env.aliases)
  end

  defp match_aliases(hint, env) do
    for {alias, _mod} <- env.aliases,
        [name] = Module.split(alias),
        String.starts_with?(name, hint) do
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
    root
    |> get_modules(env)
    |> Enum.sort()
    |> Enum.dedup()
    |> Enum.drop_while(&(not String.starts_with?(&1, hint)))
    |> Enum.take_while(&String.starts_with?(&1, hint))
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

  defp match_module_funs(mod, hint, include_builtin, env) do
    IO.inspect({mod, hint}, label: "match mod funs")
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
        Matcher.match?(name, hint) do
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
            include_builtin || not ({f, a} in @builtin_functions) do
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
        String.starts_with?(key, hint) do
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


  defp value_from_binding(ast_node, env) do
    # with {var, map_key_path} <- extract_from_ast(ast_node, []) do
      IO.inspect ast_node, label: "ast_node"
      # IO.inspect var, label: "var"
      # IO.inspect map_key_path, label: "map_key_path"
      # IEx.Evaluator.value_from_binding(evaluator, server, var, map_key_path)

      # need to init namespace and aliases here so expansion work as expected
      namespace =
        case env.scope_module do
          m when m in [nil, Elixir] ->
            [[Elixir]]

          _ ->
            split =
              Module.split(env.scope_module)
              |> Enum.reverse()
              |> Enum.map(&String.to_atom/1)
              |> Kernel.++([Elixir])

            [split, [Elixir]]
        end

      binding_type =
        MetadataBuilder.get_binding_type(
          %State{
            namespace: namespace,
            aliases: [env.aliases]
          },
          ast_node
        )

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
        binding_type
      ) do
        :none -> :error
        nil -> :error
        {:atom, a} -> {:ok, a}
        other -> {:ok, other}
      end
    # else
    #   _ -> :error
    # end
  end

  # defp extract_from_ast(var_name, acc) when is_atom(var_name) do
  #   {var_name, acc}
  # end

  # defp extract_from_ast({var_name, _, nil}, acc) when is_atom(var_name) do
  #   {var_name, acc}
  # end

  # defp extract_from_ast({{:., _, [ast_node, fun]}, _, []}, acc) when is_atom(fun) do
  #   extract_from_ast(ast_node, [fun | acc])
  # end

  # defp extract_from_ast(_ast_node, _acc) do
  #   :error
  # end
end
