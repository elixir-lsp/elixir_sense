defmodule Alchemist.Helpers.Complete do
  alias ElixirSense.Core.BuiltinFunctions
  alias ElixirSense.Core.EdocReader
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.TypeInfo

  @erlang_module_builtin_functions [{:module_info, 0}, {:module_info, 1}]
  @elixir_module_builtin_functions [{:__info__, 1}]
  @builtin_functions @erlang_module_builtin_functions ++ @elixir_module_builtin_functions

  @moduledoc false

  # This module is based on Alchemist.Completer which in
  # turn was originally based on Elixir IEx.Autocomplete taken
  # from version ~ 1.1
  # Since then the codebases have diverged as the requirements
  # put on editor and REPL autocomplete are different.
  # However some relevant changes have been merged back
  # from upstream Elixir (1.9).
  # Changes made to the original version include:
  # - different result format with added docs and spec
  # - built in funcss are not excluded
  # - recursive evalutor `run/2` added on top of `expand/2`
  # - added expansion basing on metadata besides introspection
  # - uses custom docs extraction function
  # - gets metadata by argument instead of environment variables
  # (original Elixir 1.1) and later GenServer

  defmodule Env do
    @moduledoc false

    @type t :: %Alchemist.Helpers.Complete.Env{
            aliases: [{module, module}],
            imports: [module],
            scope_module: nil | module,
            mods_and_funs: ElixirSense.Core.State.mods_funs_to_positions_t(),
            specs: ElixirSense.Core.State.specs_t()
          }
    defstruct aliases: [],
              imports: [],
              scope_module: nil,
              mods_and_funs: %{},
              specs: %{}
  end

  def run(exp, %Alchemist.Helpers.Complete.Env{} = env) do
    code =
      case is_bitstring(exp) do
        true -> exp |> String.to_charlist()
        _ -> exp
      end

    {status, result, list} = expand(code |> Enum.reverse(), env)

    case {status, result, list} do
      {:no, _, _} ->
        {%{type: :hint, value: "#{exp}"}, []}

      {:yes, [], _} ->
        {%{type: :hint, value: "#{exp}"}, list}

      {:yes, _, []} ->
        run(code ++ result, env)

      {:yes, _, r} ->
        case r do
          [%{subtype: :struct}] -> {%{type: :hint, value: "#{exp}#{result}"}, list}
          # TODO maybe run recursively in some cases
          [_] -> {%{type: :hint, value: "#{exp}#{result}"}, list}
          [_ | _] -> run(code ++ result, env)
        end
    end
  end

  def expand('', env) do
    expand_import("", env)
  end

  def expand([h | t] = expr, env) do
    cond do
      h === ?. and t != [] ->
        expand_dot(reduce(t), env)

      h === ?: and t == [] ->
        # we are expanding all erlang modules
        # for performance reasons we do not extract edocs
        expand_erlang_modules(env, false)

      identifier?(h) ->
        expand_expr(reduce(expr), env)

      h == ?/ and t != [] and identifier?(hd(t)) ->
        expand_expr(reduce(t), env)

      h in '([{' ->
        expand('', env)

      true ->
        no()
    end
  end

  defp identifier?(h) do
    h in ?a..?z or h in ?A..?Z or h in ?0..?9 or h in [?_, ??, ?!]
  end

  defp expand_dot(expr, env) do
    case Code.string_to_quoted(expr) do
      {:ok, atom} when is_atom(atom) ->
        expand_call(atom, "", env)

      {:ok, {:__aliases__, _, list}} ->
        expand_elixir_modules(list, "", env)

      {:ok, {_, _, _} = ast_node} ->
        expand_call(ast_node, "", env)

      _ ->
        no()
    end
  end

  defp expand_expr(expr, env) do
    case Code.string_to_quoted(expr) do
      {:ok, atom} when is_atom(atom) ->
        expand_erlang_modules(Atom.to_string(atom), env, true)

      {:ok, {atom, _, nil}} when is_atom(atom) ->
        expand_import(Atom.to_string(atom), env)

      {:ok, {:__aliases__, _, [root]}} ->
        expand_elixir_modules([], Atom.to_string(root), env)

      {:ok, {:__aliases__, _, [h | _] = list}} when is_atom(h) ->
        hint = Atom.to_string(List.last(list))
        list = Enum.take(list, length(list) - 1)
        expand_elixir_modules(list, hint, env)

      {:ok, {{:., _, [ast_node, fun]}, _, []}} when is_atom(fun) ->
        expand_call(ast_node, Atom.to_string(fun), env)

      _ ->
        no()
    end
  end

  defp reduce(expr) do
    Enum.reduce(' ([{', expr, fn token, acc ->
      hd(:string.tokens(acc, [token]))
    end)
    |> Enum.reverse()
    |> trim_leading(?&)
    |> trim_leading(?%)
  end

  defp trim_leading([char | rest], char),
    do: rest

  defp trim_leading(expr, _char),
    do: expr

  defp yes(hint, entries) do
    {:yes, String.to_charlist(hint), entries}
  end

  defp no do
    {:no, '', []}
  end

  ## Formatting

  defp format_expansion([], _) do
    no()
  end

  defp format_expansion([uniq], hint) do
    case to_hint(uniq, hint) do
      "" -> yes("", to_uniq_entries(uniq))
      hint -> yes(hint, to_uniq_entries(uniq))
    end
  end

  defp format_expansion([first | _] = entries, hint) do
    binary = Enum.map(entries, & &1.name)
    length = byte_size(hint)
    prefix = :binary.longest_common_prefix(binary)

    if prefix in [0, length] do
      yes("", Enum.flat_map(entries, &to_entries/1))
    else
      yes(binary_part(first.name, prefix, length - prefix), [])
    end
  end

  ## Expand calls

  # :atom.fun
  defp expand_call(mod, hint, env) when is_atom(mod) do
    expand_require(mod, hint, env)
  end

  # Elixir.fun
  defp expand_call({:__aliases__, _, list}, hint, env) do
    case expand_alias(list, env) do
      {:ok, alias} -> expand_require(alias, hint, env)
      :error -> no()
    end
  end

  # variable.fun_or_key
  defp expand_call({_, _, _} = _ast_node, _hint, _env) do
    # FIXME not supported
    no()
    # case value_from_binding(ast_node, server) do
    #   {:ok, mod} when is_atom(mod) -> expand_call(mod, hint, server)
    #   {:ok, map} when is_map(map) -> expand_map_field_access(map, hint)
    #   _otherwise -> no()
    # end
  end

  defp expand_call(_, _, _) do
    no()
  end

  defp expand_require(mod, hint, env) do
    format_expansion(match_module_funs(mod, hint, true, env), hint)
  end

  defp expand_import(hint, env) do
    # import calls of buildin functions are not possible
    funs =
      match_module_funs(Kernel, hint, false, env) ++
        match_module_funs(Kernel.SpecialForms, hint, false, env) ++
        match_module_funs(env.scope_module, hint, false, env) ++
        (env.imports
         |> Enum.flat_map(fn scope_import -> match_module_funs(scope_import, hint, false, env) end))

    format_expansion(funs, hint)
  end

  ## Erlang modules

  defp expand_erlang_modules(hint \\ "", env, provide_edocs?) do
    format_expansion(match_erlang_modules(hint, env, provide_edocs?), hint)
  end

  defp match_erlang_modules(hint, env, provide_edocs?) do
    for mod <- match_modules(hint, true, env), usable_as_unquoted_module?(mod) do
      mod_as_atom = String.to_atom(mod)
      subtype = Introspection.get_module_subtype(mod_as_atom)

      desc =
        if provide_edocs? do
          Introspection.get_module_docs_summary(mod_as_atom)
        else
          ""
        end

      %{kind: :module, name: mod, type: :erlang, desc: desc, subtype: subtype}
    end
  end

  ## Elixir modules

  defp expand_elixir_modules([], hint, env) do
    expand_elixir_modules_from_aliases(Elixir, hint, match_aliases(hint, env), env)
  end

  defp expand_elixir_modules(list, hint, env) do
    case expand_alias(list, env) do
      {:ok, alias} -> expand_elixir_modules_from_aliases(alias, hint, [], env)
      :error -> no()
    end
  end

  defp expand_elixir_modules_from_aliases(mod, hint, aliases, env) do
    aliases
    |> Kernel.++(match_elixir_modules(mod, hint, env))
    |> Kernel.++(match_module_funs(mod, hint, true, env))
    |> format_expansion(hint)
  end

  defp expand_alias(mod_parts, env) do
    Source.concat_module_parts(mod_parts, env.scope_module, env.aliases)
  end

  defp match_aliases(hint, env) do
    for {alias, _mod} <- env.aliases,
        [name] = Module.split(alias),
        starts_with?(name, hint) do
      %{kind: :module, type: :alias, name: name, desc: "", subtype: nil}
    end
  end

  defp match_elixir_modules(module, hint, env) do
    name = Atom.to_string(module)
    depth = length(String.split(name, ".")) + 1
    base = name <> "." <> hint

    for mod <- match_modules(base, module === Elixir, env),
        parts = String.split(mod, "."),
        depth <= length(parts),
        name = Enum.at(parts, depth - 1),
        valid_alias_piece?("." <> name),
        uniq: true do
      mod_as_atom = mod |> String.to_atom()
      desc = Introspection.get_module_docs_summary(mod_as_atom)
      subtype = Introspection.get_module_subtype(mod_as_atom)
      %{kind: :module, type: :elixir, name: name, desc: desc, subtype: subtype}
    end
    |> Enum.uniq_by(fn %{name: name} -> name end)
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
    |> Enum.drop_while(&(not starts_with?(&1, hint)))
    |> Enum.take_while(&starts_with?(&1, hint))
  end

  defp get_modules(true, env) do
    ["Elixir.Elixir"] ++ get_modules(false, env)
  end

  defp get_modules(false, env) do
    modules = Enum.map(:code.all_loaded(), &Atom.to_string(elem(&1, 0)))

    case :code.get_mode() do
      :interactive -> modules ++ get_modules_from_applications() ++ get_modules_from_metadata(env)
      _otherwise -> modules ++ get_modules_from_metadata(env)
    end
  end

  defp get_modules_from_applications do
    for module <- Introspection.all_modules() do
      Atom.to_string(module)
    end
  end

  defp get_modules_from_metadata(env) do
    for {{k, nil, nil}, _} <- env.mods_and_funs, do: Atom.to_string(k)
  end

  defp match_module_funs(mod, hint, include_builtin, env) do
    falist =
      cond do
        env.mods_and_funs |> Map.has_key?({mod, nil, nil}) ->
          get_metadata_module_funs(mod, include_builtin, env)

        match?({:module, _}, ensure_loaded(mod)) ->
          get_module_funs(mod, include_builtin)

        true ->
          []
      end
      |> Enum.sort_by(fn {f, a, _, _, _, _} -> {f, -a} end)

    list =
      Enum.reduce(falist, [], fn {f, a, func_kind, doc, spec, arg}, acc ->
        case :lists.keyfind(f, 1, acc) do
          {f, aa, func_kind, docs, specs, args} ->
            :lists.keyreplace(
              f,
              1,
              acc,
              {f, [a | aa], func_kind, [doc | docs], [spec | specs], [arg | args]}
            )

          false ->
            [{f, [a], func_kind, [doc], [spec], [arg]} | acc]
        end
      end)

    for {fun, arities, func_kind, docs, specs, args} <- list,
        name = Atom.to_string(fun),
        starts_with?(name, hint) do
      %{
        kind: :function,
        name: name,
        arities: arities,
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
        for {{^mod, f, a}, info} <- env.mods_and_funs,
            a != nil,
            (mod == env.scope_module and not include_builtin) or Introspection.is_pub(info.type),
            include_builtin || not ({f, a} in @builtin_functions) do
          specs =
            case env.specs[{mod, f, a}] do
              nil -> nil
              %ElixirSense.Core.State.SpecInfo{specs: specs} -> specs |> Enum.join("\n")
            end

          {f, a, info.type, "", specs,
           info.params |> hd |> Enum.map_join(", ", &Macro.to_string/1)}
        end
    end
  end

  defp get_module_funs(mod, include_builtin) do
    docs = NormalizedCode.get_docs(mod, :docs)
    specs = TypeInfo.get_module_specs(mod)

    if docs != nil do
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
            :defmacro -> Map.get(specs, {:"MACRO-#{f}", new_arity + 1})
            :def -> Map.get(specs, {f, new_arity})
            nil -> nil
          end

        doc =
          case func_doc do
            nil -> ""
            {{_fun, _}, _line, _kind, _args, doc} -> Introspection.extract_summary_from_docs(doc)
          end

        fun_args = Introspection.extract_fun_args(func_doc)

        {f, a, func_kind, doc, Introspection.spec_to_string(spec), fun_args}
      end
      |> Kernel.++(
        for {f, a} <- @builtin_functions, include_builtin, do: {f, a, :def, nil, nil, nil}
      )
    else
      funs =
        mod.module_info(:exports)
        |> Kernel.--(if include_builtin, do: [], else: @builtin_functions)
        |> Kernel.++(BuiltinFunctions.erlang_builtin_functions(mod))

      edoc_results = get_edocs(mod)

      for {f, a} <- funs do
        spec = specs[{f, a}]
        spec_str = Introspection.spec_to_string(spec)

        case f |> Atom.to_string() do
          "MACRO-" <> name ->
            params = format_params(spec, a - 1)
            # TODO test this arity and spec
            # TODO is this ranch reachable?
            {String.to_atom(name), a - 1, :defmacro, "", spec_str, params}

          _name ->
            params = format_params(spec, a)
            {f, a, :def, edoc_results[{f, a}] || "", spec_str, params}
        end
      end
    end
  end

  defp format_params({_kind, {{_name, _arity}, [params | _]}}, _arity_1) do
    TypeInfo.extract_params(params)
    |> Enum.map_join(", ", &Atom.to_string/1)
  end

  defp format_params(nil, 0), do: ""

  defp format_params(nil, arity) do
    1..arity
    |> Enum.map_join(", ", fn _ -> "term" end)
  end

  defp get_edocs(mod) do
    EdocReader.get_docs(mod, :any)
    |> Map.new(fn {{:function, fun, arity}, _, _, maybe_doc, _} ->
      {{fun, arity},
       EdocReader.extract_docs(maybe_doc)
       |> Introspection.extract_summary_from_docs()}
    end)
  end

  defp special_buildins(mod) do
    mod.module_info(:exports)
    |> Enum.filter(fn {f, a} ->
      {f, a} in [{:behaviour_info, 1}]
    end)
  end

  def find_doc(fun, _docs) when fun in @builtin_functions, do: {:def, nil}

  def find_doc(fun, docs) do
    doc =
      docs
      |> Enum.find(&match?({^fun, _, _, _, _}, &1))

    case doc do
      nil -> {nil, nil}
      {_, _, func_kind, _, _} = d -> {func_kind, d}
    end
  end

  defp default_arg_functions(docs) do
    for {{fun_name, arity}, _, _kind, args, _} <- docs,
        count = Introspection.count_defaults(args),
        count > 0,
        new_arity <- (arity - count)..(arity - 1),
        into: %{},
        do: {{fun_name, new_arity}, arity}
  end

  defp ensure_loaded(Elixir), do: {:error, :nofile}
  defp ensure_loaded(mod), do: Code.ensure_compiled(mod)

  defp starts_with?(_string, ""), do: true
  defp starts_with?(string, hint), do: String.starts_with?(string, hint)

  ## Ad-hoc conversions

  defp to_entries(%{kind: :module, name: name, desc: desc, subtype: subtype}) do
    [%{type: :module, name: name, subtype: subtype, summary: desc}]
  end

  defp to_entries(%{
         kind: :function,
         name: name,
         arities: arities,
         module: mod,
         func_kind: func_kind,
         docs: docs,
         specs: specs,
         args: args
       }) do
    docs_specs = docs |> Enum.zip(specs)
    arities_docs_specs = arities |> Enum.zip(docs_specs)
    arities_docs_specs_args = arities_docs_specs |> Enum.zip(args)

    for {{a, {doc, spec}}, args} <- arities_docs_specs_args do
      kind =
        case func_kind do
          k when k in [:defmacro, :defmacrop, :defguard, :defguardp] -> :macro
          _ -> :function
        end

      mod_name = inspect(mod)

      fa = {name |> String.to_atom(), a}

      if fa in BuiltinFunctions.all() do
        %{
          type: kind,
          name: name,
          arity: a,
          args: BuiltinFunctions.get_args(fa) |> Enum.join(", "),
          origin: mod_name,
          summary: "Built-in function",
          spec: BuiltinFunctions.get_specs(fa) |> Enum.join("\n")
        }
      else
        %{
          type: kind,
          name: name,
          arity: a,
          args: args,
          origin: mod_name,
          summary: doc,
          spec: spec || ""
        }
      end
    end
  end

  defp to_uniq_entries(%{kind: :module} = mod) do
    to_entries(mod)
  end

  defp to_uniq_entries(%{kind: :function} = fun) do
    to_entries(fun)
  end

  defp to_hint(%{kind: :module, name: name}, hint) when name == hint do
    format_hint(name, name) <> "."
  end

  defp to_hint(%{kind: :module, name: name}, hint) do
    format_hint(name, hint)
  end

  defp to_hint(%{kind: :function, name: name}, hint) do
    format_hint(name, hint)
  end

  defp format_hint(name, hint) do
    hint_size = byte_size(hint)
    binary_part(name, hint_size, byte_size(name) - hint_size)
  end
end
