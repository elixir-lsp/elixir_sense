defmodule Alchemist.Helpers.Complete do

  alias Alchemist.Helpers.ModuleInfo
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.TypeInfo
  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode

  @builtin_functions [{:__info__, 1}, {:module_info, 0}, {:module_info, 1}]

  @moduledoc false

  # This Alchemist.Completer holds a codebase copy of the
  # IEx.Autocomplete because for the use of context specific
  # aliases.
  #
  # With the release of Elixir v1.1 the IEx.Autocomplete will
  # look for aliases in a certain environment variable
  # `Application.get_env(:iex, :autocomplete_server)` and until
  # then we'll use our own autocomplete codebase.

  def run(exp) do
    code = case is_bitstring(exp) do
             true -> exp |> String.to_charlist
             _ -> exp
           end

    {status, result, list} = expand(code |> Enum.reverse)

    case {status, result, list} do
      {:no, _, _}  -> ''
      {:yes, [], _} -> List.insert_at(list, 0, %{type: :hint, value: "#{exp}"})
      {:yes, _, []} -> run(code ++ result)
      {:yes, _,  _} -> List.insert_at(run(code ++ result), 1, Enum.at(list, 0))
      #
    end
  end

  def run(hint, modules) do
    context_module = modules |> Enum.at(0)

    exported? = fn mod, f, a ->
      (function_exported?(mod, f, a) or macro_exported?(mod, f, a))
    end
    accept_function = fn
      (mod, mod, _, _, _)          -> true
      (_  , _  , _, _, :undefined) -> false
      (_  , mod, f, a, _)          -> exported?.(mod, f, a)
    end

    for module <- modules, module != Elixir do
      funs = ModuleInfo.get_functions(module, hint)
      funs_info = Introspection.module_functions_info(module)

      for {f, a} <- funs,
          {func_kind, fun_args, desc, spec} = Map.get(funs_info, {f, a}, {:undefined, "", "", ""}),
          accept_function.(context_module, module, f, a, func_kind)
      do
        kind = case {context_module, module, func_kind} do
          {m, m, :defmacro}  -> "public_macro"
          {_, _, :defmacro}  -> "macro"
          {m, m, :def}       -> "public_function"
          {_, _, :def}       -> "function"
          {m, m, :undefined} -> if exported?.(module, f, a), do: "public_function", else: "private_function"
          _                  -> "unknown"
        end

        func_name = Atom.to_string(f)
        mod_name = module |> Introspection.module_to_string
        %{type: kind, name: func_name, arity: a, args: fun_args, origin: mod_name, summary: desc, spec: spec}
      end
    end |> List.flatten |> add_builtin_functions(hint)
  end

  defp add_builtin_functions(list, hint) do
    builtin_list =
      if String.contains?(hint, ".") do
        local_hint = String.split(hint, ".") |> List.last()
        for {f, a} <- @builtin_functions,
            f_name = to_string(f),
            String.starts_with?(f_name, local_hint)
            do
          %{name: f_name, arity: a, args: "", origin: "", summary: "Built-in function", spec: nil, type: "function"}
        end
      else
        []
      end
    list ++ builtin_list
  end

  def expand('') do
    expand_import("")
  end

  def expand([h|t] = expr) do
    cond do
      h === ?. and t != [] ->
        expand_dot(reduce(t))
      h === ?: and t == [] ->
        expand_erlang_modules()
      identifier?(h) ->
        expand_expr(reduce(expr))
      (h == ?/) and t != [] and identifier?(hd(t)) ->
        expand_expr(reduce(t))
      h in '([{' ->
        expand('')
      true ->
        no()
    end
  end

  defp identifier?(h) do
    (h in ?a..?z) or (h in ?A..?Z) or (h in ?0..?9) or h in [?_, ??, ?!]
  end

  defp expand_dot(expr) do
    case Code.string_to_quoted expr do
      {:ok, atom} when is_atom(atom) ->
        expand_call(atom, "")
      {:ok, {:__aliases__, _, list}} ->
        expand_elixir_modules(list, "")
      {:ok, {_, _, _} = ast_node} ->
        expand_call(ast_node, "")
      _ ->
        no()
    end
  end

  defp expand_expr(expr) do
    case Code.string_to_quoted expr do
      {:ok, atom} when is_atom(atom) ->
        expand_erlang_modules(Atom.to_string(atom))
      {:ok, {atom, _, nil}} when is_atom(atom) ->
        expand_import(Atom.to_string(atom))
      {:ok, {:__aliases__, _, [root]}} ->
        expand_elixir_modules([], Atom.to_string(root))
      {:ok, {:__aliases__, _, [h|_] = list}} when is_atom(h) ->
        hint = Atom.to_string(List.last(list))
        list = Enum.take(list, length(list) - 1)
        expand_elixir_modules(list, hint)
      {:ok, {{:., _, [ast_node, fun]}, _, []}} when is_atom(fun) ->
        expand_call(ast_node, Atom.to_string(fun))
      _ ->
        no()
    end
  end

  defp reduce(expr) do
    Enum.reduce(' ([{', expr, fn token, acc ->
      hd(:string.tokens(acc, [token]))
    end)
    |> Enum.reverse
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
      ""   -> yes("", to_uniq_entries(uniq))
      hint -> yes(hint, to_uniq_entries(uniq))
    end
  end

  defp format_expansion([first|_] = entries, hint) do
    binary = Enum.map(entries, &(&1.name))
    length = byte_size(hint)
    prefix = :binary.longest_common_prefix(binary)
    if prefix in [0, length] do
      yes("", Enum.flat_map(entries, &to_entries/1))
    else
      yes(:binary.part(first.name, prefix, length - prefix), [])
    end
  end

  ## Expand calls

  # :atom.fun
  defp expand_call(mod, hint) when is_atom(mod) do
    expand_require(mod, hint)
  end

  # Elixir.fun
  defp expand_call({:__aliases__, _, list}, hint) do
    case expand_alias(list) do
      {:ok, alias} -> expand_require(alias, hint)
      :error -> no()
    end
  end

  # variable.fun_or_key
  defp expand_call({_, _, _} = _ast_node, _hint) do
    # FIXME not supported
    no()
    # case value_from_binding(ast_node, server) do
    #   {:ok, mod} when is_atom(mod) -> expand_call(mod, hint, server)
    #   {:ok, map} when is_map(map) -> expand_map_field_access(map, hint)
    #   _otherwise -> no()
    # end
  end

  defp expand_call(_, _) do
    no()
  end

  defp expand_require(mod, hint) do
    format_expansion(match_module_funs(mod, hint), hint)
  end

  defp expand_import(hint) do
    funs =
      match_module_funs(Kernel, hint) ++
      match_module_funs(Kernel.SpecialForms, hint)
    format_expansion funs, hint
  end

  ## Erlang modules

  defp expand_erlang_modules(hint \\ "") do
    format_expansion match_erlang_modules(hint), hint
  end

  defp match_erlang_modules(hint) do
    for mod <- match_modules(hint, true), usable_as_unquoted_module?(mod) do
      %{kind: :module, name: mod, type: :erlang, desc: ""}
    end
  end

  ## Elixir modules

  defp expand_elixir_modules([], hint) do
    expand_elixir_modules_from_aliases(Elixir, hint, match_aliases(hint))
  end

  defp expand_elixir_modules(list, hint) do
    case expand_alias(list) do
      {:ok, alias} -> expand_elixir_modules_from_aliases(alias, hint, [])
      :error -> no()
    end
  end

  defp expand_elixir_modules_from_aliases(mod, hint, aliases) do
    aliases
    |> Kernel.++(match_elixir_modules(mod, hint))
    |> Kernel.++(match_module_funs(mod, hint))
    |> format_expansion(hint)
  end

  defp expand_alias([name | rest]) when is_atom(name)
  do
    case Keyword.fetch(aliases_from_env(), Module.concat(Elixir, name)) do
      {:ok, name} when rest == [] -> {:ok, name}
      {:ok, name} -> {:ok, Module.concat([name | rest])}
      :error -> {:ok, Module.concat([name | rest])}
    end
  end
  defp expand_alias([_ | _]) do
    :error
  end

  defp aliases_from_env do
    :"alchemist.el"
    |> Application.get_env(:aliases)
    |> format_aliases
  end

  defp format_aliases(nil), do: []
  defp format_aliases(list), do: list

  defp match_aliases(hint) do
    for {alias, _mod} <- aliases_from_env(),
    [name] = Module.split(alias),
    starts_with?(name, hint) do
      %{kind: :module, type: :alias, name: name, desc: ""}
    end
  end

  defp match_elixir_modules(module, hint) do
    name  = Atom.to_string(module)
    depth = length(String.split(name, ".")) + 1
    base  = name <> "." <> hint

    for mod <- match_modules(base, module === Elixir),
    parts = String.split(mod, "."),
    depth <= length(parts),
    name = Enum.at(parts, depth - 1),
    valid_alias_piece?("." <> name) do
      mod_as_atom = mod |> String.to_atom
      desc = Introspection.get_module_docs_summary(mod_as_atom)
      subtype = Introspection.get_module_subtype(mod_as_atom)
      %{kind: :module, type: :elixir, name: name,
        desc: desc, subtype: subtype}
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

  defp match_modules(hint, root) do
    root
    |> get_modules()
    |> :lists.usort()
    |> Enum.drop_while(& not starts_with?(&1, hint))
    |> Enum.take_while(& starts_with?(&1, hint))
  end

  defp get_modules(true) do
    ["Elixir.Elixir"] ++ get_modules(false)
  end

  defp get_modules(false) do
    modules = Enum.map(:code.all_loaded(), &Atom.to_string(elem(&1, 0)))
    case :code.get_mode() do
      :interactive -> modules ++ get_modules_from_applications()
      _otherwise -> modules
    end
  end

  defp get_modules_from_applications do
    for [app] <- loaded_applications(),
    {:ok, modules} = :application.get_key(app, :modules),
    module <- modules do
      Atom.to_string(module)
    end
  end

  defp loaded_applications do
    # If we invoke :application.loaded_applications/0,
    # it can error if we don't call safe_fixtable before.
    # Since in both cases we are reaching over the
    # application controller internals, we choose to match
    # for performance.
    :ets.match(:ac_tab, {{:loaded, :"$1"}, :_})
  end

  defp match_module_funs(mod, hint) do
    case ensure_loaded(mod) do
      {:module, _} ->
        falist = get_module_funs(mod)

      list = Enum.reduce falist, [], fn {f, a, func_kind, doc, spec}, acc ->
        case :lists.keyfind(f, 1, acc) do
          {f, aa, func_kind, docs, specs} ->
            :lists.keyreplace(f, 1, acc, {f, [a|aa], func_kind, [doc|docs], [spec|specs]})
          false -> [{f, [a], func_kind, [doc], [spec]}|acc]
        end
      end

      for {fun, arities, func_kind, docs, specs} <- list,
      name = Atom.to_string(fun),
      starts_with?(name, hint) do
        %{kind: :function, name: name, arities: arities, module: mod,
          func_kind: func_kind, docs: docs, specs: specs}
      end |> :lists.sort()

      _otherwise -> []
    end
  end

  defp get_module_funs(mod) do
    if function_exported?(mod, :__info__, 1) do
      if docs = NormalizedCode.get_docs(mod, :docs) do
        specs = TypeInfo.get_module_specs(mod)
        for {{f, a}, _line, func_kind, _sign, _doc} = func_doc <- docs, not hidden_fun?({f, a}, docs) do
          spec = Map.get(specs, {f, a})
          {f, a, func_kind, func_doc, Introspection.spec_to_string(spec)}
        end
      else
        macros = :macros
        |> mod.__info__()
        |> Enum.map(fn {f, a} -> {f, a, :macro, nil, nil} end)
        functions = :functions
        |> mod.__info__()
        # TODO should we reject :__info__ like in IEx?
        |> Enum.map(fn {f, a} -> {f, a, :function, nil, nil} end)
        (macros ++ functions)
        |> Enum.reject(fn {f, a, _, nil, nil} -> underscored_fun?({f, a}) end)
      end
    else
      # TODO should we reject underscored funs here?
      for {f, a} <- mod.module_info(:exports) do
        case f |> Atom.to_string do
          "MACRO-" <> name -> {String.to_atom(name), a, :macro, nil, nil}
          _name            -> {f, a, :function, nil, nil}
        end
      end
    end
  end

  defp hidden_fun?(fun, docs) do
    case Enum.find(docs, &match?({^fun, _, _, _, _}, &1)) do
      nil -> underscored_fun?(fun)
      doc -> not has_content?(doc)
    end
  end

  defp has_content?({_, _, _, _, false}),
     do: false
  defp has_content?({fun, _, _, _, nil}),
     do: not underscored_fun?(fun)
   defp has_content?({_, _, _, _, _}),
     do: true

  defp underscored_fun?({name, _}),
     do: hd(Atom.to_charlist(name)) == ?_

  defp ensure_loaded(Elixir), do: {:error, :nofile}
  defp ensure_loaded(mod), do: Code.ensure_compiled(mod)

  defp starts_with?(_string, ""),  do: true
  defp starts_with?(string, hint), do: String.starts_with?(string, hint)

  ## Ad-hoc conversions

  defp to_entries(%{kind: :module, name: name, desc: desc, subtype: subtype}) when subtype != nil do
    [%{type: :module, name: name, subtype: subtype, summary: desc}]
  end

  defp to_entries(%{kind: :module, name: name, desc: desc}) do
    [%{type: :module, name: name, subtype: nil, summary: desc}]
  end

  defp to_entries(%{kind: :function, name: name, arities: arities, module: mod, func_kind: func_kind, docs: docs, specs: specs}) do
    docs_specs = docs |> Enum.zip(specs)
    arities_docs_specs = arities |> Enum.zip(docs_specs)

    for {a, {doc, spec}} <- arities_docs_specs do
      {fun_args, desc} = Introspection.extract_fun_args_and_desc(doc)
      kind = case func_kind do
        :defmacro -> "macro"
        _         -> "function"
      end
      mod_name = mod
      |> Introspection.module_to_string
      %{type: kind, name: name, arity: a, args: fun_args, origin: mod_name, summary: desc, spec: spec}
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
    :binary.part(name, hint_size, byte_size(name) - hint_size)
  end

end
