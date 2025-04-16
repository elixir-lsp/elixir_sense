defmodule ElixirSense.Core.Normalized.Macro.Env do
  def expand_import(env, meta, name, arity, opts \\ [])
      when is_list(meta) and is_atom(name) and is_integer(arity) and is_list(opts) do
    local_for_callback =
      Keyword.get(opts, :local_for_callback, fn meta, name, arity, kinds, e ->
        if Version.match?(System.version(), ">= 1.14.0-dev") do
          :elixir_def.local_for(meta, name, arity, kinds, e)
        else
          :elixir_def.local_for(e.module, name, arity, kinds)
        end
      end)

    case Macro.special_form?(name, arity) do
      true ->
        {:error, :not_found}

      false ->
        allow_locals = Keyword.get(opts, :allow_locals, true)
        trace = Keyword.get(opts, :trace, true)
        # module = env.module

        # elixir version passes module.__info__(:macros) as extra, we do not need that
        # instead we override local_for_callback
        extra = []
        # case allow_locals and function_exported?(module, :__info__, 1) do
        #   true -> [{module, module.__info__(:macros)}]
        #   false -> []
        # end

        case __MODULE__.Dispatch.expand_import(
               meta,
               name,
               arity,
               env,
               extra,
               allow_locals,
               trace,
               local_for_callback
             ) do
          {:macro, receiver, expander} ->
            {:macro, receiver, wrap_expansion(receiver, expander, meta, name, arity, env, opts)}

          {:function, receiver, name} ->
            {:function, receiver, name}

          error ->
            {:error, error}
        end
    end
  end

  defp wrap_expansion(receiver, expander, _meta, _name, _arity, env, _opts) do
    fn expansion_meta, args ->
      quoted = expander.(args, env)
      next = :elixir_module.next_counter(env.module)

      if Version.match?(System.version(), ">= 1.14.0-dev") do
        :elixir_quote.linify_with_context_counter(expansion_meta, {receiver, next}, quoted)
      else
        :elixir_quote.linify_with_context_counter(
          expansion_meta |> Keyword.get(:line, 0),
          {receiver, next},
          quoted
        )
      end
    end
  end

  if Version.match?(System.version(), ">= 1.17.0-dev") do
    # defdelegate expand_import(env, meta, fun, arity, opts), to: Macro.Env
    defdelegate expand_require(env, meta, module, fun, arity, opts), to: Macro.Env
    defdelegate expand_alias(env, meta, list, opts), to: Macro.Env
    defdelegate define_alias(env, meta, arg, opts), to: Macro.Env
    defdelegate define_require(env, meta, arg, opts), to: Macro.Env
    defdelegate define_import(env, meta, arg, opts), to: Macro.Env
  else
    def fake_expand_callback(_meta, _args) do
      {:__block__, [], []}
    end

    def expand_require(env, meta, module, name, arity, opts \\ [])
        when is_list(meta) and is_atom(module) and is_atom(name) and is_integer(arity) and
               is_list(opts) do
      trace = Keyword.get(opts, :trace, true)

      case __MODULE__.Dispatch.expand_require(meta, module, name, arity, env, trace) do
        {:macro, receiver, expander} ->
          {:macro, receiver, wrap_expansion(receiver, expander, meta, name, arity, env, opts)}

        :error ->
          :error
      end
    end

    def expand_alias(env, meta, list, opts \\ [])
        when is_list(meta) and is_list(list) and is_list(opts) do
      trace = Keyword.get(opts, :trace, true)

      case __MODULE__.Aliases.expand(meta, list, env, trace) do
        atom when is_atom(atom) -> {:alias, atom}
        [_ | _] -> :error
      end
    end

    def define_require(env, meta, module, opts \\ [])
        when is_list(meta) and is_atom(module) and is_list(opts) do
      {trace, opts} = Keyword.pop(opts, :trace, true)
      env = __MODULE__.Aliases.require(meta, module, opts, env, trace)
      result = __MODULE__.Aliases.alias(meta, module, false, opts, env, trace)
      maybe_define_error(result, :elixir_aliases)
    end

    def define_alias(env, meta, module, opts \\ [])
        when is_list(meta) and is_atom(module) and is_list(opts) do
      {trace, opts} = Keyword.pop(opts, :trace, true)
      result = __MODULE__.Aliases.alias(meta, module, true, opts, env, trace)
      maybe_define_error(result, :elixir_aliases)
    end

    def define_import(env, meta, module, opts \\ [])
        when is_list(meta) and is_atom(module) and is_list(opts) do
      {trace, opts} = Keyword.pop(opts, :trace, true)
      {warnings, opts} = Keyword.pop(opts, :emit_warnings, true)
      {info_callback, opts} = Keyword.pop(opts, :info_callback, &module.__info__/1)

      result = __MODULE__.Import.import(meta, module, opts, env, warnings, trace, info_callback)
      maybe_define_error(result, :elixir_import)
    end

    defp maybe_define_error({:ok, env}, _mod),
      do: {:ok, env}

    defp maybe_define_error({:error, reason}, _mod),
      do: {:error, inspect(reason)}
  end

  defmodule Aliases do
    def require(meta, ref, opts, e, trace) do
      trace && :elixir_env.trace({:require, meta, ref, opts}, e)
      %{e | requires: :ordsets.add_element(ref, e.requires)}
    end

    def alias(meta, ref, include_by_default, opts, e, trace) do
      %{aliases: aliases, macro_aliases: macro_aliases} = e

      case expand_as(:lists.keyfind(:as, 1, opts), include_by_default, ref) do
        {:ok, ^ref} ->
          {:ok,
           %{
             e
             | aliases: remove_alias(ref, aliases),
               macro_aliases: remove_macro_alias(meta, ref, macro_aliases)
           }}

        {:ok, new} ->
          trace && :elixir_env.trace({:alias, meta, ref, new, opts}, e)

          {:ok,
           %{
             e
             | aliases: store_alias(new, ref, aliases),
               macro_aliases: store_macro_alias(meta, new, ref, macro_aliases)
           }}

        :none ->
          {:ok, e}

        {:error, reason} ->
          {:error, reason}
      end
    end

    defp expand_as({:as, atom}, _include_by_default, _ref)
         when is_atom(atom) and not is_boolean(atom) do
      case Atom.to_charlist(atom) do
        ~c"Elixir." ++ ([first_letter | _] = rest) when first_letter in ?A..?Z ->
          case :string.tokens(rest, ~c".") do
            [_] ->
              {:ok, atom}

            _ ->
              {:error, {:invalid_alias_for_as, :nested_alias, atom}}
          end

        _ ->
          {:error, {:invalid_alias_for_as, :not_alias, atom}}
      end
    end

    defp expand_as({:as, other}, _include_by_default, _ref) do
      {:error, {:invalid_alias_for_as, :not_alias, other}}
    end

    defp expand_as(false, true, ref) do
      case Atom.to_charlist(ref) do
        ~c"Elixir." ++ [first_letter | _] = list when first_letter in ?A..?Z ->
          last = last(Enum.reverse(list), [])
          {:ok, :"Elixir.#{last}"}

        _ ->
          {:error, {:invalid_alias_module, ref}}
      end
    end

    defp expand_as(false, false, _ref) do
      :none
    end

    defp last([?. | _], acc), do: acc
    defp last([h | t], acc), do: last(t, [h | acc])
    defp last([], acc), do: acc

    defp store_alias(new, old, aliases) do
      :lists.keystore(new, 1, aliases, {new, old})
    end

    defp store_macro_alias(meta, new, old, aliases) do
      case :lists.keyfind(:counter, 1, meta) do
        {:counter, counter} ->
          :lists.keystore(new, 1, aliases, {new, {counter, old}})

        false ->
          aliases
      end
    end

    defp remove_alias(atom, aliases) do
      :lists.keydelete(atom, 1, aliases)
    end

    defp remove_macro_alias(meta, atom, aliases) do
      case :lists.keyfind(:counter, 1, meta) do
        {:counter, _counter} ->
          :lists.keydelete(atom, 1, aliases)

        false ->
          aliases
      end
    end

    def expand(_meta, [Elixir | _] = list, _e, _trace) do
      list
    end

    def expand(_meta, [h | _] = list, _e, _trace) when not is_atom(h) do
      list
    end

    def expand(meta, list, %{aliases: aliases, macro_aliases: macro_aliases} = e, trace) do
      case :lists.keyfind(:alias, 1, meta) do
        {:alias, false} ->
          expand(meta, list, macro_aliases, e, trace)

        {:alias, atom} when is_atom(atom) ->
          atom

        false ->
          expand(meta, list, aliases, e, trace)
      end
    end

    def expand(meta, [h | t], aliases, e, trace) do
      lookup = String.to_atom("Elixir." <> Atom.to_string(h))

      counter =
        case :lists.keyfind(:counter, 1, meta) do
          {:counter, c} -> c
          _ -> nil
        end

      case lookup(lookup, aliases, counter) do
        ^lookup ->
          [h | t]

        atom ->
          trace && :elixir_env.trace({:alias_expansion, meta, lookup, atom}, e)

          case t do
            [] -> atom
            _ -> Module.concat([atom | t])
          end
      end
    end

    defp lookup(else_val, list, counter) do
      case :lists.keyfind(else_val, 1, list) do
        {^else_val, {^counter, value}} -> value
        {^else_val, value} when is_atom(value) -> value
        _ -> else_val
      end
    end
  end

  defmodule Import do
    alias ElixirSense.Core.Normalized.Macro.Env.Aliases

    def import(meta, ref, opts, e, warn, trace) do
      import(meta, ref, opts, e, warn, trace, &ref.__info__/1)
    end

    def import(meta, ref, opts, e, warn, trace, info_callback) do
      case import_only_except(meta, ref, opts, e, warn, info_callback) do
        {functions, macros, _added} ->
          ei = %{e | functions: functions, macros: macros}
          {:ok, Aliases.require(meta, ref, opts, ei, trace)}

        {:error, reason} ->
          {:error, reason}
      end
    end

    defp import_only_except(meta, ref, opts, e, warn, info_callback) do
      maybe_only = :lists.keyfind(:only, 1, opts)

      case :lists.keyfind(:except, 1, opts) do
        false ->
          import_only_except(meta, ref, maybe_only, false, e, warn, info_callback)

        {:except, dup_except} when is_list(dup_except) ->
          case ensure_keyword_list(dup_except) do
            :ok ->
              except = ensure_no_duplicates(dup_except, :except, meta, e, warn)
              import_only_except(meta, ref, maybe_only, except, e, warn, info_callback)

            :error ->
              {:error, {:invalid_option, :except, dup_except}}
          end

        {:except, other} ->
          {:error, {:invalid_option, :except, other}}
      end
    end

    def import_only_except(meta, ref, maybe_only, except, e, warn, info_callback) do
      case maybe_only do
        {:only, :functions} ->
          {added1, _used1, funs} = import_functions(meta, ref, except, e, warn, info_callback)
          {funs, :lists.keydelete(ref, 1, e.macros), added1}

        {:only, :macros} ->
          {added2, _used2, macs} = import_macros(meta, ref, except, e, warn, info_callback)
          {:lists.keydelete(ref, 1, e.functions), macs, added2}

        {:only, :sigils} ->
          {added1, _used1, funs} =
            import_sigil_functions(meta, ref, except, e, warn, info_callback)

          {added2, _used2, macs} =
            import_sigil_macros(meta, ref, except, e, warn, info_callback)

          {funs, macs, added1 or added2}

        {:only, dup_only} when is_list(dup_only) ->
          case ensure_keyword_list(dup_only) do
            :ok when except == false ->
              only = ensure_no_duplicates(dup_only, :only, meta, e, warn)

              {added1, _used1, funs} =
                import_listed_functions(meta, ref, only, e, warn, info_callback)

              {added2, _used2, macs} =
                import_listed_macros(meta, ref, only, e, warn, info_callback)

              # for {name, arity} <- (only -- used1) -- used2, warn, do: elixir_errors.file_warn(meta, e, __MODULE__, {:invalid_import, {ref, name, arity}})
              {funs, macs, added1 or added2}

            :ok ->
              {:error, :only_and_except_given}

            :error ->
              {:error, {:invalid_option, :only, dup_only}}
          end

        {:only, other} ->
          {:error, {:invalid_option, :only, other}}

        false ->
          {added1, _used1, funs} = import_functions(meta, ref, except, e, warn, info_callback)
          {added2, _used2, macs} = import_macros(meta, ref, except, e, warn, info_callback)
          {funs, macs, added1 or added2}
      end
    end

    def import_listed_functions(meta, ref, only, e, warn, info_callback) do
      new = intersection(only, get_functions(ref, info_callback))
      calculate_key(meta, ref, Map.get(e, :functions), new, e, warn)
    end

    def import_listed_macros(meta, ref, only, e, warn, info_callback) do
      new = intersection(only, get_macros(info_callback))
      calculate_key(meta, ref, Map.get(e, :macros), new, e, warn)
    end

    def import_functions(meta, ref, except, e, warn, info_callback) do
      calculate_except(meta, ref, except, Map.get(e, :functions), e, warn, fn ->
        get_functions(ref, info_callback)
      end)
    end

    def import_macros(meta, ref, except, e, warn, info_callback) do
      calculate_except(meta, ref, except, Map.get(e, :macros), e, warn, fn ->
        get_macros(info_callback)
      end)
    end

    def import_sigil_functions(meta, ref, except, e, warn, info_callback) do
      calculate_except(meta, ref, except, Map.get(e, :functions), e, warn, fn ->
        filter_sigils(info_callback.(:functions))
      end)
    end

    def import_sigil_macros(meta, ref, except, e, warn, info_callback) do
      calculate_except(meta, ref, except, Map.get(e, :macros), e, warn, fn ->
        filter_sigils(info_callback.(:macros))
      end)
    end

    defp calculate_except(meta, key, false, old, e, warn, existing) do
      new = remove_underscored(existing.())
      calculate_key(meta, key, old, new, e, warn)
    end

    defp calculate_except(meta, key, except, old, e, warn, existing) do
      new =
        case :lists.keyfind(key, 1, old) do
          false -> remove_underscored(existing.()) -- except
          {^key, old_imports} -> old_imports -- except
        end

      calculate_key(meta, key, old, new, e, warn)
    end

    defp calculate_key(meta, key, old, new, e, warn) do
      case :ordsets.from_list(new) do
        [] ->
          {false, [], :lists.keydelete(key, 1, old)}

        set ->
          final_set = ensure_no_special_form_conflict(set, key, meta, e, warn)
          {true, final_set, [{key, final_set} | :lists.keydelete(key, 1, old)]}
      end
    end

    defp get_functions(module, info_callback) do
      try do
        info_callback.(:functions)
      catch
        _, _ -> remove_internals(module.module_info(:exports))
      end
    end

    defp get_macros(info_callback) do
      try do
        info_callback.(:macros)
      catch
        _, _ -> []
      end
    end

    defp filter_sigils(funs) do
      Enum.filter(funs, &is_sigil/1)
    end

    defp is_sigil({name, 2}) do
      case Atom.to_string(name) do
        "sigil_" <> letters ->
          case letters do
            <<l>> when l in ?a..?z ->
              true

            "" ->
              false

            <<h, t::binary>> when h in ?A..?Z ->
              String.to_charlist(t)
              |> Enum.all?(fn l -> l in ?0..?9 or l in ?A..?Z end)

            _ ->
              false
          end

        _ ->
          false
      end
    end

    defp is_sigil(_) do
      false
    end

    defp intersection([h | t], all) do
      if Enum.member?(all, h) do
        [h | intersection(t, all)]
      else
        intersection(t, all)
      end
    end

    defp intersection([], _all) do
      []
    end

    defp remove_underscored(list) do
      Enum.filter(list, fn {name, _} ->
        case Atom.to_string(name) do
          "_" <> _ -> false
          _ -> true
        end
      end)
    end

    if Version.match?(System.version(), ">= 1.15.0-dev") do
      defp remove_internals(set) do
        set -- [{:behaviour_info, 1}, {:module_info, 1}, {:module_info, 0}]
      end
    else
      defp remove_internals(set) do
        set -- [{:module_info, 1}, {:module_info, 0}]
      end
    end

    defp ensure_keyword_list([]) do
      :ok
    end

    defp ensure_keyword_list([{key, value} | rest]) when is_atom(key) and is_integer(value) do
      ensure_keyword_list(rest)
    end

    defp ensure_keyword_list(_other) do
      :error
    end

    defp ensure_no_special_form_conflict(set, _key, _meta, _e, _warn) do
      Enum.filter(set, fn {name, arity} ->
        if Macro.special_form?(name, arity) do
          false
        else
          true
        end
      end)
    end

    defp ensure_no_duplicates(option, _kind, _meta, _e, _warn) do
      Enum.reduce(option, [], fn {name, arity}, acc ->
        if Enum.member?(acc, {name, arity}) do
          acc
        else
          [{name, arity} | acc]
        end
      end)
    end
  end

  defmodule Dispatch do
    def expand_import(meta, name, arity, e, extra, allow_locals, trace, local_for_callback) do
      tuple = {name, arity}
      module = e.module
      dispatch = find_import_by_name_arity(meta, tuple, extra, e)

      case dispatch do
        {:ambiguous, ambiguous} ->
          {:ambiguous, ambiguous}

        {:import, _} ->
          do_expand_import(dispatch, meta, name, arity, module, e, trace)

        _ ->
          local =
            allow_locals and local_for_callback.(meta, name, arity, [:defmacro, :defmacrop], e)

          case dispatch do
            {_, receiver} when local != false and receiver != module ->
              {:conflict, receiver}

            _ when local == false ->
              do_expand_import(dispatch, meta, name, arity, module, e, trace)

            _ ->
              trace && :elixir_env.trace({:local_macro, meta, name, arity}, e)
              {:macro, module, expander_macro_fun(meta, local, module, name, e)}
          end
      end
    end

    defp do_expand_import(result, meta, name, arity, module, e, trace) do
      case result do
        {:function, receiver} ->
          trace && :elixir_env.trace({:imported_function, meta, receiver, name, arity}, e)
          {:function, receiver, name}

        {:macro, receiver} ->
          trace && :elixir_env.trace({:imported_macro, meta, receiver, name, arity}, e)
          {:macro, receiver, expander_macro_named(meta, receiver, name, arity, e)}

        {:import, receiver} ->
          case expand_require(true, meta, receiver, name, arity, e, trace) do
            {:macro, _, _} = response ->
              response

            :error ->
              trace && :elixir_env.trace({:remote_function, meta, receiver, name, arity}, e)
              {:function, receiver, name}
          end

        false when module == Kernel ->
          case ElixirSense.Core.Compiler.Rewrite.inline(module, name, arity) do
            {ar, an} -> {:function, ar, an}
            false -> :not_found
          end

        false ->
          :not_found
      end
    end

    def expand_require(meta, receiver, name, arity, e, trace) do
      required =
        receiver == e.module or
          :lists.keyfind(:required, 1, meta) == {:required, true} or
          receiver in e.requires

      expand_require(required, meta, receiver, name, arity, e, trace)
    end

    defp expand_require(required, meta, receiver, name, arity, e, trace) do
      if is_macro(name, arity, receiver, required) do
        trace && :elixir_env.trace({:remote_macro, meta, receiver, name, arity}, e)
        {:macro, receiver, expander_macro_named(meta, receiver, name, arity, e)}
      else
        :error
      end
    end

    defp expander_macro_fun(meta, fun, receiver, name, e) do
      fn args, caller -> expand_macro_fun(meta, fun, receiver, name, args, caller, e) end
    end

    defp expander_macro_named(meta, receiver, name, arity, e) do
      proper_name = :"MACRO-#{name}"
      proper_arity = arity + 1
      fun = Function.capture(receiver, proper_name, proper_arity)
      fn args, caller -> expand_macro_fun(meta, fun, receiver, name, args, caller, e) end
    end

    defp expand_macro_fun(_meta, fun, _receiver, _name, args, caller, _e) do
      # elixir applies local macro via apply/2
      # we need to check if the macro is a function as we return a fake for local macros
      if is_function(fun) do
        apply(fun, [caller | args])
      else
        # we return a fake value and omit expansion
        :ok
      end
    end

    defp is_macro(_name, _arity, _module, false), do: false

    defp is_macro(name, arity, receiver, true) do
      try do
        macros = receiver.__info__(:macros)
        :lists.member({name, arity}, macros)
      rescue
        _ -> false
      end
    end

    defp find_import_by_name_arity(meta, {_name, arity} = tuple, extra, e) do
      case is_import(meta, arity) do
        {:import, _} = import_res ->
          import_res

        false ->
          funs = e.functions
          macs = extra ++ e.macros
          fun_match = find_import_by_name_arity(tuple, funs)
          mac_match = find_import_by_name_arity(tuple, macs)

          case {fun_match, mac_match} do
            {[], [receiver]} ->
              {:macro, receiver}

            {[receiver], []} ->
              {:function, receiver}

            {[], []} ->
              false

            _ ->
              {:ambiguous, fun_match ++ mac_match}
          end
      end
    end

    defp find_import_by_name_arity(tuple, list) do
      import :ordsets, only: [is_element: 2]
      for {receiver, set} <- list, is_element(tuple, set), do: receiver
    end

    defp is_import(meta, arity) do
      with {:ok, imports = [_ | _]} <- Keyword.fetch(meta, :imports),
           {:ok, _} <- Keyword.fetch(meta, :context),
           {_arity, receiver} <- :lists.keyfind(arity, 1, imports) do
        {:import, receiver}
      else
        _ -> false
      end
    end
  end
end
