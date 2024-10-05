defmodule ElixirSense.Core.Compiler.Dispatch do
  alias ElixirSense.Core.Compiler.Rewrite, as: ElixirRewrite
  alias ElixirSense.Core.State.ModFunInfo
  import :ordsets, only: [is_element: 2]

  def find_import(meta, name, arity, e) do
    tuple = {name, arity}

    case find_import_by_name_arity(meta, tuple, [], e) do
      {:function, receiver} ->
        # TODO trace call?
        # TODO address when https://github.com/elixir-lang/elixir/issues/13878 is resolved
        # ElixirEnv.trace({:imported_function, meta, receiver, name, arity}, e)
        receiver

      {:macro, receiver} ->
        # TODO trace call?
        # ElixirEnv.trace({:imported_macro, meta, receiver, name, arity}, e)
        receiver

      {:ambiguous, [head | _]} ->
        # elixir raises here, we choose first one
        # TODO trace call?
        head

      _ ->
        false
    end
  end

  def find_imports(meta, name, e) do
    funs = e.functions
    macs = e.macros

    acc0 = %{}
    acc1 = find_imports_by_name(funs, acc0, name, meta, e)
    acc2 = find_imports_by_name(macs, acc1, name, meta, e)

    imports = acc2 |> Map.to_list() |> Enum.sort()
    # trace_import_quoted(imports, meta, name, e)
    imports
  end

  def import_function(meta, name, arity, s, e) do
    tuple = {name, arity}

    case find_import_by_name_arity(meta, tuple, [], e) do
      {:function, receiver} ->
        remote_function(meta, receiver, name, arity, e)

      {:macro, _receiver} ->
        false

      {:import, receiver} ->
        require_function(meta, receiver, name, arity, s, e)

      {:ambiguous, [first | _]} ->
        # elixir raises here, we return first matching
        require_function(meta, first, name, arity, s, e)

      false ->
        if Macro.special_form?(name, arity) do
          false
        else
          function = e.function

          mfa = {e.module, name, arity}

          if function != nil and function != tuple and
               Enum.any?(s.mods_funs_to_positions, fn {key, info} ->
                 key == mfa and ModFunInfo.get_category(info) == :macro
               end) do
            false
          else
            {:local, name, arity}
          end
        end
    end
  end

  def require_function(meta, receiver, name, arity, s, e) do
    required = receiver in e.requires

    if is_macro(name, arity, receiver, required, s) do
      false
    else
      remote_function(meta, receiver, name, arity, e)
    end
  end

  defp remote_function(_meta, receiver, name, arity, _e) do
    case ElixirRewrite.inline(receiver, name, arity) do
      {ar, an} -> {:remote, ar, an, arity}
      false -> {:remote, receiver, name, arity}
    end
  end

  def find_imports_by_name([{mod, imports} | mod_imports], acc, name, meta, e) do
    new_acc = find_imports_by_name(name, imports, acc, mod, meta, e)
    find_imports_by_name(mod_imports, new_acc, name, meta, e)
  end

  def find_imports_by_name([], acc, _name, _meta, _e), do: acc

  def find_imports_by_name(name, [{name, arity} | imports], acc, mod, meta, e) do
    case Map.get(acc, arity) do
      nil ->
        find_imports_by_name(name, imports, Map.put(acc, arity, mod), mod, meta, e)

      _other_mod ->
        # elixir raises here ambiguous_call
        find_imports_by_name(name, imports, acc, mod, meta, e)
    end
  end

  def find_imports_by_name(name, [{import_name, _} | imports], acc, mod, meta, e)
      when name > import_name do
    find_imports_by_name(name, imports, acc, mod, meta, e)
  end

  def find_imports_by_name(_name, _imports, acc, _mod, _meta, _e), do: acc

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

  defp is_macro(_name, _arity, _module, false, _s), do: false

  defp is_macro(name, arity, receiver, true, s) do
    mfa = {receiver, name, arity}

    Enum.any?(s.mods_funs_to_positions, fn {key, info} ->
      key == mfa and ModFunInfo.get_category(info) == :macro
    end) ||
      try do
        macros = receiver.__info__(:macros)
        {name, arity} in macros
      rescue
        _error -> false
      end
  end
end
