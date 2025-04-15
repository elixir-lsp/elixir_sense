defmodule ElixirSense.Core.Compiler.Map do
  alias ElixirSense.Core.Compiler

  def expand_struct(meta, left, {:%{}, map_meta, map_args}, s, %{context: context} = e) do
    clean_map_args = clean_struct_key_from_map_args(map_args)

    {[e_left, e_right], se, ee} =
      Compiler.expand_args([left, {:%{}, map_meta, clean_map_args}], s, e)

    case validate_struct(e_left, context) do
      true when is_atom(e_left) ->
        case extract_struct_assocs(e_right) do
          {:expand, map_meta, assocs} when context != :match ->
            assoc_keys = Enum.map(assocs, fn {k, _} -> k end)
            struct = load_struct(meta, e_left, [assocs], se, ee)
            keys = [:__struct__ | assoc_keys]
            without_keys = Elixir.Map.drop(struct, keys)

            {struct_assocs, se} =
              Compiler.Macro.escape(Enum.sort(Elixir.Map.to_list(without_keys)), se)

            {{:%, meta, [e_left, {:%{}, map_meta, struct_assocs ++ assocs}]}, se, ee}

          {_, _, assocs} ->
            :elixir_env.trace({:struct_expansion, meta, e_left, assocs}, e)
            # elixir validates assocs against struct keys
            # we don't need to validate keys
            {{:%, meta, [e_left, e_right]}, se, ee}
        end

      _ ->
        # elixir raises invalid_struct_name if validate_struct returns false
        {{:%, meta, [e_left, e_right]}, se, ee}
    end
  end

  def expand_struct(meta, left, right, s, e) do
    # elixir raises here non_map_after_struct
    # try to recover from error by wrapping the expression in map
    expand_struct(meta, left, wrap_in_fake_map(right), s, e)
  end

  defp wrap_in_fake_map(right) do
    map_args =
      case right do
        list when is_list(list) ->
          if Keyword.keyword?(list) do
            list
          else
            [__fake_key__: list]
          end

        _ ->
          [__fake_key__: right]
      end

    {:%{}, [], map_args}
  end

  def expand_map(meta, [{:|, update_meta, [left, right]}], s, e) do
    # elixir raises update_syntax_in_wrong_context if e.context is not nil
    {[e_left | e_right], se, ee} = Compiler.expand_args([left | right], s, e)
    e_right = sanitize_kv(e_right, e)
    {{:%{}, meta, [{:|, update_meta, [e_left, e_right]}]}, se, ee}
  end

  def expand_map(meta, args, s, e) do
    {e_args, se, ee} = Compiler.expand_args(args, s, e)
    e_args = sanitize_kv(e_args, e)
    {{:%{}, meta, e_args}, se, ee}
  end

  defp clean_struct_key_from_map_args([{:|, pipe_meta, [left, map_assocs]}]) do
    [{:|, pipe_meta, [left, delete_struct_key(map_assocs)]}]
  end

  defp clean_struct_key_from_map_args(map_assocs) do
    delete_struct_key(map_assocs)
  end

  defp sanitize_kv(kv, %{context: context}) do
    Enum.filter(kv, fn
      {k, _v} ->
        if context == :match do
          validate_match_key(k)
        else
          true
        end

      _ ->
        false
    end)
  end

  defp validate_match_key({name, _, context})
       when is_atom(name) and is_atom(context) do
    # elixir raises here invalid_variable_in_map_key_match
    false
  end

  defp validate_match_key({:"::", _, [left, _]}) do
    validate_match_key(left)
  end

  defp validate_match_key({:^, _, [{name, _, context}]})
       when is_atom(name) and is_atom(context),
       do: true

  defp validate_match_key({:%{}, _, [_ | _]}), do: true

  defp validate_match_key({left, _, right}) do
    validate_match_key(left) and validate_match_key(right)
  end

  defp validate_match_key({left, right}) do
    validate_match_key(left) and validate_match_key(right)
  end

  defp validate_match_key(list) when is_list(list) do
    Enum.all?(list, &validate_match_key/1)
  end

  defp validate_match_key(_), do: true

  defp validate_struct({:^, _, [{var, _, ctx}]}, :match) when is_atom(var) and is_atom(ctx),
    do: true

  defp validate_struct({var, _meta, ctx}, :match) when is_atom(var) and is_atom(ctx), do: true
  defp validate_struct(atom, _) when is_atom(atom), do: true
  defp validate_struct(_, _), do: false

  defp sanitize_assocs(list) do
    Enum.filter(list, &match?({k, _} when is_atom(k), &1))
  end

  defp extract_struct_assocs({:%{}, meta, [{:|, _, [_, assocs]}]}) do
    {:update, meta, delete_struct_key(sanitize_assocs(assocs))}
  end

  defp extract_struct_assocs({:%{}, meta, assocs}) do
    {:expand, meta, delete_struct_key(sanitize_assocs(assocs))}
  end

  defp extract_struct_assocs(right) do
    # elixir raises here non_map_after_struct
    # try to recover from error by wrapping the expression in map
    extract_struct_assocs(wrap_in_fake_map(right))
  end

  defp delete_struct_key(assocs) do
    Keyword.delete(assocs, :__struct__)
  end

  def load_struct(meta, name, assocs, s, e) do
    case s.structs[name] do
      nil ->
        try do
          apply(name, :__struct__, assocs)
        else
          %{:__struct__ => ^name} = struct ->
            :elixir_env.trace({:struct_expansion, meta, name, assocs}, e)
            struct

          _ ->
            :elixir_env.trace({:struct_expansion, meta, name, assocs}, e)
            # recover from invalid return value
            [__struct__: name] |> merge_assocs(assocs)
        rescue
          _ ->
            :elixir_env.trace({:struct_expansion, meta, name, assocs}, e)
            # recover from error by building the fake struct
            [__struct__: name] |> merge_assocs(assocs)
        end

      info ->
        :elixir_env.trace({:struct_expansion, meta, name, assocs}, e)
        info.fields |> merge_assocs(assocs)
    end
  end

  defp merge_assocs(fields, []) do
    fields |> Elixir.Map.new()
  end

  defp merge_assocs(fields, [assocs]) do
    fields |> Keyword.merge(assocs) |> Elixir.Map.new()
  end
end
