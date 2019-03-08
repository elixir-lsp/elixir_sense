defmodule Core.TypeInfo do

  alias ElixirSense.Core.Introspection

  def extract_param_options(mod, fun, npar) do
    Introspection.get_function_specs(mod, fun)
    |> get_param_type_specs(npar)
    |> expand_type_specs(mod)
    |> Enum.filter(&list_type_spec?/1)
    |> extract_list_type_spec_options(mod)
  end

  def get_type_info(module, type) do
    type_str = Introspection.typespec_to_quoted(type) |> Macro.to_string()
    case extract_type_def_info(module, type) do
      {nil, name, n_args} ->
        {_full_name, spec, doc} =
          case Introspection.get_builtin_type_spec(name, n_args) do
            # Basic type
            nil ->
              doc = Introspection.get_basic_type_doc(to_string(name), n_args) || ""
              {type_str, "", doc}

            # Built-in type
            spec_ast ->
              {type_str, format_type_spec_ast(spec_ast, :type), "Built-in type"}
          end
          %{
            origin: "",
            type_spec: type_str,
            doc: doc,
            expanded_spec: spec
          }

      # Same module
      {^module, name, n_args} ->
        %{
          origin: inspect(module),
          type_spec: type_str,
          doc: Introspection.get_type_doc(module, name, n_args),
          expanded_spec: expand_type_spec(type, module) |> format_type_spec()
        }

      # Remote module
      {remote_mod, name, _} ->
        %{
          origin: inspect(remote_mod),
          type_spec: type_str,
          doc: Introspection.get_type_doc(remote_mod, name),
          expanded_spec: expand_type_spec(type, remote_mod) |> format_type_spec()
        }

      # Inline, non-existent
      _ ->
        %{
          origin: "",
          type_spec: type_str,
          doc: "",
          expanded_spec: ""
        }
    end
  end

  defp extract_list_type_spec_options(list_type_specs, mod) do
    Enum.flat_map(list_type_specs, fn type_spec ->
      type_spec
      |> expand_list_type_spec(mod)
      |> extract_union_options_name_and_type(mod)
    end)
  end

  defp get_param_type_specs(func_specs, npar) do
    for func_spec <- func_specs,
        params_types = extract_params_types(func_spec),
        length(params_types) > npar do
      params_types |> Enum.at(npar)
    end
  end

  defp expand_type_specs(types, module) do
    types |> Enum.map(fn type -> expand_type_spec(type, module) end)
  end

  defp expand_type_spec({:ann_type, _, [{:var, _, _}, type]}, module) do
    expand_type_spec(type, module)
  end

  defp expand_type_spec({:user_type, _, type_name, type_args}, module) do
    module
    |> Introspection.get_types()
    |> Enum.find(fn {_, {name, _, args}} ->
      name == type_name && length(args) == length(type_args)
    end)
  end

  defp expand_type_spec({:type, _, _, _} = type, _module) do
    {:not_found, {nil, type, []}}
  end

  defp expand_type_spec({:remote_type, _, [_, {:atom, _, type_name}, []]} = type, module) do
    module
    |> Introspection.get_types()
    |> Enum.find(fn {_, {name, _, _}} -> name == type_name end)
    |> case do
        nil -> {:not_found, type}
        type_found -> type_found
      end
  end

  defp expand_type_spec(type, _module) do
    type
  end

  defp expand_list_type_spec({_kind, {_name, {:type, _, :list, [type]}, _}}, module) do
    expand_type_spec(type, module)
  end

  # More than one option (union)
  defp extract_union_options_name_and_type({_kind, {_name, {:type, _, :union, options_types}, _}}, module) do
    options_types
    |> Enum.map(&extract_tagged_tuple_name_and_type(&1, module))
    |> List.flatten()
    |> Enum.filter(&(&1))
  end

  # Only one option (not actually a union)
  defp extract_union_options_name_and_type({_kind, {_name, {:type, _, :tuple, _} = type, _}}, module) do
    extract_tagged_tuple_name_and_type(type, module)
  end

  defp extract_tagged_tuple_name_and_type({:type, _, :tuple, [{:atom, _, name}, type]}, _module) do
    [{name, type}]
  end

  defp extract_tagged_tuple_name_and_type(type, module) do
    case expand_type_spec(type, module) do
      {_kind, {_name, {:type, _, :union, _}, _}} = expanded_type ->
        extract_union_options_name_and_type(expanded_type, module)
      _ -> []
    end
  end

  defp extract_type_def_info(_mod, {:type, _, :list, [_|_]}) do
    :inline_list
  end

  defp extract_type_def_info(_mod, {:type, _, type_name, args}) do
    {nil, type_name, length(args)}
  end

  defp extract_type_def_info(_mod, {:type, _, type_name}) do
    {nil, type_name, 0}
  end

  defp extract_type_def_info(_mod, {:remote_type, _, [{:atom, _, remote_mod}, {:atom, _, name}, args]}) do
    remote_mod = if remote_mod == :elixir, do: nil, else: remote_mod
    {remote_mod, name, length(args)}
  end

  defp extract_type_def_info(mod, {_, _, type_name, args}) do
    {mod, type_name, length(args)}
  end

  defp extract_type_def_info(_, _) do
    :not_found
  end

  defp list_type_spec?({_, {_, {:type, _, :list, [_]}, _}}) do
    true
  end

  defp list_type_spec?(_) do
    false
  end

  defp format_type_spec(nil) do
    ""
  end

  defp format_type_spec({:not_found, _}) do
    ""
  end

  defp format_type_spec({kind, type_spec}) do
    type_spec
    |> Introspection.type_to_quoted()
    |> format_type_spec_ast(kind)
  end

  defp format_type_spec_ast(spec_ast, kind) do
    kind_size = kind |> to_string() |> String.length()

    spec_ast
    |> Introspection.spec_ast_to_string()
    |> (&"@#{kind} #{&1}").()
    |> Code.format_string!(line_length: 35)
    |> to_string()
    |> String.split("\n")
    |> Enum.with_index()
    |> Enum.map(fn {l, i} when i > 0 -> String.slice(l, (kind_size + 2)..-1); {l, _} -> l end)
    |> Enum.join("\n")
  end

  defp extract_params_types({:spec, {_, [{:type, _, :fun, [{:type, _, :product, params_types}, _]}]}}) do
    params_types
  end

  defp extract_params_types({:spec, {_, [{:type, _, :bounded_fun, [type, constraints]}]}}) do
    {:type, _, :fun, [{:type, _, :product, params}, _]} = type

    vars_types =
      for {:type, _, :constraint, [{:atom, _, :is_subtype}, [{:var, _, var}, var_type]]} <- constraints, into: %{} do
        {var, var_type}
      end

    Enum.map(params, fn
      {:var, _, name} ->
        vars_types[name]
      {:type, l, :list, [{:var, _, name}]} ->
        {:type, l, :list, [vars_types[name]]}
      type -> type
    end)
  end

end
