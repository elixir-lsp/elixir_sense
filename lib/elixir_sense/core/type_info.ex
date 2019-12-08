defmodule ElixirSense.Core.TypeInfo do
  alias ElixirSense.Core.Normalized.Typespec
  alias ElixirSense.Core.BuiltinTypes
  alias ElixirSense.Core.Normalized.Code, as: NormalizedCode
  alias ElixirSense.Core.Source

  @doc_spec_line_length 75
  @param_option_spec_line_length 35

  def find_all(module, filter \\ & &1) do
    case NormalizedCode.get_docs(module, :type_docs) do
      docs when is_list(docs) ->
        for(
          {{name, arity}, _, _, doc} <- docs,
          typedef = get_type_spec(module, name, arity),
          type_ast = ElixirSense.Core.TypeAst.from_typedef(typedef),
          spec = format_type_spec(typedef, line_length: @param_option_spec_line_length),
          signature = ElixirSense.Core.TypeAst.extract_signature(type_ast),
          info = %{name: name, arity: arity, doc: doc, spec: spec, signature: signature},
          filter.(info)
        ) do
          info
        end

      nil ->
        for {kind, {name, _type, args}} = typedef <- Typespec.get_types(module),
            kind in [:type, :opaque],
            spec = format_type_spec(typedef, line_length: @param_option_spec_line_length),
            type_ast = ElixirSense.Core.TypeAst.from_typedef(typedef),
            signature = ElixirSense.Core.TypeAst.extract_signature(type_ast),
            info = %{
              name: name,
              arity: length(args),
              doc: "No documentation available",
              spec: spec,
              signature: signature
            },
            filter.(info) do
          info
        end
    end
  end

  def find_all_builtin(filter \\ & &1) do
    extract_name_and_arity = fn key ->
      name_parts = String.split(key, "/")
      arity = name_parts |> Enum.at(1, "0") |> String.to_integer()
      {Enum.at(name_parts, 0), arity}
    end

    for(
      {key, value} <- BuiltinTypes.all(),
      type_ast <- [value[:spec]],
      spec <- [format_type_spec_ast(type_ast, :type, line_length: @param_option_spec_line_length)],
      signature <- [
        value[:signature] || ElixirSense.Core.TypeAst.extract_signature(type_ast) || "#{key}()"
      ],
      {name, arity} = extract_name_and_arity.(key),
      doc = value[:doc] || "",
      info = %{name: name, arity: arity, doc: doc, spec: spec, signature: signature},
      filter.(info)
    ) do
      info
    end
  end

  def get_type_spec(module, type_name) do
    module
    |> Typespec.get_types()
    |> Enum.filter(fn {_, {name, _, _}} -> name == type_name end)
    |> Enum.sort_by(fn {_, {_, _, args}} -> length(args) end)
    |> Enum.at(0)
  end

  def get_type_spec(module, type_name, n_args) do
    module
    |> Typespec.get_types()
    |> Enum.find(fn {_, {name, _, args}} ->
      name == type_name && length(args) == n_args
    end)
  end

  def get_type_position_using_docs(module, type_name, file) do
    case get_type_doc(module, type_name) do
      {_, doc_line, _, _} ->
        {kind, _} = get_type_spec(module, type_name)
        kind_str = "@#{kind}"

        {str, index} =
          File.read!(file)
          |> Source.text_after(doc_line, 1)
          |> String.split("\n")
          |> Enum.with_index()
          |> Enum.find(fn {str, _} -> starts_with_type_def?(str, kind_str) end)

        kind_col = String.split(str, kind_str) |> Enum.at(0) |> String.length()
        col = kind_col + String.length(kind_str) + 2
        {doc_line + index, col}

      _ ->
        nil
    end
  end

  def get_type_ast(module, type_name) do
    {_kind, type} = get_type_spec(module, type_name)
    Typespec.type_to_quoted(type)
  end

  def spec_ast_to_string(ast) do
    ast |> Macro.to_string() |> String.replace("()", "")
  end

  def format_type_spec(spec) do
    format_type_spec(spec, [])
  end

  def format_type_spec({kind, type_spec}, opts) do
    type_spec
    |> Typespec.type_to_quoted()
    |> format_type_spec_ast(kind, opts)
  end

  def format_type_spec(_, _) do
    ""
  end

  def format_type_spec_ast(spec_ast, kind) do
    format_type_spec_ast(spec_ast, kind, [])
  end

  def format_type_spec_ast(nil, _kind, _opts) do
    ""
  end

  def format_type_spec_ast(spec_ast, kind, opts) do
    line_length = opts[:line_length] || @doc_spec_line_length
    kind_size = kind |> to_string() |> String.length()

    spec_ast
    |> Macro.to_string()
    |> (&"@#{kind} #{&1}").()
    |> Code.format_string!(line_length: line_length)
    |> to_string()
    |> String.split("\n")
    |> Enum.with_index()
    |> Enum.map(fn
      {l, i} when i > 0 -> String.slice(l, (kind_size + 2)..-1)
      {l, _} -> l
    end)
    |> Enum.join("\n")
  end

  def get_type_docs(module, type_name) do
    docs = NormalizedCode.get_docs(module, :type_docs) || []

    docs
    |> Enum.filter(fn {{name, _}, _, _, _} -> name == type_name end)
    |> Enum.sort_by(fn {{_, n_args}, _, _, _} -> n_args end)
  end

  def get_type_doc(module, type_name) do
    module
    |> get_type_docs(type_name)
    |> Enum.at(0)
  end

  def get_type_doc(module, type, type_n_args) do
    docs = NormalizedCode.get_docs(module, :type_docs) || []

    Enum.find(docs, fn {{name, n_args}, _, _, _} ->
      type == name && type_n_args == n_args
    end)
  end

  def get_type_doc_desc(module, type, type_n_args \\ 0) do
    case get_type_doc(module, type, type_n_args) do
      nil -> BuiltinTypes.get_builtin_type_doc(type)
      doc -> get_doc_description(doc)
    end
  end

  defp get_doc_description({{_, _}, _, _, desc}) do
    desc
  end

  defp get_doc_description(nil) do
    ""
  end

  def get_spec(module, function, arity)
      when is_atom(module) and is_atom(function) and is_integer(arity) do
    module
    |> get_module_specs()
    |> Map.get({function, arity})
  end

  def get_function_specs(module, function) when is_atom(module) and is_atom(function) do
    specs = module |> get_module_specs()

    for {{f, _}, spec} <- specs, f == function do
      spec
    end
  end

  def get_module_specs(module) do
    case Typespec.beam_specs(module) do
      nil ->
        %{}

      specs ->
        for {_kind, {{f, a}, _spec}} = spec <- specs, into: %{} do
          {{f, a}, spec}
        end
    end
  end

  # Workaround since Code.Typespec.typespec_to_quoted/1 is private
  def typespec_to_quoted(type) do
    {:"::", [], [_, quoted]} = Typespec.type_to_quoted({:fake_var, type, []})
    quoted
  end

  def extract_param_options(mod, fun, npar) do
    get_function_specs(mod, fun)
    |> get_param_type_specs(npar)
    |> expand_type_specs(mod)
    |> Enum.filter(&list_type_spec?/1)
    |> extract_list_type_spec_options()
  end

  def get_type_info(module, type, original_module) do
    module
    |> extract_type_def_info(type)
    |> build_type_info(type, original_module)
  end

  # Built-in types
  defp build_type_info({nil, name, n_args}, type, _) do
    spec_ast = BuiltinTypes.get_builtin_type_spec(name, n_args)
    spec = format_type_spec_ast(spec_ast, :type, line_length: @param_option_spec_line_length)
    doc = BuiltinTypes.get_builtin_type_doc(to_string(name), n_args)

    %{
      origin: "",
      type_spec: type_str(type),
      doc: doc,
      expanded_spec: spec
    }
  end

  # Custom Types
  defp build_type_info({module, name, n_args}, type, original_module) do
    {mod, expanded_type} = expand_type_spec(type, module)

    type_spec =
      if original_module == module || match?({:remote_type, _, _}, type) do
        type_str(type)
      else
        "#{inspect(mod)}.#{type_str(type)}"
      end

    %{
      origin: inspect(module),
      type_spec: type_spec,
      doc: get_type_doc_desc(module, name, n_args),
      expanded_spec:
        expanded_type |> format_type_spec(line_length: @param_option_spec_line_length)
    }
  end

  # Inline, non-existent
  defp build_type_info(_, type, _) do
    %{
      origin: "",
      type_spec: type_str(type),
      doc: "",
      expanded_spec: ""
    }
  end

  def type_str(type) do
    typespec_to_quoted(type) |> Macro.to_string()
  end

  defp extract_list_type_spec_options(list_type_specs) do
    Enum.flat_map(list_type_specs, fn type_spec ->
      type_spec
      |> expand_list_type_spec()
      |> extract_union_options_name_and_type()
    end)
  end

  defp get_param_type_specs(func_specs, npar) do
    for func_spec <- func_specs,
        params_types <- extract_params_types_variants(func_spec),
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
    type = get_type_spec(module, type_name, length(type_args))
    {module, type}
  end

  defp expand_type_spec({:type, _, :list, [_ | _]} = type, module) do
    {module, type}
  end

  defp expand_type_spec({:type, _, _, _} = type, module) do
    {module, {:not_found, {nil, type, []}}}
  end

  defp expand_type_spec(
         {:remote_type, _, [{:atom, _, remote_mod}, {:atom, _, type_name}, type_args]} = type,
         _module
       ) do
    case get_type_spec(remote_mod, type_name, length(type_args)) do
      nil ->
        {:not_found, type}

      type_found ->
        {remote_mod, type_found}
    end
  end

  defp expand_type_spec(type, module) do
    {module, type}
  end

  defp expand_list_type_spec({mod, {:type, _, :list, [type]}}) do
    expand_type_spec(type, mod)
  end

  defp expand_list_type_spec({mod, {_kind, {_name, {:type, _, :list, [type]}, _}}}) do
    expand_type_spec(type, mod)
  end

  # More than one option (union)
  defp extract_union_options_name_and_type(
         {mod, {_kind, {_name, {:type, _, :union, options_types}, _}}}
       ) do
    options_types
    |> Enum.map(&extract_tagged_tuple_name_and_type({mod, &1}))
    |> List.flatten()
  end

  # Only one option (not actually a union)
  defp extract_union_options_name_and_type(
         {mod, {_kind, {_name, {:type, _, :tuple, _} = type, _}}}
       ) do
    extract_tagged_tuple_name_and_type({mod, type})
  end

  defp extract_union_options_name_and_type({mod, {_kind, {_, {:atom, _, name}, _}}}) do
    [{mod, name}]
  end

  defp extract_union_options_name_and_type(
         {mod, {_kind, {_name, {:remote_type, _, _} = type, _}}}
       ) do
    extract_tagged_tuple_name_and_type({mod, type})
  end

  defp extract_union_options_name_and_type(
         {mod, {_kind, {_name, {:user_type, _, _, _} = type, _}}}
       ) do
    extract_tagged_tuple_name_and_type({mod, type})
  end

  defp extract_union_options_name_and_type({mod, {:atom, _, atom}}) when is_atom(atom) do
    [{mod, atom}]
  end

  # skip unknown type
  defp extract_union_options_name_and_type(_) do
    []
  end

  defp extract_tagged_tuple_name_and_type({mod, {:type, _, :tuple, [{:atom, _, name}, type]}}) do
    [{mod, name, type}]
  end

  defp extract_tagged_tuple_name_and_type({mod, type}) do
    case expand_type_spec(type, mod) do
      {_mod, {_kind, {_name, {:type, _, :union, _}, _}}} = expanded_type ->
        extract_union_options_name_and_type(expanded_type)

      {mod, {:atom, _, name}} ->
        [{mod, name}]

      _ ->
        []
    end
  end

  defp extract_type_def_info(_mod, {:type, _, :list, [_ | _]}) do
    :inline_list
  end

  defp extract_type_def_info(_mod, {:type, _, type_name, args}) do
    {nil, type_name, length(args)}
  end

  defp extract_type_def_info(_mod, {:type, _, type_name}) do
    {nil, type_name, 0}
  end

  defp extract_type_def_info(
         _mod,
         {:remote_type, _, [{:atom, _, remote_mod}, {:atom, _, name}, args]}
       ) do
    remote_mod = if remote_mod == :elixir, do: nil, else: remote_mod
    {remote_mod, name, length(args)}
  end

  defp extract_type_def_info(mod, {_, _, type_name, args}) do
    {mod, type_name, length(args)}
  end

  defp extract_type_def_info(_, _) do
    :not_found
  end

  defp list_type_spec?({_mod, {:type, _, :list, [_]}}) do
    true
  end

  defp list_type_spec?({_mod, {_, {_, {:type, _, :list, [_]}, _}}}) do
    true
  end

  defp list_type_spec?(_) do
    false
  end

  defp extract_params_types_variants({:spec, {_, list}}) do
    list
    |> Enum.map(&extract_params_types/1)
  end

  defp extract_params_types({:type, _, :fun, [{:type, _, :product, params_types}, _]}) do
    params_types
  end

  defp extract_params_types({:type, _, :bounded_fun, [type, constraints]}) do
    {:type, _, :fun, [{:type, _, :product, params}, _]} = type

    vars_types =
      for {:type, _, :constraint, [{:atom, _, :is_subtype}, [{:var, _, var}, var_type]]} <-
            constraints,
          into: %{} do
        {var, var_type}
      end

    params
    |> Enum.map(&expand_var_types(&1, vars_types, []))
    # reject failed expansions
    |> Enum.reject(&is_nil/1)
  end

  defp expand_var_types(var_type, vars_types, expanded_types) do
    if var_type in expanded_types do
      # break recursive type expansion
      nil
    else
      do_expand_var_types(var_type, vars_types, [var_type | expanded_types])
    end
  end

  defp do_expand_var_types({:var, _, name}, vars_types, expanded_types) do
    expand_var_types(vars_types[name], vars_types, expanded_types)
  end

  defp do_expand_var_types({:type, l, kind, tuple_elements}, vars_types, expanded_types)
       when kind in [:list, :tuple, :union] and is_list(tuple_elements) do
    expanded =
      for(element <- tuple_elements, do: expand_var_types(element, vars_types, expanded_types))
      # reject failed expansions
      |> Enum.reject(&is_nil/1)

    {:type, l, kind, expanded}
  end

  defp do_expand_var_types({:ann_type, _l, [{:var, _, _}, type]}, vars_types, expanded_types) do
    expand_var_types(type, vars_types, expanded_types)
  end

  defp do_expand_var_types(type, _vars_types, _expanded_types) do
    type
  end

  defp starts_with_type_def?(str, kind) do
    str
    |> String.trim_leading()
    |> String.split("#{kind} ")
    |> (&match?([_, _ | _], &1)).()
  end
end
