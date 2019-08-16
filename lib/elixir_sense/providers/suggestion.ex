defmodule ElixirSense.Providers.Suggestion do

  @moduledoc """
  Provider responsible for finding suggestions for auto-completing
  """

  alias Alchemist.Helpers.Complete
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.TypeInfo
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State

  @type attribute :: %{
    type: :attribute,
    name: String.t
  }

  @type variable :: %{
    type: :var,
    name: String.t
  }

  @type field :: %{
    type: :field,
    name: String.t,
    origin: String.t,
  }

  @type return :: %{
    type: :return,
    description: String.t,
    spec: String.t,
    snippet: String.t,
  }

  @type callback :: %{
    type: :callback,
    name: String.t,
    arity: non_neg_integer,
    args: String.t,
    origin: String.t,
    summary: String.t,
    spec: String.t
  }

  @type protocol_function :: %{
    type: :protocol_function,
    name: String.t,
    arity: non_neg_integer,
    args: String.t,
    origin: String.t,
    summary: String.t,
    spec: String.t
  }

  @type func :: %{
    type: :function,
    name: String.t,
    arity: non_neg_integer,
    args: String.t,
    origin: String.t,
    summary: String.t,
    spec: String.t
  }

  @type mod :: %{
    type: :module,
    name: String.t,
    subtype: String.t,
    summary: String.t
  }

  @type param_option :: %{
    type: :param_option,
    name: String.t,
    origin: String.t,
    type_spec: String.t,
    doc: String.t,
    expanded_spec: String.t
  }

  @type type_spec :: %{
    type: :type_spec,
    name: String.t,
    arity: non_neg_integer,
    origin: String.t,
    spec: String.t,
    doc: String.t,
    signature: String.t
  }

  @type hint :: %{
    type: :hint,
    value: String.t
  }

  @type suggestion :: attribute
                    | variable
                    | field
                    | return
                    | callback
                    | protocol_function
                    | func
                    | mod
                    | hint
                    | param_option
                    | type_spec

  @doc """
  Finds all suggestions for a hint based on context information.
  """
  @spec find(String.t, [module], [{module, module}], module, [String.t], [String.t], [module], State.scope, any, %{}, %{}, String.t) :: [suggestion]
  def find(hint, imports, aliases, module, vars, attributes, behaviours, scope, protocol, mods_and_funs, structs,  text_before) do
    case find_struct_fields(hint, text_before, imports, aliases, module, structs) do
      {[], _} ->
        find_all_except_struct_fields(hint, imports, aliases, vars, attributes, behaviours, scope, module, protocol, mods_and_funs, text_before)

      {fields, nil} ->
        [%{type: :hint, value: "#{hint}"} | fields]
      {fields, :maybe_struct_update} ->
        # TODO refactor hint generation
        [_hint | rest] = find_mods_funs_vars_attributes(hint, imports, aliases, vars, attributes, module, mods_and_funs)
        [%{type: :hint, value: "#{hint}"} | fields ++ rest]
    end
  end

  @spec find_all_except_struct_fields(String.t, [module], [{module, module}], [String.t], [String.t], [module], State.scope, module, any, %{}, String.t) :: [suggestion]
  defp find_all_except_struct_fields(hint, imports, aliases, vars, attributes, behaviours, scope, module, protocol, mods_and_funs, text_before) do
    vars = Enum.map(vars, fn v -> v.name end)
    %{hint: hint_suggestion, suggestions: mods_and_funcs} = find_hint_mods_funcs(hint, imports, aliases, module, mods_and_funs)

    callbacks_or_returns =
      case scope do
        {_f, _a} -> find_returns(behaviours, hint, scope)
        _mod   -> find_callbacks(behaviours, hint) ++ find_protocol_functions(protocol, hint)
      end

    [hint_suggestion]
    |> Kernel.++(callbacks_or_returns)
    |> Kernel.++(find_attributes(attributes, hint))
    |> Kernel.++(find_vars(vars, hint))
    |> Kernel.++(mods_and_funcs)
    |> Kernel.++(find_param_options(text_before, hint, imports, aliases, module))
    |> Kernel.++(find_typespecs(hint, aliases, module, scope))
    |> Enum.uniq_by(&(&1))
  end

  defp find_mods_funs_vars_attributes(hint, imports, aliases, vars, attributes, module, mods_and_funs) do
    vars = Enum.map(vars, fn v -> v.name end)
    %{hint: hint_suggestion, suggestions: mods_and_funcs} = find_hint_mods_funcs(hint, imports, aliases, module, mods_and_funs)

    [hint_suggestion]
    |> Kernel.++(find_attributes(attributes, hint))
    |> Kernel.++(find_vars(vars, hint))
    |> Kernel.++(mods_and_funcs)
  end

  defp find_struct_fields(hint, text_before, imports, aliases, module, structs) do
    with \
      {mod, fields_so_far} <- Source.which_struct(text_before),
      {actual_mod, _}      <- Introspection.actual_mod_fun({mod, nil}, imports, aliases, module),
      true                 <- Introspection.module_is_struct?(actual_mod) or Map.has_key?(structs, actual_mod)
    do
      fields = if Introspection.module_is_struct?(actual_mod) do
        actual_mod
        |> struct()
        |> Map.from_struct()
        |> Map.keys()
      else
        structs[actual_mod] |> elem(1) |> Enum.map(& &1 |> elem(0))
      end

      result = fields
      |> Kernel.--(fields_so_far)
      |> Enum.filter(fn field -> String.starts_with?("#{field}", hint)end)
      |> Enum.map(fn field -> %{type: :field, name: field, origin: Introspection.module_to_string(actual_mod)} end)
      {result, if(fields_so_far == [], do: :maybe_struct_update)}
    else
      _ -> {[], nil}
    end
  end

  @spec find_hint_mods_funcs(String.t, [module], [{module, module}], module, %{}) :: %{hint: hint, suggestions: [mod | func]}
  defp find_hint_mods_funcs(hint, imports, aliases, module, mods_and_funs) do
    env = %Complete.Env{
      aliases: aliases,
      scope_module: module,
      imports: imports,
      mods_and_funs: mods_and_funs
    }

    {hint_suggestion, suggestions} = Complete.run(hint, env)
    %{hint: hint_suggestion, suggestions: suggestions}
  end

  @spec find_vars([String.t], String.t) :: [variable]
  defp find_vars(vars, hint) do
    for var <- vars, hint == "" or String.starts_with?("#{var}", hint) do
      %{type: :variable, name: var}
    end |> Enum.sort
  end

  @spec find_attributes([String.t], String.t) :: [attribute]
  defp find_attributes(attributes, hint) do
    for attribute <- attributes, hint in ["", "@"] or String.starts_with?("@#{attribute}", hint) do
      %{type: :attribute, name: "@#{attribute}"}
    end |> Enum.sort
  end

  @spec find_returns([module], String.t, State.scope) :: [return]
  defp find_returns(behaviours, "", {fun, arity}) do
    for mod <- behaviours, Introspection.define_callback?(mod, fun, arity) do
      for return <- Introspection.get_returns_from_callback(mod, fun, arity) do
        %{type: :return, description: return.description, spec: return.spec, snippet: return.snippet}
      end
    end |> List.flatten
  end
  defp find_returns(_behaviours, _hint, _module) do
    []
  end

  @spec find_callbacks([module], String.t) :: [callback]
  defp find_callbacks(behaviours, hint) do
    behaviours |> Enum.flat_map(fn mod ->
      mod_name = mod |> Introspection.module_to_string
      for %{name: name, arity: arity, callback: spec, signature: signature, doc: doc} <- Introspection.get_callbacks_with_docs(mod),
          hint == "" or String.starts_with?("#{name}", hint)
      do
        desc = Introspection.extract_summary_from_docs(doc)
        [_, args_str] = Regex.run(Regex.recompile!(~r/.\((.*)\)/), signature)
        args = args_str |> String.replace(Regex.recompile!(~r/\s/), "")
        %{type: :callback, name: name, arity: arity, args: args, origin: mod_name, summary: desc, spec: spec}
      end
    end) |> Enum.sort
  end

  defp find_protocol_functions(nil, _hint), do: []
  defp find_protocol_functions({protocol, _implementations}, hint) do
    for {{name, arity}, {_type, args, docs, spec}} <- Introspection.module_functions_info(protocol),
    hint == "" or String.starts_with?("#{name}", hint)
    do
      %{type: :protocol_function, name: name, arity: arity, args: args, origin: Introspection.module_to_string(protocol), summary: docs, spec: spec}
    end
    |> Enum.sort
  end

  @spec find_param_options(String.t, String.t, [module], [{module, module}], module) :: [param_option]
  defp find_param_options(prefix, hint, imports, aliases, module) do
    case Source.which_func(prefix) do
      %{candidate: {mod, fun}, npar: npar, pipe_before: _pipe_before} ->
        {mod, fun} = Introspection.actual_mod_fun({mod, fun}, imports, aliases, module)
        TypeInfo.extract_param_options(mod, fun, npar)
        |> options_to_suggestions(mod)
        |> Enum.filter(&String.starts_with?("#{&1.name}", hint))
      _ ->
        []
    end
  end

  defp options_to_suggestions(options, original_module) do
    Enum.map(options, fn {mod, name, type} ->
      TypeInfo.get_type_info(mod, type, original_module)
      |> Map.merge(%{type: :param_option, name: name})
    end)
  end

  # We don't list typespecs when inside a function
  defp find_typespecs(_hint, _aliases, _module, {_m, _f}) do
    []
  end

  defp find_typespecs(hint, aliases, module, _scope) do
    hint
    |> Source.split_module_and_hint(aliases)
    |> find_typespecs_for_mod_and_hint(aliases, module)
  end

  defp find_typespecs_for_mod_and_hint({_, nil}, _aliases, _module) do
    []
  end

  defp find_typespecs_for_mod_and_hint({nil, hint}, aliases, module) when not is_nil(module) do
    local_module = find_typespecs_for_mod_and_hint({module, hint}, aliases, module)

    builtin_modules =
      TypeInfo.find_all_builtin(&String.starts_with?("#{&1.name}", hint))
      |> Enum.map(&type_info_to_suggestion(&1, nil))

    local_module ++ builtin_modules
  end

  defp find_typespecs_for_mod_and_hint({mod, hint}, aliases, _module) do
    actual_mod = Introspection.actual_module(mod, aliases)

    actual_mod
    |> TypeInfo.find_all(&String.starts_with?("#{&1.name}", hint))
    |> Enum.map(&type_info_to_suggestion(&1, actual_mod))
  end

  defp type_info_to_suggestion(type_info, module) do
    origin =
      if module do
        Introspection.module_to_string(module)
      else
        ""
      end
    %{
      type: :type_spec,
      name: type_info.name,
      arity: type_info.arity,
      signature: type_info.signature,
      origin: origin,
      doc: type_info.doc,
      spec: type_info.spec
    }
  end
end
