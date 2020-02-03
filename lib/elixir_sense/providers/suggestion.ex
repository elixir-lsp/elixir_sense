defmodule ElixirSense.Providers.Suggestion do
  @moduledoc """
  Provider responsible for finding suggestions for auto-completing
  """

  alias Alchemist.Helpers.Complete

  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State
  alias ElixirSense.Core.State.StructInfo
  alias ElixirSense.Core.TypeInfo

  @type attribute :: %{
          type: :attribute,
          name: String.t()
        }

  @type variable :: %{
          type: :var,
          name: String.t()
        }

  @type field :: %{
          type: :field,
          name: String.t(),
          origin: String.t()
        }

  @type return :: %{
          type: :return,
          description: String.t(),
          spec: String.t(),
          snippet: String.t()
        }

  @type callback :: %{
          type: :callback,
          name: String.t(),
          arity: non_neg_integer,
          args: String.t(),
          origin: String.t(),
          summary: String.t(),
          spec: String.t()
        }

  @type protocol_function :: %{
          type: :protocol_function,
          name: String.t(),
          arity: non_neg_integer,
          args: String.t(),
          origin: String.t(),
          summary: String.t(),
          spec: String.t()
        }

  @type func :: %{
          type: :function,
          name: String.t(),
          arity: non_neg_integer,
          args: String.t(),
          origin: String.t(),
          summary: String.t(),
          spec: String.t()
        }

  @type mod :: %{
          type: :module,
          name: String.t(),
          subtype: String.t(),
          summary: String.t()
        }

  @type param_option :: %{
          type: :param_option,
          name: String.t(),
          origin: String.t(),
          type_spec: String.t(),
          doc: String.t(),
          expanded_spec: String.t()
        }

  @type type_spec :: %{
          type: :type_spec,
          name: String.t(),
          arity: non_neg_integer,
          origin: String.t(),
          spec: String.t(),
          doc: String.t(),
          signature: String.t()
        }

  @type hint :: %{
          type: :hint,
          value: String.t()
        }

  @type suggestion ::
          attribute
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
  @spec find(
          String.t(),
          State.Env.t(),
          State.structs_t(),
          State.mods_funs_to_positions_t(),
          State.types_t(),
          State.specs_t(),
          String.t()
        ) :: [suggestion]
  def find(
        hint,
        %State.Env{} = env,
        structs,
        mods_and_funs,
        metadata_types,
        metadata_specs,
        text_before
      ) do
    case find_struct_fields(
           hint,
           text_before,
           env,
           structs,
           mods_and_funs,
           metadata_types
         ) do
      {[], _} ->
        find_all_except_struct_fields(
          hint,
          env,
          mods_and_funs,
          metadata_types,
          metadata_specs,
          text_before
        )

      {fields, nil} ->
        [%{type: :hint, value: "#{hint}"} | fields]

      {fields, :maybe_struct_update} ->
        # TODO refactor hint generation
        [_hint | rest] =
          find_mods_funs_vars_attributes(
            hint,
            env,
            mods_and_funs,
            metadata_specs,
            text_before
          )

        [%{type: :hint, value: "#{hint}"} | fields ++ rest]
    end
  end

  @spec find_all_except_struct_fields(
          String.t(),
          State.Env.t(),
          State.mods_funs_to_positions_t(),
          State.types_t(),
          State.specs_t(),
          String.t()
        ) :: [suggestion]
  defp find_all_except_struct_fields(
         hint,
         %State.Env{
           imports: imports,
           aliases: aliases,
           vars: vars,
           attributes: attributes,
           behaviours: behaviours,
           scope: scope,
           module: module,
           protocol: protocol
         },
         mods_and_funs,
         metadata_types,
         metadata_specs,
         text_before
       ) do
    vars = Enum.map(vars, fn v -> v.name end)

    %{hint: hint_suggestion, suggestions: mods_and_funcs} =
      find_hint_mods_funcs(
        hint,
        imports,
        aliases,
        module,
        mods_and_funs,
        metadata_specs,
        text_before
      )

    callbacks_or_returns =
      case scope do
        {_f, _a} ->
          find_returns(behaviours, protocol, hint, scope)

        _mod ->
          find_callbacks(behaviours, protocol, hint) ++ find_protocol_functions(protocol, hint)
      end

    [hint_suggestion]
    |> Kernel.++(callbacks_or_returns)
    |> Kernel.++(find_attributes(attributes, hint))
    |> Kernel.++(find_vars(vars, hint))
    |> Kernel.++(mods_and_funcs)
    |> Kernel.++(
      find_param_options(
        text_before,
        hint,
        imports,
        aliases,
        module,
        mods_and_funs,
        metadata_types
      )
    )
    |> Kernel.++(find_typespecs(hint, aliases, module, scope, mods_and_funs, metadata_types))
    |> Enum.uniq()
  end

  @spec find_mods_funs_vars_attributes(
          String.t(),
          State.Env.t(),
          State.mods_funs_to_positions_t(),
          State.specs_t(),
          String.t()
        ) :: [suggestion]
  defp find_mods_funs_vars_attributes(
         hint,
         %State.Env{
           imports: imports,
           aliases: aliases,
           vars: vars,
           attributes: attributes,
           module: module
         },
         mods_and_funs,
         metadata_specs,
         text_before
       ) do
    vars = Enum.map(vars, fn v -> v.name end)

    %{hint: hint_suggestion, suggestions: mods_and_funcs} =
      find_hint_mods_funcs(
        hint,
        imports,
        aliases,
        module,
        mods_and_funs,
        metadata_specs,
        text_before
      )

    [hint_suggestion]
    |> Kernel.++(find_attributes(attributes, hint))
    |> Kernel.++(find_vars(vars, hint))
    |> Kernel.++(mods_and_funcs)
  end

  defp expand_current_module(:__MODULE__, current_module), do: current_module
  defp expand_current_module(module, _current_module), do: module

  @spec find_struct_fields(
          String.t(),
          String.t(),
          State.Env.t(),
          State.structs_t(),
          State.mods_funs_to_positions_t(),
          State.types_t()
        ) :: {[suggestion], nil | :maybe_struct_update}
  defp find_struct_fields(
         hint,
         text_before,
         %State.Env{
           imports: imports,
           aliases: aliases,
           module: module
         },
         structs,
         mods_funs,
         metadata_types
       ) do
    with {mod, fields_so_far, elixir_prefix} when mod != :_ <-
           Source.which_struct(text_before, module),
         {actual_mod, _, true} <-
           Introspection.actual_mod_fun(
             {expand_current_module(mod, module), nil},
             imports,
             if(elixir_prefix, do: [], else: aliases),
             module,
             mods_funs,
             metadata_types
           ),
         true <- Introspection.module_is_struct?(actual_mod) or Map.has_key?(structs, actual_mod) do
      fields =
        if Introspection.module_is_struct?(actual_mod) do
          actual_mod
          |> struct()
          |> Map.from_struct()
          |> Map.keys()
          |> Kernel.++([:__struct__])
        else
          %StructInfo{fields: fields} = structs[actual_mod]
          Enum.map(fields, &(&1 |> elem(0)))
        end

      result =
        fields
        |> Kernel.--(fields_so_far)
        |> Enum.filter(fn field -> String.starts_with?("#{field}", hint) end)
        |> Enum.map(fn field ->
          %{
            type: :field,
            name: Atom.to_string(field),
            origin: inspect(actual_mod)
          }
        end)

      {result, if(fields_so_far == [], do: :maybe_struct_update)}
    else
      {:_, fields_so_far, false} when is_list(fields_so_far) ->
        result =
          [:__struct__]
          |> Kernel.--(fields_so_far)
          |> Enum.filter(fn field -> String.starts_with?("#{field}", hint) end)
          |> Enum.map(fn field ->
            %{
              type: :field,
              name: Atom.to_string(field),
              origin: ""
            }
          end)

        {result, if(fields_so_far == [], do: :maybe_struct_update)}

      _ ->
        {[], nil}
    end
  end

  @spec find_hint_mods_funcs(
          String.t(),
          [module],
          [{module, module}],
          module,
          State.mods_funs_to_positions_t(),
          State.specs_t(),
          String.t()
        ) ::
          %{
            hint: hint,
            suggestions: [mod | func]
          }
  defp find_hint_mods_funcs(
         hint,
         imports,
         aliases,
         module,
         mods_and_funs,
         metadata_specs,
         text_before
       ) do
    env = %Complete.Env{
      aliases: aliases,
      scope_module: module,
      imports: imports,
      mods_and_funs: mods_and_funs,
      specs: metadata_specs
    }

    {hint, prefix} =
      case Source.get_v12_module_prefix(text_before, module) do
        nil ->
          {hint, ""}

        module_string ->
          # v1.2 alias syntax detected
          # prepend module prefix before running completion
          prefix = module_string <> "."
          {prefix <> hint, prefix}
      end

    {hint, module_special_form_replaced} =
      if String.starts_with?(hint, "__MODULE__") do
        {hint |> String.replace_leading("__MODULE__", inspect(module)), true}
      else
        {hint, false}
      end

    {%{type: :hint, value: prefixed_value}, suggestions} = Complete.run(hint, env)

    prefixed_value =
      if module_special_form_replaced do
        prefixed_value |> String.replace_leading(inspect(module), "__MODULE__")
      else
        prefixed_value
      end

    # drop module prefix from hint if added
    value =
      if prefix != "" do
        prefixed_value |> String.replace_leading(prefix, "")
      else
        prefixed_value
      end

    %{hint: %{type: :hint, value: value}, suggestions: suggestions}
  end

  @spec find_vars([String.t()], String.t()) :: [variable]
  defp find_vars(vars, hint) do
    for var <- vars, hint == "" or String.starts_with?("#{var}", hint) do
      %{type: :variable, name: Atom.to_string(var)}
    end
    |> Enum.sort()
  end

  @spec find_attributes([State.AttributeInfo.t()], String.t()) :: [attribute]
  defp find_attributes(attributes, hint) do
    for %State.AttributeInfo{name: name} <- attributes,
        hint in ["", "@"] or String.starts_with?("@#{name}", hint) do
      %{type: :attribute, name: "@#{name}"}
    end
    |> Enum.sort()
  end

  @spec find_returns([module], nil | State.protocol_t(), String.t(), State.scope()) :: [return]
  defp find_returns(behaviours, protocol, "", {fun, arity}) do
    callbacks =
      for mod <- behaviours,
          protocol == nil or mod != elem(protocol, 0),
          Introspection.define_callback?(mod, fun, arity),
          return <- Introspection.get_returns_from_callback(mod, fun, arity) do
        %{
          type: :return,
          description: return.description,
          spec: return.spec,
          snippet: return.snippet
        }
      end

    protocol_functions =
      case protocol do
        {proto, _implementations} ->
          if Introspection.define_callback?(proto, fun, arity) do
            for return <- Introspection.get_returns_from_callback(proto, fun, arity) do
              %{
                type: :return,
                description: return.description,
                spec: return.spec,
                snippet: return.snippet
              }
            end
          else
            []
          end

        nil ->
          []
      end

    callbacks ++ protocol_functions
  end

  defp find_returns(_behaviours, _protocol, _hint, _module) do
    []
  end

  @spec find_callbacks([module], nil | State.protocol_t(), String.t()) :: [callback]
  defp find_callbacks(behaviours, protocol, hint) do
    behaviours
    |> Enum.flat_map(fn
      mod when is_atom(mod) and (protocol == nil or mod != elem(protocol, 0)) ->
        mod_name = inspect(mod)

        for %{name: name, arity: arity, callback: spec, signature: signature, doc: doc} <-
              Introspection.get_callbacks_with_docs(mod),
            hint == "" or String.starts_with?("#{name}", hint) do
          desc = Introspection.extract_summary_from_docs(doc)
          [_, args_str] = Regex.run(Regex.recompile!(~r/.\((.*)\)/), signature)
          args = args_str |> String.replace(Regex.recompile!(~r/\s/), "")

          %{
            type: :callback,
            name: Atom.to_string(name),
            arity: arity,
            args: args,
            origin: mod_name,
            summary: desc,
            spec: spec
          }
        end

      _ ->
        []
    end)
    |> Enum.sort()
  end

  @spec find_protocol_functions(nil | State.protocol_t(), String.t()) :: [protocol_function]
  defp find_protocol_functions(nil, _hint), do: []

  defp find_protocol_functions({protocol, _implementations}, hint) do
    for {{name, arity}, {_type, args, docs, spec}} <-
          Introspection.module_functions_info(protocol),
        hint == "" or String.starts_with?("#{name}", hint) do
      %{
        type: :protocol_function,
        name: Atom.to_string(name),
        arity: arity,
        args: args,
        origin: inspect(protocol),
        summary: docs,
        spec: spec
      }
    end
    |> Enum.sort()
  end

  @spec find_param_options(
          String.t(),
          String.t(),
          [module],
          [{module, module}],
          module,
          State.mods_funs_to_positions_t(),
          State.types_t()
        ) ::
          [
            param_option
          ]
  defp find_param_options(prefix, hint, imports, aliases, module, mods_funs, metadata_types) do
    with %{
           candidate: {mod, fun},
           elixir_prefix: elixir_prefix,
           npar: npar,
           pipe_before: _pipe_before
         } <-
           Source.which_func(prefix, module),
         {mod, fun, true} <-
           Introspection.actual_mod_fun(
             {mod, fun},
             imports,
             if(elixir_prefix, do: [], else: aliases),
             module,
             mods_funs,
             metadata_types
           ) do
      TypeInfo.extract_param_options(mod, fun, npar)
      |> options_to_suggestions(mod)
      |> Enum.filter(&String.starts_with?("#{&1.name}", hint))
    else
      _ ->
        []
    end
  end

  defp options_to_suggestions(options, original_module) do
    Enum.map(options, fn
      {mod, name, type} ->
        TypeInfo.get_type_info(mod, type, original_module)
        |> Map.merge(%{type: :param_option, name: name})

      {mod, name} ->
        %{
          doc: "",
          expanded_spec: "",
          name: name,
          origin: inspect(mod),
          type: :param_option,
          type_spec: ""
        }
    end)
  end

  @spec find_typespecs(
          String.t(),
          [{module, module}],
          module,
          State.scope(),
          State.mods_funs_to_positions_t(),
          State.types_t()
        ) ::
          [
            type_spec
          ]

  # We don't list typespecs when inside a function
  defp find_typespecs(_hint, _aliases, _module, {_m, _f}, _, _) do
    []
  end

  defp find_typespecs(hint, aliases, module, _scope, mods_and_funs, metadata_types) do
    {mod, hint} =
      hint
      |> Source.split_module_and_hint(module, aliases)

    find_typespecs_for_mod_and_hint({mod, hint}, aliases, module, mods_and_funs, metadata_types)
    |> Kernel.++(find_builtin_types({mod, hint}))
  end

  defp find_typespecs_for_mod_and_hint(
         {mod, hint},
         aliases,
         module,
         mods_and_funs,
         metadata_types
       ) do
    case Introspection.actual_module(mod, aliases, module, mods_and_funs) do
      {actual_mod, true} ->
        find_module_types(actual_mod, {mod, hint}, metadata_types, module)

      {nil, false} ->
        find_module_types(module, {mod, hint}, metadata_types, module)

      {_, false} ->
        []
    end
  end

  defp find_builtin_types({nil, hint}) do
    TypeInfo.find_all_builtin(&String.starts_with?("#{&1.name}", hint))
    |> Enum.map(&type_info_to_suggestion(&1, nil))
  end

  defp find_builtin_types({_mod, _hint}), do: []

  defp find_module_types(actual_mod, {mod, hint}, metadata_types, module) do
    find_metadata_types(actual_mod, {mod, hint}, metadata_types, module)
    |> Kernel.++(TypeInfo.find_all(actual_mod, &String.starts_with?("#{&1.name}", hint)))
    |> Enum.map(&type_info_to_suggestion(&1, actual_mod))
  end

  defp find_metadata_types(actual_mod, {mod, hint}, metadata_types, module) do
    include_private = mod == nil and actual_mod == module

    for {{mod, type, arity}, type_info} when is_integer(arity) <- metadata_types,
        mod == actual_mod,
        type |> Atom.to_string() |> String.starts_with?(hint),
        include_private or type_info.kind != :typep,
        do: type_info
  end

  defp type_info_to_suggestion(type_info, module) do
    origin = if module, do: inspect(module), else: ""

    case type_info do
      %ElixirSense.Core.State.TypeInfo{args: [args]} ->
        args_stringified = Enum.join(args, ", ")

        %{
          type: :type_spec,
          name: type_info.name |> Atom.to_string(),
          arity: length(args),
          signature: "#{type_info.name}(#{args_stringified})",
          origin: origin,
          doc: "",
          spec: ""
        }

      _ ->
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
end
