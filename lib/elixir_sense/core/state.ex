defmodule ElixirSense.Core.State do
  @moduledoc """
  Core State
  """

  @type fun_arity :: {atom, non_neg_integer}
  @type scope :: module | fun_arity

  defstruct namespace: [[:"Elixir"]],
            scopes: [[:"Elixir"]],
            imports: [[]],
            requires: [[]],
            aliases: [[]],
            attributes: [[]],
            protocols: [[]],
            scope_attributes: [[]],
            behaviours: [[]],
            scope_behaviours: [[]],
            vars: [[]],
            scope_vars: [[]],
            scope_id_count: 0,
            scope_ids: [0],
            vars_info_per_scope_id: %{},
            mods_funs_to_positions: %{},
            mods_funs: %{},
            lines_to_env: %{},
            calls: %{},
            structs: %{}

  defmodule Env do
    @moduledoc false
    defstruct imports: [],
              requires: [],
              aliases: [],
              # NOTE for protocol impementation this will be the first variant
              module: nil,
              module_variants: [],
              # NOTE for protocol impementation this will be the first variant
              protocol: nil,
              protocol_variants: nil,
              vars: [],
              attributes: [],
              behaviours: [],
              scope: nil,
              scope_id: nil,
              structs: %{}
  end

  defmodule VarInfo do
    @moduledoc false
    defstruct name: nil, positions: [], scope_id: nil, is_definition: nil
  end

  defmodule ModFunInfo do
    @moduledoc false
    defstruct type: nil
  end

  alias ElixirSense.Core.Introspection

  def current_aliases(state) do
    state.aliases |> List.flatten() |> Enum.uniq_by(&elem(&1, 0)) |> Enum.reverse()
  end

  def get_current_env(state) do
    current_module_variants = get_current_module_variants(state)
    current_imports = state.imports |> :lists.reverse() |> List.flatten()
    current_requires = state.requires |> :lists.reverse() |> List.flatten()
    current_aliases = current_aliases(state)
    current_vars = state |> get_current_vars()
    current_attributes = state.scope_attributes |> :lists.reverse() |> List.flatten()
    current_behaviours = hd(state.behaviours)
    current_scope = hd(hd(state.scopes))
    current_scope_id = hd(state.scope_ids)
    current_scope_protocols = hd(state.protocols)

    %Env{
      imports: current_imports,
      requires: current_requires,
      aliases: current_aliases,
      module: current_module_variants |> hd,
      module_variants: current_module_variants,
      vars: current_vars,
      attributes: current_attributes,
      behaviours: current_behaviours,
      # NOTE for protocol implementations the scope and namespace will be
      # escaped with `escape_protocol_implemntations`
      scope: current_scope,
      scope_id: current_scope_id,
      protocol:
        case current_scope_protocols do
          [] -> nil
          [head | _] -> head
        end,
      protocol_variants: current_scope_protocols,
      structs: state.structs
    }
  end

  def get_current_module(state) do
    get_current_module_variants(state) |> hd
  end

  def get_current_module_variants(state = %{protocols: [[] | _]}) do
    state.namespace |> hd |> unescape_protocol_impementations
  end

  def get_current_module_variants(%{protocols: [protocols | _]}) do
    for {protocol, implementations} <- protocols,
        implementation <- implementations do
      Module.concat(protocol, implementation)
    end
  end

  def add_current_env_to_line(state, line) when is_integer(line) do
    env = get_current_env(state)
    %{state | lines_to_env: Map.put(state.lines_to_env, line, env)}
  end

  def add_call_to_line(state, {mod, func, arity}, line, col) do
    call = %{mod: mod, func: func, arity: arity, line: line, col: col}

    calls =
      Map.update(state.calls, line, [call], fn line_calls ->
        [call | line_calls]
      end)

    %{state | calls: calls}
  end

  def add_struct(state, type, fields) do
    structs =
      get_current_module_variants(state)
      |> Enum.reduce(state.mods_funs, fn variant, acc ->
        acc |> Map.put(variant, {type, fields})
      end)

    %{state | structs: structs}
  end

  def get_scope_name(state, line) do
    case state.lines_to_env[line] do
      nil -> nil
      %Env{scope: scope} -> scope
    end
  end

  def get_current_scope_name(state) do
    case hd(hd(state.scopes)) do
      {fun, _} -> fun |> Atom.to_string()
      mod -> mod |> Atom.to_string()
    end
  end

  def get_current_vars(state) do
    state.scope_vars |> List.flatten() |> reduce_vars() |> Map.values()
  end

  def get_current_vars_refs(state) do
    state.scope_vars |> List.flatten()
  end

  def is_variable_defined(state, var_name) do
    state
    |> get_current_vars_refs()
    |> Enum.any?(fn %VarInfo{name: name, is_definition: is_definition} ->
      name == var_name && is_definition
    end)
  end

  def add_mod_fun_to_position(state, {module, fun, arity}, position, params) do
    current_info = Map.get(state.mods_funs_to_positions, {module, fun, arity}, %{})
    current_params = current_info |> Map.get(:params, [])
    current_positions = current_info |> Map.get(:positions, [])
    new_params = [params | current_params]
    new_positions = [position | current_positions]

    mods_funs_to_positions =
      Map.put(state.mods_funs_to_positions, {module, fun, arity}, %{
        positions: new_positions,
        params: new_params
      })

    %{state | mods_funs_to_positions: mods_funs_to_positions}
  end

  @dot_marker "(__dot__)"
  @or_marker "(__or__)"

  def escape_protocol_impementations({protocol, implementations}) do
    joined_implementations =
      implementations
      |> Enum.map_join(@or_marker, fn
        parts when is_list(parts) ->
          parts
          |> Enum.map_join(@dot_marker, &Atom.to_string/1)

        module when is_atom(module) ->
          Atom.to_string(module) |> String.replace("Elixir.", "")
      end)
      |> String.to_atom()

    List.wrap(protocol) ++ [joined_implementations]
  end

  def escape_protocol_impementations(module_parts), do: module_parts

  def unescape_protocol_impementations(module) when is_atom(module) do
    if Introspection.elixir_module?(module) do
      Module.split(module)
      |> Enum.reverse()
      |> Enum.map(&String.to_atom/1)
      |> unescape_protocol_impementations
    else
      [module]
    end
  end

  def unescape_protocol_impementations(parts) do
    parts
    |> Enum.reduce([[]], fn part, acc ->
      part_variants =
        part
        |> Atom.to_string()
        |> String.replace(@dot_marker, ".")
        |> String.split(@or_marker)

      for part_variant <- part_variants, acc_variant <- acc do
        [part_variant | acc_variant]
      end
    end)
    |> Enum.map(&Module.concat/1)
  end

  def new_namespace(state, module) do
    # TODO refactor to allow {:implementation, protocol, [implementations]} in scope
    module = escape_protocol_impementations(module)

    {namespace, scopes} =
      case module do
        [:"Elixir" | module] ->
          case state.namespace do
            [[:"Elixir"]] ->
              # top level module - drop prefix
              module_reversed = :lists.reverse(module)
              namespace = module_reversed ++ hd(state.namespace)
              scopes = module_reversed ++ hd(state.scopes)
              {namespace, scopes}

            [_ | _] ->
              # external submodule
              module_reversed = :lists.reverse(module)
              namespace = module_reversed
              scopes = module_reversed
              {namespace, scopes}
          end

        module when is_list(module) ->
          module_reversed = :lists.reverse(module)
          namespace = module_reversed ++ hd(state.namespace)
          scopes = module_reversed ++ hd(state.scopes)
          {namespace, scopes}

        module when is_atom(module) ->
          {module, [module]}
      end

    %{state | namespace: [namespace | state.namespace], scopes: [scopes | state.scopes]}
  end

  def remove_module_from_namespace(state, module) do
    namespace = state.namespace |> hd
    module = escape_protocol_impementations(module)
    outer_mods = state.namespace |> tl
    outer_scopes = state.scopes |> tl

    state = %{state | namespace: outer_mods, scopes: outer_scopes}

    if length(outer_scopes) > 1 and state.protocols |> hd == [] and is_list(module) do
      # submodule defined, create alias in outer module namespace

      # take only outermost submodule part as deeply nested submodules do not create aliases
      alias = module |> Enum.take(1) |> Module.concat()
      expanded = namespace |> Enum.drop(length(module) - 1) |> Enum.reverse() |> Module.concat()

      state
      |> add_alias({alias, expanded})
    else
      state
    end
  end

  def new_named_func(state, name, arity) do
    %{state | scopes: [[{name, arity} | hd(state.scopes)] | state.scopes]}
  end

  def maybe_add_protocol_implementation(state, {protocol, implementations}) do
    implementation_modules =
      implementations
      |> Enum.flat_map(fn
        module when is_list(module) ->
          expanded = expand_alias(state, Module.concat(module))
          unescape_protocol_impementations(expanded)

        module when is_atom(module) ->
          unescape_protocol_impementations(module)
      end)

    candidate =
      if is_list(protocol) do
        expand_alias(state, Module.concat(protocol))
      else
        protocol
      end

    protocols =
      unescape_protocol_impementations(candidate)
      |> Enum.map(&{&1, implementation_modules})

    %{state | protocols: [protocols | state.protocols]}
  end

  def maybe_add_protocol_implementation(state, _) do
    %{state | protocols: [[] | state.protocols]}
  end

  def remove_protocol_implementation(state) do
    %{state | protocols: tl(state.protocols)}
  end

  def remove_last_scope_from_scopes(state) do
    %{state | scopes: tl(state.scopes)}
  end

  def add_current_module_to_index(state, position) do
    current_module_variants = get_current_module_variants(state)

    current_module_variants
    |> Enum.reduce(state, fn variant, acc ->
      acc
      |> add_module_to_index(variant, position)
    end)
  end

  def add_module_to_index(state, module, position) do
    state = %{
      state
      | mods_funs:
          state.mods_funs
          |> Map.update(module, %{}, & &1)
    }

    add_mod_fun_to_position(state, {module, nil, nil}, position, nil)
  end

  def add_func_to_index(state, func, params, position, type) do
    current_module_variants = get_current_module_variants(state)
    arity = length(params)

    mods_funs =
      current_module_variants
      |> Enum.reduce(state.mods_funs, fn variant, acc ->
        acc
        |> Map.update(
          variant,
          %{{func, arity} => %ModFunInfo{type: type}},
          &(&1 |> Map.put({func, arity}, %ModFunInfo{type: type}))
        )
      end)

    state =
      current_module_variants
      |> Enum.reduce(state, fn variant, acc ->
        acc
        |> add_mod_fun_to_position({variant, func, arity}, position, params)
        |> add_mod_fun_to_position({variant, func, nil}, position, params)
      end)

    %{state | mods_funs: mods_funs}
  end

  def new_alias_scope(state) do
    %{state | aliases: [[] | state.aliases]}
  end

  def remove_alias_scope(state) do
    %{state | aliases: tl(state.aliases)}
  end

  def new_vars_scope(state) do
    scope_id = state.scope_id_count + 1

    %{
      state
      | scope_ids: [scope_id | state.scope_ids],
        scope_id_count: scope_id,
        vars: [[] | state.vars],
        scope_vars: [[] | state.scope_vars]
    }
  end

  def new_func_vars_scope(state) do
    %{state | vars: [[] | state.vars], scope_vars: [[]]}
  end

  def new_attributes_scope(state) do
    %{state | attributes: [[] | state.attributes], scope_attributes: [[]]}
  end

  def new_behaviours_scope(state) do
    %{state | behaviours: [[] | state.behaviours], scope_behaviours: [[]]}
  end

  def remove_vars_scope(state) do
    [current_scope_vars | other_scope_vars] = state.scope_vars
    [scope_id | other_scope_ids] = state.scope_ids

    vars_info_per_scope_id =
      state.vars_info_per_scope_id |> Map.put(scope_id, reduce_vars(current_scope_vars))

    %{
      state
      | scope_ids: other_scope_ids,
        vars: tl(state.vars),
        scope_vars: other_scope_vars,
        vars_info_per_scope_id: vars_info_per_scope_id
    }
  end

  def remove_func_vars_scope(state) do
    vars = tl(state.vars)
    %{state | vars: vars, scope_vars: vars}
  end

  def remove_attributes_scope(state) do
    attributes = tl(state.attributes)
    %{state | attributes: attributes, scope_attributes: attributes}
  end

  def remove_behaviours_scope(state) do
    behaviours = tl(state.behaviours)
    %{state | behaviours: behaviours, scope_behaviours: behaviours}
  end

  def add_alias(state, {alias, module}) when alias == module, do: state

  def add_alias(state, {alias, aliased}) do
    alias = Module.split(alias) |> Enum.take(-1) |> Module.concat()
    [aliases_from_scope | inherited_aliases] = state.aliases
    aliases_from_scope = aliases_from_scope |> Enum.reject(&match?({^alias, _}, &1))

    %{
      state
      | aliases: [
          [{alias, expand_alias(state, aliased)} | aliases_from_scope] | inherited_aliases
        ]
    }
  end

  def add_aliases(state, aliases_tuples) do
    Enum.reduce(aliases_tuples, state, fn tuple, state -> add_alias(state, tuple) end)
  end

  def remove_alias(state, _alias_tuple = {alias, _}) do
    [aliases_from_scope | inherited_aliases] = state.aliases
    aliases_from_scope = aliases_from_scope |> Enum.reject(&match?({^alias, _}, &1))
    %{state | aliases: [aliases_from_scope | inherited_aliases]}
  end

  def new_import_scope(state) do
    %{state | imports: [[] | state.imports]}
  end

  def new_require_scope(state) do
    %{state | requires: [[] | state.requires]}
  end

  def remove_import_scope(state) do
    %{state | imports: tl(state.imports)}
  end

  def remove_require_scope(state) do
    %{state | requires: tl(state.requires)}
  end

  def add_import(state, module) do
    module = expand_alias(state, module)
    [imports_from_scope | inherited_imports] = state.imports
    imports_from_scope = imports_from_scope -- [module]

    %{state | imports: [[module | imports_from_scope] | inherited_imports]}
    |> maybe_add_import_alias(module)
  end

  def add_imports(state, modules) do
    Enum.reduce(modules, state, fn mod, state -> add_import(state, mod) end)
  end

  defp maybe_add_import_alias(state, module) do
    if ElixirSense.Core.Introspection.elixir_module?(module) do
      case module |> Module.split() |> Enum.reverse() do
        [_] ->
          state

        [head | _] ->
          state
          |> add_alias({Module.concat([head]), module})
      end
    else
      state
    end
  end

  def add_require(state, module) do
    module = expand_alias(state, module)

    [requires_from_scope | inherited_requires] = state.requires
    requires_from_scope = requires_from_scope -- [module]
    %{state | requires: [[module | requires_from_scope] | inherited_requires]}
  end

  def add_requires(state, modules) do
    Enum.reduce(modules, state, fn mod, state -> add_require(state, mod) end)
  end

  def add_var(state, %{name: var_name} = var_info, is_definition) do
    scope = get_current_scope_name(state)
    [vars_from_scope | other_vars] = state.vars
    is_var_defined = is_variable_defined(state, var_name)
    var_name_as_string = Atom.to_string(var_name)

    vars_from_scope =
      case {is_definition, is_var_defined, var_name_as_string} do
        {_, _, "_" <> _} ->
          vars_from_scope

        {_, _, ^scope} ->
          vars_from_scope

        {true, _, _} ->
          [
            %VarInfo{var_info | scope_id: hd(state.scope_ids), is_definition: is_definition}
            | vars_from_scope
          ]

        {false, true, _} ->
          [
            %VarInfo{var_info | scope_id: hd(state.scope_ids), is_definition: is_definition}
            | vars_from_scope
          ]

        _ ->
          vars_from_scope
      end

    %{
      state
      | vars: [vars_from_scope | other_vars],
        scope_vars: [vars_from_scope | tl(state.scope_vars)]
    }
  end

  def add_attributes(state, attributes) do
    Enum.reduce(attributes, state, fn attribute, state -> add_attribute(state, attribute) end)
  end

  def add_attribute(state, attribute) do
    [attributes_from_scope | other_attributes] = state.attributes

    attributes_from_scope =
      if attribute in attributes_from_scope do
        attributes_from_scope
      else
        [attribute | attributes_from_scope]
      end

    attributes = [attributes_from_scope | other_attributes]
    scope_attributes = [attributes_from_scope | tl(state.scope_attributes)]
    %{state | attributes: attributes, scope_attributes: scope_attributes}
  end

  def add_behaviour(state, module) do
    module = expand_alias(state, module)
    [behaviours_from_scope | other_behaviours] = state.behaviours
    behaviours_from_scope = behaviours_from_scope -- [module]
    %{state | behaviours: [[module | behaviours_from_scope] | other_behaviours]}
  end

  def add_behaviours(state, modules) do
    Enum.reduce(modules, state, fn mod, state -> add_behaviour(state, mod) end)
  end

  def add_vars(state, vars, is_definition) do
    vars |> Enum.reduce(state, fn var, state -> add_var(state, var, is_definition) end)
  end

  defp reduce_vars(vars) do
    Enum.reduce(vars, %{}, fn %VarInfo{name: var, positions: positions, scope_id: scope_id},
                              acc ->
      var_info = Map.get(acc, var, %VarInfo{name: var, positions: [], scope_id: scope_id})
      var_info = %VarInfo{var_info | positions: Enum.sort(var_info.positions ++ positions)}
      Map.put(acc, var, var_info)
    end)
  end

  def get_closest_previous_env(%__MODULE__{} = metadata, line) do
    metadata.lines_to_env
    |> Enum.max_by(
      fn
        {env_line, _} when env_line < line -> env_line
        _ -> 0
      end,
      fn -> {0, default_env()} end
    )
    |> elem(1)
  end

  def default_env(), do: %ElixirSense.Core.State.Env{}

  def expand_alias(state, module) do
    if ElixirSense.Core.Introspection.elixir_module?(module) do
      current_aliases = current_aliases(state)
      module_parts = Module.split(module)

      case current_aliases
           |> Enum.find(fn {alias, _} ->
             [alis_split] = Module.split(alias)
             alis_split == hd(module_parts)
           end) do
        nil ->
          module

        {_alias, alias_expanded} ->
          Module.concat(Module.split(alias_expanded) ++ tl(module_parts))
      end
    else
      module
    end
  end
end
