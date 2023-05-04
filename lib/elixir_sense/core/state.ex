defmodule ElixirSense.Core.State do
  @moduledoc """
  Core State
  """

  alias ElixirSense.Core.Introspection

  @type fun_arity :: {atom, non_neg_integer}
  @type scope :: atom | fun_arity

  @type alias_t :: {module, module}
  @type scope_id_t :: non_neg_integer
  @type position_t :: {pos_integer, pos_integer}

  @type mods_funs_to_positions_t :: %{
          optional({module, atom, nil | non_neg_integer}) => ElixirSense.Core.State.ModFunInfo.t()
        }
  @type lines_to_env_t :: %{optional(pos_integer) => ElixirSense.Core.State.Env.t()}
  @type calls_t :: %{optional(pos_integer) => list(ElixirSense.Core.State.CallInfo.t())}

  @type types_t :: %{
          optional({module, atom, nil | non_neg_integer}) => ElixirSense.Core.State.TypeInfo.t()
        }
  @type specs_t :: %{
          optional({module, atom, nil | non_neg_integer}) => ElixirSense.Core.State.SpecInfo.t()
        }
  @type vars_info_per_scope_id_t :: %{
          optional(scope_id_t) => %{optional(atom) => ElixirSense.Core.State.VarInfo.t()}
        }
  @type structs_t :: %{optional(module) => ElixirSense.Core.State.StructInfo.t()}
  @type protocol_t :: {module, nonempty_list(module)}
  @type var_type :: nil | {:atom, atom} | {:map, keyword} | {:struct, keyword, module}

  @type t :: %ElixirSense.Core.State{
          namespace: [[atom]],
          scopes: [[scope]],
          imports: list(list(module)),
          requires: list(list(module)),
          aliases: list(list(alias_t)),
          attributes: list(list(ElixirSense.Core.State.AttributeInfo.t())),
          protocols: list(list(protocol_t())),
          scope_attributes: list(list(atom)),
          behaviours: list(list(module)),
          specs: specs_t,
          vars: list(list(ElixirSense.Core.State.VarInfo.t())),
          scope_vars: list(list(ElixirSense.Core.State.VarInfo.t())),
          scope_id_count: non_neg_integer,
          scope_ids: list(scope_id_t),
          vars_info_per_scope_id: vars_info_per_scope_id_t,
          mods_funs_to_positions: mods_funs_to_positions_t,
          lines_to_env: lines_to_env_t,
          calls: calls_t,
          structs: structs_t,
          types: types_t,
          first_alias_positions: map(),
          moduledoc_positions: map(),
          # TODO
          binding_context: list
        }

  @auto_required [Application, Kernel, Kernel.Typespec]

  defstruct namespace: [[:"Elixir"]],
            scopes: [[:"Elixir"]],
            imports: [[]],
            requires: [@auto_required],
            aliases: [[]],
            attributes: [[]],
            protocols: [[]],
            scope_attributes: [[]],
            behaviours: [[]],
            specs: %{},
            vars: [[]],
            scope_vars: [[]],
            scope_id_count: 0,
            scope_ids: [0],
            vars_info_per_scope_id: %{},
            mods_funs_to_positions: %{},
            lines_to_env: %{},
            calls: %{},
            structs: %{},
            types: %{},
            binding_context: [],
            first_alias_positions: %{},
            moduledoc_positions: %{}

  defmodule Env do
    @moduledoc """
    Line environment
    """

    @type t :: %Env{
            imports: list(module),
            requires: list(module),
            aliases: list(ElixirSense.Core.State.alias_t()),
            module: nil | module,
            module_variants: list(module),
            protocol: nil | ElixirSense.Core.State.protocol_t(),
            protocol_variants: list(ElixirSense.Core.State.protocol_t()),
            vars: list(ElixirSense.Core.State.VarInfo.t()),
            attributes: list(ElixirSense.Core.State.AttributeInfo.t()),
            behaviours: list(module),
            scope: nil | ElixirSense.Core.State.scope(),
            scope_id: nil | ElixirSense.Core.State.scope_id_t()
          }
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
              scope_id: nil
  end

  defmodule VarInfo do
    @moduledoc """
    Variable info
    """

    @type t :: %VarInfo{
            name: atom,
            positions: list(ElixirSense.Core.State.position_t()),
            scope_id: nil | ElixirSense.Core.State.scope_id_t(),
            is_definition: boolean,
            type: ElixirSense.Core.State.var_type()
          }
    defstruct name: nil,
              positions: [],
              scope_id: nil,
              is_definition: false,
              type: nil
  end

  defmodule TypeInfo do
    @moduledoc """
    Type definition info
    """
    @type t :: %TypeInfo{
            name: atom,
            args: list(list(String.t())),
            specs: [String.t()],
            kind: :type | :typep | :opaque,
            positions: [ElixirSense.Core.State.position_t()]
          }
    defstruct name: nil, args: [], specs: [], kind: :type, positions: []
  end

  defmodule SpecInfo do
    @moduledoc """
    Type definition info
    """
    @type t :: %SpecInfo{
            name: atom,
            args: list(list(String.t())),
            specs: [String.t()],
            kind: :spec | :callback | :macrocallback,
            positions: [ElixirSense.Core.State.position_t()]
          }
    defstruct name: nil, args: [], specs: [], kind: :spec, positions: []
  end

  defmodule StructInfo do
    @moduledoc """
    Structure definition info
    """
    @type field_t :: {atom, any}
    @type t :: %StructInfo{
            type: :defstruct | :defexception,
            fields: list(field_t)
          }
    defstruct type: :defstruct, fields: []
  end

  defmodule AttributeInfo do
    @moduledoc """
    Variable info
    """
    @type t :: %AttributeInfo{
            name: atom,
            positions: list(ElixirSense.Core.State.position_t()),
            type: ElixirSense.Core.State.var_type()
          }
    defstruct name: nil, positions: [], type: nil
  end

  defmodule CallInfo do
    @moduledoc """
    Function call info
    """
    @type t :: %CallInfo{
            arity: non_neg_integer,
            position: ElixirSense.Core.State.position_t(),
            func: atom,
            mod: module | {:attribute, atom}
          }
    defstruct arity: 0,
              position: {1, 1},
              func: nil,
              mod: Elixir
  end

  defmodule ModFunInfo do
    @moduledoc """
    Module or function info
    """

    @type t :: %ModFunInfo{
            params: list(list(term)),
            positions: list(ElixirSense.Core.State.position_t()),
            target: nil | {module, atom},
            overridable: false | {true, module},
            # TODO defmodule defprotocol defimpl?
            type:
              :def
              | :defp
              | :defmacro
              | :defmacrop
              | :defdelegate
              | :defguard
              | :defguardp
              | :defmodule
          }

    defstruct params: [],
              positions: [],
              target: nil,
              type: nil,
              overridable: false

    def get_arities(%ModFunInfo{params: params_variants}) do
      params_variants
      |> Enum.map(fn params ->
        {length(params), Introspection.count_defaults(params)}
      end)
    end

    def get_category(%ModFunInfo{type: type})
        when type in [:defmacro, :defmacrop, :defguard, :defguardp],
        do: :macro

    def get_category(%ModFunInfo{type: type}) when type in [:def, :defp, :defdelegate],
      do: :function

    def get_category(%ModFunInfo{}), do: :module
  end

  def current_aliases(%__MODULE__{} = state) do
    state.aliases |> List.flatten() |> Enum.uniq_by(&elem(&1, 0)) |> Enum.reverse()
  end

  def get_current_env(%__MODULE__{} = state) do
    current_module_variants = get_current_module_variants(state)
    current_imports = state.imports |> :lists.reverse() |> List.flatten()
    current_requires = state.requires |> :lists.reverse() |> List.flatten()
    current_aliases = current_aliases(state)
    current_vars = state |> get_current_vars()
    current_attributes = state |> get_current_attributes()
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
      protocol_variants: current_scope_protocols
    }
  end

  def get_current_module(%__MODULE__{} = state) do
    get_current_module_variants(state) |> hd
  end

  def get_current_module_variants(%__MODULE__{protocols: [[] | _]} = state) do
    state.namespace |> hd |> unescape_protocol_impementations
  end

  def get_current_module_variants(%__MODULE__{protocols: [protocols | _]}) do
    for {protocol, implementations} <- protocols,
        implementation <- implementations do
      Module.concat(protocol, implementation)
    end
  end

  def add_current_env_to_line(%__MODULE__{} = state, line) when is_integer(line) do
    previous_env = state.lines_to_env[line]
    current_env = get_current_env(state)

    env = merge_env_vars(current_env, previous_env)
    %__MODULE__{state | lines_to_env: Map.put(state.lines_to_env, line, env)}
  end

  defp merge_env_vars(%Env{vars: current_vars} = current_env, previous_env) do
    case previous_env do
      nil ->
        current_env

      %Env{vars: previous_vars} ->
        vars_to_preserve =
          Enum.filter(previous_vars, fn previous_var ->
            Enum.all?(current_vars, fn current_var ->
              current_var_positions = MapSet.new(current_var.positions)

              previous_var.positions
              |> MapSet.new()
              |> MapSet.disjoint?(current_var_positions)
            end)
          end)

        %Env{current_env | vars: current_vars ++ vars_to_preserve}
    end
  end

  def add_moduledoc_positions(
        %__MODULE__{} = state,
        [line: line, column: column],
        [{:moduledoc, _meta, [here_doc]}],
        line
      )
      when is_integer(line) and is_binary(here_doc) do
    module_name = module_name(state)

    new_line_count = here_doc |> String.split("\n") |> Enum.count()
    line_to_insert_alias = new_line_count + line + 1

    %__MODULE__{
      state
      | moduledoc_positions:
          Map.put(state.moduledoc_positions, module_name, {line_to_insert_alias, column})
    }
  end

  def add_moduledoc_positions(
        %__MODULE__{} = state,
        [line: line, column: column],
        [{:moduledoc, _meta, [params]}],
        line
      )
      when is_integer(line) and is_boolean(params) do
    module_name = module_name(state)

    line_to_insert_alias = line + 1

    %__MODULE__{
      state
      | moduledoc_positions:
          Map.put(state.moduledoc_positions, module_name, {line_to_insert_alias, column})
    }
  end

  def add_moduledoc_positions(state, _, _, _), do: state

  def add_first_alias_positions(%__MODULE__{} = state, line, column)
      when is_integer(line) and is_integer(column) do
    current_scope = hd(hd(state.scopes))

    is_module? = is_atom(current_scope)

    if is_module? do
      module_name = module_name(state)

      %__MODULE__{
        state
        | first_alias_positions:
            Map.put_new(state.first_alias_positions, module_name, {line, column})
      }
    else
      state
    end
  end

  defp module_name(state) do
    hd(state.scopes)
    |> Enum.reverse()
    |> after_elixir_prefix()
    |> Enum.filter(&is_atom/1)
    |> Module.concat()
  end

  defp after_elixir_prefix([Elixir | rest]), do: rest
  defp after_elixir_prefix(rest), do: rest

  def add_call_to_line(%__MODULE__{} = state, {mod, func, arity}, {line, _column} = position) do
    call = %CallInfo{mod: mod, func: func, arity: arity, position: position}

    calls =
      Map.update(state.calls, line, [call], fn line_calls ->
        [call | line_calls]
      end)

    %__MODULE__{state | calls: calls}
  end

  def remove_calls(%__MODULE__{} = state, positions) do
    Enum.reduce(positions, state, fn {line, _column} = position, state ->
      case state.calls[line] do
        nil ->
          state

        calls ->
          updated_calls = Enum.reject(calls, fn call_info -> call_info.position == position end)

          case updated_calls do
            [] ->
              %__MODULE__{state | calls: Map.delete(state.calls, line)}

            _non_empty_list ->
              %__MODULE__{state | calls: Map.put(state.calls, line, updated_calls)}
          end
      end
    end)
  end

  def add_struct(%__MODULE__{} = state, type, fields) do
    structs =
      get_current_module_variants(state)
      |> Enum.reduce(state.structs, fn variant, acc ->
        acc |> Map.put(variant, %StructInfo{type: type, fields: fields ++ [__struct__: variant]})
      end)

    %__MODULE__{state | structs: structs}
  end

  def get_scope_name(%__MODULE__{} = state, line) do
    case state.lines_to_env[line] do
      nil -> nil
      %Env{scope: scope} -> scope
    end
  end

  def get_current_scope_name(%__MODULE__{} = state) do
    case hd(hd(state.scopes)) do
      {fun, _} -> fun |> Atom.to_string()
      mod -> mod |> Atom.to_string()
    end
  end

  def get_current_vars(%__MODULE__{} = state) do
    state.scope_vars
    |> List.flatten()
    |> reduce_vars()
    |> Enum.flat_map(fn {_var, scopes} -> scopes end)
  end

  def get_current_vars_refs(%__MODULE__{} = state) do
    state.scope_vars |> List.flatten()
  end

  def get_current_attributes(%__MODULE__{} = state) do
    state.scope_attributes |> :lists.reverse() |> List.flatten()
  end

  def is_variable_defined(%__MODULE__{} = state, var_name) do
    state
    |> get_current_vars_refs()
    |> Enum.any?(fn %VarInfo{name: name, is_definition: is_definition} ->
      name == var_name && is_definition
    end)
  end

  def add_mod_fun_to_position(
        %__MODULE__{} = state,
        {module, fun, arity},
        position,
        params,
        type,
        options \\ []
      ) do
    current_info = Map.get(state.mods_funs_to_positions, {module, fun, arity}, %ModFunInfo{})
    current_params = current_info |> Map.get(:params, [])
    current_positions = current_info |> Map.get(:positions, [])
    new_params = [params | current_params]
    new_positions = [position | current_positions]

    info_type =
      if fun != nil and arity == nil and
           current_info.type not in [nil, :defp, :defmacrop, :defguardp] and
           not match?({true, _}, current_info.overridable) do
        # in case there are multiple definitions for nil arity prefer public ones
        # unless this is an overridable def
        current_info.type
      else
        type
      end

    info = %ModFunInfo{
      positions: new_positions,
      params: new_params,
      type: info_type,
      overridable: current_info |> Map.get(:overridable, false)
    }

    info =
      Enum.reduce(options, info, fn option, acc -> process_option(state, acc, type, option) end)

    mods_funs_to_positions = Map.put(state.mods_funs_to_positions, {module, fun, arity}, info)

    %__MODULE__{state | mods_funs_to_positions: mods_funs_to_positions}
  end

  defp process_option(
         state,
         info,
         :defdelegate,
         {:target, {target_module_expression, target_function}}
       ) do
    %ModFunInfo{
      info
      | target: {expand_alias(state, target_module_expression), target_function}
    }
  end

  defp process_option(_state, info, _, {:overridable, {true, module}}) do
    %ModFunInfo{
      info
      | overridable: {true, module}
    }
  end

  defp process_option(_state, info, _type, _option), do: info

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

  def add_namespace(%__MODULE__{} = state, module) do
    # TODO refactor to allow {:implementation, protocol, [implementations]} in scope
    module = escape_protocol_impementations(module)

    {namespace, scopes} =
      case module do
        [:"Elixir" | module = [_ | _]] ->
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

    %__MODULE__{state | namespace: [namespace | state.namespace], scopes: [scopes | state.scopes]}
  end

  def remove_namespace(%__MODULE__{} = state) do
    outer_mods = state.namespace |> tl
    outer_scopes = state.scopes |> tl

    %{state | namespace: outer_mods, scopes: outer_scopes}
  end

  def new_named_func(%__MODULE__{} = state, name, arity) do
    %{state | scopes: [[{name, arity} | hd(state.scopes)] | state.scopes]}
  end

  def maybe_add_protocol_implementation(%__MODULE__{} = state, {protocol, implementations}) do
    implementation_modules =
      implementations
      |> Enum.flat_map(fn
        module when is_list(module) ->
          expanded = expand_alias(state, module)
          unescape_protocol_impementations(expanded)

        module when is_atom(module) ->
          unescape_protocol_impementations(module)
      end)

    candidate =
      if is_list(protocol) do
        expand_alias(state, protocol)
      else
        protocol
      end

    protocols =
      unescape_protocol_impementations(candidate)
      |> Enum.map(&{&1, implementation_modules})

    %__MODULE__{state | protocols: [protocols | state.protocols]}
  end

  def maybe_add_protocol_implementation(%__MODULE__{} = state, _) do
    %__MODULE__{state | protocols: [[] | state.protocols]}
  end

  def remove_protocol_implementation(%__MODULE__{} = state) do
    %__MODULE__{state | protocols: tl(state.protocols)}
  end

  def remove_last_scope_from_scopes(%__MODULE__{} = state) do
    %__MODULE__{state | scopes: tl(state.scopes)}
  end

  def add_current_module_to_index(%__MODULE__{} = state, position) do
    current_module_variants = get_current_module_variants(state)

    current_module_variants
    |> Enum.reduce(state, fn variant, acc ->
      acc
      |> add_module_to_index(variant, position)
    end)
  end

  def add_module_to_index(%__MODULE__{} = state, module, position) do
    # TODO :defprotocol, :defimpl?
    add_mod_fun_to_position(state, {module, nil, nil}, position, nil, :defmodule)
  end

  def add_func_to_index(%__MODULE__{} = state, func, params, position, type, options \\ []) do
    current_module_variants = get_current_module_variants(state)
    arity = length(params)

    current_module_variants
    |> Enum.reduce(state, fn variant, acc ->
      acc
      |> add_mod_fun_to_position({variant, func, arity}, position, params, type, options)
      |> add_mod_fun_to_position({variant, func, nil}, position, params, type, options)
    end)
  end

  def make_overridable(%__MODULE__{} = state, fa_list, overridable_module) do
    module = get_current_module(state)

    mods_funs_to_positions =
      fa_list
      |> Enum.reduce(state.mods_funs_to_positions, fn {f, a}, mods_funs_to_positions_acc ->
        if Map.has_key?(mods_funs_to_positions_acc, {module, f, a}) do
          mods_funs_to_positions_acc
          |> make_def_overridable({module, f, a}, overridable_module)
          |> make_def_overridable({module, f, nil}, overridable_module)
        else
          # Some behaviour callbacks can be not implemented by __using__ macro
          mods_funs_to_positions_acc
        end
      end)

    %__MODULE__{state | mods_funs_to_positions: mods_funs_to_positions}
  end

  defp make_def_overridable(mods_funs_to_positions, mfa, overridable_module) do
    update_in(mods_funs_to_positions[mfa], fn mod_fun_info = %ModFunInfo{} ->
      %ModFunInfo{mod_fun_info | overridable: {true, overridable_module}}
    end)
  end

  def new_alias_scope(%__MODULE__{} = state) do
    %__MODULE__{state | aliases: [[] | state.aliases]}
  end

  def remove_alias_scope(%__MODULE__{} = state) do
    %__MODULE__{state | aliases: tl(state.aliases)}
  end

  def new_vars_scope(%__MODULE__{} = state) do
    scope_id = state.scope_id_count + 1

    %__MODULE__{
      state
      | scope_ids: [scope_id | state.scope_ids],
        scope_id_count: scope_id,
        vars: [[] | state.vars],
        scope_vars: [[] | state.scope_vars]
    }
  end

  def push_binding_context(%__MODULE__{} = state, binding_context) do
    %__MODULE__{
      state
      | binding_context: [binding_context | state.binding_context]
    }
  end

  def pop_binding_context(%__MODULE__{} = state) do
    %__MODULE__{
      state
      | binding_context: tl(state.binding_context)
    }
  end

  def new_func_vars_scope(%__MODULE__{} = state) do
    scope_id = state.scope_id_count + 1

    %__MODULE__{
      state
      | scope_ids: [scope_id | state.scope_ids],
        scope_id_count: scope_id,
        vars: [[] | state.vars],
        scope_vars: [[]]
    }
  end

  def new_attributes_scope(%__MODULE__{} = state) do
    %__MODULE__{state | attributes: [[] | state.attributes], scope_attributes: [[]]}
  end

  def new_behaviours_scope(%__MODULE__{} = state) do
    %__MODULE__{state | behaviours: [[] | state.behaviours]}
  end

  def remove_vars_scope(%__MODULE__{} = state) do
    %__MODULE__{
      state
      | scope_ids: tl(state.scope_ids),
        vars: tl(state.vars),
        scope_vars: tl(state.scope_vars),
        vars_info_per_scope_id: update_vars_info_per_scope_id(state)
    }
  end

  def remove_func_vars_scope(%__MODULE__{} = state) do
    %__MODULE__{
      state
      | scope_ids: tl(state.scope_ids),
        vars: tl(state.vars),
        scope_vars: tl(state.vars),
        vars_info_per_scope_id: update_vars_info_per_scope_id(state)
    }
  end

  defp update_vars_info_per_scope_id(state) do
    [scope_id | _other_scope_ids] = state.scope_ids

    [current_scope_vars | other_scope_vars] = state.scope_vars

    current_scope_reduced_vars = reduce_vars(current_scope_vars)

    vars_info =
      other_scope_vars
      |> List.flatten()
      |> reduce_vars(current_scope_reduced_vars, false)
      |> Enum.flat_map(fn {_var, scopes} -> scopes end)

    Map.put(state.vars_info_per_scope_id, scope_id, vars_info)
  end

  defp reduce_vars(vars, initial_acc \\ %{}, keep_all_same_name_vars \\ true) do
    Enum.reduce(vars, initial_acc, fn %VarInfo{name: var, positions: positions} = el, acc ->
      updated =
        case acc[var] do
          nil ->
            [el]

          [%VarInfo{is_definition: false} = var_info | same_name_vars] ->
            type =
              if Enum.all?(positions, fn position -> position in var_info.positions end) do
                merge_type(el.type, var_info.type)
              else
                el.type
              end

            [
              %VarInfo{
                el
                | positions: (var_info.positions ++ positions) |> Enum.uniq() |> Enum.sort(),
                  type: type
              }
              | same_name_vars
            ]

          [%VarInfo{is_definition: true} = var_info | same_name_vars] ->
            cond do
              Enum.all?(positions, fn position -> position in var_info.positions end) ->
                type = merge_type(el.type, var_info.type)

                [
                  %VarInfo{
                    var_info
                    | positions: (var_info.positions ++ positions) |> Enum.uniq() |> Enum.sort(),
                      type: type
                  }
                  | same_name_vars
                ]

              keep_all_same_name_vars ->
                [el, var_info | same_name_vars]

              true ->
                [var_info | same_name_vars]
            end
        end

      Map.put(acc, var, updated)
    end)
  end

  def remove_attributes_scope(%__MODULE__{} = state) do
    attributes = tl(state.attributes)
    %__MODULE__{state | attributes: attributes, scope_attributes: attributes}
  end

  def remove_behaviours_scope(%__MODULE__{} = state) do
    behaviours = tl(state.behaviours)
    %__MODULE__{state | behaviours: behaviours}
  end

  def add_alias(%__MODULE__{} = state, {alias, aliased}) when is_list(aliased) do
    if Introspection.elixir_module?(alias) do
      alias = Module.split(alias) |> Enum.take(-1) |> Module.concat()
      [aliases_from_scope | inherited_aliases] = state.aliases
      aliases_from_scope = aliases_from_scope |> Enum.reject(&match?({^alias, _}, &1))

      expanded = expand_alias(state, aliased)

      aliases_from_scope =
        if alias != expanded do
          [{alias, expanded} | aliases_from_scope]
        else
          aliases_from_scope
        end

      %__MODULE__{
        state
        | aliases: [
            aliases_from_scope | inherited_aliases
          ]
      }
    else
      state
    end
  end

  def add_alias(%__MODULE__{} = state, {alias, aliased}) when is_atom(aliased) do
    if Introspection.elixir_module?(alias) do
      [aliases_from_scope | inherited_aliases] = state.aliases
      aliases_from_scope = aliases_from_scope |> Enum.reject(&match?({^alias, _}, &1))

      aliases_from_scope =
        if alias != aliased do
          [{alias, aliased} | aliases_from_scope]
        else
          aliases_from_scope
        end

      %__MODULE__{
        state
        | aliases: [
            aliases_from_scope | inherited_aliases
          ]
      }
    else
      [aliases_from_scope | inherited_aliases] = state.aliases
      aliases_from_scope = aliases_from_scope |> Enum.reject(&match?({^alias, _}, &1))

      %__MODULE__{
        state
        | aliases: [
            [{Module.concat([alias]), aliased} | aliases_from_scope] | inherited_aliases
          ]
      }
    end
  end

  def add_aliases(%__MODULE__{} = state, aliases_tuples) do
    Enum.reduce(aliases_tuples, state, fn tuple, state -> add_alias(state, tuple) end)
  end

  def new_import_scope(%__MODULE__{} = state) do
    %__MODULE__{state | imports: [[] | state.imports]}
  end

  def new_require_scope(%__MODULE__{} = state) do
    %__MODULE__{state | requires: [[] | state.requires]}
  end

  def remove_import_scope(%__MODULE__{} = state) do
    %__MODULE__{state | imports: tl(state.imports)}
  end

  def remove_require_scope(%__MODULE__{} = state) do
    %__MODULE__{state | requires: tl(state.requires)}
  end

  def add_import(%__MODULE__{} = state, module, opts) when is_atom(module) or is_list(module) do
    module = expand_alias(state, module)
    [imports_from_scope | inherited_imports] = state.imports

    %__MODULE__{state | imports: [[module | imports_from_scope] | inherited_imports]}
  end

  def add_import(%__MODULE__{} = state, _module, _opts), do: state

  def add_imports(%__MODULE__{} = state, modules, opts) do
    Enum.reduce(modules, state, fn mod, state -> add_import(state, mod, opts) end)
  end

  def add_require(%__MODULE__{} = state, module) when is_atom(module) or is_list(module) do
    module = expand_alias(state, module)

    [requires_from_scope | inherited_requires] = state.requires

    current_requires = state.requires |> :lists.reverse() |> List.flatten()

    requires_from_scope =
      if module in current_requires do
        requires_from_scope
      else
        [module | requires_from_scope]
      end

    %__MODULE__{state | requires: [requires_from_scope | inherited_requires]}
  end

  def add_require(%__MODULE__{} = state, _module), do: state

  def add_requires(%__MODULE__{} = state, modules) do
    Enum.reduce(modules, state, fn mod, state -> add_require(state, mod) end)
  end

  def add_type(%__MODULE__{} = state, type_name, type_args, spec, kind, pos) do
    arg_names =
      type_args
      |> Enum.map(&Macro.to_string/1)

    type_info = %TypeInfo{
      name: type_name,
      args: [arg_names],
      kind: kind,
      specs: [spec],
      positions: [pos]
    }

    current_module_variants = get_current_module_variants(state)

    types =
      current_module_variants
      |> Enum.reduce(state.types, fn current_module, acc ->
        info =
          case acc[{current_module, type_name, nil}] do
            nil ->
              type_info

            %TypeInfo{positions: positions, args: args, specs: specs} = ti ->
              %TypeInfo{
                ti
                | positions: [pos | positions],
                  args: [arg_names | args],
                  specs: [spec | specs],
                  # in case there are multiple definitions for nil arity prefer public ones
                  kind: if(ti.kind != :typep, do: ti.kind, else: type_info.kind)
              }
          end

        acc
        |> Map.put({current_module, type_name, nil}, info)
        |> Map.put({current_module, type_name, length(arg_names)}, type_info)
      end)

    %__MODULE__{state | types: types}
  end

  def add_spec(%__MODULE__{} = state, type_name, type_args, spec, kind, pos) do
    arg_names =
      type_args
      |> Enum.map(&Macro.to_string/1)

    type_info = %SpecInfo{
      name: type_name,
      args: [arg_names],
      specs: [spec],
      kind: kind,
      positions: [pos]
    }

    current_module_variants = get_current_module_variants(state)

    specs =
      current_module_variants
      |> Enum.reduce(state.specs, fn current_module, acc ->
        info =
          case acc[{current_module, type_name, nil}] do
            nil ->
              type_info

            %SpecInfo{positions: positions, args: args, specs: specs} = ti ->
              %SpecInfo{
                ti
                | positions: [pos | positions],
                  args: [arg_names | args],
                  specs: [spec | specs]
              }
          end

        acc
        |> Map.put({current_module, type_name, nil}, info)
        |> Map.put({current_module, type_name, length(arg_names)}, type_info)
      end)

    %__MODULE__{state | specs: specs}
  end

  def add_var(
        %__MODULE__{} = state,
        %VarInfo{name: var_name} = var_info,
        is_definition
      ) do
    scope = get_current_scope_name(state)
    [vars_from_scope | other_vars] = state.vars
    is_var_defined = is_variable_defined(state, var_name)
    var_name_as_string = Atom.to_string(var_name)

    vars_from_scope =
      case {is_definition and var_info.is_definition, is_var_defined, var_name_as_string} do
        {_, _, "_" <> _} ->
          vars_from_scope

        {_, _, ^scope} ->
          vars_from_scope

        {is_definition, is_var_defined, _} when is_definition or is_var_defined ->
          [
            %VarInfo{
              var_info
              | scope_id: hd(state.scope_ids),
                is_definition: is_definition
            }
            | vars_from_scope
          ]

        _ ->
          vars_from_scope
      end

    %__MODULE__{
      state
      | vars: [vars_from_scope | other_vars],
        scope_vars: [vars_from_scope | tl(state.scope_vars)]
    }
  end

  @builtin_attributes ElixirSense.Core.BuiltinAttributes.all()

  def add_attributes(%__MODULE__{} = state, attributes, position) do
    Enum.reduce(attributes, state, fn attribute, state ->
      add_attribute(state, attribute, nil, true, position)
    end)
  end

  def add_attribute(%__MODULE__{} = state, attribute, type, is_definition, position)
      when attribute not in @builtin_attributes do
    [attributes_from_scope | other_attributes] = state.attributes

    existing_attribute_index =
      attributes_from_scope
      |> Enum.find_index(&(&1.name == attribute))

    attributes_from_scope =
      case existing_attribute_index do
        nil ->
          if is_definition do
            [
              %AttributeInfo{
                name: attribute,
                type: type,
                positions: [position]
              }
              | attributes_from_scope
            ]
          else
            attributes_from_scope
          end

        index ->
          attributes_from_scope
          |> List.update_at(index, fn existing ->
            type = if is_definition, do: type, else: existing.type

            %AttributeInfo{
              existing
              | # FIXME this is wrong for accumulating attributes
                type: type,
                positions: (existing.positions ++ [position]) |> Enum.uniq() |> Enum.sort()
            }
          end)
      end

    attributes = [attributes_from_scope | other_attributes]
    scope_attributes = [attributes_from_scope | tl(state.scope_attributes)]
    %__MODULE__{state | attributes: attributes, scope_attributes: scope_attributes}
  end

  def add_attribute(%__MODULE__{} = state, _attribute, _type, _is_definition, _position) do
    state
  end

  def add_behaviour(%__MODULE__{} = state, module) when is_atom(module) or is_list(module) do
    module = expand_alias(state, module)
    [behaviours_from_scope | other_behaviours] = state.behaviours
    behaviours_from_scope = behaviours_from_scope -- [module]
    %__MODULE__{state | behaviours: [[module | behaviours_from_scope] | other_behaviours]}
  end

  def add_behaviour(%__MODULE__{} = state, _module), do: state

  def add_behaviours(%__MODULE__{} = state, modules) do
    Enum.reduce(modules, state, fn mod, state -> add_behaviour(state, mod) end)
  end

  def add_vars(%__MODULE__{} = state, vars, is_definition) do
    vars |> Enum.reduce(state, fn var, state -> add_var(state, var, is_definition) end)
  end

  # Simultaneously performs two operations:
  # - deletes variables that contain any of `remove_positions`
  # - adds `vars` to the state, but with types merged with the corresponding removed variables
  def merge_new_vars(%__MODULE__{} = state, vars, remove_positions) do
    {state, vars} =
      Enum.reduce(remove_positions, {state, vars}, fn position, {state, vars} ->
        case pop_var(state, position) do
          {nil, state} ->
            {state, vars}

          {removed_var, state} ->
            vars =
              Enum.reduce(vars, [], fn %VarInfo{positions: positions} = var, vars ->
                if positions == removed_var.positions do
                  type = merge_type(var.type, removed_var.type)

                  [%VarInfo{var | type: type} | vars]
                else
                  [var | vars]
                end
              end)

            {state, vars}
        end
      end)

    add_vars(state, vars, true)
  end

  defp pop_var(%__MODULE__{} = state, position) do
    [current_scope_vars | other_vars] = state.vars

    var =
      Enum.find(current_scope_vars, fn %VarInfo{positions: positions} -> position in positions end)

    current_scope_vars =
      Enum.reject(current_scope_vars, fn %VarInfo{positions: positions} ->
        position in positions
      end)

    state = %__MODULE__{
      state
      | vars: [current_scope_vars | other_vars],
        scope_vars: [current_scope_vars | tl(state.scope_vars)]
    }

    {var, state}
  end

  def merge_same_name_vars(vars) do
    vars
    |> Enum.reduce(%{}, fn %VarInfo{name: var, positions: positions} = el, acc ->
      updated =
        case acc[var] do
          nil ->
            el

          %VarInfo{} = var_info ->
            type =
              if Enum.all?(positions, fn position -> position in var_info.positions end) do
                merge_type(el.type, var_info.type)
              else
                el.type
              end

            %VarInfo{
              var_info
              | positions: (var_info.positions ++ positions) |> Enum.uniq() |> Enum.sort(),
                type: type
            }
        end

      Map.put(acc, var, updated)
    end)
    |> Enum.map(fn {_name, var_info} -> var_info end)
  end

  defp merge_type(nil, new), do: new
  defp merge_type(old, nil), do: old
  defp merge_type(old, old), do: old
  defp merge_type(old, new), do: {:intersection, [old, new]}

  def get_closest_previous_env(%__MODULE__{} = metadata, line) do
    metadata.lines_to_env
    |> Enum.max_by(
      fn
        {env_line, _} when env_line < line -> env_line
        _ -> 0
      end,
      &>=/2,
      fn -> {0, default_env()} end
    )
    |> elem(1)
  end

  def default_env, do: %ElixirSense.Core.State.Env{}

  def expand_alias(%__MODULE__{} = _state, module) when is_atom(module) do
    module
  end

  def expand_alias(%__MODULE__{} = _state, [Elixir | _] = module) do
    Module.concat(module)
  end

  def expand_alias(%__MODULE__{} = state, [{:__MODULE__, _, nil} | rest]) do
    current_module = get_current_module(state)
    Module.concat([current_module | rest])
  end

  def expand_alias(%__MODULE__{} = state, module) when is_list(module) do
    current_aliases = current_aliases(state)
    Introspection.expand_alias(Module.concat(module), current_aliases)
  end

  def maybe_move_vars_to_outer_scope(%__MODULE__{} = state) do
    scope_vars = move_references_to_outer_scope(state.scope_vars)
    vars = move_references_to_outer_scope(state.vars)

    %__MODULE__{state | vars: vars, scope_vars: scope_vars}
  end

  defp move_references_to_outer_scope(vars) do
    {current_scope_vars, outer_scope_vars, other_scopes_vars} =
      case vars do
        [current_scope_vars, outer_scope_vars | other_scopes_vars] ->
          {current_scope_vars, outer_scope_vars, other_scopes_vars}

        [current_scope_vars | []] ->
          {current_scope_vars, [], []}
      end

    vars_to_move =
      current_scope_vars
      |> Enum.reduce(%{}, fn
        %VarInfo{name: var, is_definition: true}, acc ->
          Map.delete(acc, var)

        %VarInfo{name: var, positions: positions, is_definition: false} = el, acc ->
          updated =
            case acc[var] do
              nil ->
                el

              var_info ->
                type =
                  if Enum.all?(positions, fn position -> position in var_info.positions end) do
                    merge_type(el.type, var_info.type)
                  else
                    el.type
                  end

                %VarInfo{
                  el
                  | positions: (var_info.positions ++ positions) |> Enum.uniq() |> Enum.sort(),
                    type: type
                }
            end

          Map.put(acc, var, updated)
      end)
      |> Enum.map(fn {_name, var_info} -> var_info end)
      |> Enum.reject(fn var_info -> is_nil(var_info) end)

    [current_scope_vars, vars_to_move ++ outer_scope_vars | other_scopes_vars]
  end

  def alias_submodule(%__MODULE__{} = state, module) do
    module = escape_protocol_impementations(module)

    if length(state.namespace) > 2 and is_list(module) and state.protocols |> hd == [] do
      namespace = state.namespace |> hd

      {alias, expanded} =
        case module do
          [Elixir, alias] ->
            # an edge case with external submodule `Elixir.Some`
            # this ends up removing unaliasing `Some`
            # see https://github.com/elixir-lang/elixir/pull/12451#issuecomment-1461393633
            {Module.concat([alias]), [Elixir, alias]}

          _ ->
            alias = module |> Enum.take(1) |> Module.concat()
            expanded = namespace |> Enum.slice((length(module) - 1)..-2) |> Enum.reverse()
            {alias, expanded}
        end

      state
      |> add_alias({alias, expanded})
    else
      state
    end
  end
end
