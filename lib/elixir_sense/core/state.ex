defmodule ElixirSense.Core.State do
  @moduledoc """
  Core State
  """

  alias ElixirSense.Core.Introspection
  require Logger

  @type fun_arity :: {atom, non_neg_integer}
  @type scope :: atom | fun_arity | {:typespec, atom, non_neg_integer}

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
          module: [atom],
          scopes: [scope],
          requires: list(list(module)),
          aliases: list(list(alias_t)),
          attributes: list(list(ElixirSense.Core.State.AttributeInfo.t())),
          protocols: list(protocol_t() | nil),
          scope_attributes: list(list(atom)),
          behaviours: %{optional(module) => [module]},
          specs: specs_t,
          vars_info: list(list(ElixirSense.Core.State.VarInfo.t())),
          scope_vars_info: list(list(ElixirSense.Core.State.VarInfo.t())),
          scope_id_count: non_neg_integer,
          scope_ids: list(scope_id_t),
          vars_info_per_scope_id: vars_info_per_scope_id_t,
          mods_funs_to_positions: mods_funs_to_positions_t,
          lines_to_env: lines_to_env_t,
          calls: calls_t,
          structs: structs_t,
          types: types_t,
          generated: boolean,
          first_alias_positions: map(),
          moduledoc_positions: map(),
          context: map(),
          doc_context: list(),
          typedoc_context: list(),
          optional_callbacks_context: list(),
          # TODO better type
          binding_context: list,
          macro_env: list(Macro.Env.t())
        }

  @auto_imported_functions :elixir_env.new().functions
  @auto_imported_macros :elixir_env.new().macros
  @auto_required [Application, Kernel] ++
                   (if Version.match?(System.version(), ">= 1.17.0-dev") do
                      []
                    else
                      [Kernel.Typespec]
                    end)

  defstruct module: [nil],
            scopes: [nil],
            functions: [@auto_imported_functions],
            macros: [@auto_imported_macros],
            requires: [@auto_required],
            aliases: [[]],
            attributes: [[]],
            protocols: [nil],
            scope_attributes: [[]],
            behaviours: %{},
            specs: %{},
            vars_info: [[]],
            scope_vars_info: [[]],
            vars: {%{}, false},
            unused: {%{}, 0},
            prematch: :raise,
            stacktrace: false,
            caller: false,
            runtime_modules: [],
            scope_id_count: 0,
            scope_ids: [0],
            vars_info_per_scope_id: %{},
            mods_funs_to_positions: %{},
            lines_to_env: %{},
            calls: %{},
            structs: %{},
            types: %{},
            generated: false,
            binding_context: [],
            context: %{},
            first_alias_positions: %{},
            doc_context: [[]],
            typedoc_context: [[]],
            optional_callbacks_context: [[]],
            moduledoc_positions: %{},
            macro_env: [:elixir_env.new()]

  defmodule Env do
    @moduledoc """
    Line environment
    """

    @type t :: %Env{
            functions: [{module, [{atom, arity}]}],
            macros: [{module, [{atom, arity}]}],
            requires: list(module),
            aliases: list(ElixirSense.Core.State.alias_t()),
            module: nil | module,
            function: nil | {atom, arity},
            protocol: nil | ElixirSense.Core.State.protocol_t(),
            versioned_vars: %{optional({atom, atom}) => non_neg_integer},
            vars: list(ElixirSense.Core.State.VarInfo.t()),
            attributes: list(ElixirSense.Core.State.AttributeInfo.t()),
            behaviours: list(module),
            typespec: nil | {atom, arity},
            scope_id: nil | ElixirSense.Core.State.scope_id_t()
          }
    defstruct functions: [],
              macros: [],
              requires: [],
              aliases: [],
              # NOTE for protocol implementation this will be the first variant
              module: nil,
              function: nil,
              # NOTE for protocol implementation this will be the first variant
              protocol: nil,
              versioned_vars: %{},
              vars: [],
              attributes: [],
              behaviours: [],
              typespec: nil,
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
            positions: [ElixirSense.Core.State.position_t()],
            end_positions: [ElixirSense.Core.State.position_t() | nil],
            doc: String.t(),
            meta: map(),
            generated: list(boolean)
          }
    defstruct name: nil,
              args: [],
              specs: [],
              kind: :type,
              positions: [],
              end_positions: [],
              generated: [],
              doc: "",
              meta: %{}
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
            positions: [ElixirSense.Core.State.position_t()],
            end_positions: [ElixirSense.Core.State.position_t() | nil],
            doc: String.t(),
            meta: map(),
            generated: list(boolean)
          }
    defstruct name: nil,
              args: [],
              specs: [],
              kind: :spec,
              positions: [],
              end_positions: [],
              generated: [],
              doc: "",
              meta: %{}
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
            end_positions: list(ElixirSense.Core.State.position_t() | nil),
            target: nil | {module, atom},
            overridable: false | {true, module},
            generated: list(boolean),
            doc: String.t(),
            meta: map(),
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
              end_positions: [],
              target: nil,
              type: nil,
              generated: [],
              overridable: false,
              doc: "",
              meta: %{}

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

  def current_requires(%__MODULE__{} = state) do
    state.requires |> :lists.reverse() |> List.flatten() |> Enum.uniq() |> Enum.sort()
  end

  def get_current_env(%__MODULE__{} = state) do
    current_module = get_current_module(state)
    current_functions = state.functions |> hd()
    current_macros = state.macros |> hd()
    current_requires = current_requires(state)
    current_aliases = current_aliases(state)
    current_vars = state |> get_current_vars()
    current_attributes = state |> get_current_attributes()
    current_behaviours = state.behaviours |> Map.get(current_module, [])
    current_scope = hd(state.scopes)
    current_scope_id = hd(state.scope_ids)
    current_scope_protocol = hd(state.protocols)

    current_function =
      case current_scope do
        {_name, _arity} = function -> function
        _ -> nil
      end

    current_typespec =
      case current_scope do
        {:typespec, name, arity} -> {name, arity}
        _ -> nil
      end

    versioned_vars =
      current_vars
      |> Enum.map(&{&1.name, nil})
      |> Enum.sort()
      |> Enum.with_index()
      |> Map.new()

    %Env{
      functions: current_functions,
      macros: current_macros,
      requires: current_requires,
      aliases: current_aliases,
      module: current_module,
      function: current_function,
      typespec: current_typespec,
      vars: current_vars,
      versioned_vars: versioned_vars,
      attributes: current_attributes,
      behaviours: current_behaviours,
      scope_id: current_scope_id,
      protocol: current_scope_protocol
    }
  end

  def get_current_env(%__MODULE__{} = state, macro_env) do
    # current_vars = state |> get_current_vars()
    current_vars = state.vars |> elem(0)
    current_attributes = state |> get_current_attributes()
    current_behaviours = state.behaviours |> Map.get(macro_env.module, [])

    current_scope_id = hd(state.scope_ids)
    current_scope_protocol = hd(state.protocols)

    %Env{
      functions: macro_env.functions,
      macros: macro_env.macros,
      requires: macro_env.requires,
      aliases: macro_env.aliases,
      module: macro_env.module,
      function: macro_env.function,
      vars: current_vars,
      attributes: current_attributes,
      behaviours: current_behaviours,
      typespec: nil,
      scope_id: current_scope_id,
      protocol: current_scope_protocol
    }
  end

  def get_current_module(%__MODULE__{} = state) do
    state.module |> hd
  end

  def add_current_env_to_line(%__MODULE__{} = state, line) when is_integer(line) do
    previous_env = state.lines_to_env[line]
    current_env = get_current_env(state)

    env = merge_env_vars(current_env, previous_env)
    %__MODULE__{state | lines_to_env: Map.put(state.lines_to_env, line, env)}
  end

  def add_current_env_to_line(%__MODULE__{} = state, line, macro_env) when is_integer(line) do
    _previous_env = state.lines_to_env[line]
    current_env = get_current_env(state, macro_env)

    # TODO
    # env = merge_env_vars(current_env, previous_env)
    env = current_env
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
    module_name = get_current_module(state)

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
    module_name = get_current_module(state)

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
    current_scope = hd(state.scopes)

    is_module? = is_atom(current_scope) and current_scope != nil

    if is_module? do
      module_name = get_current_module(state)

      %__MODULE__{
        state
        | first_alias_positions:
            Map.put_new(state.first_alias_positions, module_name, {line, column})
      }
    else
      state
    end
  end

  def add_call_to_line(%__MODULE__{} = state, {nil, :__block__, _}, _position), do: state

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

  def add_struct(%__MODULE__{} = state, %__MODULE__.Env{} = env, type, fields) do
    structs =
      state.structs
      |> Map.put(env.module, %StructInfo{type: type, fields: fields ++ [__struct__: env.module]})

    %__MODULE__{state | structs: structs}
  end

  def get_current_vars(%__MODULE__{} = state) do
    state.scope_vars_info
    |> List.flatten()
    |> reduce_vars()
    |> Enum.flat_map(fn {_var, scopes} -> scopes end)
  end

  def get_current_vars_refs(%__MODULE__{} = state) do
    state.scope_vars_info |> List.flatten()
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
        end_position,
        params,
        type,
        doc,
        meta,
        options \\ []
      )
      when is_tuple(position) do
    current_info = Map.get(state.mods_funs_to_positions, {module, fun, arity}, %ModFunInfo{})
    current_params = current_info |> Map.get(:params, [])
    current_positions = current_info |> Map.get(:positions, [])
    current_end_positions = current_info |> Map.get(:end_positions, [])
    new_params = [params | current_params]
    new_positions = [position | current_positions]
    new_end_positions = [end_position | current_end_positions]

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

    overridable = current_info |> Map.get(:overridable, false)

    meta =
      if overridable do
        Map.put(meta, :overridable, true)
      else
        meta
      end

    info = %ModFunInfo{
      positions: new_positions,
      end_positions: new_end_positions,
      params: new_params,
      type: info_type,
      doc: doc,
      meta: meta,
      generated: [Keyword.get(options, :generated, false) | current_info.generated],
      overridable: overridable
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
    {module, _state, _env} = expand(target_module_expression, state)

    %ModFunInfo{
      info
      | target: {module, target_function}
    }
  end

  defp process_option(_state, info, _, {:overridable, {true, module}}) do
    %ModFunInfo{
      info
      | overridable: {true, module},
        meta: Map.put(info.meta, :overridable, true)
    }
  end

  defp process_option(_state, info, _type, _option), do: info

  def implementation_alias(protocol, [first | _]) do
    Module.concat(protocol, first)
  end

  def add_module(%__MODULE__{} = state, module) do
    # TODO refactor to allow {:implementation, protocol, [implementations]} in scope
    %__MODULE__{
      state
      | module: [module | state.module],
        scopes: [module | state.scopes],
        doc_context: [[] | state.doc_context],
        typedoc_context: [[] | state.typedoc_context],
        optional_callbacks_context: [[] | state.optional_callbacks_context]
    }
  end

  def remove_module(%__MODULE__{} = state) do
    %{
      state
      | module: tl(state.module),
        scopes: tl(state.scopes),
        doc_context: tl(state.doc_context),
        typedoc_context: tl(state.typedoc_context),
        optional_callbacks_context: tl(state.optional_callbacks_context)
    }
  end

  def add_typespec_namespace(%__MODULE__{} = state, name, arity) do
    %{state | scopes: [{:typespec, name, arity} | state.scopes]}
  end

  def register_optional_callbacks(%__MODULE__{} = state, list) do
    [_ | rest] = state.optional_callbacks_context
    %{state | optional_callbacks_context: [list | rest]}
  end

  def apply_optional_callbacks(%__MODULE__{} = state, %__MODULE__.Env{} = env) do
    [list | _rest] = state.optional_callbacks_context
    module = env.module

    updated_specs =
      list
      |> Enum.reduce(state.specs, fn {fun, arity}, acc ->
        acc
        |> Map.update!({module, fun, arity}, fn spec_info = %SpecInfo{} ->
          %{spec_info | meta: spec_info.meta |> Map.put(:optional, true)}
        end)
      end)

    %{state | specs: updated_specs}
  end

  def new_named_func(%__MODULE__{} = state, name, arity) do
    %{state | scopes: [{name, arity} | state.scopes]}
  end

  def maybe_add_protocol_implementation(
        %__MODULE__{} = state,
        protocol = {_protocol, _implementations}
      ) do
    %__MODULE__{state | protocols: [protocol | state.protocols]}
  end

  def maybe_add_protocol_implementation(%__MODULE__{} = state, _) do
    %__MODULE__{state | protocols: [nil | state.protocols]}
  end

  def remove_protocol_implementation(%__MODULE__{} = state) do
    %__MODULE__{state | protocols: tl(state.protocols)}
  end

  def remove_last_scope_from_scopes(%__MODULE__{} = state) do
    %__MODULE__{state | scopes: tl(state.scopes)}
  end

  def add_current_module_to_index(%__MODULE__{} = state, position, end_position, options)
      when (is_tuple(position) and is_tuple(end_position)) or is_nil(end_position) do
    current_module = get_current_module(state)

    add_module_to_index(state, current_module, position, end_position, options)
  end

  def add_module_to_index(%__MODULE__{} = state, module, position, end_position, options)
      when (is_tuple(position) and is_tuple(end_position)) or is_nil(end_position) do
    # TODO :defprotocol, :defimpl?
    add_mod_fun_to_position(
      state,
      {module, nil, nil},
      position,
      end_position,
      nil,
      :defmodule,
      "",
      %{},
      options
    )
  end

  # TODO require end position
  def add_func_to_index(
        state,
        env,
        func,
        params,
        position,
        end_position \\ nil,
        type,
        options \\ []
      )

  def add_func_to_index(
        %__MODULE__{} = state,
        env,
        func,
        params,
        position,
        end_position,
        type,
        options
      )
      when (is_tuple(position) and is_tuple(end_position)) or is_nil(end_position) do
    current_module = env.module
    arity = length(params)

    {state, {doc, meta}} =
      if not state.generated and Keyword.get(options, :generated, false) do
        # do not consume docs on generated functions
        # NOTE state.generated is set when expanding use macro
        # we want to consume docs there
        {state, {"", %{generated: true}}}
      else
        consume_doc_context(state)
      end

    hidden = Map.get(meta, :hidden)

    # underscored and @impl defs are hidden by default unless they have @doc
    meta =
      if (String.starts_with?(to_string(func), "_") or hidden == :impl) and doc == "" do
        Map.put(meta, :hidden, true)
      else
        if hidden != true do
          Map.delete(meta, :hidden)
        else
          meta
        end
      end

    meta =
      if type == :defdelegate do
        {target_module_expression, target_fun} = options[:target]
        {module, _state, _env} = expand(target_module_expression, state)

        Map.put(
          meta,
          :delegate_to,
          {module, target_fun, arity}
        )
      else
        meta
      end

    meta =
      if type in [:defguard, :defguardp] do
        Map.put(meta, :guard, true)
      else
        meta
      end

    doc =
      if type in [:defp, :defmacrop] do
        # documentation is discarded on private
        ""
      else
        doc
      end

    add_mod_fun_to_position(
      state,
      {current_module, func, arity},
      position,
      end_position,
      params,
      type,
      doc,
      meta,
      options
    )
  end

  def make_overridable(
        %__MODULE__{} = state,
        env,
        fa_list,
        overridable_module
      ) do
    module = env.module

    mods_funs_to_positions =
      fa_list
      |> Enum.reduce(state.mods_funs_to_positions, fn {f, a}, mods_funs_to_positions_acc ->
        if Map.has_key?(mods_funs_to_positions_acc, {module, f, a}) do
          mods_funs_to_positions_acc
          |> make_def_overridable({module, f, a}, overridable_module)
        else
          # Some behaviour callbacks can be not implemented by __using__ macro
          mods_funs_to_positions_acc
        end
      end)

    %__MODULE__{state | mods_funs_to_positions: mods_funs_to_positions}
  end

  defp make_def_overridable(mods_funs_to_positions, mfa, overridable_module) do
    update_in(mods_funs_to_positions[mfa], fn mod_fun_info = %ModFunInfo{} ->
      %ModFunInfo{
        mod_fun_info
        | overridable: {true, overridable_module},
          meta: Map.put(mod_fun_info.meta, :overridable, true)
      }
    end)
  end

  def new_vars_scope(%__MODULE__{} = state) do
    scope_id = state.scope_id_count + 1

    %__MODULE__{
      state
      | scope_ids: [scope_id | state.scope_ids],
        scope_id_count: scope_id,
        vars_info: [[] | state.vars_info],
        scope_vars_info: [[] | state.scope_vars_info]
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
        vars_info: [[] | state.vars_info],
        scope_vars_info: [[]]
    }
  end

  def new_attributes_scope(%__MODULE__{} = state) do
    %__MODULE__{state | attributes: [[] | state.attributes], scope_attributes: [[]]}
  end

  def remove_vars_scope(%__MODULE__{} = state) do
    %__MODULE__{
      state
      | scope_ids: tl(state.scope_ids),
        vars_info: tl(state.vars_info),
        scope_vars_info: tl(state.scope_vars_info),
        vars_info_per_scope_id: update_vars_info_per_scope_id(state)
    }
  end

  def remove_func_vars_scope(%__MODULE__{} = state) do
    %__MODULE__{
      state
      | scope_ids: tl(state.scope_ids),
        vars_info: tl(state.vars_info),
        scope_vars_info: tl(state.vars_info),
        vars_info_per_scope_id: update_vars_info_per_scope_id(state)
    }
  end

  def update_vars_info_per_scope_id(state) do
    [scope_id | _other_scope_ids] = state.scope_ids

    [current_scope_vars | other_scope_vars] = state.scope_vars_info

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

  def add_alias(%__MODULE__{} = state, {alias, aliased}) when is_list(aliased) do
    if Introspection.elixir_module?(alias) do
      alias = Module.split(alias) |> Enum.take(-1) |> Module.concat()
      [aliases_from_scope | inherited_aliases] = state.aliases
      aliases_from_scope = aliases_from_scope |> Enum.reject(&match?({^alias, _}, &1))

      # TODO pass env
      {expanded, state, _env} = expand(aliased, state)

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

  def add_alias(%__MODULE__{} = state, _), do: state

  def new_lexical_scope(%__MODULE__{} = state) do
    %__MODULE__{
      state
      | functions: [hd(state.functions) | state.functions],
        macros: [hd(state.macros) | state.macros],
        requires: [[] | state.requires],
        aliases: [[] | state.aliases]
    }
  end

  def remove_lexical_scope(%__MODULE__{} = state) do
    %__MODULE__{
      state
      | functions: tl(state.functions),
        macros: tl(state.macros),
        requires: tl(state.requires),
        aliases: tl(state.aliases)
    }
  end

  def add_import(%__MODULE__{} = state, module, opts) when is_atom(module) do
    {functions, macros} =
      Introspection.expand_import(
        {hd(state.functions), hd(state.macros)},
        module,
        opts,
        state.mods_funs_to_positions
      )

    %__MODULE__{
      state
      | functions: [functions | tl(state.functions)],
        macros: [macros | tl(state.macros)]
    }
  end

  def add_import(%__MODULE__{} = state, _module, _opts), do: state

  def add_require(%__MODULE__{} = state, module) when is_atom(module) do
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

  def add_type(
        %__MODULE__{} = state,
        %__MODULE__.Env{} = env,
        type_name,
        type_args,
        spec,
        kind,
        pos,
        end_pos,
        options \\ []
      ) do
    arg_names =
      type_args
      |> Enum.map(&Macro.to_string/1)

    {state, {doc, meta}} = consume_typedoc_context(state)

    # underscored types are hidden by default unless they have @typedoc
    meta =
      if String.starts_with?(to_string(type_name), "_") and doc == "" do
        Map.put(meta, :hidden, true)
      else
        meta
      end

    meta =
      if kind == :opaque do
        Map.put(meta, :opaque, true)
      else
        meta
      end

    doc =
      if kind == :typep do
        # documentation is discarded on private
        ""
      else
        doc
      end

    type_info = %TypeInfo{
      name: type_name,
      args: [arg_names],
      kind: kind,
      specs: [spec],
      generated: [Keyword.get(options, :generated, false)],
      positions: [pos],
      end_positions: [end_pos],
      doc: doc,
      meta: meta
    }

    current_module = env.module

    types =
      state.types
      |> Map.put({current_module, type_name, length(arg_names)}, type_info)

    %__MODULE__{state | types: types}
  end

  defp combine_specs(nil, new), do: new

  defp combine_specs(%SpecInfo{} = existing, %SpecInfo{} = new) do
    %SpecInfo{
      existing
      | positions: [hd(new.positions) | existing.positions],
        end_positions: [hd(new.end_positions) | existing.end_positions],
        generated: [hd(new.generated) | existing.generated],
        args: [hd(new.args) | existing.args],
        specs: [hd(new.specs) | existing.specs]
    }
  end

  def add_spec(
        %__MODULE__{} = state,
        %__MODULE__.Env{} = env,
        type_name,
        type_args,
        spec,
        kind,
        pos,
        end_pos,
        options
      ) do
    arg_names =
      type_args
      |> Enum.map(&Macro.to_string/1)

    {state, {doc, meta}} =
      if kind in [:callback, :macrocallback] do
        consume_doc_context(state)
      else
        # do not consume doc context for specs
        {state, {"", %{}}}
      end

    # underscored callbacks are hidden by default unless they have @doc
    meta =
      if String.starts_with?(to_string(type_name), "_") and doc == "" do
        Map.put(meta, :hidden, true)
      else
        meta
      end

    type_info = %SpecInfo{
      name: type_name,
      args: [arg_names],
      specs: [spec],
      kind: kind,
      generated: [Keyword.get(options, :generated, false)],
      positions: [pos],
      end_positions: [end_pos],
      doc: doc,
      meta: meta
    }

    current_module = env.module

    arity_info =
      combine_specs(state.specs[{current_module, type_name, length(arg_names)}], type_info)

    specs =
      state.specs
      |> Map.put({current_module, type_name, length(arg_names)}, arity_info)

    %__MODULE__{state | specs: specs}
  end

  def add_var(
        %__MODULE__{} = state,
        %VarInfo{name: var_name} = var_info,
        is_definition
      ) do
    scope = hd(state.scopes)
    [vars_from_scope | other_vars] = state.vars_info
    is_var_defined = is_variable_defined(state, var_name)
    var_name_as_string = Atom.to_string(var_name)

    vars_from_scope =
      case {is_definition and var_info.is_definition, is_var_defined, var_name_as_string} do
        {_, _, "__MODULE__"} ->
          raise "foo"

        {_, _, "_"} ->
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
      | vars_info: [vars_from_scope | other_vars],
        scope_vars_info: [vars_from_scope | tl(state.scope_vars_info)]
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

  def add_behaviour(module, %__MODULE__{} = state, env) when is_atom(module) do
    state =
      update_in(state.behaviours[env.module], &Enum.uniq([module | &1 || []]))

    {module, state, env}
  end

  def add_behaviour(_module, %__MODULE__{} = state, env), do: {nil, state, env}

  def register_doc(%__MODULE__{} = state, %__MODULE__.Env{} = env, :moduledoc, doc_arg) do
    current_module = env.module
    doc_arg_formatted = format_doc_arg(doc_arg)

    mods_funs_to_positions =
      state.mods_funs_to_positions
      |> Map.update!({current_module, nil, nil}, fn info = %ModFunInfo{} ->
        case doc_arg_formatted do
          {:meta, meta} ->
            %{info | meta: Map.merge(info.meta, meta)}

          text_or_hidden ->
            %{info | doc: text_or_hidden}
        end
      end)

    %{state | mods_funs_to_positions: mods_funs_to_positions}
  end

  def register_doc(%__MODULE__{} = state, %__MODULE__.Env{}, :doc, doc_arg) do
    [doc_context | doc_context_rest] = state.doc_context

    %{state | doc_context: [[doc_arg | doc_context] | doc_context_rest]}
  end

  def register_doc(%__MODULE__{} = state, %__MODULE__.Env{}, :typedoc, doc_arg) do
    [doc_context | doc_context_rest] = state.typedoc_context

    %{state | typedoc_context: [[doc_arg | doc_context] | doc_context_rest]}
  end

  defp consume_doc_context(%__MODULE__{} = state) do
    [doc_context | doc_context_rest] = state.doc_context
    state = %{state | doc_context: [[] | doc_context_rest]}

    {state, reduce_doc_context(doc_context)}
  end

  defp consume_typedoc_context(%__MODULE__{} = state) do
    [doc_context | doc_context_rest] = state.typedoc_context
    state = %{state | typedoc_context: [[] | doc_context_rest]}

    {state, reduce_doc_context(doc_context)}
  end

  defp reduce_doc_context(doc_context) do
    Enum.reduce(doc_context, {"", %{}}, fn doc_arg, {doc_acc, meta_acc} ->
      case format_doc_arg(doc_arg) do
        {:meta, meta} -> {doc_acc, Map.merge(meta_acc, meta)}
        doc -> {doc, meta_acc}
      end
    end)
  end

  defp format_doc_arg(binary) when is_binary(binary), do: binary

  defp format_doc_arg(list) when is_list(list) do
    if Keyword.keyword?(list) do
      {:meta, Map.new(list)}
    else
      to_string(list)
    end
  end

  defp format_doc_arg(false), do: {:meta, %{hidden: true}}
  defp format_doc_arg(:impl), do: {:meta, %{hidden: :impl}}

  defp format_doc_arg(quoted) do
    try do
      case Code.eval_quoted(quoted) do
        {binary, _} when is_binary(binary) ->
          binary

        {list, _} when is_list(list) ->
          if Keyword.keyword?(list) do
            {:meta, Map.new(list)}
          else
            to_string(list)
          end

        other ->
          Logger.warning("""
          Unable to format docstring expression:

          #{inspect(quoted, pretty: true)}

          Eval resulted in:

          #{inspect(other)}
          """)

          ""
      end
    rescue
      e ->
        Logger.warning("""
        Unable to format docstring expression:

        #{inspect(quoted, pretty: true)}

        #{Exception.format(:error, e, __STACKTRACE__)}
        """)

        ""
    end
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
    [current_scope_vars | other_vars] = state.vars_info

    var =
      Enum.find(current_scope_vars, fn %VarInfo{positions: positions} -> position in positions end)

    current_scope_vars =
      Enum.reject(current_scope_vars, fn %VarInfo{positions: positions} ->
        position in positions
      end)

    state = %__MODULE__{
      state
      | vars_info: [current_scope_vars | other_vars],
        scope_vars_info: [current_scope_vars | tl(state.scope_vars_info)]
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

  def default_env, do: %ElixirSense.Core.State.Env{}

  def expand(ast, %__MODULE__{} = state) do
    expand(ast, state, get_current_env(state))
  end

  def expand({:@, meta, [{:behaviour, _, [arg]}]}, state, env) do
    line = Keyword.fetch!(meta, :line)

    state =
      state
      |> add_current_env_to_line(line)

    {arg, state, env} = expand(arg, state, env)
    add_behaviour(arg, state, env)
  end

  def expand({:defoverridable, meta, [arg]}, state, env) do
    {arg, state, env} = expand(arg, state, env)

    case arg do
      keyword when is_list(keyword) ->
        {nil, make_overridable(state, env, keyword, meta[:context]), env}

      behaviour_module when is_atom(behaviour_module) ->
        if Code.ensure_loaded?(behaviour_module) and
             function_exported?(behaviour_module, :behaviour_info, 1) do
          keyword =
            behaviour_module.behaviour_info(:callbacks)
            |> Enum.map(&Introspection.drop_macro_prefix/1)

          {nil, make_overridable(state, env, keyword, meta[:context]), env}
        else
          {nil, state, env}
        end

      _ ->
        {nil, state, env}
    end
  end

  def expand({form, meta, [{{:., _, [base, :{}]}, _, refs} | rest]}, state, env)
      when form in [:require, :alias, :import, :use] do
    case rest do
      [] ->
        expand_multi_alias_call(form, meta, base, refs, [], state, env)

      [opts] ->
        opts = Keyword.delete(opts, :as)
        # if Keyword.has_key?(opts, :as) do
        #   raise "as_in_multi_alias_call"
        # end

        expand_multi_alias_call(form, meta, base, refs, opts, state, env)
    end
  end

  def expand({form, meta, [arg]}, state, env) when form in [:require, :alias, :import] do
    expand({form, meta, [arg, []]}, state, env)
  end

  def expand(module, %__MODULE__{} = state, %__MODULE__.Env{} = env) when is_atom(module) do
    {module, state, env}
  end

  def expand({:alias, meta, [arg, opts]}, state, env) do
    line = Keyword.fetch!(meta, :line)
    column = Keyword.fetch!(meta, :column)

    {arg, state, env} = expand(arg, state, env)
    # options = expand(no_alias_opts(arg), state, env, env)

    if is_atom(arg) do
      state = add_first_alias_positions(state, line, column)

      alias_tuple =
        case Keyword.get(opts, :as) do
          nil ->
            {Module.concat([List.last(Module.split(arg))]), arg}

          as ->
            # alias with `as:` option
            {no_alias_expansion(as), arg}
        end

      state =
        state
        |> add_current_env_to_line(line)
        |> add_alias(alias_tuple)

      {arg, state, env}
    else
      {nil, state, env}
    end
  rescue
    ArgumentError -> {nil, state, env}
  end

  def expand({:require, meta, [arg, opts]}, state, env) do
    line = Keyword.fetch!(meta, :line)

    {arg, state, env} = expand(arg, state, env)
    # opts = expand(no_alias_opts(opts), state, env)

    if is_atom(arg) do
      state =
        state
        |> add_current_env_to_line(line)

      state =
        case Keyword.get(opts, :as) do
          nil ->
            state

          as ->
            # require with `as:` option
            alias_tuple = {no_alias_expansion(as), arg}
            add_alias(state, alias_tuple)
        end
        |> add_require(arg)

      {arg, state, env}
    else
      {nil, state, env}
    end
  end

  def expand({:import, meta, [arg, opts]}, state, env) do
    line = Keyword.fetch!(meta, :line)

    {arg, state, env} = expand(arg, state, env)
    # opts = expand(no_alias_opts(opts), state, env)

    if is_atom(arg) do
      state =
        state
        |> add_current_env_to_line(line)
        |> add_require(arg)
        |> add_import(arg, opts)

      {arg, state, env}
    else
      {nil, state, env}
    end
  end

  def expand({:use, _meta, []} = ast, state, env) do
    # defmacro use in Kernel
    {ast, state, env}
  end

  def expand({:use, meta, _} = ast, state, env) do
    alias ElixirSense.Core.MacroExpander
    line = Keyword.fetch!(meta, :line)

    state =
      state
      |> add_current_env_to_line(line)

    # TODO pass env
    expanded_ast =
      ast
      |> MacroExpander.add_default_meta()
      |> MacroExpander.expand_use(
        env.module,
        env.aliases,
        meta |> Keyword.take([:line, :column])
      )

    {{:__generated__, [], [expanded_ast]}, %{state | generated: true}, env}
  end

  def expand(
        {:__aliases__, _, [Elixir | _] = module},
        %__MODULE__{} = state,
        %__MODULE__.Env{} = env
      ) do
    {Module.concat(module), state, env}
  end

  def expand({:__MODULE__, _, nil}, %__MODULE__{} = state, %__MODULE__.Env{} = env) do
    {env.module, state, env}
  end

  def expand(
        {:__aliases__, _, [{:__MODULE__, _, nil} | rest]},
        %__MODULE__{} = state,
        %__MODULE__.Env{} = env
      ) do
    {Module.concat([env.module | rest]), state, env}
  end

  def expand({:__aliases__, _, module}, %__MODULE__{} = state, %__MODULE__.Env{} = env)
      when is_list(module) do
    {Introspection.expand_alias(Module.concat(module), env.aliases), state, env}
  end

  def expand(ast, %__MODULE__{} = state, %__MODULE__.Env{} = env) do
    {ast, state, env}
  end

  def maybe_move_vars_to_outer_scope(%__MODULE__{} = state) do
    scope_vars = move_references_to_outer_scope(state.scope_vars_info)
    vars = move_references_to_outer_scope(state.vars_info)

    %__MODULE__{state | vars_info: vars, scope_vars_info: scope_vars}
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

  def no_alias_expansion({:__aliases__, _, [h | t]} = _aliases) when is_atom(h) do
    Module.concat([h | t])
  end

  def no_alias_expansion(other), do: other

  # defmodule Elixir.Alias
  def alias_defmodule({:__aliases__, _, [Elixir, _ | _]}, module, state, env),
    do: {module, state, env}

  # defmodule Alias in root
  def alias_defmodule({:__aliases__, _, _}, module, state, %{module: nil} = env),
    do: {module, state, env}

  # defmodule Alias nested
  def alias_defmodule({:__aliases__, _meta, [h | t]}, _module, state, env) when is_atom(h) do
    module = Module.concat([env.module, h])
    alias = String.to_atom("Elixir." <> Atom.to_string(h))
    # {:ok, env} = Macro.Env.define_alias(env, meta, module, as: alias, trace: false)
    state = add_alias(state, {alias, module})

    case t do
      [] -> {module, state, env}
      _ -> {String.to_atom(Enum.join([module | t], ".")), state, env}
    end
  end

  # defmodule _
  def alias_defmodule(_raw, module, state, env) do
    {module, state, env}
  end

  defp expand_multi_alias_call(kind, meta, base, refs, opts, state, env) do
    {base_ref, state, env} = expand(base, state, env)

    fun = fn
      {:__aliases__, _, ref}, state, env ->
        expand({kind, meta, [Module.concat([base_ref | ref]), opts]}, state, env)

      ref, state, env when is_atom(ref) ->
        expand({kind, meta, [Module.concat([base_ref, ref]), opts]}, state, env)

      _other, s, e ->
        {nil, s, e}
        # raise "expected_compile_time_module"
    end

    map_fold(fun, state, env, refs)
  end

  defp map_fold(fun, s, e, list), do: map_fold(fun, s, e, list, [])

  defp map_fold(fun, s, e, [h | t], acc) do
    {rh, rs, re} = fun.(h, s, e)
    map_fold(fun, rs, re, t, [rh | acc])
  end

  defp map_fold(_fun, s, e, [], acc), do: {Enum.reverse(acc), s, e}

  @module_functions [
    {:__info__, [:atom], :def},
    {:module_info, [], :def},
    {:module_info, [:atom], :def}
  ]

  def add_module_functions(state, env, functions, position, end_position) do
    {line, column} = position

    (functions ++ @module_functions)
    |> Enum.reduce(state, fn {name, args, kind}, acc ->
      mapped_args = for arg <- args, do: {arg, [line: line, column: column], nil}

      acc
      |> add_func_to_index(
        env,
        name,
        mapped_args,
        position,
        end_position,
        kind,
        generated: true
      )
    end)
  end
  
  def macro_env(%__MODULE__{} = state, meta \\ []) do
    function =
      case hd(hd(state.scopes)) do
        {function, arity} -> {function, arity}
        _ -> nil
      end

    context_modules =
      state.mods_funs_to_positions
      |> Enum.filter(&match?({{_module, nil, nil}, _}, &1))
      |> Enum.map(&(elem(&1, 0) |> elem(0)))

    %Macro.Env{
      aliases: current_aliases(state),
      requires: current_requires(state),
      module: get_current_module(state),
      line: Keyword.get(meta, :line, 0),
      function: function,
      # TODO context :guard, :match
      context: nil,
      context_modules: context_modules,
      functions: state.functions |> hd,
      macros: state.macros |> hd
      # TODO macro_aliases
      # TODO versioned_vars
    }
  end

  def macro_env(%__MODULE__{} = state, %__MODULE__.Env{} = env, line) do
    function =
      case env.scope do
        {function, arity} -> {function, arity}
        _ -> nil
      end

    context_modules =
      state.mods_funs_to_positions
      |> Enum.filter(&match?({{_module, nil, nil}, _}, &1))
      |> Enum.map(&(elem(&1, 0) |> elem(0)))

    %Macro.Env{
      aliases: env.aliases,
      requires: env.requires,
      module: env.module,
      line: line,
      function: function,
      # TODO context :guard, :match
      context: nil,
      context_modules: context_modules,
      functions: env.functions,
      macros: env.macros
      # TODO macro_aliases
      # TODO versioned_vars
    }
  end
end
