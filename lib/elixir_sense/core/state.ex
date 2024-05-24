defmodule ElixirSense.Core.State do
  @moduledoc """
  Core State
  """

  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.BuiltinFunctions
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
          vars_info:
            list(%{optional({atom, non_neg_integer}) => ElixirSense.Core.State.VarInfo.t()}),
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
          macro_env: list(Macro.Env.t()),
          typespec: nil | {atom, arity},
          protocol: nil | {atom, [atom]},
          cursor_env: nil | {keyword, ElixirSense.Core.State.Env.t()}
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
            vars_info: [%{}],
            vars: {%{}, false},
            unused: 0,
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
            macro_env: [:elixir_env.new()],
            typespec: nil,
            protocol: nil,
            cursor_env: nil

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

  def get_current_env(%__MODULE__{} = state, macro_env) do
    current_attributes = state |> get_current_attributes()
    current_behaviours = state.behaviours |> Map.get(macro_env.module, [])

    current_scope_id = hd(state.scope_ids)

    # Macro.Env versioned_vars is not updated
    # versioned_vars: macro_env.versioned_vars,
    {versioned_vars, _} = state.vars

    # vars_info has both read and write vars
    # filter to return only read
    [current_vars_info | _] = state.vars_info
    vars = for {{name, context}, version} <- versioned_vars, context == nil do
      Map.fetch!(current_vars_info, {name, version})
    end

    current_protocol =
      case state.protocol do
        nil ->
          nil

        {protocol, for_list} ->
          # check wether we are in implementation or implementation child module
          if Enum.any?(for_list, fn for -> macro_env.module == Module.concat(protocol, for) end) do
            {protocol, for_list}
          end
      end

    %Env{
      functions: macro_env.functions,
      macros: macro_env.macros,
      requires: macro_env.requires,
      aliases: macro_env.aliases,
      module: macro_env.module,
      function: macro_env.function,
      vars: vars,
      versioned_vars: versioned_vars,
      attributes: current_attributes,
      behaviours: current_behaviours,
      typespec: state.typespec,
      scope_id: current_scope_id,
      protocol: current_protocol
    }
  end

  def get_current_module(%__MODULE__{} = state) do
    state.module |> hd
  end

  def add_cursor_env(%__MODULE__{} = state, meta, macro_env) do
    env = get_current_env(state, macro_env)
    %__MODULE__{state | cursor_env: {meta, env}}
  end

  def add_current_env_to_line(%__MODULE__{} = state, line, macro_env) when is_integer(line) do
    env = get_current_env(state, macro_env)
    %__MODULE__{state | lines_to_env: Map.put(state.lines_to_env, line, env)}
  end

  def add_moduledoc_positions(
        %__MODULE__{} = state,
        env,
        meta
      ) do
    module = env.module

    case Keyword.get(meta, :end_of_expression) do
      nil ->
        state

      end_of_expression ->
        line_to_insert_alias = Keyword.fetch!(end_of_expression, :line) + 1
        column = Keyword.get(meta, :column, 1)

        %__MODULE__{
          state
          | moduledoc_positions:
              Map.put(state.moduledoc_positions, module, {line_to_insert_alias, column})
        }
    end
  end

  def add_first_alias_positions(
        %__MODULE__{} = state,
        %{module: module, function: nil},
        meta
      ) do
    # TODO shouldn't that look for end_of_expression
    line = Keyword.get(meta, :line, 0)

    if line > 0 do
      column = Keyword.get(meta, :column, 1)

      %__MODULE__{
        state
        | first_alias_positions: Map.put_new(state.first_alias_positions, module, {line, column})
      }
    else
      state
    end
  end

  def add_first_alias_positions(%__MODULE__{} = state, _env, _meta), do: state

  def add_call_to_line(
        %__MODULE__{} = state,
        {{:@, _meta, [{name, _name_meta, _args}]}, func, arity},
        {_line, _column} = position
      )
      when is_atom(name) do
    add_call_to_line(state, {{:attribute, name}, func, arity}, position)
  end

  def add_call_to_line(
        %__MODULE__{} = state,
        {{name, _name_meta, args}, func, arity},
        {_line, _column} = position
      )
      when is_atom(args) do
    add_call_to_line(state, {{:variable, name}, func, arity}, position)
  end

  def add_call_to_line(
        %__MODULE__{} = state,
        {nil, {:@, _meta, [{name, _name_meta, _args}]}, arity},
        {_line, _column} = position
      )
      when is_atom(name) do
    add_call_to_line(state, {nil, {:attribute, name}, arity}, position)
  end

  def add_call_to_line(
        %__MODULE__{} = state,
        {nil, {name, _name_meta, args}, arity},
        {_line, _column} = position
      )
      when is_atom(args) do
    add_call_to_line(state, {nil, {:variable, name}, arity}, position)
  end

  def add_call_to_line(%__MODULE__{} = state, {mod, func, arity}, {line, _column} = position) do
    call = %CallInfo{mod: mod, func: func, arity: arity, position: position}

    calls =
      Map.update(state.calls, line, [call], fn line_calls ->
        [call | line_calls]
      end)

    %__MODULE__{state | calls: calls}
  end

  def add_struct(%__MODULE__{} = state, env, type, fields) do
    structs =
      state.structs
      |> Map.put(env.module, %StructInfo{type: type, fields: fields ++ [__struct__: env.module]})

    %__MODULE__{state | structs: structs}
  end

  def get_current_attributes(%__MODULE__{} = state) do
    state.scope_attributes |> :lists.reverse() |> List.flatten()
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

  def add_module(%__MODULE__{} = state) do
    %__MODULE__{
      state
      | doc_context: [[] | state.doc_context],
        typedoc_context: [[] | state.typedoc_context],
        optional_callbacks_context: [[] | state.optional_callbacks_context]
    }
  end

  def remove_module(%__MODULE__{} = state) do
    %{
      state
      | doc_context: tl(state.doc_context),
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

  def apply_optional_callbacks(%__MODULE__{} = state, env) do
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
        vars_info: [hd(state.vars_info) | state.vars_info]
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
        vars_info: [%{} | state.vars_info]
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
        vars_info_per_scope_id: update_vars_info_per_scope_id(state)
    }
  end

  def remove_func_vars_scope(%__MODULE__{} = state) do
    %__MODULE__{
      state
      | scope_ids: tl(state.scope_ids),
        vars_info: tl(state.vars_info),
        vars_info_per_scope_id: update_vars_info_per_scope_id(state)
    }
  end

  def update_vars_info_per_scope_id(state) do
    [scope_id | _other_scope_ids] = state.scope_ids
    [current_scope_vars | _other_scope_vars] = state.vars_info

    Map.put(state.vars_info_per_scope_id, scope_id, current_scope_vars |> Map.values())
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
        env,
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
        env,
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

  def add_var_write(%__MODULE__{} = state, {name, meta, _}) when name != :_ do
    version = meta[:version]
    line = meta[:line]
    column = meta[:column]
    scope_id = hd(state.scope_ids)

    info = %VarInfo{
      name: name,
      is_definition: true,
      positions: [{line, column}],
      scope_id: scope_id
    }

    [vars_from_scope | other_vars] = state.vars_info
    vars_from_scope = Map.put(vars_from_scope, {name, version}, info)

    %__MODULE__{
      state
      | vars_info: [vars_from_scope | other_vars]
    }
  end

  def add_var_write(%__MODULE__{} = state, _), do: state

  def add_var_read(%__MODULE__{} = state, {name, meta, _}) when name != :_ do
    version = meta[:version]
    line = meta[:line]
    column = meta[:column]

    [vars_from_scope | other_vars] = state.vars_info
    info = Map.fetch!(vars_from_scope, {name, version})

    info = %VarInfo{info | positions: (info.positions ++ [{line, column}]) |> Enum.uniq()}
    vars_from_scope = Map.put(vars_from_scope, {name, version}, info)

    %__MODULE__{
      state
      | vars_info: [vars_from_scope | other_vars]
    }
  end

  def add_var_read(%__MODULE__{} = state, _), do: state

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

  def register_doc(%__MODULE__{} = state, env, :moduledoc, doc_arg) do
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

  def register_doc(%__MODULE__{} = state, _env, :doc, doc_arg) do
    [doc_context | doc_context_rest] = state.doc_context

    %{state | doc_context: [[doc_arg | doc_context] | doc_context_rest]}
  end

  def register_doc(%__MODULE__{} = state, _env, :typedoc, doc_arg) do
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
    # TODO pass env and expand metadata
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
      # TODO pass env to eval
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

  defp merge_type(nil, new), do: new
  defp merge_type(old, nil), do: old
  defp merge_type(old, old), do: old
  defp merge_type(old, new), do: {:intersection, [old, new]}

  def default_env, do: %ElixirSense.Core.State.Env{}

  def expand(ast, %__MODULE__{} = state) do
    expand(ast, state, nil)
  end

  def expand({:@, meta, [{:behaviour, _, [arg]}]}, state, env) do
    line = Keyword.fetch!(meta, :line)

    state =
      state
      |> add_current_env_to_line(line, env)

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

  def expand(module, %__MODULE__{} = state, env) when is_atom(module) do
    {module, state, env}
  end

  def expand({:alias, meta, [arg, opts]}, state, env) do
    line = Keyword.fetch!(meta, :line)

    {arg, state, env} = expand(arg, state, env)
    # options = expand(no_alias_opts(arg), state, env, env)

    if is_atom(arg) do
      state = add_first_alias_positions(state, env, meta)

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
        |> add_current_env_to_line(line, env)
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
        |> add_current_env_to_line(line, env)

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
        |> add_current_env_to_line(line, env)
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

  def expand({:use, meta, [_ | _]} = ast, state, env) do
    alias ElixirSense.Core.MacroExpander
    line = Keyword.fetch!(meta, :line)

    state =
      state
      |> add_current_env_to_line(line, env)

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
        env
      ) do
    {Module.concat(module), state, env}
  end

  def expand({:__MODULE__, _, nil}, %__MODULE__{} = state, env) do
    {env.module, state, env}
  end

  def expand(
        {:__aliases__, _, [{:__MODULE__, _, nil} | rest]},
        %__MODULE__{} = state,
        env
      ) do
    {Module.concat([env.module | rest]), state, env}
  end

  def expand({:__aliases__, _, module}, %__MODULE__{} = state, env)
      when is_list(module) do
    {Introspection.expand_alias(Module.concat(module), env.aliases), state, env}
  end

  def expand(ast, %__MODULE__{} = state, env) do
    {ast, state, env}
  end

  def maybe_move_vars_to_outer_scope(
        %__MODULE__{vars_info: [current_scope_vars, outer_scope_vars | other_scopes_vars]} = state
      ) do
    outer_scope_vars =
      for {key, _} <- outer_scope_vars,
          into: %{},
          # TODO merge type?
          do: {key, current_scope_vars[key]}

    vars_info = [current_scope_vars, outer_scope_vars | other_scopes_vars]

    %__MODULE__{state | vars_info: vars_info}
  end

  def maybe_move_vars_to_outer_scope(state), do: state

  def no_alias_expansion({:__aliases__, _, [h | t]} = _aliases) when is_atom(h) do
    Module.concat([h | t])
  end

  def no_alias_expansion(other), do: other

  def alias_defmodule({:__aliases__, _, [Elixir, h | t]}, module, state, env) do
    if t == [] and Version.match?(System.version(), "< 1.16.0-dev") do
      # on elixir < 1.16 unaliasing is happening
      # https://github.com/elixir-lang/elixir/issues/12456
      alias = String.to_atom("Elixir." <> Atom.to_string(h))
      state = add_alias(state, {alias, module})
      {module, state, env}
    else
      {module, state, env}
    end
  end

  # defmodule Alias in root
  def alias_defmodule({:__aliases__, _, _}, module, state, %{module: nil} = env) do
    {module, state, env}
  end

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
    context_modules =
      state.mods_funs_to_positions
      |> Enum.filter(&match?({{_module, nil, nil}, _}, &1))
      |> Enum.map(&(elem(&1, 0) |> elem(0)))

    %Macro.Env{
      aliases: env.aliases,
      requires: env.requires,
      module: env.module,
      line: line,
      function: env.function,
      # TODO context :guard, :match
      context: nil,
      context_modules: context_modules,
      functions: env.functions,
      macros: env.macros,
      # TODO macro_aliases
      versioned_vars: elem(state.vars, 0)
    }
  end

  def with_typespec(%__MODULE__{} = state, typespec) do
    %{state | typespec: typespec}
  end

  def add_struct_or_exception(state, env, type, fields, {line, column} = position, end_position) do
    fields =
      fields ++
        if type == :defexception do
          [__exception__: true]
        else
          []
        end

    options = [generated: true]

    state =
      if type == :defexception do
        {_, state, env} = add_behaviour(Exception, state, env)

        if Keyword.has_key?(fields, :message) do
          state
          |> add_func_to_index(
            env,
            :exception,
            [{:msg, [line: line, column: column], nil}],
            position,
            end_position,
            :def,
            options
          )
          |> add_func_to_index(
            env,
            :message,
            [{:exception, [line: line, column: column], nil}],
            position,
            end_position,
            :def,
            options
          )
        else
          state
        end
        |> add_func_to_index(
          env,
          :exception,
          [{:args, [line: line, column: column], nil}],
          position,
          end_position,
          :def,
          options
        )
      else
        state
      end
      |> add_func_to_index(env, :__struct__, [], position, end_position, :def, options)
      |> add_func_to_index(
        env,
        :__struct__,
        [{:kv, [line: line, column: column], nil}],
        position,
        end_position,
        :def,
        options
      )

    state
    |> add_struct(env, type, fields)
  end

  def generate_protocol_callbacks(state, env) do
    # turn specs into callbacks or create dummy callbacks
    builtins = BuiltinFunctions.all() |> Keyword.keys()

    current_module = env.module

    keys =
      state.mods_funs_to_positions
      |> Map.keys()
      |> Enum.filter(fn
        {^current_module, name, _arity} when not is_nil(name) ->
          name not in builtins

        _ ->
          false
      end)

    new_specs =
      for key = {_mod, name, _arity} <- keys,
          into: %{},
          do:
            (
              new_spec =
                case state.specs[key] do
                  nil ->
                    %ModFunInfo{positions: positions, params: params} =
                      state.mods_funs_to_positions[key]

                    args = for param_variant <- params do
                      case tl(param_variant) do
                        [] -> ["t()"]
                        other -> ["t()" | Enum.map(other, fn _ -> "term()" end)]
                      end
                    end

                    specs =
                      for arg <- args do
                        joined = Enum.join(arg, ", ")
                        "@callback #{name}(#{joined}) :: term()"
                      end

                    %SpecInfo{
                      name: name,
                      args: args,
                      specs: specs,
                      kind: :callback,
                      positions: positions,
                      end_positions: Enum.map(positions, fn _ -> nil end),
                      generated: Enum.map(positions, fn _ -> true end)
                    }

                  spec = %SpecInfo{specs: specs} ->
                    %SpecInfo{
                      spec
                      | # TODO :spec will get replaced here, refactor into array
                        kind: :callback,
                        specs:
                          specs
                          |> Enum.map(fn s ->
                            String.replace_prefix(s, "@spec", "@callback")
                          end)
                          |> Kernel.++(specs)
                    }
                end

              {key, new_spec}
            )

    specs = Map.merge(state.specs, new_specs)

    %{state | specs: specs}
  end

  def maybe_add_protocol_behaviour(%{protocol: {protocol, _}} = state, env) do
    {_, state, env} = add_behaviour(protocol, state, env)
    {state, env}
  end

  def maybe_add_protocol_behaviour(state, env), do: {state, env}

  def annotate_vars_with_inferred_types(state, vars_with_inferred_types) do
    [h | t] = state.vars_info
    h = Map.merge(h, vars_with_inferred_types)
    %{state | vars_info: [h | t]}
  end
end
