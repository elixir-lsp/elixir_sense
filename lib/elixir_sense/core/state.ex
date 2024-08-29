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
          optional(scope_id_t) => [
            %{optional({atom(), non_neg_integer()}) => ElixirSense.Core.State.VerInfo.t()}
          ]
        }
  @type structs_t :: %{optional(module) => ElixirSense.Core.State.StructInfo.t()}
  @type protocol_t :: {module, nonempty_list(module)}
  @type var_type :: nil | {:atom, atom} | {:map, keyword} | {:struct, keyword, module}

  @type t :: %ElixirSense.Core.State{
          attributes: list(list(ElixirSense.Core.State.AttributeInfo.t())),
          scope_attributes: list(list(atom)),
          behaviours: %{optional(module) => [module]},
          specs: specs_t,
          types: types_t,
          mods_funs_to_positions: mods_funs_to_positions_t,
          structs: structs_t,
          calls: calls_t,
          vars_info:
            list(%{optional({atom, non_neg_integer}) => ElixirSense.Core.State.VarInfo.t()}),
          vars_info_per_scope_id: vars_info_per_scope_id_t,
          scope_id_count: non_neg_integer,
          scope_ids: list(scope_id_t),
          typespec: nil | {atom, arity},
          protocol: nil | {atom, [atom]},

          # elixir_ex
          vars: {map, false | map()},
          unused: non_neg_integer(),
          prematch: atom | tuple,
          stacktrace: boolean(),
          caller: boolean(),
          runtime_modules: list(module),
          first_alias_positions: map(),
          moduledoc_positions: map(),
          doc_context: list(),
          typedoc_context: list(),
          optional_callbacks_context: list(),
          lines_to_env: lines_to_env_t,
          cursor_env: nil | {keyword, ElixirSense.Core.State.Env.t()},
          closest_env:
            nil
            | {{pos_integer, pos_integer}, {non_neg_integer, non_neg_integer},
               ElixirSense.Core.State.Env.t()},
          ex_unit_describe: nil | atom,
          attribute_store: %{optional({module, atom}) => term},
          cursor_position: nil | {pos_integer, pos_integer}
        }

  defstruct attributes: [[]],
            scope_attributes: [[]],
            behaviours: %{},
            specs: %{},
            types: %{},
            mods_funs_to_positions: %{},
            structs: %{},
            calls: %{},
            vars_info: [%{}],
            vars_info_per_scope_id: %{},
            scope_id_count: 0,
            scope_ids: [0],
            typespec: nil,
            protocol: nil,

            # elixir_ex
            vars: {%{}, false},
            unused: 0,
            prematch: :raise,
            stacktrace: false,
            caller: false,
            runtime_modules: [],
            first_alias_positions: %{},
            moduledoc_positions: %{},
            doc_context: [[]],
            typedoc_context: [[]],
            optional_callbacks_context: [[]],
            lines_to_env: %{},
            cursor_env: nil,
            closest_env: nil,
            ex_unit_describe: nil,
            attribute_store: %{},
            cursor_position: nil

  defmodule Env do
    @moduledoc """
    Line environment
    """

    @type t :: %Env{
            functions: [{module, [{atom, arity}]}],
            macros: [{module, [{atom, arity}]}],
            requires: list(module),
            aliases: list(ElixirSense.Core.State.alias_t()),
            macro_aliases: [{module, {term, module}}],
            context: nil | :match | :guard,
            module: nil | module,
            function: nil | {atom, arity},
            protocol: nil | ElixirSense.Core.State.protocol_t(),
            versioned_vars: %{optional({atom, atom}) => non_neg_integer},
            vars: list(ElixirSense.Core.State.VarInfo.t()),
            attributes: list(ElixirSense.Core.State.AttributeInfo.t()),
            behaviours: list(module),
            context_modules: list(module),
            typespec: nil | {atom, arity},
            scope_id: nil | ElixirSense.Core.State.scope_id_t()
          }
    defstruct functions: [],
              macros: [],
              requires: [],
              aliases: [],
              macro_aliases: [],
              # NOTE for protocol implementation this will be the first variant
              module: nil,
              function: nil,
              # NOTE for protocol implementation this will be the first variant
              protocol: nil,
              versioned_vars: %{},
              vars: [],
              attributes: [],
              behaviours: [],
              context_modules: [],
              context: nil,
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
            version: non_neg_integer(),
            type: ElixirSense.Core.State.var_type()
          }
    defstruct name: nil,
              positions: [],
              scope_id: nil,
              version: 0,
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

    def private?(%ModFunInfo{type: type}), do: type in [:defp, :defmacrop, :defguardp]
  end

  defp get_current_env(%__MODULE__{} = state, macro_env) do
    current_attributes = state |> get_current_attributes()
    current_behaviours = state.behaviours |> Map.get(macro_env.module, [])

    current_scope_id = hd(state.scope_ids)

    # Macro.Env versioned_vars is not updated
    # elixir keeps current vars instate
    {versioned_vars, _} = state.vars
    [current_vars_info | _] = state.vars_info

    # here we filter vars to only return the ones with nil context to maintain macro hygiene
    # &n capture args are an exception as they have non nil context everywhere (since elixir 1.17)
    # we return them all but the risk of breaking hygiene is small
    vars =
      for {{name, context}, version} <- versioned_vars,
          context == nil or String.starts_with?(to_string(name), "&") do
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
      macro_aliases: macro_env.macro_aliases,
      module: macro_env.module,
      function: macro_env.function,
      context_modules: macro_env.context_modules,
      context: macro_env.context,
      vars: vars,
      versioned_vars: versioned_vars,
      attributes: current_attributes,
      behaviours: current_behaviours,
      typespec: state.typespec,
      scope_id: current_scope_id,
      protocol: current_protocol
    }
  end

  def add_cursor_env(%__MODULE__{} = state, meta, macro_env) do
    env = get_current_env(state, macro_env)
    %__MODULE__{state | cursor_env: {meta, env}}
  end

  def update_closest_env(%__MODULE__{cursor_position: cursor_position} = state, meta, macro_env)
      when is_list(meta) and cursor_position != nil do
    case Keyword.get(meta, :line, 0) do
      line when is_integer(line) and line > 0 ->
        column = Keyword.get(meta, :column, 0)

        {cursor_line, cursor_column} = cursor_position

        line_distance = abs(cursor_line - line)
        column_distance = abs(cursor_column - column)

        store =
          case state.closest_env do
            nil ->
              true

            {_, {old_line_distance, old_column_distance}, _} ->
              line_distance < old_line_distance or
                (line_distance == old_line_distance and column_distance < old_column_distance)
          end

        if store do
          env = get_current_env(state, macro_env)

          %__MODULE__{
            state
            | closest_env: {{line, column}, {line_distance, column_distance}, env}
          }
        else
          state
        end

      _ ->
        state
    end
  end

  def update_closest_env(%__MODULE__{} = state, _meta, _macro_env) do
    state
  end

  def add_current_env_to_line(%__MODULE__{} = state, meta, macro_env) when is_list(meta) do
    do_add_current_env_to_line(state, Keyword.get(meta, :line, 0), macro_env)
  end

  defp do_add_current_env_to_line(%__MODULE__{} = state, line, macro_env)
       when is_integer(line) and line > 0 do
    env = get_current_env(state, macro_env)
    %__MODULE__{state | lines_to_env: Map.put(state.lines_to_env, line, env)}
  end

  defp do_add_current_env_to_line(%__MODULE__{} = state, _line, _macro_env), do: state

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
        {{:@, _meta, [{name, _name_meta, nil}]}, func, arity},
        meta
      )
      when is_atom(name) do
    do_add_call_to_line(state, {{:attribute, name}, func, arity}, meta)
  end

  def add_call_to_line(
        %__MODULE__{} = state,
        {{name, var_meta, args}, func, arity},
        meta
      )
      when is_atom(name) and is_atom(args) and
             name not in [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__, :_] do
    do_add_call_to_line(
      state,
      {{:variable, name, Keyword.get(var_meta, :version, :any)}, func, arity},
      meta
    )
  end

  def add_call_to_line(
        %__MODULE__{} = state,
        {nil, {:@, _meta, [{name, _name_meta, _args}]}, arity},
        meta
      )
      when is_atom(name) do
    do_add_call_to_line(state, {nil, {:attribute, name}, arity}, meta)
  end

  def add_call_to_line(
        %__MODULE__{} = state,
        {nil, {name, var_meta, args}, arity},
        meta
      )
      when is_atom(name) and is_atom(args) and
             name not in [:__MODULE__, :__DIR__, :__ENV__, :__CALLER__, :__STACKTRACE__, :_] do
    do_add_call_to_line(
      state,
      {nil, {:variable, name, Keyword.get(var_meta, :version, :any)}, arity},
      meta
    )
  end

  def add_call_to_line(state, call, meta) do
    do_add_call_to_line(state, call, meta)
  end

  defp do_add_call_to_line(%__MODULE__{} = state, {mod, func, arity}, meta) do
    # extract_position is not suitable here, we need to handle invalid lines
    line = Keyword.get(meta, :line, 0)
    column = Keyword.get(meta, :column, nil)

    column =
      if column do
        column + Keyword.get(meta, :column_correction, 0)
      end

    call = %CallInfo{mod: mod, func: func, arity: arity, position: {line, column}}

    calls =
      Map.update(state.calls, line, [call], fn line_calls ->
        [call | line_calls]
      end)

    %__MODULE__{state | calls: calls}
  end

  defp add_struct(%__MODULE__{} = state, env, type, fields) do
    structs =
      state.structs
      |> Map.put(env.module, %StructInfo{type: type, fields: fields ++ [__struct__: env.module]})

    %__MODULE__{state | structs: structs}
  end

  defp get_current_attributes(%__MODULE__{} = state) do
    state.scope_attributes |> :lists.reverse() |> List.flatten()
  end

  defp add_mod_fun_to_position(
         %__MODULE__{} = state,
         {module, fun, arity},
         position,
         end_position,
         params,
         type,
         doc,
         meta,
         options
       ) do
    current_info = Map.get(state.mods_funs_to_positions, {module, fun, arity}, %ModFunInfo{})
    current_params = current_info |> Map.get(:params, [])
    current_positions = current_info |> Map.get(:positions, [])
    current_end_positions = current_info |> Map.get(:end_positions, [])
    new_params = [params | current_params]
    new_positions = [position | current_positions]
    new_end_positions = [end_position | current_end_positions]

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
      type: type,
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
         _state,
         info,
         :defdelegate,
         {:target, {target, as, _as_arity}}
       ) do
    # TODO remove this and rely on meta

    %ModFunInfo{
      info
      | target: {target, as}
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

  def add_module_to_index(%__MODULE__{} = state, module, {position, end_position}, options)
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

  def add_func_to_index(
        %__MODULE__{} = state,
        env,
        func,
        params,
        {position, end_position},
        type,
        options \\ []
      )
      when (is_tuple(position) and is_tuple(end_position)) or is_nil(end_position) do
    current_module = env.module
    arity = length(params)

    {state, {doc, meta}} =
      if Keyword.get(options, :generated, false) do
        # do not consume docs on generated functions
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
        {target, as, as_arity} = options[:target]

        Map.put(
          meta,
          :delegate_to,
          {target, as, as_arity}
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
      if type in [:defp, :defmacrop, :defguardp] do
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

  def new_func_vars_scope(%__MODULE__{} = state) do
    scope_id = state.scope_id_count + 1

    %__MODULE__{
      state
      | scope_ids: [scope_id | state.scope_ids],
        scope_id_count: scope_id,
        vars_info: [%{} | state.vars_info],
        # elixir_ex entries
        # each def starts versioning from 0
        unused: 0,
        vars: {%{}, false}
    }
  end

  def new_attributes_scope(%__MODULE__{} = state) do
    %__MODULE__{state | attributes: [[] | state.attributes], scope_attributes: [[]]}
  end

  def remove_vars_scope(
        %__MODULE__{} = state,
        %{vars: vars, unused: unused},
        restore_version_counter \\ false
      ) do
    state = maybe_move_vars_to_outer_scope(state)

    state = %__MODULE__{
      state
      | scope_ids: tl(state.scope_ids),
        vars_info: tl(state.vars_info),
        vars_info_per_scope_id: update_vars_info_per_scope_id(state),
        # restore elixir_ex fields
        vars: vars
    }

    if restore_version_counter do
      # this is used by defmodule as module body does not affect outside versioning
      %__MODULE__{
        state
        | unused: unused
      }
    else
      state
    end
  end

  def remove_func_vars_scope(%__MODULE__{} = state, %{vars: vars, unused: unused}) do
    %__MODULE__{
      state
      | scope_ids: tl(state.scope_ids),
        vars_info: tl(state.vars_info),
        vars_info_per_scope_id: update_vars_info_per_scope_id(state),
        # restore elixir_ex fields
        vars: vars,
        # restore versioning
        unused: unused
    }
  end

  defp update_vars_info_per_scope_id(state) do
    [scope_id | _other_scope_ids] = state.scope_ids
    [current_scope_vars | _other_scope_vars] = state.vars_info

    for {scope_id, vars} <- state.vars_info_per_scope_id, into: %{} do
      updated_vars =
        for {key, var} <- vars, into: %{} do
          {key, Map.get(current_scope_vars, key, var)}
        end

      {scope_id, updated_vars}
    end
    |> Map.put(scope_id, current_scope_vars)
  end

  def remove_attributes_scope(%__MODULE__{} = state) do
    attributes = tl(state.attributes)
    %__MODULE__{state | attributes: attributes, scope_attributes: attributes}
  end

  def add_type(
        %__MODULE__{} = state,
        env,
        type_name,
        type_args,
        spec,
        kind,
        {pos, end_pos},
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
        {pos, end_pos},
        options \\ []
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
    scope_id = hd(state.scope_ids)

    info = %VarInfo{
      name: name,
      version: version,
      positions: [extract_position(meta)],
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

    [vars_from_scope | other_vars] = state.vars_info

    case Map.get(vars_from_scope, {name, version}) do
      nil ->
        state

      info ->
        info = %VarInfo{
          info
          | positions: (info.positions ++ [extract_position(meta)]) |> Enum.uniq()
        }

        vars_from_scope = Map.put(vars_from_scope, {name, version}, info)

        %__MODULE__{
          state
          | vars_info: [vars_from_scope | other_vars]
        }
    end
  end

  def add_var_read(%__MODULE__{} = state, _), do: state

  def add_attribute(%__MODULE__{} = state, env, attribute, meta, args, type, is_definition) do
    position = extract_position(meta)
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
              | # TODO this is wrong for accumulating attributes
                type: type,
                positions: (existing.positions ++ [position]) |> Enum.uniq() |> Enum.sort()
            }
          end)
      end

    attributes = [attributes_from_scope | other_attributes]
    scope_attributes = [attributes_from_scope | tl(state.scope_attributes)]

    # TODO handle other
    # {moduledoc, nil, nil, []},
    # {after_compile, [], accumulate, []},
    # {after_verify, [], accumulate, []},
    # {before_compile, [], accumulate, []},
    # {behaviour, [], accumulate, []},
    # {compile, [], accumulate, []},
    # {derive, [], accumulate, []},
    # {dialyzer, [], accumulate, []},
    # {external_resource, [], accumulate, []},
    # {on_definition, [], accumulate, []},
    # {type, [], accumulate, []},
    # {opaque, [], accumulate, []},
    # {typep, [], accumulate, []},
    # {spec, [], accumulate, []},
    # {callback, [], accumulate, []},
    # {macrocallback, [], accumulate, []},
    # {optional_callbacks, [], accumulate, []},
    accumulating? =
      attribute in [:before_compile, :after_compile, :after_verify, :on_definition, :on_load]

    attribute_store =
      if is_definition do
        [arg] = args

        if accumulating? do
          state.attribute_store |> Map.update({env.module, attribute}, [arg], &(&1 ++ [arg]))
        else
          state.attribute_store |> Map.put({env.module, attribute}, arg)
        end
      else
        state.attribute_store
      end

    %__MODULE__{
      state
      | attributes: attributes,
        scope_attributes: scope_attributes,
        attribute_store: attribute_store
    }
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

  def default_env, do: %ElixirSense.Core.State.Env{}

  defp maybe_move_vars_to_outer_scope(
         %__MODULE__{vars_info: [current_scope_vars, outer_scope_vars | other_scopes_vars]} =
           state
       ) do
    outer_scope_vars =
      for {key, _} <- outer_scope_vars,
          into: %{},
          # TODO merge type and positions?
          do: {key, current_scope_vars[key]}

    vars_info = [current_scope_vars, outer_scope_vars | other_scopes_vars]

    %__MODULE__{state | vars_info: vars_info}
  end

  defp maybe_move_vars_to_outer_scope(state), do: state

  @module_functions [
    {:__info__, [:atom], :def},
    {:module_info, [], :def},
    {:module_info, [:atom], :def}
  ]

  def add_module_functions(state, env, functions, range) do
    {line, column} =
      case range do
        {{line, column}, _} -> {line, column}
        _ -> {0, nil}
      end

    meta = [line: line] ++ if(column > 0, do: [column: column], else: [])

    (functions ++ @module_functions)
    |> Enum.reduce(state, fn {name, args, kind}, acc ->
      mapped_args = for arg <- args, do: {arg, meta, nil}

      acc
      |> add_func_to_index(
        env,
        name,
        mapped_args,
        range,
        kind,
        generated: true
      )
    end)
  end

  def with_typespec(%__MODULE__{} = state, typespec) do
    %{state | typespec: typespec}
  end

  def add_struct_or_exception(state, env, type, fields, range) do
    {line, column} =
      case range do
        {{line, column}, _} -> {line, column}
        _ -> {0, nil}
      end

    meta = [line: line || 0] ++ if(column > 0, do: [column: column], else: [])

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
            [{:msg, meta, nil}],
            range,
            :def,
            options
          )
          |> add_func_to_index(
            env,
            :message,
            [{:exception, meta, nil}],
            range,
            :def,
            options
          )
        else
          state
        end
        |> add_func_to_index(
          env,
          :exception,
          [{:args, meta, nil}],
          range,
          :def,
          options
        )
      else
        state
      end
      |> add_func_to_index(env, :__struct__, [], range, :def, options)
      |> add_func_to_index(
        env,
        :__struct__,
        [{:kv, meta, nil}],
        range,
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
      |> Enum.filter(fn
        {{^current_module, name, _arity}, info} when not is_nil(name) ->
          name not in builtins and info.type == :def

        _ ->
          false
      end)

    new_specs =
      for {key = {_mod, name, _arity}, mod_fun_info} <- keys,
          into: %{},
          do:
            (
              new_spec =
                case state.specs[key] do
                  nil ->
                    %ModFunInfo{positions: positions, params: params} = mod_fun_info

                    args =
                      for param_variant <- params do
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

  def merge_inferred_types(state, []), do: state

  def merge_inferred_types(state, inferred_types) do
    [h | t] = state.vars_info

    h =
      for {key, type} <- inferred_types, reduce: h do
        acc ->
          Map.update!(acc, key, fn %VarInfo{type: old} = v ->
            %{v | type: ElixirSense.Core.TypeInference.intersect(old, type)}
          end)
      end

    %{state | vars_info: [h | t]}
  end

  def extract_position(meta) do
    line = Keyword.get(meta, :line, 0)

    if line <= 0 do
      {1, 1}
    else
      {
        line,
        Keyword.get(meta, :column, 1)
      }
    end
  end

  def extract_range(meta) do
    line = Keyword.get(meta, :line, 0)

    if line <= 0 do
      {{1, 1}, nil}
    else
      position = {
        line,
        Keyword.get(meta, :column, 1)
      }

      end_position =
        case meta[:end] do
          nil ->
            case meta[:end_of_expression] do
              nil ->
                nil

              end_of_expression_meta ->
                {
                  Keyword.fetch!(end_of_expression_meta, :line),
                  Keyword.fetch!(end_of_expression_meta, :column)
                }
            end

          end_meta ->
            {
              Keyword.fetch!(end_meta, :line),
              Keyword.fetch!(end_meta, :column) + 3
            }
        end

      {position, end_position}
    end
  end
end
