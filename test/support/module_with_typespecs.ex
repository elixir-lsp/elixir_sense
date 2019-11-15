defmodule ElixirSenseExample.ModuleWithTypespecs do
  defmodule Remote do
    @typedoc "Remote type"
    @type remote_t :: atom

    @typedoc "Remote type with params"
    @type remote_t(a, b) :: {a, b}

    @typedoc "Remote list type"
    @type remote_list_t :: [remote_t]

    @type remote_option_t :: {:remote_option_1, remote_t} | {:remote_option_2, remote_list_t}
  end

  defmodule Local do
    alias Remote, as: R

    @typep private_t :: atom

    @typedoc "Local opaque type"
    @opaque opaque_t :: atom

    @typedoc "Local type"
    @type local_t :: atom

    @typedoc "Local type with params"
    @type local_t(a, b) :: {a, b}

    @typedoc "Local union type"
    @type union_t :: atom | integer

    @typedoc "Local list type"
    @type list_t :: [:trace | :log]

    @typedoc "Local type with large spec"
    @type large_t :: pid | port | (registered_name :: atom) | {registered_name :: atom, node}

    @typedoc "Remote type from aliased module"
    @type remote_aliased_t :: R.remote_t() | R.remote_list_t()

    @typedoc "Local keyword-value type"
    @type option_t ::
            {:local_o, local_t}
            | {:local_with_params_o, local_t(atom, integer)}
            | {:union_o, union_t}
            | {:inline_union_o, :a | :b}
            | {:list_o, list_t}
            | {:inline_list_o, [:trace | :log]}
            | {:basic_o, pid}
            | {:basic_with_params_o, nonempty_list(atom)}
            | {:builtin_o, keyword}
            | {:builtin_with_params_o, keyword(term)}
            | {:remote_o, Remote.remote_t()}
            | {:remote_with_params_o, Remote.remote_t(atom, integer)}
            | {:remote_aliased_o, remote_aliased_t}
            | {:remote_aliased_inline_o, R.remote_t()}
            | {:private_o, private_t}
            | {:opaque_o, opaque_t}
            | {:non_existent_o, Remote.non_existent()}
            | {:large_o, large_t}

    @typedoc "Extra option"
    @type extra_option_t :: {:option_1, atom} | {:option_2, integer}

    @typedoc "Options"
    @type options_t :: [option_t]

    @typedoc "Option | Extra option"
    @type option_or_extra_option_t ::
            {:option_1, boolean} | {:option_2, timeout} | Remote.remote_option_t()

    @spec func_with_options(options_t) :: any
    def func_with_options(options) do
      options
    end

    @spec func_with_union_of_options([option_t | extra_option_t]) :: any
    def func_with_union_of_options(options) do
      options
    end

    @spec func_with_union_of_options_as_type([option_or_extra_option_t]) :: any
    def func_with_union_of_options_as_type(options) do
      options
    end

    @spec func_with_union_of_options_inline([{:option_1, atom} | {:option_2, integer} | option_t]) ::
            any
    def func_with_union_of_options_inline(options) do
      options
    end

    @spec func_with_named_options(options :: options_t) :: any
    def func_with_named_options(options) do
      options
    end

    @spec func_with_options_as_inline_list([{:local_o, local_t} | {:builtin_o, keyword}]) :: any
    def func_with_options_as_inline_list(options) do
      options
    end

    @spec func_with_option_var_defined_in_when([opt]) :: any when opt: option_t
    def func_with_option_var_defined_in_when(options) do
      options
    end

    @spec func_with_options_var_defined_in_when(opts) :: any when opts: [option_t]
    def func_with_options_var_defined_in_when(options) do
      options
    end

    @spec func_with_one_option([{:option_1, integer}]) :: any
    def func_with_one_option(options) do
      options
    end

    @spec fun_without_options([integer]) :: integer
    def fun_without_options(a), do: length(a)
  end
end
