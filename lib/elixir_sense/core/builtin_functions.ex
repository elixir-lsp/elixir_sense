defmodule ElixirSense.Core.BuiltinFunctions do
  @moduledoc false
  require ElixirSense.Core.Introspection, as: Introspection

  @functions %{
    {:exception, 1} => %{specs: [quote(do: exception(term) :: Exception.t())], args: ["msg"]},
    {:message, 1} => %{
      specs: [quote(do: message(Exception.t()) :: String.t())],
      args: ["exception"]
    },
    {:__struct__, 0} => %{
      specs: [quote(do: __struct__() :: %{required(:__struct__) => module, optional(any) => any})],
      args: []
    },
    {:__struct__, 1} => %{
      specs: [
        quote(do: __struct__(keyword) :: %{required(:__struct__) => module, optional(any) => any})
      ],
      args: ["kv"]
    },
    {:impl_for, 1} => %{specs: [quote(do: impl_for(term) :: atom | nil)], args: ["data"]},
    {:impl_for!, 1} => %{specs: [quote(do: impl_for!(term) :: atom)], args: ["data"]},
    {:__protocol__, 1} => %{
      specs: [
        quote(do: __protocol__(:module) :: module),
        quote(do: __protocol__(:functions) :: [{atom, non_neg_integer}]),
        quote(do: __protocol__(:consolidated?) :: boolean),
        quote(do: __protocol__(:impls) :: :not_consolidated | {:consolidated, [module]})
      ],
      args: ["atom"]
    },
    {:__impl__, 1} => %{
      specs: [quote(do: __impl__(:for | :target | :protocol) :: module)],
      args: ["atom"]
    },
    {:__info__, 1} => %{
      specs: [
        quote(do: __info__(:attributes) :: keyword()),
        quote(do: __info__(:compile) :: [term()]),
        quote(do: __info__(:functions) :: [{atom, non_neg_integer}]),
        quote(do: __info__(:macros) :: [{atom, non_neg_integer}]),
        quote(do: __info__(:md5) :: binary()),
        quote(do: __info__(:module) :: module())
      ],
      args: ["atom"]
    },
    {:module_info, 0} => %{
      specs: [
        quote(
          do:
            module_info :: [{:module | :attributes | :compile | :exports | :md5 | :native, term}]
        )
      ],
      args: []
    },
    {:module_info, 1} => %{
      specs: [
        quote(do: module_info(:module) :: atom),
        quote(do: module_info(:attributes | :compile) :: [{atom, term}]),
        quote(do: module_info(:md5) :: binary),
        quote(do: module_info(:exports | :functions | :nifs) :: [{atom, non_neg_integer}]),
        quote(do: module_info(:native) :: boolean)
      ],
      args: ["key"]
    },
    {:behaviour_info, 1} => %{
      specs: [
        quote(do: behaviour_info(:callbacks | :optional_callbacks) :: [{atom, non_neg_integer}])
      ],
      args: ["key"]
    }
  }

  @docs %{
    {:module_info, 0} => """
    The `module_info/0` function in each module, returns a list of `{Key,Value}`
    tuples with information about the module. Currently, the list contain tuples with the following
    `Keys`: `module`, `attributes`, `compile`, `exports`, `md5` and `native`. The order and number
    of tuples may change without prior notice.
    """,
    {:module_info, 1} => """
    The call `module_info(Key)`, where `Key` is an atom, returns a single piece of information about the module.

    The following values are allowed for Key:

    - **`module`**
    Returns an atom representing the module name.

    - **`attributes`**
    Returns a list of `{AttributeName,ValueList}` tuples, where `AttributeName` is the name of an attribute,
    and `ValueList` is a list of values. Notice that a given attribute can occur more than once in the list
    with different values if the attribute occurs more than once in the module.  
    The list of attributes becomes empty if the module is stripped with the
    [`beam_lib(3)`](https://www.erlang.org/doc/man/beam_lib#strip-1) module (in STDLIB).

    - **`compile`**
    Returns a list of tuples with information about how the module was compiled. This list is empty
    if the module has been stripped with the [`beam_lib(3)`](https://www.erlang.org/doc/man/beam_lib#strip-1)
    module (in STDLIB).

    - **`md5`**
    Returns a binary representing the MD5 checksum of the module.

    - **`exports`**
    Returns a list of `{Name,Arity}` tuples with all exported functions in the module.

    - **`functions`**
    Returns a list of `{Name,Arity}` tuples with all functions in the module.

    - **`nifs`**
    Returns a list of `{Name,Arity}` tuples with all NIF functions in the module.

    - **`native`**
    Return `true` if the module has native compiled code. Return `false` otherwise. In a system compiled
    without HiPE support, the result is always `false`
    """,
    {:behaviour_info, 1} => """
    The `behaviour_info(Key)` function, where `Key` is an atom, retrieves specific information related to the Erlang
    behaviour module's callbacks.

    This function supports two distinct keys:

    - **`callbacks`**
    Returns a list of `{Name,Arity}` tuples, where each tuple represents a callback function within the behaviour.
    This list is generated based on the `-callback` attributes defined in the behaviour module.

    - **`optional_callbacks`**
    Returns a list of `{OptName,OptArity}` tuples, detailing the optional callback functions for the behaviour.
    These optional callbacks are defined using the `-optional_callbacks` attribute in conjunction with the `-callback` attribute.
    """,
    {:__info__, 1} =>
      Code.fetch_docs(Module)
      |> elem(6)
      |> Enum.find(fn t -> elem(t, 0) == {:callback, :__info__, 1} end)
      |> elem(3)
      |> Map.fetch!("en"),
    {:__struct__, 0} => """
    Returns the struct
    """,
    {:__struct__, 1} => """
    Returns a new struct filled from the given keyword list.
    """,
    {:impl_for, 1} => """
    Returns the module that implements the protocol for the given argument, `nil` otherwise.

    For example, for the `Enumerable` protocol we have:

      iex> Enumerable.impl_for([])
      Enumerable.List

      iex> Enumerable.impl_for(42)
      nil
    """,
    {:impl_for!, 1} => """
    Returns the module that implements the protocol for the given argument, raises `Protocol.UndefinedError`
    if an implementation is not found
    """,
    {:__protocol__, 1} => """
    Returns the protocol information.

    The function takes one of the following atoms:

      * `:consolidated?` - returns whether the protocol is consolidated

      * `:functions` - returns a keyword list of protocol functions and their arities

      * `:impls` - if consolidated, returns `{:consolidated, modules}` with the list of modules
        implementing the protocol, otherwise `:not_consolidated`

      * `:module` - the protocol module atom name

    For example, for the `Enumerable` protocol we have:

      iex> Enumerable.__protocol__(:functions)
      [count: 1, member?: 2, reduce: 3, slice: 1]
    """,
    {:__impl__, 1} => """
    Returns the protocol implementation information.

    The function takes one of the following atoms:

    * `:for` - returns the module responsible for the data structure of the
      protocol implementation

    * `:protocol` - returns the protocol module for which this implementation
    is provided

    For example, the module implementing the `Enumerable` protocol for lists is
    `Enumerable.List`. Therefore, we can invoke `__impl__/1` on this module:

      iex(1)> Enumerable.List.__impl__(:for)
      List

      iex(2)> Enumerable.List.__impl__(:protocol)
      Enumerable
    """,
    {:exception, 1} => """
    Receives the arguments given to `raise/2` and returns the exception struct.

    The default implementation accepts either a set of keyword arguments that is merged into
    the struct or a string to be used as the exception's message.
    """,
    {:message, 1} => """
    Receives the exception struct and must return its message.

    Most commonly exceptions have a message field which by default is accessed by this function.
    However, if an exception does not have a message field, this function must be explicitly
    implemented.
    """
  }

  @all Map.keys(@functions)

  @doc """
  Returns a list of all builtin functions matching the given function name and arity.

  ## Parameters
    - function: The function name (atom or string)
    - arity: The arity (integer) or :any for all arities
    
  ## Returns
    A list of tuples {function_name, arity, doc, specs} for all matching functions.
    
  ## Examples
    
      iex> get_builtin_functions_doc(:module_info, :any)
      [{"module_info", 0, "The `module_info/0` function...", ["@spec module_info() :: ..."]},
       {"module_info", 1, "The call `module_info(Key)`...", ["@spec module_info(:module) :: ..."]}]
      
      iex> get_builtin_functions_doc(:module_info, 1)
      [{"module_info", 1, "The call `module_info(Key)`...", ["@spec module_info(:module) :: ..."]}]
      
      iex> get_builtin_functions_doc(:module_info, 0)
      [{"module_info", 0, "The `module_info/0` function...", ["@spec module_info() :: ..."]}]
  """
  def get_builtin_functions_doc(function, arity) do
    function_atom = if is_atom(function), do: function, else: String.to_atom(to_string(function))

    @functions
    |> Enum.filter(fn {{f, a}, _value} ->
      f == function_atom and Introspection.matches_arity?(a, arity)
    end)
    |> Enum.map(fn {{f, a}, %{specs: specs}} ->
      doc = Map.get(@docs, {f, a}, "")
      formatted_specs = specs |> Enum.map(&"@spec #{Macro.to_string(&1)}")
      {to_string(f), a, doc, formatted_specs}
    end)
    |> Enum.sort_by(fn {_name, arity, _doc, _specs} -> arity end)
  end

  for {{f, a}, %{specs: specs}} <- @functions do
    def get_specs({unquote(f), unquote(a)}),
      do: unquote(specs |> Enum.map(&"@spec #{Macro.to_string(&1)}"))
  end

  for {{f, a}, %{args: args}} <- @functions do
    def get_args({unquote(f), unquote(a)}),
      do: unquote(args)
  end

  for {{f, a}, doc} <- @docs do
    def get_docs({unquote(f), unquote(a)}),
      do: unquote(doc)
  end

  # TODO exception message
  def get_docs(_), do: ""

  def all, do: @all

  def erlang_builtin_functions(:erlang), do: [{:andalso, 2}, {:orelse, 2}]
  def erlang_builtin_functions(_), do: []
end
