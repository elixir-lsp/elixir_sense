defmodule ElixirSense.Core.BuiltinTypes do
  @moduledoc false

  @basic_types %{
    "any" => %{
      params: [],
      doc: "The top type, the set of all terms"
    },
    "none" => %{
      params: [],
      doc: "The bottom type, contains no terms"
    },
    "atom" => %{
      params: [],
      doc:
        "An atom is a constant whose name is its own value. Some other languages call these symbols"
    },
    "map" => %{
      params: [],
      doc: "Any map"
    },
    "pid" => %{
      params: [],
      doc: "A process identifier, pid, identifies a process"
    },
    "port" => %{
      params: [],
      doc: "A port identifier identifies an Erlang port"
    },
    "reference" => %{
      params: [],
      doc:
        "A reference is a term that is unique in an Erlang runtime system, created by calling `make_ref/0`"
    },
    "struct" => %{
      params: [],
      doc: "Any struct"
    },
    "tuple" => %{
      params: [],
      doc: "Tuple of any size"
    },
    "float" => %{
      params: [],
      doc: "A floating-point number"
    },
    "integer" => %{
      params: [],
      doc: "An integer number"
    },
    "neg_integer" => %{
      params: [],
      doc: "A negative integer"
    },
    "non_neg_integer" => %{
      params: [],
      doc: "A non-negative integer"
    },
    "pos_integer" => %{
      params: [],
      doc: "A positive integer"
    },
    "list/1" => %{
      params: [:t],
      doc: "Proper list ([]-terminated)",
      signature: "list(t)"
    },
    "nonempty_list/1" => %{
      params: [:t],
      doc: "Non-empty proper list",
      signature: "nonempty_list(t)"
    },
    "maybe_improper_list/2" => %{
      params: [:type1, :type2],
      doc: "Proper or improper list (type1=contents, type2=termination)",
      signature: "maybe_improper_list(type1, type2)"
    },
    "nonempty_improper_list/2" => %{
      params: [:type1, :type2],
      doc: "Improper list (type1=contents, type2=termination)",
      signature: "nonempty_improper_list(type1, type2)"
    },
    "nonempty_maybe_improper_list/2" => %{
      params: [:type1, :type2],
      doc: "Non-empty proper or improper list",
      signature: "nonempty_maybe_improper_list(type1, type2)"
    }
  }

  @builtin_types %{
    "term" => %{
      params: [],
      spec: quote(do: term() :: any()),
      doc: "Same as `any()`"
    },
    "arity" => %{
      params: [],
      spec: quote(do: arity() :: 0..255),
      doc: "The number of arguments that a function takes"
    },
    "as_boolean/1" => %{
      params: [],
      spec: quote(do: as_boolean(t) :: t),
      doc: "A type `t` whose value will be used as a _truthy_ value"
    },
    "binary" => %{
      params: [],
      spec: quote(do: binary() :: <<_::_*8>>),
      doc: "A blob of binary data"
    },
    "bitstring" => %{
      params: [],
      spec: quote(do: bitstring() :: <<_::_*1>>),
      doc: "A bunch of bits"
    },
    "boolean" => %{
      params: [],
      spec: quote(do: boolean() :: false | true),
      doc: "`true` or `false`"
    },
    "byte" => %{
      params: [],
      spec: quote(do: byte() :: 0..255),
      doc: "A valid byte (0..255)"
    },
    "char" => %{
      params: [],
      spec: quote(do: char() :: 0..0x10FFFF),
      doc: "A valid char (0..0x10ffff)"
    },
    "charlist" => %{
      params: [],
      spec: quote(do: charlist() :: [char()]),
      doc: "A list of `char()`"
    },
    "nonempty_charlist" => %{
      params: [],
      spec: quote(do: nonempty_charlist() :: [char(), ...]),
      doc: "A non-empty list of `char()`"
    },
    "fun" => %{
      params: [],
      spec: quote(do: fun() :: (... -> any)),
      doc: "A function"
    },
    "function" => %{
      params: [],
      spec: quote(do: function() :: fun()),
      doc: "Same as `fun()`"
    },
    "identifier" => %{
      params: [],
      spec: quote(do: identifier() :: pid() | port() | reference()),
      doc: "A `pid()`, `port()` or `reference()`"
    },
    "iodata" => %{
      params: [],
      spec: quote(do: iodata() :: iolist() | binary()),
      doc: "An `iolist()` or a `binary()`"
    },
    "iolist" => %{
      params: [],
      spec:
        quote(do: iolist() :: maybe_improper_list(byte() | binary() | iolist(), binary() | [])),
      doc: "A list whose elements are either bytes, binaries or other iolists"
    },
    "keyword" => %{
      params: [],
      spec: quote(do: keyword() :: [{atom(), any()}]),
      doc: "A keyword list"
    },
    "keyword/1" => %{
      params: [:t],
      spec: quote(do: keyword(t) :: [{atom(), t}]),
      doc: "A keyword list with values of type `t`"
    },
    "list" => %{
      params: [],
      spec: quote(do: list() :: [any()]),
      doc: "A list"
    },
    "nonempty_list" => %{
      params: [],
      spec: quote(do: nonempty_list :: nonempty_list(any())),
      doc: "A non-empty list"
    },
    "maybe_improper_list" => %{
      params: [],
      spec: quote(do: maybe_improper_list() :: maybe_improper_list(any(), any())),
      doc: "An alias for `maybe_improper_list(any(), any())`"
    },
    "nonempty_maybe_improper_list" => %{
      params: [],
      spec:
        quote(do: nonempty_maybe_improper_list() :: nonempty_maybe_improper_list(any(), any())),
      doc: "An alias for `nonempty_maybe_improper_list(any(), any())`"
    },
    "mfa" => %{
      params: [],
      spec: quote(do: mfa() :: {module(), atom(), arity()}),
      doc: "A tuple with {module, function, arity}"
    },
    "module" => %{
      params: [],
      spec: quote(do: module() :: atom()),
      doc: "A module name. An alias for `atom()`"
    },
    "no_return" => %{
      params: [],
      spec: quote(do: no_return() :: none()),
      doc:
        "A return type indicating that a function throws exceptions or loops forever and never terminates"
    },
    "node" => %{
      params: [],
      spec: quote(do: node() :: atom()),
      doc: "An atom representing a node name"
    },
    "number" => %{
      params: [],
      spec: quote(do: number() :: integer() | float()),
      doc: "An integer or a float"
    },
    "struct" => %{
      params: [],
      spec: quote(do: struct() :: %{:__struct__ => atom(), optional(atom()) => any()}),
      doc: "A struct"
    },
    "timeout" => %{
      params: [],
      spec: quote(do: timeout() :: :infinity | non_neg_integer()),
      doc: "A non-negative integer or `:infinity`"
    }
  }

  @types Map.merge(@basic_types, @builtin_types)

  def get_builtin_type_doc(type, n_args \\ 0) do
    case @types[type_key(type, n_args)][:doc] do
      nil ->
        ""

      doc ->
        doc
    end
  end

  def all do
    @types
  end

  def get_builtin_type_info(type_name) do
    for {key, value} <- @types, match_key?(type_name, key) do
      value
    end
  end

  def builtin_type?(type_name) do
    Enum.any?(@types, fn {k, _v} -> match_key?(type_name, k) end)
  end

  def get_builtin_type_spec(type, n_args \\ 0) do
    @types[type_key(type, n_args)][:spec]
  end

  defp match_key?(type_name, key) do
    key == "#{type_name}" || String.starts_with?(key, "#{type_name}/")
  end

  defp type_key(type, n_args) do
    if n_args > 0 do
      "#{type}/#{n_args}"
    else
      "#{type}"
    end
  end
end
