defmodule ElixirSense.Core.BuiltinTypes do

  @basic_types %{
    "any" => "The top type, the set of all terms",
    "none" => "The bottom type, contains no terms",
    "atom" => "An atom is a constant whose name is its own value. Some other languages call these symbols",
    "map" => "Any map",
    "pid" => "A process identifier, pid, identifies a process",
    "port" => "A port identifier identifies an Erlang port",
    "reference" => "A reference is a term that is unique in an Erlang runtime system, created by calling `make_ref/0`",
    "struct" => "Any struct",
    "tuple" => "Tuple of any size",
    "float" => "A floating-point number",
    "integer" => "An integer number",
    "neg_integer" => "A negative integer",
    "non_neg_integer" => "A non-negative integer",
    "pos_integer" => "A positive integer",
    "list/1" => "Proper list ([]-terminated)",
    "nonempty_list/1" => "Non-empty proper list",
    "maybe_improper_list/2" => "Proper or improper list (type1=contents, type2=termination)",
    "nonempty_improper_list/2" => "Improper list (type1=contents, type2=termination)",
    "nonempty_maybe_improper_list/2" => "Non-empty proper or improper list"
  }

  @builtin_types %{
    "term" => %{
      spec: (quote do: term :: any()),
      doc: "Same as `any()`"
    },
    "arity" => %{
      spec: (quote do: arity :: 0..255),
      doc: "The number of arguments that a function takes"
    },
    "as_boolean/1" => %{
      spec: (quote do: as_boolean(t) :: t),
      doc: "A type `t` whose value will be used as a _truthy_ value"
    },
    "binary" => %{
      spec: (quote do: binary :: <<_::_*8>>),
      doc: "A blob of binary data"
    },
    "bitstring" => %{
      spec: (quote do: bitstring :: <<_::_*1>>),
      doc: "A bunch of bits"
    },
    "boolean" => %{
      spec: (quote do: boolean :: false | true),
      doc: "`true` or `false`"
    },
    "byte" => %{
      spec: (quote do: byte :: 0..255),
      doc: "A valid byte (0..255)"
    },
    "char" => %{
      spec: (quote do: char :: 0..0x10FFFF),
      doc: "A valid char (0..0x10ffff)"
    },
    "charlist" => %{
      spec: (quote do: charlist :: [char()]),
      doc: "A list of `char()`"
    },
    "nonempty_charlist" => %{
      spec: (quote do: nonempty_charlist :: [char(), ...]),
      doc: "A non-empty list of `char()`"
    },
    "fun" => %{
      spec: (quote do: fun :: (... -> any)),
      doc: "A function"
    },
    "function" => %{
      spec: (quote do: function :: fun()),
      doc: "Same as `fun()`"
    },
    "identifier" => %{
      spec: (quote do: identifier :: pid() | port() | reference()),
      doc: "A `pid()`, `port()` or `reference()`"
    },
    "iodata" => %{
      spec: (quote do: iodata :: iolist() | binary()),
      doc: "An `iolist()` or a `binary()`"
    },
    "iolist" => %{
      spec: (quote do: iolist :: maybe_improper_list(byte() | binary() | iolist(), binary() | [])),
      doc: "A list whose elements are either bytes, binaries or other iolists"
    },
    "keyword" => %{
      spec: (quote do: keyword :: [{atom(), any()}]),
      doc: "A keyword list"
    },
    "keyword/1" =>%{
      spec:  (quote do: keyword(t) :: [{atom(), t}]),
      doc: "A keyword list with values of type `t`"
    },
    "list" => %{
      spec: (quote do: list :: [any()]),
      doc: "A list"
    },
    "nonempty_list" => %{
      spec: (quote do: nonempty_list :: nonempty_list(any())),
      doc: "A non-empty list"
    },
    "maybe_improper_list" => %{
      spec: (quote do: maybe_improper_list :: maybe_improper_list(any(), any())),
      doc: "An alias for `maybe_improper_list(any(), any())`"
    },
    "nonempty_maybe_improper_list" => %{
      spec: (quote do: nonempty_maybe_improper_list :: nonempty_maybe_improper_list(any(), any())),
      doc: "An alias for `nonempty_maybe_improper_list(any(), any())`"
    },
    "mfa" => %{
      spec: (quote do: mfa :: {module(), atom(), arity()}),
      doc: "A tuple with {module, function, arity}"
    },
    "module" => %{
      spec: (quote do: module :: atom()),
      doc: "A module name. An alias for `atom()`"
    },
    "no_return" => %{
      spec: (quote do: no_return :: none()),
      doc: "A return type indicating that a function throws exceptions or loops forever and never terminates. An alias for `none()`"
    },
    "node" => %{
      spec: (quote do: node :: atom()),
      doc: "An atom representing a node name"
    },
    "number" => %{
      spec: (quote do: number :: integer() | float()),
      doc: "An integer or a float"
    },
    "struct" => %{
      spec: (quote do: struct :: %{:__struct__ => atom(), optional(atom()) => any()}),
      doc: "A struct"
    },
    "timeout" => %{
      spec: (quote do: timeout :: :infinity | non_neg_integer()),
      doc: "A non-negative integer or `:infinity`"
    }
  }

  def get_builtin_type_doc(type, n_args \\ 0) do
    with nil <- @builtin_types[type_key(type, n_args)][:doc],
         nil <- @basic_types[type_key(type, n_args)] do
      ""
    else
      doc -> doc
    end
  end

  def get_builtin_type_spec(type, n_args \\ 0) do
    @builtin_types[type_key(type, n_args)][:spec]
  end

  defp type_key(type, n_args) do
    if n_args > 0 do
      "#{type}/#{n_args}"
    else
      "#{type}"
    end
  end
end
